unit ConvolutionDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_Complex, DAV_DspFftReal2Complex;

type
  TConvolutionDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
  private
    FFilterKernel   : PDAVSingleFixedArray;
    FSignalPadded   : PDAVSingleFixedArray;
    FFilterFreq     : PDAVSingleFixedArray;
    FSignalFreq     : PDAVSingleFixedArray;
    FSignalTime     : array of PDAVSingleFixedArray;
    FBuffer         : PDAVSingleFixedArray;
    FSemaphore      : Integer;
    FFft            : TFftReal2ComplexNativeFloat32;
    FIRSize         : Integer;
    FFFTSize        : Integer;
    FFFTSizeHalf    : Integer;
    FFFTSizeQuarter : Integer;
    procedure PerformConvolution(Signal: PDAVSingleFixedArray);
    procedure SetIRSize(const Value: Integer);
    procedure IRSizeChanged;
  public
    procedure LoadIR(FileName: TFileName);
    property IRSize: Integer read FFFTSize write SetIRSize; 
  end;

implementation

{$R *.DFM}

uses
  Math, WaveIOX, DAV_DspWindowing, ConvolutionGUI;

procedure TConvolutionDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
 FFilterKernel := nil;
 FSignalPadded := nil;
 FFilterFreq   := nil;
 FSignalFreq   := nil;
 FBuffer       := nil;
 SetLength(FSignalTime, max(numInputs, numOutputs));
 FFFTSize       := 0;
end;

procedure TConvolutionDataModule.VSTModuleOpen(Sender: TObject);
begin
 IRSize := 64;
end;

procedure TConvolutionDataModule.VSTModuleClose(Sender: TObject);
var
  i : Integer;
begin
 Dispose(FFilterKernel);
 Dispose(FSignalPadded);
 Dispose(FFilterFreq);
 Dispose(FSignalFreq);
 for i := 0 to Length(FSignalTime) - 1
  do Dispose(FSignalTime[i]);
 FreeAndNil(FFft);
end;

procedure TConvolutionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmConvolution.Create(Self);
end;

procedure TConvolutionDataModule.SetIRSize(const Value: Integer);
begin
 if FIRSize <> Value then
  begin
   FIRSize := Value;
   IRSizeChanged;
  end;
end;

procedure TConvolutionDataModule.IRSizeChanged;
var
  i : Integer;
begin
 i := 1 + CeilLog2(FIRSize);
 if not assigned(FFft)
  then FFft := TFftReal2ComplexNativeFloat32.Create(i)
  else FFft.Order := i;

 FFFTSize        := FFft.FFTSize;
 FFFTSizeHalf    := FFFTSize div 2;
 FFFTSizeQuarter := FFFTSize div 4;
 ReallocMem(FFilterKernel, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalPadded, FFFTSize * SizeOf(Single));
 ReallocMem(FFilterFreq, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, FFFTSize * SizeOf(Single));
 ReallocMem(FBuffer, FFFTSize * SizeOf(Single));

 FillChar(FFilterKernel^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalPadded^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FFilterFreq^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FBuffer^[0], FFFTSize * SizeOf(Single), 0);

 for i := 0 to Length(FSignalTime) - 1 do
  begin
   GetMem(FSignalTime[i], FFFTSize * SizeOf(Single));
   FillChar(FSignalTime[i]^[0], FFFTSize * SizeOf(Single), 0);
  end;

 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TConvolutionDataModule.LoadIR(FileName: TFileName);
var
  sr, c, sz : Integer;
  pt        : PSingle;
begin
 if assigned(FFilterKernel) and assigned(FFilterFreq) and assigned(FFft) then
  begin
   while FSemaphore > 0 do;
   inc(FSemaphore);
   try
    pt := LoadWAVFileMono(FileName, sr, c, sz);
    IRSize := sz;
    Move(pt^, FFilterKernel^[0], sz * SizeOf(Single));
    FillChar(FFilterKernel^[sz], (FFFTSize - sz) * SizeOf(Single), 0);

    // calculate frequency
    FFft.PerformFFT32(PDAVComplexSingleFixedArray(FFilterFreq), FFilterKernel);
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure TConvolutionDataModule.PerformConvolution(Signal: PDAVSingleFixedArray);
var
  Bin  : Integer;
  Half : Integer;
begin
 FFft.PerformFFT32(PDAVComplexSingleFixedArray(FSignalFreq), @Signal[0]);
 Half := FFFTSizeHalf;

 // DC
 Bin := 0;
 FSignalFreq^[Bin] := FFilterFreq^[Bin] * FSignalFreq^[Bin];
 inc(Bin);

 // inbetween...
 while Bin < FFFTSizeHalf do
  begin
   ComplexMultiplyInplace(FSignalFreq^[Bin], FSignalFreq^[Bin + Half],
     FFilterFreq^[Bin], FFilterFreq^[Bin + Half]);
   inc(Bin);
  end;

 // Nyquist
 FSignalFreq^[Bin] := FFilterFreq^[Bin] * FSignalFreq^[Bin];

 FFft.PerformIFFT32(PDAVComplexSingleFixedArray(FSignalFreq), @Signal[0]);
end;

procedure TConvolutionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel     : Integer;
  SamplesPos  : Integer;
  SampleCount : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  if SampleFrames <= 0 then exit;
  SamplesPos := 0;

  repeat
   // calculate samples left to process
   SampleCount := SampleFrames - SamplesPos;

   // limit to 50% overlap if necessary
   if SampleCount > FFFTSizeHalf then SampleCount := FFFTSizeHalf;

   for Channel := 0 to numOutputs - 1 do
    begin
     Move(FSignalTime[Channel, SampleCount], FSignalTime[Channel, 0], (FFFTSize - SampleCount) * SizeOf(Single));
     Move(Inputs[Channel, SamplesPos], FSignalTime[Channel, (FFFTSize - SampleCount)], SampleCount * SizeOf(Single));
     Move(FSignalTime[Channel, 0], FBuffer[0], FFFTSize * SizeOf(Single));
     PerformConvolution(@FBuffer[0]);
     Move(FBuffer[(FFFTSize - SampleCount)], Outputs[Channel, SamplesPos], SampleCount * SizeOf(Single));
    end;
   inc(SamplesPos, SampleCount);
  until SamplesPos >= SampleCount;

(*
  // complete block processing
  while SamplesPos + FFFTSizeHalf < SampleFrames do
   begin
    for Channel := 0 to numOutputs - 1 do
     begin
      Move(FSignalTime[Channel, FFFTSizeHalf], FSignalTime[Channel, 0], FFFTSizeHalf * SizeOf(Single));
      Move(Inputs[Channel, SamplesPos], FSignalTime[Channel, FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single));
      Move(FSignalTime[Channel, 0], FBuffer[0], FFFTSize * SizeOf(Single));
      PerformConvolution(@FBuffer[0]);
      Move(FBuffer[FFFTSizeHalf], Outputs[Channel, SamplesPos], FFFTSizeHalf * SizeOf(Single));
     end;
    inc(SamplesPos, FFFTSizeHalf);
   end
*)

 finally
  dec(FSemaphore);
 end;
end;

end.
