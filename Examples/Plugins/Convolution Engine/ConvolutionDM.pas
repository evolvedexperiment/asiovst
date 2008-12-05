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
    FFilterKernel       : PDAVSingleFixedArray;
    FFilterFreqs        : array of PDAVSingleFixedArray;
    FSignalFreq         : PDAVSingleFixedArray;
    FConvolved          : PDAVSingleFixedArray;
    FConvolvedTime      : PDAVSingleFixedArray;
    FOutputBuffer       : array of PDAVSingleFixedArray;
    FSemaphore          : Integer;
    FFft                : TFftReal2ComplexNativeFloat32;
    FIRSize             : Integer;
    FOffsetSize         : Integer;

    FFreqRespBlockCount : Integer;
    FIRSizePadded       : Integer;
    FFFTSize            : Integer;
    FFFTSizeHalf        : Integer;
    FFFTSizeQuarter     : Integer;
    procedure SetIRSizePadded(const Value: Integer);
    procedure SetIRBlockSize(const Value: Integer);
  protected
    procedure CalculateFilterBlockFrequencyResponses;
    procedure CalculatePaddedIRSize;
    procedure CalculateFrequencyResponseBlockCount;
    procedure IRSizePaddedChanged;
    procedure IRBlockSizeChanged;
    procedure PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray);

    property IRSize: Integer read FIRSize;
    property IRSizePadded: Integer read FIRSizePadded write SetIRSizePadded;
  public
    procedure LoadIR(FileName: TFileName);
    property IRBlockSize: Integer read FFFTSize write SetIRBlockSize;
  end;

implementation

{$R *.DFM}

uses
  Math, WaveIOX, ConvolutionGUI;

procedure TConvolutionDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore          := 0;
 FFilterKernel       := nil;
 FSignalFreq         := nil;
 FConvolved          := nil;
 FConvolvedTime      := nil;
 FFFTSize            := 0;
 FFreqRespBlockCount := 0;
 FIRSizePadded       := 0;
 FFFTSize            := 0;
 FFFTSizeHalf        := 0;
 FFFTSizeQuarter     := 0;
 BlockModeSize       := 512;
 BlockModeOverlap    := BlockModeSize div 2;
 InitialDelay        := BlockModeOverlap;
 IRBlockSize         := BlockModeSize;
 FOffsetSize         := BlockModeSize - BlockModeOverlap;
 SetLength(FOutputBuffer, max(numInputs, numOutputs));
end;

procedure TConvolutionDataModule.VSTModuleOpen(Sender: TObject);
begin
 FIRSize := 0;
end;

procedure TConvolutionDataModule.VSTModuleClose(Sender: TObject);
var
  i : Integer;
begin
 Dispose(FFilterKernel);
 Dispose(FSignalFreq);
 Dispose(FConvolved);
 Dispose(FConvolvedTime);
 for i := 0 to Length(FOutputBuffer) - 1 do Dispose(FOutputBuffer[i]);
 for i := 0 to Length(FFilterFreqs)  - 1 do Dispose(FFilterFreqs[i]);
 FreeAndNil(FFft);
end;

procedure TConvolutionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmConvolution.Create(Self);
end;

procedure TConvolutionDataModule.SetIRBlockSize(const Value: Integer);
begin
 if FFFTSize <> Value then
  begin
   FFFTSize := Value;
   IRBlockSizeChanged;
  end;
end;

procedure TConvolutionDataModule.IRSizePaddedChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FOutputBuffer) - 1 do
  begin
   GetMem(FOutputBuffer[Channel], FIRSizePadded * SizeOf(Single));
   FillChar(FOutputBuffer[Channel]^[0], FIRSizePadded * SizeOf(Single), 0);
  end;
end;

procedure TConvolutionDataModule.SetIRSizePadded(const Value: Integer);
begin
 if FIRSizePadded <> Value then
  begin
   FIRSizePadded := Value;
   IRSizePaddedChanged;
  end;
end;

procedure TConvolutionDataModule.IRBlockSizeChanged;
var
  i : Integer;
begin
 i := CeilLog2(FFFTSize);
 if not assigned(FFft)
  then FFft := TFftReal2ComplexNativeFloat32.Create(i)
  else FFft.Order := i;

 FFFTSizeHalf    := FFFTSize div 2;
 FFFTSizeQuarter := FFFTSize div 4;

 ReallocMem(FFilterKernel, FFFTSize * SizeOf(Single));
 ReallocMem(FSignalFreq, FFFTSize * SizeOf(Single));
 ReallocMem(FConvolved, FFFTSize * SizeOf(Single));
 ReallocMem(FConvolvedTime, FFFTSize * SizeOf(Single));

 FillChar(FFilterKernel^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FConvolved^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FConvolvedTime^[0], FFFTSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TConvolutionDataModule.LoadIR(FileName: TFileName);
var
  sr, c : Integer;
  pt    : PSingle;
begin
 if assigned(FFilterKernel) and assigned(FFft) then
  begin
   while FSemaphore > 0 do;
   inc(FSemaphore);
   try
    pt := LoadWAVFileMono(FileName, sr, c, FIRSize);
    ReallocMem(FFilterKernel, FIRSize * SizeOf(Single));
    Move(pt^, FFilterKernel^[0], FIRSize * SizeOf(Single));

    CalculatePaddedIRSize;

    CalculateFilterBlockFrequencyResponses;
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure TConvolutionDataModule.CalculateFrequencyResponseBlockCount;
begin
 // calculate number of blocks over the whole IR
 FFreqRespBlockCount := (FIRSize + FFFTSizeHalf - 1) div FFFTSizeHalf;
end;

procedure TConvolutionDataModule.CalculatePaddedIRSize;
begin
 // calculate frequency block count
 CalculateFrequencyResponseBlockCount;

 // calculate the padded IR size (a multiply of FFT size / 2)
 IRSizePadded := FFreqRespBlockCount * FFFTSizeHalf;
end;

procedure TConvolutionDataModule.CalculateFilterBlockFrequencyResponses;
var
  TempIR     : PDAVSingleFixedArray;
  Blocks, sz : Integer;
begin
 // calculate frequency block count
 CalculateFrequencyResponseBlockCount;

 SetLength(FFilterFreqs, FFreqRespBlockCount);
 GetMem(TempIR, FFFTSize * SizeOf(Single));
 for Blocks := 0 to Length(FFilterFreqs) - 1 do
  begin
   ReallocMem(FFilterFreqs[Blocks], FFFTSize * SizeOf(Single));

   // calculate IR part size to be copied
   sz := IRSize - Blocks * FFFTSizeHalf;
   if sz > FFFTSizeHalf then sz := FFFTSizeHalf;

   // build temporary IR part
   move(FFilterKernel^[Blocks * FFFTSizeHalf], TempIR^[0], sz * SizeOf(Single));
   FillChar(TempIR^[sz], (FFFTSize - sz) * SizeOf(Single), 0);

   // perform FFT
   FFft.PerformFFT32(PDAVComplexSingleFixedArray(FFilterFreqs[Blocks]), TempIR);
  end;
end;

procedure TConvolutionDataModule.PerformConvolution(SignalIn, SignalOut: PDAVSingleFixedArray);
var
  Block  : Integer;
  Bin    : Integer;
  Half   : Integer;
  Sample : Integer;
  Temp   : PDAVSingleFixedArray;
begin
 FFft.PerformFFT32(PDAVComplexSingleFixedArray(FSignalFreq), SignalIn);
 Half := FFFTSizeHalf;

 for Block := 0 to FFreqRespBlockCount - 1 do
  begin
   // make a copy of the frequency respose
   move(FSignalFreq^[0], FConvolved^[0], FFFTSize * SizeOf(Single));

   // DC
   Bin := 0;
   FConvolved^[Bin] := FFilterFreqs[Block]^[Bin] * FConvolved^[Bin];
   inc(Bin);

   // inbetween...
   while Bin < FFFTSizeHalf do
    begin
     ComplexMultiplyInplace(FConvolved^[Bin], FConvolved^[Bin + Half],
       FFilterFreqs[Block]^[Bin], FFilterFreqs[Block]^[Bin + Half]);
     inc(Bin);
    end;

   // Nyquist
   FConvolved^[Bin] := FFilterFreqs[Block]^[Bin] * FConvolved^[Bin];

   FFft.PerformIFFT32(PDAVComplexSingleFixedArray(FConvolved), FConvolvedTime);

   // copy and combine
   Temp := @SignalOut^[Block * Half];
   for Sample := 0 to Half - 1
    do Temp^[Sample] := Temp^[Sample] + FConvolvedTime^[Half + Sample];
  end;
end;

procedure TConvolutionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel     : Integer;
begin
 // lock processing
 while FSemaphore > 0 do;
 inc(FSemaphore);

 try
  for Channel := 0 to Length(FOutputBuffer) - 1 do
   begin
    PerformConvolution(@Inputs[Channel, 0], FOutputBuffer[Channel]);

    Move(FOutputBuffer[Channel]^[0], Outputs[Channel, BlockModeOverlap], FOffsetSize * SizeOf(Single));
    Move(FOutputBuffer[Channel]^[FOffsetSize], FOutputBuffer[Channel]^[0], (FIRSizePadded - FOffsetSize) * SizeOf(Single));
    FillChar(FOutputBuffer[Channel]^[(FIRSizePadded - FOffsetSize)], FOffsetSize * SizeOf(Single), 0);
   end;
 finally
  dec(FSemaphore);
 end;
end;

end.
