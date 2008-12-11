unit LinearPhaseDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_Complex, DAV_DspFftReal2Complex;

type
  TLinearPhaseDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FFilterKernel : PDAVSingleFixedArray;
    FSignalPadded : PDAVSingleFixedArray;
    FFilterFreq   : PDAVSingleFixedArray;
    FSignalFreq   : PDAVSingleFixedArray;
    FSemaphore    : Integer;
    FFft          : TFftReal2ComplexNativeFloat32;
    procedure CalculateFilterKernel;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_DspWindowing, LinearPhaseGUI;

procedure TLinearPhaseDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
 FFilterKernel := nil;
 FSignalPadded := nil;
 FFilterFreq   := nil;
 FSignalFreq   := nil;
 BlockModeOverlap := BlockModeSize div 2;
end;

procedure TLinearPhaseDataModule.VSTModuleOpen(Sender: TObject);
begin
 GetMem(FFilterKernel, BlockModeSize * SizeOf(Single));
 GetMem(FSignalPadded, BlockModeSize * SizeOf(Single));
 GetMem(FFilterFreq, BlockModeSize * SizeOf(Single));
 GetMem(FSignalFreq, BlockModeSize * SizeOf(Single));

 FillChar(FFilterKernel^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalPadded^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);

 FFft := TFftReal2ComplexNativeFloat32.Create(round(Log2(BlockModeSize)));
 FFft.AutoScaleType := astDivideInvByN;
 CalculateFilterKernel;
end;

procedure TLinearPhaseDataModule.VSTModuleClose(Sender: TObject);
begin
 Dispose(FFilterKernel);
 Dispose(FSignalPadded);
 Dispose(FFilterFreq);
 Dispose(FSignalFreq);
 FreeAndNil(FFft);
end;

procedure TLinearPhaseDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateFilterKernel;
 if EditorForm is TFmLinearPhase then
  with TFmLinearPhase(EditorForm)
   do UpdateFrequency;
end;

procedure TLinearPhaseDataModule.CalculateFilterKernel;
var
  i, h, q : Integer;
  n       : Double;
  CutOff  : Double;
begin
 if assigned(FFilterKernel) and assigned(FFilterFreq) and assigned(FFft) then
  begin
   while FSemaphore > 0 do;
   inc(FSemaphore);
   try
    CutOff := Parameter[0] / SampleRate;
    h := BlockModeSize div 2;
    q := BlockModeSize div 4;

    // Generate sinc delayed by (N-1)/2
    for i := 0 to h - 1 do
     if (i = q)
      then FFilterKernel^[i] := 1 - 2.0 * CutOff
      else
       begin
        n := PI * (i - q);
        FFilterKernel^[i] := -sin(2.0 * Cutoff * n) / n;
       end;
    ApplyBlackmanWindow(FFilterKernel, h);
    FillChar(FFilterKernel^[h], h * SizeOf(Single), 0);

    // calculate frequency
    FFft.PerformFFT32(PDAVComplexSingleFixedArray(FFilterFreq), FFilterKernel);
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure TLinearPhaseDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmLinearPhase.Create(Self);
end;

procedure TLinearPhaseDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Bin     : Integer;
  Half    : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Channel := 0 to numOutputs - 1 do
   begin
    FFft.PerformFFT32(PDAVComplexSingleFixedArray(FSignalFreq), @Inputs[Channel, 0]);

    Half := BlockModeSize div 2;

    // DC
    Bin := 0;
    FSignalFreq^[Bin] := FFilterFreq^[Bin] * FSignalFreq^[Bin];
    inc(Bin);

    // inbetween...
    while Bin < BlockModeSize div 2 do
     begin
      ComplexMultiplyInplace(FSignalFreq^[Bin], FSignalFreq^[Bin + Half],
        FFilterFreq^[Bin], FFilterFreq^[Bin + Half]);
      inc(Bin);
     end;

    // Nyquist
    FSignalFreq^[Bin] := FFilterFreq^[Bin] * FSignalFreq^[Bin];

    FFft.PerformIFFT32(PDAVComplexSingleFixedArray(FSignalFreq), @Outputs[Channel, 0]);
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TLinearPhaseDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 CalculateFilterKernel;
end;

end.