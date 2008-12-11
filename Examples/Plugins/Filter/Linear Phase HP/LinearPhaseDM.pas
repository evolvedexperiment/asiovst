unit LinearPhaseDM;

interface

{$I ASIOVST.INC}
{-$DEFINE Use_IPPS}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_Complex,
  DAV_DspFftReal2Complex, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  DAV_VSTModule;

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
    FFilterFreq   : PDAVComplexSingleFixedArray;
    FSignalFreq   : PDAVComplexSingleFixedArray;
    FSemaphore    : Integer;
    {$IFDEF Use_IPPS}
    FFft          : TFftReal2ComplexIPPSFloat32;
    {$ELSE}
    FFft          : TFftReal2ComplexNativeFloat32;
    {$ENDIF}
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
 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(round(Log2(BlockModeSize)));

 GetMem(FFilterFreq, (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 GetMem(FSignalFreq, (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 FillChar(FFilterFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FSignalFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(round(Log2(BlockModeSize)));

 GetMem(FFilterFreq, BlockModeSize * SizeOf(Single));
 GetMem(FSignalFreq, BlockModeSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);
 {$ENDIF}

 GetMem(FFilterKernel, BlockModeSize * SizeOf(Single));
 GetMem(FSignalPadded, BlockModeSize * SizeOf(Single));
 FillChar(FFilterKernel^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalPadded^[0], BlockModeSize * SizeOf(Single), 0);

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
    {$IFDEF Use_IPPS}
    FFft.Perform_FFT(FFilterFreq, FFilterKernel);
    {$ELSE}
    FFft.PerformFFT32(FFilterFreq, FFilterKernel);
    {$ENDIF}
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
 Half := BlockModeSize div 2;
 try
  for Channel := 0 to numOutputs - 1 do
   begin
    {$IFDEF Use_IPPS}
    FFft.Perform_FFT(PDAVComplexSingleFixedArray(FSignalFreq), @Inputs[Channel, 0]);

    // DC & Nyquist
    FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
    FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

    for Bin := 1 to Half - 1
     do ComplexMultiplyInplace(FSignalFreq^[Bin], FFilterFreq^[Bin]);

    FFft.Perform_IFFT(PDAVComplexSingleFixedArray(FSignalFreq), @Outputs[Channel, 0]);

    {$ELSE}

    FFft.PerformFFT32(PDAVComplexSingleFixedArray(FSignalFreq), @Inputs[Channel, 0]);

    // DC
    Bin := 0;
    PDAVSingleFixedArray(FSignalFreq)^[Bin] :=
      PDAVSingleFixedArray(FFilterFreq)^[Bin] * PDAVSingleFixedArray(FSignalFreq)^[Bin];
    inc(Bin);

    // inbetween...
    while Bin < Half do
     begin
      ComplexMultiplyInplace(
        PDAVSingleFixedArray(FSignalFreq)^[Bin],
        PDAVSingleFixedArray(FSignalFreq)^[Bin + Half],
        PDAVSingleFixedArray(FFilterFreq)^[Bin],
        PDAVSingleFixedArray(FFilterFreq)^[Bin + Half]);
      inc(Bin);
     end;

    // Nyquist
    PDAVSingleFixedArray(FSignalFreq)^[Bin] :=
      PDAVSingleFixedArray(FFilterFreq)^[Bin] * PDAVSingleFixedArray(FSignalFreq)^[Bin];

    FFft.PerformIFFT32(PDAVComplexSingleFixedArray(FSignalFreq), @Outputs[Channel, 0]);
    {$ENDIF}
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