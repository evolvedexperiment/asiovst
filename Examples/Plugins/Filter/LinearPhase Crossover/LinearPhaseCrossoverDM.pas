unit LinearPhaseCrossoverDM;

interface

{$I DAV_Compiler.INC}
{.$DEFINE Use_IPPS}
{.$DEFINE Use_CUDA}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Types, DAV_Complex, DAV_DspFftReal2Complex,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_VSTModule;

type
  TLinearPhaseCrossoverModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FFilterKernel : PDAVSingleFixedArray;
    FSignalPadded : PDAVSingleFixedArray;
    FFilterFreq   : array [0..1] of PDAVComplexSingleFixedArray;
    FSignalFreq   : array [0..1] of PDAVComplexSingleFixedArray;
    FSemaphore    : Integer;
    {$IFDEF Use_IPPS}
    FFft          : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft          : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft          : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    procedure CalculateFilterKernel;
  end;

implementation

uses
  Math, DAV_DspWindowing;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TLinearPhaseCrossoverModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore       := 0;
 FFilterKernel    := nil;
 FSignalPadded    := nil;
 FFilterFreq[0]   := nil;
 FFilterFreq[1]   := nil;
 FSignalFreq[0]   := nil;
 FSignalFreq[1]   := nil;
 BlockModeOverlap := BlockModeSize div 2;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcessReplacing := VSTModuleProcess;
 {$ENDIF}

end;

procedure TLinearPhaseCrossoverModule.VSTModuleOpen(Sender: TObject);
begin
 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(round(Log2(BlockModeSize)));

 ReallocMem(FFilterFreq[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 ReallocMem(FFilterFreq[1], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 ReallocMem(FSignalFreq[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 ReallocMem(FSignalFreq[1], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle));
 FillChar(FFilterFreq[0]^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FFilterFreq[1]^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FSignalFreq[0]^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FSignalFreq[1]^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(round(Log2(BlockModeSize)));

 ReallocMem(FFilterFreq[0], BlockModeSize * SizeOf(Single));
 ReallocMem(FFilterFreq[1], BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalFreq[0], BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalFreq[1], BlockModeSize * SizeOf(Single));
 FillChar(FFilterFreq[0]^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FFilterFreq[1]^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq[0]^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq[1]^[0], BlockModeSize * SizeOf(Single), 0);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(round(Log2(BlockModeSize)));

 ReallocMem(FFilterFreq[0], BlockModeSize * SizeOf(Single));
 ReallocMem(FFilterFreq[1], BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalFreq[0], BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalFreq[1], BlockModeSize * SizeOf(Single));
 FillChar(FFilterFreq[0]^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FFilterFreq[1]^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq[0]^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq[1]^[0], BlockModeSize * SizeOf(Single), 0);
 {$ENDIF}{$ENDIF}

 ReallocMem(FFilterKernel, BlockModeSize * SizeOf(Single));
 ReallocMem(FSignalPadded, BlockModeSize * SizeOf(Single));
 FillChar(FFilterKernel^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalPadded^[0], BlockModeSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;
 FFft.DataOrder := doPackedComplex;
 CalculateFilterKernel;

 Parameter[0] := 1000;
 Parameter[1] := 2;
end;

procedure TLinearPhaseCrossoverModule.VSTModuleClose(Sender: TObject);
begin
 Dispose(FFilterKernel);
 Dispose(FSignalPadded);
 Dispose(FFilterFreq[0]);
 Dispose(FFilterFreq[1]);
 Dispose(FSignalFreq[0]);
 Dispose(FSignalFreq[1]);
 FreeAndNil(FFft);
end;

procedure TLinearPhaseCrossoverModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Bin     : Integer;
  Half    : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 Half := BlockModeSize div 2;
 try
  begin
   {$IFDEF Use_IPPS}
   FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FSignalFreq[0]), @Inputs[0, 0]);

   // DC & Nyquist
   FSignalFreq[1]^[0].Re := FFilterFreq[0]^[0].Re * FSignalFreq[0]^[0].Re;
   FSignalFreq[1]^[0].Im := FFilterFreq[0]^[0].Im * FSignalFreq[0]^[0].Im;
   FSignalFreq[1]^[Half].Re := FFilterFreq[0]^[Half].Re * FSignalFreq[0]^[Half].Re;

   for Bin := 1 to Half - 1
    do FSignalFreq[1]^[Bin] := ComplexMultiply(FSignalFreq[0]^[Bin], FFilterFreq[0]^[Bin]);

   FFft.PerformIFFTCCS(PDAVComplexSingleFixedArray(FSignalFreq[1]), @Outputs[0, 0]);

   // DC & Nyquist
   FSignalFreq[0]^[0].Re := FFilterFreq[1]^[0].Re * FSignalFreq[0]^[0].Re;
   FSignalFreq[0]^[0].Im := FFilterFreq[1]^[0].Im * FSignalFreq[0]^[0].Im;
   FSignalFreq[0]^[Half].Re := FFilterFreq[1]^[Half].Re * FSignalFreq[0]^[Half].Re;

   for Bin := 1 to Half - 1
    do ComplexMultiplyInplace(FSignalFreq[0]^[Bin], FFilterFreq[1]^[Bin]);

   FFft.PerformIFFTCCS(PDAVComplexSingleFixedArray(FSignalFreq[0]), @Outputs[1, 0]);

   {$ELSE}{$IFDEF Use_CUDA}

   FFft.PerformFFT(FSignalFreq[0], @Inputs[Channel, 0]);

   // DC & Nyquist
   FSignalFreq[0]^[0].Re := FFilterFreq[0]^[0].Re * FSignalFreq[0]^[0].Re;
   FSignalFreq[0]^[0].Im := FFilterFreq[0]^[0].Im * FSignalFreq[0]^[0].Im;
   FSignalFreq[0]^[Half].Re := FFilterFreq[0]^[Half].Re * FSignalFreq[0]^[Half].Re;

   for Bin := 1 to Half - 1
    do ComplexMultiplyInplace(FSignalFreq[0]^[Bin], FFilterFreq[0]^[Bin]);

   FFft.PerformIFFT(FSignalFreq, @Outputs[Channel, 0]);
   {$ELSE}
   FFft.PerformFFTPackedComplex(FSignalFreq[0], @Inputs[0, 0]);

   // DC & Nyquist
   FSignalFreq[1]^[0].Re := FFilterFreq[0]^[0].Re * FSignalFreq[0]^[0].Re;
   FSignalFreq[1]^[0].Im := FFilterFreq[0]^[0].Im * FSignalFreq[0]^[0].Im;
   FSignalFreq[1]^[Half].Re := FFilterFreq[0]^[Half].Re * FSignalFreq[0]^[Half].Re;

   for Bin := 1 to Half - 1
    do FSignalFreq[1]^[Bin] := ComplexMultiply(FSignalFreq[0]^[Bin], FFilterFreq[0]^[Bin]);

   FFft.PerformIFFTPackedComplex(FSignalFreq[1], @Outputs[0, 0]);

   // DC & Nyquist
   FSignalFreq[0]^[0].Re := FFilterFreq[1]^[0].Re * FSignalFreq[0]^[0].Re;
   FSignalFreq[0]^[0].Im := FFilterFreq[1]^[0].Im * FSignalFreq[0]^[0].Im;
   FSignalFreq[0]^[Half].Re := FFilterFreq[1]^[Half].Re * FSignalFreq[0]^[Half].Re;

   for Bin := 1 to Half - 1
    do ComplexMultiplyInplace(FSignalFreq[0]^[Bin], FFilterFreq[1]^[Bin]);

   FFft.PerformIFFTPackedComplex(FSignalFreq[0], @Outputs[1, 0]);
   {$ENDIF}{$ENDIF}
  end;
 finally
  Dec(FSemaphore);
 end;
end;

procedure TLinearPhaseCrossoverModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := IntToStr(round(Parameter[Index]));
end;

procedure TLinearPhaseCrossoverModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then Predefined := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3);
end;

procedure TLinearPhaseCrossoverModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TLinearPhaseCrossoverModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateFilterKernel;
end;

procedure TLinearPhaseCrossoverModule.CalculateFilterKernel;
var
  i, h, q : Integer;
  n       : Double;
  CutOff  : Double;
begin
 if assigned(FFilterKernel) then
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
      then FFilterKernel^[i] := 2.0 * CutOff
      else
       begin
        n := PI * (i - q);
        FFilterKernel^[i] := sin(2.0 * Cutoff * n) / n;
       end;
    ApplyHanningWindow(FFilterKernel, h);
    FillChar(FFilterKernel^[h], h * SizeOf(Single), 0);

    // calculate frequency
    {$IFDEF Use_IPPS}
    FFft.PerformFFTCCS(FFilterFreq[0], FFilterKernel);
    {$ELSE}{$IFDEF Use_CUDA}
    FFft.PerformFFTCCS(FFilterFreq[0], FFilterKernel);
    {$ELSE}
    FFft.PerformFFTPackedComplex(FFilterFreq[0], FFilterKernel);
    {$ENDIF}{$ENDIF}

    // Generate sinc delayed by (N-1)/2
    for i := 0 to h - 1 do
     if (i = q)
      then FFilterKernel^[i] := 1 - 2.0 * CutOff
      else
       begin
        n := PI * (i - q);
        FFilterKernel^[i] := -sin(2.0 * Cutoff * n) / n;
       end;
    ApplyHanningWindow(FFilterKernel, h);
    FillChar(FFilterKernel^[h], h * SizeOf(Single), 0);

    // calculate frequency
    {$IFDEF Use_IPPS}
    FFft.PerformFFTCCS(FFilterFreq[1], FFilterKernel);
    {$ELSE}{$IFDEF Use_CUDA}
    FFft.PerformFFTCCS(FFilterFreq[1], FFilterKernel);
    {$ELSE}
    FFft.PerformFFTPackedComplex(FFilterFreq[1], FFilterKernel);
    {$ENDIF}{$ENDIF}
   finally
    dec(FSemaphore);
   end;
  end;
end;

procedure TLinearPhaseCrossoverModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 10;
// if assigned(FLinearPhaseCrossover)
//  then FLinearPhaseCrossover.Order := Round(Value);
end;

procedure TLinearPhaseCrossoverModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 CalculateFilterKernel;
end;

{$IFDEF FPC}
initialization
  {$I LinearPhaseCrossoverDM.lrs}
{$ENDIF}

end.
