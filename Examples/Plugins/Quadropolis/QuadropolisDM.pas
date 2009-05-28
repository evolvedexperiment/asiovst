unit QuadropolisDM;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_Complex,
  DAV_DspFftReal2Complex, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_VSTModule,
  DAV_DspHrtf;

type
  TQuadropolisDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    FIR           : array [0..3, 0..1] of PDAVSingleFixedArray;
    FLength       : Integer;
    FHRTFs        : THRTFs;

    FFilterFreq   : array [0..3, 0..1] of PDAVComplexSingleFixedArray;
    FSignalFreq   : array [0..3] of PDAVComplexSingleFixedArray;
    FOutputFreq   : array [0..1] of PDAVComplexSingleFixedArray;
    FSemaphore    : Integer;
    {$IFDEF Use_IPPS}
    FFft          : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft          : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft          : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    procedure CalculateFilterKernel;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_DspWindowing, FmQuadropolis;

procedure TQuadropolisDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore       := 0;
 BlockModeOverlap := BlockModeSize div 2;
end;

procedure TQuadropolisDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
  MemSize : Integer;
  RS      : TResourceStream;
begin
 FLength := BlockModeSize;

 FHRTFs := THRTFs.Create;

 RS := TResourceStream.Create(hInstance, 'Default', 'HRTF');
  try
   FHRTFs.LoadFromStream(RS);
  finally
   RS.Free;
  end;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(round(Log2(FLength)));
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(round(Log2(FLength)));
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(round(Log2(FLength)));
 {$ENDIF}{$ENDIF}

 FFft.AutoScaleType := astDivideInvByN;
 FFft.DataOrder := doPackedComplex;

 for Channel := 0 to Length(FIR) - 1 do
  begin
   {$IFDEF Use_IPPS}
   MemSize := (FLength div 2 + 1) * SizeOf(TComplexSingle);
   {$ELSE} {$IFDEF Use_CUDA}
   MemSize := FLength * SizeOf(Single);
   {$ELSE}
   MemSize := FLength * SizeOf(Single);
   {$ENDIF}{$ENDIF}

   GetMem(FIR[Channel, 0], FLength * SizeOf(Single));
   FillChar(FIR[Channel, 0]^, FLength * SizeOf(Single), 0);

   GetMem(FIR[Channel, 1], FLength * SizeOf(Single));
   FillChar(FIR[Channel, 1]^, FLength * SizeOf(Single), 0);

   GetMem(FFilterFreq[Channel, 0], MemSize);
   FillChar(FFilterFreq[Channel, 0]^[0], MemSize, 0);

   GetMem(FFilterFreq[Channel, 1], MemSize);
   FillChar(FFilterFreq[Channel, 1]^[0], MemSize, 0);

   GetMem(FSignalFreq[Channel], MemSize);
   FillChar(FSignalFreq[Channel]^[0], MemSize, 0);
  end;

 for Channel := 0 to Length(FOutputFreq) - 1 do
  begin
   GetMem(FOutputFreq[Channel], MemSize);
   FillChar(FOutputFreq[Channel]^[0], MemSize, 0);
  end;

 CalculateFilterKernel;
end;

procedure TQuadropolisDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
  for Channel := 0 to Length(FIR) - 1 do
   begin
    Dispose(FFilterFreq[Channel, 0]);
    Dispose(FFilterFreq[Channel, 1]);
    Dispose(FSignalFreq[Channel]);
    Dispose(FIR[Channel, 0]);
    Dispose(FIR[Channel, 1]);
   end;

 FreeAndNil(FFft);
 FreeAndNil(FHRTFs);
end;

procedure TQuadropolisDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TQuadropolisGUI.Create(Self);
end;

procedure TQuadropolisDataModule.CalculateFilterKernel;
begin
 if assigned(FHRTFs) then
  with FHRTFs do
   begin
    InterpolateHrir( -45, 0, FLength div 2, FIR[0, 0], FIR[0, 1]);
    InterpolateHrir( +45, 0, FLength div 2, FIR[1, 0], FIR[1, 1]);
    InterpolateHrir(+135, 0, FLength div 2, FIR[2, 0], FIR[2, 1]);
    InterpolateHrir(-135, 0, FLength div 2, FIR[3, 0], FIR[3, 1]);

    {$IFDEF Use_IPPS}
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[0, 0]), FIR[0, 0]);
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[0, 1]), FIR[0, 1]);
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[1, 0]), FIR[1, 0]);
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[1, 1]), FIR[1, 1]);
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[2, 0]), FIR[2, 0]);
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[2, 1]), FIR[2, 1]);
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[3, 0]), FIR[3, 0]);
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[3, 1]), FIR[3, 1]);
    {$ELSE}{$IFDEF Use_CUDA}
    FFft.PerformFFT(FFilterFreq[0, 0], FIR[0, 0]);
    FFft.PerformFFT(FFilterFreq[0, 1], FIR[0, 1]);
    FFft.PerformFFT(FFilterFreq[1, 0], FIR[1, 0]);
    FFft.PerformFFT(FFilterFreq[1, 1], FIR[1, 1]);
    FFft.PerformFFT(FFilterFreq[2, 0], FIR[2, 0]);
    FFft.PerformFFT(FFilterFreq[2, 1], FIR[2, 1]);
    FFft.PerformFFT(FFilterFreq[3, 0], FIR[3, 0]);
    FFft.PerformFFT(FFilterFreq[3, 1], FIR[3, 1]);
    {$ELSE}
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[0, 0]), FIR[0, 0]);
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[0, 1]), FIR[0, 1]);
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[1, 0]), FIR[1, 0]);
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[1, 1]), FIR[1, 1]);
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[2, 0]), FIR[2, 0]);
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[2, 1]), FIR[2, 1]);
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[3, 0]), FIR[3, 0]);
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[3, 1]), FIR[3, 1]);
    {$ENDIF}{$ENDIF}
   end;
end;

procedure TQuadropolisDataModule.VSTModuleProcess(const Inputs,
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
  {$IFDEF Use_IPPS}

  // transform input channels to frequency domain
  for Channel := 0 to numInputs - 1 do
   begin
    FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FSignalFreq[Channel]), @Inputs[Channel, 0]);

    // DC & Nyquist
    FOutputFreq[0]^[0].Re := FOutputFreq[0]^[0].Re + FFilterFreq[Channel, 0]^[0].Re * FSignalFreq[Channel]^[0].Re;
    FOutputFreq[0]^[0].Im := FOutputFreq[0]^[0].Im + FFilterFreq[Channel, 0]^[0].Im * FSignalFreq[Channel]^[0].Im;
    FOutputFreq[0]^[Half].Re := FOutputFreq[0]^[Half].Re + FFilterFreq[Channel, 0]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

    for Bin := 1 to Half - 1
     do FOutputFreq[0]^[Bin] := ComplexAdd(FOutputFreq[0]^[Bin], ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel, 0]^[Bin]));

    // DC & Nyquist
    FOutputFreq[1]^[0].Re := FOutputFreq[1]^[0].Re + FFilterFreq[Channel, 1]^[0].Re * FSignalFreq[Channel]^[0].Re;
    FOutputFreq[1]^[0].Im := FOutputFreq[1]^[0].Im + FFilterFreq[Channel, 1]^[0].Im * FSignalFreq[Channel]^[0].Im;
    FOutputFreq[1]^[Half].Re := FOutputFreq[1]^[Half].Re + FFilterFreq[Channel, 1]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

    for Bin := 1 to Half - 1
     do FOutputFreq[1]^[Bin] := ComplexAdd(FOutputFreq[1]^[Bin], ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel, 1]^[Bin]));
   end;

  for Channel := 0 to numOutputs - 1 do
   begin
    FFft.PerformIFFTCCS(PDAVComplexSingleFixedArray(FOutputFreq[Channel]), @Outputs[Channel, 0]);
    FillChar(FOutputFreq[Channel]^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplexSingle), 0);
   end;

  {$ELSE}{$IFDEF Use_CUDA}

  // transform input channels to frequency domain
  for Channel := 0 to numInputs - 1 do
   begin
    FFft.PerformFFT(FSignalFreq[Channel], @Inputs[Channel, 0]);

    // DC & Nyquist
    FOutputFreq[0]^[0].Re := FOutputFreq[0]^[0].Re + FFilterFreq[Channel, 0]^[0].Re * FSignalFreq[Channel]^[0].Re;
    FOutputFreq[0]^[0].Im := FOutputFreq[0]^[0].Im + FFilterFreq[Channel, 0]^[0].Im * FSignalFreq[Channel]^[0].Im;
    FOutputFreq[0]^[Half].Re := FOutputFreq[0]^[Half].Re + FFilterFreq[Channel, 0]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

    for Bin := 1 to Half - 1
     do FOutputFreq[0]^[Bin] := ComplexAdd(FOutputFreq[0]^[Bin], ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel, 0]^[Bin]));

    // DC & Nyquist
    FOutputFreq[1]^[0].Re := FOutputFreq[1]^[0].Re + FFilterFreq[Channel, 1]^[0].Re * FSignalFreq[Channel]^[0].Re;
    FOutputFreq[1]^[0].Im := FOutputFreq[1]^[0].Im + FFilterFreq[Channel, 1]^[0].Im * FSignalFreq[Channel]^[0].Im;
    FOutputFreq[1]^[Half].Re := FOutputFreq[1]^[Half].Re + FFilterFreq[Channel, 1]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

    for Bin := 1 to Half - 1
     do FOutputFreq[1]^[Bin] := ComplexAdd(FOutputFreq[1]^[Bin], ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel, 1]^[Bin]));
   end;  

  for Channel := 0 to numOutputs - 1 do
   begin
    FFft.PerformIFFT(FOutputFreq[Channel], @Outputs[Channel, 0]);
    FillChar(FOutputFreq[Channel]^[0], BlockModeSize * SizeOf(Single), 0);
   end;
  {$ELSE}

  // transform input channels to frequency domain
  for Channel := 0 to numInputs - 1 do
   begin
    FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FSignalFreq[Channel]), @Inputs[Channel, 0]);

    // DC & Nyquist
    FOutputFreq[0]^[0].Re := FOutputFreq[0]^[0].Re + FFilterFreq[Channel, 0]^[0].Re * FSignalFreq[Channel]^[0].Re;
    FOutputFreq[0]^[0].Im := FOutputFreq[0]^[0].Im + FFilterFreq[Channel, 0]^[0].Im * FSignalFreq[Channel]^[0].Im;
    FOutputFreq[0]^[Half].Re := FOutputFreq[0]^[Half].Re + FFilterFreq[Channel, 0]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

    for Bin := 1 to Half - 1
     do FOutputFreq[0]^[Bin] := ComplexAdd(FOutputFreq[0]^[Bin], ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel, 0]^[Bin]));

    // DC & Nyquist
    FOutputFreq[1]^[0].Re := FOutputFreq[1]^[0].Re + FFilterFreq[Channel, 1]^[0].Re * FSignalFreq[Channel]^[0].Re;
    FOutputFreq[1]^[0].Im := FOutputFreq[1]^[0].Im + FFilterFreq[Channel, 1]^[0].Im * FSignalFreq[Channel]^[0].Im;
    FOutputFreq[1]^[Half].Re := FOutputFreq[1]^[Half].Re + FFilterFreq[Channel, 1]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

    for Bin := 1 to Half - 1
     do FOutputFreq[1]^[Bin] := ComplexAdd(FOutputFreq[1]^[Bin], ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel, 1]^[Bin]));
   end;

  for Channel := 0 to numOutputs - 1 do
   begin
    FFft.PerformIFFTPackedComplex(PDAVComplexSingleFixedArray(FOutputFreq[Channel]), @Outputs[Channel, 0]);
    FillChar(FOutputFreq[Channel]^[0], BlockModeSize * SizeOf(Single), 0);
   end;
  {$ENDIF}{$ENDIF}
 finally
  dec(FSemaphore);
 end;
end;

procedure TQuadropolisDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 CalculateFilterKernel;
end;

end.