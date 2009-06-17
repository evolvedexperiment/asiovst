unit DAV_DspCrosstalkSimulatorHRTF;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspCrosstalkSimulator, DAV_DspConvolution,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF} DAV_DspFftReal2Complex, 
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_DspHrtf;

type
  TCustomHrtfCrosstalkSimulator = class(TCustomCrosstalkSimulator)
  private
    FHrtf : THrtfs;
  protected  
    procedure HrtfChanged(Sender: TObject); virtual; abstract;
    procedure ReloadImpulseResponses; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Hrtf: THrtfs read FHrtf;
  end;

  TCustomSimpleHrtfCrosstalkSimulator = class(TCustomHrtfCrosstalkSimulator)
  private
    FConvolution : array [0..1] of TConvolution32;
  protected
    procedure HrtfChanged(Sender: TObject); override;
    procedure SamplerateChanged; override;
    procedure ReloadImpulseResponses; override;
  public
    procedure Process(var Left, Right: Single); overload; override;
    procedure Process(var Left, Right: Double); overload; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

  TCustomCompleteHrtfCrosstalkSimulator = class(TCustomHrtfCrosstalkSimulator)
  private
    FConvolution : array [0..1, 0..1] of TConvolution32;
  protected
    procedure HrtfChanged(Sender: TObject); override;
    procedure SamplerateChanged; override;
    procedure ReloadImpulseResponses; override;
  public
    procedure Process(var Left, Right: Single); overload; override;
    procedure Process(var Left, Right: Double); overload; override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, DAV_Complex;

{ TCustomHrtfCrosstalkSimulator }

constructor TCustomHrtfCrosstalkSimulator.Create;
begin
 inherited;
 FHrtf := THrtfs.Create;
 FHrtf.OnHrtfChanged := HrtfChanged;
end;

destructor TCustomHrtfCrosstalkSimulator.Destroy;
begin
 FreeAndNil(FHrtf);
 inherited;
end;

procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
asm
 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 dec ecx
@Start:
  fld [eax    ].Single  // A.Re
  fld [eax + 4].Single  // A.Im, A.Re
  fld [edx    ].Single  // B.Re, A.Im, A.Re
  fld [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
end;

{ TCustomSimpleHrtfCrosstalkSimulator }

constructor TCustomSimpleHrtfCrosstalkSimulator.Create;
begin
 inherited;
 FConvolution[0] := TConvolution32.Create;
 FConvolution[1] := TConvolution32.Create;
end;

destructor TCustomSimpleHrtfCrosstalkSimulator.Destroy;
begin
 FreeAndNil(FConvolution[0]);
 FreeAndNil(FConvolution[1]);
 inherited;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.HrtfChanged(Sender: TObject);
begin
 ReloadImpulseResponses;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.SamplerateChanged;
begin
 ReloadImpulseResponses;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.ReloadImpulseResponses;
var
  IR  : array [0..1] of PDAVSingleFixedArray;
  Frq : array [0..1] of PDAVComplexSingleFixedArray;
  Sz  : Integer;
  Ord : Integer;
  {$IFDEF Use_IPPS}
  Fft : TFftReal2ComplexIPPSFloat32;
  {$ELSE} {$IFDEF Use_CUDA}
  Fft : TFftReal2ComplexCUDA32;
  {$ELSE}
  Fft : TFftReal2ComplexNativeFloat32;
  {$ENDIF}{$ENDIF}
begin
 Sz := FHrtf.MaximumHrirSize;
 GetMem(IR[0], Sz * SizeOf(Single));
 GetMem(IR[1], Sz * SizeOf(Single));
 try
  // one ear only
  FHrtf.InterpolateHrir(-60, 90, Sz, IR[0], IR[1]);

  // update FFT order
  Ord := CeilLog2(Sz);
  FConvolution[0].FFTOrder := Ord;
  FConvolution[1].FFTOrder := Ord;

  {$IFDEF Use_IPPS}
  Fft := TFftReal2ComplexIPPSFloat32.Create(Ord + 1);
  {$ELSE} {$IFDEF Use_CUDA}
  Fft := TFftReal2ComplexCUDA32.Create(Ord + 1);
  {$ELSE}
  Fft := TFftReal2ComplexNativeFloat32.Create(Ord + 1);
  Fft.DataOrder := doPackedComplex;
  {$ENDIF}{$ENDIF}

  // deconvolution
  GetMem(Frq[0], ((Sz div 2) + 1) * SizeOf(Single));
  GetMem(Frq[1], ((Sz div 2) + 1) * SizeOf(Single));
  try
   Fft.PerformFFT(Frq[0], IR[0]);
   Fft.PerformFFT(Frq[1], IR[1]);

   // perform deconvolution

  finally
   Dispose(Frq[0]);
   Dispose(Frq[1]);
  end;

  // load impulse responses
  FConvolution[0].LoadImpulseResponse(IR[0], sz);
  FConvolution[1].LoadImpulseResponse(IR[0], sz);
 finally
  Dispose(IR[0]);
  Dispose(IR[1]);
 end;
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.Process(var Left, Right: Single);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1].ProcessBlock(@Data[0], @Data[3], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

procedure TCustomSimpleHrtfCrosstalkSimulator.Process(var Left, Right: Double);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1].ProcessBlock(@Data[0], @Data[3], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

{ TCustomCompleteHrtfCrosstalkSimulator }

constructor TCustomCompleteHrtfCrosstalkSimulator.Create;
begin
 inherited;
 FConvolution[0, 0] := TConvolution32.Create;
 FConvolution[1, 0] := TConvolution32.Create;
 FConvolution[1, 0] := TConvolution32.Create;
 FConvolution[1, 1] := TConvolution32.Create;
end;

destructor TCustomCompleteHrtfCrosstalkSimulator.Destroy;
begin
 FreeAndNil(FConvolution[0, 0]);
 FreeAndNil(FConvolution[1, 0]);
 FreeAndNil(FConvolution[1, 0]);
 FreeAndNil(FConvolution[1, 1]);
 inherited;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.HrtfChanged(Sender: TObject);
begin
 ReloadImpulseResponses;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.SamplerateChanged;
begin
 ReloadImpulseResponses;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.ReloadImpulseResponses;
var
  IR : array [0..1, 0..1] of PDAVSingleFixedArray;
  Sz : Integer;
begin
 Sz := FHrtf.MaximumHrirSize;
 GetMem(IR[0, 0], Sz * SizeOf(Single));
 GetMem(IR[0, 1], Sz * SizeOf(Single));
 GetMem(IR[1, 0], Sz * SizeOf(Single));
 GetMem(IR[1, 1], Sz * SizeOf(Single));
 try
  // update FFT order
  FConvolution[0, 0].FFTOrder := CeilLog2(Sz);
  FConvolution[0, 1].FFTOrder := CeilLog2(Sz);
  FConvolution[1, 0].FFTOrder := CeilLog2(Sz);
  FConvolution[1, 1].FFTOrder := CeilLog2(Sz);

  // left ear
  FHrtf.InterpolateHrir(-60, 90, Sz, IR[0, 0], IR[0, 1]);
  FConvolution[0, 0].LoadImpulseResponse(IR[0, 0], sz);
  FConvolution[0, 1].LoadImpulseResponse(IR[0, 1], sz);

  // right ear
  FHrtf.InterpolateHrir(+60, 90, Sz, IR[1, 0], IR[1, 1]);
  FConvolution[1, 0].LoadImpulseResponse(IR[1, 0], sz);
  FConvolution[1, 1].LoadImpulseResponse(IR[1, 1], sz);
 finally
  Dispose(IR[0, 0]);
  Dispose(IR[0, 1]);
  Dispose(IR[1, 0]);
  Dispose(IR[1, 1]);
 end;
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.Process(var Left,
  Right: Single);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0, 1].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1, 1].ProcessBlock(@Data[0], @Data[3], 1);

 // convolve direct
 FConvolution[0, 0].ProcessBlock(@Data[0], @Data[0], 1);
 FConvolution[1, 0].ProcessBlock(@Data[1], @Data[1], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

procedure TCustomCompleteHrtfCrosstalkSimulator.Process(var Left,
  Right: Double);
var
  Data : Array [0..3] of Single;
begin
 Data[0] := Left;
 Data[1] := Right;

 // convolve crosstalk
 FConvolution[0, 1].ProcessBlock(@Data[1], @Data[2], 1);
 FConvolution[1, 1].ProcessBlock(@Data[0], @Data[3], 1);

 // convolve direct
 FConvolution[0, 0].ProcessBlock(@Data[0], @Data[0], 1);
 FConvolution[1, 0].ProcessBlock(@Data[1], @Data[1], 1);

 // mix data
 Left  := Data[0] + Data[2];
 Right := Data[1] + Data[3];
end;

end.
