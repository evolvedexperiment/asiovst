unit DAV_DspChebyshevFilter;

interface

{$I ..\ASIOVST.INC}

uses
  DAV_DspFilter, DAV_Common;

type
  TChebyshev1Filter = class(TIIRFilter)
  private
    function GetRipple: Double;
    procedure SetDownsamplePower(Value: Integer);
  protected
    fRipple         : Double;
    fRippleFactors  : T2DoubleArray;
    fDownsamplePow  : Integer;
    fDownsampleFak  : Integer;
    fOrder          : Integer;
    fCoeffs         : array [0..127] of Double;
    fState          : array [0.. 63] of Double;
    procedure SetW0; override;
    procedure SetOrder(Value: Integer); override;
    procedure SetGain(const Value: Double); override;
    procedure SetRipple(const Value: Double); virtual;
    procedure SetFrequency(const Value: Double); override;
    procedure SetSampleRate(const Value: Double); override;
    procedure SetRippleFactors; virtual;
    function GetOrder: Integer; override;
  public
    constructor Create; override;
    procedure SetFilterValues(const AFrequency, AGain, ARipple : Single); virtual;
    function MagnitudeSquared(Frequency:Double):Double; override;
    function MagnitudeLog10(Frequency:Double):Double; override;
    procedure ResetStates; override;
    procedure Reset; override;
    property Ripple : Double read GetRipple write SetRipple;
    property DownsampleAmount : Integer read fDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read fDownsampleFak;
  end;

  TChebyshev1LP = class(TChebyshev1Filter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input:Double):Double; override;
    function MagnitudeSquared(Frequency:Double):Double; override;
    function MagnitudeLog10(Frequency:Double):Double; override;
  end;

  TChebyshev1HP = class(TChebyshev1Filter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input:Double):Double; override;
    function MagnitudeSquared(Frequency:Double):Double; override;
    function MagnitudeLog10(Frequency:Double):Double; override;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, SysUtils;

constructor TChebyshev1Filter.Create;
begin
 fDownsamplePow := 0;
 fDownsampleFak := 1;
 fFrequency := 0;
 fGain := 0;
 fRipple := 1;
 fOrder := 10;
 SampleRate := 44100;
end;

function TChebyshev1Filter.GetOrder: Integer;
begin
 Result := fOrder;
end;

function TChebyshev1Filter.GetRipple: Double;
begin
 Result := fRipple;
end;

procedure TChebyshev1Filter.Reset;
begin
 fGain := 0;
 CalculateCoefficients;
end;

procedure TChebyshev1Filter.ResetStates;
begin
 FillChar(fState[0], fOrder * SizeOf(Double), 0);
end;

procedure TChebyshev1Filter.SetSampleRate(const Value: Double);
begin
 if Value = 0 then Exit;
 if Value <> fSampleRate then
  begin
   fSampleRate := Value;
   fSRR := 1 / fSampleRate;
  end;
end;

procedure TChebyshev1Filter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if fDownsamplePow <> Value then
  begin
   fDownsamplePow := Value;
   fDownsampleFak := round(IntPower(2, fDownsamplePow));
   SetW0;
  end;
end;

procedure TChebyshev1Filter.SetW0;
begin
 fW0    := 2 * Pi * fSRR * (fFrequency * fDownsampleFak);
 fSinW0 := sin(fW0);
 if fW0 > 3.1 then fW0 := 3.1;
end;

procedure TChebyshev1Filter.SetGain(const Value: Double);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 fGain := Value;
 fGainSpeed := Exp(fGain * ln10_0025);
end;

procedure TChebyshev1Filter.SetOrder(Value: Integer);
begin
 fOrder := Value;
 SetRippleFactors;
 CalculateCoefficients;
end;

procedure TChebyshev1Filter.SetFrequency(const Value: Double);
begin
 if fFrequency <> Value then
  begin
   fFrequency := Value;
   SetW0;
   SetRippleFactors;
   CalculateCoefficients;
  end;
end;

procedure TChebyshev1Filter.SetRipple(const Value: Double);
begin
 if Value <> fRipple then
  begin
   fRipple := Value;
   SetRippleFactors;
   CalculateCoefficients;
  end;
end;

procedure TChebyshev1Filter.SetRippleFactors;
var
  t : Double;
begin
 if fOrder > 0 then
  begin
   t := arcsinh(1 / sqrt(Power(10, (fRipple * 0.1)) - 1)) / fOrder;
   fRippleFactors[1] := sinh(t);
   fRippleFactors[0] := sqr(cosh(t));
  end;
 ResetStates;
end;

procedure TChebyshev1Filter.SetFilterValues(const AFrequency, AGain, ARipple : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 fFrequency := AFrequency;
 fGain      := AGain;
 fRipple    := ARipple;
 fGainSpeed := Exp((fGain * ln10_0025));
 SetW0;
 SetRippleFactors;
end;

function TChebyshev1Filter.MagnitudeSquared(Frequency: Double): Double;
begin
 Result := 1;
end;

function TChebyshev1Filter.MagnitudeLog10(Frequency: Double): Double;
begin
 result := 10 * Log10(MagnitudeSquared(Frequency));
end;

{ TChebyshev1FilterLP }

constructor TChebyshev1LP.Create;
begin
 inherited Create;
 fGainSpeed := 1;
end;

procedure TChebyshev1LP.CalculateCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
begin
 K  := tan(fW0 * 0.5);
 K2 := sqr(K);
 for i := (fOrder div 2) - 1 downto 0 do
  begin
   t  := cos(((i * 2 + 1) * Pi * 0.05));
   t1 := 1 / (fRippleFactors[0] - sqr(t));
   t2 := K * t1 * fRippleFactors[1] * (2 * t);
   t  := 1 / (t2 + K2 + t1);
   fCoeffs[4 * i    ] := K2 * t;
   fCoeffs[4 * i + 1] := 2 * fCoeffs[4 * i];
   fCoeffs[4 * i + 2] := 2 * (-K2 + t1) * t;
   fCoeffs[4 * i + 3] :=   (-K2 - t1 + t2) * t;
  end;
 fCoeffs[0] := fCoeffs[0] * fGainSpeed;
 fCoeffs[1] := fCoeffs[1] * fGainSpeed;
{$ELSE}
const
  chalf : Double = 0.5;
asm
 mov ecx, [self.fOrder]                     // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz @done                                   // exit if filter order = 0
 shr ecx, 1                                 // ecx = order div 2
 test ecx, ecx                              // set flags according to ecx
 jz @done                                   // exit if filter order = 0

 fld1                                       // 1
 fild [self.fOrder]                         // fOrder, 1
 fadd st(0), st(0)                          // 2 * fOrder, 1
 fdivp                                      // 1 / 2 * fOrder

 fld [self.fW0]                             // fW0, 1 / 2*fOrder
 fmul chalf                                 // fW0 / 2, 1 / 2 * fOrder
 fsincos                                    // sin(fW0/2), cos(fW0/2), 1/2*fOrder
 fdivp                                      // K = tan(fW0*0.5), 1/2*fOrder
 fld st(0)                                  // K, K, 1/2*fOrder
 fmul st(0), st(0)                          // K², K, 1/2*fOrder
 fxch                                       // K, K², 1/2*fOrder

 @OrderLoop:
  mov edx, ecx                              // edx = i
  imul edx, 2                               // edx = 2 * i
  dec edx                                   // edx = 2 * i + 1
  mov [esp - 4], edx                        // edx to stack
  dec edx                                   // edx = 2 * i
  shl edx, 1                                // edx = 4 * i
  fild [esp - 4].Integer                    // edx in st(0) = 2*i+1, K, K², 1/2*fOrder
  fldpi                                     // Pi, 2*i+1, K, K², 1/2*fOrder
  fmulp                                     // Pi * (2*i+1), K, K², 1/2*fOrder
  fmul st(0), st(3)                         // Pi * (2*i+1)/(2*Order), K, K², 1/2*fOrder
  fcos                                      // cos((i*2+1)*Pi/(2*Order)) = t, K, K², 1/2*fOrder
  fld st(0)                                 // t, t, K, K², 1/2*fOrder
  fmul st(0), st(0)                         // t²,t, K, K², 1/2*fOrder
  fld [self.fRippleFactors].Double          // fRipple[0], t²,t, K, K², 1 / 2 * fOrder
  fsubrp                                    // fRipple[0] - t²,t, K, K², 1 / 2 * fOrder
  fld1                                      // 1, fRipple[0] - t²,t, K, K², 1 / 2 * fOrder
  fdivrp                                    // 1 / (fRipple[0] - t²) = t1, t, K, K², 1 / 2 * fOrder
  fxch                                      // t, t1, K, K², 1 / 2 * fOrder
  fadd st(0), st(0)                         // 2*t, t1, K, K², 1 / 2 * fOrder
  fmul st(0), st(1)                         // 2*t * t1, t1, K, K², 1 / 2 * fOrder
  fmul [self.fRippleFactors + 8].Double     // fRipple[1]*2*t*t1, t1, K, K², 1 / 2 * fOrder
  fmul st(0), st(2)                         // K*fRipple[1]*2*t*t1 = t2, t1, K, K², 1 / 2 * fOrder
  fld st(0)                                 // t2, t2, t1, K, K², 1 / 2 * fOrder
  fadd st(0), st(2)                         // t1+t2, t2, t1, K, K², 1 / 2 * fOrder
  fadd st(0), st(4)                         // t1+t2+K², t2, t1, K, K², 1 / 2 * fOrder
  fld1                                      // 1, t1 + t2 + K², t2, t1, K, K², 1 / 2 * fOrder
  fdivrp                                    // (1 / t1 + t2 + K²) = t, t2, t1, K, K², 1 / 2 * fOrder
  fld st(0)                                 // t, t, t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(5)                          // t*K²=fA[2*i], t, t2, t1, K, K², 1/2*fOrder
  fst [Self.fCoeffs + 8 * edx].Double       // store to fA[2*i], 1/2*fOrder
  fadd st(0),st(0)                          // 2*fA[2*i], t, t2, t1, K, K², 1/2*fOrder
  fstp [self.fCoeffs + 8 * edx + 8].Double  // store to fA[2*i+1], 1/2*fOrder

  fld st(2)                                 // t1, t, t2, t1, K, K², 1/2*fOrder
  fsub st(0), st(5)                         // t1-K², t, t2, t1, K, K², 1/2*fOrder
  fadd st(0), st(0)                         // 2*(t1-K²), t, t2, t1, K, K², 1/2*fOrder
  fmul st(0), st(1)                         // 2*(t1-K²)*t, t, t2, t1, K, K², 1/2*fOrder
  fstp [self.fCoeffs + 8 * edx + 16].Double // store to fB[2*i], 1/2*fOrder
  fxch                                      // t2, t, t1, K, K², 1/2*fOrder
  fsubrp st(2), st(0)                       // t, t2-t1, K, K², 1/2*fOrder
  fxch                                      // t2-t1, t, K, K², 1/2*fOrder
  fsub st(0), st(3)                         // t2-t1-K², t, K, K², 1/2*fOrder
  fmulp                                     // (t2-t1-K²) * t, K, K², 1/2*fOrder
  fstp [self.fCoeffs + 8 * edx + 24].Double // store to fB[2*i+1], 1/2*fOrder
 loop @OrderLoop
 fstp st(0)                                 // K², 1 / 2 * fOrder
 fstp st(0)                                 // 1 /  2 * fOrder
 fstp st(0)                                 // stack free!

 fld  [self.fCoeffs].Double                 // load fA[0]
 fmul [self.fGainSpeed].Double              // apply fGainSpeed
 fstp [self.fCoeffs].Double                 // store fA[0]
 fld  [self.fCoeffs + 8].Double             // load fA[1]
 fmul [self.fGainSpeed].Double              // apply fGainSpeed
 fstp [self.fCoeffs + 8].Double             // store fA[1]
@done:
{$ENDIF}
end;

function TChebyshev1LP.MagnitudeSquared(Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw     := 2 * cos(Frequency * pi * fSRR);
 a      := sqr(cw - 2);
 Result := 1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fCoeffs[4 * i]) * a / (1 + sqr(fCoeffs[4 * i + 2]) + sqr(fCoeffs[4 * i + 3]) + 2 * fCoeffs[4 * i + 3] + cw*((fCoeffs[4 * i + 2] - cw) * fCoeffs[4 * i + 3] - fCoeffs[4 * i + 2]));
end;

function TChebyshev1LP.MagnitudeLog10(Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw     := 2 * cos(Frequency * Pi * fSRR);
 a      := sqr(cw - 2);
 Result := 1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fCoeffs[4 * i]) * a / (1 + sqr(fCoeffs[4 * i + 2]) + sqr(fCoeffs[4 * i + 3]) + 2 * fCoeffs[4 * i + 3] + cw * ((fCoeffs[4 * i + 2] - cw) * fCoeffs[4 * i + 3] - fCoeffs[4 * i + 2]));
 Result := 10 * Log10(Result);
end;

function TChebyshev1LP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := Input;
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x := Result;
   Result            := fCoeffs[4 * i    ] * x                               + fState[2 * i    ];
   fState[2 * i    ] := fCoeffs[4 * i + 1] * x + fCoeffs[4 * i + 2] * Result + fState[2 * i + 1];
   fState[2 * i + 1] := fCoeffs[4 * i    ] * x + fCoeffs[4 * i + 3] * Result;
  end;
 if (fOrder mod 2) = 1 then
  begin
   i := ((fOrder + 1) div 2) - 1;
   x             := fCoeffs[4 * i] * Result;
   Result        := x + fState[2 * i];
   fState[2 * i] := x + fCoeffs[4 * i + 2] * Result;
  end;
{$ELSE}
asm
 fld Input.Double;
 mov ecx, [self.fOrder]
 test ecx, ecx
 jz @End
 shr ecx, 1
 shl ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub ecx, 4
  fld st(0)
  fmul [self.fCoeffs + ecx * 8].Double
  fadd [self.fState + ecx * 4].Double
  fld st(0)
  fld st(0)
  fmul [self.fCoeffs + ecx * 8 + 16].Double
  fadd [self.fState + ecx * 4 + 8].Double
  fld st(3)
  fmul [self.fCoeffs + ecx * 8 + 8].Double
  faddp
  fstp [self.fState + ecx * 4].Double
  fmul [self.fCoeffs + ecx * 8 + 24].Double
  fxch
  fxch st(2)
  fmul [self.fCoeffs + ecx * 8].Double
  faddp
  fstp [self.fState + ecx * 4 + 8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [self.fOrder]
 jz @End
  mov ecx, [self.fOrder]
  dec ecx
  shl ecx, 1
  fmul [self.fCoeffs + ecx * 8].Double
  fld st(0)
  fadd [self.fState + ecx * 4].Double
  fld st(0)
  fmul [self.fCoeffs + ecx * 8 + 16].Double
  faddp st(2), st(0)
  fxch
  fstp [self.fState + ecx * 4].Double
 @End:
 {$ENDIF}
end;

{ TChebyshev1FilterHP }

constructor TChebyshev1HP.Create;
begin
 inherited Create;
 fGainSpeed := 1;
end;

procedure TChebyshev1HP.CalculateCoefficients;
{$IFNDEF PUREPASCAL}
const chalf : Double = 0.5;
asm
 fld1
 fild [self.fOrder]
 fadd st(0),st(0)                     // fOrder, 1
 fadd st(0),st(0)                     // 2*fOrder, 1
 fdivp                                // 4*fOrder, 1
                                      // 1/2*fOrder
 fld [self.fW0]
 fmul chalf                           // fW0, 1/2*fOrder
 fsincos                              // fW0/2, 1/2*fOrder
 fdivp                                // sin(fW0/2), cos(fW0/2), 1/2*fOrder
 fld st(0)                            // K = tan(fW0*0.5), 1/2*fOrder
 fmul st(0),st(0)                     // K, K, 1/2*fOrder
 fxch                                 // K², K, 1/2*fOrder
                                      // K, K², 1/2*fOrder
 mov ecx,[self.fOrder]
 @OrderLoop:                          // ecx = order
  mov edx,ecx                      
  dec edx                             // edx = 2*i
  mov [esp-4],edx                     // edx = 2*i+1
  dec edx                             // edx to stack
  shl edx,1                           // edx = 2*i
  fild [esp-4].Integer                // edx = 4*i
  fldpi                               // edx in st(0) = 2*i+1, K, K²
  fmulp                               // Pi, 2*i+1, K, K², 1/2*fOrder
  fmul st(0),st(3)                    // Pi * (2*i+1), K, K², 1/2*fOrder
  fsin                                // Pi * (2*i+1) / (4*Order), K, K², 1/2*fOrder
  fmul st(0),st(0)                    // sin((i*2+1)*Pi/(2*Order)) = t, K, K², 1/2*fOrder
                                      // sqr(sin((i*2+1)*Pi*0.025)), K, K², 1/2*fOrder
  fld st(0)                        
  fmul st(0),st(0)                    // t, t, K, K², 1/2*fOrder
  fld st(1)                           // t²,t, K, K², 1/2*fOrder
  fsubrp                              // t, t²,t, K, K², 1/2*fOrder
  fadd st(0),st(0)                    // t-t²,t, K, K², 1/2*fOrder
  fadd st(0),st(0)                    // 2*(t-t²),t, K, K², 1/2*fOrder
  fld [self.fRippleFactors].Double    // 4*(t-t²),t, K, K², 1/2*fOrder
  faddp                               // fRipple[0], 4*t²,t, K, K², 1/2*fOrder// fRipple[0] - 4*t²,t, K, K², 1/2*fOrder
  fld1                                // 1, fRipple[0]+4*t-4*t²,t, K, K², 1/2*fOrder
  fsub st(1),st(0)                    // 1, fRipple[0]+4*t-4*t²-1,t, K, K², 1/2*fOrder
  fdivrp                              // 1 / (fRipple[0]+4*t-4*t²-1) = t1, t, K, K², 1/2*fOrder

  fxch                                // t, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                    // 2*t, t1, K, K², 1/2*fOrder
  fld1                                // 1, 2*t, t1, K, K², 1/2*fOrder
  fsubrp                              // 1 - 2*t, t1, K, K², 1/2*fOrder
  fmul [self.fRippleFactors+8].Double // fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fmul st(0),st(2)                    // K*fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fmul st(0),st(1)                    // t1*K*fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                    // t2=2*t1*K*fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fld st(1)                           // t1, t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(4)                    // t1*K², t2, t1, K, K², 1/2*fOrder
  fld1                                // 1, t1*K², t2, t1, K, K², 1/2*fOrder
  faddp                               // 1+t1*K², t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(1)                    // 1+t1*K²+t2, t2, t1, K, K², 1/2*fOrder
  fld1                                // 1, 1+t1*K²+t2, t2, t1, K, K², 1/2*fOrder
  fdivrp                              // 1/(1+t1*K²+t2)=A[2*i], t2, t1, K, K², 1/2*fOrder
  fst [Self.fCoeffs+8*edx].Double         // store to fA[2*i]

  fld st(0)                           // A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                    // 2*A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fchs                                // -2*A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fstp [self.fCoeffs+8*edx+8].Double       // store to fA[2*i+1]

  fld st(2)                           // t1, A[2*i], t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(5)                    // t1*K², A[2*i], t2, t1, K, K², 1/2*fOrder
  fld1                                // 1, t1*K², A[2*i], t2, t1, K, K², 1/2*fOrder
  fsubrp                              // 1-t1*K², A[2*i], t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                    // 2*(1-t1*K²), A[2*i], t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(1)                    // 2*(1-t1*K²)*A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fstp [self.fCoeffs+8*edx+16].Double     // store to fB[2*i]

  fxch st(2)                          // t1, t2, A[2*i], K, K², 1/2*fOrder
  fmul st(0), st(4)                   // t1*K², t2, A[2*i], K, K², 1/2*fOrder
  fld1                                // 1, t1*K², t2, A[2*i], K, K², 1/2*fOrder
  faddp                               // 1 + t1*K², t2, A[2*i], K, K², 1/2*fOrder
  fsubp                               // t2 - (1 + t1*K²), A[2*i], K, K², 1/2*fOrder
  fmulp                               // (t2 - (1 + t1*K²)) * A[2*i], K, K², 1/2*fOrder
  fstp [self.fCoeffs+8*edx+24].Double     // store to fB[2*i+1], 1/2*fOrder
  sub ecx,2
 jnz @OrderLoop
 fstp st(0)                           // K², 1/2*fOrder
 fstp st(0)                           // 1/2*fOrder
 fstp st(0)                           // stack free!

 fld [self.fCoeffs].Double                // load fA[0]
 fmul [self.fGainSpeed].Double        // apply fGainSpeed
 fstp [self.fCoeffs].Double               // store fA[0]
 fld [self.fCoeffs+8].Double              // load fA[1]
 fmul [self.fGainSpeed].Double        // apply fGainSpeed
 fstp [self.fCoeffs+8].Double             // store fA[1]
end;
{$ELSE}
var
  K         : Double;
  t, t1, t2 : Double;
  i         : Integer;
begin
 K := tan(fW0 * 0.5);
 for i := (fOrder div 2) - 1 downto 0 do
  begin
   t  := sqr( sin( ((i * 2 + 1) * Pi / (4 * fOrder)) ) );
   t1 := 1 / (fRippleFactors[0] + 4 * t - 4 * sqr(t)-1);
   t2 := 2 * K * t1 * fRippleFactors[1] * (1 - 2 * t);
   fCoeffs[4 *i    ] := 1 / (t2 + 1 + t1 * sqr(K));
   fCoeffs[4 *i + 1] :=-2 * fCoeffs[4 * i];
   fCoeffs[4 *i + 2] := 2 * (     1 - t1 * sqr(K)) * fCoeffs[4*i];
   fCoeffs[4 *i + 3] :=     (t2 - 1 - t1 * sqr(K)) * fCoeffs[4*i];
  end;
 fCoeffs[0] := fCoeffs[0] * fGainSpeed;
 fCoeffs[1] := fCoeffs[1] * fGainSpeed;
end;
{$ENDIF}

function TChebyshev1HP.MagnitudeSquared(Frequency:Double):Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw := 2 * cos(Frequency * Pi * fSRR);
 a  := sqr(cw - 2);
 Result := 1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fCoeffs[4 * i] * a / (1 + sqr(fCoeffs[4 * i + 2]) + sqr(fCoeffs[4 * i + 3]) + 2 * fCoeffs[4 * i + 3] + cw * ((fCoeffs[4 * i + 2] - cw) * fCoeffs[4 * i + 3] - fCoeffs[4 * i + 2])));
end;

function TChebyshev1HP.MagnitudeLog10(Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * cos(Frequency * pi * fSRR);
 a  := sqr(cw - 2);
 Result:=1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fCoeffs[4 * i]) * a / (1 + sqr(fCoeffs[4 * i + 2])+sqr(fCoeffs[4*i+3])+2*fCoeffs[4*i+3]+cw*((fCoeffs[4*i+2]-cw)*fCoeffs[4*i+3]-fCoeffs[4*i+2]));
 Result := 10 * log10(Result);
end;

function TChebyshev1HP.ProcessSample(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 mov ecx, [self.fOrder]
 shl ecx, 1
 fld Input.Double;
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.fCoeffs+ecx*8].Double
  fadd [self.fState+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.fCoeffs+ecx*8+16].Double
  fadd [self.fState+ecx*4+8].Double
  fld st(3)
  fmul [self.fCoeffs+ecx*8+8].Double
  faddp
  fstp [self.fState + ecx*4].Double
  fmul [self.fCoeffs + ecx * 8 + 24].Double
  fxch
  fxch st(2)
  fmul [self.fCoeffs + ecx * 8].Double
  faddp
  fstp [self.fState + ecx * 4 + 8].Double
 jnz @FilterLoop
end;
{$ELSE}
var
  x : Double;
  i : Integer;
begin
 Result:=Input;
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x:=Result;
   Result          := fCoeffs[4 * i    ] * x                          + fState[2 * i];
   fState[2 * i    ] := fCoeffs[4 * i + 1] * x + fCoeffs[4 * i + 2]*Result  + fState[2 * i + 1];
   fState[2 * i + 1] := fCoeffs[4 * i    ] * x + fCoeffs[4 * i + 3]*Result;
  end;
end;
{$ENDIF}

end.
