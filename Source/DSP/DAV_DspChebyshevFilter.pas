unit DAV_DspChebyshevFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspFilter;

type
  TCustomChebyshevFilter = class(TCustomOrderFilter)
  private
    function GetRipple: Double;
  protected
    FRipple         : Double;
    FRippleFactors  : TDAV2DoubleArray;
    procedure SetRipple(const Value: Double); virtual;
    procedure CalculateRippleFactors; virtual; abstract;
  public
    constructor Create(const Order: Integer = 0); reintroduce; virtual;
    property Ripple : Double read GetRipple write SetRipple;
  end;

  TCustomChebyshev1Filter = class(TCustomChebyshevFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FFilterGain     : Double;
    FCoeffs         : array [0..127] of Double;
    FState          : array [0.. 63] of Double;
    procedure CalculateW0; override;
    procedure CalculateRippleFactors; override;
    procedure OrderChanged; override;
    class function GetMaxOrder: Cardinal; override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure SetFilterValues(const AFrequency, AGain, ARipple : Single); virtual;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure ResetStates; override;
    procedure Reset; override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

  TChebyshev1LP = class(TCustomChebyshev1Filter)
  public
    constructor Create(const Order: Integer = 0); override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  end;

  TChebyshev1HP = class(TCustomChebyshev1Filter)
  public
    constructor Create(const Order: Integer = 0); override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  end;

  TCustomChebyshev2Filter = class(TCustomChebyshevFilter)
  protected
    FOrder          : Integer;
    FCoeffs         : array [0..127] of Double;
    FState          : array [0.. 63] of Double;
    procedure CalculateW0; override;
    procedure OrderChanged; override;
    procedure CalculateRippleFactors; override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure SetFilterValues(const AFrequency, AGain, ARipple : Single); virtual;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure ResetStates; override;
    procedure Reset; override;
  end;

(*
  TChebyshev2LP = class(TCustomChebyshev1Filter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  end;

  TChebyshev2HP = class(TCustomChebyshev1Filter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  end;
*)

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, SysUtils;

const
  CHalf32 : Single = 0.5;
  CHalf64 : Double = 0.5;

{ TCustomChebyshevFilter }

constructor TCustomChebyshevFilter.Create(const Order: Integer = 0);
begin
 inherited Create;
 FOrder := Order;
 FRipple := 1;
end;

function TCustomChebyshevFilter.GetRipple: Double;
begin
 Result := FRipple;
end;

procedure TCustomChebyshevFilter.SetRipple(const Value: Double);
begin
 if Value <> FRipple then
  begin
   FRipple := Value;
   CalculateRippleFactors;
   CalculateCoefficients;
  end;
end;

{ TCustomChebyshev1Filter }

constructor TCustomChebyshev1Filter.Create;
begin
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 FFilterGain := 1;
 inherited;
end;

procedure TCustomChebyshev1Filter.CalculateW0;
begin
 FW0    := 2 * Pi * FSRR * (FFrequency * FDownsampleFak);
 fSinW0 := sin(fW0);
 if fW0 > 3.14 then fW0 := 3.14;
end;

class function TCustomChebyshev1Filter.GetMaxOrder: Cardinal;
begin
 result := 32;
end;

procedure TCustomChebyshev1Filter.Reset;
begin
 Gain := 0;
end;

procedure TCustomChebyshev1Filter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomChebyshev1Filter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TCustomChebyshev1Filter.CalculateRippleFactors;
var
  t : Double;
begin
 if FOrder > 0 then
  begin
   t := arcsinh(1 / sqrt(Power(10, (FRipple * 0.1)) - 1)) / FOrder;
   FRippleFactors[1] := sinh(t);
   FRippleFactors[0] := sqr(cosh(t));
  end;
 ResetStates;
end;

procedure TCustomChebyshev1Filter.SetFilterValues(const AFrequency, AGain, ARipple : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency  := AFrequency;
 FGain_dB    := AGain;
 FRipple     := ARipple;
 CalculateW0;
 CalculateGainFactor;
 CalculateRippleFactors;
end;

function TCustomChebyshev1Filter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

procedure TCustomChebyshev1Filter.OrderChanged;
begin
 CalculateRippleFactors;
 inherited;
end;

function TCustomChebyshev1Filter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 10 * Log10(MagnitudeSquared(Frequency));
end;

{ TChebyshev1FilterLP }

constructor TChebyshev1LP.Create(const Order: Integer = 0);
begin
 inherited;
 CalculateCoefficients;
end;

procedure TChebyshev1LP.CalculateCoefficients;
{$IFNDEF XPUREPASCAL}
var
  Pi2ndOrder : Double;
  K, K2      : Double;
  t, t1, t2  : Double;
  i          : Integer;
begin
 Pi2ndOrder := Pi / (2 * FOrder);
 K  := tan(FW0 * 0.5);
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 for i := (FOrder div 2) - 1 downto 0 do
  begin
   t  := cos( (i * 2 + 1) * Pi2ndOrder );
   t1 := 1 / (FRippleFactors[0] - sqr(t));
   t2 := K * t1 * FRippleFactors[1] * (2 * t);
   t  := 1 / (t2 + K2 + t1);

   FFilterGain := FFilterGain * K2 * t;
   FCoeffs[4 * i + 2] := 2 * (-K2 + t1) * t;
   FCoeffs[4 * i + 3] :=   (-K2 - t1 + t2) * t;
  end;
{$ELSE}
asm
 mov ecx, [self.FOrder]                     // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz @done                                   // exit if filter order = 0
 shr ecx, 1                                 // ecx = order div 2
 test ecx, ecx                              // set flags according to ecx
 jz @done                                   // exit if filter order = 0

 fld  [self.FGainFactor].Double
 fstp [self.FFilterGain].Double

 fld1                                       // 1
 fild [self.FOrder]                         // FOrder, 1
 fadd st(0), st(0)                          // 2 * FOrder, 1
 fdivp                                      // FOrder / 2

 fld [self.fW0]                             // fW0, 1 / 2 * FOrder
 fmul CHalf32                               // fW0 / 2, 1 / 2 * FOrder
 fsincos                                    // sin(fW0/2), cos(fW0/2), 1/2*FOrder
 fdivp                                      // K = tan(fW0*0.5), 1/2*FOrder
 fld st(0)                                  // K, K, 1/2*FOrder
 fmul st(0), st(0)                          // K², K, 1/2*FOrder
 fxch                                       // K, K², 1/2*FOrder

 @OrderLoop:
  mov edx, ecx                              // edx = i
  imul edx, 2                               // edx = 2 * i
  dec edx                                   // edx = 2 * i + 1
  mov [esp - 4], edx                        // edx to stack
  dec edx                                   // edx = 2 * i
  shl edx, 1                                // edx = 4 * i
  fild [esp - 4].Integer                    // edx in st(0) = 2*i+1, K, K², 1/2*FOrder
  fldpi                                     // Pi, 2*i+1, K, K², 1/2*FOrder
  fmulp                                     // Pi * (2*i+1), K, K², 1/2*FOrder
  fmul st(0), st(3)                         // Pi * (2*i+1)/(2*Order), K, K², 1/2*FOrder
  fcos                                      // cos((i*2+1)*Pi/(2*Order)) = t, K, K², 1/2*FOrder
  fld st(0)                                 // t, t, K, K², 1/2*FOrder
  fmul st(0), st(0)                         // t²,t, K, K², 1/2*FOrder
  fld [self.FRippleFactors].Double          // FRipple[0], t²,t, K, K², 1 / 2 * FOrder
  fsubrp                                    // FRipple[0] - t²,t, K, K², 1 / 2 * FOrder
  fld1                                      // 1, FRipple[0] - t²,t, K, K², 1 / 2 * FOrder
  fdivrp                                    // 1 / (FRipple[0] - t²) = t1, t, K, K², 1 / 2 * FOrder
  fxch                                      // t, t1, K, K², 1 / 2 * FOrder
  fadd st(0), st(0)                         // 2*t, t1, K, K², 1 / 2 * FOrder
  fmul st(0), st(1)                         // 2*t * t1, t1, K, K², 1 / 2 * FOrder
  fmul [self.FRippleFactors + 8].Double     // FRipple[1]*2*t*t1, t1, K, K², 1 / 2 * FOrder
  fmul st(0), st(2)                         // K*FRipple[1]*2*t*t1 = t2, t1, K, K², 1 / 2 * FOrder
  fld st(0)                                 // t2, t2, t1, K, K², 1 / 2 * FOrder
  fadd st(0), st(2)                         // t1+t2, t2, t1, K, K², 1 / 2 * FOrder
  fadd st(0), st(4)                         // t1+t2+K², t2, t1, K, K², 1 / 2 * FOrder
  fld1                                      // 1, t1 + t2 + K², t2, t1, K, K², 1 / 2 * FOrder
  fdivrp                                    // (1 / t1 + t2 + K²) = t, t2, t1, K, K², 1 / 2 * FOrder
  fld st(0)                                 // t, t, t2, t1, K, K², 1/2*FOrder
  fmul st(0),st(5)                          // t * K²=fA[2*i], t, t2, t1, K, K², 1/2*FOrder

   fld  [self.FFilterGain].Double
   fmul st(0), st(1)
   fstp [self.FFilterGain].Double

  fst [Self.FCoeffs + 8 * edx].Double       // store to fA[2*i], 1/2*FOrder
  fadd st(0),st(0)                          // 2 * fA[2 * i], t, t2, t1, K, K², 1/2*FOrder
  fstp [self.FCoeffs + 8 * edx + 8].Double  // store to fA[2*i+1], 1/2*FOrder

  fld st(2)                                 // t1, t, t2, t1, K, K², 1/2*FOrder
  fsub st(0), st(5)                         // t1-K², t, t2, t1, K, K², 1/2*FOrder
  fadd st(0), st(0)                         // 2*(t1-K²), t, t2, t1, K, K², 1/2*FOrder
  fmul st(0), st(1)                         // 2*(t1-K²)*t, t, t2, t1, K, K², 1/2*FOrder
  fstp [self.FCoeffs + 8 * edx + 16].Double // store to fB[2*i], 1/2*FOrder
  fxch                                      // t2, t, t1, K, K², 1/2*FOrder
  fsubrp st(2), st(0)                       // t, t2-t1, K, K², 1/2*FOrder
  fxch                                      // t2-t1, t, K, K², 1/2*FOrder
  fsub st(0), st(3)                         // t2-t1-K², t, K, K², 1/2*FOrder
  fmulp                                     // (t2-t1-K²) * t, K, K², 1/2*FOrder
  fstp [self.FCoeffs + 8 * edx + 24].Double // store to fB[2*i+1], 1/2*FOrder
 loop @OrderLoop
 fstp st(0)                                 // K², 1 / 2 * FOrder
 fstp st(0)                                 // 1 /  2 * FOrder
 fstp st(0)                                 // stack free!

 fld  [self.FCoeffs].Double                 // load fA[0]
 fmul [self.FGainFactor].Double             // apply FGainFactor
 fstp [self.FCoeffs].Double                 // store fA[0]
 fld  [self.FCoeffs + 8].Double             // load fA[1]
 fmul [self.FGainFactor].Double             // apply FGainFactor
 fstp [self.FCoeffs + 8].Double             // store fA[1]
@done:
{$ENDIF}
end;

function TChebyshev1LP.MagnitudeSquared(const Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw     := 2 * cos(Frequency * pi * fSRR);
 a      := sqr(cw - 2);
 Result := 1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * sqr(FCoeffs[4 * i]) * a / (1 + sqr(FCoeffs[4 * i + 2]) + sqr(FCoeffs[4 * i + 3]) + 2 * FCoeffs[4 * i + 3] + cw*((FCoeffs[4 * i + 2] - cw) * FCoeffs[4 * i + 3] - FCoeffs[4 * i + 2]));
end;

function TChebyshev1LP.MagnitudeLog10(const Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw     := 2 * cos(Frequency * Pi * fSRR);
 a      := sqr(cw - 2);
 Result := 1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * sqr(FCoeffs[4 * i]) * a / (1 + sqr(FCoeffs[4 * i + 2]) + sqr(FCoeffs[4 * i + 3]) + 2 * FCoeffs[4 * i + 3] + cw * ((FCoeffs[4 * i + 2] - cw) * FCoeffs[4 * i + 3] - FCoeffs[4 * i + 2]));
 Result := 10 * Log10(Result);
end;

{$DEFINE PUREPASCAL}
function TChebyshev1LP.ProcessSample(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=     x                               + FState[2 * i    ];
   FState[2 * i    ] := 2 * x + FCoeffs[4 * i + 2] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=     x + FCoeffs[4 * i + 3] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        := x + FState[2 * i];
   FState[2 * i] := x + FCoeffs[4 * i + 2] * Result;
  end;
{$ELSE}
asm
 fld   Input.Double;
 fmul  [Self.FFilterGain].Double;
 mov   ecx, [Self.FOrder]
 test  ecx, ecx
 jz    @End
 shr   ecx, 1
 shl   ecx, 2
 push  ecx
 jz @SingleStage
 @FilterLoop:
  sub   ecx, 4
  fld   st(0)
  fadd  [self.FState + ecx * 4].Double
  fld   st(0)
  fld   st(0)
  fmul  [self.FCoeffs + ecx * 8 + 16].Double
  fadd  [self.FState + ecx * 4 + 8].Double
  fld   st(3)
  fadd  st(0), st(0)
  faddp
  fstp  [self.FState + ecx * 4].Double
  fmul  [self.FCoeffs + ecx * 8 + 24].Double
  fxch
  fxch  st(2)
  faddp
  fstp  [self.FState + ecx * 4 + 8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [self.FOrder]
 jz @End
  mov ecx, [self.FOrder]
  dec ecx
  shl ecx, 1
  fmul [self.FCoeffs + ecx * 8].Double
  fld st(0)
  fadd [self.FState + ecx * 4].Double
  fld st(0)
  fmul [self.FCoeffs + ecx * 8 + 16].Double
  faddp st(2), st(0)
  fxch
  fstp [self.FState + ecx * 4].Double
 @End:
 {$ENDIF}
end;


{ TChebyshev1FilterHP }

constructor TChebyshev1HP.Create(const Order: Integer = 0);
begin
 inherited Create;
end;

procedure TChebyshev1HP.CalculateCoefficients;
{$IFNDEF XPUREPASCAL}
var
  Pi4thOrder : Double;
  K, K2      : Double;
  t, t1, t2  : Double;
  i          : Integer;
begin
 Pi4thOrder := Pi / (4 * FOrder);
 K  := tan(FW0 * 0.5);
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 for i := (FOrder div 2) - 1 downto 0 do
  begin
   t  := sqr( sin( (i * 2 + 1) * Pi4thOrder ) );
   t1 := 1 / (FRippleFactors[0] + 4 * t - 4 * sqr(t)-1);
   t2 := 2 * K * t1 * FRippleFactors[1] * (1 - 2 * t);
   t  := 1 / (t2 + 1 + t1 * K2);
   FFilterGain := FFilterGain * t;
   FCoeffs[2 * i    ] := 2 * (     1 - t1 * K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - 1 - t1 * K2) * t;
  end;
{$ELSE}
asm
 mov ecx, [self.FOrder]                     // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz @done                                   // exit if filter order = 0
 shr ecx, 1                                 // ecx = order div 2
 test ecx, ecx                              // set flags according to ecx
 jz @done                                   // exit if filter order = 0

 fldpi                                      // Pi
 fild [self.FOrder]                         // FOrder, Pi
 fadd st(0),st(0)                           // 2 * FOrder, Pi
 fadd st(0),st(0)                           // 4 * FOrder, Pi
 fdivp                                      // Pi / (4 * FOrder)

 fld [self.FW0]                             // FW0, Pi / (4 * FOrder)
 fmul CHalf32                               // FW0 * 0.5, Pi / (4 * FOrder)
 fsincos                                    // sin(FW0 * 0.5), cos(FW0 * 0.5), Pi / (4 * FOrder)
 fdivp                                      // K = tan(FW0 * 0.5), Pi / (4 * FOrder)
 fld st(0)                                  // K, K, Pi / (4 * FOrder)
 fmul st(0),st(0)                           // K², K, Pi / (4 * FOrder)
 fxch                                       // K, K², Pi / (4 * FOrder)

 fld  [self.FFilterGain]
 fmul [self.FGainFactorSquared]
 fstp [self.FFilterGain]

 mov ecx, [self.FOrder]                     // ecx = order
 @OrderLoop:
  mov edx, ecx                              // edx = 2 * i
  dec edx                                   // edx = 2 * i + 1
  mov [esp - 4], edx                        // edx to stack
  dec edx                                   // edx = 2 * i
  shl edx, 1                                // edx = 4 * i
  fild [esp - 4].Integer                    // edx in st(0) = 2 * i + 1, K, K², Pi / (4 * FOrder)
  fmul st(0), st(3)                         // (2 * i + 1) * Pi / (4 * Order), K, K², Pi / (4 * FOrder)
  fsin                                      // sin((2 * i + 1) * Pi / (4 * Order)), K, K², Pi / (4 * FOrder)
  fmul st(0),st(0)                          // t = sqr(sin((2 * i + 1) * Pi / (4 * Order))), K, K², Pi / (4 * FOrder)

  fld st(0)                                 // t, t, K, K², Pi / (4 * Order)
  fmul st(0),st(0)                          // t²,t, K, K², Pi / (4 * Order)
  fld st(1)                                 // t, t²,t, K, K², Pi / (4 * Order)
  fsubrp                                    // t - t²,t, K, K², Pi / (4 * Order)
  fadd st(0),st(0)                          // 2 * (t-t²), t, K, K², Pi / (4 * Order)
  fadd st(0),st(0)                          // 4 * (t-t²), t, K, K², Pi / (4 * Order)
  fld [self.FRippleFactors].Double          // FRipple[0], 4*t²,t, K, K², Pi / (4 * Order)
  faddp                                     // FRipple[0] + 4 * t², t, K, K², Pi / (4 * Order)
  fld1                                      // 1, FRipple[0]+4*t-4*t²,t, K, K², Pi / (4 * Order)
  fsub st(1),st(0)                          // 1, FRipple[0]+4*t-4*t²-1,t, K, K², Pi / (4 * Order)
  fdivrp                                    // 1 / (FRipple[0]+4*t-4*t²-1) = t1, t, K, K², Pi / (4 * Order)

  fxch                                      // t, t1, K, K², Pi / (4 * Order)
  fadd st(0),st(0)                          // 2*t, t1, K, K², Pi / (4 * Order)
  fld1                                      // 1, 2*t, t1, K, K², Pi / (4 * Order)
  fsubrp                                    // 1 - 2*t, t1, K, K², Pi / (4 * Order)
  fmul [self.FRippleFactors + 8].Double     // FRipple[1]*(1-2*t), t1, K, K², Pi / (4 * Order)
  fmul st(0),st(2)                          // K*FRipple[1]*(1-2*t), t1, K, K², Pi / (4 * Order)
  fmul st(0),st(1)                          // t1*K*FRipple[1]*(1-2*t), t1, K, K², Pi / (4 * Order)
  fadd st(0),st(0)                          // t2=2*t1*K*FRipple[1]*(1-2*t), t1, K, K², Pi / (4 * Order)
  fld st(1)                                 // t1, t2, t1, K, K², Pi / (4 * Order)
  fmul st(0),st(4)                          // t1*K², t2, t1, K, K², Pi / (4 * Order)
  fld1                                      // 1, t1*K², t2, t1, K, K², Pi / (4 * Order)
  faddp                                     // 1+t1*K², t2, t1, K, K², Pi / (4 * Order)
  fadd st(0),st(1)                          // 1+t1*K²+t2, t2, t1, K, K², Pi / (4 * Order)
  fld1                                      // 1, 1+t1*K²+t2, t2, t1, K, K², Pi / (4 * Order)
  fdivrp                                    // 1/(1+t1*K²+t2)=A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fst [Self.FCoeffs + 8 * edx].Double       // store to fA[2*i]

  fld st(0)                                 // A[2*i], A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fadd st(0),st(0)                          // 2*A[2*i], A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fchs                                      // -2*A[2*i], A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fstp [self.FCoeffs + 8 * edx + 8].Double  // store to fA[2*i+1]

  fld st(2)                                 // t1, A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fmul st(0), st(5)                         // t1*K², A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fld1                                      // 1, t1*K², A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fsubrp                                    // 1-t1*K², A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fadd st(0), st(0)                         // 2*(1-t1*K²), A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fmul st(0), st(1)                         // 2*(1-t1*K²)*A[2*i], A[2*i], t2, t1, K, K², Pi / (4 * Order)
  fstp [self.FCoeffs + 8 * edx + 16].Double // store to fB[2*i]

  fxch st(2)                                // t1, t2, A[2*i], K, K², Pi / (4 * Order)
  fmul st(0), st(4)                         // t1*K², t2, A[2*i], K, K², Pi / (4 * Order)
  fld1                                      // 1, t1*K², t2, A[2*i], K, K², Pi / (4 * Order)
  faddp                                     // 1 + t1*K², t2, A[2*i], K, K², Pi / (4 * Order)
  fsubp                                     // t2 - (1 + t1*K²), A[2*i], K, K², Pi / (4 * Order)
  fmulp                                     // (t2 - (1 + t1*K²)) * A[2*i], K, K², Pi / (4 * Order)
  fstp [self.FCoeffs + 8 * edx + 24].Double // store to fB[2*i+1], Pi / (4 * Order)
  sub ecx, 2
 jnz @OrderLoop
 fstp st(0)                                 // K², Pi / (4 * Order)
 fstp st(0)                                 // Pi / (4 * Order)
 fstp st(0)                                 // stack free!

 fld  [self.FCoeffs].Double                 // load fA[0]
 fmul [self.FGainFactor].Double             // apply FGainFactor
 fstp [self.FCoeffs].Double                 // store fA[0]
 fld  [self.FCoeffs + 8].Double             // load fA[1]
 fmul [self.FGainFactor].Double             // apply FGainFactor
 fstp [self.FCoeffs + 8].Double             // store fA[1]

@done:
{$ENDIF}
end;

function TChebyshev1HP.MagnitudeSquared(const Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw := 2 * cos(Frequency * Pi * fSRR);
 a  := sqr(cw - 2);
 Result := 1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * sqr(FCoeffs[4 * i] * a / (1 + sqr(FCoeffs[4 * i + 2]) + sqr(FCoeffs[4 * i + 3]) + 2 * FCoeffs[4 * i + 3] + cw * ((FCoeffs[4 * i + 2] - cw) * FCoeffs[4 * i + 3] - FCoeffs[4 * i + 2])));
end;

function TChebyshev1HP.MagnitudeLog10(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * cos(Frequency * pi * fSRR);
 a  := sqr(cw - 2);
 Result:=1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * sqr(FCoeffs[4 * i]) * a / (1 + sqr(FCoeffs[4 * i + 2])+sqr(FCoeffs[4*i+3])+2*FCoeffs[4*i+3]+cw*((FCoeffs[4*i+2]-cw)*FCoeffs[4*i+3]-FCoeffs[4*i+2]));
 Result := 10 * log10(Result);
end;

function TChebyshev1HP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=      x                                + FState[2 * i];
   FState[2 * i    ] := -2 * x + FCoeffs[2 * i    ] * Result  + FState[2 * i + 1];
   FState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        := x + FState[2 * i];
   FState[2 * i] := x + FCoeffs[2 * i] * Result;
  end;
end;
{$ELSE}
asm
 fld   Input.Double;
 fmul  [Self.FFilterGain].Double
 mov   ecx, [Self.FOrder]
 test  ecx, ecx
 jz    @End
 push  ecx
 shr   ecx, 1
 shl   ecx, 1
 @FilterLoopBiquad:
  sub  ecx, 2
  fld  st(0)
  fadd [self.FState + ecx * 8].Double
  fld  st(0)
  fld  st(0)
  fmul [self.FCoeffs + ecx * 8].Double
  fadd [self.FState + ecx * 8 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  fsubp
  fstp [self.FState + ecx * 8].Double
  fmul [self.FCoeffs + ecx * 8 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [self.FState + ecx * 8 + 8].Double
 jnz @FilterLoopBiquad
 pop  ecx
 and  ecx, 1
 test  ecx, ecx
 jz    @End

 // add first order here

 @End:
end;
{$ENDIF}

{ TCustomChebyshev2Filter }

constructor TCustomChebyshev2Filter.Create(const Order: Integer = 0);
begin
 inherited;
end;

function TCustomChebyshev2Filter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 10 * Log10(MagnitudeSquared(Frequency));
end;

function TCustomChebyshev2Filter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

procedure TCustomChebyshev2Filter.OrderChanged;
begin
 inherited;
 CalculateRippleFactors;
 CalculateCoefficients;
end;

procedure TCustomChebyshev2Filter.Reset;
begin
 Gain := 0;
end;

procedure TCustomChebyshev2Filter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomChebyshev2Filter.SetFilterValues(const AFrequency, AGain,
  ARipple: Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency := AFrequency;
 FGain_dB   := AGain;
 FRipple    := ARipple;
 CalculateW0;
 CalculateGainFactor;
 CalculateRippleFactors;
end;

procedure TCustomChebyshev2Filter.CalculateRippleFactors;
var
  t : Double;
begin
 if FOrder > 0 then
  begin
   t := arcsinh(1 / sqrt(Power(10, (FRipple * 0.1)) - 1)) / FOrder;
   FRippleFactors[1] := sinh(t);
   FRippleFactors[0] := sqr(cosh(t));
  end;
 ResetStates;
end;

procedure TCustomChebyshev2Filter.CalculateW0;
begin
 inherited;
 fW0 := 2 * Pi * fSRR * (FFrequency);
 fSinW0 := sin(fW0);
 if fW0 > 3.1 then fW0 := 3.1;
end;

(*
{ TChebyshev2LP }

constructor TChebyshev2LP.Create;
begin
  inherited;

end;

procedure TChebyshev2LP.CalculateCoefficients;
begin
  inherited;

end;

function TChebyshev2LP.MagnitudeLog10(const Frequency: Double): Double;
begin

end;

function TChebyshev2LP.MagnitudeSquared(const Frequency: Double): Double;
begin

end;

function TChebyshev2LP.ProcessSample(const Input: Double): Double;
begin

end;

{ TChebyshev2HP }

constructor TChebyshev2HP.Create;
begin
  inherited;

end;

procedure TChebyshev2HP.CalculateCoefficients;
begin
  inherited;

end;

function TChebyshev2HP.MagnitudeLog10(Frequency: Double): Double;
begin

end;

function TChebyshev2HP.MagnitudeSquared(Frequency: Double): Double;
begin

end;

function TChebyshev2HP.ProcessSample(const Input: Double): Double;
begin

end;
*)

end.
