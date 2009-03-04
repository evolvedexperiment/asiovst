unit DAV_DspChebyshevFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspFilter, DAV_Complex;

type
  TCustomChebyshevFilter = class(TCustomOrderFilter)
  private
    function GetRipple: Double;
  protected
    FRipple         : Double;
    FRippleFactors  : TDAV2DoubleArray;
    procedure SetRipple(const Value: Double); virtual;
    procedure RippleChanged; virtual;
    procedure CalculateRippleFactors; virtual; abstract;
  public
    constructor Create(const Order: Integer = 0); reintroduce; virtual;
    procedure ResetStatesInt64; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;

    property Ripple : Double read GetRipple write SetRipple;
  end;

  TCustomChebyshev1Filter = class(TCustomChebyshevFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FTanW0Half      : Double;
    FFilterGain     : Double;
    FOrderInv       : Double;
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FCosOrderOffset : TComplexDouble;
    FHalfPiOrderInv : Double;
    FCoeffs         : array [0..63] of Double;
    FState          : array [0..63] of Double;
    FStateStack     : array of array [0..63] of Double;
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
    procedure PushStates; override;
    procedure PopStates; override;
    procedure Reset; override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

  TCustomChebyshev1LowpassFilter = class(TCustomChebyshev1Filter)
  public
    function ProcessSample(const Input: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
    function Phase(const Frequency: Double): Double; override;
  end;

  TChebyshev1LowpassFilter = class(TCustomChebyshev1LowpassFilter)
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev1HighCutFilter = TChebyshev1LowpassFilter;

  TChebyshev1LowpassFilterAutomatable = class(TCustomChebyshev1LowpassFilter)
  protected
    FRippleFactor : Single;
    procedure RippleChanged; override;
    procedure CalculateW0; override;
    procedure CalculateRippleFactors; override;
    procedure OrderChanged; override;
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev1HighCutFilterAutomatable = TChebyshev1LowpassFilterAutomatable;

  TCustomChebyshev1HighpassFilter = class(TCustomChebyshev1Filter)
  public
    function ProcessSample(const Input: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Single;
      out Imaginary: Single); override;
    function Phase(const Frequency: Double): Double; override;
  end;

  TChebyshev1HighpassFilter = class(TCustomChebyshev1HighpassFilter)
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev1LowCutFilter = TChebyshev1HighpassFilter;

  TChebyshev1HighpassFilterAutomatable = class(TCustomChebyshev1HighpassFilter)
  protected
    FRippleFactor : Single;
    procedure RippleChanged; override;
    procedure CalculateW0; override;
    procedure CalculateRippleFactors; override;
    procedure OrderChanged; override;
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev1LowCutFilterAutomatable = TChebyshev1HighpassFilterAutomatable;

  TCustomChebyshev2Filter = class(TCustomChebyshevFilter)
  protected
    FOrderInv       : Double;
    FHalfPiOrderInv : Double;
    FCoeffs         : array [0..127] of Double;
    FState          : array [0.. 63] of Double;
    procedure CalculateW0; override;
    procedure OrderChanged; override;
    procedure CalculateRippleFactors; override;
  public
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
  Math, SysUtils, DAV_Approximations;

const
  CHalf32 : Single = 0.5;
  CHalf64 : Double = 0.5;

{ TCustomChebyshevFilter }

constructor TCustomChebyshevFilter.Create(const Order: Integer = 0);
begin
 FOrder    := Order;
 FRipple   := 1;
 OrderChanged;
 inherited Create;
end;

function TCustomChebyshevFilter.GetRipple: Double;
begin
 Result := FRipple;
end;

function TCustomChebyshevFilter.Imaginary(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TCustomChebyshevFilter.SetRipple(const Value: Double);
begin
 if Value <> FRipple then
  begin
   FRipple := Value;
   RippleChanged;
  end;
end;

function TCustomChebyshevFilter.Real(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, result, Temp);
end;

procedure TCustomChebyshevFilter.ResetStatesInt64;
begin
 inherited;
 ResetStates;
end;

procedure TCustomChebyshevFilter.RippleChanged;
begin
 CalculateRippleFactors;
 CalculateCoefficients;
end;

{ TCustomChebyshev1Filter }

constructor TCustomChebyshev1Filter.Create(const Order: Integer = 0);
begin
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 FFilterGain := 1;
 inherited Create(Order);
end;

procedure TCustomChebyshev1Filter.CalculateW0;
begin
 FTanW0Half := tan(2 * Pi * FSRR * (FFrequency * FDownsampleFak) * CHalf64);
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
  t : array [0..1] of Double;
begin
 t[0] := arcsinh(1 / sqrt(Power(10, (FRipple * 0.1)) - 1));
 t[1] := Exp(-t[0] * FOrderInv) * CHalf64;
 t[0] := CQuarter64 / t[1];
 FRippleFactors[1] := t[0] - t[1];
 FRippleFactors[0] := sqr(t[0] + t[1]);
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
 CalculateCoefficients;
end;

function TCustomChebyshev1Filter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

procedure TCustomChebyshev1Filter.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   FHalfPiOrderInv := Pi * CHalf64 * FOrderInv;
   {$IFDEF PUREPASCAL}
   GetSinCos(FHalfPiOrderInv, FCosOrderOffset.Im, FCosOrderOffset.Re);
   {$ENDIF}
   CalculateRippleFactors;
   ResetStates;
   inherited;
  end else FOrderInv := 1;
end;

procedure TCustomChebyshev1Filter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0],FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TCustomChebyshev1Filter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0], FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

function TCustomChebyshev1Filter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 10 * Log10(MagnitudeSquared(Frequency));
end;


{ TCustomChebyshev1LowpassFilter }

procedure TCustomChebyshev1LowpassFilter.Complex(const Frequency: Double;
  out Real, Imaginary: Double);
var
  i           : Integer;
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Real := FFilterGain;
 Imaginary := 0;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   Divider   := 1 / (sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] + sqr(FCoeffs[2 * i]) + 1
                      + 2 * cw * (FCoeffs[2 * i] * (FCoeffs[2 * i + 1] - 1) - 2 * cw * FCoeffs[2 * i + 1]));
   ComplexMultiply(Real, Imaginary,
     (1 - 2 * FCoeffs[2 * i] - FCoeffs[2 * i + 1]
                + 2 * cw * (1 - FCoeffs[2 * i + 1] - FCoeffs[2 * i])
                + (2 * sqr(cw) - 1) * (1 - FCoeffs[2 * i + 1])) * Divider,
     2 * (1 + FCoeffs[2 * i + 1]) * sqrt(1 - sqr(cw)) * Divider);
  end;
end;

function TCustomChebyshev1LowpassFilter.Phase(const Frequency: Double): Double;
var
  Cmplx : array [0..1] of TComplexDouble;
  i     : Integer;
begin
(*
  Complex(Frequency, Cmplx[1].Re, Cmplx[1].Im);
*)
 GetSinCos(2 * Frequency * Pi * SampleRateReciprocal, Cmplx[0].Im, Cmplx[0].Re);
 Cmplx[1].Im := 0; Cmplx[1].Re := 1;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   ComplexMultiplyInplace(Cmplx[1].Re, Cmplx[1].Im,
     (Cmplx[0].Re * (1 - FCoeffs[2 * i + 1] - FCoeffs[2 * i] + Cmplx[0].Re * (1 - FCoeffs[2 * i + 1])) - FCoeffs[2 * i]),
     (Cmplx[0].Im * (1 + FCoeffs[2 * i + 1]) * (Cmplx[0].Re + 1)));
  end;
 Result := ArcTan2(Cmplx[1].Im, Cmplx[1].Re);
end;

function TCustomChebyshev1LowpassFilter.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=     x                               + FState[2 * i    ];
   FState[2 * i    ] := 2 * x + FCoeffs[2 * i    ] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=     x + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        := x + FState[2 * i];
   FState[2 * i] := x + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
 fld   Input.Double;
 fmul  [Self.FFilterGain].Double;
 mov   ecx, [Self.FOrder]
 test  ecx, ecx
 jz    @End
 push  ecx
 shr   ecx, 1
 shl   ecx, 1
 jz @SingleStage
 @FilterLoop:
  sub   ecx, 2
  fld   st(0)
  fadd  [self.FState + ecx * 8].Double
  fld   st(0)
  fld   st(0)
  fmul  [self.FCoeffs + ecx * 8 + 0].Double
  fadd  [self.FState + ecx * 8 + 8].Double
  fld   st(3)
  fadd  st(0), st(0)
  faddp
  fstp  [self.FState + ecx * 8].Double
  fmul  [self.FCoeffs + ecx * 8 + 8].Double
  fxch
  fxch  st(2)
  faddp
  fstp  [self.FState + ecx * 8 + 8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 and ecx, 1
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


{ TChebyshev1LowpassFilter }

procedure TChebyshev1LowpassFilter.CalculateCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2      : Double;
  t, t1, t2  : Double;
  i          : Integer;
  Cmplx      : TComplexDouble;
begin
 if FOrder = 0 then exit;
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FCosOrderOffset;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   t  := Cmplx.Re; //cos((i * 2 + 1) * FHalfPiOrderInv);
   ComplexMultiply2Inplace(Cmplx, FCosOrderOffset);
   t1 := 1 / (FRippleFactors[0] - sqr(t));
   t2 := 2 * t * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + K2 + t1);
   FFilterGain := FFilterGain * K2 * t;
   FCoeffs[2 * i    ] := 2 * (     t1 - K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - t1 - K2) * t;
  end;
{$ELSE}
asm
 mov  ecx, [self.FOrder]                    // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz   @done                                 // exit if filter order = 0
 shr  ecx, 1                                // ecx = order div 2
 test ecx, ecx                              // set flags according to ecx
 jz   @done                                 // exit if filter order = 0

 fld  [self.FGainFactorSquared]             // FFilterGain 
 fld  [self.FHalfPiOrderInv]                // Pi / (2 * FOrder), FFilterGain
 fld  [self.FTanW0Half]                     // K, Pi / (2 * FOrder), FFilterGain

 mov  ecx, [self.FOrder]                    // ecx = order
 @OrderLoop:
  // calculate t = cos((i * 2 + 1) * FHalfPiOrderInv);
  mov edx, ecx                              // edx = 2 * i
  dec edx                                   // edx = 2 * i + 1
  mov [esp - 4], edx                        // edx to stack
  dec edx                                   // edx = 2 * i
  fild [esp - 4].Integer                    // edx in st(0) = 2 * i + 1, K, Pi / (2 * FOrder), FFilterGain
  fmul st(0), st(2)                         // (2 * i + 1) * Pi / (2 * Order), K, Pi / (2 * FOrder), FFilterGain
  fcos                                      // t = cos((2 * i + 1) * Pi / (2 * Order)), K, Pi / (2 * FOrder), FFilterGain

  // calculate t1 = 1 / (FRippleFactors[0] - sqr(t));
  fld  st(0)                                // t, t, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(0)                         // t², t, K, Pi / (2 * Order), FFilterGain
  fld  [self.FRippleFactors].Double         // FRippleFactors[0], t², t, K, Pi / (2 * Order), FFilterGain
  fsubrp                                    // FRippleFactors[0] - t², t, K, Pi / (2 * Order), FFilterGain
  fld1                                      // 1, FRippleFactors[0] - t², t, K, Pi / (2 * Order), FFilterGain
  fdivrp                                    // t1 = 1 / (FRippleFactors[0] - t²), t, K, Pi / (2 * Order), FFilterGain

  // calculate t2 = 2 * t * t1 * K * FRippleFactors[1];
  fxch                                      // t, t1, K, Pi / (2 * Order), FFilterGain
  fadd st(0), st(0)                         // 2 * t, t1, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(1)                         // 2 * t * t1, t1, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(2)                         // 2 * t * t1 * K, t1, K, Pi / (2 * Order), FFilterGain
  fmul [self.FRippleFactors + 8].Double     // t2 = FRippleFactors[1]* 2 * t * t1 * K, t1, K, Pi / (2 * Order), FFilterGain

  // calculate t = 1 / (t2 + K² + t1);
  fld  st(2)                                // K, t2, t1, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(0)                         // K², t2, t1, K, K², Pi / (2 * Order), FFilterGain
  fadd st(0), st(1)                         // K² + t2, t2, t1, K, Pi / (2 * Order), FFilterGain
  fadd st(0), st(2)                         // K² + t2 + t1, t2, t1, K, Pi / (2 * Order), FFilterGain
  fld1                                      // 1, K² + t2 + t1, t2, t1, K, Pi / (2 * Order), FFilterGain
  fdivrp                                    // t = 1 / (K² + t2 + t1), t2, t1, K, Pi / (2 * Order), FFilterGain

  // FFilterGain = FFilterGain * t;

  fmul st(5), st(0)                         // t = 1 / (1 + t1 * K² + t2), t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fxch st(3)                               // K, t2, t1, t, Pi / (2 * Order), FFilterGain * t
  fmul st(5), st(0)                        // K, t2, t1, t, Pi / (2 * Order), FFilterGain * t * K
  fmul st(5), st(0)                        // K, t2, t1, t, Pi / (2 * Order), FFilterGain * t * K²
  fxch st(3)                               // t, t2, t1, K, Pi / (2 * Order), FFilterGain * t

  // calculate Coeff[0] = 2 * (t1 - K2) * t
  fld  st(3)                                // K, t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(0)                         // K², t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fsubr st(0), st(3)                        // t1 - K², t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(1)                         // t * (t1 - K²), t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fadd st(0), st(0)                         // 2 * t * (t1 - K²), t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fstp [self.FCoeffs + 8 * edx].Double      // store to FCoeffs[2 * i]

  // calculate Coeff[1] = (t2 - t1 - K2) * t;
  fxch st(2)                                // t1, t2, t, K, Pi / (2 * Order), FFilterGain * t
  fld  st(3)                                // K, t1, t2, t, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(0)                         // K², t1, t2, t, K, Pi / (2 * Order), FFilterGain * t
  faddp                                     // t1 + K², t2, t, K, Pi / (2 * Order), FFilterGain * t
  fsubp                                     // t2 - t1 - K², t, K, Pi / (2 * Order), FFilterGain * t
  fmulp                                     // (t2 - t1 - K²) * t, K, Pi / (2 * Order), FFilterGain * t
  fstp [self.FCoeffs + 8 * edx + 8].Double  // store to FCoeffs[2 * i + 1], Pi / (2 * Order), FFilterGain * t
  sub  ecx, 2
 jnz  @OrderLoop
 fstp st(0)                                 // Pi / (2 * Order), FFilterGain * t
 fstp st(0)                                 // FFilterGain * t
 fstp [self.FFilterGain].Double             // stack free!

@done:
{$ENDIF}
end;

function TChebyshev1LowpassFilter.MagnitudeSquared(const Frequency: Double): Double;
var                                    
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal);
 a      := sqr(cw + 2);
 Result := sqr(FFilterGain);
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + sqr(FCoeffs[2 * i]) +
       sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
       cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) / (1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := CDenorm64 + Abs(Result);
end;


{ TCustomChebyshev1HighpassFilter }

procedure TCustomChebyshev1HighpassFilter.Complex(const Frequency: Double;
  out Real, Imaginary: Single);
var
  i           : Integer;
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 for i := 0 to (FOrder div 2) - 1 do
  begin
   Divider   := 1 / (sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] + sqr(FCoeffs[2 * i]) + 1
                      + 2 * cw * (FCoeffs[2 * i] * (FCoeffs[2 * i + 1] - 1) - 2 * cw * FCoeffs[2 * i + 1]));
   Real      := (1 + 2 * FCoeffs[2 * i] - FCoeffs[2 * i + 1]
                +        cw     * (2 * (FCoeffs[2 * i + 1] - 1) - FCoeffs[2 * i] * 2)
                + (2 * sqr(cw) - 1) * (1 - FCoeffs[2 * i + 1])) * Divider;
   Imaginary := (- 2 * (1 + FCoeffs[2 * i + 1])) * sqrt(1 - sqr(cw)) * Divider;
  end;
end;

function TCustomChebyshev1HighpassFilter.Phase(const Frequency: Double): Double;
var
  cw, sw   : Double;
  Nom, Den : Double;
  i        : Integer;
begin
(*
 Complex(Frequency, Den, Nom);
*)
 GetSinCos(2 * Frequency * Pi * SampleRateReciprocal, sw, cw);
 Nom := 0; Den := 1;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   ComplexMultiplyInplace(Den, Nom,
     (cw * (FCoeffs[2 * i + 1] - FCoeffs[2 * i] - 1 + cw * (1 - FCoeffs[2 * i + 1])) + FCoeffs[2 * i]),
     (sw * (FCoeffs[2 * i + 1] + 1) * (cw - 1)));
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   ComplexMultiplyInplace(Den, Nom, (1 + FCoeffs[2 * i]) * (1 - cw),
     sw * (FCoeffs[2 * i] - 1));
  end;
 Result := ArcTan2(Nom, Den);
end;

function TCustomChebyshev1HighpassFilter.ProcessSample(const Input: Double): Double;
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


{ TChebyshev1LowpassFilterAutomatable }

procedure TChebyshev1LowpassFilterAutomatable.CalculateCoefficients;
var
  K, K2      : Double;
  t, t1, t2  : Double;
  i          : Integer;
  Cmplx      : TComplexDouble;
begin
 if FOrder = 0 then exit;
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FCosOrderOffset;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   t  := Cmplx.Re; //cos((i * 2 + 1) * FHalfPiOrderInv);
   ComplexMultiply2Inplace(Cmplx, FCosOrderOffset);
   t1 := 1 / (FRippleFactors[0] - sqr(t));
   t2 := 2 * t * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + K2 + t1);
   FFilterGain := FFilterGain * K2 * t;
   FCoeffs[2 * i    ] := 2 * (     t1 - K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - t1 - K2) * t;
  end;
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateRippleFactors;
var
  t : array [0..1] of Single;
begin
 t[0] := 1 / FastSqrtBab1(sqr(FRippleFactor) - 1);
 t[0] := FastLog2MinError3(t[0] + FastSqrtBab1(sqr(t[0]) + 1)) * FOrderInv;
 t[1] := FastPower2MinError3(t[0]) * CHalf32;
 t[0] := CQuarter32 / t[1];
 FRippleFactors[1] := t[1] - t[0];
 FRippleFactors[0] := sqr(t[1] + t[0]);
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateW0;
begin
 FTanW0Half := FastTan2Term(2 * Pi * FSRR * (FFrequency * FDownsampleFak) * CHalf64);
end;

function TChebyshev1LowpassFilterAutomatable.MagnitudeSquared(
  const Frequency: Double): Double;
var                                    
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * FastCosInBounds4Term(2 * Frequency * Pi * SampleRateReciprocal);
 a      := sqr(cw + 2);
 Result := sqr(FFilterGain);
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + sqr(FCoeffs[2 * i]) +
       sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
       cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) / (1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := CDenorm64 + Abs(Result);
end;


procedure TChebyshev1LowpassFilterAutomatable.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   FHalfPiOrderInv := Pi * CHalf64 * FOrderInv;
   GetSinCos(FHalfPiOrderInv, FCosOrderOffset.Im, FCosOrderOffset.Re);
   CalculateRippleFactors;
   ResetStates;
   CalculateCoefficients;
  end else FOrderInv := 1;
end;

procedure TChebyshev1LowpassFilterAutomatable.RippleChanged;
begin
 FRippleFactor := FastdBtoAmpMinError3(FRipple);
 inherited;
end;

{ TChebyshev1HighpassFilter }

procedure TChebyshev1HighpassFilter.CalculateCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2      : Double;
  t, t1, t2  : Double;
  i          : Integer;
  Cmplx      : TComplexDouble;
begin
 if FOrder = 0 then exit;
 K  := FTanW0Half;
 K2 := sqr(K);
 Cmplx := FCosOrderOffset;
 FFilterGain := FGainFactorSquared;
 for i := (FOrder div 2) - 1 downto 0 do
  begin
   t  := Cmplx.Re;
   ComplexMultiply2Inplace(Cmplx, FCosOrderOffset);
   t1 := 1 / (FRippleFactors[0] - sqr(t));
   t2 := 2 * t * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + 1 + t1 * K2);
   FFilterGain := FFilterGain * t;
   FCoeffs[2 * i    ] := 2 * (     1 - t1 * K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - 1 - t1 * K2) * t;
  end;
{$ELSE}
asm
 mov  ecx, [self.FOrder]                   // ecx = order
 test ecx, ecx                             // set flags according to ecx
 jz   @done                                // exit if filter order = 0
 shr  ecx, 1                               // ecx = order div 2
 test ecx, ecx                             // set flags according to ecx
 jz   @done                                // exit if filter order = 0

 fld  [self.FGainFactorSquared]            // FFilterGain
 fld  [self.FHalfPiOrderInv]               // Pi / (2 * FOrder), FFilterGain
 fld  [self.FTanW0Half]                    // K, Pi / (2 * FOrder), FFilterGain

 mov  ecx, [self.FOrder]                   // ecx = order
 @OrderLoop:
  // calculate t = cos((i * 2 + 1) * Pi2ndOrder);
  mov edx, ecx                             // edx = 2 * i
  dec edx                                  // edx = 2 * i + 1
  mov [esp - 4], edx                       // edx to stack
  dec edx                                  // edx = 2 * i
  fild [esp - 4].Integer                   // edx in st(0) = 2 * i + 1, K, Pi / (2 * FOrder), FFilterGain
  fmul st(0), st(2)                        // (2 * i + 1) * Pi / (2 * Order), K, Pi / (2 * FOrder), FFilterGain
  fcos                                     // t = cos((2 * i + 1) * Pi / (2 * Order)), K, Pi / (2 * FOrder), FFilterGain

  // calculate t1 = 1 / (FRippleFactors[0] - sqr(t));
  fld  st(0)                               // t, t, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(0)                        // t², t, K, Pi / (2 * Order), FFilterGain
  fld  [self.FRippleFactors].Double        // FRippleFactors[0], t², t, K, Pi / (2 * Order), FFilterGain
  fsubrp                                   // FRippleFactors[0] - t², t, K, Pi / (2 * Order), FFilterGain
  fld1                                     // 1, FRippleFactors[0] - t², t, K, Pi / (2 * Order), FFilterGain
  fdivrp                                   // t1 = 1 / (FRippleFactors[0] - t²), t, K, Pi / (2 * Order), FFilterGain

  // calculate t2 = 2 * t * t1 * K * FRippleFactors[1];
  fxch                                     // t, t1, K, Pi / (2 * Order), FFilterGain
  fadd st(0), st(0)                        // 2 * t, t1, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(1)                        // 2 * t * t1, t1, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(2)                        // 2 * t * t1 * K, t1, K, Pi / (2 * Order), FFilterGain
  fmul [self.FRippleFactors + 8].Double    // t2 = FRippleFactors[1]* 2 * t * t1 * K, t1, K, Pi / (2 * Order), FFilterGain

  // calculate t = 1 / (t2 + 1 + t1 * K²)
  fld st(1)                                // t1, t2, t1, K, Pi / (2 * Order), FFilterGain
  fmul st(0), st(3)                        // t1 * K, t2, t1, K, K², Pi / (2 * Order), FFilterGain
  fmul st(0), st(3)                        // t1 * K², t2, t1, K, Pi / (2 * Order), FFilterGain
  fld1                                     // 1, t1 * K², t2, t1, K, Pi / (2 * Order), FFilterGain
  faddp                                    // 1 + t1 * K², t2, t1, K, Pi / (2 * Order), FFilterGain
  fadd st(0),st(1)                         // 1 + t1 * K² + t2, t2, t1, K, Pi / (2 * Order), FFilterGain
  fld1                                     // 1, 1 + t1 * K² + t2, t2, t1, K, Pi / (2 * Order), FFilterGain
  fdivrp                                   // t = 1 / (1 + t1 * K² + t2), t2, t1, K, Pi / (2 * Order), FFilterGain

  // FFilterGain = FFilterGain * t;
  fmul st(5), st(0)                        // t = 1 / (1 + t1 * K² + t2), t2, t1, K, Pi / (2 * Order), FFilterGain * t

  // calculate Coeff[0] := 2 * (1 - t1 * K²) * t
  fld st(2)                                // t1, t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(4)                        // t1 * K, t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(4)                        // t1 * K², t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fld1                                     // 1, t1 * K², t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fsubrp                                   // 1 - t1 * K², t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fadd st(0), st(0)                        // 2 * (1 - t1 * K²), t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(1)                        // 2 * (1 - t1 * K²) * t, t, t2, t1, K, Pi / (2 * Order), FFilterGain * t
  fstp [self.FCoeffs + 8 * edx].Double     // store to FCoeffs[2 * i]

  // calculate Coeff[1] = (t2 - 1 - t1 * K²) * t;
  fxch st(2)                               // t1, t2, t, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(3)                        // t1 * K, t2, t, K, Pi / (2 * Order), FFilterGain * t
  fmul st(0), st(3)                        // t1 * K², t2, t, K, Pi / (2 * Order), FFilterGain * t
  fld1                                     // 1, t1 * K², t2, t, K, Pi / (2 * Order), FFilterGain * t
  faddp                                    // 1 + t1 * K², t2, t, K, Pi / (2 * Order), FFilterGain * t
  fsubp                                    // t2 - (1 + t1 * K²), t, K, Pi / (2 * Order), FFilterGain * t
  fmulp                                    // (t2 - (1 + t1 * K²)) * t, K, Pi / (2 * Order), FFilterGain * t
  fstp [self.FCoeffs + 8 * edx + 8].Double // store to FCoeffs[2 * i + 1], Pi / (2 * Order), FFilterGain * t
  sub ecx, 2
 jnz @OrderLoop
 fstp st(0)                                // Pi / (2 * Order), FFilterGain * t
 fstp st(0)                                // FFilterGain * t
 fstp [self.FFilterGain].Double            // stack free!

@done:
{$ENDIF}
end;

function TChebyshev1HighpassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * cos(2 * Frequency * pi * fSRR);
 a      := sqr(cw - 2);
 Result := sqr(FFilterGain);

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a /
  (1 + sqr(FCoeffs[2 * i]) + sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
   cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
end;

{ TChebyshev1HighpassFilterAutomatable }

procedure TChebyshev1HighpassFilterAutomatable.CalculateCoefficients;
var
  K, K2      : Double;
  t, t1, t2  : Double;
  i          : Integer;
  Cmplx      : TComplexDouble;
begin
 if FOrder = 0 then exit;
 K  := FTanW0Half;
 K2 := sqr(K);
 Cmplx := FCosOrderOffset;
 FFilterGain := FGainFactorSquared;
 for i := (FOrder div 2) - 1 downto 0 do
  begin
   t  := Cmplx.Re;
   ComplexMultiply2Inplace(Cmplx, FCosOrderOffset);
   t1 := 1 / (FRippleFactors[0] - sqr(t));
   t2 := 2 * t * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + 1 + t1 * K2);
   FFilterGain := FFilterGain * t;
   FCoeffs[2 * i    ] := 2 * (     1 - t1 * K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - 1 - t1 * K2) * t;
  end;
end;

procedure TChebyshev1HighpassFilterAutomatable.CalculateRippleFactors;
var
  t : array [0..1] of Single;
const
  CExp1: Single = 0.69314718055994530941723212145818;
begin
 t[0] := 1 / FastSqrtBab1(sqr(FRippleFactor) - 1);
 t[0] := FastLog2MinError3(t[0] + FastSqrtBab1(sqr(t[0]) + 1)) * FOrderInv;
 t[1] := FastPower2MinError3(t[0]);
 t[0] := 1 / t[1];
 FRippleFactors[1] := (t[1] - t[0]) * CHalf32;
 FRippleFactors[0] := sqr((t[1] + t[0]) * CHalf32);
end;

procedure TChebyshev1HighpassFilterAutomatable.CalculateW0;
begin
 FTanW0Half := FastTan2Term(2 * Pi * FSRR * (FFrequency * FDownsampleFak) * CHalf64);
end;

function TChebyshev1HighpassFilterAutomatable.MagnitudeSquared(
  const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * FastCosInBounds3Term(2 * Frequency * pi * fSRR);
 a      := sqr(cw - 2);
 Result := sqr(FFilterGain);

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a /
  (1 + sqr(FCoeffs[2 * i]) + sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
   cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
end;

procedure TChebyshev1HighpassFilterAutomatable.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   FHalfPiOrderInv := Pi * CHalf64 * FOrderInv;
   GetSinCos(FHalfPiOrderInv, FCosOrderOffset.Im, FCosOrderOffset.Re);
   CalculateRippleFactors;
   ResetStates;
   CalculateCoefficients;
  end else FOrderInv := 1;
end;

procedure TChebyshev1HighpassFilterAutomatable.RippleChanged;
begin
 FRippleFactor := FastdBtoAmpMinError3(FRipple);
 inherited;
end;

{ TCustomChebyshev2Filter }

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
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   FHalfPiOrderInv := Pi * CHalf64 * FOrderInv;
   CalculateRippleFactors;
   CalculateCoefficients;
   ResetStates;
  end else FOrderInv := 1;
 inherited;
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
 t := arcsinh(1 / sqrt(Power(10, (FRipple * 0.1)) - 1)) * FOrderInv;
 FRippleFactors[1] := sinh(t);
 FRippleFactors[0] := sqr(cosh(t));
end;

procedure TCustomChebyshev2Filter.CalculateW0;
begin
 inherited;
 FW0 := 2 * Pi * fSRR * (FFrequency);
 FSinW0 := sin(FW0);
 if FW0 > 3.1 then FW0 := 3.1;
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
