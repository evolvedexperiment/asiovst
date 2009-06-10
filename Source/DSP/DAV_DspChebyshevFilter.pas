unit DAV_DspChebyshevFilter;

interface

{$I ..\DAV_Compiler.inc}
{-$DEFINE PUREPASCAL}

uses
  DAV_Common, DAV_DspFilter, DAV_Complex;

type
  TCustomChebyshevFilterClass = class of TCustomChebyshevFilter;
  TCustomChebyshevFilter = class(TCustomOrderFilter)
  private
    function GetRipple: Double;
  protected
    FRipple        : Double;
    FRippleGain    : Double;
    FRippleFactors : TDAV2DoubleArray;
    FFilterGain    : Double;
    FTanW0Half     : Double;
    FOrderInv      : Double;
    FExpOrdPiHalf  : TComplexDouble;
    procedure CalculateW0; override;
    procedure SetRipple(const Value: Double); virtual;
    procedure RippleChanged; virtual;
    procedure CalculateRippleFactors; virtual; abstract;
    procedure OrderChanged; override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure ResetStatesInt64; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;

    property Ripple : Double read GetRipple write SetRipple;
  end;

  TCustomDownsampledChebyshevFilter = class(TCustomChebyshevFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow : Integer;
    FDownsampleFak : Integer;
    procedure CalculateW0; override;
  public
    constructor Create(const Order: Integer = 0); override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

  // Chebyshev Type I

  TCustomChebyshev1Filter = class(TCustomDownsampledChebyshevFilter)
  protected
    FCoeffs        : array [0..63] of Double;
    FState         : array [0..63] of Double;
    FStateStack    : array of array [0..63] of Double;
    procedure CalculateRippleFactors; override;
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
    procedure CalculateW0; override;
    procedure CalculateRippleFactors; override;
    procedure OrderChanged; override;
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev1LowCutFilterAutomatable = TChebyshev1HighpassFilterAutomatable;


  // Chebyshev Type II

  TCustomChebyshev2Filter = class(TCustomChebyshevFilter)
  protected
    FCoeffs        : array [0..63] of Double;
    FState         : array [0..63] of Double;
    FStateStack    : array of array [0..63] of Double;
    procedure CalculateRippleFactors; override;
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
  end;

  TCustomChebyshev2LowpassFilter = class(TCustomChebyshev2Filter)
  public
    function ProcessSample(const Input: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
    function Phase(const Frequency: Double): Double; override;
  end;

  TChebyshev2LowpassFilter = class(TCustomChebyshev2LowpassFilter)
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev2HighCutFilter = TChebyshev2LowpassFilter;

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
 FOrder  := Order;
 FRipple := 1;
 OrderChanged;
 inherited Create(Order);
end;

procedure TCustomChebyshevFilter.CalculateW0;
begin
 // inherited; FTanW0Half := FExpW0.Im / (1 + FExpW0.Re);
 FW0 := Pi * FSRR * FFrequency;
 FTanW0Half := tan(FW0);
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

procedure TCustomChebyshevFilter.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
   CalculateRippleFactors;
   ResetStates;
   inherited;
  end
 else
  begin
   FOrderInv := 1;
   FFilterGain := 1;
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

procedure TCustomChebyshevFilter.SetRipple(const Value: Double);
begin
 if Value <> FRipple then
  begin
   FRipple := Value;
   RippleChanged;
  end;
end;

{ TCustomDownsampledChebyshevFilter }

constructor TCustomDownsampledChebyshevFilter.Create(const Order: Integer = 0);
begin
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 inherited Create(Order);
end;

procedure TCustomDownsampledChebyshevFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TCustomDownsampledChebyshevFilter.CalculateW0;
begin
 // inherited; FTanW0Half := FExpW0.Im / (1 + FExpW0.Re);
 FW0 := Pi * FSRR * FFrequency * FDownsampleFak;
 FTanW0Half := tan(FW0);
end;

{ TCustomChebyshev1Filter }

constructor TCustomChebyshev1Filter.Create(const Order: Integer = 0);
begin
 FFilterGain := 1;
 inherited Create(Order);
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

procedure TCustomChebyshev1Filter.CalculateRippleFactors;
var
  t : array [0..1] of Double;
begin
 FRippleGain := dB_to_Amp(FRipple);
 t[0] := arcsinh(1 / sqrt(sqr(FRippleGain) - 1));
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
{.$DEFINE PUREPASCAL}
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=     x + FState[2 * i];
   FState[2 * i    ] := 2 * x + FCoeffs[2 * i] * Result + FState[2 * i + 1];
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
 fld Input.Double;
 {$IFDEF HandleDenormals}
 fadd CDenorm32
 {$ENDIF}
 fmul  [eax.FFilterGain].Double
 mov   ecx, [eax.FOrder]
 test  ecx, ecx
 jz    @End
 shr   ecx, 1
 shl   ecx, 2
 push  ecx
 jz @SingleStage
 @FilterLoop:
  sub   ecx, 4
  fld   st(0)
  fadd  [eax.FState + ecx * 4].Double
  fld   st(0)
  fld   st(0)
  fmul  [eax.FCoeffs + ecx * 4].Double
  fadd  [eax.FState + ecx * 4 + 8].Double
  fld   st(3)
  fadd  st(0), st(0)
  faddp
  fstp  [eax.FState + ecx * 4].Double
  fmul  [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch  st(2)
  faddp
  fstp  [eax.FState + ecx * 4 + 8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [eax.FOrder]
 jz @End
  mov ecx, [eax.FOrder]
  dec ecx
  shl ecx, 1
  fld st(0)
  fadd [eax.FState + ecx * 4].Double
  fld st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  faddp st(2), st(0)
  fxch
  fstp [eax.FState + ecx * 4].Double
 @End:
 {$ENDIF}
end;


{ TChebyshev1LowpassFilter }

procedure TChebyshev1LowpassFilter.CalculateCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
  Cmplx     : TComplexDouble;
begin
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t1 := K * FRippleFactors[1];
   t  := 1 / (1 + t1);
   FFilterGain := FRippleGain * FFilterGain * t * t1;
   FCoeffs[FOrder - 1] := (1 - t1) * t;
   ComplexMultiplyInplace(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t1 := 1 / (FRippleFactors[0] - sqr(Cmplx.Re));
   t2 := 2 * Cmplx.Re * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + t1 + K2);
   FFilterGain := FFilterGain * K2 * t;
   FCoeffs[2 * i    ] := 2 * (     t1 - K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - t1 - K2) * t;

   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
  end;
{$ELSE}
asm
 mov  ecx, [self.FOrder]                    // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz   @done                                 // exit if filter order = 0

 fld  [self.FGainFactorSquared]             // FFilterGain
 fld  [self.FTanW0Half]                     // K, FFilterGain
 fld  [self.FExpOrdPiHalf.Im]               // Im(E') := A.Im, K, FFilterGain
 fld  [self.FExpOrdPiHalf.Re]               // Re(E') := A.Re, A.Im, K, FFilterGain

 mov  ecx, [self.FOrder]                    // ecx = order

 test  ecx, 1
 jz @OrderLoop

  fld  [self.FExpOrdPiHalf.Re]              // B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B.Re - A.Im * B.Im := New A.Re, A.Re, A.Im, K, FFilterGain

  fxch st(2)                                // A.Im, A.Re, New A.Re, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Re]              // A.Im * B.Re, A.Re, New A.Re, K, FFilterGain
  fxch st(1)                                // A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  fmul  [self.FExpOrdPiHalf.Im]             // B.Im * A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  faddp                                     // B.Im * A.Re, A.Im * B.Re := New A.Im, New A.Re, K, FFilterGain
  fxch                                      // New A.Re, New A.Im, K, FFilterGain

  fld  [self.FRippleFactors + 8].Double     // FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain

  fld1                                      // 1, K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // 1 + K * FRippleFactors[1], K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, 1 + K * FRippleFactors[1], K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fdivrp                                    // 1 / (1 + K * FRippleFactors[1]) := t, K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain

  fmul st(5), st(0)                         // t, K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain * t
  fxch st(1)                                // K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t
  fmul st(5), st(0)                         // K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t * K * FRippleFactors[1]

  fld1                                      // 1, K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t
  fsubrp                                    // 1 - K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t
  fmulp                                     // (1 - K * FRippleFactors[1]) * t, A.Re, A.Im, K, FFilterGain * t

  fstp [self.FCoeffs + 8 * ecx - 8].Double  // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t * K�

  fxch st(3)                                // FFilterGain * t * K�, A.Im, K, A.Re
  fmul  [self.FRippleGain].Double           // FRippleGain * FFilterGain * t * K�, A.Im, K, A.Re
  fxch st(3)                                // A.Im, K, A.Re, FRippleGain * FFilterGain * t * K�


 dec ecx
 jz @clean

 @OrderLoop:
  // calculate t1 = 1 / (FRippleFactors[0] - sqr(A.Re));
  fld  st(0)                                // A.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re�, A.Re, A.Im, K, FFilterGain
  fld  [self.FRippleFactors].Double         // FRippleFactors[0], A.Re�, A.Re, A.Im, K, FFilterGain
  fsubrp                                    // FRippleFactors[0] - A.Re�, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, FRippleFactors[0] - A.Re�, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t1 = 1 / (FRippleFactors[0] - A.Re�), A.Re, A.Im, K, FFilterGain

  // calculate t2 = 2 * A.Re * t1 * K * FRippleFactors[1];
  fld st(1)                                 // A.Re, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * A.Re, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // 2 * A.Re * t1, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(4)                         // 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain
  fmul [self.FRippleFactors + 8].Double     // t2 = FRippleFactors[1]* 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain

  // calculate t = 1 / (t2 + K� + t1);
  fld  st(4)                                // K, t2, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // K�, t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // K� + t2, t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(2)                         // K� + t2 + t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, K� + t2 + t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t = 1 / (K� + t2 + t1), t2, t1, A.Re, A.Im, K, FFilterGain

  // FFilterGain = FFilterGain * t;
  fmul st(6), st(0)                        // t, t2, t1, A.Re, A.Im, K, FFilterGain * t
  fxch st(5)                               // K, t2, t1, A.Re, A.Im, t, FFilterGain * t
  fmul st(6), st(0)                        // K, t2, t1, A.Re, A.Im, t, FFilterGain * t * K
  fmul st(6), st(0)                        // K, t2, t1, A.Re, A.Im, t, FFilterGain * t * K�
  fxch st(5)                               // t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�

  // calculate Coeff[0] = 2 * (t1 - K2) * t
  fld  st(5)                                // K, t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fmul st(0), st(0)                         // K�, t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fsubr st(0), st(3)                        // t1 - K�, t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fmul st(0), st(1)                         // t * (t1 - K�), t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fadd st(0), st(0)                         // 2 * t * (t1 - K�), t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fstp [self.FCoeffs + 8 * ecx - 16].Double // store to FCoeffs[2 * i]

  // calculate Coeff[1] = (t2 - t1 - K2) * t;
  fxch st(2)                                // t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  fld  st(5)                                // K, t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  fmul st(0), st(0)                         // K�, t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  faddp                                     // t1 + K�, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  fsubp                                     // t2 - t1 - K�, t, A.Re, A.Im, K, FFilterGain * t * K�
  fmulp                                     // (t2 - t1 - K�) * t, A.Re, A.Im, K, FFilterGain * t * K�
  fstp [self.FCoeffs + 8 * ecx - 8].Double  // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t * K�

  // advance complex
  fld  [self.FExpOrdPiHalf.Re]              // B.Re, A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, B.Re, A.Re, A.Im, K, FFilterGain
  fmulp                                     // B.Im * B.Re, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * B.Im * B.Re = B'', A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Re]              // B.Re, B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Re�, B'', A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, B.Re�, B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Im�, B.Re�, B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // B.Im� + B.Re� = B', B'', A.Re, A.Im, K, FFilterGain
  fld st(2)                                 // A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fld st(4)                                 // A.Im, A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B'', A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B' - A.Im * B'' := New A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fxch st(4)                                // A.Im, B', B'', A.Re, New A.Re, K, FFilterGain
  fmulp                                     // A.Im * B', B'', A.Re, New A.Re, K, FFilterGain
  fxch st(2)                                // A.Re, B'', A.Im * B', New A.Re, K, FFilterGain
  fmulp                                     // A.Re * B'', A.Im * B', New A.Re, K, FFilterGain
  faddp                                     // A.Re * B'' + A.Im * B' := New A.Im, New A.Re, K, FFilterGain
  fxch st(1)                                // New A.Re, New A.Im, K, FFilterGain

  // advance
  sub  ecx, 2
 ja  @OrderLoop

@clean:
 fstp st(0)                                 // New A.Im, K, FFilterGain * t * K�
 fstp st(0)                                 // K, FFilterGain  * t * K�
 fstp st(0)                                 // FFilterGain * t * K�
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
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
  Cmplx     : TComplexDouble;
begin
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t1 := 1 / FRippleFactors[1];
   t  := 1 / (t1 + K);
   FFilterGain := FRippleGain * FFilterGain * t * K;
   FCoeffs[FOrder - 1] := (t1 - K) * t;
   ComplexMultiplyInplace(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t1 := 1 / (FRippleFactors[0] - sqr(Cmplx.Re));
   t2 := 2 * Cmplx.Re * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + t1 + K2);
   FFilterGain := FFilterGain * K2 * t;
   FCoeffs[2 * i    ] := 2 * (     t1 - K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - t1 - K2) * t;

   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
  end;
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateRippleFactors;
var
  t : array [0..1] of Single;
begin
 FRippleGain := FastdBtoAmpMinError3(FRipple);
 t[0] := 1 / FastSqrtBab1(sqr(FRippleGain) - 1);
 t[0] := FastLog2MinError3(t[0] + FastSqrtBab1(sqr(t[0]) + 1)) * FOrderInv;
 t[1] := FastPower2MinError3(t[0]) * CHalf32;
 t[0] := CQuarter32 / t[1];
 FRippleFactors[1] := t[1] - t[0];
 FRippleFactors[0] := sqr(t[1] + t[0]);
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateW0;
begin
 FTanW0Half := FastTan2Term(Pi * FSRR * (FFrequency * FDownsampleFak));
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
   GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
   CalculateRippleFactors;
   ResetStates;
   CalculateCoefficients;
  end else FOrderInv := 1;
end;


{ TChebyshev1HighpassFilter }

procedure TChebyshev1HighpassFilter.CalculateCoefficients;
{$DEFINE PUREPASCAL}
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
  Cmplx     : TComplexDouble;
begin
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t1 := 1 / FRippleFactors[1];
   t  := 1 / (1 + t1 * K);
   FFilterGain := FRippleGain * FFilterGain * t;
   FCoeffs[FOrder - 1] := (1 - t1 * K) * t;
   ComplexMultiplyInplace(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t1 := 1 / (FRippleFactors[0] - sqr(Cmplx.Re));
   t2 := 2 * Cmplx.Re * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + 1 + t1 * K2);
   FFilterGain := FFilterGain * t;
   FCoeffs[2 * i    ] := 2 * (     1 - t1 * K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - 1 - t1 * K2) * t;

   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
  end;
{$ELSE}
asm
 mov  ecx, [self.FOrder]                    // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz   @done                                 // exit if filter order = 0

 fld  [self.FGainFactorSquared]             // FFilterGain
 fld  [self.FTanW0Half]                     // K, FFilterGain
 fld  [self.FExpOrdPiHalf.Im]               // Im(E') := A.Im, K, FFilterGain
 fld  [self.FExpOrdPiHalf.Re]               // Re(E') := A.Re, A.Im, K, FFilterGain

 mov  ecx, [self.FOrder]                    // ecx = order

 test  ecx, 1
 jz @OrderLoop

  fld  [self.FExpOrdPiHalf.Re]              // B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B.Re - A.Im * B.Im := New A.Re, A.Re, A.Im, K, FFilterGain

  fxch st(2)                                // A.Im, A.Re, New A.Re, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Re]              // A.Im * B.Re, A.Re, New A.Re, K, FFilterGain
  fxch st(1)                                // A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  fmul  [self.FExpOrdPiHalf.Im]             // B.Im * A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  faddp                                     // B.Im * A.Re, A.Im * B.Re := New A.Im, New A.Re, K, FFilterGain
  fxch                                      // New A.Re, New A.Im, K, FFilterGain

  fld  [self.FRippleFactors + 8].Double     // FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain

  fld1                                      // 1, K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // 1 + K * FRippleFactors[1], K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, 1 + K * FRippleFactors[1], K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain
  fdivrp                                    // 1 / (1 + K * FRippleFactors[1]) := t, K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain

  fmul st(5), st(0)                         // t, K * FRippleFactors[1], A.Re, A.Im, K, FFilterGain * t
  fxch st(1)                                // K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t
  fmul st(5), st(0)                         // K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t * K * FRippleFactors[1]

  fld1                                      // 1, K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t
  fsubrp                                    // 1 - K * FRippleFactors[1], t, A.Re, A.Im, K, FFilterGain * t
  fmulp                                     // (1 - K * FRippleFactors[1]) * t, A.Re, A.Im, K, FFilterGain * t

  fstp [self.FCoeffs + 8 * ecx - 8].Double  // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t * K�

  fxch st(3)                                // FFilterGain * t * K�, A.Im, K, A.Re
  fmul  [self.FRippleGain].Double           // FRippleGain * FFilterGain * t * K�, A.Im, K, A.Re
  fxch st(3)                                // A.Im, K, A.Re, FRippleGain * FFilterGain * t * K�


 dec ecx
 jz @clean

 @OrderLoop:
  // calculate t1 = 1 / (FRippleFactors[0] - sqr(A.Re));
  fld  st(0)                                // A.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re�, A.Re, A.Im, K, FFilterGain
  fld  [self.FRippleFactors].Double         // FRippleFactors[0], A.Re�, A.Re, A.Im, K, FFilterGain
  fsubrp                                    // FRippleFactors[0] - A.Re�, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, FRippleFactors[0] - A.Re�, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t1 = 1 / (FRippleFactors[0] - A.Re�), A.Re, A.Im, K, FFilterGain

  // calculate t2 = 2 * A.Re * t1 * K * FRippleFactors[1];
  fld st(1)                                 // A.Re, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * A.Re, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // 2 * A.Re * t1, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(4)                         // 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain
  fmul [self.FRippleFactors + 8].Double     // t2 = FRippleFactors[1]* 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain

  // calculate t = 1 / (t2 + 1 + t1 * K�)
  fld st(1)                                 // t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(5)                         // t1 * K, t2, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(5)                         // t1 * K�, t2, t1, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, t1 * K�, t2, t1, A.Re, A.Im, K, FFilterGain
  faddp                                     // 1 + t1 * K�, t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // 1 + t1 * K� + t2, t2, t1, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, 1 + t1 * K� + t2, t2, t1, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t = 1 / (1 + t1 * K� + t2), t2, t1, A.Re, A.Im, K, FFilterGain

  // FFilterGain = FFilterGain * t;
  fmul st(6), st(0)                         // t = 1 / (1 + t1 * K� + t2), t2, t1, A.Re, A.Im, K, FFilterGain * t

  // calculate t1 * K2
  fxch st(2)                                // t1, t2, t, A.Re, A.Im, K, FFilterGain * t
  fmul st(0), st(5)                         // t1 * K, t2, t, A.Re, A.Im, K, FFilterGain * t
  fmul st(0), st(5)                         // t1 * K�, t2, t, A.Re, A.Im, K, FFilterGain * t
  fxch st(2)                                // t, t2, t1 * K�, A.Re, A.Im, K, FFilterGain * t

  // calculate Coeff[0] := 2 * (1 - t1 * K�) * t
  fld1                                      // 1, t, t2, t1 * K�, A.Re, A.Im, K, FFilterGain * t
  fsub st(0), st(3)                         // 1 - t1 * K�, t, t2, t1 * K�, A.Re, A.Im, K, FFilterGain * t
  fadd st(0), st(0)                         // 2 * (1 - t1 * K�), t, t2, t1 * K�, A.Re, A.Im, K, FFilterGain * t
  fmul st(0), st(1)                         // 2 * (1 - t1 * K�) * t, t, t2, t1 * K�, A.Re, A.Im, K, FFilterGain * t
  fstp [self.FCoeffs + 8 * ecx - 16].Double // store to FCoeffs[2 * i]

  // calculate Coeff[1] = (t2 - 1 - t1 * K�) * t;
  fxch st(2)                                // t1 * K�, t2, t, A.Re, A.Im, K, FFilterGain * t
  fld1                                      // 1, t1 * K�, t2, t, A.Re, A.Im, K, FFilterGain * t
  faddp                                     // 1 + t1 * K�, t2, t, A.Re, A.Im, K, FFilterGain * t
  fsubp                                     // t2 - (1 + t1 * K�), t, A.Re, A.Im, K, FFilterGain * t
  fmulp                                     // (t2 - (1 + t1 * K�)) * t, A.Re, A.Im, K, FFilterGain * t
  fstp [self.FCoeffs + 8 * ecx - 8].Double  // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t

  // advance complex
  fld  [self.FExpOrdPiHalf.Re]              // B.Re, A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, B.Re, A.Re, A.Im, K, FFilterGain
  fmulp                                     // B.Im * B.Re, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * B.Im * B.Re = B'', A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Re]              // B.Re, B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Re�, B'', A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, B.Re�, B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Im�, B.Re�, B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // B.Im� + B.Re� = B', B'', A.Re, A.Im, K, FFilterGain
  fld st(2)                                 // A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fld st(4)                                 // A.Im, A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B'', A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B' - A.Im * B'' := New A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fxch st(4)                                // A.Im, B', B'', A.Re, New A.Re, K, FFilterGain
  fmulp                                     // A.Im * B', B'', A.Re, New A.Re, K, FFilterGain
  fxch st(2)                                // A.Re, B'', A.Im * B', New A.Re, K, FFilterGain
  fmulp                                     // A.Re * B'', A.Im * B', New A.Re, K, FFilterGain
  faddp                                     // A.Re * B'' + A.Im * B' := New A.Im, New A.Re, K, FFilterGain
  fxch st(1)                                // New A.Re, New A.Im, K, FFilterGain

  // advance
  sub  ecx, 2
 jnz @OrderLoop

@clean:
 fstp st(0)                                 // New A.Im, K, FFilterGain * t * K�
 fstp st(0)                                 // K, FFilterGain  * t * K�
 fstp st(0)                                 // FFilterGain * t * K�
 fstp [self.FFilterGain].Double             // stack free!

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
 Cmplx := FExpOrdPiHalf;
 FFilterGain := FGainFactorSquared;
 for i := (FOrder div 2) - 1 downto 0 do
  begin
   t  := Cmplx.Re;
   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
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
 FRippleGain := FastdBtoAmpMinError3(FRipple);
 t[0] := 1 / FastSqrtBab1(sqr(FRippleGain) - 1);
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

function TChebyshev1HighpassFilterAutomatable.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * FastCosInBounds4Term(2 * Frequency * pi * fSRR);
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
   GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
   CalculateRippleFactors;
   ResetStates;
   CalculateCoefficients;
  end else FOrderInv := 1;
end;

{ TCustomChebyshev2Filter }

constructor TCustomChebyshev2Filter.Create(const Order: Integer = 0);
begin
 FFilterGain := 1;
 inherited Create(Order);
end;

class function TCustomChebyshev2Filter.GetMaxOrder: Cardinal;
begin
 result := 32;
end;

procedure TCustomChebyshev2Filter.Reset;
begin
 Gain := 0;
end;

procedure TCustomChebyshev2Filter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomChebyshev2Filter.CalculateRippleFactors;
var
  t : array [0..1] of Double;
begin
 t[0] := arcsinh(1 / sqrt(Power(10, (FRipple * 0.1)) - 1));
 t[1] := Exp(-t[0] * FOrderInv) * CHalf64;
 t[0] := CQuarter64 / t[1];
 FRippleFactors[1] := t[0] - t[1];
 FRippleFactors[0] := sqr(t[0] + t[1]);
end;

procedure TCustomChebyshev2Filter.SetFilterValues(const AFrequency, AGain, ARipple : Single);
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

function TCustomChebyshev2Filter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

procedure TCustomChebyshev2Filter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0],FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TCustomChebyshev2Filter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0], FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

function TCustomChebyshev2Filter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 10 * Log10(MagnitudeSquared(Frequency));
end;


{ TCustomChebyshev2LowpassFilter }

procedure TCustomChebyshev2LowpassFilter.Complex(const Frequency: Double;
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

function TCustomChebyshev2LowpassFilter.Phase(const Frequency: Double): Double;
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

function TCustomChebyshev2LowpassFilter.ProcessSample(const Input: Double): Double;
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

{ TChebyshev2LowpassFilter }
{$DEFINE PUREPASCAL}

procedure TChebyshev2LowpassFilter.CalculateCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
  Cmplx     : TComplexDouble;
begin
 if FOrder = 0 then exit;
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FExpOrdPiHalf;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t1 := 1 / (FRippleFactors[0] - sqr(Cmplx.Re));
   t2 := 2 * Cmplx.Re * t1 * K * FRippleFactors[1];
   t  := 1 / (t2 + K2 + t1);
   FFilterGain := FFilterGain * K2 * t;
   FCoeffs[2 * i    ] := 2 * (     t1 - K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - t1 - K2) * t;

   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
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
 fld  [self.FTanW0Half]                     // K, FFilterGain
 fld  [self.FExpOrdPiHalf.Im]               // Im(E') := A.Im, K, FFilterGain
 fld  [self.FExpOrdPiHalf.Re]               // Re(E') := A.Re, A.Im, K, FFilterGain

 mov  ecx, [self.FOrder]                    // ecx = order
 @OrderLoop:
  // calculate t1 = 1 / (FRippleFactors[0] - sqr(A.Re));
  fld  st(0)                                // A.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re�, A.Re, A.Im, K, FFilterGain
  fld  [self.FRippleFactors].Double         // FRippleFactors[0], A.Re�, A.Re, A.Im, K, FFilterGain
  fsubrp                                    // FRippleFactors[0] - A.Re�, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, FRippleFactors[0] - A.Re�, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t1 = 1 / (FRippleFactors[0] - A.Re�), A.Re, A.Im, K, FFilterGain

  // calculate t2 = 2 * A.Re * t1 * K * FRippleFactors[1];
  fld st(1)                                 // A.Re, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * A.Re, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // 2 * A.Re * t1, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(4)                         // 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain
  fmul [self.FRippleFactors + 8].Double     // t2 = FRippleFactors[1]* 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain

  // calculate t = 1 / (t2 + K� + t1);
  fld  st(4)                                // K, t2, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // K�, t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // K� + t2, t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(2)                         // K� + t2 + t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, K� + t2 + t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t = 1 / (K� + t2 + t1), t2, t1, A.Re, A.Im, K, FFilterGain

  // FFilterGain = FFilterGain * t;
  fmul st(6), st(0)                        // t, t2, t1, A.Re, A.Im, K, FFilterGain * t
  fxch st(5)                               // K, t2, t1, A.Re, A.Im, t, FFilterGain * t
  fmul st(6), st(0)                        // K, t2, t1, A.Re, A.Im, t, FFilterGain * t * K
  fmul st(6), st(0)                        // K, t2, t1, A.Re, A.Im, t, FFilterGain * t * K�
  fxch st(5)                               // t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�

  // calculate Coeff[0] = 2 * (t1 - K2) * t
  fld  st(5)                                // K, t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fmul st(0), st(0)                         // K�, t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fsubr st(0), st(3)                        // t1 - K�, t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fmul st(0), st(1)                         // t * (t1 - K�), t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fadd st(0), st(0)                         // 2 * t * (t1 - K�), t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K�
  fstp [self.FCoeffs + 8 * ecx - 16].Double // store to FCoeffs[2 * i]

  // calculate Coeff[1] = (t2 - t1 - K2) * t;
  fxch st(2)                                // t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  fld  st(5)                                // K, t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  fmul st(0), st(0)                         // K�, t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  faddp                                     // t1 + K�, t2, t, A.Re, A.Im, K, FFilterGain * t * K�
  fsubp                                     // t2 - t1 - K�, t, A.Re, A.Im, K, FFilterGain * t * K�
  fmulp                                     // (t2 - t1 - K�) * t, A.Re, A.Im, K, FFilterGain * t * K�
  fstp [self.FCoeffs + 8 * ecx - 8].Double  // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t * K�

  // advance complex
  fld  st(0)                                // A.Re, A.Re, A.Im, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Re]              // A.Re * Re(E'), A.Re, A.Im, K, FFilterGain
  fld  st(2)                                // A.Im, A.Re * Re(E'), A.Re, A.Im, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Im]              // A.Im * Im(E'), A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * Re(E') - A.Im * Im(E'), A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * (A.Re * Re(E') - A.Im * Im(E')), A.Re, A.Im, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Re]              // 2 * Re(E') * (A.Re * Re(E') - A.Im * Im(E')), A.Re, A.Im, K, FFilterGain
  fsub st(0), st(1)                         // 2 * Re(E') * (A.Re * Re(E') - A.Im * Im(E')) - A.Re := New A.Re, A.Re, A.Im, K, FFilterGain

  fxch st(2)                                // A.Im, A.Re, New A.Re, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Re]              // A.Im * Re(E'), A.Re, New A.Re, K, FFilterGain
  fld  st(1)                                // A.Re, A.Im * Re(E'), A.Re, New A.Re, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Im]              // A.Re * Im(E'), A.Im * Re(E'), A.Re, New A.Re, K, FFilterGain
  faddp                                     // A.Im * Re(E') + A.Re * Im(E'), A.Re, New A.Re, K, FFilterGain
  fadd st(0), st(0)                         // 2 * (A.Im * Re(E') + A.Re * Im(E')), A.Re, New A.Re, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Re]              // 2 * Re(E') * (A.Im * Re(E') + A.Re * Im(E')), A.Re, New A.Re, K, FFilterGain
  fsubrp                                    // 2 * Re(E') * (A.Im * Re(E') + A.Re * Im(E')) - A.Re := New A.Im, New A.Re, K, FFilterGain
  fxch st(1)                                // New A.Re, New A.Im, K, FFilterGain

  // advance
  sub  ecx, 2
 jnz  @OrderLoop
 fstp st(0)                                 // New A.Im, K, FFilterGain * t * K�
 fstp st(0)                                 // K, FFilterGain  * t * K�
 fstp st(0)                                 // FFilterGain * t * K�
 fstp [self.FFilterGain].Double             // stack free!

@done:
{$ENDIF}
end;

function TChebyshev2LowpassFilter.MagnitudeSquared(const Frequency: Double): Double;
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

end.
