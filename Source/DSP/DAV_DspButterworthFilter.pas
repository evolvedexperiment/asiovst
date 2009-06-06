unit DAV_DspButterworthFilter;

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  DAV_DspFilter, DAV_Common, DAV_Complex;

type
  TCustomButterworthFilterClass = class of TCustomButterworthFilter;
  TCustomButterworthFilter = class(TCustomOrderFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FFilterGain     : Double;
    FOrderInv       : Double;
    FPiHalfOrderInv : Double;
    FCosOrderOffset : TComplexDouble;
    FTanW0          : Double;
    FCoeffs         : array [0..63] of Double;
    FState          : array [0..63] of Double;
    FStateStack     : array of array [0.. 63] of Double;
    procedure CalculateW0; override;
    class function GetMaxOrder: Cardinal; override;
    procedure OrderChanged; override;
  public
    constructor Create(const Order: Integer = 0); override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    procedure ResetStates; override;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure ResetStatesInt64; override;
    function Imaginary(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

  TCustomButterworthLowPassFilter = class(TCustomButterworthFilter)
  public
    constructor Create(const Order: Integer = 0); override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;
  end;

  TCustomButterworthHighPassFilter = class(TCustomButterworthFilter)
  public
    constructor Create(const Order: Integer = 0); override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Single): Single; overload;
    function ProcessSample(const Input: Double): Double; overload; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;
  end;

  TCustomButterworthSplitBandFilter = class(TCustomButterworthFilter)
  protected
    FKs      : Double;
    FHPState : array [0..63] of Double;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure CalculateCoefficients; override;
    procedure ProcessSample(const Input: Double; out Lowpass, Highpass: Double); reintroduce; overload;
    procedure ProcessSample(const Input: Single; out Lowpass, Highpass: Single); reintroduce; overload;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;

  TButterworthHighPassFilter = class(TCustomButterworthHighPassFilter)
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;
  TButterworthLowCut = TCustomButterworthHighPassFilter;

  TButterworthLowPassFilter = class(TCustomButterworthLowPassFilter)
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;
  TButterworthHighCutFilter = TCustomButterworthLowPassFilter;

  TButterworthSplitBandFilter = class(TCustomButterworthSplitBandFilter)
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;

  TButterworthLowPassFilterAutomatable = class(TCustomButterworthLowPassFilter)
  private
  public
    procedure CalculateW0; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;

  TButterworthHighPassFilterAutomatable = class(TCustomButterworthHighPassFilter)
  public
    procedure CalculateW0; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;


//  TCustomButterworthLowPassFilterAutomatable = class(TCustomButterworthLowPassFilter);

implementation

uses
  Math, SysUtils, DAV_Approximations;

{$IFDEF HandleDenormals}
var
  DenormRandom   : Single;
const
  CDenorm32      : Single = 1E-24;
  CDenorm64      : Double = 1E-34;
{$ENDIF}

constructor TCustomButterworthFilter.Create(const Order: Integer = 0);
begin
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 FFilterGain    := 1;
 inherited Create(Order);
 CalculateCoefficients;
end;

class function TCustomButterworthFilter.GetMaxOrder: Cardinal;
begin
 result := 64;
end;

procedure TCustomButterworthFilter.Reset;
begin
 Gain := 0;
end;

procedure TCustomButterworthFilter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomButterworthFilter.ResetStatesInt64;
begin
 PInt64(@FState[0])^ := 0;
 PInt64(@FState[1])^ := 0;
end;

procedure TCustomButterworthFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TCustomButterworthFilter.CalculateW0;
begin
 FW0 := 2 * Pi * SampleRateReciprocal * (Frequency * FDownsampleFak);
 FTanW0 := Tan(FW0 * CHalf64)
end;

procedure TCustomButterworthFilter.SetFilterValues(const AFrequency, AGain : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency := AFrequency;
 FGain_dB := AGain;
 FGainFactor := Exp(FGain_dB * ln10_0025);
 CalculateW0;
end;

function TCustomButterworthFilter.Real(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, result, Temp);
end;

function TCustomButterworthFilter.Imaginary(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, Temp, result);
end;

function TCustomButterworthFilter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

procedure TCustomButterworthFilter.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   FPiHalfOrderInv := PI * CHalf64 * FOrderInv;
   GetSinCos(FPiHalfOrderInv, FCosOrderOffset.Im, FCosOrderOffset.Re);
   inherited;
  end
 else
  begin
   FFilterGain := FGainFactor;
  end; 
end;

function TCustomButterworthFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 20 * Log10(MagnitudeSquared(Frequency));
end;

procedure TCustomButterworthFilter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0],FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TCustomButterworthFilter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0], FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

{ TButterworthFilterLP }

constructor TCustomButterworthLowPassFilter.Create(const Order: Integer = 0);
begin
 inherited Create(Order);
end;

procedure TCustomButterworthLowPassFilter.CalculateCoefficients;
var
  i           : Integer;
  K, K2, t, a : Double;
  Cmplx       : TComplexDouble;
begin
 if FOrder = 0 then exit;
 FFilterGain := sqr(FGainFactor);
 K := FTanW0;
 K2 := K * K;

 i := 0;
 Cmplx := FCosOrderOffset;
 while i < Integer(FOrder) - 1 do
  begin
   a := 2 * Cmplx.Im * K; // 2 * sin((i + 1) * FPiHalfOrderInv) * K;
   ComplexMultiply2Inplace(Cmplx, FCosOrderOffset);

   t := 1 / (K2 + a + 1);
   FFilterGain := FFilterGain * t * K2;
   FCoeffs[i    ] := -2 * (K2 - 1) * t;
   FCoeffs[i + 1] := (a - K2 - 1) * t;
   inc(i, 2);
  end;
 if i < Integer(FOrder) then
  begin
   t := 1 / (K + 1);
   FFilterGain := FFilterGain * t * K;
   FCoeffs[i] := (1 - K) * t;
  end;
end;

function TCustomButterworthLowPassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal); a := sqr(cw + 2);
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
 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(Result);
end;

function TCustomButterworthLowPassFilter.Phase(const Frequency: Double): Double;
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

procedure TCustomButterworthLowPassFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  i           : Integer;
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Real := FFilterGain;
 Imaginary := 0;
 for i := 0 to (FOrder div 2) - 1 do
  begin
(*
   Divider   := 1 / (sqr(FCoeffs[2 * i + 1] + 1) + sqr(FCoeffs[2 * i])
                      + 2 * cw * (FCoeffs[2 * i] * (FCoeffs[2 * i + 1] - 1) - 2 * cw * FCoeffs[2 * i + 1]));
   ComplexMultiplyInplace(Real, Imaginary,
     (1 - 2 * FCoeffs[2 * i] - FCoeffs[2 * i + 1]
      + 2 * cw * (1 - FCoeffs[2 * i + 1] - FCoeffs[2 * i])
      + (2 * sqr(cw) - 1) * (1 - FCoeffs[2 * i + 1])) * Divider,
      + 2 * (1 + FCoeffs[2 * i + 1]) * sqrt(1 - sqr(cw)) * Divider);
*)
   Divider   := 1 / ( sqr(FCoeffs[2 * i + 1]) - 2 * FCoeffs[2 * i + 1] + sqr(FCoeffs[2 * i]) + 1
                    + 2 * cw * (FCoeffs[2 * i] * (FCoeffs[2 * i + 1] + 1) + 2 * cw * FCoeffs[2 * i + 1]));
   ComplexMultiplyInplace(Real, Imaginary,
     (1 + 2 * FCoeffs[2 * i] + FCoeffs[2 * i + 1]
      + cw * (2 * (1 + FCoeffs[2 * i + 1]) + FCoeffs[2 * i] * 2)
      + (2 * sqr(cw)-1) * (FCoeffs[2 * i + 1] + 1)) * Divider,
      (2 * (1 - FCoeffs[2 * i + 1]) + 2 * cw * (1 - FCoeffs[2 * i + 1])) * sqrt(1 - sqr(cw)) * Divider);
  end;
end;


 function TCustomButterworthLowPassFilter.ProcessSample(const Input: Double): Double;
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
 fmul [eax.FFilterGain].Double
 mov ecx, [eax.FOrder]
 test ecx, ecx
 jz @End
 shr ecx, 1
 shl ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub ecx, 4
  fld st(0)
  fadd [eax.FState + ecx * 4].Double
  fld st(0)
  fld st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FState + ecx * 4 + 8].Double
  fld st(3)
  fadd st(0), st(0)
  faddp
  fstp [eax.FState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FState + ecx * 4 + 8].Double
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

{ TButterworthFilterHP }

constructor TCustomButterworthHighPassFilter.Create(const Order: Integer = 0);
begin
 inherited Create(Order);
 {$IFDEF HandleDenormals}
 DenormRandom := Random;
 {$ENDIF}
end;

procedure TCustomButterworthHighPassFilter.CalculateCoefficients;
var
  i           : Integer;
  K, K2, t, a : Double;
  Cmplx       : TComplexDouble;
begin
 if FOrder = 0 then exit;
 FFilterGain := sqr(FGainFactor);
 K := FTanW0;
 K2 := K * K;

 i := 0;
 Cmplx := FCosOrderOffset;
 while i < Integer(FOrder) - 1 do
  begin
   a := 2 * K * Cmplx.Im;
   ComplexMultiply2Inplace(Cmplx, FCosOrderOffset);

   t := 1 / (K2 + a + 1);
   FFilterGain := FFilterGain * t;
   FCoeffs[i    ] := -2 * (K2 - 1) * t;
   FCoeffs[i + 1] := (a - K2 - 1) * t;
   inc(i, 2);
  end;
 if i < Integer(FOrder) then
  begin
   t := 1 / (K + 1);
   FFilterGain := FFilterGain * t;
   FCoeffs[i] := (1 - K) * t;
  end;
end;

function TCustomButterworthHighPassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal);
 a := sqr(cw - 2);
 Result := 1;
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + sqr(FCoeffs[2 * i]) + sqr(FCoeffs[2 * i + 1]) +
       2 * FCoeffs[2 * i + 1] + cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw - 2) / (1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF} Abs(sqr(FFilterGain) * Result);
end;

function TCustomButterworthHighPassFilter.Phase(const Frequency: Double): Double;
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

procedure TCustomButterworthHighPassFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  i           : Integer;
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Real := FFilterGain;
 Imaginary := 0;
 for i := 0 to (FOrder div 2) - 1 do
  begin
(*
   Divider   := 1 / (sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] + sqr(FCoeffs[2 * i]) + 1
                      + 2 * cw * (FCoeffs[2 * i] * (FCoeffs[2 * i + 1] - 1) - 2 * cw * FCoeffs[2 * i + 1]));
   ComplexMultiplyInplace(Real, Imaginary,
     (1 + 2 * FCoeffs[2 * i] - FCoeffs[2 * i + 1]
      + 2 * cw * ((FCoeffs[2 * i + 1] - 1) - FCoeffs[2 * i])
      + (2 * sqr(cw) - 1) * (1 - FCoeffs[2 * i + 1])) * Divider,
      - 2 * (1 + FCoeffs[2 * i + 1]) * sqrt(1 - sqr(cw)) * Divider);
*)
   Divider   := 1 / ( sqr(FCoeffs[2 * i + 1]) - 2 * FCoeffs[2 * i + 1] + sqr(FCoeffs[2 * i]) + 1
                      + 2 * cw * (FCoeffs[2 * i] * (FCoeffs[2 * i + 1] + 1) + 2 * cw * FCoeffs[2 * i + 1]));
   ComplexMultiplyInplace(Real, Imaginary,
     (1 -2 * FCoeffs[2 * i] + 1 * FCoeffs[2 * i + 1]
      +        cw * 2 * (-(1 + FCoeffs[2 * i + 1]) + FCoeffs[2 * i])
      + (2 * sqr(cw) - 1) * (1 * FCoeffs[2 * i + 1] + 1)) * Divider,
      (-2 * (1 - FCoeffs[2 * i + 1])
      + 2 * cw * (1 - FCoeffs[2 * i + 1])) * sqrt(1 - sqr(cw)) * Divider);
  end;
end;

function TCustomButterworthHighPassFilter.ProcessSample(const Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=      x + FState[2 * i];
   FState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i             := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        :=  x + FState[2 * i];
   FState[2 * i] := -x + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
 fld  Input.Single

 // eventually add denormal
 {$IFDEF HandleDenormals}
 mov  edx, DenormRandom
 imul edx, DenormRandom, $08088405
 inc  edx
 shr  edx, 23
 or   edx, $20000000
 mov  DenormRandom, edx
 fadd DenormRandom
 {$ENDIF}

 fmul [eax.FFilterGain].Double
 mov  ecx, [eax.FOrder]
 test ecx, ecx
 jz @End
 shr ecx, 1
 shl ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub  ecx, 4
  fld  st(0)
  fadd [eax.FState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FState + ecx * 4 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  fsubp
  fstp [eax.FState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FState + ecx * 4 + 8].Double
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
  fsubrp st(2), st(0)
  fxch
  fstp [eax.FState + ecx * 4].Double
 @End:
 {$ENDIF}
end;

function TCustomButterworthHighPassFilter.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=      x + FState[2 * i];
   FState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i             := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        :=  x + FState[2 * i];
   FState[2 * i] := -x + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
 fld  Input.Double

 // eventually add denormal
 {$IFDEF HandleDenormals}
 mov  edx, DenormRandom
 imul edx, DenormRandom, $08088405
 inc  edx
 shr  edx, 23
 or   edx, $20000000
 mov  DenormRandom, edx
 fadd DenormRandom
 {$ENDIF} 

 fmul [eax.FFilterGain].Double
 mov  ecx, [eax.FOrder]
 test ecx, ecx
 jz @End
 shr ecx, 1
 shl ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub  ecx, 4
  fld  st(0)
  fadd [eax.FState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FState + ecx * 4 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  fsubp
  fstp [eax.FState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FState + ecx * 4 + 8].Double
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
  fsubrp st(2), st(0)
  fxch
  fstp [eax.FState + ecx * 4].Double
 @End:
 {$ENDIF}
end;

{ TCustomButterworthSplitBandFilter }

constructor TCustomButterworthSplitBandFilter.Create(const Order: Integer = 0);
begin
 inherited Create(Order);
 Randomize;
 {$IFDEF HandleDenormals}
 DenormRandom := Random;
 {$ENDIF}
end;

procedure TCustomButterworthSplitBandFilter.CalculateCoefficients;
var
  i           : Integer;
  K, K2, t, a : Double;
begin
 FFilterGain := sqr(FGainFactor);
 K := FTanW0; K2 := K * K; FKs := IntPower(K, FOrder);

 for i := 0 to (FOrder div 2) - 1 do
  begin
   a := 2 * sin((2 * i + 1) * FPiHalfOrderInv) * K;
   t := 1 / (K2 + a + 1);
   FFilterGain := FFilterGain * t;
   FCoeffs[2 * i    ] := -2 * (K2 - 1) * t;
   FCoeffs[2 * i + 1] := (a - K2 - 1) * t;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   t := 1 / (K + 1);
   FFilterGain := FFilterGain * t;
   FCoeffs[2 * i] := (1 - K) * t;
  end;
end;

function TCustomButterworthSplitBandFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i  : Integer;
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal);
 Result := sqr(FFilterGain) * sqr(FKs);
 for i := 0 to (FOrder div 2) - 1 do
  begin
   Result := Result * sqr(cw + 2) * sqr(cw - 2) / sqr(1 + sqr(FCoeffs[2 * i]) +
     sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
     cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) * (cw - 2) / sqr(1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;

 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(sqr(FFilterGain) * Result);
end;

procedure TCustomButterworthSplitBandFilter.ProcessSample(const Input: Single; out Lowpass,
  Highpass: Single);
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Highpass := FFilterGain * Input;
 Lowpass  := FFilterGain * Input * FKs;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Lowpass;
   Lowpass             :=      x + FState[2 * i];
   FState[2 * i    ]   :=  2 * x + FCoeffs[2 * i] * Lowpass + FState[2 * i + 1];
   FState[2 * i + 1]   :=      x + FCoeffs[2 * i + 1] * Lowpass;

   x := Highpass;
   Highpass            :=      x + FHPState[2 * i];
   FHPState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Highpass + FHPState[2 * i + 1];
   FHPState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Highpass;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x               :=  Lowpass;
   Lowpass        :=   x + FState[2 * i];
   FState[2 * i]   :=  x + FCoeffs[2 * i] * Lowpass;

   x               :=  Highpass;
   Highpass        :=  x + FHPState[2 * i];
   FHPState[2 * i] := -x + FCoeffs[2 * i] * Highpass;
  end;
{$ELSE}
asm
 fld  Input.Single               // highpass

 // eventuall add denormal
 {$IFDEF HandleDenormals}
 push ebx
 mov  ebx, DenormRandom
 imul ebx, DenormRandom, $08088405
 inc  ebx
 shr  ebx, 23
 or   ebx, $20000000
 mov  DenormRandom, ebx
 fadd DenormRandom
 pop ebx
 {$ENDIF}

 fmul [eax.FFilterGain].Double
 fld  st(0)                      // lowpass, highpass
 fmul [eax.FKs].Double
 push ecx
 mov  ecx, [eax.FOrder]
 test ecx, ecx
 jz  @End
 shr  ecx, 1
 shl  ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub  ecx, 4

  // lowpass
  fld  st(0)
  fadd [eax.FState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FState + ecx * 4 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  faddp
  fstp [eax.FState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FState + ecx * 4 + 8].Double
  fxch

  // highpass
  fld  st(0)
  fadd [eax.FHPState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FHPState + ecx * 4 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  fsubp
  fstp [eax.FHPState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FHPState + ecx * 4 + 8].Double
  fxch
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [eax.FOrder]
 jz @End
  mov ecx, [eax.FOrder]
  dec ecx
  shl ecx, 1

  // lowpass
  fld st(0)
  fadd [eax.FState + ecx * 4].Double
  fld st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  faddp st(2), st(0)
  fxch
  fstp [eax.FState + ecx * 4].Double
  fxch

  // highpass
  fld st(0)
  fadd [eax.FHPState + ecx * 4].Double
  fld st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fsubrp st(2), st(0)
  fxch
  fstp [eax.FHPState + ecx * 4].Double
  fxch
 @End:
 fstp Lowpass.Single
 pop ecx
 fstp Highpass.Single
 {$ENDIF}
end;

procedure TCustomButterworthSplitBandFilter.ProcessSample(const Input: Double; out Lowpass,
  Highpass: Double);
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Highpass := {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF} FFilterGain * Input;
 Lowpass  := {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF} FFilterGain * Input * FKs;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Lowpass;
   Lowpass             :=      x + FState[2 * i];
   FState[2 * i    ]   :=  2 * x + FCoeffs[2 * i] * Lowpass + FState[2 * i + 1];
   FState[2 * i + 1]   :=      x + FCoeffs[2 * i + 1] * Lowpass;

   x := Highpass;
   Highpass            :=      x + FHPState[2 * i];
   FHPState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Highpass + FHPState[2 * i + 1];
   FHPState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Highpass;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x               :=  Lowpass;
   Lowpass        :=   x + FState[2 * i];
   FState[2 * i]   :=  x + FCoeffs[2 * i] * Lowpass;

   x               :=  Highpass;
   Highpass        :=  x + FHPState[2 * i];
   FHPState[2 * i] := -x + FCoeffs[2 * i] * Highpass;
  end;
{$ELSE}
asm
 fld  Input.Double               // highpass
 fmul [eax.FFilterGain].Double

 // eventually add denormal
 {$IFDEF HandleDenormals}
 push ebx
 mov  ebx, DenormRandom
 imul ebx, DenormRandom, $08088405
 inc  ebx
 shr  ebx, 23
 or   ebx, $20000000
 mov  DenormRandom, ebx
 fadd DenormRandom
 pop ebx
 {$ENDIF}

 fld st(0)                       // lowpass, highpass
 fmul [eax.FKs].Double
 push ecx
 mov  ecx, [eax.FOrder]
 test ecx, ecx
 jz  @End
 shr  ecx, 1
 shl  ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub  ecx, 4

  // lowpass
  fld  st(0)
  fadd [eax.FState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FState + ecx * 4 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  faddp
  fstp [eax.FState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FState + ecx * 4 + 8].Double
  fxch

  // highpass
  fld  st(0)
  fadd [eax.FHPState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FHPState + ecx * 4 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  fsubp
  fstp [eax.FHPState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FHPState + ecx * 4 + 8].Double
  fxch
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [eax.FOrder]
 jz @End
  mov ecx, [eax.FOrder]
  dec ecx
  shl ecx, 1

  // lowpass
  fld st(0)
  fadd [eax.FState + ecx * 4].Double
  fld st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  faddp st(2), st(0)
  fxch
  fstp [eax.FState + ecx * 4].Double
  fxch

  // highpass
  fld st(0)
  fadd [eax.FHPState + ecx * 4].Double
  fld st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fsubrp st(2), st(0)
  fxch
  fstp [eax.FHPState + ecx * 4].Double
  fxch
 @End:
 fstp Lowpass.Double
 pop ecx
 fstp Highpass.Double
 {$ENDIF}
end;

{ TButterworthLowPassFilterAutomatable }

procedure TButterworthLowPassFilterAutomatable.CalculateW0;
begin
 FW0 := 2 * Pi * SampleRateReciprocal * (Frequency * FDownsampleFak);
 FTanW0 := FastTanInBounds4Term(FW0 * CHalf64)
end;

function TButterworthLowPassFilterAutomatable.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 20 * FastLog2ContinousError4(MagnitudeSquared(Frequency));
end;

function TButterworthLowPassFilterAutomatable.MagnitudeSquared(
  const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * FastCosInBounds3Term(2 * Frequency * Pi * SampleRateReciprocal); a := sqr(cw + 2);
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
 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(Result);
end;

{ TButterworthHighPassFilterAutomatable }

procedure TButterworthHighPassFilterAutomatable.CalculateW0;
begin
 FW0 := 2 * Pi * SampleRateReciprocal * (Frequency * FDownsampleFak);
 FTanW0 := FastTanInBounds4Term(FW0 * CHalf32)
end;

function TButterworthHighPassFilterAutomatable.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 20 * FastLog2ContinousError4(MagnitudeSquared(Frequency));
end;

function TButterworthHighPassFilterAutomatable.MagnitudeSquared(
  const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * FastCosInBounds3Term(2 * Frequency * Pi * SampleRateReciprocal);
 a := sqr(cw - 2);
 Result := 1;
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + sqr(FCoeffs[2 * i]) + sqr(FCoeffs[2 * i + 1]) +
       2 * FCoeffs[2 * i + 1] + cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw - 2) / (1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;

 Result := {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF} Abs(sqr(FFilterGain) * Result);
end;

end.
