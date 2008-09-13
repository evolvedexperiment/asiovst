unit DAV_DspBesselFilter;

interface

{$I ASIOVST.INC}

uses
  DAV_DspFilter, DAV_Common;

type
  TBesselFilter = class(TIIRFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    fDownsamplePow  : Integer;
    fDownsampleFak  : Integer;
    fOrder          : Integer;
    fA              : array [0..63] of Double;
    fB              : array [0..63] of Double;
    fState          : array [0..127] of Double;
    fStateStack     : array of array [0..127] of Double;
    procedure SetW0; override;
    procedure SetOrder(Value: Integer); override;
    procedure SetGain(const Value: Double); override;
    procedure SetFrequency(const Value: Double); override;
    procedure SetSampleRate(const Value: Double); override;
    function GetOrder:Integer; override;
  public
    constructor Create; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    function MagnitudeSquared(Frequency:Double):Double; override;
    function MagnitudeLog10(Frequency:Double):Double; override;
    procedure ResetStates; override;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure ResetStatesInt64; override;
    procedure Complex(Frequency: Double; out Real: Double; out Imaginary: Double); override;
    procedure Complex(Frequency: Double; out Real: Single; out Imaginary: Single); override;
    function Imaginary(Frequency: Double): Double; override;
    function Phase(Frequency: Double): Double; override;
    function Real(Frequency: Double): Double; override;
    property DownsampleAmount : Integer read fDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read fDownsampleFak;
  end;

  TBesselLP = class(TBesselFilter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input:Double):Double; override;
    function MagnitudeSquared(Frequency:Double):Double; override;
    function Phase(Frequency: Double): Double; override;
  end;
  TBesselHighCut = TBesselLP;

  TBesselHP = class(TBesselFilter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input:Double):Double; override;
    function MagnitudeSquared(Frequency:Double):Double; override;
  end;
  TBesselLowCut = TBesselHP;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, Dialogs, SysUtils, DAV_Complex;

constructor TBesselFilter.Create;
begin
 fDownsamplePow := 0;
 fDownsampleFak := 1;
 fFrequency     := 0;
 fGain          := 0;
 fOrder         := 6;
 SampleRate     := 44100;
 CalculateCoefficients;
end;

procedure TBesselFilter.Reset;
begin
 fGain := 0;
 CalculateCoefficients;
end;

procedure TBesselFilter.ResetStates;
begin
 FillChar(fState[0],fOrder*SizeOf(Double),0);
end;

procedure TBesselFilter.ResetStatesInt64;
begin
 PInt64(@fState[0])^ := 0;
 PInt64(@fState[1])^ := 0;
end;

procedure TBesselFilter.SetSampleRate(const Value: Double);
begin
 if Value=0 then Exit;
 if Value<>fSampleRate then
  begin
   fSampleRate := Value;
   fSRR := 1 / fSampleRate;
   SetW0;
   CalculateCoefficients;
  end;
end;

procedure TBesselFilter.SetDownsamplePower(Value: Integer);
begin
 if Value<0 then Value:=0;
 if fDownsamplePow<>Value then
  begin
   fDownsamplePow := Value;
   fDownsampleFak := round(IntPower(2,fDownsamplePow));
   SetW0;
  end;
end;

procedure TBesselFilter.SetW0;
begin
 fW0 := 2 * Pi * fSRR * (fFrequency * fDownsampleFak);
 fSinW0 := sin(fW0);
 if fW0 > 3.1 then fW0 := 3.1;
end;

procedure TBesselFilter.SetGain(const Value: Double);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 fGain := Value;
 fGainSpeed := Exp(fGain * ln10_0025);
end;

procedure TBesselFilter.SetOrder(Value: Integer);
begin
 if Value <  1 then Value :=  1 else
 if Value > 64 then Value := 64;
 if fOrder <> Value then
  begin
   fOrder := Value;
   CalculateCoefficients;
  end;
end;

procedure TBesselFilter.SetFrequency(const Value: Double);
begin
 if fFrequency <> Value then
  begin
   fFrequency := Value;
   SetW0;
   CalculateCoefficients;
  end;
end;

procedure TBesselFilter.SetFilterValues(const AFrequency, AGain : Single);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 fFrequency := AFrequency;
 fGain := AGain;
 fGainSpeed := Exp(fGain * ln10_0025);
 SetW0;
end;

function TBesselFilter.GetOrder: Integer;
begin
 Result := fOrder;
end;

function TBesselFilter.Real(Frequency: Double): Double;
var Temp : Double;
begin
 Complex(Frequency, result, Temp);
end;

function TBesselFilter.Imaginary(Frequency: Double): Double;
var Temp : Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TBesselFilter.Complex(Frequency: Double; out Real, Imaginary: Double);
(*var cw, Divider  : Double;
    cmplx        : TComplexDouble;
    i            : Integer;*)
begin
(*
 if fOrder = 0 then
  begin
   Real := 1;
   Imaginary := 1;
  end
 else
  begin
   cw := cos(2 * Frequency * pi * fSRR);
   Divider   := 1 / ( sqr(fAB[3]) - 2 * fAB[3] + sqr(fAB[2]) + 1
                      + 2 * cw * (fAB[2] * (fAB[3] + 1) + 2 * cw * fAB[3]));
   Real      := (fAB[0] + fAB[1] * fAB[2] + fAB[0] * fAB[3]
                + cw * (fAB[1] * (1 + fAB[3]) + fAB[2] * 2 * fAB[0])
                + (2 * sqr(cw) - 1) * fAB[0] * (fAB[3] + 1)) * Divider;
   Imaginary := (fAB[1] * (1 - fAB[3])
                + 2 * cw * fAB[0] * (1 - fAB[3])) * sqrt(1 - sqr(cw)) * Divider;
   for i := 1 to (fOrder div 2) - 1 do
    begin
     Divider   := 1 / ( sqr(fAB[4*i+3]) - 2 * fAB[4*i+3] + sqr(fAB[4*i+2]) + 1
                + 2 * cw * (fAB[4*i+2] * (fAB[4*i+3] + 1) + 2 * cw * fAB[4*i+3]));
     cmplx.Re  := (fAB[4*i+0] + fAB[4*i+1] * fAB[4*i+2] + fAB[4*i+0] * fAB[4*i+3]
                 + cw * (fAB[4*i+1] * (1 + fAB[4*i+3]) + fAB[4*i+2] * 2 * fAB[4*i+0])
                 + (2*sqr(cw)-1) * fAB[4*i+0] * (fAB[4*i+3] + 1)) * Divider;
     cmplx.Im := (fAB[4*i+1] * (1 - fAB[4*i+3])
                 + 2 * cw * (fAB[4*i+0] - fAB[4*i+0] * fAB[4*i+3])) * sqrt(1 - sqr(cw)) * Divider;
{$IFNDEF FPC}
     ComplexMultiplyInplace(Real, Imaginary, cmplx.Re, cmplx.Im);
{$ENDIF}
    end;
  end;
*)
end;

procedure TBesselFilter.Complex(Frequency: Double; out Real, Imaginary: Single);
var cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Real := cmplx.Re;
 Imaginary := cmplx.Im;
end;

function TBesselFilter.MagnitudeSquared(Frequency: Double): Double;
begin
 Result := 1;
end;

function TBesselFilter.MagnitudeLog10(Frequency: Double): Double;
begin
 result := 20 * Log10(MagnitudeSquared(Frequency));
end;

function TBesselFilter.Phase(Frequency: Double): Double;
var cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

procedure TBesselFilter.PopStates;
begin
 if Length(fStateStack) > 0 then
  begin
   Move(fStateStack[0,0], fState[0], Length(fStateStack[0])*SizeOf(Double));
   if Length(fStateStack) > 1
    then Move(fStateStack[1,0],fStateStack[0,0], (Length(fStateStack)-1)*Length(fStateStack[0])*SizeOf(Double));
   SetLength(fStateStack, Length(fStateStack) - 1);
  end;
end;

procedure TBesselFilter.PushStates;
begin
 SetLength(fStateStack, Length(fStateStack) + 1);
 if Length(fStateStack) > 1
  then Move(fStateStack[0,0], fStateStack[1,0], (Length(fStateStack) - 1) * Length(fStateStack[0]) * SizeOf(Double));
 Move(fState[0], fStateStack[0,0], Length(fStateStack[0]) * SizeOf(Double));
end;

{ TBesselFilterLP }

constructor TBesselLP.Create;
begin
 inherited Create;
 fGainSpeed := 1;
 fOrder := 6;
end;

function CalculateReverseBesselPolynomial(Order : Integer;  X : Double): Double;
begin
 if Order = 0 then Result := 1 else
 if Order = 1 then Result := X + 1
  else Result := (2 * Order - 1) * CalculateReverseBesselPolynomial(Order - 1, X)
                        + sqr(X) * CalculateReverseBesselPolynomial(Order - 2, X);
end;

function CalculateReverseBesselPolynomial0(Order : Integer): Double;
begin
 if Order < 2 then Result := 1
  else Result := (2 * Order - 1) * CalculateReverseBesselPolynomial0(Order - 1);
end;

function CalculateBesselFactor(Order, Term : Integer): Double;
begin
 result := Factorial(2 * Order - Term) / (IntPower(2, Order - Term) * Factorial(Term) * Factorial(Order - Term));
end;

procedure TBesselLP.CalculateCoefficients;
var K, t  : Double;
begin
// ShowMessage(FloatToStr(CalculateReverseBesselPolynomial0(6)));

 K := tan(fW0 * 0.5);
 if Order = 4 then
  begin
   t     := 1 / ((((    K + 10) * K + 45) * K + 105)*K + 105);
   fB[0] :=   - ( ((4 * K + 20) * K * K       - 210)*K - 420)*t;
   fB[1] :=   - (  (6 * K * K       - 90) * K * K      + 630)*t;
   fB[2] :=   - ( ((4 * K - 20) * K * K       + 210)*K - 420)*t;
   fB[3] :=   - ((((    K - 10) * K + 45) * K - 105)*K + 105)*t;

   fA[0] := 105 * t * fGainSpeed;
   fA[1] := - 4 * fA[0];
   fA[2] := 6 * fA[0];
   fA[3] := fA[1];
   fA[4] := fA[0];


(*
   t     := 1 / ((((105 * K + 105) * K + 45) * K + 10)*K + 1);
   fB[0] :=   - ( ((420 * K + 210) * K * K       - 20)*K - 4)*t;
   fB[1] :=   - (  (630 * K * K        - 90) * K * K     + 6)*t;
   fB[2] :=   - ( ((420 * K - 210) * K * K       + 20)*K - 4)*t;
   fB[3] :=   - ((((105 * K - 105) * K + 45) * K - 10)*K + 1)*t;

   fA[0] := 105 * t * fGainSpeed;
   fA[1] := 4 * fA[0];
   fA[2] := 6 * fA[0];
   fA[3] := fA[1];
   fA[4] := fA[0];
*)
  end else
 if Order = 6 then
  begin
   t     := 1 / ((((((     K + 1 * 21) * K + 1 * 210)* K + 1 * 1260)*K + 1 * 4725) * K + 1 * 10395)*K +  1 * 10395);
   fB[0] :=   - ( (((( 6 * K + 4 * 21) * K + 2 * 210)* K * K           - 2 * 4725) * K - 4 * 10395)*K -  6 * 10395)*t;
   fB[1] :=   - ((((((15 * K + 5 * 21) * K - 1 * 210)* K - 3 * 1260)*K - 1 * 4725) * K + 5 * 10395)*K + 15 * 10395)*t;
   fB[2] :=   - (   ((20 * K * K           - 4 * 210)* K * K           + 4 * 4725) * K * K            - 20 * 10395)*t;
   fB[3] :=   - ((((((15 * K - 5 * 21) * K - 1 * 210)* K + 3 * 1260)*K - 1 * 4725) * K - 5 * 10395)*K + 15 * 10395)*t;
   fB[4] :=   - ( (((( 6 * K - 4 * 21) * K + 2 * 210)* K * K           - 2 * 4725) * K + 4 * 10395)*K -  6 * 10395)*t;
   fB[5] :=   - ((((((     K - 1 * 21) * K + 1 * 210)* K - 1 * 1260)*K + 1 * 4725) * K - 1 * 10395)*K +  1 * 10395)*t;

   fA[0] :=   10395 * t * fGainSpeed;
   fA[1] := - 6 * fA[0];
   fA[2] :=  15 * fA[0];
   fA[3] := -20 * fA[0];
   fA[4] :=  15 * fA[0];
   fA[5] := - 6 * fA[0];
   fA[6] :=       fA[0];
  end;
end;

function TBesselLP.MagnitudeSquared(Frequency:Double):Double;
(*var
  i    : Integer;
  a,cw : Double;*)
begin
  Result:=1; // dummy
(*
 cw:=2*cos(2*Frequency*pi*fSRR); a:=sqr(cw+2);
 Result:=1;
 for i := 0 to (fOrder div 2) - 1
  do Result:=Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (fOrder mod 2) = 1 then
  begin
   i:=((fOrder+1) div 2) - 1;
   Result:=Result*sqr(fAB[4*i])*(cw+2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result:=Abs(1E-32+Result);
*)
end;

function TBesselLP.Phase(Frequency:Double):Double;
(*var cw, sw, Nom, Den : Double;
    i : Integer;*)
begin
  Result:=0; // dummy
(*
 GetSinCos(2*Frequency*pi*fSRR,sw,cw);
 Nom := sw * fAB[0] * 2 * (fAB[3] -1) * (1 + cw);
 Den := fAB[0] * (2 * fAB[2] * (1 + cw) + cw * (2 * fAB[3] * (cw + 1) + 2 * (1 + cw)));
 for i := 1 to (fOrder div 2) - 1 do
  begin
   Nom := Nom * sw * fAB[4*i] * 2 * (fAB[4*i+3] - 1) * (cw + 1);
   Den := Den * fAB[4*i] * (2 * fAB[4*i+2] * (1 + cw) + cw * (2 * fAB[4*i+3] * (cw + 1) + 2 * (1 + cw)));
  end;
 Result:=ArcTan2(Nom,Den);
*)
end;

function TBesselLP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 result    := fA[0] * Input + fState[0];
 for i := 1 to fOrder - 1
  do fState[i - 1] := fA[i] * Input + fB[i - 1] * result + fState[i];
 fState[fOrder - 1] := fA[fOrder] * Input + fB[fOrder - 1] * result;
end;
{$ELSE}
asm
 fld Input.Double
 fmul [self.fA].Double
 fadd [self.fState].Double
 mov ecx, 1
 mov edx, [self.fOrder]

@StageLoop:
 fld st(0)
 fmul [self.fB + ecx * 8 - 8].Double
 fadd [self.fState + ecx * 8].Double
 fld Input.Double
 fmul [self.fA + ecx * 8].Double
 faddp
 fstp [self.fState + ecx * 8 - 8].Double
 inc ecx
 test ecx, edx
 jnp @StageLoop

 fld st(0)
 fmul [self.fB + ecx * 8 - 8].Double
 fld Input.Double
 fmul [self.fA + ecx * 8].Double
 faddp
 fstp [self.fState + ecx * 8 - 8].Double
end;
{$ENDIF}

{ TBesselFilterHP }

constructor TBesselHP.Create;
begin
 inherited Create;
 fGainSpeed:=1;
end;

procedure TBesselHP.CalculateCoefficients;
var K, t  : Double;
begin
 K := tan(fW0 * 0.5);
 if Order = 4 then
  begin
   t     := 1 / ((((105 * K + 105) * K + 45) * K + 10)*K + 1);
   fB[0] :=   - ( ((420 * K + 210) * K * K       - 20)*K - 4)*t;
   fB[1] :=   - (  (630 * K * K        - 90) * K * K     + 6)*t;
   fB[2] :=   - ( ((420 * K - 210) * K * K       + 20)*K - 4)*t;
   fB[3] :=   - ((((105 * K - 105) * K + 45) * K - 10)*K + 1)*t;

   fA[0] := 105 * t * fGainSpeed;
   fA[1] := - 4 * fA[0];
   fA[2] :=  6 * fA[0];
   fA[3] := fA[1];
   fA[4] := fA[0];
  end else
 if Order = 6 then
  begin
   t     := 1 / ((((((   K +   21)*K +  210)*K + 1260)*K +  4725)*K + 10395)*K +  10395);
   fB[0] :=   - ( (((( 6*K +   84)*K +  420)*K*K         -  9450)*K - 41580)*K -  62370) * t;
   fB[1] :=   - ((((((15*K +  105)*K -  210)*K - 3780)*K -  4725)*K + 51975)*K + 155925) * t;
   fB[2] :=   - (   ((20*K*K         -  840)*K*K         + 18900)*K*K          - 207900) * t;
   fB[3] :=   - ((((((15*K -  105)*K -  210)*K + 3780)*K -  4725)*K - 51975)*K + 155925) * t;
   fB[4] :=   - ( (((( 6*K -   84)*K +  420)*K*K         -  9450)*K + 41580)*K -  62370) * t;
   fB[5] :=   - ((((((   K -   21)*K +  210)*K - 1260)*K +  4725)*K - 10395)*K +  10395) * t;
   fA[0] := 10395 * t * fGainSpeed;
   fA[1] := - 6 * fA[0];
   fA[2] :=  15 * fA[0];
   fA[3] := -20 * fA[0];
   fA[4] :=  15 * fA[0];
   fA[5] := - 6 * fA[0];
   fA[6] :=       fA[0];
  end;
end;

function TBesselHP.MagnitudeSquared(Frequency:Double):Double;
(*var
  i    : Integer;
  a,cw : Double;*)
begin
  Result:=1; // dummy
(*
 cw:=2*cos(2*Frequency*pi*fSRR); a:=sqr(cw-2);
 Result:=1;
 for i := 0 to (fOrder div 2) - 1
  do Result:=Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (fOrder mod 2) = 1 then
  begin
   i:=((fOrder+1) div 2) - 1;
   Result:=Result*sqr(fAB[4*i])*(cw-2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result:=Abs(1E-32+Result);
*)
end;

function TBesselHP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 result    := fA[0] * Input + fState[0];
 for i := 1 to fOrder - 1
  do fState[i - 1] := fA[i] * Input + fB[i - 1] * result + fState[i];
 fState[fOrder - 1] := fA[fOrder] * Input + fB[fOrder - 1] * result;
end;
{$ELSE}
asm
 fld Input.Double
 fmul [self.fA].Double
 fadd [self.fState].Double
 mov ecx, 1
 mov edx, [self.fOrder]

@StageLoop:
 fld st(0)
 fmul [self.fB + ecx * 8 - 8].Double
 fadd [self.fState + ecx * 8].Double
 fld Input.Double
 fmul [self.fA + ecx * 8].Double
 faddp
 fstp [self.fState + ecx * 8 - 8].Double
 inc ecx
 test ecx, edx
 jnp @StageLoop

 fld st(0)
 fmul [self.fB + ecx * 8 - 8].Double
 fld Input.Double
 fmul [self.fA + ecx * 8].Double
 faddp
 fstp [self.fState + ecx * 8 - 8].Double
end;
{$ENDIF}

end.
