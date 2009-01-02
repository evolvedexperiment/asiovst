unit DAV_DspBesselFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_DspFilter, DAV_Common;

type
  TBesselFilter = class(TCustomOrderFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FOrder          : Integer;
    FA              : array [0..63] of Double;
    FB              : array [0..63] of Double;
    FState          : array [0..127] of Double;
    FStateStack     : array of array [0..127] of Double;
    procedure CalculateW0; override;
    class function GetMaxOrder: Cardinal; override;
  public
    constructor Create; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure ResetStates; override;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure ResetStatesInt64; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;
    procedure Complex(const Frequency: Double; out Real: Single; out Imaginary: Single); override;
    function Imaginary(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

  TBesselLP = class(TBesselFilter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
  end;
  TBesselHighCut = TBesselLP;

  TBesselHP = class(TBesselFilter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
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
 inherited;
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 CalculateCoefficients;
end;

class function TBesselFilter.GetMaxOrder: Cardinal;
begin
 result := 64;
end;

procedure TBesselFilter.Reset;
begin
 Gain := 0;
end;

procedure TBesselFilter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TBesselFilter.ResetStatesInt64;
begin
 PInt64(@FState[0])^ := 0;
 PInt64(@FState[1])^ := 0;
end;

procedure TBesselFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TBesselFilter.CalculateW0;
begin
 fW0 := 2 * Pi * fSRR * (fFrequency * FDownsampleFak);
 fSinW0 := sin(fW0);
 if fW0 > 3.1 then fW0 := 3.1;
end;

procedure TBesselFilter.SetFilterValues(const AFrequency, AGain : Single);
begin
 fFrequency := AFrequency;
 FGain_dB := AGain;
 FGainFactor := dB_to_Amp(FGain_dB);
 CalculateW0;
end;

function TBesselFilter.Real(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, result, Temp);
end;

function TBesselFilter.Imaginary(const Frequency: Double): Double;
var Temp : Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TBesselFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
(*var cw, Divider  : Double;
    cmplx        : TComplexDouble;
    i            : Integer;*)
begin
(*
 if FOrder = 0 then
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
   for i := 1 to (FOrder div 2) - 1 do
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

procedure TBesselFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Real := cmplx.Re;
 Imaginary := cmplx.Im;
end;

function TBesselFilter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

function TBesselFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 20 * Log10(MagnitudeSquared(Frequency));
end;

function TBesselFilter.Phase(const Frequency: Double): Double;
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

procedure TBesselFilter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0,0], FState[0], Length(FStateStack[0])*SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1,0],FStateStack[0,0], (Length(FStateStack)-1)*Length(FStateStack[0])*SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TBesselFilter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0,0], FStateStack[1,0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0,0], Length(FStateStack[0]) * SizeOf(Double));
end;

{ TBesselFilterLP }

constructor TBesselLP.Create;
begin
 inherited Create;
 Order := 6;
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
   FB[0] :=   - ( ((4 * K + 20) * K * K       - 210)*K - 420)*t;
   FB[1] :=   - (  (6 * K * K       - 90) * K * K      + 630)*t;
   FB[2] :=   - ( ((4 * K - 20) * K * K       + 210)*K - 420)*t;
   FB[3] :=   - ((((    K - 10) * K + 45) * K - 105)*K + 105)*t;

   FA[0] := 105 * t * fGainFactor;
   FA[1] := - 4 * FA[0];
   FA[2] := 6 * FA[0];
   FA[3] := FA[1];
   FA[4] := FA[0];


(*
   t     := 1 / ((((105 * K + 105) * K + 45) * K + 10)*K + 1);
   FB[0] :=   - ( ((420 * K + 210) * K * K       - 20)*K - 4)*t;
   FB[1] :=   - (  (630 * K * K        - 90) * K * K     + 6)*t;
   FB[2] :=   - ( ((420 * K - 210) * K * K       + 20)*K - 4)*t;
   FB[3] :=   - ((((105 * K - 105) * K + 45) * K - 10)*K + 1)*t;

   FA[0] := 105 * t * fGainFactor;
   FA[1] := 4 * FA[0];
   FA[2] := 6 * FA[0];
   FA[3] := FA[1];
   FA[4] := FA[0];
*)
  end else
 if Order = 6 then
  begin
   t     := 1 / ((((((     K + 1 * 21) * K + 1 * 210)* K + 1 * 1260)*K + 1 * 4725) * K + 1 * 10395)*K +  1 * 10395);
   FB[0] :=   - ( (((( 6 * K + 4 * 21) * K + 2 * 210)* K * K           - 2 * 4725) * K - 4 * 10395)*K -  6 * 10395)*t;
   FB[1] :=   - ((((((15 * K + 5 * 21) * K - 1 * 210)* K - 3 * 1260)*K - 1 * 4725) * K + 5 * 10395)*K + 15 * 10395)*t;
   FB[2] :=   - (   ((20 * K * K           - 4 * 210)* K * K           + 4 * 4725) * K * K            - 20 * 10395)*t;
   FB[3] :=   - ((((((15 * K - 5 * 21) * K - 1 * 210)* K + 3 * 1260)*K - 1 * 4725) * K - 5 * 10395)*K + 15 * 10395)*t;
   FB[4] :=   - ( (((( 6 * K - 4 * 21) * K + 2 * 210)* K * K           - 2 * 4725) * K + 4 * 10395)*K -  6 * 10395)*t;
   FB[5] :=   - ((((((     K - 1 * 21) * K + 1 * 210)* K - 1 * 1260)*K + 1 * 4725) * K - 1 * 10395)*K +  1 * 10395)*t;

   FA[0] :=   10395 * t * fGainFactor;
   FA[1] := - 6 * FA[0];
   FA[2] :=  15 * FA[0];
   FA[3] := -20 * FA[0];
   FA[4] :=  15 * FA[0];
   FA[5] := - 6 * FA[0];
   FA[6] :=       FA[0];
  end;
end;

function TBesselLP.MagnitudeSquared(const Frequency: Double): Double;
(*var
  i    : Integer;
  a,cw : Double;*)
begin
  Result:=1; // dummy
(*
 cw:=2*cos(2*Frequency*pi*fSRR); a:=sqr(cw+2);
 Result:=1;
 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (FOrder mod 2) = 1 then
  begin
   i:=((FOrder+1) div 2) - 1;
   Result:=Result*sqr(fAB[4*i])*(cw+2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result:=Abs(1E-32+Result);
*)
end;

function TBesselLP.Phase(const Frequency: Double): Double;
(*var cw, sw, Nom, Den : Double;
    i : Integer;*)
begin
  Result:=0; // dummy
(*
 GetSinCos(2*Frequency*pi*fSRR,sw,cw);
 Nom := sw * fAB[0] * 2 * (fAB[3] -1) * (1 + cw);
 Den := fAB[0] * (2 * fAB[2] * (1 + cw) + cw * (2 * fAB[3] * (cw + 1) + 2 * (1 + cw)));
 for i := 1 to (FOrder div 2) - 1 do
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
 result    := FA[0] * Input + FState[0];
 for i := 1 to FOrder - 1
  do FState[i - 1] := FA[i] * Input + FB[i - 1] * result + FState[i];
 FState[FOrder - 1] := FA[FOrder] * Input + FB[FOrder - 1] * result;
end;
{$ELSE}
asm
 fld Input.Double
 fmul [self.FA].Double
 fadd [self.FState].Double
 mov ecx, 1
 mov edx, [self.FOrder]

@StageLoop:
 fld st(0)
 fmul [self.FB + ecx * 8 - 8].Double
 fadd [self.FState + ecx * 8].Double
 fld Input.Double
 fmul [self.FA + ecx * 8].Double
 faddp
 fstp [self.FState + ecx * 8 - 8].Double
 inc ecx
 test ecx, edx
 jnp @StageLoop

 fld st(0)
 fmul [self.FB + ecx * 8 - 8].Double
 fld Input.Double
 fmul [self.FA + ecx * 8].Double
 faddp
 fstp [self.FState + ecx * 8 - 8].Double
end;
{$ENDIF}

{ TBesselFilterHP }

constructor TBesselHP.Create;
begin
 inherited Create;
 fGainFactor:=1;
end;

procedure TBesselHP.CalculateCoefficients;
var K, t  : Double;
begin
 K := tan(fW0 * 0.5);
 if Order = 4 then
  begin
   t     := 1 / ((((105 * K + 105) * K + 45) * K + 10)*K + 1);
   FB[0] :=   - ( ((420 * K + 210) * K * K       - 20)*K - 4)*t;
   FB[1] :=   - (  (630 * K * K        - 90) * K * K     + 6)*t;
   FB[2] :=   - ( ((420 * K - 210) * K * K       + 20)*K - 4)*t;
   FB[3] :=   - ((((105 * K - 105) * K + 45) * K - 10)*K + 1)*t;

   FA[0] := 105 * t * fGainFactor;
   FA[1] := - 4 * FA[0];
   FA[2] :=  6 * FA[0];
   FA[3] := FA[1];
   FA[4] := FA[0];
  end else
 if Order = 6 then
  begin
   t     := 1 / ((((((   K +   21)*K +  210)*K + 1260)*K +  4725)*K + 10395)*K +  10395);
   FB[0] :=   - ( (((( 6*K +   84)*K +  420)*K*K         -  9450)*K - 41580)*K -  62370) * t;
   FB[1] :=   - ((((((15*K +  105)*K -  210)*K - 3780)*K -  4725)*K + 51975)*K + 155925) * t;
   FB[2] :=   - (   ((20*K*K         -  840)*K*K         + 18900)*K*K          - 207900) * t;
   FB[3] :=   - ((((((15*K -  105)*K -  210)*K + 3780)*K -  4725)*K - 51975)*K + 155925) * t;
   FB[4] :=   - ( (((( 6*K -   84)*K +  420)*K*K         -  9450)*K + 41580)*K -  62370) * t;
   FB[5] :=   - ((((((   K -   21)*K +  210)*K - 1260)*K +  4725)*K - 10395)*K +  10395) * t;
   FA[0] := 10395 * t * fGainFactor;
   FA[1] := - 6 * FA[0];
   FA[2] :=  15 * FA[0];
   FA[3] := -20 * FA[0];
   FA[4] :=  15 * FA[0];
   FA[5] := - 6 * FA[0];
   FA[6] :=       FA[0];
  end;
end;

function TBesselHP.MagnitudeSquared(const Frequency: Double): Double;
(*var
  i    : Integer;
  a,cw : Double;*)
begin
  Result:=1; // dummy
(*
 cw:=2*cos(2*Frequency*pi*fSRR); a:=sqr(cw-2);
 Result:=1;
 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (FOrder mod 2) = 1 then
  begin
   i:=((FOrder+1) div 2) - 1;
   Result:=Result*sqr(fAB[4*i])*(cw-2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result:=Abs(1E-32+Result);
*)
end;

function TBesselHP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 result    := FA[0] * Input + FState[0];
 for i := 1 to FOrder - 1
  do FState[i - 1] := FA[i] * Input + FB[i - 1] * result + FState[i];
 FState[FOrder - 1] := FA[FOrder] * Input + FB[FOrder - 1] * result;
end;
{$ELSE}
asm
 fld Input.Double
 fmul [self.FA].Double
 fadd [self.FState].Double
 mov ecx, 1
 mov edx, [self.FOrder]

@StageLoop:
 fld st(0)
 fmul [self.FB + ecx * 8 - 8].Double
 fadd [self.FState + ecx * 8].Double
 fld Input.Double
 fmul [self.FA + ecx * 8].Double
 faddp
 fstp [self.FState + ecx * 8 - 8].Double
 inc ecx
 test ecx, edx
 jnp @StageLoop

 fld st(0)
 fmul [self.FB + ecx * 8 - 8].Double
 fld Input.Double
 fmul [self.FA + ecx * 8].Double
 faddp
 fstp [self.FState + ecx * 8 - 8].Double
end;
{$ENDIF}

end.
