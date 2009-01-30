unit DAV_DspButterworthFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_DspFilter, DAV_Common;

type
  TButterworthFilter = class(TCustomOrderFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FAB             : array [0..127] of Double;
    FState          : array [0.. 63] of Double;
    FStateStack     : array of array [0.. 63] of Double;
    procedure CalculateW0; override;
    class function GetMaxOrder: Cardinal; override;
  public
    constructor Create; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
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

  TButterworthLP = class(TButterworthFilter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;
  TButterworthHighCut = TButterworthLP;

  TButterworthHP = class(TButterworthFilter)
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  published
    property Gain;
    property Order;
    property Frequency;
    property SampleRate;
  end;
  TButterworthLowCut = TButterworthHP;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, SysUtils, DAV_Complex;

constructor TButterworthFilter.Create;
begin
 inherited;
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 CalculateCoefficients;
end;

class function TButterworthFilter.GetMaxOrder: Cardinal;
begin
 result := 64;
end;

procedure TButterworthFilter.Reset;
begin
 Gain := 0;
end;

procedure TButterworthFilter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TButterworthFilter.ResetStatesInt64;
begin
 PInt64(@FState[0])^ := 0;
 PInt64(@FState[1])^ := 0;
end;

procedure TButterworthFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TButterworthFilter.CalculateW0;
begin
 fW0 := 2 * Pi * fSRR * (fFrequency * FDownsampleFak);
 fSinW0 := sin(fW0);
 if fW0 > 3.1 then fW0 := 3.1;
end;

procedure TButterworthFilter.SetFilterValues(const AFrequency, AGain : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 fFrequency := AFrequency;
 FGain_dB := AGain;
 FGainFactor := Exp(FGain_dB * ln10_0025);
 CalculateW0;
end;

function TButterworthFilter.Real(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, result, Temp);
end;

function TButterworthFilter.Imaginary(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TButterworthFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
var
  cw, Divider  : Double;
  cmplx        : TComplexDouble;
  i            : Integer;
begin
 if FOrder = 0 then
  begin
   Real := 1;
   Imaginary := 1;
  end
 else
  begin
   cw := cos(2 * Frequency * pi * fSRR);
   Divider   := 1 / ( sqr(FAB[3]) - 2 * FAB[3] + sqr(FAB[2]) + 1
                      + 2 * cw * (FAB[2] * (FAB[3] + 1) + 2 * cw * FAB[3]));
   Real      := (FAB[0] + FAB[1] * FAB[2] + FAB[0] * FAB[3]
                + cw * (FAB[1] * (1 + FAB[3]) + FAB[2] * 2 * FAB[0])
                + (2 * sqr(cw) - 1) * FAB[0] * (FAB[3] + 1)) * Divider;
   Imaginary := (FAB[1] * (1 - FAB[3])
                + 2 * cw * FAB[0] * (1 - FAB[3])) * sqrt(1 - sqr(cw)) * Divider;
   for i := 1 to (FOrder div 2) - 1 do
    begin
     Divider   := 1 / ( sqr(FAB[4*i+3]) - 2 * FAB[4*i+3] + sqr(FAB[4*i+2]) + 1
                + 2 * cw * (FAB[4*i+2] * (FAB[4*i+3] + 1) + 2 * cw * FAB[4*i+3]));
     cmplx.Re  := (FAB[4*i+0] + FAB[4*i+1] * FAB[4*i+2] + FAB[4*i+0] * FAB[4*i+3]
                 + cw * (FAB[4*i+1] * (1 + FAB[4*i+3]) + FAB[4*i+2] * 2 * FAB[4*i+0])
                 + (2*sqr(cw)-1) * FAB[4*i+0] * (FAB[4*i+3] + 1)) * Divider;
     cmplx.Im := (FAB[4*i+1] * (1 - FAB[4*i+3])
                 + 2 * cw * (FAB[4*i+0] - FAB[4*i+0] * FAB[4*i+3])) * sqrt(1 - sqr(cw)) * Divider;
     ComplexMultiplyInplace(Real, Imaginary, cmplx.Re, cmplx.Im);
    end;
  end;
end;

procedure TButterworthFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Real := cmplx.Re;
 Imaginary := cmplx.Im;
end;

function TButterworthFilter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

function TButterworthFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 20 * Log10(MagnitudeSquared(Frequency));
end;

function TButterworthFilter.Phase(const Frequency: Double): Double;
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

procedure TButterworthFilter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0],FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TButterworthFilter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0], FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

{ TButterworthFilterLP }

constructor TButterworthLP.Create;
begin
 inherited Create;
 FGainFactor := 1;
end;

procedure TButterworthLP.CalculateCoefficients;
var
  i : Integer;
  K, K2, t, a  : Double;
begin
 K := tan(fW0 * 0.5); K2 := K * K;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   a := -2 * cos((2 * i + Integer(FOrder) + 1) / (2 * FOrder) * PI) * K;
   t := 1 / (K2 + a + 1);
   FAB[4 * i    ] := t*K2;
   FAB[4 * i + 1] := 2*FAB[4*i];
   FAB[4 * i + 2] := -2*(K2-1)*t;
   FAB[4 * i + 3] := (a-K2-1)*t;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder+1) div 2)-1; t := 1/(K+1);
   FAB[4*i] := K*t;
   FAB[4*i+1] := FAB[4*i];
   FAB[4*i+2] := (1-K)*t;
  end;
 t := sqr(FGainFactor);
 FAB[0] := FAB[0]*t;
 FAB[1] := FAB[1]*t;
end;

function TButterworthLP.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2*cos(2*Frequency*pi*fSRR); a := sqr(cw+2);
 Result := 1;
 for i := 0 to (FOrder div 2) - 1
  do Result := Result*sqr(FAB[4*i])*a/(1+sqr(FAB[4*i+2])+sqr(FAB[4*i+3])+2*FAB[4*i+3]+cw*((FAB[4*i+2]-cw)*FAB[4*i+3]-FAB[4*i+2]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder+1) div 2) - 1;
   Result := Result*sqr(FAB[4*i])*(cw+2)/(1+sqr(FAB[4*i+2])-cw*FAB[4*i+2]);
  end;
 Result := Abs(1E-32+Result);
end;

function TButterworthLP.Phase(const Frequency: Double): Double;
var
  cw, sw, Nom, Den : Double;
  i : Integer;
begin
 GetSinCos(2*Frequency*pi*fSRR,sw,cw);
 Nom := sw * FAB[0] * 2 * (FAB[3] -1) * (1 + cw);
 Den := FAB[0] * (2 * FAB[2] * (1 + cw) + cw * (2 * FAB[3] * (cw + 1) + 2 * (1 + cw)));
 for i := 1 to (FOrder div 2) - 1 do
  begin
   Nom := Nom * sw * FAB[4*i] * 2 * (FAB[4*i+3] - 1) * (cw + 1);
   Den := Den * FAB[4*i] * (2 * FAB[4*i+2] * (1 + cw) + cw * (2 * FAB[4*i+3] * (cw + 1) + 2 * (1 + cw)));
  end;
 Result := ArcTan2(Nom,Den);
end;

function TButterworthLP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            := FAB[4 * i    ] * x                           + FState[2 * i    ];
   FState[2 * i    ] := FAB[4 * i + 1] * x + FAB[4 * i + 2] * Result + FState[2 * i + 1];
   FState[2 * i + 1] := FAB[4 * i    ] * x + FAB[4 * i + 3] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x             := FAB[4 * i] * Result;
   Result        := x + FState[2 * i];
   FState[2 * i] := x + FAB[4 * i + 2] * Result;
  end;
{$ELSE}
asm
 fld Input.Double;
 mov ecx, [self.FOrder]
 test ecx, ecx
 jz @End
 shr ecx, 1
 shl ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub ecx, 4
  fld st(0)
  fmul [self.FAB + ecx * 8].Double
  fadd [self.FState + ecx * 4].Double
  fld st(0)
  fld st(0)
  fmul [self.FAB + ecx * 8 + 16].Double
  fadd [self.FState + ecx * 4 + 8].Double
  fld st(3)
  fmul [self.FAB + ecx * 8 + 8].Double
  faddp
  fstp [self.FState + ecx * 4].Double
  fmul [self.FAB + ecx * 8 + 24].Double
  fxch
  fxch st(2)
  fmul [self.FAB + ecx * 8].Double
  faddp
  fstp [self.FState + ecx * 4 + 8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [self.FOrder]
 jz @End
  mov ecx, [self.FOrder]
  dec ecx
  shl ecx, 1
  fmul [self.FAB + ecx * 8].Double
  fld st(0)
  fadd [self.FState + ecx * 4].Double
  fld st(0)
  fmul [self.FAB + ecx * 8 + 16].Double
  faddp st(2), st(0)
  fxch
  fstp [self.FState + ecx * 4].Double
 @End:
 {$ENDIF}
end;

{ TButterworthFilterHP }

constructor TButterworthHP.Create;
begin
 inherited Create;
 FGainFactor := 1;
end;

procedure TButterworthHP.CalculateCoefficients;
var i : Integer;
    K,K2,t,a  : Double;
begin
 K := tan(fW0*0.5); K2 := K*K;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   a := -2*cos((2 * i + Integer(FOrder) + 1) / (2 * FOrder) * PI) * K;
   t := 1 / (K2 + a + 1);
   FAB[4 * i    ] := t;
   FAB[4 * i + 1] := -2 * t;
   FAB[4 * i + 2] := -2 * (K2 - 1) * t;
   FAB[4 * i + 3] := (a - K2 - 1) * t;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1; t := 1 / (K + 1);
   FAB[4 * i    ] := t;
   FAB[4 * i + 1] := FAB[4 * i];
   FAB[4 * i + 2] := (1 - K) * t;
  end;
 t := sqr(FGainFactor);
 FAB[0] := FAB[0] * t;
 FAB[1] := FAB[1] * t;
end;

function TButterworthHP.MagnitudeSquared(const Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * fSRR); a := sqr(cw - 2);
 Result := 1;
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * sqr(FAB[4 * i]) * a / (1 + sqr(FAB[4 * i + 2]) + sqr(FAB[4 * i + 3]) + 2 * FAB[4 * i + 3] + cw * ((FAB[4 * i + 2] - cw) * FAB[4 * i + 3] - FAB[4 * i + 2]));
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder+1) div 2) - 1;
   Result := Result * sqr(FAB[4 * i]) * (cw - 2) / (1 + sqr(FAB[4 * i + 2]) - cw * FAB[4 * i + 2]);
  end;
 Result := Abs(CDenorm32 + Result);
end;

function TButterworthHP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            := FAB[4 * i    ] * x                           + FState[2 * i];
   FState[2 * i    ] := FAB[4 * i + 1] * x + FAB[4 * i + 2] * Result + FState[2 * i + 1];
   FState[2 * i + 1] := FAB[4 * i    ] * x + FAB[4 * i + 3] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i             := ((FOrder + 1) div 2) - 1;
   x             := FAB[4 * i] * Result;
   Result        :=  x + FState[2 * i];
   FState[2 * i] := -x + FAB[4 * i + 2] * Result;
  end;
{$ELSE}
asm
 fld Input.Double;
 mov ecx, [self.FOrder]
 test ecx, ecx
 jz @End
 shr ecx, 1
 shl ecx, 2
 push ecx
 jz @SingleStage
 @FilterLoop:
  sub  ecx, 4
  fld  st(0)
  fmul [self.FAB + ecx * 8].Double
  fadd [self.FState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [self.FAB + ecx * 8 + 16].Double
  fadd [self.FState + ecx * 4 + 8].Double
  fld  st(3)
  fmul [self.FAB + ecx * 8 + 8].Double
  faddp
  fstp [self.FState+ecx*4].Double
  fmul [self.FAB + ecx * 8 + 24].Double
  fxch
  fxch st(2)
  fmul [self.FAB + ecx * 8].Double
  faddp
  fstp [self.FState + ecx * 4 + 8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [self.FOrder]
 jz @End
  mov ecx, [self.FOrder]
  dec ecx
  shl ecx, 1
  fmul [self.FAB+ecx*8].Double
  fld st(0)
  fadd [self.FState+ecx*4].Double
  fld st(0)
  fmul [self.FAB+ecx*8+16].Double
  fsubrp st(2), st(0)
  fxch
  fstp [self.FState+ecx*4].Double
 @End:
 {$ENDIF}
end;

end.
