unit DAV_DspButterworthFilter;

interface

{$I ..\ASIOVST.INC}

uses
  DAV_DspFilter, DAV_Common;

type
  TButterworthFilter = class(TCustomOrderFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    fDownsamplePow  : Integer;
    fDownsampleFak  : Integer;
    fAB             : array [0..127] of Double;
    fState          : array [0.. 63] of Double;
    fStateStack     : array of array [0.. 63] of Double;
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
    property DownsampleAmount : Integer read fDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read fDownsampleFak;
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
 fDownsamplePow := 0;
 fDownsampleFak := 1;
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
 FillChar(fState[0], fOrder * SizeOf(Double), 0);
end;

procedure TButterworthFilter.ResetStatesInt64;
begin
 PInt64(@fState[0])^ := 0;
 PInt64(@fState[1])^ := 0;
end;

procedure TButterworthFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if fDownsamplePow <> Value then
  begin
   fDownsamplePow := Value;
   fDownsampleFak := round(IntPower(2, fDownsamplePow));
   CalculateW0;
  end;
end;

procedure TButterworthFilter.CalculateW0;
begin
 fW0 := 2 * Pi * fSRR * (fFrequency * fDownsampleFak);
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
 if Length(fStateStack) > 0 then
  begin
   Move(fStateStack[0, 0], fState[0], Length(fStateStack[0]) * SizeOf(Double));
   if Length(fStateStack) > 1
    then Move(fStateStack[1, 0],fStateStack[0, 0], (Length(fStateStack) - 1) * Length(fStateStack[0]) * SizeOf(Double));
   SetLength(fStateStack, Length(fStateStack) - 1);
  end;
end;

procedure TButterworthFilter.PushStates;
begin
 SetLength(fStateStack, Length(fStateStack) + 1);
 if Length(fStateStack) > 1
  then Move(fStateStack[0, 0], fStateStack[1, 0], (Length(fStateStack) - 1) * Length(fStateStack[0]) * SizeOf(Double));
 Move(fState[0], fStateStack[0, 0], Length(fStateStack[0]) * SizeOf(Double));
end;

{ TButterworthFilterLP }

constructor TButterworthLP.Create;
begin
 inherited Create;
 FGainFactor := 1;
end;

procedure TButterworthLP.CalculateCoefficients;
var i : Integer;
    K,K2,t,a  : Double;
begin
 K := tan(fW0*0.5); K2 := K*K;

 for i := 0 to (fOrder div 2) - 1 do
  begin
   a := -2*cos((2*i+fOrder+1)/(2*fOrder)*PI)*K;
   t := 1/(K2+a+1);
   fAB[4*i+0] := t*K2;
   fAB[4*i+1] := 2*fAB[4*i];
   fAB[4*i+2] := -2*(K2-1)*t;
   fAB[4*i+3] := (a-K2-1)*t;
  end;
 if (fOrder mod 2) = 1 then
  begin
   i := ((fOrder+1) div 2)-1; t := 1/(K+1);
   fAB[4*i] := K*t;
   fAB[4*i+1] := fAB[4*i];
   fAB[4*i+2] := (1-K)*t;
  end;
 t := sqr(FGainFactor);
 fAB[0] := fAB[0]*t;
 fAB[1] := fAB[1]*t;
end;

function TButterworthLP.MagnitudeSquared(const Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw := 2*cos(2*Frequency*pi*fSRR); a := sqr(cw+2);
 Result := 1;
 for i := 0 to (fOrder div 2) - 1
  do Result := Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (fOrder mod 2) = 1 then
  begin
   i := ((fOrder+1) div 2) - 1;
   Result := Result*sqr(fAB[4*i])*(cw+2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result := Abs(1E-32+Result);
end;

function TButterworthLP.Phase(const Frequency: Double): Double;
var cw, sw, Nom, Den : Double;
    i : Integer;
begin
 GetSinCos(2*Frequency*pi*fSRR,sw,cw);
 Nom := sw * fAB[0] * 2 * (fAB[3] -1) * (1 + cw);
 Den := fAB[0] * (2 * fAB[2] * (1 + cw) + cw * (2 * fAB[3] * (cw + 1) + 2 * (1 + cw)));
 for i := 1 to (fOrder div 2) - 1 do
  begin
   Nom := Nom * sw * fAB[4*i] * 2 * (fAB[4*i+3] - 1) * (cw + 1);
   Den := Den * fAB[4*i] * (2 * fAB[4*i+2] * (1 + cw) + cw * (2 * fAB[4*i+3] * (cw + 1) + 2 * (1 + cw)));
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
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x := Result;
   Result            := fAB[4 * i    ] * x                           + fState[2 * i    ];
   fState[2 * i    ] := fAB[4 * i + 1] * x + fAB[4 * i + 2] * Result + fState[2 * i + 1];
   fState[2 * i + 1] := fAB[4 * i    ] * x + fAB[4 * i + 3] * Result;
  end;
 if (fOrder mod 2) = 1 then
  begin
   i := ((fOrder+1) div 2) - 1;
   x             := fAB[4 * i] * Result;
   Result        := x + fState[2 * i];
   fState[2 * i] := x + fAB[4 * i + 2] * Result;
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
  fmul [self.fAB + ecx * 8].Double
  fadd [self.fState + ecx * 4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB + ecx * 8 + 16].Double
  fadd [self.fState + ecx * 4 + 8].Double
  fld st(3)
  fmul [self.fAB + ecx * 8 + 8].Double
  faddp
  fstp [self.fState + ecx * 4].Double
  fmul [self.fAB + ecx * 8 + 24].Double
  fxch
  fxch st(2)
  fmul [self.fAB + ecx * 8].Double
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
  fmul [self.fAB + ecx * 8].Double
  fld st(0)
  fadd [self.fState + ecx * 4].Double
  fld st(0)
  fmul [self.fAB + ecx * 8 + 16].Double
  faddp st(2), st(0)
  fxch
  fstp [self.fState + ecx * 4].Double
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

 for i := 0 to (fOrder div 2) - 1 do
  begin
   a := -2*cos((2*i+fOrder+1)/(2*fOrder)*PI)*K;
   t := 1/(K2+a+1);
   fAB[4*i+0] := t;
   fAB[4*i+1] := -2*t;
   fAB[4*i+2] := -2*(K2-1)*t;
   fAB[4*i+3] := (a-K2-1)*t;
  end;
 if (fOrder mod 2) = 1 then
  begin
   i := ((fOrder+1) div 2)-1; t := 1/(K+1);
   fAB[4*i] := t;
   fAB[4*i+1] := fAB[4*i];
   fAB[4*i+2] := (1-K)*t;
  end;
 t := sqr(FGainFactor);
 fAB[0] := fAB[0]*t;
 fAB[1] := fAB[1]*t;
end;

function TButterworthHP.MagnitudeSquared(const Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw := 2*cos(2*Frequency*pi*fSRR); a := sqr(cw-2);
 Result := 1;
 for i := 0 to (fOrder div 2) - 1
  do Result := Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (fOrder mod 2) = 1 then
  begin
   i := ((fOrder+1) div 2) - 1;
   Result := Result*sqr(fAB[4*i])*(cw-2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result := Abs(1E-32+Result);
end;

function TButterworthHP.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := Input;
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x := Result;
   Result            := fAB[4 * i    ] * x                           + fState[2 * i];
   fState[2 * i    ] := fAB[4 * i + 1] * x + fAB[4 * i + 2] * Result + fState[2 * i + 1];
   fState[2 * i + 1] := fAB[4 * i    ] * x + fAB[4 * i + 3] * Result;
  end;
 if (fOrder mod 2) = 1 then
  begin
   i             := ((fOrder + 1) div 2) - 1;
   x             := fAB[4 * i] * Result;
   Result        :=  x + fState[2 * i];
   fState[2 * i] := -x + fAB[4 * i + 2] * Result;
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
  sub ecx,4
  fld st(0)
  fmul [self.fAB+ecx*8].Double
  fadd [self.fState+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB+ecx*8+16].Double
  fadd [self.fState+ecx*4+8].Double
  fld st(3)
  fmul [self.fAB+ecx*8+8].Double
  faddp
  fstp [self.fState+ecx*4].Double
  fmul [self.fAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.fAB+ecx*8].Double
  faddp
  fstp [self.fState+ecx*4+8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [self.fOrder]
 jz @End
  mov ecx, [self.fOrder]
  dec ecx
  shl ecx, 1
  fmul [self.fAB+ecx*8].Double
  fld st(0)
  fadd [self.fState+ecx*4].Double
  fld st(0)
  fmul [self.fAB+ecx*8+16].Double
  fsubrp st(2), st(0)
  fxch
  fstp [self.fState+ecx*4].Double
 @End:
 {$ENDIF}
end;

end.
