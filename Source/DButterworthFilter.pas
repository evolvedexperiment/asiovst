unit DButterworthFilter;

interface

{$IFDEF FPC}
{$MODE Delphi}
{$ELSE}
{$DEFINE x87}
{$ENDIF}

uses DFilter;

type
  TButterworthFilter = class(TIIRFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    fDownsamplePow  : Integer;
    fDownsampleFak  : Integer;
    fOrder          : Integer;
    fAB             : array [0..127] of Double;
    fD64            : array [0.. 63] of Double;
    procedure SetW0; override;
    procedure SetOrder(Value: Integer); override;
    procedure SetGain(const Value: Double); override;
    procedure SetFrequency(const Value: Double); override;
    procedure SetSampleRate(const Value: Double); override;
    function GetOrder:Integer; override;
  public
    constructor Create; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    function Magnitude(Frequency:Double):Double; override;
    function MagnitudeLog10(Frequency:Double):Double; virtual;
    procedure ResetStates; override;
    procedure Reset; override;
    property DownsampleAmount : Integer read fDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read fDownsampleFak;
  end;

  TButterworthLP = class(TButterworthFilter)
  private
  protected
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input:Double):Double; override;
    function Magnitude(Frequency:Double):Double; override;
    function MagnitudeLog10(Frequency:Double):Double; override;
  end;

  TButterworthHP = class(TButterworthFilter)
  private
  protected
  public
    constructor Create; override;
    procedure CalculateCoefficients; override;
    function ProcessSample(const Input:Double):Double; override;
    function Magnitude(Frequency:Double):Double; override;
    function MagnitudeLog10(Frequency:Double):Double; override;
  end;

implementation

uses Math, DDSPBase, SysUtils;

constructor TButterworthFilter.Create;
begin
 fDownsamplePow:=0;
 fDownsampleFak:=1;
 fFrequency:=0;
 fGain:=0;
 fOrder:=10;
 SampleRate:=44100;
 CalculateCoefficients;
end;

procedure TButterworthFilter.Reset;
begin
 fGain:=0;
 CalculateCoefficients;
end;

procedure TButterworthFilter.ResetStates;
begin
 FillChar(fD64[0],fOrder*SizeOf(Double),0);
end;

procedure TButterworthFilter.SetSampleRate(const Value: Double);
begin
 if Value=0 then Exit;
 if Value<>fSampleRate then
  begin
   fSampleRate := Value;
   fSRR:=1/fSampleRate;
  end;
end;

procedure TButterworthFilter.SetDownsamplePower(Value: Integer);
begin
 if Value<0 then Value:=0;
 if fDownsamplePow<>Value then
  begin
   fDownsamplePow := Value;
   fDownsampleFak := round(IntPower(2,fDownsamplePow));
   SetW0;
  end;
end;

procedure TButterworthFilter.SetW0;
begin
 fW0:=2*Pi*fSRR*(fFrequency*fDownsampleFak);
 fSinW0:=sin(fW0);
 if fW0>3.1 then fW0:=3.1;
end;

procedure TButterworthFilter.SetGain(const Value: Double);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 fGain := Value;
 fGainSpeed:=Exp(fGain*ln10_0025);
end;

procedure TButterworthFilter.SetOrder(Value: Integer);
begin
 if Value<2 then Value:=2 else
 if Value>64 then Value:=64;
 if fOrder<>Value then
  begin
   fOrder := Value;
   CalculateCoefficients;
  end;
end;

procedure TButterworthFilter.SetFrequency(const Value: Double);
begin
 if fFrequency <> Value then
  begin
   fFrequency:=Value;
   SetW0;
   CalculateCoefficients;
  end;
end;

procedure TButterworthFilter.SetFilterValues(const AFrequency, AGain : Single);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 fFrequency:=AFrequency; fGain:=AGain;
 fGainSpeed:=Exp((fGain*ln10_0025));
 SetW0;
end;

function TButterworthFilter.GetOrder: Integer;
begin
 Result:=fOrder;
end;

function TButterworthFilter.Magnitude(Frequency: Double): Double;
begin
 Result:=1;
end;

function TButterworthFilter.MagnitudeLog10(Frequency: Double): Double;
begin
 result:=20*Log10(Magnitude(Frequency));
end;

{ TButterworthFilterLP }

constructor TButterworthLP.Create;
begin
 inherited Create;
 fGainSpeed:=1;
end;

procedure TButterworthLP.CalculateCoefficients;
var i : Integer;
    K,K2,t,a  : Double;
begin
 K:=tan(fW0*0.5); K2:=K*K;
 for i:=0 to (fOrder div 2) - 1 do
  begin
   a:=-2*cos((2*i+fOrder+1)/(2*fOrder)*PI)*K;
   t:=1/(K2+a+1);
   fAB[4*i+0]:=t*K2;
   fAB[4*i+1]:=2*fAB[4*i];
   fAB[4*i+2]:=-2*(K2-1)*t;
   fAB[4*i+3]:=(a-K2-1)*t;
  end;
 t:=fGainSpeed*fGainSpeed;
 fAB[0]:=fAB[0]*t;
 fAB[1]:=fAB[1]*t;
end;

function TButterworthLP.Magnitude(Frequency:Double):Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(Frequency*pi*fSRR); a:=4+cw*(4+cw);
 Result:=1;

 for i := 0 to (fOrder div 2) - 1
  do Result:=Result*fAB[4*i]*fAB[4*i]*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 Result:=sqrt(Abs(1E-32+Result));
end;

function TButterworthLP.MagnitudeLog10(Frequency:Double):Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(Frequency*pi*fSRR); a:=4+cw*(4+cw);
 Result:=1;

 for i := 0 to (fOrder div 2) - 1
  do Result:=Result*fAB[4*i]*fAB[4*i]*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 Result:=10*Log10(Abs(1E-32+Result));
end;

function TButterworthLP.ProcessSample(const Input: Double): Double;
{$IFDEF x87}
asm
 mov ecx, [self.fOrder]
 shl ecx, 1
 fld Input.Double;
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.fAB+ecx*8].Double
  fadd [self.fD64+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB+ecx*8+16].Double
  fadd [self.fD64+ecx*4+8].Double
  fld st(3)
  fmul [self.fAB+ecx*8+8].Double
  faddp
  fstp [self.fD64+ecx*4].Double
  fmul [self.fAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.fAB+ecx*8].Double
  faddp
  fstp [self.fD64+ecx*4+8].Double
 jnz @FilterLoop
end;
{$ELSE}
var
  y,x : Double;
  i   : Integer;
begin
 Result:=Input;
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x:=Result;
   Result      := fAB[4*i+0]*x                      + fD64[2*i];
   fD64[2*i  ] := fAB[4*i+1]*x + fAB[4*i+2]*Result  + fD64[2*i+1];
   fD64[2*i+1] := fAB[4*i+0]*x + fAB[4*i+3]*Result;
  end;
end;
{$ENDIF}

{ TButterworthFilterHP }

constructor TButterworthHP.Create;
begin
 inherited Create;
 fGainSpeed:=1;
end;

procedure TButterworthHP.CalculateCoefficients;
var i : Integer;
    K,K2,t,a  : Double;
begin
 K:=tan(fW0*0.5); K2:=K*K;
 for i:=0 to (fOrder div 2) - 1 do
  begin
   a:=-2*cos((2*i+fOrder+1)/(2*fOrder)*PI)*K;
   t:=1/(K2+a+1);
   fAB[4*i+0]:=t;
   fAB[4*i+1]:=-2*t;
   fAB[4*i+2]:=-2*(K2-1)*t;
   fAB[4*i+3]:=-(K2-a+1)*t;
  end;
 t:=fGainSpeed*fGainSpeed;
 fAB[0]:=fAB[0]*t;
 fAB[1]:=fAB[1]*t;
end;

function TButterworthHP.Magnitude(Frequency:Double):Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(Frequency*pi*fSRR); a:=sqr(cw-2);
 Result:=1;

 for i := 0 to (fOrder div 2) - 1
  do Result:=Result*fAB[4*i]*fAB[4*i]*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 Result:=sqrt(Result);
end;

function TButterworthHP.MagnitudeLog10(Frequency: Double): Double;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(Frequency*pi*fSRR); a:=sqr(cw-2);
 Result:=1;

 for i := 0 to (fOrder div 2) - 1
  do Result:=Result*fAB[4*i]*fAB[4*i]*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 Result:=10*log10(Result);
end;

function TButterworthHP.ProcessSample(const Input: Double): Double;
{$IFDEF x87}
asm
 mov ecx, [self.fOrder]
 shl ecx, 1
 fld Input.Double;
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.fAB+ecx*8].Double
  fadd [self.fD64+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB+ecx*8+16].Double
  fadd [self.fD64+ecx*4+8].Double
  fld st(3)
  fmul [self.fAB+ecx*8+8].Double
  faddp
  fstp [self.fD64+ecx*4].Double
  fmul [self.fAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.fAB+ecx*8].Double
  faddp
  fstp [self.fD64+ecx*4+8].Double
 jnz @FilterLoop
end;
{$ELSE}
var
  y,x : Double;
  i   : Integer;
begin
 Result:=Input;
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x:=Result;
   Result      := fAB[4*i+0]*x                      + fD64[2*i];
   fD64[2*i  ] := fAB[4*i+1]*x + fAB[4*i+2]*Result  + fD64[2*i+1];
   fD64[2*i+1] := fAB[4*i+0]*x + fAB[4*i+3]*Result;
  end;
end;
{$ENDIF}

end.