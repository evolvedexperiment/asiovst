unit DAV_DspWaveshaper;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Math, DAV_Common, DAV_DspCommon;

type
  TChebyshevWaveshaper = class(TDspObject)
  private
    function GetGain(Harmonic: Integer): Double;
    function GetInverted(Harmonic: Integer): Boolean;
    function GetLevel(Harmonic: Integer): Double;
    function GetOrder: Integer;
    procedure SetGain(Harmonic: Integer; const Value: Double);
    procedure SetLevel(Harmonic: Integer; const Value: Double);
    procedure SetOrder(Value: Integer);
    procedure SetInverted(Harmonic: Integer; const Value: Boolean);
  protected
    FChebyshevCoeffs  : TDAVDoubleDynArray;
    FGains            : TDAVDoubleDynArray;
    procedure RecalculateHarmonics; virtual;
    procedure OrderChanged; virtual;
  public
    constructor Create;
    function ProcessSample(Input: Double): Double;
    property Gain[Harmonic: Integer]: Double read GetGain write SetGain;
    property Level[Harmonic: Integer]: Double read GetLevel write SetLevel;
    property Inverted[Harmonic: Integer]: Boolean read GetInverted write SetInverted;
  published
    property Order: Integer read GetOrder write SetOrder;
  end;

  TChebyshevWaveshaperSquare = class(TChebyshevWaveshaper)
  protected
    procedure OrderChanged; override;
  end;

  TChebyshevWaveshaperSquarelShape = class(TChebyshevWaveshaper)
  private
    FShape: Double;
    procedure SetShape(const Value: Double);
  published
  protected
    procedure OrderChanged; override;
  published
    property Shape: Double read FShape write SetShape;
  end;

function Waveshaper1(x, t: Single): Single; overload;
function Waveshaper1(x, t: Double): Double; overload;
function Waveshaper2(x, t: Single): Single; overload;
function Waveshaper2(x, t: Double): Double; overload;
function Waveshaper3(x, a: Single): Single; overload;
function Waveshaper3(x, a: Double): Double; overload;
function Waveshaper4(x, a: Single): Single; overload;
function Waveshaper4(x, a: Double): Double; overload;
function Waveshaper5(x, a: Single): Single; overload;
function Waveshaper5(x, a: Double): Double; overload;
function Waveshaper6(x: Single): Single; overload;
function Waveshaper6(x: Double): Double; overload;
function Waveshaper7(x, a: Single): Single; overload;
function Waveshaper7(x, a: Double): Double; overload;
function Waveshaper8(x, a: Single): Single; overload;
function Waveshaper8(x, a: Double): Double; overload;
function Saturate(input, Limit: Single): Single; overload;
function Saturate(input, Limit: Double): Double; overload;
function Saturate2(input, Limit: Single): Single; overload;
function Saturate2(input, Limit: Double): Double; overload;
function SoftSat(x, a:Single): Single; overload;
function SoftSat(x, a:Double): Double; overload;

implementation

function Waveshaper1(x, t :Single): Single;
begin
 if abs(x) < t
  then Result := x
  else
   begin
    if x > 0
     then Result :=   t + (1 - t) * tanh(( x - t) / (1 - t))
     else Result := -(t + (1 - t) * tanh((-x - t) / (1 - t)));
   end;
end;

function Waveshaper1(x, t :Double): Double;
begin
 if abs(x) < t
  then Result := x
  else
   begin
    if x > 0
     then Result :=   t + (1 - t) * tanh(( x - t) / (1 - t))
     else Result := -(t + (1 - t) * tanh((-x - t) / (1 - t)));
   end;
end;

function Waveshaper2(x, t: Single): Single;
begin
 if abs(x) < t
  then Result := x
  else
   begin
    if x > 0
     then Result :=   t + (1 - t) * sigmoid( (x - t) / ((1 - t) * 1.5))
     else Result := -(t + (1 - t) * sigmoid((-x - t) / ((1 - t) * 1.5)));
   end;
end;

function Waveshaper2(x, t: Double): Double;
begin
 if abs(x) < t
  then Result := x
  else
   begin
    if x > 0
     then Result :=   t + (1 - t) * sigmoid( (x - t) / ((1 - t) * 1.5))
     else Result := -(t + (1 - t) * sigmoid((-x - t) / ((1 - t) * 1.5)));
   end;
end;

function Waveshaper3(x, a: Single): Single;
begin
 Result := x * (Abs(x) + a) / (x * x + (a - 1) * Abs(x) + 1);
end;

function Waveshaper3(x, a: Double): Double;
begin
 Result := x * (Abs(x) + a) / (x * x + (a - 1) * Abs(x) + 1);
end;

function Waveshaper4(x, a: Single): Single;
begin
 Result := sign(x) * power(ArcTan(Power(Abs(x), a)), (1 / a));
end;

function Waveshaper4(x, a: Double): Double;
begin
 Result := sign(x) * power(arctan(power(Abs(x), a)), (1 / a));
end;

function Waveshaper5(x, a: Single): Single;
begin
 a := 2 * a / (1 - a);
 Result := (1 + a) * x / (1 + a * Abs(x));
end;

function Waveshaper5(x, a: Double): Double;
begin
 a := 2 * a / (1 - a);
 Result := (1 + a) * x / (1 + a * Abs(x));
end;

function Waveshaper6(x: Single): Single;
var
  a, b : Single;
begin
 x := x * 0.686306;
 a := 1 + Exp(sqrt(Abs(x)) * -0.75);
 b := Exp(x);
 Result := (b - Exp(-x * a)) * b / (b * b + 1);
end;

function Waveshaper6(x: Double): Double;
var
  a, b : Double;
begin
 x := x * 0.686306;
 a := 1 + Exp(sqrt(Abs(x)) * -0.75);
 b := Exp(x);
 Result := (b - Exp(-x * a)) * b / (b * b + 1);
end;

function Waveshaper7(x, a: Single): Single;
begin
 Result := sign(x) * Exp(ln(Abs(x)) * a);
end;

function Waveshaper7(x, a: Double): Double;
begin
 Result := sign(x) * Exp(ln(Abs(x)) * a);
end;

function Waveshaper8(x, a: Single): Single;
begin
 Result := sign(x) * Exp(ln(a) * Abs(x));
end;

function Waveshaper8(x, a: Double): Double;
begin
 Result := sign(x) * Exp(ln(a) * Abs(x));
end;

function Saturate(Input, Limit: Single): Single;
{$IFNDEF FPC}
const
  CGrdDiv : Double = 0.5;
asm
 fld Input.Single
 fadd Limit
 fabs
 fld Input.Single
 fsub Limit
 fabs
 fsubp
 fmul CGrdDiv;
// result := CGrdDiv * (Abs(Input + Limit) - Abs(Input - Limit));
end;
{$ELSE}
begin
 result := 0.5 * (Abs(Input + Limit) - Abs(Input - Limit));
end;
{$ENDIF}

function Saturate(Input, Limit: Double): Double;
{$IFNDEF FPC}
const
  CGrdDiv : Double = 0.5;
asm
 fld Input.Double
 fadd Limit.Double
 fabs
 fld Input.Double
 fsub Limit.Double
 fabs
 fsubp
 fmul CGrdDiv;
end;
{$ELSE}
begin
 result := 0.5 * (Abs(Input + Limit) - Abs(Input - Limit));
end;
{$ENDIF}

function Saturate2(Input, Limit: Single): Single;
begin
 if Input > Limit
  then result := Limit
  else
   if Input < -Limit
    then result := -Limit
    else Result := Input;
end;

function Saturate2(Input, Limit: Double): Double;
begin
 if Input > Limit
  then result := Limit
  else
   if Input < -Limit
    then result := -Limit
    else Result := Input;
end; 

function SoftSat(x, a: Single): Single;
var
  b, c : Single;
begin
 b := Abs(x);
 if b < a then Result := x else
 if b > 1 then Result := sign(x) * (a + 1) * 0.5 else
  begin
   c := ((x - a) / (1 - a));
   Result := a + (x - a) / (1 + c * c);
  end;
end;

function SoftSat(x, a: Double): Double;
var
  b, c : Double;
begin
 b := Abs(x);
 if b < a then Result := x else
 if b > 1 then Result := sign(x) * (a + 1) * 0.5 else
  begin
   c := ((x - a) / (1 - a));
   Result := a + (x - a) / (1 + c * c);
  end;
end;

{ TChebyshevWaveshaper }

constructor TChebyshevWaveshaper.Create;
begin
 Order := 1;
 Gain[0] := 1;
end;

function TChebyshevWaveshaper.GetGain(Harmonic: Integer): Double;
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.Create('Index out of bounds')
  else result := FGains[Harmonic];
end;

function TChebyshevWaveshaper.GetInverted(Harmonic: Integer): Boolean;
begin
 result := Gain[Harmonic] < 0;
end;

function TChebyshevWaveshaper.GetLevel(Harmonic: Integer): Double;
begin
 result := Amp_to_dB(abs(FGains[Harmonic]));
end;

function TChebyshevWaveshaper.GetOrder: Integer;
begin
 result := Length(FGains);
end;

function TChebyshevWaveshaper.ProcessSample(Input: Double): Double;
var
  i : Integer;
begin
 result := FChebyshevCoeffs[Order];
 for i := Order - 1 downto 0
  do result := result * Input + FChebyshevCoeffs[i];
end;

function ChebyPolynome(Order, Power: Integer): Integer;
begin
 if (Power < 0) or (Order < Power) then result := 0 else
  case Order of
    0 : if (Power = 0) then result := 1 else result := 0;
    1 : if (Power = 1) then result := 1 else result := 0;
    2 : case Power of
         0 : result := -1;
         2 : result :=  2;
         else result := 0;
        end;
    3 : case Power of
         1 : result := -3;
         3 : result :=  4;
         else result := 0;
        end;
    4 : case Power of
         0 : result :=  1;
         2 : result := -8;
         4 : result :=  8;
         else result := 0;
        end;
    5 : case Power of
         1 : result :=   5;
         3 : result := -20;
         5 : result :=  16;
         else result := 0;
        end;
    6 : case Power of
         0 : result :=  -1;
         2 : result :=  18;
         4 : result := -48;
         6 : result :=  32;
         else result := 0;
        end;
    7 : case Power of
         1 : result :=  -7;
         3 : result :=  56;
         5 : result := -112;
         7 : result :=  64;
         else result := 0;
        end;
    8 : case Power of
         0 : result :=    1;
         2 : result :=  -32;
         4 : result :=  160;
         6 : result := -256;
         8 : result :=  128;
         else result := 0;
        end;
   else result := 2 * ChebyPolynome(Order - 1, Power - 1) - ChebyPolynome(Order - 2, Power);
  end;
end;

procedure TChebyshevWaveshaper.OrderChanged;
begin

 RecalculateHarmonics;
end;

procedure TChebyshevWaveshaper.RecalculateHarmonics;
var
  x, y  : Integer;
begin
 for y := 0 to Order do
  begin
   FChebyshevCoeffs[y] := FGains[0] * ChebyPolynome(1, y);
   for x := 1 to Order - 1 do
    if FGains[x] <> 0
     then FChebyshevCoeffs[y] := FChebyshevCoeffs[y] + FGains[x] * ChebyPolynome(1 + x, y);
  end;
end;

procedure TChebyshevWaveshaper.SetGain(Harmonic: Integer; const Value: Double);
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.Create('Index out of bounds (' + IntToStr(Harmonic) + ')')
  else
   begin
    FGains[Harmonic] := Value;
    RecalculateHarmonics;
   end;
end;

procedure TChebyshevWaveshaper.SetInverted(Harmonic: Integer;
  const Value: Boolean);
begin
 if Value
  then Gain[Harmonic] := -abs(Gain[Harmonic])
  else Gain[Harmonic] :=  abs(Gain[Harmonic]);
end;

procedure TChebyshevWaveshaper.SetLevel(Harmonic: Integer; const Value: Double);
begin
 if FGains[Harmonic] < 0
  then Gain[Harmonic] := -dB_to_Amp(Value)
  else Gain[Harmonic] :=  dB_to_Amp(Value);
end;

procedure TChebyshevWaveshaper.SetOrder(Value: Integer);
begin
 if Value < 1 then Value := 1 else
 if Value > 24 then Value := 24;
 if Value <> Order then
  begin
   SetLength(FGains, Value);
   SetLength(FChebyshevCoeffs, Value + 1);
   FGains[0] := 1;
   OrderChanged;
   RecalculateHarmonics;
  end;
end;

{ TChebyshevWaveshaperSquare }

procedure TChebyshevWaveshaperSquare.OrderChanged;
var
  i : Integer;
begin
 for i := 0 to Order - 1 do
  case i mod 4 of
   0: FGains[i] := -1 / (i + 1);
   2: FGains[i] :=  1 / (i + 1);
   else FGains[i] := 0;
  end;
 inherited;
end;

{ TChebyshevWaveshaperSquarelShape }

procedure TChebyshevWaveshaperSquarelShape.OrderChanged;
var
  i : Integer;
begin
 for i := 0 to Order - 1 do
  case i mod 4 of
   0: FGains[i] := -1 / Power(i + 1, FShape);
   2: FGains[i] :=  1 / Power(i + 1, FShape);
   else FGains[i] := 0;
  end;
 inherited;
end;

procedure TChebyshevWaveshaperSquarelShape.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   OrderChanged;
  end;
end;

end.
