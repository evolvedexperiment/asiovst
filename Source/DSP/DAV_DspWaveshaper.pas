unit DAV_DspWaveshaper;

interface

{$I ASIOVST.INC}

uses
  Math, DAV_Common;

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
function Saturate(input, fMax: Single): Single; overload;
function Saturate(input, fMax: Double): Double; overload;
function Saturate2(input, fMax: Single): Single; overload;
function Saturate2(input, fMax: Double): Double; overload;
function SoftSat(x, a:Single): Single; overload;
function SoftSat(x, a:Double): Double; overload;

implementation

{$IFDEF DELPHI5}
function Sign(const AValue: Double): TValueSign;
begin
 if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000)
  then Result := 0 else
 if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000)
  then Result := -1 else Result := 1;
end;
{$ENDIF}

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
     then Result:=  t + (1 - t) * tanh(( x - t) / (1 - t))
     else Result:=-(t + (1 - t) * tanh((-x - t) / (1 - t)));
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

function Waveshaper7(x,a:Double):Double;
begin
 Result := sign(x) * Exp(ln(Abs(x)) * a);
end;

function Waveshaper8(x,a:Single):Single;
begin
 Result := sign(x) * Exp(ln(a) * Abs(x));
end;

function Waveshaper8(x,a:Double):Double;
begin
 Result := sign(x) * Exp(ln(a) * Abs(x));
end;

function Saturate(input, fMax: Single): Single;
{$IFNDEF FPC}
const fGrdDiv : Double = 0.5;
asm
 fld input.Single
 fadd fMax
 fabs
 fld input.Single
 fsub fMax
 fabs
 fsubp
 fmul fGrdDiv;
// result := fGrdDiv * (Abs(input + fMax) - Abs(input - fMax));
end;
{$ELSE}
begin
 result := 0.5 * (Abs(input + fMax) - Abs(input - fMax));
end;
{$ENDIF}

function Saturate(input, fMax: Double): Double;
{$IFNDEF FPC}
const fGrdDiv : Double = 0.5;
asm
 fld input.Double
 fadd fMax.Double
 fabs
 fld input.Double
 fsub fMax.Double
 fabs
 fsubp
 fmul fGrdDiv;
end;
{$ELSE}
begin
 result := 0.5 * (Abs(input + fMax) - Abs(input - fMax));
end;
{$ENDIF}

function Saturate2(input, fMax: Single): Single;
begin
 if input > fMax
  then result := fMax
  else
   if input < -fMax
    then result := -fMax
    else Result := input;
end;

function Saturate2(input, fMax: Double): Double;
begin
 if input > fMax
  then result := fMax
  else
   if input < -fMax
    then result := -fMax
    else Result := input;
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

end.
