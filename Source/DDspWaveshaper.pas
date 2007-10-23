unit DDspWaveshaper;

interface

  function Waveshaper1(x,t:Single):Single; overload;
  function Waveshaper1(x,t:Double):Double; overload;
  function Waveshaper2(x,t:Single):Single; overload;
  function Waveshaper2(x,t:Double):Double; overload;
  function Waveshaper3(x,a:Single):Single; overload;
  function Waveshaper3(x,a:Double):Double; overload;
  function Waveshaper4(x,a:Single):Single; overload;
  function Waveshaper4(x,a:Double):Double; overload;
  function Waveshaper5(x,a:Single):Single; overload;
  function Waveshaper5(x,a:Double):Double; overload;
  function Waveshaper6(x:Single):Single; overload;
  function Waveshaper6(x:Double):Double; overload;
  function Waveshaper7(x,a:Single):Single; overload;
  function Waveshaper7(x,a:Double):Double; overload;
  function Waveshaper8(x,a:Single):Single; overload;
  function Waveshaper8(x,a:Double):Double; overload;
  function Saturate(input, fMax: single): single; overload;
  function Saturate(input, fMax: Double): Double; overload;
  function Saturate2(input, fMax: single): single; overload;
  function Saturate2(input, fMax: Double): Double; overload;
  function SoftSat(x,a:Single):Single; overload;
  function SoftSat(x,a:Double):Double; overload;

implementation

uses DAVDCommon, Math;

function Waveshaper1(x, t :Single):Single;
begin
 if f_abs(x)<t
  then Result:=x
  else
   begin
    if x>0
     then Result:=  t + (1-t)*tanh((x-t)/(1-t))
     else Result:=-(t + (1-t)*tanh((-x-t)/(1-t)));
   end;
end;

function Waveshaper1(x, t :Double):Double;
begin
 if f_abs(x)<t
  then Result:=x
  else
   begin
    if x>0
     then Result:=  t + (1-t)*tanh((x-t)/(1-t))
     else Result:=-(t + (1-t)*tanh((-x-t)/(1-t)));
   end;
end;

function Waveshaper2(x,t:Single):Single;
begin
 if f_abs(x)<t
  then Result:=x
  else
   begin
    if x>0
     then Result:=  t + (1-t)*sigmoid( (x-t)/((1-t)*1.5))
     else Result:=-(t + (1-t)*sigmoid((-x-t)/((1-t)*1.5)));
   end;
end;

function Waveshaper2(x,t:Double):Double;
begin
 if f_abs(x)<t
  then Result:=x
  else
   begin
    if x>0
     then Result:=  t + (1-t)*sigmoid( (x-t)/((1-t)*1.5))
     else Result:=-(t + (1-t)*sigmoid((-x-t)/((1-t)*1.5)));
   end;
end;

function Waveshaper3(x,a:Single):Single;
begin
 Result:=x*(abs(x)+a)/(x*x+(a-1)*abs(x)+1);
end;

function Waveshaper3(x,a:Double):Double;
begin
 Result:=x*(abs(x)+a)/(x*x+(a-1)*abs(x)+1);
end;

function Waveshaper4(x,a:Single):Single;
begin
 Result:=sign(x)*power(arctan(power(abs(x), a)), (1/a));
end;

function Waveshaper4(x,a:Double):Double;
begin
 Result:=sign(x)*power(arctan(power(abs(x), a)), (1/a));
end;

function Waveshaper5(x,a:Single):Single;
begin
 a:= 2*a/(1-a);
 Result:=(1+a)*x/(1+a*abs(x));
end;

function Waveshaper5(x,a:Double):Double;
begin
 a:= 2*a/(1-a);
 Result:=(1+a)*x/(1+a*abs(x));
end;

function Waveshaper6(x:Single):Single;
var a,b :Single;
begin
 x:=x*0.686306;
 a:=1+exp(sqrt(f_abs(x))*-0.75);
 b:=exp(x);
 Result:=(b-exp(-x*a))*b/(b*b+1);
end;

function Waveshaper6(x:Double):Double;
var a,b :Double;
begin
 x:=x*0.686306;
 a:=1+exp(sqrt(f_abs(x))*-0.75);
 b:=exp(x);
 Result:=(b-exp(-x*a))*b/(b*b+1);
end;

function Waveshaper7(x,a:Single):Single;
begin
 Result:=sign(x)*exp(ln(abs(x))*a);
end;

function Waveshaper7(x,a:Double):Double;
begin
 Result:=sign(x)*exp(ln(abs(x))*a);
end;

function Waveshaper8(x,a:Single):Single;
begin
 Result:=sign(x)*exp(ln(a)*abs(x));
end;

function Waveshaper8(x,a:Double):Double;
begin
 Result:=sign(x)*exp(ln(a)*abs(x));
end;

function Saturate(input, fMax: single): single;
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
// result := fGrdDiv * (f_abs(input + fMax) - f_abs(input - fMax));
end;

function Saturate(input, fMax: Double): Double;
const fGrdDiv : Double = 0.5;
{$IFNDEF FPC}
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
 result := fGrdDiv * (f_abs(input + fMax) - f_abs(input - fMax));
end;
{$ENDIF}

function Saturate2(input, fMax: single): single;
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

function SoftSat(x,a:Single):Single;
var b,c : Single;
begin
 b:=f_abs(x);
 if b<a then Result:=x else
 if b>1 then Result:=sign(x)*(a+1)*0.5 else
  begin
   c:=((x-a)/(1-a));
   Result:=a+(x-a)/(1+c*c);
  end;
end;

function SoftSat(x,a:Double):Double;
var b,c : Double;
begin
 b:=f_abs(x);
 if b<a then Result:=x else
 if b>1 then Result:=sign(x)*(a+1)*0.5 else
  begin
   c:=((x-a)/(1-a));
   Result:=a+(x-a)/(1+c*c);
  end;
end;


end.
