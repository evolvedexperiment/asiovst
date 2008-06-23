unit DAVDComplex;

interface

{$I ASIOVST.inc}

type
  PComplexSingle = ^TComplexSingle;
  TComplexSingle = record
                    Re : Single;
                    Im : Single;
                   end;

  PComplexDouble = ^TComplexDouble;
  TComplexDouble = record
                    Re : Double;
                    Im : Double;
                   end;

  PAVDComplexSingleDynArray = ^TAVDComplexSingleDynArray;
  TAVDComplexSingleDynArray = array of TComplexSingle;

  PAVDComplexDoubleDynArray = ^TAVDComplexDoubleDynArray;
  TAVDComplexDoubleDynArray = array of TComplexDouble;

  PAVDComplexSingleFixedArray = ^TAVDComplexSingleFixedArray;
  TAVDComplexSingleFixedArray = array [0..0] of TComplexSingle;

  PAVDComplexDoubleFixedArray = ^TAVDComplexDoubleFixedArray;
  TAVDComplexDoubleFixedArray = array [0..0] of TComplexDouble;

  function Complex(Re, Im : Double):TComplexDouble; overload;
  function Complex(Re, Im : Single):TComplexSingle; overload;

  function ComplexPolar(Magnitude, Angle: Single): TComplexSingle; overload;
  function ComplexPolar(Magnitude, Angle: Double): TComplexDouble; overload;

  function ComplexSign(A : TComplexSingle):Single; overload;
  function ComplexSign(A : TComplexDouble):Double; overload;
  function ComplexSign(Re, Im : Single):Single; overload;
  function ComplexSign(Re, Im : Double):Double; overload;

  function ComplexConjugate(Re, Im : Double): TComplexDouble; overload;
  function ComplexConjugate(Re, Im : Single): TComplexSingle; overload;
  function ComplexConjugate(a: TComplexDouble): TComplexDouble; overload;
  function ComplexConjugate(a: TComplexSingle): TComplexSingle; overload;

  function ComplexInvert(Re, Im : Double): TComplexDouble; overload;
  function ComplexInvert(Re, Im : Single): TComplexSingle; overload;
  function ComplexInvert(a: TComplexDouble): TComplexDouble; overload;
  function ComplexInvert(a: TComplexSingle): TComplexSingle; overload;

  function ComplexMagnitude(Re, Im : Double):Double; overload;
  function ComplexMagnitude(Re, Im : Single):Single; overload;
  function ComplexMagnitude(Complex:TComplexDouble):Double; overload;
  function ComplexMagnitude(Complex:TComplexSingle):Single; overload;

  function ComplexArgument(Re, Im : Double):Double; overload;
  function ComplexArgument(Re, Im : Single):Single; overload;
  function ComplexArgument(Complex:TComplexDouble):Double; overload;
  function ComplexArgument(Complex:TComplexSingle):Single; overload;

  function ComplexLog10(Re, Im : Single):TComplexSingle; overload;
  function ComplexLog10(Re, Im : Double):TComplexDouble; overload;
  function ComplexLog10(Complex: TComplexSingle):TComplexSingle; overload;
  function ComplexLog10(Complex: TComplexDouble):TComplexDouble; overload;

  function ComplexAdd(A,B : TComplexSingle):TComplexSingle; overload;
  function ComplexAdd(A,B : TComplexDouble):TComplexDouble; overload;
  function ComplexAdd(ARe,AIm,BRe,BIm : Single):TComplexSingle; overload;
  function ComplexAdd(ARe,AIm,BRe,BIm : Double):TComplexDouble; overload;

  procedure ComplexAddInplace(var A : TComplexSingle; B : TComplexSingle); overload;
  procedure ComplexAddInplace(var A : TComplexDouble; B : TComplexDouble); overload;
  procedure ComplexAddInplace(var ARe,AIm : Single; BRe,BIm : Single); overload;
  procedure ComplexAddInplace(var ARe,AIm : Double; BRe,BIm : Double); overload;

  function ComplexSubtract(A,B : TComplexSingle):TComplexSingle; overload;
  function ComplexSubtract(A,B : TComplexDouble):TComplexDouble; overload;
  function ComplexSubtract(ARe,AIm,BRe,BIm : Single):TComplexSingle; overload;
  function ComplexSubtract(ARe,AIm,BRe,BIm : Double):TComplexDouble; overload;

  procedure ComplexSubtractInplace(var A : TComplexSingle; B : TComplexSingle); overload;
  procedure ComplexSubtractInplace(var A : TComplexDouble; B : TComplexDouble); overload;
  procedure ComplexSubtractInplace(var ARe,AIm : Single; BRe,BIm : Single); overload;
  procedure ComplexSubtractInplace(var ARe,AIm : Double; BRe,BIm : Double); overload;

  function ComplexMultiply(A,B : TComplexSingle):TComplexSingle; overload;
  function ComplexMultiply(A,B : TComplexDouble):TComplexDouble; overload;
  function ComplexMultiply(ARe,AIm,BRe,BIm : Single):TComplexSingle; overload;
  function ComplexMultiply(ARe,AIm,BRe,BIm : Double):TComplexDouble; overload;

  procedure ComplexMultiplyInplace(var A : TComplexSingle; B : TComplexSingle); overload;
  procedure ComplexMultiplyInplace(var A : TComplexDouble; B : TComplexDouble); overload;
  procedure ComplexMultiplyInplace(var ARe,AIm : Single; BRe,BIm : Single); overload;
  procedure ComplexMultiplyInplace(var ARe,AIm : Double; BRe,BIm : Double); overload;

  function ComplexDivide(A,B : TComplexSingle):TComplexSingle; overload;
  function ComplexDivide(A,B : TComplexDouble):TComplexDouble; overload;
  function ComplexDivide(ARe,AIm,BRe,BIm : Single):TComplexSingle; overload;
  function ComplexDivide(ARe,AIm,BRe,BIm : Double):TComplexDouble; overload;

  procedure ComplexDivideInplace(var A : TComplexSingle; B : TComplexSingle); overload;
  procedure ComplexDivideInplace(var A : TComplexDouble; B : TComplexDouble); overload;
  procedure ComplexDivideInplace(var ARe,AIm : Single; BRe,BIm : Single); overload;
  procedure ComplexDivideInplace(var ARe,AIm : Double; BRe,BIm : Double); overload;

  function ComplexSqr(Re, Im : Single): TComplexSingle; overload;
  function ComplexSqr(Re, Im : Double): TComplexDouble; overload;
  function ComplexSqr(a: TComplexSingle): TComplexSingle; overload;
  function ComplexSqr(a: TComplexDouble): TComplexDouble; overload;

  function ComplexSqrt(Re, Im : Single): TComplexSingle; overload;
  function ComplexSqrt(Re, Im : Double): TComplexDouble; overload;
  function ComplexSqrt(a: TComplexSingle): TComplexSingle; overload;
  function ComplexSqrt(a: TComplexDouble): TComplexDouble; overload;

implementation

uses
  Math;

{$IFDEF DELPHI5}
function Sign(const AValue: Double): TValueSign;
begin
 if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000)
  then Result := 0 else
 if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000)
  then Result := -1 else Result := 1;
end;
{$ENDIF}

function Complex(Re, Im : Double):TComplexDouble;
begin
 Result.Re:=Re;
 Result.Im:=Im;
end;

function Complex(Re, Im : Single):TComplexSingle;
begin
 Result.Re:=Re;
 Result.Im:=Im;
end;             


function ComplexPolar(Magnitude, Angle: Single): TComplexSingle;
begin
  Result.Re := Magnitude * cos(Angle);
  Result.Im := Magnitude * sin(Angle);
end;

function ComplexPolar(Magnitude, Angle: Double): TComplexDouble;
begin
  Result.Re := Magnitude * cos(Angle);
  Result.Im := Magnitude * sin(Angle);
end;

function ComplexSign(A : TComplexSingle):Single;
begin
 if (A.Re >= 0) and (A.Im > 0) then result :=  1 else
 if (A.Re <= 0) and (A.Im < 0) then result := -1
  else Result := sign(A.Re);
end;

function ComplexSign(A : TComplexDouble):Double;
begin
 if (A.Re >= 0) and (A.Im > 0) then result :=  1 else
 if (A.Re <= 0) and (A.Im < 0) then result := -1
  else Result := sign(A.Re);
end;

function ComplexSign(Re, Im : Single):Single;
begin
 if (Re >= 0) and (Im > 0) then result :=  1 else
 if (Re <= 0) and (Im < 0) then result := -1
  else Result := sign(Re);
end;

function ComplexSign(Re, Im : Double):Double;
begin
 if (Re >= 0) and (Im > 0) then result :=  1 else
 if (Re <= 0) and (Im < 0) then result := -1
  else Result := sign(Re);
end;

function ComplexConjugate(Re, Im : Double): TComplexDouble;
begin
  Result.Re :=  Re;
  Result.Im := -Im;
end;

function ComplexConjugate(Re, Im : Single): TComplexSingle;
begin
  Result.Re :=  Re;
  Result.Im := -Im;
end;


function ComplexConjugate(a: TComplexSingle): TComplexSingle;
begin
  Result.Re :=  a.Re;
  Result.Im := -a.Im;
end;

function ComplexConjugate(a: TComplexDouble): TComplexDouble;
begin
  Result.Re :=  a.Re;
  Result.Im := -a.Im;
end;

     
function ComplexInvert(Re, Im : Double): TComplexDouble;
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert(Re, Im : Single): TComplexSingle; 
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert(a: TComplexSingle): TComplexSingle;
begin
  Result.Re := -a.Re;
  Result.Im := -a.Im;
end;

function ComplexInvert(a: TComplexDouble): TComplexDouble;
begin
  Result.Re := -a.Re;
  Result.Im := -a.Im;
end;


function ComplexLog10(Re, Im : Single): TComplexSingle;
begin
 Result.Re := Log10((sqr(Re) + Sqr(Im)));
 Result.Im := ArcTan2(Im,Re);
end;

function ComplexLog10(Re, Im : Double): TComplexDouble;
begin
 Result.Re := Log10((sqr(Re) + Sqr(Im)));
 Result.Im := ArcTan2(Im,Re);
end;

function ComplexLog10(Complex : TComplexSingle):TComplexSingle;
begin
 Result.Re := Log10((sqr(Complex.Re) + Sqr(Complex.Im)));
 Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexLog10(Complex: TComplexDouble):TComplexDouble;
begin
 Result.Re := Log10((sqr(Complex.Re) + Sqr(Complex.Im)));
 Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexMagnitude(Re, Im : Single):Single;
begin
 result := hypot(Re, Im);
end;

function ComplexMagnitude(Re, Im : Double):Double;
begin
 result := hypot(Re, Im);
end;

function ComplexMagnitude(Complex:TComplexDouble):Double;
begin
 result := hypot(Complex.Re, Complex.Im);
end;

function ComplexMagnitude(Complex:TComplexSingle):Single;
begin
 result := hypot(Complex.Re, Complex.Im);
end;

function ComplexArgument(Re, Im : Single): Single;
begin
 result := ArcTan2(Im, Re);
end;

function ComplexArgument(Re, Im : Double): Double;
begin
 result := ArcTan2(Im, Re);
end;

function ComplexArgument(Complex: TComplexDouble): Double;
begin
 result := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexArgument(Complex: TComplexSingle): Single;
begin
 result := ArcTan2(Complex.Im, Complex.Re);
end;


function ComplexAdd(ARe, AIm, BRe, BIm : Single): TComplexSingle;
begin
 Result.Re := ARe + BRe;
 Result.Im := AIm + BIm;
end;

function ComplexAdd(ARe, AIm, BRe, BIm : Double): TComplexDouble;
begin
 Result.Re := ARe + BRe;
 Result.Im := AIm + BIm;
end;

function ComplexAdd(A,B : TComplexSingle): TComplexSingle;
begin
 Result.Re := A.Re + B.Re;
 Result.Im := A.Im + B.Im;
end;

function ComplexAdd(A,B : TComplexDouble): TComplexDouble;
begin
 Result.Re := A.Re + B.Re;
 Result.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace(var A : TComplexSingle; B : TComplexSingle);
begin
 A.Re := A.Re + B.Re;
 A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace(var A : TComplexDouble; B : TComplexDouble);
begin
 A.Re := A.Re + B.Re;
 A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace(var ARe, AIm : Single; BRe, BIm : Single);
begin
 ARe := ARe + BRe;
 AIm := AIm + BIm;
end;

procedure ComplexAddInplace(var ARe, AIm  : Double; BRe, BIm : Double);
begin
 ARe := ARe + BRe;
 AIm := AIm + BIm;
end;


function ComplexSubtract(ARe,AIm,BRe,BIm : Single):TComplexSingle;
begin
 Result.Re := ARe - BRe;
 Result.Im := AIm - BIm;
end;

function ComplexSubtract(ARe,AIm,BRe,BIm : Double):TComplexDouble;
begin
 Result.Re := ARe - BRe;
 Result.Im := AIm - BIm;
end;

function ComplexSubtract(A,B : TComplexSingle):TComplexSingle;
begin
 Result.Re := A.Re - B.Re;
 Result.Im := A.Im - B.Im;
end;

function ComplexSubtract(A,B : TComplexDouble):TComplexDouble;
begin
 Result.Re := A.Re - B.Re;
 Result.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace(var A : TComplexSingle; B : TComplexSingle);
begin
 A.Re := A.Re - B.Re;
 A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace(var A : TComplexDouble; B : TComplexDouble);
begin
 A.Re := A.Re - B.Re;
 A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace(var ARe, AIm : Single; BRe, BIm : Single);
begin
 ARe := ARe - BRe;
 AIm := AIm - BIm;
end;

procedure ComplexSubtractInplace(var ARe, AIm  : Double; BRe, BIm : Double);
begin
 ARe := ARe - BRe;
 AIm := AIm - BIm;
end;


function ComplexMultiply(ARe, AIm, BRe, BIm : Single):TComplexSingle;
begin
 Result.Re := ARe * BRe - AIm * BIm;
 Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply(ARe, AIm, BRe, BIm : Double):TComplexDouble;
begin
 Result.Re := ARe * BRe - AIm * BIm;
 Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply(A, B : TComplexSingle):TComplexSingle;
begin
 Result.Re := A.Re * B.Re - A.Im * B.Im;
 Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

function ComplexMultiply(A, B : TComplexDouble):TComplexDouble;
begin
 Result.Re := A.Re * B.Re - A.Im * B.Im;
 Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

procedure ComplexMultiplyInplace(var A : TComplexSingle; B : TComplexSingle);
var
  Temp : Single;
begin
 Temp := A.Re;
 A.Re := A.Re * B.Re - A.Im * B.Im;
 A.Im := A.Im * B.Re + Temp * B.Im;
end;

procedure ComplexMultiplyInplace(var A : TComplexDouble; B : TComplexDouble);
var
  Temp : Double;
begin
 Temp := A.Re;
 A.Re := A.Re * B.Re - A.Im * B.Im;
 A.Im := A.Im * B.Re + Temp * B.Im;
end;

procedure ComplexMultiplyInplace(var ARe, AIm : Single; BRe, BIm : Single);
var
  Tmp : Single;
begin
 Tmp := ARe;
 ARe := ARe * BRe - AIm * BIm;
 AIm := AIm * BRe + Tmp * BIm;
end;

procedure ComplexMultiplyInplace(var ARe, AIm  : Double; BRe, BIm : Double);
var
  Tmp : Double;
begin
 Tmp := ARe;
 ARe := ARe * BRe - AIm * BIm;
 AIm := AIm * BRe + Tmp * BIm;
end;


function ComplexDivide(ARe, AIm, BRe, BIm : Single): TComplexSingle;
var
  Divisor : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Result.Re := (ARe * BRe + AIm * BIm) / Divisor;
 Result.Im := (AIm * BRe - ARe * BIm) / Divisor;
end;

function ComplexDivide(ARe, AIm, BRe, BIm : Double): TComplexDouble;
var
  Divisor : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Result.Re := (ARe * BRe + AIm * BIm) / Divisor;
 Result.Im := (AIm * BRe - ARe * BIm) / Divisor;
end;

function ComplexDivide(A, B : TComplexSingle): TComplexSingle;
var
  Divisor : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Result.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 Result.Im := (A.Im * B.Re - A.Re * B.Im) / Divisor;
end;

function ComplexDivide(A,B : TComplexDouble): TComplexDouble;
var
  Divisor : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Result.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 Result.Im := (A.Im * B.Re - A.Re * B.Im) / Divisor;
end;

procedure ComplexDivideInplace(var A : TComplexSingle; B : TComplexSingle);
var
  Divisor, Temp : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Temp := A.Re;
 A.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 A.Im := (A.Im * B.Re - Temp * B.Im) / Divisor;
end;

procedure ComplexDivideInplace(var A : TComplexDouble; B : TComplexDouble);
var
  Divisor, Temp : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Temp := A.Re;
 A.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 A.Im := (A.Im * B.Re - Temp * B.Im) / Divisor;
end;

procedure ComplexDivideInplace(var ARe, AIm : Single; BRe, BIm : Single);
var
  Divisor, Temp : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Temp := ARe;
 ARe  := (ARe * BRe + AIm  * BIm) / Divisor;
 AIm  := (AIm * BRe - Temp * BIm) / Divisor;
end;

procedure ComplexDivideInplace(var ARe, AIm  : Double; BRe, BIm : Double);
var
  Divisor, Temp : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Temp := ARe;
 ARe  := (ARe * BRe + AIm  * BIm) / Divisor;
 AIm  := (AIm * BRe - Temp * BIm) / Divisor;
end;



function ComplexSqr(Re, Im : Single): TComplexSingle;
begin
 Result.Re := sqr(Re) - sqr(Im);
 Result.Im := 2 * Re * Im;
end;

function ComplexSqr(Re, Im : Double): TComplexDouble;
begin
 Result.Re := sqr(Re) - sqr(Im);
 Result.Im := 2 * Re * Im;
end;

function ComplexSqr(a: TComplexSingle): TComplexSingle;
begin
 Result.Re := sqr(a.Re) - sqr(a.Im);
 Result.Im := 2 * a.Re * a.Im;
end;

function ComplexSqr(a: TComplexDouble): TComplexDouble;
begin
 Result.Re := sqr(a.Re) - sqr(a.Im);
 Result.Im := 2 * a.Re * a.Im;
end;



function ComplexSqrt(Re, Im : Single): TComplexSingle;

  function FSqrt(x: Single): Double;
  begin
    if x > 0
     then Result:= Sqrt(x)
     else Result := 0;
  end;

var
  Mag: Single;
begin
 Mag:= ComplexMagnitude(Re, Im);
 Result.Re := FSqrt(0.5 * (Mag + Re));
 Result.Im := FSqrt(0.5 * (Mag - Re));
 if (Im < 0.0) then Result.Im := -Result.Im;
end;

function ComplexSqrt(Re, Im : Double): TComplexDouble;

  function FSqrt(x: Double): Double;
  begin
   if x > 0 then Result:= Sqrt(x) else Result := 0;
  end;
  
var
  Mag: Double;
begin
  Mag       := ComplexMagnitude(Re, Im);
  Result.Re := FSqrt(0.5 * (Mag + Re));
  Result.Im := FSqrt(0.5 * (Mag - Re));
  if (Im < 0.0) then Result.Im := -Result.Im;
end;

function ComplexSqrt(a: TComplexSingle): TComplexSingle;

  function FSqrt(x: Single): Double;
  begin
    if x>0 then Result := Sqrt(x) else Result := 0;
  end;

var
  Mag: Single;
begin
  Mag       := ComplexMagnitude(a);
  Result.Re := FSqrt(0.5 * (Mag + a.Re));
  Result.Im := FSqrt(0.5 * (Mag - a.Re));
  if (a.Im < 0.0)
   then Result.Im:= -Result.Im;
end;

function ComplexSqrt(a: TComplexDouble): TComplexDouble;

  function FSqrt(x: Double): Double;
  begin
    if x>0 then Result:= Sqrt(x) else Result := 0;
  end;

var
  Mag: Double;
begin
  Mag       := ComplexMagnitude(a);
  Result.Re := FSqrt(0.5 * (Mag + a.Re));
  Result.Im := FSqrt(0.5 * (Mag - a.Re));
  if (a.Im < 0.0)
   then Result.Im := -Result.Im;
end;

end.
