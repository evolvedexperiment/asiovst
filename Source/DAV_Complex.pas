unit DAV_Complex;

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

  PDAVComplexSingleDynArray = ^TDAVComplexSingleDynArray;
  TDAVComplexSingleDynArray = array of TComplexSingle;

  PDAVComplexDoubleDynArray = ^TDAVComplexDoubleDynArray;
  TDAVComplexDoubleDynArray = array of TComplexDouble;

  PDAVComplexSingleFixedArray = ^TDAVComplexSingleFixedArray;
  TDAVComplexSingleFixedArray = array [0..0] of TComplexSingle;

  PDAVComplexDoubleFixedArray = ^TDAVComplexDoubleFixedArray;
  TDAVComplexDoubleFixedArray = array [0..0] of TComplexDouble;

  PDAV2ComplexSingleArray = ^TDAV2ComplexSingleArray;
  TDAV2ComplexSingleArray = array [0..1] of TComplexSingle;
  PDAV4ComplexSingleArray = ^TDAV4ComplexSingleArray;
  TDAV4ComplexSingleArray = array [0..3] of TComplexSingle;

  PDAV2ComplexDoubleArray = ^TDAV2ComplexDoubleArray;
  TDAV2ComplexDoubleArray = array [0..1] of TComplexDouble;
  PDAV4ComplexDoubleArray = ^TDAV4ComplexDoubleArray;
  TDAV4ComplexDoubleArray = array [0..3] of TComplexDouble;

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

  procedure ComplexMultiplyInplace(var A : TComplexSingle; const B : TComplexSingle); overload;
  procedure ComplexMultiplyInplace(var A : TComplexDouble; const B : TComplexDouble); overload;
  procedure ComplexMultiplyInplace(var ARe,AIm : Single; const BRe, BIm : Single); overload;
  procedure ComplexMultiplyInplace(var ARe,AIm : Double; const BRe, BIm : Double); overload;

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
  Math {$IFDEF Delphi5}, DAV_Common{$ENDIF};

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

procedure ComplexMultiplyInplace(var A : TComplexSingle; const B : TComplexSingle);
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
 Temp := A.Re;
 A.Re := A.Re * B.Re - A.Im * B.Im;
 A.Im := A.Im * B.Re + Temp * B.Im;
end;
{$ELSE}
asm
 fld A.Re.Single    // A.Re
 fld A.Im.Single    // A.Im, A.Re
 fld B.Re.Single    // B.Re, A.Im, A.Re
 fld B.Im.Single    // B.Im, B.Re, A.Im, A.Re
 fld st(3)          // A.Re, B.Im, B.Re, A.Im, A.Re
 fmul st(0), st(2)  // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fld st(3)          // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fmul st(0), st(2)  // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fsubp              // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
 fstp A.Re.Single   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
 fxch st(2)         // A.Im, B.Re, B.Im, A.Re
 fmulp              // A.Im * B.Re, B.Im, A.Re
 fxch st(2)         // B.Im, A.Re, A.Im * B.Re
 fmulp              // B.Im * A.Re, A.Im * B.Re
 faddp              // A.Im * B.Re + A.Re * B.Im
 fstp A.Im.Single   // A.Im := A.Im * B.Re + A.Re * B.Im
end;
{$ENDIF}

procedure ComplexMultiplyInplace(var A : TComplexDouble; const B : TComplexDouble);
var
  Temp : Double;
begin
 Temp := A.Re;
 A.Re := A.Re * B.Re - A.Im * B.Im;
 A.Im := A.Im * B.Re + Temp * B.Im;
end;

procedure ComplexMultiplyInplace(var ARe, AIm : Single; const BRe, BIm : Single);
var
  Tmp : Single;
begin
 Tmp := ARe;
 ARe := ARe * BRe - AIm * BIm;
 AIm := AIm * BRe + Tmp * BIm;
end;

procedure ComplexMultiplyInplace(var ARe, AIm  : Double; const BRe, BIm : Double);
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
