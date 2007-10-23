unit DAVDComplex;

interface
  type
    TComplexSingle = record
                    Re : Single;
                    Im : Single;
                   end;
    TComplexDouble = record
                    Re : Double;
                    Im : Double;
                   end;

  function Complex(Re, Im : Double):TComplexDouble; overload;
  function Complex(Re, Im : Single):TComplexSingle; overload;

  function ComplexSign(A : TComplexSingle):Single; overload;
  function ComplexSign(A : TComplexDouble):Double; overload;
  function ComplexSign(Re, Im : Single):Single; overload;
  function ComplexSign(Re, Im : Double):Double; overload;

  function ComplexAbsolute(Re, Im : Double):Double; overload;
  function ComplexAbsolute(Re, Im : Single):Single; overload;
  function ComplexAbsolute(Complex:TComplexDouble):Double; overload;
  function ComplexAbsolute(Complex:TComplexSingle):Single; overload;

  function ComplexArgument(Re, Im : Double):Double; overload;
  function ComplexArgument(Re, Im : Single):Single; overload;
  function ComplexArgument(Complex:TComplexDouble):Double; overload;
  function ComplexArgument(Complex:TComplexSingle):Single; overload;

  function ComplexLog10(Re, Im : Single):TComplexSingle; overload;
  function ComplexLog10(Re, Im : Double):TComplexDouble; overload;
  function ComplexLog10(Complex: TComplexSingle):TComplexSingle; overload;
  function ComplexLog10(Complex: TComplexDouble):TComplexDouble; overload;

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
  
implementation

uses Math;

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

function ComplexSign(A : TComplexSingle):Single;
begin
 if (A.Re >= 0) and (A.Im > 0) then result:=1 else
 if (A.Re <= 0) and (A.Im < 0) then result:=-1
  else Result:=sign(A.Re);
end;

function ComplexSign(A : TComplexDouble):Double;
begin
 if (A.Re >= 0) and (A.Im > 0) then result:=1 else
 if (A.Re <= 0) and (A.Im < 0) then result:=-1
  else Result:=sign(A.Re);
end;

function ComplexSign(Re, Im : Single):Single;
begin
 if (Re >= 0) and (Im > 0) then result:=1 else
 if (Re <= 0) and (Im < 0) then result:=-1
  else Result:=sign(Re);
end;

function ComplexSign(Re, Im : Double):Double;
begin
 if (Re >= 0) and (Im > 0) then result:=1 else
 if (Re <= 0) and (Im < 0) then result:=-1
  else Result:=sign(Re);
end;

function ComplexLog10(Re, Im : Single):TComplexSingle;
begin
 Result.Re:=Log10((sqr(Re)+Sqr(Im)));
 Result.Im:=ArcTan2(Im,Re);
end;

function ComplexLog10(Re, Im : Double):TComplexDouble;
begin
 Result.Re:=Log10((sqr(Re)+Sqr(Im)));
 Result.Im:=ArcTan2(Im,Re);
end;

function ComplexLog10(Complex : TComplexSingle):TComplexSingle;
begin
 Result.Re:=Log10((sqr(Complex.Re)+Sqr(Complex.Im)));
 Result.Im:=ArcTan2(Complex.Im,Complex.Re);
end;

function ComplexLog10(Complex: TComplexDouble):TComplexDouble;
begin
 Result.Re:=Log10((sqr(Complex.Re)+Sqr(Complex.Im)));
 Result.Im:=ArcTan2(Complex.Im,Complex.Re);
end;

function ComplexAbsolute(Re, Im : Single):Single;
begin
 result:=sqrt(sqr(Re)+Sqr(Im));
end;

function ComplexAbsolute(Re, Im : Double):Double;
begin
 result:=sqrt(sqr(Re)+Sqr(Im));
end;

function ComplexAbsolute(Complex:TComplexDouble):Double;
begin
 result:=sqrt(sqr(Complex.Re)+Sqr(Complex.Im));
end;

function ComplexAbsolute(Complex:TComplexSingle):Single;
begin
 result:=sqrt(sqr(Complex.Re)+Sqr(Complex.Im));
end;

function ComplexArgument(Re, Im : Single):Single;
begin
 result:=ArcTan2(Im,Re);
end;

function ComplexArgument(Re, Im : Double):Double;
begin
 result:=ArcTan2(Im,Re);
end;

function ComplexArgument(Complex:TComplexDouble):Double;
begin
 result:=ArcTan2(Complex.Im,Complex.Re);
end;

function ComplexArgument(Complex:TComplexSingle):Single;
begin
 result:=ArcTan2(Complex.Im,Complex.Re);
end;

function ComplexMultiply(ARe,AIm,BRe,BIm : Single):TComplexSingle;
begin
 Result.Re := ARe * BRe - AIm * BIm;
 Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply(ARe,AIm,BRe,BIm : Double):TComplexDouble;
begin
 Result.Re := ARe * BRe - AIm * BIm;
 Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply(A,B : TComplexSingle):TComplexSingle;
begin
 Result.Re := A.Re * B.Re - A.Im * B.Im;
 Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

function ComplexMultiply(A,B : TComplexDouble):TComplexDouble;
begin
 Result.Re := A.Re * B.Re - A.Im * B.Im;
 Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

procedure ComplexMultiplyInplace(var A : TComplexSingle; B : TComplexSingle);
var Temp : Single;
begin
 Temp := A.Re;
 A.Re := A.Re * B.Re - A.Im * B.Im;
 A.Im := A.Im * B.Re + Temp * B.Im;
end;

procedure ComplexMultiplyInplace(var A : TComplexDouble; B : TComplexDouble);
var Temp : Double;
begin
 Temp := A.Re;
 A.Re := A.Re * B.Re - A.Im * B.Im;
 A.Im := A.Im * B.Re + Temp * B.Im;
end;

procedure ComplexMultiplyInplace(var ARe, AIm : Single; BRe, BIm : Single);
var Tmp : Single;
begin
 Tmp := ARe;
 ARe := ARe * BRe - AIm * BIm;
 AIm := AIm * BRe + Tmp * BIm;
end;

procedure ComplexMultiplyInplace(var ARe, AIm  : Double; BRe, BIm : Double);
var Tmp : Double;
begin
 Tmp := ARe;
 ARe := ARe * BRe - AIm * BIm;
 AIm := AIm * BRe + Tmp * BIm;
end;


function ComplexDivide(ARe,AIm,BRe,BIm : Single):TComplexSingle;
var Divisor : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Result.Re := (ARe * BRe + AIm * BIm) / Divisor;
 Result.Im := (AIm * BRe - ARe * BIm) / Divisor;
end;

function ComplexDivide(ARe,AIm,BRe,BIm : Double):TComplexDouble;
var Divisor : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Result.Re := (ARe * BRe + AIm * BIm) / Divisor;
 Result.Im := (AIm * BRe - ARe * BIm) / Divisor;
end;

function ComplexDivide(A,B : TComplexSingle):TComplexSingle;
var Divisor : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Result.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 Result.Im := (A.Im * B.Re - A.Re * B.Im) / Divisor;
end;

function ComplexDivide(A,B : TComplexDouble):TComplexDouble;
var Divisor : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Result.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 Result.Im := (A.Im * B.Re - A.Re * B.Im) / Divisor;
end;

procedure ComplexDivideInplace(var A : TComplexSingle; B : TComplexSingle);
var Divisor, Temp : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Temp := A.Re;
 A.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 A.Im := (A.Im * B.Re - Temp * B.Im) / Divisor;
end;

procedure ComplexDivideInplace(var A : TComplexDouble; B : TComplexDouble);
var Divisor, Temp : Double;
begin
 Divisor := sqr(B.Re) + sqr(B.Im);
 Temp := A.Re;
 A.Re := (A.Re * B.Re + A.Im * B.Im) / Divisor;
 A.Im := (A.Im * B.Re - Temp * B.Im) / Divisor;
end;

procedure ComplexDivideInplace(var ARe, AIm : Single; BRe, BIm : Single);
var Divisor, Temp : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Temp := ARe;
 ARe  := (ARe * BRe + AIm  * BIm) / Divisor;
 AIm  := (AIm * BRe - Temp * BIm) / Divisor;
end;

procedure ComplexDivideInplace(var ARe, AIm  : Double; BRe, BIm : Double);
var Divisor, Temp : Double;
begin
 Divisor := sqr(BRe) + sqr(BIm);
 Temp := ARe;
 ARe  := (ARe * BRe + AIm  * BIm) / Divisor;
 AIm  := (AIm * BRe - Temp * BIm) / Divisor;
end;

end.
