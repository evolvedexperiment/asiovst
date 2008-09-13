unit DAVDspInterpolation;

interface

{$I ASIOVST.INC}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

  function Hermite1(const x,y0,y1,y2,y3:Single):Single;
  function Hermite2(const x,y0,y1,y2,y3:Single):Single;
  function Hermite3(const x,y0,y1,y2,y3:Single):Single;
  function Hermite4(const frac_pos, xm1, x0, x1, x2: Single): Single;
  function Hermite32_asm(const frac_pos: Single; pntr : PSingle) : Single;
  function Hermite64_asm(const frac_pos: Double; pntr : PDouble) : Double;
  function Hermite32I_asm(const frac_pos: Single; pntr : PSingle) : Single;
  function Hermite64I_asm(const frac_pos: Double; pntr : PDouble) : Double;
  function LinearInterpolation(f, a, b: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function LinearInterpolation(f, a, b: Double): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function CubicInterpolation(fr,inm1,inp,inp1,inp2:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}

implementation

const
  c_half : Double = 0.5;

{
  Hermite Interpolators
  ---------------------

  Parameter Explanation:

     frac_pos  :  fractional value [0.0f - 1.0f] to interpolator
     pntr      :  pointer to float array where:
     pntr[0]   :  previous sample (idx = -1)
     pntr[1]   :  current sample (idx = 0)
     pntr[2]   :  next sample (idx = +1)
     pntr[3]   :  after next sample (idx = +2)

     The interpolation takes place between pntr[1] and pntr[2].

  Types:

    Hermite32_asm   :  Direct Data, Single Precision
    Hermite64_asm   :  Direct Data, Double Precision
    Hermite32I_asm  :  Interleaved Data, Single Precision
    Hermite64I_asm  :  Interleaved Data, Double Precision

    The interleaved versions are interesting for performing an interpolation
    of complex values.
}

function Hermite32_asm(const frac_pos: Single; pntr : PSingle) : Single;
asm
 fld  [pntr +  8].Single;      // x1
 fsub [pntr     ].Single       // x1-xm1
 fld  [pntr +  4].Single       // x0           x1-xm1
 fsub [pntr +  8].Single       // v            x1-xm1
 fld  [pntr + 12].Single       // x2           v            x1-xm1
 fsub [pntr +  4].Single       // x2-x0        v            x1-xm1
 fxch st(2)                    // x1-m1        v            x2-x0
 fmul c_half                   // c            v            x2-x0
 fxch st(2)                    // x2-x0        v            c
 fmul c_half                   // 0.5*(x2-x0)  v            c
 fxch st(2)                    // c            v            0.5*(x2-x0)
 fst st(3)                     // c            v            0.5*(x2-x0)  c
 fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
 fxch st(2)                    // 0.5*(x2-x0)  v            w            c
 faddp st(1), st(0)            // v+.5(x2-x0)  w            c
 fadd st(0), st(1)             // a            w            c
 fadd st(1), st(0)             // a            b_neg        c
 fmul frac_pos.Single          // a*frac       b_neg        c
 fsubrp st(1), st(0)           // a*f-b        c
 fmul frac_pos.Single          // (a*f-b)*f    c
 faddp st(1), st(0)            // res-x0/f
 fmul frac_pos.Single          // res-x0
 fadd [pntr + 4].Single        // res
end;

function Hermite64_asm(const frac_pos: Double; pntr : PDouble) : Double;
asm
 fld   [pntr + 16].Double      // x1
 fsub  [pntr     ].Double      // x1-xm1
 fld   [pntr +  8].Double      // x0           x1-xm1
 fsub  [pntr + 16].Double      // v            x1-xm1
 fld   [pntr + 24].Double      // x2           v            x1-xm1
 fsub  [pntr +  8].Double      // x2-x0        v            x1-xm1
 fxch st(2)                    // x1-m1        v            x2-x0
 fmul c_half                   // c            v            x2-x0
 fxch st(2)                    // x2-x0        v            c
 fmul c_half                   // 0.5*(x2-x0)  v            c
 fxch st(2)                    // c            v            0.5*(x2-x0)
 fst st(3)                     // c            v            0.5*(x2-x0)  c
 fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
 fxch st(2)                    // 0.5*(x2-x0)  v            w            c
 faddp st(1), st(0)            // v+.5(x2-x0)  w            c
 fadd st(0), st(1)             // a            w            c
 fadd st(1), st(0)             // a            b_neg        c
 fmul frac_pos.Double          // a*frac       b_neg        c
 fsubrp st(1), st(0)           // a*f-b        c
 fmul frac_pos.Double          // (a*f-b)*f    c
 faddp st(1), st(0)            // res-x0/f
 fmul frac_pos.Double          // res-x0
 fadd [pntr + 8].Double        // res
end;

function Hermite32I_asm(const frac_pos: Single; pntr : PSingle) : Single;
asm
 fld   [pntr + 16].Single      // x1
 fsub  [pntr     ].Single      // x1-xm1
 fld   [pntr +  8].Single      // x0           x1-xm1
 fsub  [pntr + 16].Single      // v            x1-xm1
 fld   [pntr + 24].Single      // x2           v            x1-xm1
 fsub  [pntr +  8].Single      // x2-x0        v            x1-xm1
 fxch st(2)                    // x1-m1        v            x2-x0
 fmul c_half                   // c            v            x2-x0
 fxch st(2)                    // x2-x0        v            c
 fmul c_half                   // 0.5*(x2-x0)  v            c
 fxch st(2)                    // c            v            0.5*(x2-x0)
 fst st(3)                     // c            v            0.5*(x2-x0)  c
 fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
 fxch st(2)                    // 0.5*(x2-x0)  v            w            c
 faddp st(1), st(0)            // v+.5(x2-x0)  w            c
 fadd st(0), st(1)             // a            w            c
 fadd st(1), st(0)             // a            b_neg        c
 fmul frac_pos.Single          // a*frac       b_neg        c
 fsubrp st(1), st(0)           // a*f-b        c
 fmul frac_pos.Single          // (a*f-b)*f    c
 faddp st(1), st(0)            // res-x0/f
 fmul frac_pos.Single          // res-x0
 fadd [pntr + 8].Single        // res
end;

function Hermite64I_asm(const frac_pos: Double; pntr : PDouble) : Double;
asm
 fld   [pntr + 32].Double      // x1
 fsub  [pntr     ].Double      // x1-xm1
 fld   [pntr + 16].Double      // x0           x1-xm1
 fsub  [pntr + 32].Double      // v            x1-xm1
 fld   [pntr + 48].Double      // x2           v            x1-xm1
 fsub  [pntr + 16].Double      // x2-x0        v            x1-xm1
 fxch st(2)                    // x1-m1        v            x2-x0
 fmul c_half                   // c            v            x2-x0
 fxch st(2)                    // x2-x0        v            c
 fmul c_half                   // 0.5*(x2-x0)  v            c
 fxch st(2)                    // c            v            0.5*(x2-x0)
 fst st(3)                     // c            v            0.5*(x2-x0)  c
 fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
 fxch st(2)                    // 0.5*(x2-x0)  v            w            c
 faddp st(1), st(0)            // v+.5(x2-x0)  w            c
 fadd st(0), st(1)             // a            w            c
 fadd st(1), st(0)             // a            b_neg        c
 fmul frac_pos.Double          // a*frac       b_neg        c
 fsubrp st(1), st(0)           // a*f-b        c
 fmul frac_pos.Double          // (a*f-b)*f    c
 faddp st(1), st(0)            // res-x0/f
 fmul frac_pos.Double          // res-x0
 fadd [pntr + 16].Double       // res
end;

function Hermite1(const x, y0, y1, y2, y3: Single): Single;
var
  c0, c1, c2, c3: Single;
begin
 // 4-point, 3rd-order Hermite (x-form)
 c0     := y1;
 c1     := 0.5 * (y2 - y0);
 c2     := y0 - 2.5 * y1 + 2 * y2 - 0.5 * y3;
 c3     := 1.5 * (y1 - y2) + 0.5 * (y3 - y0);
 Result := ((c3 * x + c2) * x + c1) * x + c0;
end;

function Hermite2(const x, y0, y1, y2, y3: Single): Single;
var
  c0, c1, c2, c3: Single;
begin
 // 4-point, 3rd-order Hermite (x-form)
 c0     := y1;
 c1     := 0.5 * (y2 - y0);
 c3     := 1.5 * (y1 - y2) + 0.5 * (y3 - y0);
 c2     := y0 - y1 + c1 - c3;
 Result := ((c3 * x + c2) * x + c1) * x + c0;
end;

function Hermite3(const x, y0, y1, y2, y3: Single): Single;
var
  c0, c1, c2, c3, y0my1 : Single;
begin
 // 4-point, 3rd-order Hermite (x-form)
 c0     := y1;
 c1     := 0.5 * (y2 - y0);
 y0my1  := y0 - y1;
 c3     := (y1 - y2) + 0.5 * (y3 - y0my1 - y2);
 c2     := y0my1 + c1 - c3;
 Result := ((c3 * x + c2) * x + c1) * x + c0;
end;

function Hermite4(const frac_pos, xm1, x0, x1, x2: Single): Single;
var
  c, v, w, a : Single;
  b_neg      : Single;
begin
 c  := (x1-xm1)*0.5;
 v := x0-x1;
 w := c+v;
 a := w+v+(x2-x0)*0.5;
 b_neg := w + a;
 Result := ((((a * frac_pos) - b_neg) * frac_pos + c) * frac_pos + x0);
end;

function LinearInterpolation(f, a , b: Single): Single;
begin
 Result := (1 - f) * a + f * b;
end;

function LinearInterpolation(f, a , b: Double): Double;
begin
 Result := (1 - f) * a + f * b;
end;

function CubicInterpolation(fr, inm1, inp, inp1, inp2: Single): Single;
begin
 Result := inp + 0.5 * fr * (inp1 - inm1 + fr * (4 * inp1 + 2 * inm1 - 5 * inp - inp2 + fr * (3 * (inp - inp1) - inm1 + inp2)));
end;

end.
