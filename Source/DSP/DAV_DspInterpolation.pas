unit DAV_DspInterpolation;

interface

{$I ..\ASIOVST.INC}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  DAV_Common;

function Hermite1(const Fractional: Single; const y: TDAV4SingleArray): Single;
function Hermite2(const Fractional: Single; const y: TDAV4SingleArray): Single;
function Hermite3(const Fractional: Single; const y: TDAV4SingleArray): Single;
function Hermite4(const Fractional: Single; const y: TDAV4SingleArray): Single;
function Hermite32_asm(const Fractional: Single; Pntr: PDAV4SingleArray): Single;
function Hermite64_asm(const Fractional: Double; Pntr: PDAV4DoubleArray): Double;
function Hermite32I_asm(const Fractional: Single; Pntr: PSingle): Single;
function Hermite64I_asm(const Fractional: Double; Pntr: PDouble): Double;
function LinearInterpolation(f, a, b: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function LinearInterpolation(f, a, b: Double): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function CubicInterpolation(fr, inm1, inp, inp1, inp2: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}

implementation

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Hermite Interpolators                                                     //
//  ---------------------                                                     //
//                                                                            //
//  Parameter Explanation:                                                    //
//                                                                            //
//     Fractional  :  fractional value [0.0f - 1.0f] to interpolator            //
//     Pntr      :  pointer to float array where:                             //
//     Pntr[0]   :  previous sample (idx = -1)                                //
//     Pntr[1]   :  current sample (idx = 0)                                  //
//     Pntr[2]   :  next sample (idx = +1)                                    //
//     Pntr[3]   :  after next sample (idx = +2)                              //
//                                                                            //
//     The interpolation takes place between Pntr[1] and Pntr[2].             //
//                                                                            //
//  Types:                                                                    //
//                                                                            //
//    Hermite32_asm   :  Direct Data, Single Precision                        //
//    Hermite64_asm   :  Direct Data, Double Precision                        //
//    Hermite32I_asm  :  Interleaved Data, Single Precision                   //
//    Hermite64I_asm  :  Interleaved Data, Double Precision                   //
//                                                                            //
//    The interleaved versions are interesting for performing an              //
//    interpolation of complex values.                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function Hermite32_asm(const Fractional: Single; Pntr: PDAV4SingleArray): Single;
{$IFDEF PUREPASCAL}
var
  c : TDAV4SingleArray;
  b : Single;
begin
  c[0] := (Pntr^[2] - Pntr[0]) * CHalf32;
  c[1] := Pntr[1] - Pntr[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] + (Pntr[3] - Pntr[1]) * CHalf32;
  b    := c[2] + c[3];
  Result := ((((c[3] * Fractional) - b) * Fractional + c[0]) * Fractional + Pntr[1]);
end;
{$ELSE}
asm
    fld  [Pntr +  8].Single;      // x1
    fsub [Pntr     ].Single       // x1-xm1
    fld  [Pntr +  4].Single       // x0           x1-xm1
    fsub [Pntr +  8].Single       // v            x1-xm1
    fld  [Pntr + 12].Single       // x2           v            x1-xm1
    fsub [Pntr +  4].Single       // x2-x0        v            x1-xm1
    fxch st(2)                    // x1-m1        v            x2-x0
    fmul CHalf32                  // c            v            x2-x0
    fxch st(2)                    // x2-x0        v            c
    fmul CHalf32                  // 0.5*(x2-x0)  v            c
    fxch st(2)                    // c            v            0.5*(x2-x0)
    fst st(3)                     // c            v            0.5*(x2-x0)  c
    fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
    fxch st(2)                    // 0.5*(x2-x0)  v            w            c
    faddp st(1), st(0)            // v+.5(x2-x0)  w            c
    fadd st(0), st(1)             // a            w            c
    fadd st(1), st(0)             // a            b_neg        c
    fmul Fractional.Single        // a * frac     b_neg        c
    fsubrp st(1), st(0)           // a * f-b      c
    fmul Fractional.Single        // (a*f-b)*f    c
    faddp st(1), st(0)            // res-x0/f
    fmul Fractional.Single        // res-x0
    fadd [Pntr + 4].Single        // res
end;
{$ENDIF}

function Hermite64_asm(const Fractional: Double; Pntr: PDAV4DoubleArray): Double;
{$IFDEF PUREPASCAL}
var
  c : TDAV4DoubleArray;
  b : Single;
begin
  c[0] := (Pntr[2] - Pntr[0]) * CHalf32;
  c[1] := Pntr[1] - Pntr[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] + (Pntr[3] - Pntr[1]) * CHalf32;
  b    := c[2] + c[3];
  Result := ((((c[3] * Fractional) - b) * Fractional + c[0]) * Fractional + Pntr[1]);
end;
{$ELSE}
asm
    fld   [Pntr + 16].Double      // x1
    fsub  [Pntr     ].Double      // x1-xm1
    fld   [Pntr +  8].Double      // x0           x1-xm1
    fsub  [Pntr + 16].Double      // v            x1-xm1
    fld   [Pntr + 24].Double      // x2           v            x1-xm1
    fsub  [Pntr +  8].Double      // x2-x0        v            x1-xm1
    fxch st(2)                    // x1-m1        v            x2-x0
    fmul CHalf64                  // c            v            x2-x0
    fxch st(2)                    // x2-x0        v            c
    fmul CHalf64                  // 0.5*(x2-x0)  v            c
    fxch st(2)                    // c            v            0.5*(x2-x0)
    fst st(3)                     // c            v            0.5*(x2-x0)  c
    fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
    fxch st(2)                    // 0.5*(x2-x0)  v            w            c
    faddp st(1), st(0)            // v+.5(x2-x0)  w            c
    fadd st(0), st(1)             // a            w            c
    fadd st(1), st(0)             // a            b_neg        c
    fmul Fractional.Double        // a*frac       b_neg        c
    fsubrp st(1), st(0)           // a*f-b        c
    fmul Fractional.Double        // (a*f-b)*f    c
    faddp st(1), st(0)            // res-x0/f
    fmul Fractional.Double        // res-x0
    fadd [Pntr + 8].Double        // res
end;
{$ENDIF}

function Hermite32I_asm(const Fractional: Single; Pntr: PSingle): Single;
asm
    fld   [Pntr + 16].Single      // x1
    fsub  [Pntr     ].Single      // x1-xm1
    fld   [Pntr +  8].Single      // x0           x1-xm1
    fsub  [Pntr + 16].Single      // v            x1-xm1
    fld   [Pntr + 24].Single      // x2           v            x1-xm1
    fsub  [Pntr +  8].Single      // x2-x0        v            x1-xm1
    fxch st(2)                    // x1-m1        v            x2-x0
    fmul CHalf32                  // c            v            x2-x0
    fxch st(2)                    // x2-x0        v            c
    fmul CHalf32                  // 0.5*(x2-x0)  v            c
    fxch st(2)                    // c            v            0.5*(x2-x0)
    fst st(3)                     // c            v            0.5*(x2-x0)  c
    fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
    fxch st(2)                    // 0.5*(x2-x0)  v            w            c
    faddp st(1), st(0)            // v+.5(x2-x0)  w            c
    fadd st(0), st(1)             // a            w            c
    fadd st(1), st(0)             // a            b_neg        c
    fmul Fractional.Single        // a*frac       b_neg        c
    fsubrp st(1), st(0)           // a*f-b        c
    fmul Fractional.Single        // (a*f-b)*f    c
    faddp st(1), st(0)            // res-x0/f
    fmul Fractional.Single        // res-x0
    fadd [Pntr + 8].Single        // res
end;

function Hermite64I_asm(const Fractional: Double; Pntr: PDouble): Double;
asm
    fld   [Pntr + 32].Double      // x1
    fsub  [Pntr     ].Double      // x1-xm1
    fld   [Pntr + 16].Double      // x0           x1-xm1
    fsub  [Pntr + 32].Double      // v            x1-xm1
    fld   [Pntr + 48].Double      // x2           v            x1-xm1
    fsub  [Pntr + 16].Double      // x2-x0        v            x1-xm1
    fxch st(2)                    // x1-m1        v            x2-x0
    fmul CHalf64                  // c            v            x2-x0
    fxch st(2)                    // x2-x0        v            c
    fmul CHalf64                  // 0.5*(x2-x0)  v            c
    fxch st(2)                    // c            v            0.5*(x2-x0)
    fst st(3)                     // c            v            0.5*(x2-x0)  c
    fadd st(0), st(1)             // w            v            0.5*(x2-x0)  c
    fxch st(2)                    // 0.5*(x2-x0)  v            w            c
    faddp st(1), st(0)            // v+.5(x2-x0)  w            c
    fadd st(0), st(1)             // a            w            c
    fadd st(1), st(0)             // a            b_neg        c
    fmul Fractional.Double        // a*frac       b_neg        c
    fsubrp st(1), st(0)           // a*f-b        c
    fmul Fractional.Double        // (a*f-b)*f    c
    faddp st(1), st(0)            // res-x0/f
    fmul Fractional.Double        // res-x0
    fadd [Pntr + 16].Double       // res
end;

function Hermite1(const Fractional: Single; const y: TDAV4SingleArray): Single;
var
  c0, c1, c2, c3: Single;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c0 := y[1];
  c1 := CHalf32 * (y[2] - y[0]);
  c2 := y[0] - 2.5 * y[1] + 2 * y[2] - CHalf32 * y[3];
  c3 := 1.5 * (y[1] - y[2]) + CHalf32 * (y[3] - y[0]);
  Result := ((c3 * Fractional + c2) * Fractional + c1) * Fractional + c0;
end;

function Hermite2(const Fractional: Single; const y: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[0] := y[1];
  c[1] := CHalf32 * (y[2] - y[0]);
  c[3] := 1.5 * (y[1] - y[2]) + CHalf32 * (y[3] - y[0]);
  c[2] := y[0] - y[1] + c[1] - c[3];
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + c[0];
end;

function Hermite3(const Fractional: Single; const y: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
begin
  // 4-point, 3rd-order Hermite (x-form)
  c[1]   := CHalf32 * (y[2] - y[0]);
  c[0]   := y[0] - y[1];
  c[3]   := (y[1] - y[2]) + CHalf32 * (y[3] - c[0] - y[2]);
  c[2]   := c[0] + c[1] - c[3];
  Result := ((c[3] * Fractional + c[2]) * Fractional + c[1]) * Fractional + y[1];
end;

function Hermite4(const Fractional: Single; const y: TDAV4SingleArray): Single;
var
  c : TDAV4SingleArray;
  b : Single;
begin
  c[0] := (y[2] - y[0]) * CHalf32;
  c[1] := y[1] - y[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] + (y[3] - y[1]) * CHalf32;
  b    := c[2] + c[3];
  Result := ((((c[3] * Fractional) - b) * Fractional + c[0]) * Fractional + y[1]);
end;

function LinearInterpolation(f, a, b: Single): Single;
begin
  Result := (1 - f) * a + f * b;
end;

function LinearInterpolation(f, a, b: Double): Double;
begin
  Result := (1 - f) * a + f * b;
end;

function CubicInterpolation(fr, inm1, inp, inp1, inp2: Single): Single;
begin
  Result := inp + 0.5 * fr * (inp1 - inm1 + fr *
    (4 * inp1 + 2 * inm1 - 5 * inp - inp2 + fr *
    (3 * (inp - inp1) - inm1 + inp2)));
end;

end.
