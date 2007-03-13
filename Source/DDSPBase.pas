unit DDSPBase;

interface

{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

{$DEFINE x87}

{$IFNDEF FPC} uses Windows, Types;
{$ELSE} uses Types;
{$ENDIF}

type
  TDoubleDynArray = Types.TDoubleDynArray;
  TSingleDynArray = Types.TSingleDynArray;
  TSingleFixedArray = Array [0..0] of Single;
  PSingleFixedArray = ^TSingleFixedArray;
  TDoubleFixedArray = Array [0..0] of Double;
  PDoubleFixedArray = ^TDoubleFixedArray;

  TArrayOfSingleDynArray = array of TSingleDynArray;
  PArrayOfSingleDynArray = ^TArrayOfSingleDynArray;
  TArrayOfDoubleDynArray = array of TDoubleDynArray;
  PArrayOfDoubleDynArray = ^TArrayOfDoubleDynArray;

  T4SingleArray = array[0..3] of Single;
  P4SingleArray = ^T4SingleArray;
  T4DoubleArray = array[0..3] of Double;
  P4DoubleArray = ^T4DoubleArray;

  T2SingleArray = array [0..1] of Single;
  P2SingleArray = ^T2SingleArray;
  T2DoubleArray = array [0..1] of Double;
  P2DoubleArray = ^T2SingleArray;

  TStrArray = array of string;

  {$IFNDEF FPC}
  function GetApplicationFilename: string;
  function GetApplicationDirectory: string;
  {$ENDIF}
  function ms2smp(ms, SampleRate: Single): Single;
  function smp2ms(smp, SampleRate: Single): Single;
  function getSyncFactor(base_factor: Single; dotted, triads: boolean): Single;
  function Sync2Smp(SyncFactor, bpm, SampleRate: Single): Integer;
  function f_Limit(v:Single;l:Single=-1;u:Single=1):Single; overload;
  function f_Limit(v:Double;l:Double=-1;u:Double=1):Double; overload;
  function f_Clip(x,l,h:Single):Single;
  function f_Cliplo(x,l:Single):Single;
  function f_Cliphi(x,h:Single):Single;
  function dB_to_Amp(g:Single):Single;
  function Amp_to_dB(v:Single):Single; overload;
  {$IFNDEF FPC}
  function Amp_to_dB(v:T4SingleArray):Single; overload;
  {$ENDIF}
  function Smallest(A, B: Single): Single;
  function Largest(A, B: Single): Single;
  function LimitAngle(const Angle: Single): Single;
  function LinearInterpolation(f,a,b:Single):Single;
  function CubicInterpolation(fr,inm1,inp,inp1,inp2:Single):Single;
  function f_Ln2(f:Single):Single;
  function f_Floorln2(f:Single):Integer;
  function f_Arctan(fValue:Single):Single;
  {$IFNDEF FPC}
  function f_Frac(Sample:Single):Single;
  function f_Int(Sample:Single):Single;
  function f_Trunc(Sample:Single):Integer; overload;
  procedure f_Trunc(Input:PSingle; Output:PInteger; SampleFrames: Integer); overload;
  function f_Round(Sample:Single):Integer;
  function f_Exp(x:Single):Single;
  function f_Abs(f:Single):Single; overload;
  function f_Abs(f:Double):Double; overload;
  function f_Abs(f:T4SingleArray):Single; overload;
  function f_Neg(f:Single):Single;
  function f_Root(i:Single;n:Integer):Single;
  function f_Power(i:Single;n:Integer):Single;
  function f_Log2(val:Single):Single;
  function f_Sin(fAngle:Single):Single;
  function f_Cos(fAngle:Single):Single;
  function f_Sgn(f:Single):Integer;
  function f_Min(const A, B: Single) : Single;
  function f_Max(const A, B: Single) : Single;
  function f_ArcTan2(const Y, X: Extended): Extended;
  function f_Tan(const X: Extended): Extended;
  function f_CoTan(const X: Extended): Extended;
  function f_Log10(const X: Extended): Extended;

  {$ENDIF}
  // scale logarithmically from 20 Hz to 20 kHz
  function FreqLinearToLog(value:Single):Single;
  function FreqLogToLinear(value:Single):Single;

  procedure GetSinCos(Frequency: Double; var SinValue, CosValue : Double);

  function OnOff(fvalue:Single):boolean;
  function unDenormalize(fvalue:Single):Single;

  function Saturate(input, fMax: Single): Single;

  {$IFNDEF FPC}
  procedure Msg(b:boolean); overload;
  procedure Msg(m:string;m2:string=''); overload;
  procedure Msg(i:Integer); overload;
  procedure Msg(s:Single); overload;
  procedure Msg(m:string;i:Integer); overload;

  function FloatWithUnit(f:Double):string;
  function SplitString(S: String; Delimiter: char): TStrArray;
  function MakeGoodFileName(s: string): string;

  function Hermite1(const x,y0,y1,y2,y3:Single):Single;
  function Hermite2(const x,y0,y1,y2,y3:Single):Single;
  function Hermite3(const x,y0,y1,y2,y3:Single):Single;
  function Hermite4(const frac_pos, xm1, x0, x1, x2: Single): Single;
  function Hermite_asm(const frac_pos: Single; pntr : PSingle) : Single;

  function Tanh2a(x:Single):Single;
  function Tanh2b(x:Single):Single;
  function Tanh2c(x:Single):Single;
  function Tanh2d(x:Single):Single;
  function Sigmoid(x:Single):Single;
  function Waveshaper1(x,t:Single):Single;
  function Waveshaper2(x,t:Single):Single;
  function Waveshaper3(x,a:Single):Single;
  function Waveshaper4(x,a:Single):Single;
  function Waveshaper5(x,a:Single):Single;
  function Waveshaper6(x:Single):Single;
  function Waveshaper7(x,a:Single):Single;
  function Waveshaper8(x,a:Single):Single;
  function SoftSat(x,a:Single):Single;
{$ENDIF}

  function FindMaximum(InBuffer: PSingle; Samples: Integer): Integer; overload;
  function FindMaximum(InBuffer: PDouble; Samples: Integer): Integer; overload;
  procedure DCSubstract(InBuffer: PSingle; Samples: Integer); overload;
  procedure DCSubstract(InBuffer: PDouble; Samples: Integer); overload;

var ln10, ln2, ln22, ln2Rez : Double;

implementation

{$IFNDEF FPC}
uses Math, SysUtils;
{$ENDIF}

const Half   : Double = 0.5;
      LN2R   : Double = 1.442695041;
      Twenty : Double = 20;

{ Math }

// Limit a value to be l<=v<=u
function f_Limit(v:Single;l:Single=-1;u:Single=1):Single; overload;
begin
 if v<l then Result:=l
 else if v>u then Result:=u else Result:=v;
end;

// Limit a value to be l<=v<=u
function f_Limit(v:Double;l:Double=-1;u:Double=1):Double; overload;
begin
 if v<l then Result:=l
 else if v>u then Result:=u else Result:=v;
end;

// Convert a value in dB's to a linear amplitude
function dB_to_Amp(g:Single):Single;
begin
 if (g>-144.0)
  then Result:=exp(g*0.115129254)
  else Result:=0;
end;

{$IFNDEF FPC}
{$WARNINGS OFF}
function f_ArcTan2(const Y, X: Extended): Extended;
asm
 fld Y
 fld X
 fpatan
end;

function f_Tan(const X: Extended): Extended;
asm
 fld X
 fptan
 fstp st(0)
end;

function f_CoTan(const X: Extended): Extended;
asm
 fld X
 fptan
 fdivrp
end;

function f_Log10(const X: Extended): Extended;
asm
 fldlg2
 fld x
 fyl2x
end;
{$ENDIF}

function Smallest(A, B: Single): Single;
begin
 if A < B
  then Result := A
  else Result := B;
end;

function Largest(A, B: Single): Single;
begin
 if A > B
  then Result := A
  else Result := B;
end;

function LimitAngle(const Angle: Single): Single;
begin
 Result := Angle;
 while Result < 0 do Result:=Result+360;
 while Result > 359 do Result:=Result-360;
end;

function LinearInterpolation(f,a,b:Single):Single;
begin
 Result:=(1-f)*a+f*b;
end;

function CubicInterpolation(fr,inm1,inp,inp1,inp2:Single):Single;
begin
 Result:=inp+0.5*fr*(inp1-inm1+fr*(4*inp1+2*inm1-5*inp-inp2 +fr*(3*(inp-inp1)-inm1+inp2)));
end;

function Amp_to_dB(v:Single):Single;
asm
 fldlg2
 fld v
 fyl2x
 fmul Twenty.Double
end;

{$IFNDEF FPC}
function Amp_to_dB(v:T4SingleArray):Single;
asm
 fldlg2
 fld [eax].Single
 fyl2x
 fmul Twenty.Double
 fstp [eax].Single
 fldlg2
 fld [eax+4].Single
 fyl2x
 fmul Twenty.Double
 fstp [eax+4].Single
 fldlg2
 fld [eax+8].Single
 fyl2x
 fmul Twenty.Double
 fstp [eax+8].Single
 fldlg2
 fld [eax+12].Single
 fyl2x
 fmul Twenty.Double
 fstp [eax+12].Single
end;

function f_Trunc(Sample:Single):Integer;
asm
 fld Sample.Single
 fsub half
 fistp Result.Integer
end;

procedure f_Trunc(Input:PSingle; Output:PInteger; SampleFrames: Integer);
asm
 @Start:
 fld [eax].Single
 fsub half
 fistp [edx].Integer
 add eax,4
 add edx,4
 loop    @Start
end;

function f_Frac(Sample:Single):Single;
const magic:Single=2E21;
var i : Integer;
asm
 fld Sample.Single
 fld Sample.Single
 fsub half
// fistp i fild i //instead of frndint->Faster
// fadd magic.Single fsub magic.Single //instead of frndint->HyperFast
 frndint
 fsubp
end;

function f_Int(Sample:Single):Single;
asm
 fld Sample.Single
 fsub half
 frndint
end;

function f_Round(Sample:Single):Integer;
asm
 fld Sample.Single
 frndint
 fistp Result.Integer
end;

function f_Exp(x:Single):Single;
begin
 Result:=Exp(x*LN2R*ln(2));
end;

function f_Sin(fAngle:Single):Single;
const sin1:Double=7.61e-03;
      sin2:Double=-1.6605e-01;
      sin3:Double=1;
asm
 fld fAngle.Single
 fmul fAngle.Single
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fadd sin3.Double
 fmul fAngle
end;

function f_Cos(fAngle:Single):Single;
const sin1:Double=3.705e-02;
      sin2:Double=-4.967e-01;
      sin3:Double=1;
asm
 fld fAngle.Single
 fmul fAngle.Single
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fadd sin3.Double
 fmul fAngle
end;

{$ENDIF}

function f_ArcTan(fValue:Single):Single;
var fVSqr,fResult:Single;
begin
 fVSqr:=fValue*fValue;
 fResult:=0.0208351;
 fResult:=fResult*fVSqr;
 fResult:=fResult-0.085133;
 fResult:=fResult*fVSqr;
 fResult:=fResult+0.180141;
 fResult:=fResult*fVSqr;
 fResult:=fResult-0.3302995;
 fResult:=fResult*fVSqr;
 fResult:=fResult+0.999866;
 fResult:=fResult*fValue;
 Result:=fResult;
end;

function f_Ln2(f:Single):Single;
begin
 Result:=(((Integer((@f)^) and $7f800000) shr 23)-$7f)+(Integer((@f)^) and $007fffff)/$800000;
end;

function f_FloorLn2(f:Single):Integer;
begin
 Result:=(((Integer((@f)^) and $7f800000) shr 23)-$7f);
end;

function f_Abs(f:Single):Single; overload;
{$IFNDEF FPC}
asm
 fld f.Single
 fabs
end;
{$ELSE}
var i:Integer;
begin
 i:=Integer((@f)^) and $7FFFFFFF;
 Result:=Single((@i)^);
end;
{$ENDIF}

{$IFNDEF FPC}
function f_Abs(f:Double):Double; overload;
asm
 fld f.Double
 fabs
end;

function f_Abs(f:T4SingleArray):Single; overload;
asm
 fld [eax].Single
 fabs
 fstp [eax].Single
 fld [eax+4].Single
 fabs
 fstp [eax+4].Single
 fld [eax+8].Single
 fabs
 fstp [eax+8].Single
 fld [eax+12].Single
 fabs
 fstp [eax+12].Single
end;
{$ENDIF}

function f_Neg(f:Single):Single;
var i,j:Integer;
begin
 j:=$80000000;
 i:=Integer((@f)^) xor j;
 Result:=Single((@i)^);
end;

{$IFNDEF FPC}

function f_Sgn(f:Single):Integer;
begin
 Result:=1-((Integer((@f)^) shr 31)shl 1);
end;

{$ENDIF}

function f_Log2(val:Single):Single;
var log2,x:Integer;
begin
 x:=Integer((@val)^);
 log2:=((x shr 23) and 255)-128;
 x:=x and (not(255 shl 23));
 x:=x+127 shl 23;
 Result:=Single((@x)^)+log2;
end;

function f_Power(i:Single;n:Integer):Single;
var l:Integer;
begin
 l:=Integer((@i)^);
 l:=l-$3F800000;l:=l shl (n-1);l:=l+$3F800000;
 Result:=Single((@l)^);
end;

function f_Root(i:Single;n:Integer):Single;
var l:Integer;
begin
 l:=Integer((@i)^);
 l:=l-$3F800000;l:=l shr (n-1);l:=l+$3F800000;
 Result:=Single((@l)^);
end;

function f_Cliplo(x,l:Single):Single;
begin
 x:=x-l;
 x:=x+f_abs(x);
 x:=x*0.5;
 x:=x+l;
 Result:=x;
end;

function f_Cliphi(x,h:Single):Single;
begin
 x:=h-x;
 x:=x+f_abs(x);
 x:=x*0.5;
 x:=h-x;
 Result:=x;
end;

function f_Clip(x,l,h:Single):Single;
var x1,x2:Single;
begin
 x1:=f_abs(x-l);
 x2:=f_abs(x-h);
 x:=x1+(l+h);
 x:=x-x2;
 x:=x*0.5;
 Result:=x;
end;

function Saturate(input, fMax: Single): Single;
var x1, x2: Single;
begin
 x1 := f_abs(input + fMax);
 x2 := f_abs(input - fMax);
 Result := Half * (x1 - x2);
end;

// scale logarithmicly from 20 Hz to 20 kHz
function FreqLinearToLog(value:Single):Single;
const fltl1:Double=20;
      fltl2:Double=6.907755279;
asm
 FLD Value.Single
 FMUL fltl2
 FLDL2E              { y := x*log2e;      }
 FMUL
 FLD     ST(0)       { i := round(y);     }
 FRNDINT
 FSUB    ST(1), ST   { f := y - i;        }
 FXCH    ST(1)       { z := 2**f          }
 F2XM1
 FLD1
 FADD
 FSCALE              { result := z * 2**i }
 FSTP    ST(1)
 FMUL fltl1
// Result:=(20.0*Exp(value*flt1));
end;

function FreqLogToLinear(Value:Single):Single;
const fltl1:Double=0.05;
      fltl2:Double=1.44764826019E-1;
asm
 fldln2
 fld value.Single
 fmul fltl1
 fyl2x
 fmul fltl2
// Result:=(ln(value*0.05)*ln2Rez))*0.10034333188799371439986123402106;
end;

procedure GetSinCos(Frequency: Double; var SinValue, CosValue : Double);
asm
 fld Frequency.Double;
 fsincos
 fstp [CosValue].Double;
 fstp [SinValue].Double;
end;

function OnOff(fvalue:Single):boolean;
begin Result:=fvalue>0.5 end;

function UnDenormalize(fvalue:Single):Single;
begin
 if (f_abs(fvalue)<1.0e-15) then fvalue := 0.0;
 Result:=fvalue;
end;

{ String Functions }

{$IFNDEF FPC}
procedure Msg(b:boolean);
begin if b then Msg('TRUE') else Msg('FALSE');end;
procedure Msg(m:string;m2:string='');
begin MessageBox(0,PChar(m),PChar(m2),mb_ok); end;
procedure Msg(i:Integer);
begin MessageBox(0,PChar(inttostr(i)),'',mb_ok); end;
procedure Msg(s:Single);
begin MessageBox(0,PChar(floattostrf(s,fffixed,3,3)),'',mb_ok); end;
procedure Msg(m:string;i:Integer);
begin MessageBox(0,PChar(m+' '+inttostr(i)),'',mb_ok); end;
{$WARNINGS ON}

function GetApplicationFilename:string;
var s  : array[0..1500] of char;
begin
 GetModuleFilename(hinstance,s,sizeof(s));
 Result:=strpas(s);
 Result:=ExtractFilename(Result);
end;

function GetApplicationDirectory:string;
var s  : array[0..1500] of char;
begin
 GetModuleFilename(hinstance,s,sizeof(s));
 Result:=strpas(s);
 Result:=ExtractFilename(Result);
end;

function FloatWithUnit(f:Double):string;
begin
 if f>1 then result:=FloatToStrF(f,ffFixed,3,3)+ 's' else
 if f>0.001 then result:=FloatToStrF(1000*f,ffFixed,3,3)+ 'ms' else
 if f>0.000001
  then result:=FloatToStrF(1000000*f,ffFixed,3,3)+ 'µs'
  else result:=FloatToStrF(1000000000*f,ffFixed,3,3)+ 'ns'
end;

function SplitString(S: String; Delimiter: char): TStrArray;
var C: Integer;
begin
 repeat
  SetLength(Result, Length(Result)+ 1);
  C := Pos(Delimiter, S);
  if C = 0 then C := Length(S) + 1;
  Result[Length(Result)- 1] := Copy(S, 1, C- 1);
  Delete(S, 1, C);
 until length(S)= 0;
end;

function MakeGoodFileName(s: string): string;
var i: Integer;
begin
 Result := '';
 for i := 1 to length(s) do
  if not (s[i] in ['*', '\', '/', '[', ']', '"', '|', '<', '>', '?', ':'])
   then Result := Result + s[i]
   else Result := Result + '-';
end;

{$ENDIF}

function Sync2Smp(SyncFactor, bpm, SampleRate: Single): Integer;
begin
 {$IFNDEF FPC}
 Result := f_Round(SyncFactor * SampleRate * 60 / bpm);
 {$ELSE}
 Result := Round(SyncFactor * SampleRate * 60 / bpm);
 {$ENDIF}
end;

function ms2smp(ms, SampleRate: Single): Single;
begin
 Result := ms * SampleRate * 0.001;
end;

function smp2ms(smp, SampleRate: Single): Single;
begin
 Result := smp * 1000 / SampleRate;
end;

function getSyncFactor(base_factor: Single; dotted, triads: boolean): Single;
begin
 Result := base_factor;
 if dotted then Result := Result * 1.5;
 if triads then Result := Result / 3;
end;

function Hermite_asm(const frac_pos: Single; pntr : PSingle) : Single;
// Parameter explanation:
// frac_pos: fractional value [0.0f - 1.0f] to interpolator
// pntr: pointer to float array where:
// pntr[0] = previous sample (idx = -1)
// pntr[1] = current sample (idx = 0)
// pntr[2] = next sample (idx = +1)
// pntr[3] = after next sample (idx = +2)
// The interpolation takes place between pntr[1] and pntr[2].
const c_half : Double = 0.5;
asm
 fld dword ptr [pntr+8];       // x1
 fsub dword ptr [pntr];        // x1-xm1
 fld dword ptr [pntr+4];       // x0           x1-xm1
 fsub dword ptr [pntr+8];      // v            x1-xm1
 fld dword ptr [pntr+12];      // x2           v            x1-xm1
 fsub dword ptr [pntr+4];      // x2-x0        v            x1-xm1
 fxch st(2);                   // x1-m1        v            x2-x0
 fmul c_half;                  // c            v            x2-x0
 fxch st(2);                   // x2-x0        v            c
 fmul c_half;                  // 0.5*(x2-x0)  v            c
 fxch st(2);                   // c            v            0.5*(x2-x0)
 fst st(3);                    // c            v            0.5*(x2-x0)  c
 fadd st(0), st(1);            // w            v            0.5*(x2-x0)  c
 fxch st(2);                   // 0.5*(x2-x0)  v            w            c
 faddp st(1), st(0);           // v+.5(x2-x0)  w            c
 fadd st(0), st(1);            // a            w            c
 fadd st(1), st(0);            // a            b_neg        c
 fmul frac_pos.Single;         // a*frac       b_neg        c
 fsubrp st(1), st(0);          // a*f-b        c
 fmul frac_pos.Single;         // (a*f-b)*f    c
 faddp st(1), st(0);           // res-x0/f
 fmul frac_pos.Single;         // res-x0
 fadd dword ptr [pntr+4]       // res
end;

function Hermite1(const x,y0,y1,y2,y3:Single):Single;
var c0,c1,c2,c3: Single;
begin
 // 4-point, 3rd-order Hermite (x-form)
 c0:=y1;
 c1:=0.5*(y2-y0);
 c2:=y0-2.5*y1+2*y2-0.5*y3;
 c3:=1.5*(y1-y2)+0.5*(y3-y0);
 Result:=((c3*x+c2)*x+c1)*x+c0;
end;

function Hermite2(const x,y0,y1,y2,y3:Single):Single;
var c0,c1,c2,c3: Single;
begin
 // 4-point, 3rd-order Hermite (x-form)
 c0:=y1;
 c1:=0.5*(y2-y0);
 c3:=1.5*(y1-y2)+0.5*(y3-y0);
 c2:=y0-y1+c1-c3;
 Result:=((c3*x+c2)*x+c1)*x+c0;
end;

function Hermite3(const x,y0,y1,y2,y3:Single):Single;
var c0,c1,c2,c3, y0my1 : Single;
begin
 // 4-point, 3rd-order Hermite (x-form)
 c0:=y1;
 c1:=0.5*(y2-y0);
 y0my1:=y0-y1;
 c3:=(y1-y2)+0.5*(y3-y0my1-y2);
 c2:=y0my1+c1-c3;
 Result:=((c3*x+c2)*x+c1)*x+c0;
end;

function Hermite4(const frac_pos, xm1, x0, x1, x2: Single): Single;
var c,v,w,a : Single;
    b_neg   : Single;
begin
 c :=(x1-xm1)*0.5;
 v := x0-x1;
 w := c+v;
 a := w+v+(x2-x0)*0.5;
 b_neg := w + a;
 Result:=((((a * frac_pos) - b_neg) * frac_pos + c) * frac_pos + x0);
end;

function Tanh2a(x:Single):Single;
var a,b:Single;
begin
 a:=f_abs(x);
 b:=12+a*(6+a*(3+a));
 Result:=(x*b)/(a*b+24);
end;

function Tanh2b(x:Single):Single;
var a,b:Single;
begin
 a:=f_abs(x);
 b:=(6+a*(3+a));
 Result:=(x*b)/(a*b+12);
end;

{$IFNDEF FPC}

function Tanh2c(x:Single):Single;
const c3:Single=3;
      c6:Single=6;
asm
 fld x.Single;
 fabs
 fld c3
 fadd st(0),st(1)
 fld st(0)
 fmul x.single
 fxch st(2)
 fmulp
 fadd c6.Single
 fdiv
end;
{ var a,b:Single; begin a:=f_abs(x); b:=3+a; Result:=(x*b)/(a*b+6); end; }

function Tanh2d(x:Single):Single;
const c3:Single=3;
asm
 fld x.Single;
 fld x.Single;
 fabs
 fadd c3
 fdiv
end;
{ begin Result:=x/(f_abs(x)+3); end; }

function Sigmoid(x:Single):Single;
begin
 if(f_abs(x)<1)
  then Result:=x*(1.5 - 0.5*x*x)
  else
   if x < 0
    then Result:=-1
    else Result:= 1;
end;

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

function Waveshaper3(x,a:Single):Single;
begin
 Result:=x*(abs(x)+a)/(x*x+(a-1)*abs(x)+1);
end;

function sign(x:Single):Single;
begin
 result:=0;
 if x<0 then result:=-1 else
 if x>0 then result:=1;
end;

function Waveshaper4(x,a:Single):Single;
begin
 Result:=sign(x)*power(arctan(power(abs(x), a)), (1/a));
end;

function Waveshaper5(x,a:Single):Single;
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

function Waveshaper7(x,a:Single):Single;
begin
 Result:=sign(x)*exp(ln(abs(x))*a);
end;

function Waveshaper8(x,a:Single):Single;
begin
 Result:=sign(x)*exp(ln(a)*abs(x));
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

function f_Min(const A, B: Single) : Single;
asm
 fld     dword ptr [ebp+$08]
 fld     dword ptr [ebp+$0c]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
end;

function f_Max(const A, B: Single) : Single;
asm
 fld     dword ptr [ebp+$0c]
 fld     dword ptr [ebp+$08]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
end;
{$ENDIF}

function FindMaximum(InBuffer: PSingle; Samples: Integer): Integer; overload; platform;
{$IFDEF x87}
asm
 test edx,edx
 jz @End

 mov result,edx                // Result := edx
 dec edx
 jnz @End                      // only one sample -> exit!
 fld  [eax+4*edx].Single       // Value
 fabs                          // |Value| = Max

 @FindMaxLoop:
   fld  [eax+4*edx-4].Single   // Value, Max
   fabs                        // |Value|, Max

   fcomi st(0), st(1)          // |Value| <-> Max ?
   fstsw ax                    // ax = FPU Status Word
   sahf                        // ax -> EFLAGS register
   jae @NextSample             // if |Value| <-> Max then next sample!
   fxch                        // OldMax, |Value|
   mov result,edx              // Result := edx

   @NextSample:
   fstp st(0)                  // Value, Max
   dec edx
 jnz @FindMaxLoop

 mov edx,result              // edx := Result
 sub edx,1                   // edx := edx - 1  -> index starts at 0!
 mov result,edx              // Result := edx

 @End:
end;
{$ELSE}
var i : Integer;
    d : Double;
begin
 result:=0; d:=f_abs(InBuffer^);
 for i:=1 to Samples-1 do
  begin
   if f_abs(InBuffer^)>d then
    begin
     Result:=i;
     d:=f_abs(InBuffer^);
    end;
   inc(InBuffer);
  end;
end;
{$ENDIF}

function FindMaximum(InBuffer: PDouble; Samples: Integer): Integer; overload; platform;
{$IFDEF x87}
asm
 test edx,edx
 jz @End

 mov result,edx                // Result := edx
 dec edx
 jnz @End                      // only one sample -> exit!
 fld  [eax+8*edx].Double       // Value
 fabs                          // |Value| = Max

 @FindMaxLoop:
   fld  [eax+8*edx-8].Double   // Value, Max
   fabs                        // |Value|, Max

   fcomi st(0), st(1)          // |Value| <-> Max ?
   fstsw ax                    // ax = FPU Status Word
   sahf                        // ax -> EFLAGS register
   jae @NextSample             // if |Value| <-> Max then next sample!
   fxch                        // OldMax, |Value|
   mov result,edx              // Result := edx

   @NextSample:
   fstp st(0)                  // Value, Max
   dec edx
 jnz @FindMaxLoop

 mov edx,result              // edx := Result
 sub edx,1                   // edx := edx - 1  -> index starts at 0!
 mov result,edx              // Result := edx

 @End:
end;
{$ELSE}
var i : Integer;
    d : Double;
begin
 result:=0; d:=f_abs(InBuffer^);
 for i:=1 to Samples-1 do
  begin
   if f_abs(InBuffer^)>d then
    begin
     Result:=i;
     d:=f_abs(InBuffer^);
    end;
   inc(InBuffer);
  end;
end;
{$ENDIF}

procedure DCSubstract(InBuffer: PSingle; Samples: Integer); overload; platform;
{$IFDEF x87}
asm
 test edx,edx
 jz @End

 push edx
 fldz                          // DC
 @CalcDCLoop:
   dec edx
   fadd  [eax+4*edx].Single    // DC = DC + Value
 jnz @CalcDCLoop
 pop edx

 mov [esp-4],edx
 fild [esp-4].Integer          // Length, DC
 fdivp                         // RealDC = DC / Length

 @SubstractDCLoop:
   dec edx
   fld  [eax+4*edx].Single     // Value, RealDC
   fsub st(0),st(1)            // Value-RealDC, RealDC
   fstp  [eax+4*edx].Single    // RealDC
 jnz @SubstractDCLoop
 fstp st(0)                    // clear stack

 @End:
end;
{$ELSE}
var InBuf : array [0..0] of Double absolute InBuffer;
    d : Double;
    i : Integer;
begin
 if Samples=0 then Exit;
 d:=InBuf[0];
 for i:=1 to Samples-1
  do d:=d+InBuf[i];
 d:=d/Samples;
 for i:=0 to Samples-1
  do InBuf[i]:=InBuf[i]-d;
end;
{$ENDIF}

procedure DCSubstract(InBuffer: PDouble; Samples: Integer); overload; platform;
{$IFDEF x87}
asm
 test edx,edx
 jz @End

 push edx
 fldz                          // DC
 @CalcDCLoop:
   dec edx
   fadd  [eax+8*edx].Double    // DC = DC + Value
 jnz @CalcDCLoop
 pop edx

 mov [esp-4],edx
 fild [esp-4].Integer          // Length, DC
 fdivp                         // RealDC = DC / Length

 @SubstractDCLoop:
   dec edx
   fld  [eax+8*edx].Double     // Value, RealDC
   fsub st(0),st(1)            // Value-RealDC, RealDC
   fstp  [eax+8*edx].Double    // RealDC
 jnz @SubstractDCLoop
 fstp st(0)                    // clear stack

 @End:
end;
{$ELSE}
var InBuf : array [0..0] of Double absolute InBuffer;
    d : Double;
    i : Integer;
begin
 if Samples=0 then Exit;
 d:=InBuf[0];
 for i:=1 to Samples-1
  do d:=d+InBuf[i];
 d:=d/Samples;
 for i:=0 to Samples-1
  do InBuf[i]:=InBuf[i]-d;
end;
{$ENDIF}

procedure InitConstants;
begin
 ln2    := ln(2);
 ln22   := ln2*0.5;
 ln2Rez := 1/ln2;
 ln10   := ln(10);
end;

initialization
 InitConstants;

end.
