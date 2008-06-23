unit DAVDCommon;

interface

{$I ASIOVST.inc}

{$IFDEF DELPHI5_UP}
uses Windows {$IFDEF UseNativeTypes}, Types{$ENDIF};
{$ELSE}
uses LCLIntf; {$DEFINE PUREPASCAL}
{$ENDIF}

type
  {$IFNDEF DELPHI7_UP}
    TAVDSingleDynArray = Array of Single;
    TAVDDoubleDynArray = Array of Double;
  {$ELSE}
    {$IFDEF UseNativeTypes}
      TAVDSingleDynArray = Types.TSingleDynArray;
      TAVDDoubleDynArray = Types.TDoubleDynArray;
    {$ELSE}
      TAVDSingleDynArray = Array of Single;
      TAVDDoubleDynArray = Array of Double;
    {$ENDIF}
  {$ENDIF}

  PAVDSingleDynArray = ^TAVDSingleDynArray;
  PAVDDoubleDynArray = ^TAVDDoubleDynArray;

  TAVDSingleFixedArray = Array [0..0] of Single;
  PAVDSingleFixedArray = ^TAVDSingleFixedArray;
  TAVDDoubleFixedArray = Array [0..0] of Double;
  PAVDDoubleFixedArray = ^TAVDDoubleFixedArray;

  TAVDArrayOfSingleDynArray = array of TAVDSingleDynArray;
  PAVDArrayOfSingleDynArray = ^TAVDArrayOfSingleDynArray;
  TAVDArrayOfDoubleDynArray = array of TAVDDoubleDynArray;
  PAVDArrayOfDoubleDynArray = ^TAVDArrayOfDoubleDynArray;

  TAVDArrayOfSingleFixedArray = array [0..0] of TAVDSingleFixedArray;
  PAVDArrayOfSingleFixedArray = ^TAVDArrayOfSingleFixedArray;
  TAVDArrayOfDoubleFixedArray = array [0..0] of TAVDDoubleFixedArray;
  PAVDArrayOfDoubleFixedArray = ^TAVDArrayOfDoubleFixedArray;

  TAVDSingleDynMatrix = TAVDArrayOfSingleDynArray;
  PAVDSingleDynMatrix = ^TAVDSingleDynMatrix;
  TAVDDoubleDynMatrix = TAVDArrayOfDoubleDynArray;
  PAVDDoubleDynMatrix = ^TAVDDoubleDynMatrix;

  T4SingleArray = array[0..3] of Single;
  P4SingleArray = ^T4SingleArray;
  T4DoubleArray = array[0..3] of Double;
  P4DoubleArray = ^T4DoubleArray;

  T2SingleArray = array [0..1] of Single;
  P2SingleArray = ^T2SingleArray;
  T2DoubleArray = array [0..1] of Double;
  P2DoubleArray = ^T2SingleArray;

  TAVDMinMaxSingle = record
    min : Single;
    max : Single;
  end;
  TAVDMinMaxDouble = record
    min : Double;
    max : Double;
  end;

  TStrArray = array of string;

  TAVDMidiEvent = record
    MidiData        : array[0..3] of Byte;  // 1 thru 3 midi Bytes; midiData[3] is reserved (zero)
    DeltaFrames     : LongInt;              // sample frames related to the current block start sample position
    NoteOffset      : LongInt;              // offset into note from note start if available, else 0
    NoteLength      : LongInt;              // (in sample frames) of entire note, if available, else 0
    Detune          : Byte;                 // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
    NoteOffVelocity : Byte;
  end;

  {$IFNDEF FPC}
  function GetApplicationFilename: string; {$IFDEF useinlining} inline; {$ENDIF}
  function GetApplicationDirectory: string; {$IFDEF useinlining} inline; {$ENDIF}
  procedure SetMatrixLength(Matrix : TAVDDoubleDynMatrix; Size : TPoint); overload;
  procedure SetMatrixLength(Matrix : TAVDSingleDynMatrix; Size : TPoint); overload;
  {$ENDIF}
  function ms2smp(ms, SampleRate: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function smp2ms(smp, SampleRate: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function getSyncFactor(base_factor: Single; dotted, triads: boolean): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function Sync2Smp(SyncFactor, bpm, SampleRate: Single): Integer; {$IFDEF useinlining} inline; {$ENDIF}
  function Factorial(Order : Integer) : Double;
  function f_Limit(v:Single;l:Single=-1;u:Single=1):Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Limit(v:Double;l:Double=-1;u:Double=1):Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Clip(x,l,h:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Cliplo(x,l:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Cliphi(x,h:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function dB_to_Amp(g:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function Amp_to_dB(v:Single):Single; overload;
  {$IFNDEF FPC}
  procedure Amp_to_dB(var v:T4SingleArray); overload;
  {$ENDIF}
  function Smallest(A, B: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function Largest(A, B: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function LimitAngle(const Angle: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Ln2(f:Single):Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Floorln2(f:Single):Integer; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Arctan(Value:Single):Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Arctan(Value:Double):Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Frac(Sample:Single):Single; overload;
  function f_Frac(Sample:Double):Double; overload;
  procedure f_Abs(var f:Single); {$IFDEF useinlining} inline; {$ENDIF} overload;
  procedure f_Abs(var f:Double); {$IFDEF useinlining} inline; {$ENDIF} overload;
  procedure f_Abs(var f:T4SingleArray); overload;

  {$IFNDEF FPC}
  function f_Int(Sample:Single):Single; overload;
  function f_Int(Sample:Double):Double; overload;
  function f_Trunc(Sample:Single):Integer; overload;
  function f_Trunc(Sample:Double):Integer; overload;
  procedure f_Trunc(Input:PSingle; Output:PInteger; SampleFrames: Integer); overload;
  function f_Round(Sample:Single):Integer; overload;
  function f_Round(Sample:Double):Integer; overload;

  function f_Exp(x:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}

  function f_Neg(f:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Root(i:Single;n:Integer):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_IntPower(i:Single;n:Integer):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Power(base, exp :Double) : Double; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Log2Laurent(val:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Log2Continous5(val:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Log2MinError5(val:Single):Single; {$IFDEF useinlining} inline; {$ENDIF}
  function f_Sin(Angle:Single):Single;
  function f_Cos(Angle:Single):Single;
  function f_Sgn(f:Single):Integer; {$IFDEF useinlining} inline; {$ENDIF}
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

  procedure GetSinCos(Frequency: Double; var SinValue, CosValue : Double); overload;
  procedure GetSinCos(Frequency: Extended; var SinValue, CosValue : Extended); overload;
  procedure GetSinCos(Frequency: Single; var SinValue, CosValue : Single); overload;

  function IsPowerOf2(Value:Integer) : Boolean;
  function RoundToPowerOf2(Value:Integer) : Integer;
  function TruncToPowerOf2(Value:Integer) : Integer;
  function ExtendToPowerOf2(Value:Integer) : Integer;
  function TruncLog2(Value : Extended): Integer; overload;
  function TruncLog2(Value : Integer): Integer; overload;
  function CeilLog2(Value : Extended): Integer; overload;
  function CeilLog2(Value : Integer): Integer; overload;
  function OnOff(value:Single) : Boolean;
  function unDenormalize(value:Single) : Single;

  function FastTanhOpt3(x: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4(x: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5(x: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6(x: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7(x: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt3(x: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4(x: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5(x: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6(x: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7(x: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  function FastTanhOpt3asm(x: Single): Single; assembler; overload;
  function FastTanhOpt4asm(x: Single): Single; assembler; overload;
  function FastTanhOpt5asm(x: Single): Single; assembler; overload;
  function FastTanhOpt6asm(x: Single): Single; assembler; overload;
  function FastTanhOpt7asm(x: Single): Single; assembler; overload;
  function FastTanhOpt3asm(x: Double): Double; assembler; overload;
  function FastTanhOpt4asm(x: Double): Double; assembler; overload;
  function FastTanhOpt5asm(x: Double): Double; assembler; overload;
  function FastTanhOpt6asm(x: Double): Double; assembler; overload;
  function FastTanhOpt7asm(x: Double): Double; assembler; overload;

  function Tanh2a(x:Single):Single;
  function Tanh2b(x:Single):Single;
  function Tanh2c(x:Single):Single;
  function Tanh2d(x:Single):Single;
  function Sigmoid(x:Single):Single;
  function Sinc(x:Double):Double;
  {$IFNDEF FPC}
  procedure Msg(b:boolean); overload;
  procedure Msg(m:string;m2:string=''); overload;
  procedure Msg(i:Integer); overload;
  procedure Msg(s:Single); overload;
  procedure Msg(m:string;i:Integer); overload;

  function FloatWithUnit(f:Double):string;
  function SplitString(S: String; Delimiter: char): TStrArray;
  function MakeGoodFileName(s: string): string;
  {$ENDIF}

  function FindMaximum(InBuffer: PSingle; Samples: Integer): Integer; overload;
  function FindMaximum(InBuffer: PDouble; Samples: Integer): Integer; overload;
  procedure CalcMinMax(InBuffer: PSingle; Samples: Integer; var MinMax : TAVDMinMaxSingle); overload;
  procedure CalcMinMax(InBuffer: PDouble; Samples: Integer; var MinMax : TAVDMinMaxDouble); overload;
  procedure DCSubstract(InBuffer: PSingle; Samples: Integer); overload;
  procedure DCSubstract(InBuffer: PDouble; Samples: Integer); overload;
  procedure ConvertSingleToDouble(Singles : PSingle; Doubles : PDouble; SampleFrames:Integer);
  procedure ConvertDoubleToSingle(Doubles : PDouble; Singles : PSingle; SampleFrames:Integer);

var
  ln10, ln2, ln22, ln2Rez : Double;

const
  MinusOneThird : Double = -1/3;
  Two           : Double = 2;
  MinusTwoThird : Double = -2/3;


implementation

uses Math, SysUtils;

const
  {$IFNDEF PUREPASCAL}
  Half          : Double = 0.5;
  {$ENDIF}
  Twenty        : Double = 20;

{$IFNDEF FPC}
procedure SetMatrixLength(Matrix : TAVDDoubleDynMatrix; Size : TPoint);
var i : Integer;
begin
 SetLength(Matrix,Size.X);
 for i:=0 to Size.X-1
  do SetLength(Matrix[i],Size.Y);
end;

procedure SetMatrixLength(Matrix : TAVDSingleDynMatrix; Size : TPoint);
var i : Integer;
begin
 SetLength(Matrix,Size.X);
 for i:=0 to Size.X-1
  do SetLength(Matrix[i],Size.Y);
end;
{$ENDIF}

{ Math }

function Factorial(Order : Integer) : Double;
var i : Integer;
begin
 if Order = 0
  then result := 1
  else
   begin
    result := 1;
    for i := 2 to Order
     do result := result * i;
   end;
end;

// Limit a value to be l<=v<=u
function f_Limit(v:Single;l:Single=-1;u:Single=1):Single;
begin
 if v<l then Result:=l
 else if v>u then Result:=u else Result:=v;
end;

// Limit a value to be l<=v<=u
function f_Limit(v:Double;l:Double=-1;u:Double=1):Double;
begin
 if v<l then Result:=l
 else if v>u then Result:=u else Result:=v;
end;

// Convert a value in dB's to a linear amplitude
function dB_to_Amp(g:Single) : Single;
begin
 if (g > -300.0)
  then Result := exp(g * 0.11512925464970228420089957273422) //Power(10, g / 20) //Power(2, g * 0.015051499783199059760686944736225)
  else Result := 0;
end;

{$IFNDEF FPC}
{$WARNINGS OFF}
function f_ArcTan2(const Y, X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result:=ArcTan2(Y,X);
{$ELSE}
asm
 fld Y
 fld X
 fpatan
{$ENDIF}
end;

function f_Tan(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result:=Tan(X);
{$ELSE}
asm
 fld X
 fptan
 fstp st(0)
{$ENDIF}
end;

function f_CoTan(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result:=CoTan(X);
{$ELSE}
asm
 fld X
 fptan
 fdivrp
{$ENDIF}
end;

function f_Log10(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result:=Log10(X);
{$ELSE}
asm
 fldlg2
 fld x
 fyl2x
{$ENDIF}
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
 while Result >= 360 do Result:=Result-360;
end;

function Amp_to_dB(v:Single):Single;
{$IFDEF PUREPASCAL}
begin
 result := Twenty * Log10(v);
{$ELSE}
asm
 fldlg2
 fld v
 fyl2x
 fmul Twenty.Double
{$ENDIF}
end;

function f_Frac(Sample: Single): Single;
{$IFDEF PUREPASCAL}
begin
 result:=Sample - Round(Sample - 0.5);
{$ELSE}
var i : Integer;
asm
 fld Sample.Single
 fld Sample.Single
 fsub half
 frndint
 fsubp
{$ENDIF}
end;

function f_Frac(Sample: Double): Double;
{$IFDEF PUREPASCAL}
begin
 result := Sample - Round(Sample - 0.5);
{$ELSE}
var i : Integer;
asm
 fld Sample.Double
 fld Sample.Double
 fsub half
 frndint
 fsubp
{$ENDIF}
end;

procedure f_Abs(var f: Single);
var i : Integer absolute f;
begin
 i := i and $7FFFFFFF;
end;

procedure f_Abs(var f: Double);
var i : array [0..1] of Integer absolute f;
begin
 i[0] := i[0] and $7FFFFFFF;
end;

{$IFNDEF FPC}
procedure Amp_to_dB(var v:T4SingleArray);
{$IFDEF PUREPASCAL}
begin
 v[0] := Amp_to_dB(v[0]);
 v[1] := Amp_to_dB(v[1]);
 v[2] := Amp_to_dB(v[2]);
 v[3] := Amp_to_dB(v[3]);
{$ELSE}
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
{$ENDIF}
end;

function f_Trunc(Sample:Single):Integer;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample - 0.5);
{$ELSE}
asm
 fld Sample.Single
 fsub half
 fistp Result.Integer
{$ENDIF}
end;

function f_Trunc(Sample:Double):Integer;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample - 0.5);
{$ELSE}
asm
 fld Sample.Double
 fsub half
 fistp Result.Integer
{$ENDIF}
end;

procedure f_Trunc(Input: PSingle; Output: PInteger; SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Output^ := Round(Input^ - 0.5);
   inc(Output);
   inc(Input);
  end;
{$ELSE}
asm
 @Start:
 fld [eax].Single
 fsub half
 fistp [edx].Integer
 add eax,4
 add edx,4
 loop    @Start
{$ENDIF}
end;

function f_Int(Sample: Single): Single;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample - 0.5);
{$ELSE}
asm
 fld Sample.Single
 fsub half
 frndint
{$ENDIF}
end;

function f_Int(Sample: Double): Double;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample - 0.5);
{$ELSE}
asm
 fld Sample.Double
 fsub half
 frndint
{$ENDIF}
end;

function f_Round(Sample: Single): Integer;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample);
{$ELSE}
asm
 fld Sample.Single
 frndint
 fistp Result.Integer
{$ENDIF}
end;

function f_Round(Sample: Double): Integer;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample);
{$ELSE}
asm
 fld Sample.Double
 frndint
 fistp Result.Integer
{$ENDIF}
end;   

function f_Exp(x: Single): Single;
begin
 Result := Exp(x * ln2);
end;

function f_Sin(Angle: Single): Single;
const
  sin1 : Double = 7.61e-03;
  sin2 : Double = -1.6605e-01;
{$IFDEF PUREPASCAL}
var Asqr : Double;
begin
 Asqr   := sqr(Angle);
 result := (((Asqr * sin1) * Asqr + sin2 * Asqr) + 1) * Angle;
{$ELSE}
asm
 fld Angle.Single
 fmul Angle.Single
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fld1
 faddp
 fmul Angle
{$ENDIF}
end;

function f_Cos(Angle:Single):Single;
const
  sin1 : Double =  3.705e-02;
  sin2 : Double = -4.967e-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Double;
begin
 Asqr   := sqr(Angle);
 result := (((Asqr * sin1) * Asqr + sin2 * Asqr) + 1) * Angle;
{$ELSE}
asm
 fld Angle.Single
 fmul Angle.Single
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fld1
 faddp
 fmul Angle
{$ENDIF}
end;

{$ENDIF}

function f_ArcTan(Value: Single): Single;
var VSqr : Double;
begin
 VSqr   := sqr(Value);
 Result := ((((0.0208351 * VSqr - 0.085133) * VSqr + 0.180141) * VSqr - 0.3302995) * VSqr + 0.999866) * Value;
end;

function f_ArcTan(Value: Double): Double;
var VSqr : Double;
begin
 VSqr   := sqr(Value);
 Result := ((((0.0208351 * VSqr - 0.085133) * VSqr + 0.180141) * VSqr - 0.3302995) * VSqr + 0.999866) * Value;
end;

function f_Ln2(f: Single): Single;
begin
 Result := (((Integer((@f)^) and $7F800000) shr 23) - $7F) +
             (Integer((@f)^) and $007FFFFF) / $800000;
end;

function f_FloorLn2(f: Single): Integer;
begin
 Result:=(((Integer((@f)^) and $7F800000) shr 23)-$7F);
end;

procedure f_Abs(var f: T4SingleArray); overload;
{$IFDEF PUREPASCAL}
var
  i0 : Integer absolute f[0];
  i1 : Integer absolute f[0];
  i2 : Integer absolute f[0];
  i3 : Integer absolute f[0];
begin
 i0 := i0 and $7FFFFFFF;
 i1 := i1 and $7FFFFFFF;
 i2 := i2 and $7FFFFFFF;
 i3 := i3 and $7FFFFFFF;
{$ELSE}
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
{$ENDIF}
end;

function f_Neg(f: Single): Single;
var i,j:Integer;
begin
 j := $80000000;
 i := Integer((@f)^) xor j;
 Result := Single((@i)^);
end;

{$IFNDEF FPC}
function f_Sgn(f:Single):Integer;
begin
 Result := 1 - ((Integer((@f)^) shr 31) shl 1);
end;
{$ENDIF}

function f_Log2Laurent(val: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute val;
begin
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 val := ((MinusOneThird * val) + Two) * val + MinusTwoThird;
 Result := val + log2;
end;

function f_Log2MinError5(val: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute val;
begin
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 val := ((( - 8.18038640187952054E-2 *
        val + 6.46216635143615381E-1) *
        val - 2.12293700635511007) *
        val + 4.07217052527789480) *
        val - 1.51355930430330177;
 Result := val + log2;
end;

function f_Log2Continous5(val: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute val;
begin
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 val := ((( - 8.21343513178931783E-2 *
        val + 6.49732456739820052E-1) *
        val - 2.13417801862571777) *
        val + 4.08642207062728868) *
        val - 1.51984215742349793;
 Result := val + log2;
end;

function f_IntPower(i:Single;n:Integer):Single;
var l:Integer;
begin
 l := Integer((@i)^);
 l := (l - $3F800000) shl (n-1) + $3F800000;
 Result:=Single((@l)^);
end;

function f_Power(base, exp :Double) : Double;
begin
 Result := Power(base, exp);
end;

function f_Root(i:Single;n:Integer):Single;
var l:Integer;
begin
 l := Integer((@i)^);
 l := (l - $3F800000) shr (n-1) + $3F800000;
 Result:=Single((@l)^);
end;

function f_Cliplo(x,l:Single):Single;
begin
 x := x - l;
 Result := (x + abs(x)) * 0.5 + l;
end;

function f_Cliphi(x,h:Single):Single;
begin
 x := h - x;
 Result := h -(x + abs(x)) * 0.5;
end;

function f_Clip(x,l,h:Single):Single;
begin
 Result := (abs(x-l) + (l+h) - abs(x-h)) * 0.5;
end;

// scale logarithmicly from 20 Hz to 20 kHz
function FreqLinearToLog(value:Single):Single;
{$IFDEF PUREPASCAL}
begin
 Result := (Twenty * Exp(value * 6.907755279));
{$ELSE}
const fltl2:Double=6.907755279;
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
 FMUL Twenty
{$ENDIF}
end;

function FreqLogToLinear(Value:Single):Single;
{$IFDEF PUREPASCAL}
begin
 Result:=ln(value*0.05)*1.44764826019E-1;
{$ELSE}
const fltl1:Double=0.05;
      fltl2:Double=1.44764826019E-1;
asm
 fldln2
 fld value.Single
 fmul fltl1
 fyl2x
 fmul fltl2
{$ENDIF}
end;   

procedure GetSinCos(Frequency: Extended; var SinValue, CosValue : Extended);
{$IFDEF PUREPASCAL}
begin
 SinValue:=Sin(Frequency);
 CosValue:=Cos(Frequency);
{$ELSE}
asm
  fld Frequency;
  fsincos
  fstp    tbyte ptr [edx]    // Cos
  fstp    tbyte ptr [eax]    // Sin
{$ENDIF}
end;

procedure GetSinCos(Frequency: Double; var SinValue, CosValue : Double);
{$IFDEF PUREPASCAL}
begin
 SinValue:=Sin(Frequency);
 CosValue:=Cos(Frequency);
{$ELSE}
asm
 fld Frequency.Double;
 fsincos
 fstp [CosValue].Double;
 fstp [SinValue].Double;
{$ENDIF}
end;

procedure GetSinCos(Frequency: Single; var SinValue, CosValue : Single);
{$IFDEF PUREPASCAL}
begin
 SinValue := Sin(Frequency);
 CosValue := Cos(Frequency);
{$ELSE}
asm
 fld Frequency;
 fsincos
 fstp [CosValue].Single;
 fstp [SinValue].Single;
{$ENDIF}
end;

function IsPowerOf2(Value:Integer) : Boolean;
begin
 result := abs(IntPower(2, round(Log2(Value))) - Value) < 1E-20;
end;

function RoundToPowerOf2(Value:Integer) : Integer;
begin
 Result := round(Log2(Value));
 Result := (Value shr (Result - 1)) shl (Result - 1);
end;

function TruncToPowerOf2(Value:Integer) : Integer;
begin
 result := 1;
 while result <= value do result := result shl 1;
 result := result shr 1;
end;

function ExtendToPowerOf2(Value:Integer) : Integer;
begin
 result := 1;
 while result < value do result := result shl 1;
end;

function TruncLog2(Value : Extended): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value));
{$ELSE}
asm
 fld Value.Extended
 fxtract
 fstp st(0)
 fistp result.Integer
{$ENDIF}
end;

function TruncLog2(Value : Integer): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value));
{$ELSE}
var
  temp : Integer;
asm
 mov temp, Value;
 fild temp.Integer
 fld Value.Extended
 fxtract
 fstp st(0)
 fistp result.Integer
{$ENDIF}
end;

function CeilLog2(Value : Extended): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value) + 1);
{$ELSE}
asm
 fld Value.Extended
 fld1
 fsubp
 fxtract
 fstp st(0)
 fld1
 faddp
 fistp result.Integer
{$ENDIF}
end;

function CeilLog2(Value : Integer): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value) + 1);
{$ELSE}
var
  temp : Integer;
asm
 dec Value
 mov temp, Value;
 fild temp.Integer
 fxtract
 fstp st(0)
 fistp result.Integer
 inc result
{$ENDIF}
end;

function OnOff(value:Single):boolean;
begin Result := value > 0.5 end;

function UnDenormalize(value : Single) : Single;
begin
 if (abs(value) < 1.0e-20)
  then Result := 0.0
  else Result := value;
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
 GetModuleFilename(hinstance, s, sizeof(s));
 Result := strpas(s);
 Result := ExtractFilename(Result);
end;

function GetApplicationDirectory:string;
var s  : array[0..1500] of char;
begin
 GetModuleFilename(hinstance, s, sizeof(s));
 Result := strpas(s);
 Result := ExtractFileDir(Result);
end;

function FloatWithUnit(f: Double):string;
begin
 if f > 1 then result := FloatToStrF(f, ffFixed, 6, 3)+ 's' else
 if f > 0.001 then result:=FloatToStrF(1E3 * f, ffFixed, 6, 3)+ 'ms' else
 if f > 0.000001
  then result := FloatToStrF(1E6 * f, ffFixed, 6, 3)+ 'µs'
  else result := FloatToStrF(1E9 * f, ffFixed, 6, 3)+ 'ns'
end;

function SplitString(S: String; Delimiter: char): TStrArray;
var C: Integer;
begin
 repeat
  SetLength(Result, Length(Result) + 1);
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
 Result := Round(SyncFactor * SampleRate * 60 / bpm);
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

// SINC Function
function Sinc(x:Double):Double;
var pix : Double;
begin
 if (x=0)
  then result:=1
  else
   begin
    pix := PI * x;
    result := sin(pix) / pix;
   end;
end;

function Tanh2a(x:Single):Single;
var a,b:Single;
begin
 a := abs(x);
 b := 12 + a * (6 + a * (3 + a));
 Result := (x * b) / (a * b + 24);
end;

function Tanh2b(x:Single):Single;
var a,b:Single;
begin
 a := abs(x);
 b := (6 + a * (3 + a));
 Result := (x * b) / (a * b + 12);
end;

function Tanh2c(x:Single):Single;
{$IFDEF PUREPASCAL}
var a,b:Single;
begin
 a := abs(x);
 b := 3 + a;
 Result := (x * b) / (a * b + 6 );
{$ELSE}
const
  c3: Single = 3;
  c6: Single = 6;
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
{$ENDIF}
end;

function Tanh2d(x:Single):Single;
{$IFDEF PUREPASCAL}
begin
 Result:=x / (abs(x) + 3);
{$ELSE}
const c3:Single=3;
asm
 fld x.Single;
 fld x.Single;
 fabs
 fadd c3
 fdiv
{$ENDIF}
end;

function FastTanhOpt3(x: Single): Single;
var
  a, b : Double;
begin
 a := abs(x);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt4(x: Single): Single;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  0.89690305801668457 + a *
     ( 1.89047619399687661 + a *
     (-1.35205169119085666 + a *
       1.74656303770202670));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt5(x: Single): Single;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt6(x: Single): Single;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  0.98516470896867081 + a *
     ( 1.21020234045009012 + a *
     (-0.22720155259481389 + a *
     ( 1.89719615102030725 + a *
     (-1.07161642656874956 + a *
     ( 0.40487405571569546)))));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt7(x: Single): Single;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  1.00518193411912860 + a *
     ( 0.91005085146116016 + a *
     ( 1.14542500876429276 + a *
     (-0.76509890972158046 + a *
     ( 1.34808969964882519 + a *
     (-0.60147655894944263 + a *
     ( 0.15264109378548973))))));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt3(x: Double): Double;
var
  a, b : Double;
begin
 a := abs(x);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt4(x: Double): Double;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  0.89690305801668457 + a *
     ( 1.89047619399687661 + a *
     (-1.35205169119085666 + a *
       1.74656303770202670));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt5(x: Double): Double;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt6(x: Double): Double;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  0.98516470896867081 + a *
     ( 1.21020234045009012 + a *
     (-0.22720155259481389 + a *
     ( 1.89719615102030725 + a *
     (-1.07161642656874956 + a *
     ( 0.40487405571569546)))));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt7(x: Double): Double;
var
  a, b : Double;
begin
 a := abs(x);
 b :=  1.00518193411912860 + a *
     ( 0.91005085146116016 + a *
     ( 1.14542500876429276 + a *
     (-0.76509890972158046 + a *
     ( 1.34808969964882519 + a *
     (-0.60147655894944263 + a *
     ( 0.15264109378548973))))));
 Result := (b * x) / (b * a + 1);
end;

function FastTanhOpt3asm(x: Single): Single; assembler;
const
  c0 : Double =  2.66559097474027817;
  c1 : Double = -0.54699348440059470;
  c2 : Double =  1.26175667589988239;
asm
 fld x.Single      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: b := c2 + a * (c1 + a * c0), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt4asm(x: Single): Single; assembler;
const
  c0 : Double =  1.74656303770202670;
  c1 : Double = -1.35205169119085666;
  c2 : Double =  1.89047619399687661;
  c3 : Double =  0.89690305801668457;
asm
 fld x.Single      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt5asm(x: Single): Single; assembler;
const
  c0 : Double =  0.91996358346770157;
  c1 : Double = -1.46060069227128242;
  c2 : Double =  2.13184139104070569;
  c3 : Double =  0.54953758170495126;
  c4 : Double =  1.03971379878158321;
asm
 fld x.Single      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt6asm(x: Single): Single; assembler;
const
  c0 : Double =  0.40487405571569546;
  c1 : Double = -1.07161642656874956;
  c2 : Double =  1.89719615102030725;
  c3 : Double = -0.22720155259481389;
  c4 : Double =  1.21020234045009012;
  c5 : Double =  0.98516470896867081;
asm
 fld x.Single      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fadd c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fadd c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt7asm(x: Single): Single; assembler;
const
  c0 : Double =  0.152641093785489734;
  c1 : Double = -0.60147655894944263;
  c2 : Double =  1.34808969964882519;
  c3 : Double = -0.765098909721580456;
  c4 : Double =  1.14542500876429276;
  c5 : Double =  0.91005085146116016;
  c6 : Double =  1.00518193411912860;
asm
 fld x.Single      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fadd c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fmul st(0), st(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, x
 fadd c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt3asm(x: Double): Double; assembler;
const
  c0 : Double =  2.66559097474027817;
  c1 : Double = -0.54699348440059470;
  c2 : Double =  1.26175667589988239;
asm
 fld x.Double      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: b := c2 + a * (c1 + a * c0), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt4asm(x: Double): Double; assembler;
const
  c0 : Double =  1.74656303770202670;
  c1 : Double = -1.35205169119085666;
  c2 : Double =  1.89047619399687661;
  c3 : Double =  0.89690305801668457;
asm
 fld x.Double      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt5asm(x: Double): Double; assembler;
const
  c4 : Double =  0.91996358346770157;
  c3 : Double = -1.46060069227128242;
  c2 : Double =  2.13184139104070569;
  c1 : Double =  0.54953758170495126;
  c0 : Double =  1.03971379878158321;
asm
 fld x.Double      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt6asm(x: Double): Double; assembler;
const
  c0 : Double =  0.40487405571569546;
  c1 : Double = -1.07161642656874956;
  c2 : Double =  1.89719615102030725;
  c3 : Double = -0.22720155259481389;
  c4 : Double =  1.21020234045009012;
  c5 : Double =  0.98516470896867081;
asm
 fld x.Double      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fadd c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fadd c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function FastTanhOpt7asm(x: Double): Double; assembler;
const
  c0 : Double =  0.152641093785489734;
  c1 : Double = -0.60147655894944263;
  c2 : Double =  1.34808969964882519;
  c3 : Double = -0.765098909721580456;
  c4 : Double =  1.14542500876429276;
  c5 : Double =  0.91005085146116016;
  c6 : Double =  1.00518193411912860;
asm
 fld x.Double      // Load x
 fld st(0)         // Copy x
 fabs              // Stack: abs(x), x
 fld c0            // Load c0 as working value, abs(x) => a
 fmul st(0), st(1) // Stack: a * c0, a, x
 fadd c1           // Stack: c1 + a * c0, a, x
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, x
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, x
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, x
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, x
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, x
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fadd c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, x
 fmul st(0), st(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, x
 fadd c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, x
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * x, a, b
 fxch st(2)        // exchange b * x and x, Stack: b, a, b * x
 fmulp             // Stack: b * a, b * x
 fld1              // Stack: 1, b * a, b * x
 faddp             // Stack: 1 + b * a, b * x
 fdivp             // Stack: (b * x) / (1 + b * a)
end;

function Sigmoid(x:Single):Single;
begin
 if(abs(x)<1)
  then Result := x * (1.5 - 0.5 * x * x)
  else
   if x < 0
    then Result:=-1
    else Result:= 1;
end;

{$IFNDEF FPC}
function f_Min(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
 if A>B
  then result:=B
  else result:=A
{$ELSE}
asm
 fld     dword ptr [ebp+$08]
 fld     dword ptr [ebp+$0c]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
{$ENDIF}
end;

function f_Max(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
 if A<B
  then result:=B
  else result:=A
{$ELSE}
asm
 fld     dword ptr [ebp+$0c]
 fld     dword ptr [ebp+$08]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
{$ENDIF}
end;
{$ENDIF}

procedure CalcMinMax(InBuffer: PSingle; Samples: Integer; var MinMax : TAVDMinMaxSingle);
var i : Integer;
begin
 assert(Samples > 0);
 MinMax.min := InBuffer^;
 MinMax.max := InBuffer^;
 for i := 1 to Samples - 1 do
  begin
   if InBuffer^ > MinMax.max then MinMax.max := InBuffer^ else
   if InBuffer^ < MinMax.min then MinMax.min := InBuffer^;
   inc(InBuffer);
  end;
end;

procedure CalcMinMax(InBuffer: PDouble; Samples: Integer; var MinMax : TAVDMinMaxDouble);
var i : Integer;
begin
 assert(Samples > 0);
 MinMax.min := InBuffer^;
 MinMax.max := InBuffer^;
 for i := 1 to Samples - 1 do
  begin
   if InBuffer^ > MinMax.max then MinMax.max := InBuffer^ else
   if InBuffer^ < MinMax.min then MinMax.min := InBuffer^;
   inc(InBuffer);
  end;
end;

procedure DCSubstract(InBuffer: PSingle; Samples: Integer);
{$IFDEF PUREPASCAL}
var
  InBuf : array [0..0] of Double absolute InBuffer;
  d : Double;
  i : Integer;
begin
 if Samples = 0 then Exit;
 d := InBuf[0];
 for i := 1 to Samples - 1
  do d := d + InBuf[i];
 d := d / Samples;
 for i := 0 to Samples - 1
  do InBuf[i] := InBuf[i] - d;
{$ELSE}
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
{$ENDIF}
end;

procedure DCSubstract(InBuffer: PDouble; Samples: Integer);
{$IFDEF PUREPASCAL}
var
  InBuf : array [0..0] of Double absolute InBuffer;
  d : Double;
  i : Integer;
begin
 if Samples = 0 then Exit;
 d := InBuf[0];
 for i := 1 to Samples - 1
  do d := d + InBuf[i];
 d := d / Samples;
 for i := 0 to Samples - 1
  do InBuf[i] := InBuf[i] - d;
{$ELSE}
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
{$ENDIF}
end;

procedure ConvertSingleToDouble(Singles : PSingle; Doubles : PDouble; SampleFrames:Integer);
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Doubles^ := Singles^;
   inc(Singles);
   inc(Doubles);
  end;
{$ELSE}
asm
@MarioLand:
 fld  [eax + ecx * 4 - 4].Single
 fstp [edx + ecx * 8 - 8].Double
 loop @MarioLand
{$ENDIF}
end;

procedure ConvertDoubleToSingle(Doubles : PDouble; Singles : PSingle; SampleFrames:Integer);
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 for i:=0 to SampleFrames-1 do
  begin
   Singles^:=Doubles^;
   inc(Singles);
   inc(Doubles);
  end;
{$ELSE}
asm
@MarioLand:
 fld [eax + ecx * 8 - 8].Double
 fstp [edx + ecx * 4 - 4].Single
 loop @MarioLand
{$ENDIF}
end;

function FindMaximum(InBuffer: PSingle; Samples: Integer): Integer;
{$IFDEF PUREPASCAL}
var i : Integer;
    d : Double;
begin
 result := 0;
 assert(Samples > 0);
 d := abs(InBuffer^);
 for i:=1 to Samples-1 do
  begin
   if abs(InBuffer^) > d then
    begin
     Result := i;
     d := abs(InBuffer^);
    end;
   inc(InBuffer);
  end;
{$ELSE}
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
{$ENDIF}
end;

function FindMaximum(InBuffer: PDouble; Samples: Integer): Integer;
{$DEFINE PUREPASCAL}
{$IFDEF PUREPASCAL}
var
  i : Integer;
  d : Double;
begin
 result := 0;
 assert(Samples > 0);
 d := abs(InBuffer^);
 for i := 1 to Samples - 1 do
  begin
   if abs(InBuffer^) > d then
    begin
     Result := i;
     d := abs(InBuffer^);
    end;
   inc(InBuffer);
  end;
{$ELSE}
asm
 test edx,edx
 jz @End

 mov result,edx                // Result := edx
 dec edx
 jz @End                       // only one sample -> exit!
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
{$ENDIF}
end;

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
