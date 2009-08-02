unit DAV_Common;

interface

{$I DAV_Compiler.inc}

{$IFDEF FPC}
uses LCLIntf; {$DEFINE PUREPASCAL}
{$ELSE}
uses Windows {$IFDEF UseNativeTypes}, Types{$ENDIF};
{$ENDIF}

type
  {$IFNDEF DELPHI7_UP}
    TDAVSingleDynArray = Array of Single;
    TDAVDoubleDynArray = Array of Double;
  {$ELSE}
    {$IFDEF UseNativeTypes}
      TDAVSingleDynArray = Types.TSingleDynArray;
      TDAVDoubleDynArray = Types.TDoubleDynArray;
    {$ELSE}
      TDAVSingleDynArray = Array of Single;
      TDAVDoubleDynArray = Array of Double;
    {$ENDIF}
  {$ENDIF}

  PDAVSingleDynArray = ^TDAVSingleDynArray;
  PDAVDoubleDynArray = ^TDAVDoubleDynArray;

  TDAVSingleFixedArray = Array [0..0] of Single;
  PDAVSingleFixedArray = ^TDAVSingleFixedArray;
  TDAVDoubleFixedArray = Array [0..0] of Double;
  PDAVDoubleFixedArray = ^TDAVDoubleFixedArray;

  TDAVArrayOfSingleDynArray = array of TDAVSingleDynArray;
  PDAVArrayOfSingleDynArray = ^TDAVArrayOfSingleDynArray;
  TDAVArrayOfDoubleDynArray = array of TDAVDoubleDynArray;
  PDAVArrayOfDoubleDynArray = ^TDAVArrayOfDoubleDynArray;

  TDAVArrayOfSingleFixedArray = array [0..0] of TDAVSingleFixedArray;
  PDAVArrayOfSingleFixedArray = ^TDAVArrayOfSingleFixedArray;
  TDAVArrayOfDoubleFixedArray = array [0..0] of TDAVDoubleFixedArray;
  PDAVArrayOfDoubleFixedArray = ^TDAVArrayOfDoubleFixedArray;

  TDAVSingleDynMatrix = TDAVArrayOfSingleDynArray;
  PDAVSingleDynMatrix = ^TDAVSingleDynMatrix;
  TDAVDoubleDynMatrix = TDAVArrayOfDoubleDynArray;
  PDAVDoubleDynMatrix = ^TDAVDoubleDynMatrix;

  TDAV2SingleArray = Array [0..1] of Single;
  PDAV2SingleArray = ^TDAV2SingleArray;
  TDAV2DoubleArray = Array [0..1] of Double;
  PDAV2DoubleArray = ^TDAV2DoubleArray;

  TDAV4SingleArray = Array [0..3] of Single;
  PDAV4SingleArray = ^TDAV4SingleArray;
  TDAV4DoubleArray = Array [0..3] of Double;
  PDAV4DoubleArray = ^TDAV4DoubleArray;

  TDAV6SingleArray = Array [0..5] of Single;
  PDAV6ingleArray = ^TDAV6SingleArray;
  TDAV6DoubleArray = Array [0..5] of Double;
  PDAV6DoubleArray = ^TDAV6DoubleArray;

  TDAV8SingleArray = Array [0..7] of Single;
  PDAV8SingleArray = ^TDAV8SingleArray;
  TDAV8DoubleArray = Array [0..7] of Double;
  PDAV8DoubleArray = ^TDAV8SingleArray;

  TDAVMinMaxSingle = record
    min : Single;
    max : Single;
  end;
  TDAVMinMaxDouble = record
    min : Double;
    max : Double;
  end;

  TStrArray = array of string;

  TChunkName = array [0..3] of AnsiChar;

  {$IFDEF Delphi5}
  PCardinal = ^Cardinal;
//  TValueSign = set of (-1, 0, 1);
  PSingle = ^Single;
  PDouble = ^Double;
  {$ENDIF}

  TDAVMidiEvent = record
    MidiData        : array[0..3] of Byte;  // 1 thru 3 midi Bytes; midiData[3] is reserved (zero)
    DeltaFrames     : LongInt;              // sample frames related to the current block start sample position
    NoteOffset      : LongInt;              // offset into note from note start if available, else 0
    NoteLength      : LongInt;              // (in sample frames) of entire note, if available, else 0
    Detune          : Byte;                 // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
    NoteOffVelocity : Byte;
  end;


  { Byte Ordering }

  function SWAP_16(value: SmallInt): SmallInt;
  function SWAP_32(value: LongInt): LongInt;
  function SWAP_64(value: Int64): Int64;

  function SwapLong(var Value): LongInt;
  procedure FlipWord(var Value); overload;
  procedure FlipLong(var Value); overload;
  procedure FlipExtended(var Value : Extended); overload;


  { Compatibility }

  {$IFDEF DELPHI5}
  function Sign(const AValue: Single): Single; overload;
  function Sign(const AValue: Double): Double; overload;
  {$ENDIF}


  { Convert }

  function ms2Samples(const ms, SampleRate: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Samples2ms(const Samples, SampleRate: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Sync2Samples(const SyncFactor, BPM, SampleRate: Single): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function GetSyncFactor(const BaseFactor: Single; const Dotted, Triads: Boolean): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Compare4(S1, S2 : PAnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  // dB stuff
  function dB_to_Amp(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function dB_to_Amp(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function SqrAmp2dB(const Value: Single): Single; overload;
  function SqrAmp2dB(const Value: Double): Double; overload;
  function Amp_to_dB(const Value: Single): Single; overload;
  function Amp_to_dB(const Value: Double): Double; overload;
  {$IFNDEF FPC}
  procedure Amp_to_dB(var v: TDAV4SingleArray); overload; // TODO: move to VectorMath!
  {$ENDIF}

  // scale logarithmically from 20 Hz to 20 kHz
  function FreqLinearToLog(const Value: Single): Single; overload;
  function FreqLinearToLog(const Value: Double): Double; overload;
  function FreqLogToLinear(const Value: Single): Single; overload;
  function FreqLogToLinear(const Value: Double): Double; overload;


  { Limit & Clip, Min & Max }

  function Limit(const Value: Single; Lower: Single = -1; Upper: Single = 1): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Limit(const Value: Double; Lower: Double = -1; Upper: Double = 1): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Limit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function BranchlessClip(const Value, Lower, Upper: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function BranchlessClip(const Value, Lower, Upper: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function BranchlessClipLower(Value: Single; const Lower: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function BranchlessClipLower(Value: Double; const Lower: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function BranchlessClipUpper(Value: Single; const Upper: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function BranchlessClipUpper(Value: Double; const Upper: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Smallest(const A, B: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Smallest(const A, B: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Largest(const A, B: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Largest(const A, B: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function LimitAngle(const Angle: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function LimitAngle(const Angle: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  function RandomGauss: Extended; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  function Factorial(const Order: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Factorial(const Order: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function Factorial(const Order: Integer): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function FastFractional(const Value: Single): Single; overload;
  function FastFractional(const Value: Double): Double; overload;
  function FastRandom: Single;
  procedure FastAbs(var Value: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
  procedure FastAbs(var Value: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
  procedure FastAbs(var Value: TDAV4SingleArray); overload;
  procedure FastNegative(var Value: Single); overload;
  function FastSgn(const Value: Single): Integer;
  function FastMin(const A, B: Single) : Single;
  function FastMax(const A, B: Single) : Single;
  function FastMod(const Arg1, Arg2: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {$IFNDEF FPC}
  function FastInt(Sample: Single): Single; overload;
  function FastInt(Sample: Double): Double; overload;
  function FastTrunc(const Value: Single): Integer; overload;
  function FastTrunc(const Value: Double): Integer; overload;
  procedure FastTrunc(Input: PSingle; Output:PInteger; SampleFrames: Integer); overload;
  function FastRound(Sample: Single): Integer; overload;
  function FastRound(Sample: Double): Integer; overload;
  {$ENDIF}

  function Tanh(const X: Extended): Extended; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
  function Tanh(const X: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
  function Tanh(const X: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

  procedure GetSinCos(const Frequency: Double; var SinValue, CosValue : Double); overload;
  procedure GetSinCos(const Frequency: Extended; var SinValue, CosValue : Extended); overload;
  procedure GetSinCos(const Frequency: Single; var SinValue, CosValue : Single); overload;

  function IsPowerOf2(const Value: Integer): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function RoundToPowerOf2(const Value: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function TruncToPowerOf2(const Value: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function ExtendToPowerOf2(const Value: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function TruncLog2(Value : Extended): Integer; overload;
  function TruncLog2(Value : Integer): Integer; overload;
  function CeilLog2(Value : Extended): Integer; overload;
  function CeilLog2(Value : Integer): Integer; overload;
  function OnOff(const Value: Single): Boolean;
  function unDenormalize(const Value: Single): Single;

  function Sigmoid(const Input: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
  function Sigmoid(const Input: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
  function Sinc(const Input: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
  function Sinc(const Input: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

  { String Stuff & Messages }

  {$IFNDEF FPC}
  function GetApplicationFilename: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function GetApplicationDirectory: string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  procedure Msg(b: Boolean); overload;
  procedure Msg(m: string; m2: string = ''); overload;
  procedure Msg(i: Integer); overload;
  procedure Msg(s: Single); overload;
  procedure Msg(m: string; i: Integer); overload;

  function FloatWithUnit(const Value: Double):string;
  function SplitString(S: String; Delimiter: AnsiChar): TStrArray;
  function MakeGoodFileName(s: string): string;
  {$ENDIF}

  { Block Routines }

  function FindMaximum(InBuffer: PSingle; Samples: Integer): Integer; overload;
  function FindMaximum(InBuffer: PDouble; Samples: Integer): Integer; overload;
  procedure CalcMinMax(InBuffer: PSingle; Samples: Integer; var MinMax : TDAVMinMaxSingle); overload;
  procedure CalcMinMax(InBuffer: PDouble; Samples: Integer; var MinMax : TDAVMinMaxDouble); overload;
  procedure DCSubstract(InBuffer: PSingle; Samples: Integer); overload;
  procedure DCSubstract(InBuffer: PDouble; Samples: Integer); overload;
  procedure ConvertSingleToDouble(Singles : PSingle; Doubles : PDouble; SampleFrames:Integer);
  procedure ConvertDoubleToSingle(Doubles : PDouble; Singles : PSingle; SampleFrames:Integer);

var
  ln10, ln2, ln22, ln2Rez : Double;

const
  CDenorm32          : Single = 1E-24;
  CTwoPI32           : Single = 2 * Pi;
  CFourPI32          : Single = 4 * Pi;
  CHalf32            : Single = 0.5;
  CQuarter32         : Single = 0.25;
  CTen32             : Single = 10;
  CTwenty32          : Single = 20;
  COneTwelfth32      : Single = 1 / 12;
  CMinusOneSixteenth : Single = -0.0625;
  CTwoMulTwo2Neg32   : Single = ((2.0/$10000) / $10000);  // 2^-32

  CDenorm64          : Double = 1E-34;
  CTwoPI64           : Double = 2 * Pi;
  CFourPI64          : Double = 4 * Pi;
  CHalf64            : Double = 0.5;
  CQuarter64         : Double = 0.25;
  CTen64             : Double = 10;
  CTwenty64          : Double = 20;

  CMaxLongInt        : Integer =  $7FFFFFFF;
  CMinLongInt        : Integer = -$7FFFFFFF - 1;
  CMaxInt64          : Int64 =  9223372036854775807;
  CMinInt64          : Int64 = -9223372036854775807 - 1;
  CMaxSingle         : Single = 3.40282346638528860e+38;

implementation

uses
  Math, SysUtils;

{ Byte Ordering }

type
  T16Bit = record
    case integer of
      0 :  (v: SmallInt);
      1 :  (b: array[0..1] of byte);
  end;

  T32Bit = record
    case integer of
      0 :  (v: LongInt);
      1 :  (b: array[0..3] of byte);
  end;

  T64Bit = record
    case integer of
      0 :  (v: Int64);
      1 :  (b: array[0..7] of byte);
  end;

function SWAP_16(Value: SmallInt): SmallInt;
var
  t: byte;
begin
  with T16Bit(value) do
   begin
    t := b[0];
    b[0] := b[1];
    b[1] := t;
    Result := v;
   end;
end;

function SWAP_32(Value: LongInt): LongInt;
var
  t: byte;
begin
 with T32Bit(value) do
  begin
   t := b[0];
   b[0] := b[3];
   b[3] := t;
   t := b[1];
   b[1] := b[2];
   b[2] := t;
   Result := v;
  end;
end;

function SWAP_64(Value: Int64): Int64;
var
   t: byte;
begin
 with T64Bit(value) do
  begin
   t := b[0];
   b[0] := b[7];
   b[7] := t;
   t := b[1];
   b[1] := b[6];
   b[6] := t;
   t := b[2];
   b[2] := b[5];
   b[5] := t;
   t := b[3];
   b[3] := b[4];
   b[4] := t;
   Result := v;
  end;
end;

function SwapLong(var Value): LongInt;
var
  t : Integer;
type
  X = array [0..1] of word;
begin
 T := Swap(X(Value)[1]);
 X(Value)[1] := Swap(X(Value)[0]);
 X(Value)[0] := T;
 result := t;
end;

procedure FlipLong(var Value); overload;
var
  VA   : array [0..3] of Byte absolute Value;
  temp : Byte;
begin
  temp  := VA[0];
  VA[0] := VA[3];
  VA[3] := temp;
  temp  := VA[1];
  VA[1] := VA[2];
  VA[2] := temp;
end;

procedure FlipWord(var Value);
var
  VA   : array [0..1] of Byte absolute Value;
  temp : Byte;
begin
  temp  := VA[0];
  VA[0] := VA[1];
  VA[1] := temp;
end;

procedure FlipExtended(var Value : Extended); overload;
var
  VA   : array [0..9] of Byte absolute Value;
  temp : Byte;
begin
 temp := VA[0]; VA[0] := VA[9]; VA[9] := temp;
 temp := VA[1]; VA[1] := VA[8]; VA[8] := temp;
 temp := VA[2]; VA[2] := VA[7]; VA[7] := temp;
 temp := VA[3]; VA[3] := VA[6]; VA[6] := temp;
 temp := VA[4]; VA[4] := VA[5]; VA[5] := temp;
end;

{ Convert }

function ms2Samples(const ms, SampleRate: Single): Single;
begin
 Result := ms * SampleRate * 0.001;
end;

function Samples2ms(const Samples, SampleRate: Single): Single;
begin
 Result := Samples * 1000 / SampleRate;
end;

function Sync2Samples(const SyncFactor, BPM, SampleRate: Single): Integer;
begin
 Result := Round(SyncFactor * SampleRate * 60 / BPM);
end;

function GetSyncFactor(const BaseFactor: Single; const Dotted, Triads: Boolean): Single;
begin
 Result := BaseFactor;
 if Dotted then Result := Result * 1.5;
 if Triads then Result := Result / 3;
end;

function Compare4(S1, S2: PAnsiChar): Boolean;
var
  i, Diff : Byte;
begin
 Result := False;
 for i := 0 to 3 do
  begin
   Diff := Byte(S1[i]) - Byte(S2[i]);
   if not (Diff in [0, 32, 224]) then Exit;
  end;
 Result := True;
end;


// Convert a value in dB's to a linear amplitude
function dB_to_Amp(const Value: Single): Single;
begin
 if (Value > -300.0)
  then Result := Exp(Value * 0.11512925464970228420089957273422) //Power(10, g / 20) //Power(2, g * 0.015051499783199059760686944736225)
  else Result := 0;
end;

// Convert a value in dB's to a linear amplitude
function dB_to_Amp(const Value: Double): Double;
begin
 if (Value > -300.0)
  then Result := Exp(Value * 0.11512925464970228420089957273422) //Power(10, g / 20) //Power(2, g * 0.015051499783199059760686944736225)
  else Result := 0;
end;                                                             // e^(x) = 2^(log2(e^x)) = 2^(x / ln(2))

function SqrAmp2dB(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 result := 10 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTen32
end;
{$ENDIF}

function SqrAmp2dB(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 result := 10 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTen64
end;
{$ENDIF}

function Amp_to_dB(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 result := CTwenty32 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTwenty32
end;
{$ENDIF}

function Amp_to_dB(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 result := CTwenty64 * Log10(Value);
end;
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
 fmul CTwenty64
end;
{$ENDIF}

procedure Amp_to_dB(var v: TDAV4SingleArray);
{$IFDEF PUREPASCAL}
begin
 v[0] := Amp_to_dB(v[0]);
 v[1] := Amp_to_dB(v[1]);
 v[2] := Amp_to_dB(v[2]);
 v[3] := Amp_to_dB(v[3]);
end;
{$ELSE}
asm
 fldlg2
 fld    [eax].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax].Single
 fldlg2
 fld    [eax + 4].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax + 4].Single
 fldlg2
 fld    [eax + 8].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax + 8].Single
 fldlg2
 fld    [eax + 12].Single
 fyl2x
 fmul   CTwenty32.Double
 fstp   [eax + 12].Single
end;
{$ENDIF}

// scale logarithmicly from 20 Hz to 20 kHz
function FreqLinearToLog(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := (CTwenty32 * Exp(value * 6.907755279));
end;
{$ELSE}
const
  fltl2: Single = 6.907755279;
asm
 fld     Value.Single
 fmul    fltl2
 fldl2e
 fmul
 fld     st(0)
 frndint
 fsub    st(1), st
 fxch    st(1)
 f2xm1
 fld1
 fadd
 fscale
 fstp    st(1)
 fmul CTwenty64.Double
end;
{$ENDIF}

function FreqLinearToLog(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result := (CTwenty64 * Exp(value * 6.907755279));
end;
{$ELSE}
const
  fltl2: Double = 6.907755279;
asm
 fld     Value.Double
 fmul    fltl2
 fldl2e
 fmul
 fld     st(0)
 frndint
 fsub    st(1), st
 fxch    st(1)
 f2xm1
 fld1
 fadd
 fscale
 fstp    st(1)
 fmul CTwenty64.Double
end;
{$ENDIF}

function FreqLogToLinear(const Value: Single): Single;
const
  fltl1 : Single = 0.05;
  fltl2 : Single = 1.44764826019E-1;
{$IFDEF PUREPASCAL}
begin
 Result := ln(value * fltl1) * fltl2;
end;
{$ELSE}
asm
 fldln2
 fld Value.Single
 fmul fltl1
 fyl2x
 fmul fltl2
end;
{$ENDIF}

function FreqLogToLinear(const Value: Double): Double;
const
  fltl1 : Double = 0.05;
  fltl2 : Double = 1.44764826019E-1;
{$IFDEF PUREPASCAL}
begin
 Result := ln(value * fltl1) * fltl2;
end;
{$ELSE}
asm
 fldln2
 fld Value.Double
 fmul fltl1
 fyl2x
 fmul fltl2
end;
{$ENDIF}


{ Limit & Clip, Min & Max }

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Single; Lower: Single = -1; Upper: Single = 1): Single;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Double; Lower: Double = -1; Upper: Double = 1): Double;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

function BranchlessClip(const Value, Lower, Upper: Single): Single;
begin
 Result := (abs(Value - Lower) + (Lower + Upper) - abs(Value - Upper)) * 0.5;
end;

function BranchlessClip(const Value, Lower, Upper: Double): Double;
begin
 Result := (abs(Value - Lower) + (Lower + Upper) - abs(Value - Upper)) * 0.5;
end;

function BranchlessClipLower(Value: Single; const Lower: Single): Single;
begin
 Value := Value - Lower;
 Result := (Value + abs(Value)) * 0.5 + Lower;
end;

function BranchlessClipLower(Value: Double; const Lower: Double): Double;
begin
 Value := Value - Lower;
 Result := (Value + abs(Value)) * 0.5 + Lower;
end;

function BranchlessClipUpper(Value: Single; const Upper: Single): Single;
begin
 Value := Upper - Value;
 Result := Upper -(Value + abs(Value)) * 0.5;
end;

function BranchlessClipUpper(Value: Double; const Upper: Double): Double;
begin
 Value := Upper - Value;
 Result := Upper -(Value + abs(Value)) * 0.5;
end;

function Smallest(const A, B: Single): Single;
begin
 if A < B
  then Result := A
  else Result := B;
end;

function Smallest(const A, B: Double): Double;
begin
 if A < B
  then Result := A
  else Result := B;
end;

function Largest(const A, B: Single): Single;
begin
 if A > B
  then Result := A
  else Result := B;
end;

function Largest(const A, B: Double): Double;
begin
 if A > B
  then Result := A
  else Result := B;
end;

function LimitAngle(const Angle: Single): Single;
begin
 Result := Angle;
 while Result <    0 do Result := Result + 360;
 while Result >= 360 do Result := Result - 360;
end;

function LimitAngle(const Angle: Double): Double;
begin
 Result := Angle;
 while Result <    0 do Result := Result + 360;
 while Result >= 360 do Result := Result - 360;
end;


{ Math }

function Factorial(const Order : Single): Single;
var
  i : Integer;
begin
 result := 1;
 for i := 2 to round(Order)
  do result := result * i;
end;

function Factorial(const Order : Double): Double;
var
  i : Integer;
begin
 result := 1;
 for i := 2 to round(Order)
  do result := result * i;
end;

function Factorial(const Order : Integer): Integer;
var
  i : Integer;
begin
 result := 1;
 for i := 2 to Order
  do result := result * i;
end;

function FastFractional(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
 result := Value - Round(Value - 0.5);
end;
{$ELSE}
var i : Integer;
asm
 fld Value.Single
 fld Value.Single
 fsub CHalf64
 frndint
 fsubp
end;
{$ENDIF}

function FastFractional(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
 result := Value - Round(Value - 0.5);
end;
{$ELSE}
var i : Integer;
asm
 fld Value.Double
 fld Value.Double
 fsub CHalf64
 frndint
 fsubp
end;
{$ENDIF}

function FastRandom: Single;
asm
 IMUL  EDX, RandSeed, 08088405H
 INC   EDX
 MOV   RandSeed, EDX
 FLD   CTwoMulTwo2Neg32
 PUSH  0
 PUSH  EDX
 FILD  qword ptr [ESP]
 ADD   ESP,8
 FMULP ST(1), ST(0)
 FLD1
 FSUBP
end;

function RandomGauss: Extended;
var
  U1, S2: Extended;
begin
  repeat
    U1 := 2 * Random - 1;
    S2 := Sqr(U1) + Sqr(2 * Random - 1);
  until S2 < 1;
  Result := Sqrt(CMinusOneSixteenth * Ln(S2) / S2) * U1;
end;

procedure FastAbs(var Value: Single);
var
  i : Integer absolute Value;
begin
 i := i and $7FFFFFFF;
end;

procedure FastAbs(var Value: Double);
var
  i : array [0..1] of Integer absolute Value;
begin
 i[0] := i[0] and $7FFFFFFF;
end;

procedure FastAbs(var Value: TDAV4SingleArray); overload;
{$IFDEF PUREPASCAL}
var
  i0 : Integer absolute Value[0];
  i1 : Integer absolute Value[1];
  i2 : Integer absolute Value[2];
  i3 : Integer absolute Value[3];
begin
 i0 := i0 and $7FFFFFFF;
 i1 := i1 and $7FFFFFFF;
 i2 := i2 and $7FFFFFFF;
 i3 := i3 and $7FFFFFFF;
end;
{$ELSE}
asm
 fld  [eax].Single
 fabs
 fstp [eax].Single
 fld  [eax +  4].Single
 fabs
 fstp [eax +  4].Single
 fld  [eax +  8].Single
 fabs
 fstp [eax +  8].Single
 fld  [eax + 12].Single
 fabs
 fstp [eax + 12].Single
end;
{$ENDIF}

procedure FastNegative(var Value: Single);
var
  i : Integer absolute Value;
const
  CBitMask = $80000000;
begin
 i := Cardinal((@Value)^) xor CBitMask;
end;

function FastMod(const Arg1, Arg2: Single): Single;
var
  Norm : Single;
begin
 Norm := Arg1 / Arg2;
 result := (Norm - round(Norm - 0.5)) * Arg2
end;

function FastTrunc(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 result := Round(Value - CHalf64);
end;
{$ELSE}
asm
 fld Value.Single
 fsub CHalf64
 fistp Result.Integer
end;
{$ENDIF}

function FastTrunc(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 result := Round(Value - CHalf64);
end;
{$ELSE}
asm
 fld Value.Double
 fsub CHalf64
 fistp Result.Integer
end;
{$ENDIF}

procedure FastTrunc(Input: PSingle; Output: PInteger; SampleFrames: Integer); overload;
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
end;
{$ELSE}
asm
 @Start:
 fld [eax].Single
 fsub CHalf64
 fistp [edx].Integer
 add eax,4
 add edx,4
 loop    @Start
end;
{$ENDIF}

function FastInt(Sample: Single): Single; overload;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample - 0.5);
end;
{$ELSE}
asm
 fld Sample.Single
 fsub CHalf64
 frndint
end;
{$ENDIF}

function FastInt(Sample: Double): Double; overload;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample - 0.5);
end;
{$ELSE}
asm
 fld Sample.Double
 fsub CHalf64
 frndint
end;
{$ENDIF}

function FastRound(Sample: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample);
end;
{$ELSE}
asm
 fld Sample.Single
 frndint
 fistp Result.Integer
end;
{$ENDIF}

function FastRound(Sample: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
 result := Round(Sample);
end;
{$ELSE}
asm
 fld Sample.Double
 frndint
 fistp Result.Integer
end;
{$ENDIF}

function FastSgn(const Value: Single): Integer;
var
  IntCast : Integer absolute Value;
begin
 Result := 1 - ((Intcast shr 31) shl 1);
end;

function Tanh(const X: Extended): Extended;
var
  ep : Extended;
begin
 ep := Exp(2 * X);
 Result := (ep - 1) / (ep + 1);
end;

function Tanh(const X: Double): Double;
var
  ep : Extended;
begin
 ep := Exp(2 * X);
 Result := (ep - 1) / (ep + 1);
end;

function Tanh(const X: Single): Single;
var
  ep : Extended;
begin
 ep := Exp(2 * X);
 Result := (ep - 1) / (ep + 1);
end;

procedure GetSinCos(const Frequency: Extended; var SinValue, CosValue : Extended);
{$IFDEF PUREPASCAL}
begin
 SinValue := Sin(Frequency);
 CosValue := Cos(Frequency);
end;
{$ELSE}
asm
  fld Frequency;
  fsincos
  fstp    tbyte ptr [edx]    // Cos
  fstp    tbyte ptr [eax]    // Sin
end;
{$ENDIF}

procedure GetSinCos(const Frequency: Double; var SinValue, CosValue : Double);
{$IFDEF PUREPASCAL}
begin
 SinValue := Sin(Frequency);
 CosValue := Cos(Frequency);
end;
{$ELSE}
asm
 fld Frequency.Double;
 fsincos
 fstp [CosValue].Double;
 fstp [SinValue].Double;
end;
{$ENDIF}

procedure GetSinCos(const Frequency: Single; var SinValue, CosValue : Single);
{$IFDEF PUREPASCAL}
begin
 SinValue := Sin(Frequency);
 CosValue := Cos(Frequency);
end;
{$ELSE}
asm
 fld Frequency;
 fsincos
 fstp [CosValue].Single;
 fstp [SinValue].Single;
end;
{$ENDIF}

function IsPowerOf2(const Value: Integer): Boolean;
begin
 result := abs(IntPower(2, round(Log2(Value))) - Value) < 1E-20;
end;

function RoundToPowerOf2(const Value: Integer): Integer;
begin
 Result := round(Log2(Value));
 Result := (Value shr (Result - 1)) shl (Result - 1);
end;

function TruncToPowerOf2(const Value: Integer): Integer;
begin
 result := 1;
 while result <= value do result := result shl 1;
 result := result shr 1;
end;

function ExtendToPowerOf2(const Value: Integer): Integer;
begin
 result := 1;
 while result < value do result := result shl 1;
end;

function TruncLog2(Value : Extended): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value));
end;
{$ELSE}
asm
 fld Value.Extended
 fxtract
 fstp st(0)
 fistp result.Integer
end;
{$ENDIF}

function TruncLog2(Value : Integer): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value));
end;
{$ELSE}
var
  temp : Integer;
asm
 mov temp, Value;
 fild temp.Integer
 fxtract
 fstp st(0)
 fistp result.Integer
end;
{$ENDIF}

function CeilLog2(Value : Extended): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value) + 1);
end;
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
end;
{$ENDIF}

function CeilLog2(Value : Integer): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value) + 1);
end;
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
end;
{$ENDIF}

function OnOff(const value: Single): Boolean;
begin Result := value > 0.5 end;

function UnDenormalize(const Value : Single) : Single;
begin
 if (abs(value) < 1.0e-20)
  then Result := 0.0
  else Result := value;
end;

{ String Functions }

{$IFNDEF FPC}
function GetApplicationFilename: string;
var
  s : array[0..$7FF] of AnsiChar;
begin
 GetModuleFilename(hInstance, s, SizeOf(s));
 Result := StrPas(s);
 Result := ExtractFilename(Result);
end;

function GetApplicationDirectory: string;
var
  s : array[0..$7FF] of AnsiChar;
begin
 GetModuleFilename(hInstance, s, SizeOf(s));
 Result := StrPas(s);
 Result := ExtractFileDir(Result);
end;

procedure Msg(b: Boolean);
begin if b then Msg('TRUE') else Msg('FALSE');end;
procedure Msg(m: string; m2: string = '');
begin MessageBox(0, PAnsiChar(m), PChar(m2), MB_OK); end;
procedure Msg(i: Integer);
begin Msg(IntToStr(i)); end;
procedure Msg(s: Single);
begin Msg(FloatToStrF(s, ffFixed, 3, 3)); end;
procedure Msg(m: string; i:Integer);
begin MessageBox(0, PAnsiChar(m + ' ' + IntToStr(i)), '', MB_OK); end;
{$WARNINGS ON}

function FloatWithUnit(const Value: Double): string;
begin
 if Value > 1    then result := FloatToStrF(Value, ffFixed, 6, 3)+ 's' else
 if Value > 1E-3 then result := FloatToStrF(1E3 * Value, ffFixed, 6, 3)+ 'ms' else
 if Value > 1E-6
  then result := FloatToStrF(1E6 * Value, ffFixed, 6, 3)+ 'µs'
  else result := FloatToStrF(1E9 * Value, ffFixed, 6, 3)+ 'ns'
end;

function SplitString(S: String; Delimiter: AnsiChar): TStrArray;
var
  C : Integer;
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
var
  i: Integer;
begin
 Result := '';
 for i := 1 to length(s) do
  if not (s[i] in ['*', '\', '/', '[', ']', '"', '|', '<', '>', '?', ':'])
   then Result := Result + s[i]
   else Result := Result + '-';
end;
{$ENDIF}

// SINC Function
function Sinc(const Input: Double): Double;
var
  pix : Double;
begin
 if (Input = 0)
  then result := 1
  else
   begin
    pix := PI * Input;
    result := sin(pix) / pix;
   end;
end;

function Sinc(const Input: Single): Single;
var
  pix : Double;
begin
 if (Input = 0)
  then result := 1
  else
   begin
    pix := PI * Input;
    result := sin(pix) / pix;
   end;
end;

function Sigmoid(const Input: Single): Single;
begin
 if (abs(Input) < 1)
  then Result := Input * (1.5 - 0.5 * Input * Input)
  else
   if Input < 0
    then Result := -1
    else Result :=  1;
end;

function Sigmoid(const Input: Double): Double;
begin
 if (abs(Input) < 1)
  then Result := Input * (1.5 - 0.5 * Input * Input)
  else
   if Input < 0
    then Result := -1
    else Result :=  1;
end;

{ Compatibility }

{$IFDEF DELPHI5}
function Sign(const AValue: Single): Single;
begin
 if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000)
  then Result := 0 else
 if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000)
  then Result := -1 else Result := 1;
end;

function Sign(const AValue: Double): Double;
begin
 if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000)
  then Result := 0 else
 if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000)
  then Result := -1 else Result := 1;
end;
{$ENDIF}

{$DEFINE PUREPASCAL}

function FastMin(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
 if A > B
  then result := B
  else result := A
end;
{$ELSE}
asm
 fld     DWORD PTR [EBP + $08]
 fld     DWORD PTR [EBP + $0C]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
end;
{$ENDIF}

function FastMax(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
 if A < B
  then result := B
  else result := A
end;
{$ELSE}
asm
 fld     DWORD PTR [EBP + $0C]
 fld     DWORD PTR [EBP + $08]
 fcomi   st(0), st(1)
 fcmovnb st(0), st(1)
 ffree   st(1)
end;
{$ENDIF}

procedure CalcMinMax(InBuffer: PSingle; Samples: Integer; var MinMax: TDAVMinMaxSingle);
var
  i : Integer;
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

procedure CalcMinMax(InBuffer: PDouble; Samples: Integer; var MinMax: TDAVMinMaxDouble);
var
  i : Integer;
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
end;
{$ELSE}
asm
 test EDX, EDX
 jz @End

 push EDX
 fldz                            // DC
 @CalcDCLoop:
   dec EDX
   fadd  [EAX + 4 * EDX].Single  // DC = DC + Value
 jnz @CalcDCLoop
 pop edx

 mov  [ESP - 4], EDX
 fild [ESP - 4].Integer          // Length, DC
 fdivp                           // RealDC = DC / Length

 @SubstractDCLoop:
   dec EDX
   fld  [EAX + 4 * edx].Single   // Value, RealDC
   fsub st(0), st(1)             // Value-RealDC, RealDC
   fstp  [EAX + 4 * edx].Single  // RealDC
 jnz @SubstractDCLoop
 fstp st(0)                      // clear stack

 @End:
end;
{$ENDIF}

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
end;
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
end;
{$ENDIF}

procedure ConvertSingleToDouble(Singles : PSingle; Doubles : PDouble; SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Doubles^ := Singles^;
   inc(Singles);
   inc(Doubles);
  end;
end;
{$ELSE}
asm
@MarioLand:
 fld  [eax + ecx * 4 - 4].Single
 fstp [edx + ecx * 8 - 8].Double
 loop @MarioLand
end;
{$ENDIF}

procedure ConvertDoubleToSingle(Doubles : PDouble; Singles : PSingle; SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 for i:=0 to SampleFrames-1 do
  begin
   Singles^:=Doubles^;
   inc(Singles);
   inc(Doubles);
  end;
end;
{$ELSE}
asm
@MarioLand:
 fld [eax + ecx * 8 - 8].Double
 fstp [edx + ecx * 4 - 4].Single
 loop @MarioLand
end;
{$ENDIF}

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
end;
{$ENDIF}

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
end;
{$ENDIF}

procedure InitConstants;
begin
 ln2    := ln(2);
 ln22   := ln2 * 0.5;
 ln2Rez := 1 / ln2;
 ln10   := ln(10);
end;

initialization
 InitConstants;

end.
