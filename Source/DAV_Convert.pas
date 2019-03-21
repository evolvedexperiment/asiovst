unit DAV_Convert;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types;

{ Convert }

function ms2Samples(const ms, SampleRate: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Samples2ms(const Samples, SampleRate: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Sync2Samples(const SyncFactor, BPM, SampleRate: Single): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function GetSyncFactor(const BaseFactor: Single; const Dotted, Triads: Boolean): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function FrequencyToBark(Frequency: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FrequencyToBark(Frequency: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Frequency2CriticalBandwidth(Frequency: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Frequency2CriticalBandwidth(Frequency: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

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

function ScaleLinearToLog(const Value: Single; const Min, Max: Single): Single; overload;
function ScaleLinearToLog(const Value: Double; const Min, Max: Double): Single; overload;
function ScaleLogToLinear(const Value: Single; const Min, Max: Single): Single; overload;
function ScaleLogToLinear(const Value: Double; const Min, Max: Double): Double; overload;


implementation

uses
  Math, DAV_Consts;

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
  if Dotted then
    Result := Result * 1.5;
  if Triads then
    Result := Result / 3;
end;


////////////////////////////////////////////////////////////////////////
//                                                                    //
// see for example "Zwicker: Psychoakustik, 1982; ISBN 3-540-11401-7  //
//                                                                    //
// input: freq in hz                                                  //
// output: barks                                                      //
//                                                                    //
////////////////////////////////////////////////////////////////////////

function FrequencyToBark(Frequency: Single): Single;
begin
  if (Frequency < 0) then
    Frequency := 0;
  Frequency := Frequency * 0.001;
  Result := 13.0 * ArcTan(0.76 * Frequency) +
    3.5 * ArcTan(Sqr(Frequency * 0.1333333333333333));
end;

function FrequencyToBark(Frequency: Double): Double;
begin
  if (Frequency < 0) then
    Frequency := 0;
  Frequency := Frequency * 0.001;
  Result := 13.0 * ArcTan(0.76 * Frequency) +
    3.5 * ArcTan(Sqr(Frequency / 7.5));
end;


////////////////////////////////////////////////////////////////////////
//                                                                    //
// see for example "Zwicker: Psychoakustik, 1982; ISBN 3-540-11401-7  //
//                                                                    //
// input: freq in hz                                                  //
// output: critical band width                                        //
//                                                                    //
////////////////////////////////////////////////////////////////////////

function Frequency2CriticalBandwidth(Frequency: Single): Single;
begin
  Result := 25 + 75 * Power(1 + 1.4 * Sqr(Frequency * 0.001), 0.69);
end;

function Frequency2CriticalBandwidth(Frequency: Double): Double;
begin
  Result := 25 + 75 * Power(1 + 1.4 * Sqr(Frequency * 0.001), 0.69);
end;


////////////////////////////////////////////////////
//                                                //
// Convert a value in dB's to a linear amplitude  //
//                                                //
////////////////////////////////////////////////////

function dB_to_Amp(const Value: Single): Single;
begin
  if (Value > -400.0) then
    Result := Exp(Value * 0.11512925464970228420089957273422) //Power(10, g / 20) //Power(2, g * 0.015051499783199059760686944736225)
  else
    Result := 0;
end;

function dB_to_Amp(const Value: Double): Double;
begin
  if (Value > -1000.0) then
    Result := Exp(Value * 0.11512925464970228420089957273422) //Power(10, g / 20) //Power(2, g * 0.015051499783199059760686944736225)
  else
    Result := 0;
end;                                                             // e^(x) = 2^(log2(e^x)) = 2^(x / ln(2))


///////////////////////////////////////////////////////////
//                                                       //
// Convert a squared value in dB's to a linear amplitude //
//                                                       //
///////////////////////////////////////////////////////////

{$IFDEF CPUx86_64}
{$DEFINE PUREPASCAL}
{$ENDIF}

function SqrAmp2dB(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
  Result := 10 * Log10(Value);
{$ELSE}
asm
    FLDLG2
    FLD     Value
    FYL2X
    FMUL    CTen32
{$ENDIF}
end;

function SqrAmp2dB(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
  Result := 10 * Log10(Value);
{$ELSE}
asm
    FLDLG2
    FLD     Value
    FYL2X
    FMUL    CTen64
{$ENDIF}
end;

function Amp_to_dB(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
  Result := CTwenty32 * Log10(Value);
{$ELSE}
asm
    FLDLG2
    FLD Value
    FYL2X
    FMUL CTwenty32
{$ENDIF}
end;

function Amp_to_dB(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
  Result := CTwenty64 * Log10(Value);
{$ELSE}
asm
    FLDLG2
    FLD Value
    FYL2X
    FMUL CTwenty64
{$ENDIF}
end;

procedure Amp_to_dB(var v: TDAV4SingleArray);
{$IFDEF PUREPASCAL}
begin
  v[0] := Amp_to_dB(v[0]);
  v[1] := Amp_to_dB(v[1]);
  v[2] := Amp_to_dB(v[2]);
  v[3] := Amp_to_dB(v[3]);
{$ELSE}
asm
    FLDLG2
    FLD    [EAX].Single
    FYL2X
    FMUL   CTwenty32.Double
    FSTP   [EAX].Single
    FLDLG2
    FLD    [EAX + 4].Single
    FYL2X
    FMUL   CTwenty32.Double
    FSTP   [EAX + 4].Single
    FLDLG2
    FLD    [EAX + 8].Single
    FYL2X
    FMUL   CTwenty32.Double
    FSTP   [EAX + 8].Single
    FLDLG2
    FLD    [EAX + 12].Single
    FYL2X
    FMUL   CTwenty32.Double
    FSTP   [EAX + 12].Single
{$ENDIF}
end;


//////////////////////////////////////////////
//                                          //
// scale logarithmicly from 20 Hz to 20 kHz //
//                                          //
//////////////////////////////////////////////

function FreqLinearToLog(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
  Result := CTwenty32 * Exp(value * 6.907755279);
{$ELSE}
const
  fltl2: Single = 6.907755279;
asm
    FLD     Value.Single
    FMUL    fltl2
    FLDL2E
    FMUL
    FLD     ST(0)
    FRNDINT
    FSUB    ST(1), ST
    FXCH    ST(1)
    F2XM1
    FLD1
    FADD
    FSCALE
    FSTP    ST(1)
    FMUL    CTwenty64.Double
{$ENDIF}
end;

function FreqLinearToLog(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
//  Result := 20 * Exp(Value * ln(20000 / 20));
  Result := CTwenty64 * Exp(Value * 6.907755279);
{$ELSE}
const
  fltl2: Double = 6.907755279;
asm
    FLD     Value.Double
    FMUL    fltl2
    FLDL2E
    FMUL
    FLD     ST(0)
    FRNDINT
    FSUB    ST(1), ST
    FXCH    ST(1)
    F2XM1
    FLD1
    FADD
    FSCALE
    FSTP    ST(1)
    FMUL    CTwenty64.Double
{$ENDIF}
end;

function FreqLogToLinear(const Value: Single): Single;
const
  fltl1 : Single = 0.05;
  fltl2 : Single = 1.44764826019E-1;
{$IFDEF PUREPASCAL}
begin
//  Result := Ln(Value / 20) / ln(20000 / 20);
  Result := Ln(Value * fltl1) * fltl2;
{$ELSE}
asm
    FLDLN2
    FLD     Value.Single
    FMUL    fltl1
    FYL2X
    FMUL    fltl2
{$ENDIF}
end;

function FreqLogToLinear(const Value: Double): Double;
const
  fltl1 : Double = 0.05;
  fltl2 : Double = 1.44764826019E-1;
{$IFDEF PUREPASCAL}
begin
  Result := Ln(Value * fltl1) * fltl2;
{$ELSE}
asm
    FLDLN2
    FLD     Value.Double
    FMUL    fltl1
    FYL2X
    FMUL    fltl2
{$ENDIF}
end;

function ScaleLinearToLog(const Value: Single; const Min, Max: Single): Single;
begin
  Result := Min * Exp(Value * ln(Max / Min));
end;

function ScaleLinearToLog(const Value: Double; const Min, Max: Double): Single; overload;
begin
  Result := Min * Exp(Value * ln(Max / Min));
end;

function ScaleLogToLinear(const Value: Single; const Min, Max: Single): Single; overload;
begin
  Result := ln(Value / Min) / ln(Max / Min);
end;

function ScaleLogToLinear(const Value: Double; const Min, Max: Double): Double; overload;
begin
  Result := ln(Value / Min - Max / Min);
end;


end.
