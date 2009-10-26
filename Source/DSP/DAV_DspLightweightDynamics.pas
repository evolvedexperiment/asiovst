unit DAV_DspLightweightDynamics;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Classes, DAV_DspDynamics;

type
  //////////////////////////////////////////////////////////////////////////////
  //                                                                          //
  //  TLightweightSoftKneeLimiter                                             //
  //  ---------------------------                                             //
  //                                                                          //
  //  Lightweight soft knee limiter that uses approximations to obtain a      //
  //  controllable knee [in dB] around a given threshold.                     //
  //                                                                          //
  //////////////////////////////////////////////////////////////////////////////

  TLightweightSoftKneeLimiter = class(TCustomKneeLimiter)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateLogScaledThresholdValue;
  protected
    FThrshlddB  : Single;
    FKneeFactor : Single;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeFeedbackLikeLimiter = class(TCustomKneeLimiter)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateTimeFactors;
  protected
    FThrshlddB          : Single;
    FKneeFactor         : Single;
    FAttackSampleCycle  : Single;
    FReleaseSampleCycle : Single;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
    procedure Reset; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
  protected
    FRatioFactor : Single;
    FThrshlddB   : Single;
    FKneeFactor  : Single;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeUpwardCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
  protected
    FRatioFactor : Single;
    FThrshlddB   : Single;
    FKneeFactor  : Single;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeFeedbackCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
  protected
    FRatioFactor   : Single;
    FThrshlddB     : Single;
    FKneeFactor    : Single;
    FPrevAbsSample : Double;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

  TLightweightSoftKneeFeedbackLikeCompressor = class(TCustomKneeCompressor)
  private
    procedure CalculateKneeFactor;
    procedure CalculateAutoMakeUpGain;
    procedure CalculateTimeFactors;
  protected
    FRatioFactor        : Single;
    FThrshlddB          : Single;
    FKneeFactor         : Single;
    FAttackSampleCycle  : Single;
    FReleaseSampleCycle : Single;
    procedure RatioChanged; override;
    procedure KneeChanged; override;
    procedure ThresholdChanged; override;
    procedure AutoMakeUpChanged; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateReleaseFactor; override;
    procedure Reset; override;
  public
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; override;
    procedure InputSample(const Input: Double); override;
    function GainSample(const Input: Double): Double; override;
  published
    property MakeUpGain_dB;
    property Knee_dB;
  end;

implementation

uses
  SysUtils, DAV_Approximations;

resourcestring
  RCStrAttackTimeInvalid = 'Attack time must be larger than zero!';
  RCStrReleaseTimeInvalid = 'Release time must be larger than zero!';

const
  CSoftKnee : array [0..7] of Single = (-8.21343513178931783E-2,
    6.49732456739820052E-1, -2.13417801862571777, 4.08642207062728868,
    -1.51984215742349793, 5.48668824216034384E-2, 2.42162975514835621E-1,
    6.93292707161004662E-1);
  CDenorm32  : Single = 1E-24;
  CHalf32    : Single = 0.5;
  CQuarter32 : Single = 0.25;

{ TLightweightSoftKneeLimiter }

procedure TLightweightSoftKneeLimiter.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeLimiter.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeLimiter.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * -CHalf32;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp + CHalf32 * FKnee_dB;
 FMakeUpGain := FastdBtoAmpMinError3(FMakeUpGain_dB);
end;

procedure TLightweightSoftKneeLimiter.CalculateKneeFactor;
begin
 FKneeFactor := sqr(2 * CdBtoAmpExpGain32 * FKnee_dB);
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeLimiter.ThresholdChanged;
begin
 inherited;
 CalculateLogScaledThresholdValue;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeLimiter.CalculateLogScaledThresholdValue;
begin
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
end;

function TLightweightSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := FastLog2ContinousError5(result) - FThrshlddB;
 result := FastPower2MinError3(-CHalf32 * (result + FastSqrtBab2(sqr(result) + FKneeFactor)));
end;

function TLightweightSoftKneeLimiter.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := -CHalf32 * (InputLevel_dB - FThreshold_dB);
 result := Temp - FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

constructor TLightweightSoftKneeLimiter.Create;
begin
 inherited;
 CalculateLogScaledThresholdValue;
end;

function TLightweightSoftKneeLimiter.GainSample(const Input: Double): Double;
begin
 result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeLimiter.InputSample(const Input: Double);
{$IFDEF XPUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FThrshlddB - FastLog2ContinousError5(FPeak);
 FGain := FastPower2MinError3(CHalf32 * (Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   Input
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double     // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsub  st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CSoftKnee        ].Single
 fadd  [CSoftKnee + 4    ].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsubr  [edx.FThrshlddB]       // Stack : Temp

 // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));
 fld   st(0)                   // Stack : Temp, Temp
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
 fld   st(0)                   // Stack : Intemp, Intemp, Temp
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp
 faddp                         // Stack: Intemp / newresult + CQuarter32 * newresult, Temp

 fsubp                         // Stack: Temp + SqrtTemp
 fmul  CHalf32                 // Stack: CHalf32 * (FMkpdB - (Temp + SqrtTemp))

 fld   st(0)                   // Stack: temp, temp
 frndint                       // Stack: round(temp), temp

 fist  IntCast                 // Stack: round(temp), temp
 fsubp                         // Stack: newtemp = temp - round(temp)

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fmul  CastedSingle

 fstp [edx.FGain]
end;
{$ENDIF}

function TLightweightSoftKneeLimiter.ProcessSample64(Input: Double): Double;
{$IFDEF XPUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FThrshlddB - FastLog2ContinousError5(FPeak);
 FGain := FastPower2MinError3(CHalf32 * (Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));

 result := FGain * FMakeUpGain * Input;
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   Input
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double     // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsub  st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CSoftKnee        ].Single
 fadd  [CSoftKnee + 4    ].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsubr  [edx.FThrshlddB]       // Stack : Temp

 // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));
 fld   st(0)                   // Stack : Temp, Temp
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
 fld   st(0)                   // Stack : Intemp, Intemp, Temp
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp
 faddp                         // Stack: Intemp / newresult + CQuarter32 * newresult, Temp

 fsubp                         // Stack: Temp + SqrtTemp
 fmul  CHalf32                 // Stack: CHalf32 * (FMkpdB - (Temp + SqrtTemp))

 fld   st(0)                   // Stack: temp, temp
 frndint                       // Stack: round(temp), temp

 fist  IntCast                 // Stack: round(temp), temp
 fsubp                         // Stack: newtemp = temp - round(temp)

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fmul  CastedSingle

 fst  [edx.FGain]
 fmul Input
 fmul [edx.FMakeUpGain]
end;
{$ENDIF}

{ TLightweightSoftKneeFeedbackLikeLimiter }

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateAttackFactor;
begin
 if FAttack <= 0
  then raise Exception.Create('Attack time must be larger than zero!')
  else
   begin
    FAttackSampleCycle := -1 / (FAttack * 0.001 * SampleRate);
    FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateReleaseFactor;
begin
 if FRelease <= 0
  then raise Exception.Create('Release time must be larger than zero!')
  else
   begin
    FReleaseSampleCycle := -1 / (FRelease * 0.001 * SampleRate);
    FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.Reset;
begin
 inherited;
 CalculateTimeFactors;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateTimeFactors;
begin
 FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else //CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := FThreshold_dB * CHalf32;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp;
// CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.CalculateKneeFactor;
begin
 FKneeFactor := sqr(CdBtoAmpExpGain32 * FKnee_dB);
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.ThresholdChanged;
begin
 inherited;
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TLightweightSoftKneeFeedbackLikeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := CHalf32 * (FThrshlddB - FastLog2ContinousError5(result));
 result := FastPower2MinError3(result - FastSqrtBab2(sqr(result) + FKneeFactor));
end;

function TLightweightSoftKneeFeedbackLikeLimiter.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := CHalf32 * (FThreshold_dB - InputLevel_dB);
 result := Temp - FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeFeedbackLikeLimiter.GainSample(const Input: Double): Double;
begin
 result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeFeedbackLikeLimiter.InputSample(const Input: Double);
{$IFNDEF XPUREPASCAL}
var
  Temp  : array [0..1] of Single;
begin
 Temp[0] := CDenorm32 + abs(Input);

 if Temp[0] > FPeak
  then FPeak := FPeak + (Temp[0] - FPeak) * FAttackFactor
  else FPeak := Temp[0] + (FPeak - Temp[0]) * FReleaseFactor;

 Temp[0] := -CHalf32 * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 Temp[1] := FastSqrtBab2(sqr(Temp[0]) + FKneeFactor);
 FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
// Temp[1] := 2 * Temp[1] / (2 * Temp[1] - 1E10 * (1 / 1E10 - 1) * Temp[1] - Ratio * (1 / FRatio - 1) * Temp[0]);
 Temp[1] := 2 * Temp[1] / ((1E10 + 1) * Temp[1] + (1E10 - 1) * Temp[0]);
 FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   Input
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double     // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsub  st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CSoftKnee        ].Single
 fadd  [CSoftKnee + 4    ].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsub  [edx.FThrshlddB]       // Stack : Temp
 fmul  [edx.FRatioFactor]     // Stack : Temp[0]

 // Temp[1] := FastSqrtBab2(sqr(Temp[0]) + FKneeFactor);
 fld   st(0)                   // Stack : Temp[0], Temp[0]
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp[0] * Temp[0] + FKneeFactor, Temp[0]
 fld   st(0)                   // Stack : Intemp, Intemp, Temp[0]
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp[0]

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp[0]
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp[0]
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp[0]
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp[0]
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp[0]
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp[0]
 faddp                         // Stack: Temp[1] := Intemp / newresult + CQuarter32 * newresult, Temp[0]

 // FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
 fld   st(0)                   // Stack: Temp[1], Temp[1], Temp[0]
 fsubr st(0), st(2)            // Stack: Temp[0] - Temp[1], Temp[1], Temp[0]

 fld   st(0)                   // Stack: Temp[0] - Temp[1], Temp[0] - Temp[1], Temp[1], Temp[0]
 frndint                       // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]

 fist  IntCast                 // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]
 fsubp                         // Stack: newtemp = (Temp[0] - Temp[1]) - round(Temp[0] - Temp[1]), Temp[1], Temp[0]

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp, Temp[1], Temp[0]
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[1], Temp[0]
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[1], Temp[0]
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
 fmul  CastedSingle               // NewGain, Temp[1], Temp[0]
 fstp  [edx.FGain]

 // Temp[1] := 2 * Temp[1] / ((FRatio + 1) * Temp[1] + (FRatio - 1) * Temp[0]);
 fld1                             // 1, Temp[1], Temp[0]
 fadd  [edx.FRatio]               // Ratio + 1, Temp[1], Temp[0]
 fmul  st(0), st(1)               // Temp[1] * (Ratio + 1), Temp[1], Temp[0]
 fxch  st(2)                      // Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 fld1                             // 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 fsubr [edx.FRatio]               // Ratio - 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 fmulp                            // (Ratio - 1) * Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 faddp st(2), st(0)               // Temp[1], (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1),
 fdivrp                           // Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)
 fadd  st(0), st(0)               // 2 * Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)

 // FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
 fld   st(0)                      // Temp[1], Temp[1]
 fmul  [edx.FAttackSampleCycle]   // Temp[0], Temp[1]

 fld   st(0)                      // Stack: Temp[0], Temp[0], Temp[1]
 frndint                          // Stack: round(Temp[0]), Temp[0], Temp[1]

 fist  IntCast                    // Stack: round(Temp[0]), Temp[0], Temp[1]
 fsubp                            // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0], Temp[1]

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp, Temp[0], Temp[1]
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0], Temp[1]
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0], Temp[1]
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
 fmul  CastedSingle               // NewAttackFactor, Temp[1]
 fld1                             // 1, NewAttackFactor, Temp[1]
 fsubrp                           // 1 - NewAttackFactor, Temp[1]
 fstp  [edx.FAttackFactor]

 // FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
 fmul  [edx.FReleaseSampleCycle]  // Temp[0]

 fld   st(0)                      // Stack: Temp[0], Temp[0]
 frndint                          // Stack: round(Temp[0]), Temp[0]

 fist  IntCast                    // Stack: round(Temp[0]), Temp[0]
 fsubp                            // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0]

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp, Temp[0]
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0]
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0]
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
 fmul  CastedSingle               // NewReleaseFactor
 fstp  [edx.FReleaseFactor]
end;
{$ENDIF}

function TLightweightSoftKneeFeedbackLikeLimiter.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 result := FGain * FMakeUpGain * Input;
end;

{ TLightweightSoftKneeCompressor }

procedure TLightweightSoftKneeCompressor.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3( -1 / (FAttack * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeCompressor.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeCompressor.RatioChanged;
begin
 inherited;
 FRatioFactor := CHalf32 * (1 / Ratio - 1);
end;

procedure TLightweightSoftKneeCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeCompressor.CalculateKneeFactor;
begin
 FKneeFactor := sqr(CdBtoAmpExpGain32 * FKnee_dB);
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeCompressor.ThresholdChanged;
begin
 inherited;
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TLightweightSoftKneeCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := FRatioFactor * (FastLog2ContinousError5(result) - FThrshlddB);
 result := FastPower2MinError3(result - FastSqrtBab2(sqr(result) + FKneeFactor));
end;

function TLightweightSoftKneeCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor * (InputLevel_dB - FThreshold_dB);
 result := Temp - FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeCompressor.GainSample(const Input: Double): Double;
begin
 result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeCompressor.InputSample(const Input: Double);
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FRatioFactor * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 FGain := FastPower2MinError3(Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor));
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   Input
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double     // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsub  st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CSoftKnee        ].Single
 fadd  [CSoftKnee + 4    ].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsub  [edx.FThrshlddB]       // Stack : Temp
 fmul  [edx.FRatioFactor]

 // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));
 fld   st(0)                   // Stack : Temp, Temp
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
 fld   st(0)                   // Stack : Intemp, Intemp, Temp
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp
 faddp                         // Stack: Intemp / newresult + CQuarter32 * newresult, Temp

 fsubp                         // Stack: Temp + SqrtTemp

 fld   st(0)                   // Stack: temp, temp
 frndint                       // Stack: round(temp), temp

 fist  IntCast                 // Stack: round(temp), temp
 fsubp                         // Stack: newtemp = temp - round(temp)

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fmul  CastedSingle

 fstp [edx.FGain]
end;
{$ENDIF}

function TLightweightSoftKneeCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 Result := FGain * FMakeUpGain * Input;
end;


{ TLightweightSoftKneeUpwardCompressor }

procedure TLightweightSoftKneeUpwardCompressor.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3( -1 / (FAttack * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate));
end;

procedure TLightweightSoftKneeUpwardCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeUpwardCompressor.RatioChanged;
begin
 inherited;
 FRatioFactor := 1 - Ratio;
end;

procedure TLightweightSoftKneeUpwardCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeUpwardCompressor.CalculateKneeFactor;
begin
 FKneeFactor := sqr(CdBtoAmpExpGain32 * FKnee_dB);
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeUpwardCompressor.ThresholdChanged;
begin
 inherited;
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TLightweightSoftKneeUpwardCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := FRatioFactor * (FThrshlddB - FastLog2ContinousError5(result));
 result := FastPower2MinError3(result + FastSqrtBab2(sqr(result) + FKneeFactor));
end;

function TLightweightSoftKneeUpwardCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor * (FThreshold_dB - InputLevel_dB);
 result := Temp + FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeUpwardCompressor.GainSample(const Input: Double): Double;
begin
 result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeUpwardCompressor.InputSample(const Input: Double);
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + abs(Input);

 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FRatioFactor * (FThrshlddB - FastLog2ContinousError5(FPeak));
 FGain := FastPower2MinError3(Temp + FastSqrtBab2(sqr(Temp) + FKneeFactor));
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   Input
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double     // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsub  st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CSoftKnee        ].Single
 fadd  [CSoftKnee + 4    ].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsubr [edx.FThrshlddB]       // Stack : Temp
 fmul  [edx.FRatioFactor]

 // FGain := FastPower2MinError3(Temp + FastSqrtBab2(sqr(Temp) + FKneeFactor)));
 fld   st(0)                   // Stack : Temp, Temp
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
 fld   st(0)                   // Stack : Intemp, Intemp, Temp
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp
 faddp                         // Stack: Intemp / newresult + CQuarter32 * newresult, Temp

 faddp                         // Stack: Temp + SqrtTemp

 fld   st(0)                   // Stack: temp, temp
 frndint                       // Stack: round(temp), temp

 fist  IntCast                 // Stack: round(temp), temp
 fsubp                         // Stack: newtemp = temp - round(temp)

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fmul  CastedSingle

 fstp [edx.FGain]
end;
{$ENDIF}

function TLightweightSoftKneeUpwardCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 result := FGain * FMakeUpGain * Input;
end;


{ TLightweightSoftKneeFeedbackCompressor }

procedure TLightweightSoftKneeFeedbackCompressor.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate * FRatio));
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate * FRatio));
end;

procedure TLightweightSoftKneeFeedbackCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeFeedbackCompressor.RatioChanged;
begin
 inherited;
 CalculateAttackFactor;
 CalculateReleaseFactor;
 FRatioFactor := CHalf32 * (1 - Ratio);
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeFeedbackCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackCompressor.CalculateKneeFactor;
begin
 FKneeFactor := sqr(CdBtoAmpExpGain32 * FKnee_dB) * Ratio;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackCompressor.ThresholdChanged;
begin
 inherited;
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TLightweightSoftKneeFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := FRatioFactor * (FastLog2ContinousError5(result) - FThrshlddB);
 result := FastPower2MinError3(result - FastSqrtBab2(sqr(result) + FKneeFactor));
end;

function TLightweightSoftKneeFeedbackCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor/Ratio * (InputLevel_dB - FThreshold_dB);
 result := Temp - FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeFeedbackCompressor.GainSample(const Input: Double): Double;
begin
 result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeFeedbackCompressor.InputSample(const Input: Double);
{$IFNDEF XPUREPASCAL}
var
  Temp : Single;
begin
 Temp := CDenorm32 + FPrevAbsSample;

(*

 if FPeak < FThreshold
  then FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate))
  else FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate * FRatio));

 if FPeak < FThreshold
  then FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate))
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate * FRatio));
*)


 if Temp > FPeak
  then FPeak := FPeak + (Temp - FPeak) * FAttackFactor
  else FPeak := Temp + (FPeak - Temp) * FReleaseFactor;

 Temp  := FRatioFactor * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 FGain := FastPower2MinError3(Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor));

 FPrevAbsSample := abs(FGain * Input);
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   [edx.FPrevAbsSample]
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double     // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsub  st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CSoftKnee        ].Single
 fadd  [CSoftKnee + 4    ].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsub  [edx.FThrshlddB]       // Stack : Temp
 fmul  [edx.FRatioFactor]

 // FGain := FastPower2MinError3(CHalf32 * (FMkpdB - Temp - FastSqrtBab2(sqr(Temp) + FKneeFactor)));
 fld   st(0)                   // Stack : Temp, Temp
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp * Temp + FKneeFactor, Temp
 fld   st(0)                   // Stack : Intemp, Intemp, Temp
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp
 faddp                         // Stack: Intemp / newresult + CQuarter32 * newresult, Temp

 fsubp                         // Stack: Temp + SqrtTemp

 fld   st(0)                   // Stack: temp, temp
 frndint                       // Stack: round(temp), temp

 fist  IntCast                 // Stack: round(temp), temp
 fsubp                         // Stack: newtemp = temp - round(temp)

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)))
 fmul  CastedSingle

 fst  [edx.FGain]
 fmul Input
 fstp [edx.FPrevAbsSample]
end;
{$ENDIF}

function TLightweightSoftKneeFeedbackCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 result := FGain * FMakeUpGain * Input;
end;


{ TLightweightSoftKneeFeedbackLikeCompressor }

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateAttackFactor;
begin
 if FAttack <= 0
  then raise Exception.Create(RCStrAttackTimeInvalid)
  else
   begin
    FAttackSampleCycle := -1 / (FAttack * 0.001 * SampleRate);
    FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateReleaseFactor;
begin
 if FRelease <= 0
  then raise Exception.Create(RCStrReleaseTimeInvalid)
  else
   begin
    FReleaseSampleCycle := -1 / (FRelease * 0.001 * SampleRate);
    FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
   end;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.KneeChanged;
begin
 inherited;
 CalculateKneeFactor;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.RatioChanged;
begin
 inherited;
 FRatioFactor := CHalf32 * (1 / Ratio - 1);
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.Reset;
begin
 inherited;
 CalculateTimeFactors;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateTimeFactors;
begin
 FAttackFactor := 1 - FastPower2MinError3(FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(FReleaseSampleCycle);
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.AutoMakeUpChanged;
begin
 if AutoMakeUp
  then CalculateAutoMakeUpGain
  else CalculateMakeUpGain;
 Changed;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateAutoMakeUpGain;
var
  Temp: Single;
begin
 Temp := -FThreshold_dB * FRatioFactor;
 FMakeUpGain_dB := FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) - Temp;
 CalculateMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.CalculateKneeFactor;
begin
 FKneeFactor := sqr(CdBtoAmpExpGain32 * FKnee_dB);
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.ThresholdChanged;
begin
 inherited;
 FThrshlddB := Threshold_dB * CdBtoAmpExpGain32;
 if AutoMakeUp
  then CalculateAutoMakeUpGain;
end;

function TLightweightSoftKneeFeedbackLikeCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 result := PeakLevel;
 result := FRatioFactor * (FastLog2ContinousError5(result) - FThrshlddB);
 result := FastPower2MinError3(result - FastSqrtBab2(sqr(result) + FKneeFactor));
end;

function TLightweightSoftKneeFeedbackLikeCompressor.CharacteristicCurve_dB(
  const InputLevel_dB: Double): Double;
var
  Temp: Single;
begin
 Temp   := FRatioFactor * (InputLevel_dB - FThreshold_dB);
 result := Temp - FastSqrtBab2(sqr(Temp) + sqr(FKnee_dB)) + MakeUpGain_dB + InputLevel_dB;
end;

function TLightweightSoftKneeFeedbackLikeCompressor.GainSample(const Input: Double): Double;
begin
 result := FGain * FMakeUpGain * Input;
end;

procedure TLightweightSoftKneeFeedbackLikeCompressor.InputSample(const Input: Double);
{$IFDEF PUREPASCAL}
var
  Temp  : array [0..1] of Single;
begin
 Temp[0] := CDenorm32 + abs(Input);

 if Temp[0] > FPeak
  then FPeak := FPeak + (Temp[0] - FPeak) * FAttackFactor
  else FPeak := Temp[0] + (FPeak - Temp[0]) * FReleaseFactor;

 Temp[0] := FRatioFactor * (FastLog2ContinousError5(FPeak) - FThrshlddB);
 Temp[1] := FastSqrtBab2(sqr(Temp[0]) + FKneeFactor);
 FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
 Temp[1] := 2 * Temp[1] / ((FRatio + 1) * Temp[1] + (FRatio - 1) * Temp[0]);
 FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
 FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
end;
{$ELSE}
var
  CastedSingle : Single;
  IntCast      : Integer absolute CastedSingle;
asm
 // Temp := CDenorm32 + abs(Input);
 mov   edx, eax               // edx = Self
 fld   Input
 fabs
 fadd  CDenorm32              // Stack: temp

 fcom  [edx.FPeak].Double     // Stack: temp
 fstsw ax
 sahf
 jbe   @Release
@Attack:
 fsub  [edx.FPeak]
 fmul  [edx.FAttackFactor]
 fadd  [edx.FPeak]
 fst   [edx.FPeak]
 jmp   @AmpTodB
@Release:
 fld   [edx.FPeak]            // Stack: FPeak, temp
 fsub  st(0), st(1)           // Stack: (FPeak - temp), temp
 fmul  [edx.FReleaseFactor]   // Stack: (FPeak - temp) * FReleaseFactor, temp
 faddp                        // Stack: (FPeak - temp) * FReleaseFactor + temp
 fst   [edx.FPeak]

@AmpTodB:
 fstp  IntCast                // Stack: (empty)
 mov   eax, IntCast
 mov   ecx, eax               // copy eax to ecx
 and   eax, $807fffff
 add   eax, $3f800000
 mov   IntCast, eax
 fld   CastedSingle
 fmul  [CSoftKnee        ].Single
 fadd  [CSoftKnee + 4    ].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 2].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 3].Single
 fmul  CastedSingle
 fadd  [CSoftKnee + 4 * 4].Single

 shr   ecx, $17
 and   ecx, $000000ff
 sub   ecx, $00000080
 mov   IntCast, ecx
 fild  IntCast
 faddp


 fsub  [edx.FThrshlddB]       // Stack : Temp
 fmul  [edx.FRatioFactor]     // Stack : Temp[0]

 // Temp[1] := FastSqrtBab2(sqr(Temp[0]) + FKneeFactor);
 fld   st(0)                   // Stack : Temp[0], Temp[0]
 fmul  st(0), st(0)
 fadd  [edx.FKneeFactor]       // Stack : Temp[0] * Temp[0] + FKneeFactor, Temp[0]
 fld   st(0)                   // Stack : Intemp, Intemp, Temp[0]
 fst   CastedSingle            // Stack : Intemp, Intemp, Temp[0]

 mov   eax, IntCast
 sub   eax, $00800000
 shr   eax, 1
 add   eax, $20000000
 mov   IntCast, eax
 fdiv  CastedSingle            // Stack: Intemp / CastedSingle, Intemp, Temp[0]
 fadd  CastedSingle            // Stack: newresult = CastedSingle + Intemp / CastedSingle, Intemp, Temp[0]
 fld   st(0)                   // Stack: newresult, newresult, Intemp, Temp[0]
 fmul  CQuarter32              // Stack: CQuarter32 * newresult, newresult, Intemp, Temp[0]
 fxch                          // Stack: newresult, CQuarter32 * newresult, Intemp, Temp[0]
 fdivp st(2), st(0)            // Stack: Intemp / newresult, CQuarter32 * newresult, Temp[0]
 faddp                         // Stack: Temp[1] := Intemp / newresult + CQuarter32 * newresult, Temp[0]

 // FGain   := FastPower2MinError3(Temp[0] - Temp[1]);
 fld   st(0)                   // Stack: Temp[1], Temp[1], Temp[0]
 fsubr st(0), st(2)            // Stack: Temp[0] - Temp[1], Temp[1], Temp[0]

 fld   st(0)                   // Stack: Temp[0] - Temp[1], Temp[0] - Temp[1], Temp[1], Temp[0]
 frndint                       // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]

 fist  IntCast                 // Stack: round(Temp[0] - Temp[1]), Temp[0] - Temp[1], Temp[1], Temp[0]
 fsubp                         // Stack: newtemp = (Temp[0] - Temp[1]) - round(Temp[0] - Temp[1]), Temp[1], Temp[0]

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp, Temp[1], Temp[0]
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[1], Temp[0]
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[1], Temp[0]
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[1], Temp[0]
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[1], Temp[0]
 fmul  CastedSingle               // NewGain, Temp[1], Temp[0]
 fstp  [edx.FGain]

 // Temp[1] := 2 * Temp[1] / ((FRatio + 1) * Temp[1] + (FRatio - 1) * Temp[0]);
 fld1                             // 1, Temp[1], Temp[0]
 fadd  [edx.FRatio]               // Ratio + 1, Temp[1], Temp[0]
 fmul  st(0), st(1)               // Temp[1] * (Ratio + 1), Temp[1], Temp[0]
 fxch  st(2)                      // Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 fld1                             // 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 fsubr [edx.FRatio]               // Ratio - 1, Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 fmulp                            // (Ratio - 1) * Temp[0], Temp[1], Temp[1] * (Ratio + 1)
 faddp st(2), st(0)               // Temp[1], (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1),
 fdivrp                           // Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)
 fadd  st(0), st(0)               // 2 * Temp[1] / (Ratio - 1) * Temp[0] + Temp[1] * (Ratio + 1)

 // FAttackFactor := 1 - FastPower2MinError3(Temp[1] * FAttackSampleCycle);
 fld   st(0)                      // Temp[1], Temp[1]
 fmul  [edx.FAttackSampleCycle]   // Temp[0], Temp[1]

 fld   st(0)                      // Stack: Temp[0], Temp[0], Temp[1]
 frndint                          // Stack: round(Temp[0]), Temp[0], Temp[1]

 fist  IntCast                    // Stack: round(Temp[0]), Temp[0], Temp[1]
 fsubp                            // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0], Temp[1]

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp, Temp[0], Temp[1]
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0], Temp[1]
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0], Temp[1]
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0], Temp[1]
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0], Temp[1]
 fmul  CastedSingle               // NewAttackFactor, Temp[1]
 fld1                             // 1, NewAttackFactor, Temp[1]
 fsubrp                           // 1 - NewAttackFactor, Temp[1]
 fstp  [edx.FAttackFactor]

 // FReleaseFactor := FastPower2MinError3(Temp[1] * FReleaseSampleCycle);
 fmul  [edx.FReleaseSampleCycle]  // Temp[0]

 fld   st(0)                      // Stack: Temp[0], Temp[0]
 frndint                          // Stack: round(Temp[0]), Temp[0]

 fist  IntCast                    // Stack: round(Temp[0]), Temp[0]
 fsubp                            // Stack: newtemp = Temp[0] - round(Temp[0], Temp[0]

 mov   eax, IntCast
 add   eax, $7F
 shl   eax, $17
 mov   IntCast, eax

 fld   st(0)                      // Stack: newtemp, newtemp, Temp[0]
 fmul  [CSoftKnee + 4 * 5].Single // Stack: CP2MinError3[2] * newtemp, newtemp, Temp[0]
 fadd  [CSoftKnee + 4 * 6].Single // Stack: CP2MinError3[1] + (CP2MinError3[2] * newtemp), newtemp, Temp[0]
 fmul  st(0), st(1)               // Stack: newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
 fadd  [CSoftKnee + 4 * 7].Single // Stack: CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp)), newtemp, Temp[0]
 fmulp                            // Stack: newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
 fld1
 faddp                            // Stack: 1 + newtemp * (CP2MinError3[0] + newtemp * (CP2MinError3[1] + (CP2MinError3[2] * newtemp))), Temp[0]
 fmul  CastedSingle               // NewReleaseFactor
 fstp  [edx.FReleaseFactor]
end;
{$ENDIF}

function TLightweightSoftKneeFeedbackLikeCompressor.ProcessSample64(Input: Double): Double;
begin
 InputSample(Input);
 result := FGain * FMakeUpGain * Input;
end;

end.
