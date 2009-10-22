unit DAV_DspPlateReverb;

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
  DAV_Types, DAV_Classes, DAV_DspDelayLines, DAV_DspFilter,
  DAV_DspLFO, DAV_DSPFilterButterworth;

const
  CInternalSampleRate : Single = 29761;

type
  TDiffusor = class(TDspPersistent, IDspProcessor32)
  private
    FAmount             : Single;
    FInternalBufferSize : Integer;
    FBuffer             : PDAVSingleFixedArray;
    FBufferSize         : Integer;
    FBufferPos          : Integer;
    procedure SetBufferSize(const Value: Integer);
    function GetSample(Index: Integer): Single;
  protected
    procedure BuffersizeChanged; virtual;
  public
    constructor Create(const Buffersize: Integer = 0; Amount: Single = 0.5); virtual;
    destructor Destroy; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; register;

    procedure Mute;

    property Amount: Single read FAmount write FAmount;
    property BufferSize : Integer read FBufferSize write SetBufferSize;
    property Sample[Index: Integer]: Single read GetSample;
  end;

  TModulatedDiffusor = class(TDspPersistent, IDspProcessor32)
  private
    FAmount             : Single;
    FExcursion          : Integer;
    FLFO                : TLFOSine;
    FInternalBufferSize : Integer;
    FBuffer             : PDAVSingleFixedArray;
    FBufferSize         : Integer;
    FBufferInPos        : Integer;
    FBufferOutPos       : Integer;
    FModulation         : Single;
    FAllpass            : TFirstOrderAllpassFilter;
    procedure SetBufferSize(const Value: Integer);
    procedure SetExcursion(const Value: Integer);
    function GetSample(Index: Integer): Single;
    procedure SetModulation(const Value: Single);
  protected
    procedure BuffersizeChanged; virtual;
  public
    constructor Create(const Buffersize: Integer = 0; Amount: Single = 0.5; Excursion: Integer = 16); virtual;
    destructor Destroy; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; register;

    procedure Mute;
    property Amount: Single read FAmount write FAmount;
    property Modulation: Single read FModulation write SetModulation;
    property Excursion: Integer read FExcursion write SetExcursion;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property BufferPointer: PDAVSingleFixedArray read FBuffer;
    property Sample[Index: Integer]: Single read GetSample;
  end;

  TCustomPlateReverb = class(TDspSampleRatePersistent, IDspProcessor32)
  public
    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; virtual; abstract;
  end;

  TPlateReverb = class(TCustomPlateReverb)
  private
    procedure SetDamping(const Value: Single);
    procedure SetDecay(const Value: Single);
    procedure SetDecayDiffusion(const Value: Single);
    procedure SetInputDiffusion(const Value: Single);
    procedure SetModulation(const Value: Single);
    procedure SetPreDelay(const Value: Single);
  protected
    FResampleFilter     : TButterworthLowpassFilter;
    FLowpass            : array [0..1] of TButterworthLowpassFilter;
    FHighpass           : array [0..1] of TButterworthHighpassFilter;
    FDiffusors          : array [0..5] of TDiffusor;
    FDelays             : array [0..3] of TDelayLineSamples32;
    FModulatedDiffusors : array [0..1] of TModulatedDiffusor;
    FLastOutput         : array [0..1] of Single;
    FBuffer             : array [0..1] of TDAV4SingleArray;
    FCurrentOutput      : array [0..1] of Single;
    FPreDelayBuffer     : PDAVSingleFixedArray;
    FInternalPDBufSize  : Integer;
    FPreDelayBufferSize : Integer;
    FPreDelayBufferPos  : Integer;
    FPreDelay           : Single;
    FCurrentInput       : Single;
    FModulation         : Single;
    FDecay              : Single;
    FDecayDiffusion     : Single;
    FBandwidth          : Single;
    FInputDiffusion     : Single;
    FDampingFrequency   : Single;
    FResampleFactor     : Single;
    FResamplePos        : Single;
    procedure DampingFrequencyChanged; virtual;
    procedure DecayDiffusionChanged; virtual;
    procedure DecayChanged; virtual;
    procedure InputDiffusionChanged; virtual;
    procedure ModulationChanged; virtual;
    procedure PreDelayChanged; virtual;
    procedure ResizePreDelayBuffer; virtual;
    procedure SampleRateChanged; override;
 public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; override;

    property Decay: Single read FDecay write SetDecay;
    property DecayDiffusion: Single read FDecayDiffusion write SetDecayDiffusion;
    property InputDiffusion: Single read FInputDiffusion write SetInputDiffusion;
    property DampingFrequency: Single read FDampingFrequency write SetDamping;
    property Modulation: Single read FModulation write SetModulation;
    property PreDelay: Single read FPreDelay write SetPreDelay;  
    property OutputLeft: Single read FCurrentOutput[0];
    property OutputRight: Single read FCurrentOutput[1];
  published
    property SampleRate;
  end;

implementation

uses
  Math, SysUtils, DAV_Common, DAV_Approximations, DAV_DspInterpolation;

resourcestring
  RCStrBuffersizePositive = 'Buffersize must be larger or equal than zero!';
  RCStrInputDiffusionPositive = 'Input diffusion parameter must be equal or ' +
  'larger than zero!';
  RCStrDecayDiffusionPositive = 'Decay diffusion parameter must be equal or ' +
  'larger than zero!';
  RCStrDecayPositive = 'Decay parameter must be equal or larger than zero!';
  RCStrBandwidthPositive = 'Bandwidth parameter must be equal or larger than zero!';
  RCStrDampingPositive = 'Damping parameter must be equal or larger than zero!';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrPreDelayPositive = 'Pre delay must be larger or equal zero!';

{ TDiffusor }

constructor TDiffusor.Create(const Buffersize: Integer = 0; Amount: Single = 0.5);
begin
 inherited Create;
 FAmount := Amount;
 FBufferSize := Buffersize;
 BuffersizeChanged;
end;

destructor TDiffusor.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function TDiffusor.GetSample(Index: Integer): Single;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FInternalBufferSize)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Pos := FBufferPos - Index;
 if Pos < 0
  then Inc(Pos, FInternalBufferSize);
 result := FBuffer^[Pos];
end;

procedure TDiffusor.SetBufferSize(const Value: Integer);
begin
 if Value < 0
  then raise Exception.Create(RCStrBuffersizePositive);
 if FBufferSize <> Value then
  begin
   FBufferSize := Value + 1;
   BuffersizeChanged;
  end;
end;

procedure TDiffusor.BuffersizeChanged;
begin
 FInternalBufferSize := FBufferSize + 1;
 ReallocMem(FBuffer, FInternalBufferSize * SizeOf(Single));
 FillChar(FBuffer^, FInternalBufferSize * SizeOf(Single), 0);
 FBufferPos := 0;
end;

procedure TDiffusor.Mute;
begin
 Fillchar(FBuffer^[0], FInternalBufferSize * SizeOf(Single), 0);
end;

procedure TDiffusor.ProcessBlock32(Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDiffusor.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  WritePos: PSingle;
begin
 WritePos := @FBuffer^[FBufferPos];
 inc(FBufferPos);
 if FBufferPos >= FInternalBufferSize
  then FBufferPos := 0;

 WritePos^ := Input - FAmount * FBuffer^[FBufferPos];
 result := WritePos^ * FAmount + FBuffer^[FBufferPos];
end;
{$ELSE}
asm
  push ebx
  mov  ecx, [eax].FBuffer                 // FBuffer start in ecx
  mov  edx, [eax].FBufferPos              // FBuffer index in edx
  mov  ebx, edx                           // FBuffer index in ebx
  inc  edx
  cmp  edx, [eax].FInternalBufferSize     // are we at end of FBuffer?
  jb   @OK
  xor  edx, edx                           // if so, reset FBuffer index
@OK:
  mov  [eax].FBufferPos, edx              // and store new index,
                                          // result already in st(0),

  fld  Input                              // load Input

(*
  // This checks for very small values that can cause a processor
  // to switch in extra precision fMode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // The code is equivalent to the C inline macro by Jezar
  // This is the same spot where the original C macro appears
  test dword ptr [ecx + 4 * edx], $7F800000 // test if denormal
  jnz @Normal
  mov dword ptr [ecx + 4 * edx], 0          // if so, zero out
*)
@normal:


  fld  [ecx + 4 * edx]                    // load FBuffer^[FBufferPos]
  fmul [eax].FAmount                      // multiply Amount
  fsubp                                   // Input - FAmount * FBuffer^[FBufferPos]
  fst  [ecx + 4 * ebx]                    // write at WritePos
  fmul [eax].FAmount                      // multiply Amount
  fadd [ecx + 4 * edx]                    // add FBuffer^[FBufferPos]
  fstp result                             // store to result
  pop  ebx
end;
{$ENDIF}


{ TModulatedDiffusor }

constructor TModulatedDiffusor.Create(const Buffersize: Integer = 0; Amount: Single = 0.5; Excursion: Integer = 16);
begin
 inherited Create;
 FAmount := Amount;
 FLFO := TLFOSine.Create;
 FLFO.SampleRate := CInternalSampleRate;
 FLFO.Frequency := 0.1;
 FLFO.Amplitude := 0.5 - 1 / Excursion;
 FAllpass := TFirstOrderAllpassFilter.Create;
 FExcursion := Excursion;
 FBufferSize := Buffersize;
 BuffersizeChanged;
end;

destructor TModulatedDiffusor.Destroy;
begin
 Dispose(FBuffer);
 FreeAndNil(FAllpass);
 FreeAndNil(FLFO);
 inherited;
end;

function TModulatedDiffusor.GetSample(Index: Integer): Single;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FInternalBufferSize)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Pos := FBufferInPos - Index;
 if Pos < 0
  then Inc(Pos, FInternalBufferSize);
 result := FBuffer^[Pos];
end;

procedure TModulatedDiffusor.SetBufferSize(const Value: Integer);
begin
 if Value < 0
  then raise Exception.Create(RCStrBuffersizePositive);
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BuffersizeChanged;
  end;
end;

procedure TModulatedDiffusor.SetExcursion(const Value: Integer);
begin
 if FExcursion <> Value then
  begin
   FExcursion := Value;
   FLFO.Amplitude := 0.5 - 1 / Excursion;
   FBufferOutPos := FBufferInPos + Excursion div 2;
   if FBufferOutPos >= FInternalBufferSize
    then FBufferOutPos := FBufferOutPos - FInternalBufferSize;
   BuffersizeChanged;
  end;
end;

procedure TModulatedDiffusor.SetModulation(const Value: Single);
begin
 if FModulation <> Value then
  begin
   FModulation := Value;
   if Value = 0
    then FLFO.Frequency := 0
    else FLFO.Frequency := Power(10, 2 * Value - 1);
  end;
end;

procedure TModulatedDiffusor.BuffersizeChanged;
begin
 FInternalBufferSize := FBufferSize + FExcursion + 1;
 ReallocMem(FBuffer, FInternalBufferSize * SizeOf(Single));
 FillChar(FBuffer^, FInternalBufferSize * SizeOf(Single), 0);
 FBufferOutPos := Excursion div 2;
 FBufferInPos := 0;
end;

procedure TModulatedDiffusor.Mute;
begin
 Fillchar(FBuffer^[0], FInternalBufferSize * SizeOf(Single), 0);
end;

procedure TModulatedDiffusor.ProcessBlock32(Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TModulatedDiffusor.ProcessSample32(Input: Single): Single;
var
  temp : Single;
  SPos : Single;
  Pos  : Integer;
begin
 temp := FBuffer^[FBufferOutPos];

 // increase output position
 inc(FBufferOutPos);
 if FBufferOutPos >= FInternalBufferSize
  then FBufferOutPos := 0;

 inc(FBufferInPos);
 if FBufferInPos >= FInternalBufferSize
  then FBufferInPos := FBufferInPos - FInternalBufferSize;
 SPos := Excursion * FLFO.Sine;
 Pos := round(SPos);
 FAllpass.Frequency := Pos - SPos;
 assert(abs(FAllpass.Frequency) < 1);
 Pos := FBufferInPos + Pos;

 if Pos >= FInternalBufferSize then Pos := Pos - FInternalBufferSize else
 if Pos < 0 then Pos := Pos + FInternalBufferSize;
 FLFO.CalculateNextSample;

 FBuffer^[Pos] := Input + FAmount * temp;
 result := FAllpass.ProcessSample64(FBuffer^[Pos]) * FAmount + temp
end;


{ TCustomPlateReverb }

procedure TCustomPlateReverb.ProcessBlock32(Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;


{ TPlateReverb }

constructor TPlateReverb.Create;
begin
 inherited;
 FResampleFilter := TButterworthLowpassFilter.Create(2);

// FPreDelay := TDelayLineSamples32.Create(12);
 FDecay := 0.5;
 FResamplePos := 1;
 FPreDelay := 0.01;
 SampleRateChanged;
 FLowpass[0] := TButterworthLowpassFilter.Create(1);
 FLowpass[0].Frequency := 13.400;
 FLowpass[0].SampleRate := 44100;
 FLowpass[1] := TButterworthLowpassFilter.Create(1);
 FLowpass[1].Frequency := 13.400;
 FLowpass[1].SampleRate := 44100;
 FHighpass[0] := TButterworthHighpassFilter.Create(1);
 FHighpass[0].Frequency := 13.4;
 FHighpass[0].SampleRate := 44100;
 FHighpass[1] := TButterworthHighpassFilter.Create(1);
 FHighpass[1].Frequency := 13.4;
 FHighpass[1].SampleRate := 44100;
 FDiffusors[0] := TDiffusor.Create(142, 0.75);
 FDiffusors[1] := TDiffusor.Create(107, 0.75);
 FDiffusors[2] := TDiffusor.Create(379, 0.625);
 FDiffusors[3] := TDiffusor.Create(277, 0.625);
 FDiffusors[4] := TDiffusor.Create(1800, 0.5);
 FDiffusors[5] := TDiffusor.Create(2656, 0.5);
 FDelays[0] := TDelayLineSamples32.Create(4453);
 FDelays[1] := TDelayLineSamples32.Create(3720);
 FDelays[2] := TDelayLineSamples32.Create(4217);
 FDelays[3] := TDelayLineSamples32.Create(3163);
 FModulatedDiffusors[0] := TModulatedDiffusor.Create(672, 0.7, 16);
 FModulatedDiffusors[1] := TModulatedDiffusor.Create(908, 0.7, 16);
end;

destructor TPlateReverb.Destroy;
begin
 FreeAndNil(FResampleFilter);
 FreeAndNil(FLowpass[0]);
 FreeAndNil(FLowpass[1]);
 FreeAndNil(FDelays[0]);
 FreeAndNil(FDelays[1]);
 FreeAndNil(FDelays[2]);
 FreeAndNil(FDelays[3]);
 FreeAndNil(FDiffusors[0]);
 FreeAndNil(FDiffusors[1]);
 FreeAndNil(FDiffusors[2]);
 FreeAndNil(FDiffusors[3]);
 FreeAndNil(FDiffusors[4]);
 FreeAndNil(FDiffusors[5]);
 FreeAndNil(FModulatedDiffusors[0]);
 FreeAndNil(FModulatedDiffusors[1]);

 Dispose(FPreDelayBuffer);
 inherited;
end;

procedure TPlateReverb.SampleRateChanged;
begin
 inherited;
 ResizePreDelayBuffer;
 FResampleFactor := CInternalSampleRate / SampleRate;
 FResampleFilter.SampleRate := SampleRate;
 FResampleFilter.Frequency := 0.9 * CHalf32 * CInternalSampleRate;
end;

procedure TPlateReverb.SetDamping(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create(RCStrDampingPositive);
 if FDampingFrequency <> Value then
  begin
   FDampingFrequency := Value;
   DampingFrequencyChanged;
  end;
end;

procedure TPlateReverb.DampingFrequencyChanged;
begin
 FLowpass[0].Frequency := FDampingFrequency;
 FLowpass[1].Frequency := FDampingFrequency;
 Changed;
end;

procedure TPlateReverb.SetDecay(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create(RCStrDecayPositive);
 if FDecay <> Value then
  begin
   FDecay := Value;
   DecayChanged;
  end;
end;

procedure TPlateReverb.DecayChanged;
begin
 FModulatedDiffusors[0].Amount := Limit(0.15 + FDecay, 0.25, 0.5);
 FModulatedDiffusors[1].Amount := FModulatedDiffusors[0].Amount;
 Changed;
end;

procedure TPlateReverb.SetDecayDiffusion(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create(RCStrDecayDiffusionPositive);
 if FDecayDiffusion <> Value then
  begin
   FDecayDiffusion := Value;
   DecayDiffusionChanged;
  end;
end;

procedure TPlateReverb.DecayDiffusionChanged;
begin
 FDiffusors[4].FAmount := FDecayDiffusion;
 FDiffusors[5].FAmount := FDecayDiffusion;
 Changed;
end;

procedure TPlateReverb.SetInputDiffusion(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create(RCStrInputDiffusionPositive);
 if FInputDiffusion <> Value then
  begin
   FInputDiffusion := Value;
   InputDiffusionChanged;
  end;
end;

procedure TPlateReverb.InputDiffusionChanged;
begin
 FDiffusors[0].FAmount := FInputDiffusion;
 FDiffusors[1].FAmount := FInputDiffusion;
 FDiffusors[2].FAmount := FInputDiffusion / 1.2;
 FDiffusors[3].FAmount := FInputDiffusion / 1.2;
 Changed;
end;

procedure TPlateReverb.SetModulation(const Value: Single);
begin
 if FModulation <> Value then
  begin
   FModulation := Value;
   ModulationChanged;
  end;
end;

procedure TPlateReverb.SetPreDelay(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create(RCStrPreDelayPositive);
 if FPreDelay <> Value then
  begin
   FPreDelay := Value;
   PreDelayChanged;
  end;
end;

procedure TPlateReverb.PreDelayChanged;
begin
 ResizePreDelayBuffer;
 Changed;
end;

procedure TPlateReverb.ResizePreDelayBuffer;
begin
 FPreDelayBufferSize := round(FPreDelay * SampleRate);
 if FPreDelayBufferPos > FPreDelayBufferSize
  then FPreDelayBufferPos := 0;
 FInternalPDBufSize := 3 + FPreDelayBufferSize;
 ReallocMem(FPreDelayBuffer, FInternalPDBufSize * SizeOf(Single));
 FillChar(FPreDelayBuffer^, FInternalPDBufSize * SizeOf(Single), 0);
end;

procedure TPlateReverb.ModulationChanged;
begin
 FModulatedDiffusors[0].Modulation := FModulation;
 FModulatedDiffusors[1].Modulation := FModulation;
 Changed;
end;

procedure TPlateReverb.ProcessBlock32(Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TPlateReverb.ProcessSample32(Input: Single): Single;
var
  Temp : Single;
begin
 Temp := FResampleFilter.ProcessSample64(CDenorm32 + Input);

 FPreDelayBuffer[FPreDelayBufferPos] := Temp;
 inc(FPreDelayBufferPos);
 if FPreDelayBufferPos = 3
  then Move(FPreDelayBuffer[0], FPreDelayBuffer[FPreDelayBufferSize], 3 * SizeOf(Single)) else
 if FPreDelayBufferPos >= FPreDelayBufferSize
  then FPreDelayBufferPos := 0;

 while FResamplePos >= 1 do
  begin
   FResamplePos := FResamplePos - 1;
   FCurrentInput := Hermite32_asm(1 - FResamplePos, @FPreDelayBuffer[FPreDelayBufferPos]);

   move(FBuffer[0, 0], FBuffer[0, 1], 3 * SizeOf(Single));
   move(FBuffer[1, 0], FBuffer[1, 1], 3 * SizeOf(Single));

   FBuffer[0, 0] := FCurrentInput;
   FBuffer[1, 0] := FCurrentInput;

   Temp := FDiffusors[0].ProcessSample32(
           FDiffusors[1].ProcessSample32(
           FDiffusors[2].ProcessSample32(
           FDiffusors[3].ProcessSample32(FCurrentInput))));

   FBuffer[0, 0]  := FLastOutput[1] + Temp;
   FBuffer[1, 0]  := FLastOutput[0] + Temp;

   FLastOutput[1] := FHighpass[0].ProcessSample64(
                     FDelays[1].ProcessSample32(
                     FDiffusors[4].ProcessSample32(
                     FDecay *
                     FLowpass[0].ProcessSample64(
                     FDelays[0].ProcessSample32(
                     FModulatedDiffusors[0].ProcessSample32(
                     CDenorm32 + FBuffer[0, 0]))))));
   FLastOutput[0] := FHighpass[1].ProcessSample64(
                     FDelays[3].ProcessSample32(
                     FDiffusors[5].ProcessSample32(
                     FDecay *
                     FLowpass[1].ProcessSample64(
                     FDelays[2].ProcessSample32(
                     FModulatedDiffusors[1].ProcessSample32(
                     CDenorm32 + FBuffer[1, 0]))))));

   FBuffer[0, 0] := 0.6 * (FDelays[2].Sample[266] +
                           FDelays[2].Sample[2974] -
                           FDiffusors[5].Sample[1913] +
                           FDelays[3].Sample[1996] -
                           FDelays[0].Sample[1990] -
                           FDiffusors[4].Sample[187] -
                           FDelays[1].Sample[1066]);

   FBuffer[1, 0] := 0.6 * (FDelays[0].Sample[333] +
                           FDelays[0].Sample[3627] -
                           FDiffusors[4].Sample[1228] +
                           FDelays[1].Sample[2673] -
                           FDelays[2].Sample[2111] -
                           FDiffusors[5].Sample[335] -
                           FDelays[3].Sample[121]);
  end;
 FCurrentOutput[0] := Hermite32_asm(1 - FResamplePos, @FBuffer[0, 0]);
 FCurrentOutput[1] := Hermite32_asm(1 - FResamplePos, @FBuffer[1, 0]);
 FResamplePos := FResamplePos + FResampleFactor;

 result := CHalf32 * (FCurrentOutput[0] + FCurrentOutput[1]);
end;

end.
