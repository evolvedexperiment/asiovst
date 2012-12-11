{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_StkFM;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK abstract TFM synthesis base class.

  This class controls an arbitrary number of waves and envelopes, determined
  via a constructor argument.

  Control Change Numbers:
  - Control One = 2
  - Control Two = 4
  - LFO Speed = 11
  - LFO Depth = 1
  - FAdsr 2 & 4 Target = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  SysUtils, DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkAdsr,
  DAV_StkTwoZero, DAV_StkLfo, DAV_StkWavePlayer;

const
  CMaxOperators = 20;

type
  TStkFM = class(TStkControlableInstrument)
  private
    function GetModulationSpeed: Single;

    // Set the modulation speed in Hz.
    procedure SetModulationSpeed(const Value: Single);

    // Set the modulation depth.
    procedure SetModulationDepth(const Value: Single);

    // Set the value of FControlA.
    procedure SetControlA(const Value: Single);

    // Set the value of FControlB.
    procedure SetControlB(const Value: Single);

    // Set the frequency ratio for the specified wave.
    procedure SetRatio(WaveIndex: Integer; ratio: Single);
    function GetRatio(WaveIndex: Integer): Single;

    // Set the gain for the specified wave.
    procedure SetGain(WaveIndex: Integer; gain: Single);
    function GetGain(WaveIndex: Integer): Single;
  protected
    FAdsr: array [0 .. CMaxOperators - 1] of TStkAdsr;
    FWaves: array [0 .. CMaxOperators - 1] of TStkWaveplayer;
    FVibrato: TStkLfo;
    FTwoZero: TStkTwozero;
    FNOperators: Integer;
    FModDepth: Single;
    FControlA: Single; // = 0.5 * Control1 !!!
    FControlB: Single; // = 0.5 * Control2 !!!
    FBaseFrequency: Single;
    FGains: array [0 .. CMaxOperators - 1] of Single;
    FRatios: array [0 .. CMaxOperators - 1] of Single;
    FFmGains: array [0 .. 99] of Single;
    FFmSusLevels: array [0 .. 15] of Single;
    FFmAttTimes: array [0 .. 31] of Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;
    procedure GainsChanged; virtual;
    procedure RatioChanged(const WaveIndex: Integer); virtual;
  public
    constructor Create(const SampleRate: Single = 44100); overload; override;

    // Class constructor, taking the number of wave/envelope Operators to control.
    constructor Create(const SampleRate: Single; const Operators: Integer);
      reintroduce; overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all wave and envelope states.
    procedure Clear;

    // Load the rawwave filenames in FWaves.
    procedure LoadWave(const WaveIndex: Integer; const FileName: TFileName);

    // Start envelopes toward "on" targets.
    procedure KeyOn; virtual;

    // Start envelopes toward "off" targets.
    procedure KeyOff; virtual;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Pure virtual function ... must be defined in subclasses.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer;
      const Value: Single); override;

    property ControlA: Single read FControlA write SetControlA;
    property ControlB: Single read FControlB write SetControlB;

    property ratio[WaveIndex: Integer]: Single read GetRatio write SetRatio;
    property gain[WaveIndex: Integer]: Single read GetGain write SetGain;

    property Frequency: Single read FBaseFrequency write SetFrequency;
    property ModulationSpeed: Single read GetModulationSpeed
      write SetModulationSpeed;
    property ModulationDepth: Single read FModDepth write SetModulationDepth;
  end;

implementation

{ TFM }

procedure TStkFM.Clear;
begin

end;

procedure TStkFM.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  case Number of
    CMidiBreath:
      SetControlA(norm); // 2
    CMidiFootControl:
      SetControlB(norm); // 4
    CMidiModFrequency:
      SetModulationSpeed(norm * 12.0); // 11
    CMidiModWheel:
      SetModulationDepth(norm); // 1
    CMidiAfterTouchContour: // 128
      begin
        // FAdsr[0].SetTarget(norm);
        FAdsr[1].Target := norm;
        // FAdsr[2].setTarget(norm);
        FAdsr[3].Target := norm;
      end;
  end;
end;

constructor TStkFM.Create(const SampleRate: Single = 44100);
begin
  Create(SampleRate, 4);
end;

constructor TStkFM.Create(const SampleRate: Single; const Operators: Integer);
var
  i: Integer;
  temp: Single;
begin
  inherited Create(SampleRate);
  if (FNOperators <= 0) then
    FNOperators := 4;

  FTwoZero := TStkTwozero.Create(SampleRate);
  FTwoZero.SetB2(-1.0);
  FTwoZero.gain := 0.0;

  FVibrato := TStkLfo.Create(SampleRate);
  FVibrato.Frequency := 6.0;

  for i := 0 to FNOperators - 1 do
  begin
    FRatios[i] := 1.0;
    FGains[i] := 1.0;
    FAdsr[i] := TStkAdsr.Create(SampleRate);
  end;

  FModDepth := 0.0;
  FControlA := 1.0;
  FControlB := 1.0;
  FBaseFrequency := 440.0;

  temp := 1.0;
  for i := 99 downto 0 do
  begin
    FFmGains[i] := temp;
    temp := temp * 0.933033;
  end;

  temp := 1.0;
  for i := 15 downto 0 do
  begin
    FFmSusLevels[i] := temp;
    temp := temp * 0.707101;
  end;

  temp := 8.498186;
  for i := 0 to 31 do
  begin
    FFmAttTimes[i] := temp;
    temp := temp * 0.707101;
  end;
end;

destructor TStkFM.Destroy;
var
  Op: Integer;
begin
  FreeAndNil(FVibrato);
  FreeAndNil(FTwoZero);
  for Op := 0 to FNOperators - 1 do
  begin
    FreeAndNil(FAdsr[Op]);
    FreeAndNil(FWaves[Op]);
  end;
  inherited Destroy;
end;

procedure TStkFM.KeyOff;
var
  i: Integer;
begin
  for i := 0 to FNOperators - 1 do
    FAdsr[i].KeyOff;
end;

procedure TStkFM.KeyOn;
var
  i: Integer;
begin
  for i := 0 to FNOperators - 1 do
    FAdsr[i].KeyOn;
end;

procedure TStkFM.LoadWave(const WaveIndex: Integer; const FileName: TFileName);
begin
  FWaves[WaveIndex] := TStkWaveplayer.Create(SampleRate, FileName);
end;

procedure TStkFM.NoteOff(const Amplitude: Single);
begin
  KeyOff;
end;

procedure TStkFM.SetControlA(const Value: Single);
begin
  FControlA := Value;
end;

procedure TStkFM.SetControlB(const Value: Single);
begin
  FControlB := Value;
end;

procedure TStkFM.SetFrequency(const Value: Single);
begin
  if FBaseFrequency <> Value then
  begin
    FBaseFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TStkFM.FrequencyChanged;
var
  i: Integer;
begin
  for i := 0 to FNOperators - 1 do
    FWaves[i].Frequency := FBaseFrequency * FRatios[i];
end;

function TStkFM.GetFrequency: Single;
begin
  result := FBaseFrequency;
end;

function TStkFM.GetGain(WaveIndex: Integer): Single;
begin
  if WaveIndex in [0 .. FNOperators] then
    result := FGains[WaveIndex]
  else
    result := 0;
end;

function TStkFM.GetModulationSpeed: Single;
begin
  result := FVibrato.Frequency;
end;

function TStkFM.GetRatio(WaveIndex: Integer): Single;
begin
  if WaveIndex in [0 .. FNOperators] then
    result := FRatios[WaveIndex]
  else
    result := 0;
end;

procedure TStkFM.SetGain(WaveIndex: Integer; gain: Single);
begin
  if not(WaveIndex in [0 .. FNOperators]) then
    raise Exception.CreateFmt('WaveIndex out of bounds (%d)', [WaveIndex]);
  if FGains[WaveIndex] <> gain then
  begin
    FGains[WaveIndex] := gain;
    GainsChanged;
  end;
end;

procedure TStkFM.GainsChanged;
begin
  // nothing todo here yet
end;

procedure TStkFM.SetModulationDepth(const Value: Single);
begin
  FModDepth := Value;
end;

procedure TStkFM.SetModulationSpeed(const Value: Single);
begin
  FVibrato.Frequency := Value;
end;

procedure TStkFM.SetRatio(WaveIndex: Integer; ratio: Single);
begin
  if not(WaveIndex in [0 .. FNOperators]) then
    raise Exception.CreateFmt('WaveIndex out of bounds (%d)', [WaveIndex]);

  if FRatios[WaveIndex] <> ratio then
  begin
    FRatios[WaveIndex] := ratio;
    RatioChanged(WaveIndex);
  end;
end;

procedure TStkFM.RatioChanged(const WaveIndex: Integer);
begin
  if (FRatios[WaveIndex] > 0.0) then
    FWaves[WaveIndex].Frequency := FBaseFrequency * FRatios[WaveIndex]
  else
    FWaves[WaveIndex].Frequency := FRatios[WaveIndex];
end;

function TStkFM.Tick: Single;
begin
  result := 0;
end;

end.
