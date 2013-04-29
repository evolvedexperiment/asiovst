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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_StkBlowBottle;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK blown bottle instrument class.

  This class implements a helmholtz resonator (Biquad Filter) with a
  polynomial jet excitation (a la Cook).

  Control Change Numbers:
  - Noise Gain = 4
  - Vibrato Frequency = 11
  - Vibrato Gain = 1
  - Volume = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkJetTable, DAV_StkNoise,
  DAV_StkBiquad, DAV_StkPolezero, DAV_StkAdsr, DAV_StkLfo;

type
  TStkBlowBottle = class(TStkControlableInstrument)
  private
  protected
    FJetTable: TStkJetTable;
    FResonator: TStkBiquad;
    FDCBlock: TStkPoleZero;
    FNoise: TStkNoise;
    FAdsr: TStkAdsr;
    FVibrato: TStkLfo;
    FMaxPressure: Single;
    FNoiseGain: Single;
    FVibratoGain: Single;
    FOutputGain: Single;
    FBaseFrequency: Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;
  public
    // Class constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Apply breath velocity to instrument with given amplitude and rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    // Decrease breath velocity with given rate of decrease.
    procedure StopBlowing(const Rate: Single);

    // Start a note with the given frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer;
      const Value: Single); override;
  end;

implementation

uses
  SysUtils, DAV_StkFilter;

const
  CBottleRadius = 0.999;

constructor TStkBlowBottle.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FJetTable := TStkJetTable.Create(SampleRate);
  FDCBlock := TStkPoleZero.Create(SampleRate);
  FDCBlock.SetBlockZero;
  FVibrato := TStkLfo.Create(SampleRate);
  FVibrato.Frequency := 5.925;
  FVibratoGain := 0.0;
  FResonator := TStkBiquad.Create(SampleRate);
  FResonator.SetResonance(500.0, CBottleRadius, True);
  FAdsr := TStkAdsr.Create(SampleRate);
  FAdsr.SetAllTimes(0.005, 0.01, 0.8, 0.010);
  FNoise := TStkNoise.Create(SampleRate);
  FNoiseGain := 20.0;
  FMaxPressure := 0.0;
end;

destructor TStkBlowBottle.Destroy;
begin
  FreeAndNil(FJetTable);
  FreeAndNil(FResonator);
  FreeAndNil(FDCBlock);
  FreeAndNil(FNoise);
  FreeAndNil(FAdsr);
  FreeAndNil(FVibrato);
  inherited Destroy;
end;

procedure TStkBlowBottle.Clear;
begin
  FResonator.Clear;
end;

procedure TStkBlowBottle.SetFrequency(const Value: Single);
begin
  if FBaseFrequency <> Value then
  begin
    if (FBaseFrequency <= 0.0) then
      FBaseFrequency := 220.0
    else
      FBaseFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TStkBlowBottle.FrequencyChanged;
begin
  FResonator.SetResonance(FBaseFrequency, CBottleRadius, True);
end;

function TStkBlowBottle.GetFrequency: Single;
begin
  result := FBaseFrequency;
end;

procedure TStkBlowBottle.StartBlowing(const Amplitude, Rate: Single);
begin
  FAdsr.AttackRate := Rate;
  FMaxPressure := Amplitude;
  FAdsr.KeyOn;
end;

procedure TStkBlowBottle.StopBlowing(const Rate: Single);
begin
  FAdsr.ReleaseRate := Rate;
  FAdsr.KeyOff;
end;

procedure TStkBlowBottle.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  StartBlowing(1.1 + (Amplitude * 0.20), Amplitude * 0.02);
  FOutputGain := Amplitude + 0.001;
end;

procedure TStkBlowBottle.NoteOff(const Amplitude: Single);
begin
  StopBlowing(Amplitude * 0.02);
end;

function TStkBlowBottle.Tick: Single;
var
  BreathPressure, RandPressure, PressureDiff: Single;
begin
  // Calculate the breath pressure (envelope + vibrato)
  BreathPressure := FMaxPressure * FAdsr.Tick + FVibratoGain * FVibrato.Tick;

  PressureDiff := BreathPressure - FResonator.LastOutput;

  RandPressure := FNoiseGain * FNoise.Tick * BreathPressure *
    (1.0 + PressureDiff);

  FResonator.Tick(BreathPressure + RandPressure - FJetTable.Tick(PressureDiff) *
    PressureDiff);

  FLastOutput := 0.2 * FOutputGain * FDCBlock.Tick(PressureDiff);

  result := FLastOutput;
end;

procedure TStkBlowBottle.ControlChange(const Number: Integer;
  const Value: Single);
var
  Norm: Single;
begin
  Norm := Limit(Value, 0, 1);

  case Number of
    CMidiNoiseLevel:
      FNoiseGain := Norm * 30.0; // 4
    CMidiModFrequency:
      FVibrato.Frequency := Norm * 12.0 // 11
        CMidiModWheel: FVibratoGain := Norm * 0.4 // 1
        CMidiAfterTouchContour: FAdsr.Target := Norm; // 128
  end;
end;

end.
