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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_StkSimple;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK wavetable/noise instrument.

  This class combines a looped wave, a noise source, a biquad resonance filter,
  a one-pole filter, and an ADSR envelope to create some interesting sounds.

  Control Change Numbers:
  - Filter Pole Position = 2
  - Noise/Pitched Cross-Fade = 4
  - Envelope Rate = 11
  - Gain = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkAdsr, DAV_StkWavePlayer,
  DAV_StkOnePole, DAV_StkBiquad, DAV_StkNoise;

type
  TStkSimple = class(TStkControlableInstrument)
  protected
    FADSR: TStkAdsr;
    FLoop: TStkWavePlayer;
    FFilter: TStkOnePole;
    FBiQuad: TStkBiquad;
    FNoise: TStkNoise;
    FBaseFrequency: Single;
    FLoopGain: Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;
  public
    // Class constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Start envelope toward "on" target.
    procedure KeyOn;

    // Start envelope toward "off" target.
    procedure KeyOff;

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by number and value (0.0 - 1.0).
    procedure ControlChange(const Number: Integer;
      const Value: Single); override;
  end;

implementation

uses
  SysUtils;

constructor TStkSimple.Create;
begin
  inherited Create(SampleRate);
  FADSR := TStkAdsr.Create(SampleRate);
  FBaseFrequency := 440.0;

  FLoop := TStkWavePlayer.Create(SampleRate, 'impuls10.raw');

  FFilter := TStkOnePole.Create(SampleRate, 0.5);
  FNoise := TStkNoise.Create(SampleRate);
  FBiQuad := TStkBiquad.Create(SampleRate);

  SetFrequency(FBaseFrequency);
  FLoopGain := 0.5;
end;

destructor TStkSimple.Destroy;
begin
  FreeAndNil(FADSR);
  FreeAndNil(FLoop);
  FreeAndNil(FFilter);
  FreeAndNil(FBiQuad);
  inherited Destroy;
end;

function TStkSimple.GetFrequency: Single;
begin
  Result := FLoop.Frequency;
end;

procedure TStkSimple.KeyOn;
begin
  FADSR.KeyOn;
end;

procedure TStkSimple.KeyOff;
begin
  FADSR.KeyOff;
end;

procedure TStkSimple.NoteOn(const Frequency, Amplitude: Single);
begin
  KeyOn;
  SetFrequency(Frequency);
  FFilter.Gain := Amplitude;
end;

procedure TStkSimple.NoteOff(const Amplitude: Single);
begin
  KeyOff;
end;

procedure TStkSimple.SetFrequency(const Value: Single);
begin
  FBiQuad.SetResonance(Value, 0.98, True);
  FLoop.Frequency := Value;
end;

function TStkSimple.Tick: Single;
begin
  FLastOutput := FLoopGain * FLoop.Tick;
  FBiQuad.Tick(FNoise.Tick);
  FLastOutput := lastoutput + (1.0 - FLoopGain) * FBiQuad.lastoutput;
  FLastOutput := FFilter.Tick(lastoutput) * FADSR.Tick;
  Result := FLastOutput;
end;

procedure TStkSimple.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiBreath) then // 2
    FFilter.setPole(0.99 * (1.0 - (norm * 2.0)))
  else if (Number = CMidiNoiseLevel) then // 4
    FLoopGain := norm
  else if (Number = CMidiModFrequency) then
  begin // 11
    norm := norm / (0.2 * SampleRate);
    FADSR.AttackRate := norm;
    FADSR.DecayRate := norm;
    FADSR.ReleaseRate := norm;
  end
  else if (Number = CMidiAfterTouchCont) then // 128
    FADSR.Target := norm;
end;

end.
