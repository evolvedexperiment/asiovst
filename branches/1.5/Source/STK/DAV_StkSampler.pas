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

unit DAV_StkSampler;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK sampling synthesis abstract base class.

  This instrument contains up to 5 attack waves, 5 looped waves, and an ADSR
  envelope.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkWavePlayer, DAV_StkInstrument, DAV_StkAdsr,
  DAV_StkOnePole;

type
  TStkSampler = class(TStkControlableInstrument)
  protected
    FADSR: TStkAdsr;
    FAttacks: array [0 .. 4] of TStkWavePlayer;
    FLoops: array [0 .. 4] of TStkWavePlayer;
    FFilter: TStkOnepole;
    FAttackGain: Single;
    FLoopGain: Single;
    FBaseFrequency: Single;
    FLoopratios: Single;
    FAttackRatios: array [0 .. 4] of Single;
    FWhichOne: Integer;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual; abstract;
  public
    // Default constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear; virtual;

    // Initiate the envelopes with a key-on event and reset the attack waves.
    procedure KeyOn; virtual;

    // Signal a key-off event to the envelopes.
    procedure KeyOff; virtual;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

constructor TStkSampler.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  // We don't make the waves here yet, because
  // we don't know what they will be.
  FADSR := TStkAdsr.Create(SampleRate);
  FBaseFrequency := 440.0;
  FFilter := TStkOnepole.Create(SampleRate);
  FAttackGain := 0.25;
  FLoopGain := 0.25;
  FWhichOne := 0;
end;

destructor TStkSampler.Destroy;
begin
  FreeAndNil(FADSR);
  FreeAndNil(FFilter);
  inherited Destroy;
end;

function TStkSampler.GetFrequency: Single;
begin
  Result := FBaseFrequency;
end;

procedure TStkSampler.KeyOn;
begin
  FADSR.KeyOn;
  FAttacks[0].reset;
end;

procedure TStkSampler.KeyOff;
begin
  FADSR.KeyOff;
end;

procedure TStkSampler.NoteOff(const Amplitude: Single);
begin
  KeyOff;
end;

procedure TStkSampler.SetFrequency(const Value: Single);
begin
  if FBaseFrequency <> Value then
  begin
    if (Value <= 0.0) then
      FBaseFrequency := 220.0
    else
      FBaseFrequency := Value;
    FrequencyChanged;
  end;
  inherited;
end;

function TStkSampler.Tick: Single;
begin
  FLastOutput := FAttackGain * FAttacks[FWhichOne].Tick;
  FLastOutput := FLastOutput + FLoopGain * FLoops[FWhichOne].Tick;
  FLastOutput := FFilter.Tick(FLastOutput) * FADSR.Tick;
  Result := FLastOutput;
end;

procedure TStkSampler.Clear;
begin
  // nothing in here yet
end;

end.
