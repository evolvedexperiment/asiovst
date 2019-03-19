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

unit DAV_StkSingWave;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK "singing" looped soundfile class.

  This class contains all that is needed to make a pitched musical sound, like
  a simple voice or violin.  In general, it will not be used alone because of
  munchkinification effects from pitch shifting.  It will be used as an
  excitation source for other instruments.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkWavePlayer, DAV_StkModulate,
  DAV_StkEnvelope;

type
  TStkSingWave = class(TStk)
  private
    // Set the vibrato frequency in Hz.
    procedure SetVibratoRate(const Value: Single);

    // Set the vibrato gain.
    procedure SetVibratoGain(const Value: Single);

    // Set the random-ness amount.
    procedure SetRandomGain(const Value: Single);

    // Set the sweep rate.
    procedure SetSweepRate(const Value: Single);

    // Set the gain rate.
    procedure SetGainRate(const Value: Single);

    // Set the gain target value.
    procedure SetGainTarget(const Value: Single);
    function GetGainRate: Single;
    function GetGainTarget: Single;
    function GetRandomGain: Single;
    function GetVibratoGain: Single;
    function GetVibratoRate: Single;

  protected
    FWave: TStkWavePlayer;
    FModulator: TStkModulate;
    FEnvelope: TStkEnvelope;
    FPitchEnvelope: TStkEnvelope;
    FRate: Single;
    FSweepRate: Single;
    FLastOutput: Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(Frequency: Single);

  public
    // Class constructor taking filename argument.
    {
      An StkError will be thrown if the file is not found, its format is
      unknown, or a read error occurs.
    }
    constructor Create(const SampleRate: Single; const FileName: string);
      reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset file to beginning.
    procedure Reset;

    // Start a note.
    procedure NoteOn;

    // Stop a note.
    procedure NoteOff;

    // Return the last output value.
    function LastOut: Single;

    // Compute one output sample.
    function Tick: Single;

    property VibratoRate: Single read GetVibratoRate write SetVibratoRate;
    property VibratoGain: Single read GetVibratoGain write SetVibratoGain;
    property RandomGain: Single read GetRandomGain write SetRandomGain;
    property SweepRate: Single read FSweepRate write SetSweepRate;
    property GainRate: Single read GetGainRate write SetGainRate;
    property GainTarget: Single read GetGainTarget write SetGainTarget;
  end;

implementation

uses
  SysUtils;

constructor TStkSingWave.Create;
begin
  inherited Create(SampleRate);
  FWave := TStkWavePlayer.Create(SampleRate, FileName);
  FWave.OneShot := False;
  FRate := 1.0;
  FSweepRate := 0.001;
  FModulator := TStkModulate.Create(SampleRate);
  FModulator.VibratoRate := 6.0;
  FModulator.VibratoGain := 0.04;
  FModulator.RandomGain := 0.005;
  FEnvelope := TStkEnvelope.Create(SampleRate);
  FPitchEnvelope := TStkEnvelope.Create(SampleRate);
  SetFrequency(75.0);
  FPitchEnvelope.Rate := 1.0;
  Tick;
  Tick;
  FPitchEnvelope.Rate := FSweepRate * FRate;
end;

destructor TStkSingWave.Destroy;
begin
  FreeAndNil(FWave);
  FreeAndNil(FModulator);
  FreeAndNil(FEnvelope);
  FreeAndNil(FPitchEnvelope);
  inherited Destroy;
end;

procedure TStkSingWave.Reset;
begin
  FWave.Reset;
  FLastOutput := 0.0;
end;

procedure TStkSingWave.SetFrequency;
var
  temp: Single;
begin
  temp := FRate;
  FRate := Frequency * FSampleRateInv;
  temp := temp - FRate;
  if (temp < 0) then
    temp := -temp;
  FPitchEnvelope.Target := FRate;
  FPitchEnvelope.Rate := FSweepRate * temp;
end;

procedure TStkSingWave.SetVibratoRate(const Value: Single);
begin
  FModulator.VibratoRate := Value;
end;

function TStkSingWave.GetVibratoRate: Single;
begin
  Result := FModulator.VibratoRate;
end;

procedure TStkSingWave.SetVibratoGain(const Value: Single);
begin
  FModulator.VibratoGain := Value;
end;

function TStkSingWave.GetVibratoGain: Single;
begin
  Result := FModulator.VibratoGain
end;

procedure TStkSingWave.SetRandomGain(const Value: Single);
begin
  FModulator.RandomGain := Value;
end;

function TStkSingWave.GetRandomGain: Single;
begin
  Result := FModulator.RandomGain;
end;

procedure TStkSingWave.SetSweepRate(const Value: Single);
begin
  FSweepRate := Value;
end;

procedure TStkSingWave.SetGainRate(const Value: Single);
begin
  FEnvelope.Rate := Value;
end;

function TStkSingWave.GetGainRate: Single;
begin
  Result := FEnvelope.Rate;
end;

procedure TStkSingWave.SetGainTarget(const Value: Single);
begin
  FEnvelope.Target := Value;
end;

function TStkSingWave.GetGainTarget: Single;
begin
  Result := FEnvelope.Target;
end;

procedure TStkSingWave.NoteOn;
begin
  FEnvelope.KeyOn;
end;

procedure TStkSingWave.NoteOff;
begin
  FEnvelope.keyOff;
end;

function TStkSingWave.Tick: Single;
var
  newrate: Single;
begin
  // Set the FWave FRate.
  newrate := FPitchEnvelope.Tick;
  newrate := newrate + newrate * FModulator.Tick;
  FWave.Frequency := newrate;
  FLastOutput := FWave.Tick;
  FLastOutput := FLastOutput * FEnvelope.Tick;
  Result := FLastOutput;
end;

function TStkSingWave.LastOut: Single;
begin
  Result := FLastOutput;
end;

end.
