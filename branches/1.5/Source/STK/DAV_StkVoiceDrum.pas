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

unit DAV_StkVoiceDrum;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK vocal drum sample player class.

  This class implements a drum sampling synthesizer using WvIn objects and
  one-pole filters.  The drum rawwave files are sampled at 22050 Hz, but will
  be appropriately interpolated for other sample rates. You can specify the
  maximum polyphony (maximum number of simultaneous voices) via a #define in
  the Drummer.h.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkWavePlayer,
  DAV_StkOnePole;

const
  CVoiceNumWaves = 11;
  CVoicePolyphony = 4;

type
  TStkVoiceDrum = class(TStkControlableInstrument)
  protected
    FWaves: array [0 .. CVoicePolyphony - 1] of TStkWavePlayer;
    FFilters: array [0 .. CVoicePolyphony - 1] of TStkOnePole;
    FSounding: array [0 .. CVoicePolyphony - 1] of Integer;
    FNumSounding: Integer;
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;
  public
    // Class constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Start a note with the given drum type and amplitude.
    procedure NoteOn(const Instrument, Amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    procedure ControlChange(const Number: Integer;
      const Value: Single); override;
  end;

implementation

uses
  SysUtils;

procedure TStkVoiceDrum.ControlChange(const Number: Integer;
  const Value: Single);
begin
  inherited;
  // nothing in here yet
end;

constructor TStkVoiceDrum.Create;
var
  i: Integer;
begin
  inherited Create(SampleRate);
  for i := 0 to CVoicePolyphony - 1 do
  begin
    FFilters[i] := TStkOnePole.Create(SampleRate);
    FSounding[i] := -1;
  end;
  // This counts the number of FSounding voices.
  FNumSounding := 0;
end;

destructor TStkVoiceDrum.Destroy;
var
  i: Integer;
begin
  for i := 0 to FNumSounding - 2 do
    FreeAndNil(FWaves[i]);
  for i := 0 to CVoicePolyphony - 1 do
    FreeAndNil(FFilters[i]);
  inherited Destroy;
end;

function TStkVoiceDrum.GetFrequency: Single;
begin
  Result := 0;
end;

procedure TStkVoiceDrum.NoteOn(const Instrument, Amplitude: Single);
const
  CVoiceNames: array [0 .. CVoiceNumWaves - 1] of String = ('tak2.wav',
    'tak1.wav', 'bee1.wav', 'dee1.wav', 'dee2.wav', 'din1.wav', 'gun1.wav',
    'jun1.wav', 'jun2.wav', 'tak3.wav', 'tak4.wav');
var
  gain: Single;
  i, waveindex, notenum: Integer;
  tempwv: TStkWavePlayer;
  tempfilt: TStkOnePole;
begin
  gain := Amplitude;
  if (Amplitude > 1.0) then
    gain := 1.0
  else if (Amplitude < 0.0) then
    gain := 0;
  notenum := Round(Instrument) mod 11;
  // Check first to see if there's already one like this sounding.
  waveindex := -1;
  for i := 0 to CVoicePolyphony - 1 do
    if (FSounding[i] = notenum) then
      waveindex := i;

  if (waveindex >= 0) then
  begin
    // Reset this sound.
    FWaves[waveindex].Reset;
    FFilters[waveindex].SetPole(0.999 - (gain * 0.6));
    FFilters[waveindex].gain := gain;
  end
  else
  begin
    if (FNumSounding = CVoicePolyphony) then
    begin
      // If we're already at maximum polyphony, then preempt the oldest voice.
      FWaves[0].Free;
      FFilters[0].Clear;
      tempwv := FWaves[0];
      tempfilt := FFilters[0];
      // Re-order the list.
      for i := 0 to CVoicePolyphony - 2 do
      begin
        FWaves[i] := FWaves[i + 1];
        FFilters[i] := FFilters[i + 1];
      end;
      FWaves[CVoicePolyphony - 1] := tempwv;
      FFilters[CVoicePolyphony - 1] := tempfilt;
    end
    else
      FNumSounding := FNumSounding + 1;
    FSounding[FNumSounding - 1] := notenum;
    FWaves[FNumSounding - 1] := TStkWavePlayer.Create(SampleRate,
      CVoiceNames[notenum]);
    FWaves[FNumSounding - 1].OneShot := True;
    FWaves[FNumSounding - 1].Reset;
    FWaves[FNumSounding - 1].frequency := 1 / FWaves[FNumSounding - 1].length;
    FFilters[FNumSounding - 1].SetPole(0.999 - (gain * 0.6));
    FFilters[FNumSounding - 1].gain := gain;
  end;
end;

procedure TStkVoiceDrum.SetFrequency(const Value: Single);
begin
  inherited;
  // nothing in here yet!
end;

procedure TStkVoiceDrum.NoteOff(const Amplitude: Single);
var
  i: Integer;
begin
  // Set all FSounding wave filter gains low.
  i := 0;
  while (i < FNumSounding) do
  begin
    FFilters[i].gain := Amplitude * 0.01;
    i := i + 1;
  end;
end;

function TStkVoiceDrum.Tick: Single;
var
  output: Single;
  tempfilt: TStkOnePole;
  i, j: Integer;
begin
  output := 0.0;
  i := 0;
  while (i < FNumSounding) do
  begin
    if (FWaves[i].isFinished) then
    begin
      FWaves[i].Free;
      tempfilt := FFilters[i];
      // Re-order the list.
      for j := i to FNumSounding - 2 do
      begin
        FSounding[j] := FSounding[j + 1];
        FWaves[j] := FWaves[j + 1];
        FFilters[j] := FFilters[j + 1];
      end;
      FFilters[FNumSounding - 2] := tempfilt;
      FFilters[FNumSounding - 2].Clear;
      FSounding[FNumSounding - 2] := -1;
      FNumSounding := FNumSounding - 1;
      i := i - 1;
    end
    else
      output := output + FFilters[i].Tick(FWaves[i].Tick);
    i := i + 1;
  end;
  Result := output;
end;

end.
