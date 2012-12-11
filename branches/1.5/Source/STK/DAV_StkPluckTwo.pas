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

unit DAV_StkPluckTwo;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK enhanced plucked string model class.

  This class implements an enhanced two-string, plucked physical model, a la
  Jaffe-Smith, Smith, and others.

  TStkPluckTwo is an abstract class, with no excitation specified. Therefore, it
  can't be directly instantiated.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayl, DAV_StkDelaya,
  DAV_StkOneZero;

type
  TStkPluckTwo = class(TStkControlableInstrument)
  private
    // Set the base loop gain.
    {
      The actual loop gain is set according to the Frequency.
      Because of high-Frequency loop FFilter roll-off, higher
      Frequency settings have greater loop gains.
    }
    procedure SetBaseLoopGain(const Value: Single);

    // Detune the two strings by the given factor.  A value of 1.0 produces unison strings.
    procedure SetDetune(const Value: Single);

    // Set the pluck or "excitation" position along the string (0.0 - 1.0).
    procedure SetPluckPosition(const Value: Single);
  protected
    FDelayLine: TStkDelayA;
    FDelayline2: TStkDelayA;
    FCombDelay: TStkDelayl;
    FFilter: TStkOneZero;
    FFilter2: TStkOneZero;
    FLength: Longint;
    FLoopGain: Single;
    FBaseLoopGain: Single;
    FLastFrequency: Single;
    FLastLength: Single;
    FDetuning: Single;
    FPluckAmplitude: Single;
    FPluckPosition: Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;
    procedure DetuneChanged; virtual;
  public
    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(const SampleRate, lowestFrequency: Single);
      reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Efficient combined setting of Frequency and FDetuning.
    procedure SetFreqAndDetune(const Frequency, Detune: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Virtual (abstract) tick function is implemented by subclasses.
    function Tick: Single; override;

    property BaseLoopGain: Single read FBaseLoopGain write SetBaseLoopGain;
    property Detune: Single read FDetuning write SetDetune;
    property PluckPosition: Single read FPluckPosition write SetPluckPosition;
  end;

implementation

uses
  SysUtils;

constructor TStkPluckTwo.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / lowestFrequency + 1);
  FBaseLoopGain := 0.995;
  FLoopGain := 0.999;
  FDelayLine := TStkDelayA.Create(SampleRate, (FLength / 2.0), FLength);
  FDelayline2 := TStkDelayA.Create(SampleRate, (FLength / 2.0), FLength);
  FCombDelay := TStkDelayl.Create(SampleRate, (FLength / 2.0), FLength);
  FFilter := TStkOneZero.Create(SampleRate);
  FFilter2 := TStkOneZero.Create(SampleRate);
  FPluckAmplitude := 0.3;
  FPluckPosition := 0.4;
  FDetuning := 0.995;
  FLastFrequency := lowestFrequency * 2.0;
  FLastLength := FLength * 0.5;
end;

destructor TStkPluckTwo.Destroy;
begin
  FreeAndNil(FDelayLine);
  FreeAndNil(FDelayline2);
  FreeAndNil(FCombDelay);
  FreeAndNil(FFilter);
  FreeAndNil(FFilter2);
  inherited Destroy;
end;

procedure TStkPluckTwo.Clear;
begin
  FDelayLine.Clear;
  FDelayline2.Clear;
  FCombDelay.Clear;
  FFilter.Clear;
  FFilter2.Clear;
end;

procedure TStkPluckTwo.SetFrequency(const Value: Single);
begin
  if FLastFrequency <> Value then
  begin
    FLastFrequency := Value;
    if (FLastFrequency <= 0.0) then
      FLastFrequency := 220.0;
  end;
end;

procedure TStkPluckTwo.FrequencyChanged;
var
  Delay: Single;
begin
  // Delay := FLength - approximate FFilter delay.
  FLastLength := (SampleRate / FLastFrequency);
  Delay := (FLastLength / FDetuning) - 0.5;
  if (Delay <= 0.0) then
    Delay := 0.3
  else if (Delay > FLength) then
    Delay := FLength;
  FDelayLine.Delay := Delay;

  Delay := (FLastLength * FDetuning) - 0.5;
  if (Delay <= 0.0) then
    Delay := 0.3
  else if (Delay > FLength) then
    Delay := FLength;
  FDelayline2.Delay := Delay;

  FLoopGain := FBaseLoopGain + (FLastFrequency * 0.000005);
  if (FLoopGain > 1.0) then
    FLoopGain := 0.99999;
end;

function TStkPluckTwo.GetFrequency: Single;
begin
  result := FLastFrequency;
end;

procedure TStkPluckTwo.SetDetune(const Value: Single);
begin
  if FDetuning <> Value then
  begin
    FDetuning := Value;
    if (FDetuning <= 0.0) then
      FDetuning := 0.1;
    DetuneChanged;
  end;
end;

procedure TStkPluckTwo.DetuneChanged;
begin
  FDelayLine.Delay := FLastLength / FDetuning - 0.5;
  FDelayline2.Delay := FLastLength * FDetuning - 0.5;
end;

procedure TStkPluckTwo.SetFreqAndDetune(const Frequency, Detune: Single);
begin
  FDetuning := Detune;
  SetFrequency(Frequency);
end;

procedure TStkPluckTwo.SetPluckPosition(const Value: Single);
begin
  FPluckPosition := Limit(Value, 0, 1);
end;

procedure TStkPluckTwo.SetBaseLoopGain(const Value: Single);
begin
  if FBaseLoopGain <> Value then
  begin
    FBaseLoopGain := Value;
    FLoopGain := FBaseLoopGain + (FLastFrequency * 0.000005);
    if (FLoopGain > 0.99999) then
      FLoopGain := 0.99999;
  end;
end;

procedure TStkPluckTwo.NoteOff(const Amplitude: Single);
begin
  FLoopGain := (1.0 - Amplitude) * 0.5;
end;

function TStkPluckTwo.Tick: Single;
begin
  result := 0;
end;

end.
