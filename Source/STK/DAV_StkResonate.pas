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

unit DAV_StkResonate;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK noise driven formant filter.

  This instrument contains a noise source, which excites a biquad resonance
  filter, with volume controlled by an ADSR.

  Control Change Numbers:
  - Resonance Frequency (0-Nyquist) = 2
  - Pole Radii = 4
  - Notch Frequency (0-Nyquist) = 11
  - Zero Radii = 1
  - Envelope Gain = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkAdsr, DAV_StkBiquad,
  DAV_StkNoise;

type
  TStkResonate = class(TStkControlableInstrument)
  protected
    FAdsr: TStkAdsr;
    FFilter: TStkBiquad;
    FNoise: TStkNoise;
    FPoleFrequency: Single;
    FPoleRadius: Single;
    FZeroFrequency: Single;
    FZeroRadius: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the filter for a resonance at the given frequency (Hz) and radius.
    procedure SetResonance(const Frequency, Radius: Single);

    // Set the filter for a notch at the given frequency (Hz) and radius.
    procedure SetNotch(const Frequency, Radius: Single);

    // Set the filter zero coefficients for contant resonance gain.
    procedure SetEqualGainZeroes;

    // Initiate the envelope with a key-on event.
    procedure KeyOn;

    // Signal a key-off event to the envelope.
    procedure KeyOff;

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
  SysUtils;

constructor TStkResonate.Create;
begin
  inherited Create(SampleRate);
  FAdsr := TStkAdsr.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);
  FFilter := TStkBiquad.Create(SampleRate);
  FPoleFrequency := 4000.0;
  FPoleRadius := 0.95;
  // Set the FFilter parameters.
  FFilter.SetResonance(FPoleFrequency, FPoleRadius, True);
  FZeroFrequency := 0.0;
  FZeroRadius := 0.0;
end;

destructor TStkResonate.Destroy;
begin
  FreeAndNil(FAdsr);
  FreeAndNil(FFilter);
  FreeAndNil(FNoise);
  inherited Destroy;
end;

procedure TStkResonate.KeyOn;
begin
  FAdsr.KeyOn();
end;

procedure TStkResonate.KeyOff;
begin
  FAdsr.KeyOff();
end;

procedure TStkResonate.NoteOn(const Frequency, Amplitude: Single);
begin
  FAdsr.Target := Amplitude;
  KeyOn;
  SetResonance(Frequency, FPoleRadius);
end;

procedure TStkResonate.NoteOff(const Amplitude: Single);
begin
  KeyOff;
end;

procedure TStkResonate.SetResonance(const Frequency, Radius: Single);
begin
  FPoleFrequency := Frequency;
  if (Frequency < 0.0) then
    FPoleFrequency := 0.0;
  FPoleRadius := Radius;
  if (Radius < 0.0) then
    FPoleRadius := 0.0
  else if (Radius >= 1.0) then
    FPoleRadius := 0.9999;
  FFilter.SetResonance(FPoleFrequency, FPoleRadius, True);
end;

procedure TStkResonate.SetNotch(const Frequency, Radius: Single);
begin
  FZeroFrequency := Frequency;
  if (Frequency < 0.0) then
    FZeroFrequency := 0.0;
  FZeroRadius := Radius;
  if (Radius < 0.0) then
    FZeroRadius := 0.0;
  FFilter.SetNotch(FZeroFrequency, FZeroRadius);
end;

procedure TStkResonate.SetEqualGainZeroes;
begin
  FFilter.SetEqualGainZeroes;
end;

function TStkResonate.Tick: Single;
begin
  FLastOutput := FFilter.Tick(FNoise.Tick) * FAdsr.Tick;
  Result := FLastOutput;
end;

procedure TStkResonate.ControlChange(const Number: Integer;
  const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = 2) then // 2
    SetResonance(norm * SampleRate * 0.5, FPoleRadius)
  else if (Number = 4) then // 4
    SetResonance(FPoleFrequency, norm * 0.9999)
  else if (Number = 11) then // 11
    SetNotch(norm * SampleRate * 0.5, FZeroRadius)
  else if (Number = 1) then
    SetNotch(FZeroFrequency, norm)
  else if (Number = CMidiAfterTouchCont) then // 128
    FAdsr.Target := norm;
end;

procedure TStkResonate.Clear;
begin
  FFilter.Clear;
end;

end.
