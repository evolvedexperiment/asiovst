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

unit DAV_StkBrass;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK simple TStkBrass instrument class.

  This class implements a simple Brass instrument waveguide model, a la Cook
  (TBone, HosePlayer).

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
  - Lip Tension = 2
  - Slide FLength = 4
  - Vibrato Frequency = 11
  - Vibrato Gain = 1
  - Volume = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayA, DAV_StkBiquad,
  DAV_StkPolezero, DAV_StkAdsr, DAV_StkLfo;

type
  TStkBrass = class(TStkControlableInstrument)
  protected
    FDelayLine: TStkDelayA;
    FLipFilter: TStkBiQuad;
    FDcBlock: TStkPoleZero;
    FAdsr: TStkADSR;
    FVibrato: TStkLFO;
    FLength: Integer;
    FLipTarget: Single;
    FSlideTarget: Single;
    FVibratoGain: Single;
    FMaxPressure: Single;
    FBaseFrequency: Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;

    procedure FrequencyChanged; virtual;
  public
    // Class constructor, taking the lowest desired playing frequency.
    constructor Create(const SampleRate, LowestFrequency: Single);
      reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the lips frequency.
    procedure SetLip(const Frequency: Single);

    // Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    // Decrease breath pressure with given rate of decrease.
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
  SysUtils, Math, DAV_StkFilter;

constructor TStkBrass.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  FDelayLine := TStkDelayA.Create(SampleRate, 0.5 * FLength, FLength);

  FLipFilter := TStkBiQuad.Create(SampleRate);
  FLipFilter.Gain := 0.03;
  FDcBlock := TStkPoleZero.Create(SampleRate);
  FDcBlock.setBlockZero(0.99);

  FAdsr := TStkADSR.Create(SampleRate);
  FAdsr.setAllTimes(0.005, 0.001, 1.0, 0.010);

  FVibrato := TStkLFO.Create(SampleRate);
  FVibrato.Frequency := 6.137;
  FVibratoGain := 0.0;

  Clear;
  FMaxPressure := 0.0;
  FLipTarget := 0.0;

  // Necessary to initialize variables.
  SetFrequency(220.0);
end;

destructor TStkBrass.Destroy;
begin
  FreeAndNil(FDelayLine);
  FreeAndNil(FLipFilter);
  FreeAndNil(FDcBlock);
  FreeAndNil(FAdsr);
  FreeAndNil(FVibrato);
  inherited Destroy;
end;

procedure TStkBrass.Clear;
begin
  FDelayLine.Clear;
  FLipFilter.Clear;
  FDcBlock.Clear;
end;

procedure TStkBrass.SetFrequency(const Value: Single);
begin
  if FBaseFrequency <> Frequency then
  begin
    if (Value <= 0.0) then
      FBaseFrequency := 220.0
    else
      FBaseFrequency := Frequency;
    FrequencyChanged;
  end;
end;

procedure TStkBrass.FrequencyChanged;
begin
  // Fudge correction for filter delays.
  FSlideTarget := (SampleRate / FBaseFrequency * 2.0) + 3.0;
  FDelayLine.Delay := FSlideTarget; // play a harmonic

  FLipTarget := FBaseFrequency;
  FLipFilter.setResonance(FBaseFrequency, 0.997, False);
end;

function TStkBrass.GetFrequency: Single;
begin
  Result := FBaseFrequency;
end;

procedure TStkBrass.SetLip(const Frequency: Single);
var
  Freakency: Single;
begin
  Freakency := Frequency;
  if (Frequency <= 0.0) then
    Freakency := 220.0;

  FLipFilter.setResonance(Freakency, 0.997, False);
end;

procedure TStkBrass.StartBlowing(const Amplitude, Rate: Single);
begin
  FAdsr.AttackRate := Rate;
  FMaxPressure := Amplitude;
  FAdsr.keyOn;
end;

procedure TStkBrass.StopBlowing(const Rate: Single);
begin
  FAdsr.ReleaseRate := Rate;
  FAdsr.keyOff;
end;

procedure TStkBrass.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  StartBlowing(Amplitude, Amplitude * 0.001);
end;

procedure TStkBrass.NoteOff(const Amplitude: Single);
begin
  StopBlowing(Amplitude * 0.005);
end;

function TStkBrass.Tick: Single;
var
  deltaPressure, borePressure, mouthPressure, breathPressure: Single;
begin
  breathPressure := FMaxPressure * FAdsr.Tick;
  breathPressure := breathPressure + FVibratoGain * FVibrato.Tick;

  mouthPressure := 0.3 * breathPressure;
  borePressure := 0.85 * FDelayLine.LastOutput;
  deltaPressure := mouthPressure - borePressure; // Differential pressure.
  deltaPressure := FLipFilter.Tick(deltaPressure); // Force - > position.
  deltaPressure := deltaPressure * deltaPressure;
  // Basic position to area mapping.
  if (deltaPressure > 1.0) then
    deltaPressure := 1.0; // Non-linear saturation.
  // The following input scattering assumes the mouthPressure := area.
  FLastOutput := deltaPressure * mouthPressure + (1.0 - deltaPressure) *
    borePressure;
  FLastOutput := FDelayLine.Tick(FDcBlock.Tick(LastOutput));

  Result := FLastOutput;
end;

procedure TStkBrass.ControlChange(const Number: Integer; const Value: Single);
var
  temp, norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiLipTension) then
  begin // 2
    temp := FLipTarget * power(4.0, (2.0 * norm) - 1.0);
    SetLip(temp);
  end
  else if (Number = CMidiSlideLength) then // 4
    FDelayLine.Delay := FSlideTarget * (0.5 + norm)
  else if (Number = CMidiModFrequency) then // 11
    FVibrato.Frequency := norm * 12
  else if (Number = CMidiModWheel) then // 1
    FVibratoGain := norm * 0.4
  else if (Number = CMidiAfterTouchCont) then // 128
    FAdsr.Target := norm;
end;

end.
