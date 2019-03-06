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

unit DAV_StkWurley;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK Wurlitzer electric piano FM synthesis instrument.

  This class implements two simple FM Pairs summed together, also referred to
  as algorithm 5 of the TX81Z.

  Algorithm 5 is :  4.3 --\
                           + -> Out
                    2.1 --/

  Control Change Numbers:
    - Modulator Index One = 2
    - Crossfade of Outputs = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - ADSR 2 & 4 Target = 128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha. If you are of the type who
  should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} DAV_StkCommon, DAV_StkFm,
  DAV_StkWavePlayer;

type
  TStkWurley = class(TStkFM)
  protected
    // Set instrument parameters for a particular Frequency.
    procedure FrequencyChanged; override;
  public
    constructor Create(const SampleRate: Single = 44100); override;
    destructor Destroy; override;

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    function Tick: Single; override; // Compute one output sample.
  end;

implementation

constructor TStkWurley.Create(const SampleRate: Single = 44100);
begin
  inherited Create(SampleRate);
  FWaves[0] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[1] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[2] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[3] := TStkWavePlayer.Create(SampleRate, 'fwavblnk.wav');
  FWaves[0].OneShot := False;
  FWaves[1].OneShot := False;
  FWaves[2].OneShot := False;
  FWaves[3].OneShot := False;

  Ratio[0] := 1.0;
  Ratio[1] := 4.0;
  Ratio[2] := -510.0;
  Ratio[3] := -510.0;

  FGains[0] := FFmGains[99];
  FGains[1] := FFmGains[82];
  FGains[2] := FFmGains[92];
  FGains[3] := FFmGains[68];

  FAdsr[0].setAllTimes(0.001, 1.50, 0.0, 0.04);
  FAdsr[1].setAllTimes(0.001, 1.50, 0.0, 0.04);
  FAdsr[2].setAllTimes(0.001, 0.25, 0.0, 0.04);
  FAdsr[3].setAllTimes(0.001, 0.15, 0.0, 0.04);

  FTwoZero.Gain := 2.0;
  FVibrato.Frequency := 5.5;
end;

destructor TStkWurley.Destroy;
begin
  inherited Destroy;
end;

procedure TStkWurley.FrequencyChanged;
begin
  FWaves[0].Frequency := FBaseFrequency * FRatios[0];
  FWaves[1].Frequency := FBaseFrequency * FRatios[1];
  FWaves[2].Frequency := FRatios[2]; // Note here a 'fixed resonance'.
  FWaves[3].Frequency := FRatios[3];
end;

procedure TStkWurley.NoteOn(const Frequency, Amplitude: Single);
begin
  FGains[0] := Amplitude * FFmGains[99];
  FGains[1] := Amplitude * FFmGains[82];
  FGains[2] := Amplitude * FFmGains[82];
  FGains[3] := Amplitude * FFmGains[68];
  Self.Frequency := Frequency;
  KeyOn;
end;

function TStkWurley.Tick: Single;
var
  temp: Single;
begin
  temp := FGains[1] * FAdsr[1].Tick * FWaves[1].Tick * 2 * FControlA;
  FWaves[0].addPhaseOffset(temp);
  FWaves[3].addPhaseOffset(FTwoZero.LastOutput);
  temp := FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  FTwoZero.Tick(temp);
  FWaves[2].addPhaseOffset(temp);
  temp := (1.0 - FControlB) * FGains[0] * FAdsr[0].Tick * FWaves[0].Tick;
  temp := temp + FControlB * FGains[2] * FAdsr[2].Tick * FWaves[2].Tick;

  // Calculate Amplitude modulation and apply it to output.
  FLastOutput := 0.5 * temp * (1.0 + FVibrato.Tick * FModDepth);

  Result := FLastOutput;
end;

end.
