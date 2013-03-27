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

unit DAV_StkTubeBell;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK tubular bell (orchestral chime) FM synthesis instrument.

  This class implements two simple FM Pairs summed together, also referred to
  as algorithm 5 of the TX81Z.

    Algorithm 5 is :  4.3 --\
                            +--> Out
                      2.1 --/

  Control Change Numbers:
    - Modulator Index One = 2
    - Crossfade of Outputs = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - FAdsr 2 & 4 Target = 128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha. If you are of the type who
  should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer;

type
  TStkTubeBell = class(TStkFM)
  public
    constructor Create(const SampleRate: Single;
      const Operators: Integer = 4); override;
    destructor Destroy; override;

    // Start a note with the given frequency and amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

constructor TStkTubeBell.Create(const SampleRate: Single;
  const Operators: Integer = 4);
begin
  inherited Create(SampleRate, Operators);
  FWaves[0] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[1] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[2] := TStkWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[3] := TStkWavePlayer.Create(SampleRate, 'fwavblnk.wav');
  FWaves[0].OneShot := False;
  FWaves[1].OneShot := False;
  FWaves[2].OneShot := False;
  FWaves[3].OneShot := False;

  Ratio[0] := 1.0 * 0.995;
  Ratio[1] := 1.414 * 0.995;
  Ratio[2] := 1.0 * 1.005;
  Ratio[3] := 1.414 * 1.000;

  FGains[0] := FFmGains[94];
  FGains[1] := FFmGains[76];
  FGains[2] := FFmGains[99];
  FGains[3] := FFmGains[71];

  FAdsr[0].SetAllTimes(0.005, 4.0, 0.0, 0.04);
  FAdsr[1].SetAllTimes(0.005, 4.0, 0.0, 0.04);
  FAdsr[2].SetAllTimes(0.001, 2.0, 0.0, 0.04);
  FAdsr[3].SetAllTimes(0.004, 4.0, 0.0, 0.04);

  FTwoZero.Gain := 0.5;
  FVibrato.Frequency := 2.0;
end;

destructor TStkTubeBell.Destroy;
begin
  inherited Destroy;
end;

procedure TStkTubeBell.NoteOn;
begin
  FGains[0] := Amplitude * FFmGains[94];
  FGains[1] := Amplitude * FFmGains[76];
  FGains[2] := Amplitude * FFmGains[99];
  FGains[3] := Amplitude * FFmGains[71];
  setFrequency(Frequency);
  keyOn;
end;

function TStkTubeBell.Tick: Single;
var
  temp, temp2: Single;
begin
  temp := FGains[1] * FAdsr[1].Tick * FWaves[1].Tick;
  temp := temp * 2 * FControlA;
  FWaves[0].addPhaseOffset(temp);

  FWaves[3].addPhaseOffset(FTwoZero.LastOutput);
  temp := FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  FTwoZero.Tick(temp);
  temp := temp * FEnvelope.Target;
  FWaves[2].addPhaseOffset(temp);

  temp := FGains[0] * FAdsr[0].Tick * FWaves[0].Tick;
  // temp:=( 1.0 - (control2 * 0.5)) * FGains[0] * FAdsr[0].tick * FWaves[0].tick;
  temp := { temp+control2 * 0.5 * } temp + FGains[2] * FAdsr[2].Tick *
    FWaves[2].Tick;

  // Calculate amplitude modulation and apply it to output.
  temp2 := FVibrato.Tick * modDepth;
  temp := temp * (1.0 + temp2);
  FLastOutput := temp * 0.5;
  Result := FLastOutput;
end;

end.
