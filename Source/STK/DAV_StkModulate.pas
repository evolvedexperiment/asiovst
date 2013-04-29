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

unit DAV_StkModulate;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK periodic/random modulator.

  This class combines random and periodic modulations to give a nice, natural
  human modulation function.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkSubNoise, DAV_StkOnePole, DAV_StkLfo;

type
  TStkModulate = class(TStk)
  private
    FVibratoRate: Single;
    procedure SetVibratoRate(const Value: Single); // in Hz
    procedure SetVibratoGain(const Value: Single);
    procedure SetRandomGain(const Value: Single);
    function GetVibratoRate: Single;
  protected
    FVibrato: TStkLFO;
    FNoise: TStkSubNoise;
    FFilter: TStkOnePole;
    FVibratoGain: Single;
    FRandomGain: Single;
    FLastOutput: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Reset internal state.
    procedure Reset;

    // Compute one output sample.
    function Tick: Single; overload;

    // Return \e VectorSize outputs in \e Vector.
    procedure Tick(const Data: PDavSingleFixedArray;
      const SampleFrames: Integer); overload;

  published
    property VibratoRate: Single read GetVibratoRate write SetVibratoRate;
    property VibratoGain: Single read FVibratoRate write SetVibratoGain;
    property RandomGain: Single read FRandomGain write SetRandomGain;
    property LastOutput: Single read FLastOutput;
  end;

implementation

uses
  SysUtils;

constructor TStkModulate.Create;
begin
  inherited Create(SampleRate);
  FVibrato := TStkLFO.Create(SampleRate);
  FVibrato.Frequency := 6.0;
  FVibratoGain := 0.04;

  FNoise := TStkSubNoise.Create(SampleRate, 330);
  FRandomGain := 0.05;

  FFilter := TStkOnePole.Create(SampleRate, 0.999);
  FFilter.Gain := FRandomGain;
end;

destructor TStkModulate.Destroy;
begin
  FreeAndNil(FVibrato);
  FreeAndNil(FNoise);
  FreeAndNil(FFilter);
  inherited Destroy;
end;

procedure TStkModulate.Reset;
begin
  FLastOutput := 0.0;
end;

function TStkModulate.GetVibratoRate: Single;
begin
  result := FVibrato.Frequency;
end;

procedure TStkModulate.SetVibratoRate(const Value: Single);
begin
  if VibratoRate <> Value then
  begin
    FVibrato.Frequency := Value;
  end;
end;

procedure TStkModulate.SetVibratoGain(const Value: Single);
begin
  if FVibratoGain <> Value then
  begin
    FVibratoGain := Value;
  end;
end;

procedure TStkModulate.SetRandomGain(const Value: Single);
begin
  if FRandomGain <> Value then
  begin
    FRandomGain := Value;
    FFilter.Gain := FRandomGain;
  end;
end;

function TStkModulate.Tick: Single;
begin
  // Compute periodic and random modulations.
  FLastOutput := FVibratoGain * FVibrato.Tick;
  FLastOutput := FLastOutput + FFilter.Tick(FNoise.Tick);
  result := FLastOutput;
end;

procedure TStkModulate.Tick(const Data: PDavSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Data^[Sample] := Tick;
end;

end.
