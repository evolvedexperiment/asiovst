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

unit DAV_StkEcho;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK echo effect class.

  This class implements a echo effect.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Types, DAV_StkCommon, DAV_StkDelay;

type
  TStkEcho = class(TStk)
  private
    procedure SetDelay(const Value: Single);
    procedure SetEffectMix(const Value: Single);
    function GetDelay: Single;
  protected
    FDelayLine: TStkDelay;
    FLength: Integer;
    FLastOutput: Single;
    FEffectMix: Single;
  public
    constructor Create(const SampleRate, LongestDelay: Single);
      reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Tick(const Input: Single): Single; overload;
    procedure Tick(const Input, Output: PDAVSingleFixedArray;
      const SampleFrames: Integer); overload;
  published
    property EffectMix: Single read FEffectMix write SetEffectMix;
    property Delay: Single read GetDelay write SetDelay;
    property LastOutput: Single read FLastOutput;
  end;

implementation

uses
  SysUtils;

constructor TStkEcho.Create(const SampleRate, LongestDelay: Single);
begin
  inherited Create(SampleRate);
  FLength := round(LongestDelay) + 2;
  FDelayLine := TStkDelay.Create(SampleRate, FLength shr 1, FLength);
  FEffectMix := 0.5;
  Clear;
end;

destructor TStkEcho.Destroy;
begin
  FreeAndNil(FDelayLine);
  inherited Destroy;
end;

function TStkEcho.GetDelay: Single;
begin
  Result := FDelayLine.Delay;
end;

procedure TStkEcho.Clear;
begin
  FDelayLine.Clear;
  FLastOutput := 0.0;
end;

procedure TStkEcho.SetDelay(const Value: Single);
begin
  FDelayLine.Delay := round(Limit(Value, 0, FLength));
end;

procedure TStkEcho.SetEffectMix(const Value: Single);
begin
  FEffectMix := Limit(Value, 0, 1);
end;

function TStkEcho.Tick(const Input: Single): Single;
begin
  FLastOutput := FEffectMix * FDelayLine.Tick(Input);
  FLastOutput := FLastOutput + Input * (1.0 - FEffectMix);
  Result := FLastOutput;
end;

procedure TStkEcho.Tick(const Input, Output: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Output^[Sample] := Tick(Input^[Sample]);
end;

end.
