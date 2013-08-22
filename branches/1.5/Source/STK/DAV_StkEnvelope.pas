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

unit DAV_StkEnvelope;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} DAV_Common, DAV_StkCommon;

type
  TStkEnvelope = class(TStk)
  private
    function GetTime: Single;
    procedure SetCurrentValue(const Value: Single);
    procedure SetRate(const Value: Single);
    procedure SetTime(const Value: Single);
    procedure SetTarget(const Value: Single);
  protected
    FCurrentValue: Single;
    FTarget: Single;
    FRate: Single;
    FState: Integer;

    procedure CurrentValueChanged; virtual;
    procedure RateChanged; virtual;
    procedure TargetChanged; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); override;
    destructor Destroy; override;

    procedure KeyOn; virtual;
    procedure KeyOff; virtual;

    function Tick: Single; overload; virtual;
    procedure Tick(const Vector: PDAVSingleFixedArray;
      const VectorSize: Cardinal); overload; virtual;

    property CurrentValue: Single read FCurrentValue write SetCurrentValue;
    property Rate: Single read FRate write SetRate;
    property Time: Single read GetTime write SetTime;
    property Target: Single read FTarget write SetTarget;
    property State: Integer read FState;
  end;

implementation

uses
  SysUtils;

constructor TStkEnvelope.Create;
begin
  inherited Create(SampleRate);
  FTarget := 0;
  FCurrentValue := 0;
  FRate := 0.001;
  FState := 0;
end;

destructor TStkEnvelope.Destroy;
begin
  inherited Destroy;
end;

function TStkEnvelope.GetTime: Single;
begin
  result := -1.0 / (Rate * SampleRate);
end;

procedure TStkEnvelope.KeyOn;
begin
  FTarget := 1;
  if (FCurrentValue <> FTarget) then
    FState := 1;
end;

procedure TStkEnvelope.KeyOff;
begin
  FTarget := 0;
  if (FCurrentValue <> FTarget) then
    FState := 1;
end;

procedure TStkEnvelope.SetRate(const Value: Single);
begin
  if Value < 0 then
    raise Exception.Create('Rate must be above 0!');
  if FRate <> Value then
  begin
    FRate := Value;
    RateChanged;
  end;
end;

procedure TStkEnvelope.RateChanged;
begin
  // nothing here yet!
end;

procedure TStkEnvelope.SetTime(const Value: Single);
begin
  Rate := 1.0 / (-Value * SampleRate)
end;

procedure TStkEnvelope.SetTarget(const Value: Single);
begin
  if FTarget <> Value then
  begin
    FTarget := Value;
    TargetChanged;
  end;
end;

procedure TStkEnvelope.TargetChanged;
begin
  if (FCurrentValue <> FTarget) then
    FState := 1;
end;

procedure TStkEnvelope.SetCurrentValue(const Value: Single);
begin
  if FCurrentValue <> Value then
  begin
    FCurrentValue := Value;
    CurrentValueChanged;
  end;
end;

procedure TStkEnvelope.CurrentValueChanged;
begin
  FTarget := FCurrentValue;
  FState := 0;
end;

function TStkEnvelope.Tick: Single;
begin
  if (FState > 0) then
    if (FTarget > FCurrentValue) then
    begin
      FCurrentValue := FCurrentValue + FRate;
      if (FCurrentValue >= FTarget) then
      begin
        FCurrentValue := FTarget;
        FState := 0;
      end;
    end
    else
    begin
      FCurrentValue := FCurrentValue - FRate;
      if (FCurrentValue <= FTarget) then
      begin
        FCurrentValue := FTarget;
        FState := 0;
      end;
    end;
  result := FCurrentValue;
end;

procedure TStkEnvelope.Tick(const Vector: PDAVSingleFixedArray;
  const VectorSize: Cardinal);
var
  i: Integer;
begin
  for i := 0 to VectorSize - 1 do
    Vector^[i] := Tick;
end;

end.