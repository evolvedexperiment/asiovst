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

unit DAV_StkADSR;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkEnvelope, DAV_StkCommon;

type
  TADSRStates = (asAttack, asDecay, asSustain, asRelease, asDone);

  TStkADSR = class(TStkEnvelope)
  private
    FTarget: Single;
    // Return the current envelope \e FState (asAttack, asDecay, asSustain, asRelease, asDone).
    function GetState: TADSRStates;

    // Set the asAttack FRate.
    procedure SetAttackRate(const Value: Single);

    // Set the asDecay FRate.
    procedure SetDecayRate(const Value: Single);

    // Set the asSustain level.
    procedure SetSustainLevel(const Value: Single);

    // Set the asRelease FRate.
    procedure setReleaseRate(const Value: Single);

    // Set the asAttack FRate based on a time duration.
    procedure SetAttackTime(const Value: Single);

    // Set the asDecay FRate based on a time duration.
    procedure SetDecayTime(const Value: Single);

  protected
    FAttackRate: Single;
    FDecayRate: Single;
    FSustainLevel: Single;
    FReleaseRate: Single;
    FState: TADSRStates;

    procedure TargetChanged; override;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    procedure KeyOn; override;
    procedure KeyOff; override;

    // Set the asRelease FRate based on a time duration.
    procedure SetReleaseTime(const Value: Single);

    // Set asSustain level and asAttack, asDecay, and asRelease FState rates based on time durations.
    procedure SetAllTimes(const aTime, dTime, sLevel, rTime: Single);

    // Set to FState := ADSR::asSustain with current and FTarget values of \e Value.
    procedure SetValue(Value: Single);

    function Tick: Single; overload; override;

    property State: TADSRStates read GetState;

    property AttackRate: Single read FAttackRate write SetAttackRate;
    property DecayRate: Single read FDecayRate write SetDecayRate;
    property ReleaseRate: Single read FReleaseRate write setReleaseRate;
    property SustainLevel: Single read FSustainLevel write SetSustainLevel;
  end;

implementation

constructor TStkADSR.Create;
begin
  inherited Create(SampleRate);
  FTarget := 0.0;
  FCurrentValue := 0.0;
  FAttackRate := 0.001;
  FDecayRate := 0.001;
  FSustainLevel := 0.5;
  FReleaseRate := 0.01;
  FState := asAttack;
end;

destructor TStkADSR.Destroy;
begin
  inherited Destroy;
end;

procedure TStkADSR.KeyOn;
begin
  FTarget := 1.0;
  FRate := FAttackRate;
  FState := asAttack;
end;

procedure TStkADSR.KeyOff;
begin
  FTarget := 0.0;
  FRate := FReleaseRate;
  FState := asRelease;
end;

procedure TStkADSR.SetAttackRate(const Value: Single);
begin
  FAttackRate := Abs(Value);
end;

procedure TStkADSR.SetDecayRate(const Value: Single);
begin
  FDecayRate := Abs(Value);
end;

procedure TStkADSR.SetSustainLevel(const Value: Single);
begin
  FSustainLevel := Limit(Value, 0, Value);
end;

procedure TStkADSR.setReleaseRate(const Value: Single);
begin
  FReleaseRate := Abs(Value);
end;

procedure TStkADSR.SetAttackTime(const Value: Single);
begin
  FAttackRate := 1.0 / Abs(Value * SampleRate);
end;

procedure TStkADSR.SetDecayTime(const Value: Single);
begin
  FDecayRate := 1.0 / Abs(Value * SampleRate);
end;

procedure TStkADSR.SetReleaseTime(const Value: Single);
begin
  FReleaseRate := 1.0 / Abs(Value * SampleRate);
end;

procedure TStkADSR.SetAllTimes(const aTime, dTime, sLevel, rTime: Single);
begin
  SetAttackTime(aTime);
  SetDecayTime(dTime);
  SetSustainLevel(sLevel);
  SetReleaseTime(rTime);
end;

procedure TStkADSR.TargetChanged;
begin
  if (FCurrentValue < FTarget) then
  begin
    FState := asAttack;
    SetSustainLevel(FTarget);
    FRate := FAttackRate;
  end;
  if (FCurrentValue > FTarget) then
  begin
    SetSustainLevel(FTarget);
    FState := asDecay;
    FRate := FDecayRate;
  end;
end;

procedure TStkADSR.SetValue(Value: Single);
begin
  FState := asSustain;
  FTarget := Value;
  FCurrentValue := Value;
  SetSustainLevel(Value);
  FRate := 0.0;
end;

function TStkADSR.GetState: TADSRStates;
begin
  Result := FState;
end;

function TStkADSR.Tick: Single;
begin
  case FState of
    asAttack:
      begin
        FCurrentValue := FCurrentValue + FRate;
        if (FCurrentValue >= FTarget) then
        begin
          FCurrentValue := FTarget;
          FRate := FDecayRate;
          FTarget := FSustainLevel;
          FState := asDecay;
        end;
      end;
    asDecay:
      begin
        FCurrentValue := FCurrentValue - FDecayRate;
        if (FCurrentValue <= FSustainLevel) then
        begin
          FCurrentValue := FSustainLevel;
          FRate := 0.0;
          FState := asSustain;
        end;
      end;
    asRelease:
      begin
        FCurrentValue := FCurrentValue - FReleaseRate;
        if (FCurrentValue <= 0.0) then
        begin
          FCurrentValue := 0.0;
          FState := asDone;
        end;
      end;
  end;
  Result := FCurrentValue;
end;

end.
