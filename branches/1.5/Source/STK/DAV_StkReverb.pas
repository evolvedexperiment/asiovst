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

unit DAV_StkReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_StkCommon;

type
  TStkReverb = class(TStk)
  private
    procedure SetEffectMix(const Value: Single);
    function GetLastOutput: Single;
  protected
    FLastOutput: array [0 .. 1] of Single;
    FEffectMix: Single;
    function IsPrime(const Number: Integer): Boolean;
    procedure EffectMixChanged; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); override;
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear; virtual; abstract;

    // Abstract Tick function ... must be implemented in subclasses.
    function Tick(const Input: Single): Single; overload; virtual;

    // Take VectorSize inputs, compute the same Number of outputs and return them in \e Vector.
    procedure Tick(const Input: PDAVSingleFixedArray;
      out Output: PDAVSingleFixedArray; const SampleFrames: Cardinal);
      overload; virtual;

    property EffectMix: Single read FEffectMix write SetEffectMix;
    // (0.0 = input only, 1.0 = reverb only).
    property LastOutputLeft: Single read FLastOutput[0];
    // Return the last left output value.
    property LastOutputRight: Single read FLastOutput[1];
    // Return the last right output value.
    property LastOutput: Single read GetLastOutput;
    // Return the last output value.
  end;

implementation

constructor TStkReverb.Create(const SampleRate: Single = 44100);
begin
  inherited Create(SampleRate);
end;

destructor TStkReverb.Destroy;
begin
  inherited Destroy;
end;

procedure TStkReverb.SetEffectMix(const Value: Single);
begin
  if EffectMix <> Value then
  begin
    FEffectMix := Value;
    EffectMixChanged;
  end;
end;

procedure TStkReverb.EffectMixChanged;
begin
  // nothing in here yet...
end;

function TStkReverb.GetLastOutput: Single;
begin
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkReverb.IsPrime(const Number: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (Number = 2) then
  begin
    Result := True;
    Exit;
  end;
  if (Number and 1 > 0) then
  begin
    i := 3;
    repeat
      if ((Number mod i) = 0) then
        Exit;
      i := i + 2;
    until (i >= Round(Sqrt(Number) + 1));
    Result := True;
  end;
end;

function TStkReverb.Tick(const Input: Single): Single;
begin
  Result := 0;
end;

procedure TStkReverb.Tick(const Input: PDAVSingleFixedArray;
  out Output: PDAVSingleFixedArray; const SampleFrames: Cardinal);
var
  i: Integer;
begin
  for i := 0 to SampleFrames - 1 do
    Output^[i] := Tick(Input^[i]);
end;

end.
