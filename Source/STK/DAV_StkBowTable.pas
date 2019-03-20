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

unit DAV_StkBowTable;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK bowed string table class.

  This class implements a simple bowed string non-linear function, as
  described by Smith (1986).
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, Math;

type
  TStkBowTable = class(TStk)
  private
    // Set the table FOffSet value.
    {
      The table FOffSet is a bias which controls the
      symmetry of the friction.  If you want the
      friction to vary with direction, use a non-zero
      value for the FOffSet.  The default value is zero.
    }
    procedure SetOffset(const Value: Single);

    // Set the table FSlope value.
    {
      The table FSlope controls the width of the friction
      pulse, which is related to bow force.
    }
    procedure SetSlope(aValue: Single);

  protected
    FOffSet: Single;
    FSlope: Single;
    FLastOutput: Single;
  public
    // Default constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Return the function value for \e input.
    {
      The function input represents differential
      string-to-bow velocity.
    }
    function Tick(const Input: Single): Single; overload;

    // Take \e vectorSize inputs and return the corresponding function values in \e vector.
    function Tick(vector: PSingle; vectorSize: longint): PSingle; overload;

    property LastOutput: Single read FLastOutput;
    property Offset: Single read FOffSet write SetOffset;
    property Slope: Single read FSlope write SetSlope;
  end;

implementation

constructor TStkBowTable.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FOffSet := 0.0;
  FSlope := 0.1;
end;

destructor TStkBowTable.Destroy;
begin
  inherited Destroy;
end;

procedure TStkBowTable.SetOffset;
begin
  FOffSet := Value;
end;

procedure TStkBowTable.SetSlope;
begin
  FSlope := aValue;
end;

function TStkBowTable.Tick(const Input: Single): Single;
var
  sample: Single;
begin
  // The input represents differential string vs. bow velocity.
  sample := Input + FOffSet; // add bias to input
  sample := sample * FSlope; // then scale it
  FLastOutput := Abs(sample) + 0.75;
  FLastOutput := power(FLastOutput, -4.0);

  // Set minimum friction to 0.0
  // if (FLastOutput < 0.0 ) FLastOutput := 0.0;
  // Set maximum friction to 1.0.
  if (FLastOutput > 1.0) then
    FLastOutput := 1.0;

  Result := FLastOutput;
end;

function TStkBowTable.Tick(vector: PSingle; vectorSize: longint): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
  begin
    p^ := Tick(p^);
    Inc(p);
  end;
  Result := vector;
end;

end.
