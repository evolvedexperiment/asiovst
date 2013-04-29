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

unit DAV_StkJetTable;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK Jet Table class.

  This class implements a flue jet non-linear function, computed by a
  polynomial calculation. Contrary to the name, this is not a "table".

  Consult Fletcher and Rossing, Karjalainen, Cook, and others for more
  information.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon;

type
  TStkJetTable = class(TStk)
  protected
    FLastOutput: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    function Tick(const Input: Single): Single; overload;
    function Tick(Vector: PSingle; VectorSize: Integer): PSingle; overload;

    property LastOutput: Single read FLastOutput;
  end;

implementation

constructor TStkJetTable.Create;
begin
  inherited Create(SampleRate);
  FLastOutput := 0.0;
end;

destructor TStkJetTable.Destroy;
begin
  inherited Destroy;
end;

function TStkJetTable.Tick(const Input: Single): Single;
begin
  // Perform "table lookup" using a polynomial
  // calculation (x^3 - x), which approximates
  // the jet sigmoid behavior.
  FLastOutput := Input * (Input * Input - 1.0);

  // Saturate at +/- 1.0.
  if (FLastOutput > 1.0) then
    FLastOutput := 1.0;
  if (FLastOutput < -1.0) then
    FLastOutput := -1.0;
  Result := FLastOutput;
end;

function TStkJetTable.Tick(Vector: PSingle; VectorSize: Integer): PSingle;
var
  i: Integer;
  p: PSingle;
begin
  p := Vector;
  for i := 0 to VectorSize - 1 do
  begin
    p^ := Tick(p^);
    Inc(p);
  end;
  Result := Vector;
end;

end.
