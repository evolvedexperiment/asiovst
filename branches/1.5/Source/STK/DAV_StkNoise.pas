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

unit DAV_StkNoise;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK noise generator.

  Generic random number generation using the random function.
  The quality of the random function varies from one OS to another.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon;

type
  TStkNoise = class(TStk)
  protected
    FLastOutput: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Return a random number between -1.0 and 1.0 using rand().
    function Tick: Single; overload; virtual;

    // Return VectorSize random numbers between -1.0 and 1.0 in \ Vector.
    procedure Tick(const Data: PDavSingleFixedArray;
      const SampleFrames: Integer); overload;

    property LastOutput: Single read FLastOutput;
  end;

implementation

constructor TStkNoise.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FLastOutput := 0.0;
end;

destructor TStkNoise.Destroy;
begin
  inherited Destroy;
end;

function TStkNoise.Tick: Single;
begin
  FLastOutput := (2.0 * random) - 1;
  Result := FLastOutput;
end;

procedure TStkNoise.Tick(const Data: PDavSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Data^[Sample] := Tick;
end;

end.
