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

unit DAV_StkInstrument;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK instrument abstract base class.

  This class provides a common interface for all STK instruments.
}
interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon;

type
  TStkInstrument = class(TStk)
  protected
    FLastOutput: Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Value: Single); virtual; abstract;
    function GetFrequency: Single; virtual; abstract;
  public
    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); virtual; abstract;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); virtual; abstract;

    // Compute one output sample.
    function Tick: Single; overload; virtual;

    // Computer VectorSize outputs and return them in Vector.
    procedure Tick(const Data: PDavSingleFixedArray;
      const SampleFrames: Integer); overload; virtual;

    property LastOutput: Single read FLastOutput;
    property Frequency: Single read GetFrequency write SetFrequency;
  end;

  TStkControlableInstrument = class(TStkInstrument)
  public
    // Perform the control change specified by number and value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single);
      virtual; abstract;
  end;

implementation

function TStkInstrument.Tick: Single;
begin
  Result := 0;
end;

procedure TStkInstrument.Tick(const Data: PDavSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Data^[Sample] := Tick;
end;

end.
