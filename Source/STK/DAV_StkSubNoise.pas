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

unit DAV_StkSubNoise;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK sub-sampled noise generator.

  Generates a new random number every "rate" ticks using the random function.
  The quality of the random function varies from one OS to another.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkNoise;

type
  TStkSubNoise = class(TStkNoise)
  private
    // Set the sub-sampling rate.
    procedure SetSubRate(const Value: Integer);
  protected
    FCounter: Integer;
    FSubRate: Integer;
  public
    // Default constructor sets sub-sample rate to 16.
    constructor Create(const SampleRate: Single; const subRate: Integer);
      reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Return a sub-sampled random number between -1.0 and 1.0.
    function Tick: Single; override;

    property subRate: Integer read FSubRate write SetSubRate;
  end;

implementation

constructor TStkSubNoise.Create;
begin
  inherited Create(SampleRate);
  FSubRate := subRate;
  FCounter := FSubRate;
end;

destructor TStkSubNoise.Destroy;
begin
  inherited Destroy;
end;

procedure TStkSubNoise.SetSubRate(const Value: Integer);
begin
  if (Value > 0) then
    FSubRate := Value;
end;

function TStkSubNoise.Tick: Single;
begin
  FCounter := FCounter + 1;
  if (FCounter > FSubRate) then
  begin
    inherited Tick;
    FCounter := 1;
  end;
  Result := lastOutput;
end;

end.
