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

unit DAV_StkCommon;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

{$I DAV_StkConsts.inc}

type
  TStk = class
  public
    procedure SetSampleRate(const Value: Single);
  protected
    FSampleRate: Single;
    FSampleRateInv: Single;
    procedure SampleRateChanged; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); virtual;

    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

implementation

constructor TStk.Create(const SampleRate: Single = 44100);
begin
  FSampleRate := SampleRate;
  SampleRateChanged;
end;

procedure TStk.SampleRateChanged;
begin
  FSampleRateInv := 1 / FSampleRate;
end;

procedure TStk.SetSampleRate(const Value: Single);
begin
  if (Value > 0) then
  begin
    FSampleRate := Value;
    SampleRateChanged;
  end;
end;

end.
