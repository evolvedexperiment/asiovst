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

unit DAV_GuiAudioDataDisplayCursor;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

type
  TCustomGuiAudioDataDisplayCursor = class(TPersistent)
  private
    FSampleActive: Int64;
    FSamplePassive: Int64;
    FOnChanged: TNotifyEvent;
    procedure SetSampleActive(const Value: Int64);
    procedure SetSamplePassive(const Value: Int64);
    function GetSampleLower: Int64;
    function GetSampleUpper: Int64;
  protected
    procedure CursorChanged; virtual;
    procedure SampleActiveChanged; virtual;
    procedure SamplePassiveChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property SampleActive: Int64 read FSampleActive write SetSampleActive;
    property SamplePassive: Int64 read FSamplePassive write SetSamplePassive;
    property SampleLower: Int64 read GetSampleLower;
    property SampleUpper: Int64 read GetSampleUpper;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TGuiAudioDataDisplayCursor = class(TCustomGuiAudioDataDisplayCursor)
  published
    property SampleActive;
    property SamplePassive;
    property SampleLower;
    property SampleUpper;
  end;

implementation

{ TCustomGuiAudioDataDisplayCursor }

procedure TCustomGuiAudioDataDisplayCursor.SetSampleActive(const Value: Int64);
begin
  if FSampleActive <> Value then
  begin
    FSampleActive := Value;
    SampleActiveChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplayCursor.SetSamplePassive(const Value: Int64);
begin
  if FSamplePassive <> Value then
  begin
    FSamplePassive := Value;
    SamplePassiveChanged;
  end;
end;

function TCustomGuiAudioDataDisplayCursor.GetSampleLower: Int64;
begin
  if FSampleActive < FSamplePassive then
    result := FSampleActive
  else
    result := FSamplePassive;
end;

function TCustomGuiAudioDataDisplayCursor.GetSampleUpper: Int64;
begin
  if FSampleActive > FSamplePassive then
    result := FSampleActive
  else
    result := FSamplePassive;
end;

procedure TCustomGuiAudioDataDisplayCursor.SampleActiveChanged;
begin
  CursorChanged;
end;

procedure TCustomGuiAudioDataDisplayCursor.SamplePassiveChanged;
begin
  CursorChanged;
end;

procedure TCustomGuiAudioDataDisplayCursor.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomGuiAudioDataDisplayCursor then
    with TCustomGuiAudioDataDisplayCursor(Dest) do
    begin
      FSampleActive := Self.FSampleActive;
      FSamplePassive := Self.FSamplePassive;
      FOnChanged := Self.FOnChanged;
    end
  else
    inherited;
end;

procedure TCustomGuiAudioDataDisplayCursor.CursorChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

end.
