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
{   SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/       }
{                                                                              }
{******************************************************************************}

unit SECorrelationMeterGUI;

interface

uses
  Windows, Classes, Controls, DAV_SEModule, DAV_SEGUI, DAV_GuiCorrelationMeter,
  SECorrelationMeterModule;

const
  pinEnumOut = 2;

type
  TSECorrelationMeterGui = class(TSEGUIBase)
  private
    FCorrelationMeter: TGuiCorrelationMeter;
    function Handle: THandle;
    function InvalidateControl: Integer;
  protected
    procedure GuiWindowOpen(WI: PSEWndInfo); override;
    procedure GuiPaint(hDC: hDC; WI: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer;
      AData: Pointer); override;
    procedure GuiWindowClose(WI: PSEWndInfo); override;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback;
      AHostPtr: Pointer); override;
    destructor Destroy; override;
  end;

implementation

uses
  DAV_Types, SysUtils, Graphics, DAV_GuiBaseControl;

constructor TSECorrelationMeterGui.Create(SEGuiCallback: TSEGuiCallback;
  AHostPtr: Pointer);
begin
  inherited;
  CallHost(seGuiHostSetWindowSize, 64, 64);
  CallHost(seGuiHostSetWindowType, 1);
  // 0 = Draw on SE's window (default), 1 = HWND based

  // CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE));
end;

destructor TSECorrelationMeterGui.Destroy;
begin
  if Assigned(FCorrelationMeter) then
    FreeAndNil(FCorrelationMeter);
  inherited;
end;

procedure TSECorrelationMeterGui.GuiWindowClose(WI: PSEWndInfo);
begin
  if Assigned(FCorrelationMeter) then
    FreeAndNil(FCorrelationMeter);
  inherited;
end;

procedure TSECorrelationMeterGui.GuiWindowOpen(WI: PSEWndInfo);
begin
  FCorrelationMeter := TGuiCorrelationMeter.Create(nil);
  with FCorrelationMeter do
  begin
    ParentWindow := HWND(CallHost(seGuiHostGetWindowHandle, WI.ContextHandle));
    Align := alClient;
  end;
  inherited;
end;

procedure TSECorrelationMeterGui.GuiPaint(hDC: hDC; WI: PSEWndInfo);
begin
  if Assigned(FCorrelationMeter) then
    with FCorrelationMeter do
    begin
      if Width <> WI.Width then
        Width := WI.Width;
      if Height <> WI.Height then
        Height := WI.Height;
    end;
end;

procedure TSECorrelationMeterGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
begin
  inherited;
  case CurrentPin.PinIndex of
    0:
      FCorrelationMeter.Direction := TCorrelationMeterDirection
        (CurrentPin.ValueAsInteger);
    1:
      FCorrelationMeter.Correlation := CurrentPin.ValueAsSingle;
  end;
end;

procedure TSECorrelationMeterGui.GuiModuleMsg(AUserMsgID, ALength: Integer;
  AData: Pointer);
begin
  InvalidateControl;
end;

function TSECorrelationMeterGui.Handle: THandle;
begin
  Result := CallHost(seGuiHostGetHandle);
end;

function TSECorrelationMeterGui.InvalidateControl: Integer;
begin
  Result := CallHost(seGuiHostRequestRepaint);
end;

end.
