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

unit GroupBoxTestMain;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF} SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  DAV_GuiGroup, DAV_GuiPixelMap, DAV_GuiSlider, DAV_GuiCheckBox,
  DAV_GuiGraphicControl, DAV_GuiLabel;

type
  TFmGroupBoxTest = class(TForm)
    CbTransparent: TGuiControlsCheckBox;
    ColorDialog: TColorDialog;
    GroupA: TGuiGroup;
    GroupB: TGuiGroup;
    GroupC: TGuiGroup;
    GroupD: TGuiGroup;
    LbColor: TGuiLabel;
    LbOutlineWidth: TGuiLabel;
    LbRoundRadius: TGuiLabel;
    ShGroupColor: TShape;
    SlBorderWidth: TGuiSlider;
    SlRoundRadius: TGuiSlider;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SlRoundRadiusChange(Sender: TObject);
    procedure SlBorderWidthChange(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure ShGroupColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FBackground: TGuiCustomPixelMap;
  end;

var
  FmGroupBoxTest: TFmGroupBoxTest;

implementation

uses
  DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmGroupBoxTest.FormCreate(Sender: TObject);
begin
  FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmGroupBoxTest.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBackground);
end;

procedure TFmGroupBoxTest.FormPaint(Sender: TObject);
begin
  if Assigned(FBackground) then
    FBackground.PaintTo(Canvas);
end;

procedure TFmGroupBoxTest.FormResize(Sender: TObject);
var
  X, Y: Integer;
  Filter: array [0 .. 1] of Single;
  h, hr: Single;
  ScnLn: PPixel32Array;
begin
  with FBackground do
  begin
    SetSize(ClientWidth, ClientHeight);
    Filter[0] := 0;
    Filter[1] := 0;
    hr := 1 / Height;
    for Y := 0 to Height - 1 do
    begin
      ScnLn := Scanline[Y];
      h := 0.1 * (1 - Sqr(2 * (Y - Height div 2) * hr));
      for X := 0 to Width - 1 do
      begin
        Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
        Filter[0] := Filter[1];

        ScnLn[X].B := Round($70 - $34 * (Filter[1] - h));
        ScnLn[X].G := Round($84 - $48 * (Filter[1] - h));
        ScnLn[X].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

procedure TFmGroupBoxTest.ShGroupColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with ColorDialog do
  begin
    Color := ShGroupColor.Brush.Color;
    if Execute then
    begin
      ShGroupColor.Brush.Color := Color;
      GroupA.GroupColor := Color;
      GroupB.GroupColor := Color;
      GroupC.GroupColor := Color;
      GroupD.GroupColor := Color;
    end;
  end;
end;

procedure TFmGroupBoxTest.SlBorderWidthChange(Sender: TObject);
begin
  GroupA.BorderWidth := SlBorderWidth.Value;
  GroupB.BorderWidth := SlBorderWidth.Value;
  GroupC.BorderWidth := SlBorderWidth.Value;
  GroupD.BorderWidth := SlBorderWidth.Value;
end;

procedure TFmGroupBoxTest.SlRoundRadiusChange(Sender: TObject);
begin
  GroupA.BorderRadius := SlRoundRadius.Value;
  GroupB.BorderRadius := SlRoundRadius.Value;
  GroupC.BorderRadius := SlRoundRadius.Value;
  GroupD.BorderRadius := SlRoundRadius.Value;
end;

procedure TFmGroupBoxTest.CbTransparentClick(Sender: TObject);
begin
  GroupA.Transparent := CbTransparent.Checked;
  GroupB.Transparent := CbTransparent.Checked;
  GroupC.Transparent := CbTransparent.Checked;
  GroupD.Transparent := CbTransparent.Checked;
  SlBorderWidth.Transparent := CbTransparent.Checked;
  SlRoundRadius.Transparent := CbTransparent.Checked;
  LbOutlineWidth.Transparent := CbTransparent.Checked;
  LbRoundRadius.Transparent := CbTransparent.Checked;
  Invalidate;
end;

end.
