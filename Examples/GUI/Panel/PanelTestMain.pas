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

unit PanelTestMain;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiPanel, DAV_GuiSlider,
  DAV_GuiCheckBox, DAV_GuiGraphicControl, DAV_GuiLabel, DAV_GuiBackgrounds;

type
  TFmPanelTest = class(TForm)
    CbTransparent: TGuiControlsCheckBox;
    LbLineWidth: TGuiLabel;
    LbRoundRadius: TGuiLabel;
    PanelA: TGuiPanel;
    PanelB: TGuiPanel;
    PanelC: TGuiPanel;
    PanelD: TGuiPanel;
    SlLineWidth: TGuiSlider;
    SlRoundRadius: TGuiSlider;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure SlLineWidthChange(Sender: TObject);
    procedure SlRoundRadiusChange(Sender: TObject);
    procedure PanelAClick(Sender: TObject);
    procedure PanelBClick(Sender: TObject);
  private
    FBackground: TGuiCustomPixelMap;
  end;

var
  FmPanelTest: TFmPanelTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmPanelTest.FormCreate(Sender: TObject);
begin
  // Create Background Image
  FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmPanelTest.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBackground);
end;

procedure TFmPanelTest.FormPaint(Sender: TObject);
begin
  if Assigned(FBackground) then
    FBackground.PaintTo(Canvas);
end;

procedure TFmPanelTest.FormResize(Sender: TObject);
const
  CBaseColor: TPixel32 = (ARGB: $FF8D8470);
begin
  with FBackground do
  begin
    Width := ClientWidth;
    Height := ClientHeight;
    FillBrushedMetal(FBackground, CBaseColor, 0.5, 0.1);
  end;
end;

procedure TFmPanelTest.PanelAClick(Sender: TObject);
begin
  SlLineWidth.Value := 3;
  SlRoundRadius.Value := 3.9;
end;

procedure TFmPanelTest.PanelBClick(Sender: TObject);
begin
  SlLineWidth.Value := 6.632;
  SlRoundRadius.Value := 5.054;
end;

procedure TFmPanelTest.CbTransparentClick(Sender: TObject);
begin
  PanelA.Transparent := CbTransparent.Checked;
  PanelB.Transparent := CbTransparent.Checked;
  PanelC.Transparent := CbTransparent.Checked;
  PanelD.Transparent := CbTransparent.Checked;
end;

procedure TFmPanelTest.SlLineWidthChange(Sender: TObject);
begin
  PanelA.Borderwidth := SlLineWidth.Value;
  PanelB.Borderwidth := SlLineWidth.Value;
  PanelC.Borderwidth := SlLineWidth.Value;
  PanelD.Borderwidth := SlLineWidth.Value;
end;

procedure TFmPanelTest.SlRoundRadiusChange(Sender: TObject);
begin
  PanelA.BorderRadius := SlRoundRadius.Value;
  PanelB.BorderRadius := SlRoundRadius.Value;
  PanelC.BorderRadius := SlRoundRadius.Value;
  PanelD.BorderRadius := SlRoundRadius.Value;
end;

end.
