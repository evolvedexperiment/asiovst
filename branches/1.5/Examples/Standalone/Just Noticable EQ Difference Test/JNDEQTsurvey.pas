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

unit JNDEQTsurvey;

interface

{$I DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiPanel, DAV_GuiLED,
  DAV_GuiGraphicControl;

type
  TFormSurvey = class(TForm)
    EditAge: TEdit;
    EditSetup: TEdit;
    LabelAge: TGuiLabel;
    LabelGender: TGuiLabel;
    LabelGenderFemale: TGuiLabel;
    LabelGenderMale: TGuiLabel;
    LabelOK: TGuiLabel;
    LabelSetup: TGuiLabel;
    LEDGenderFemale: TGuiLED;
    LEDGenderMale: TGuiLED;
    PanelAge: TGuiPanel;
    PanelOK: TGuiPanel;
    PanelSetup: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LabelGenderMaleClick(Sender: TObject);
    procedure LabelGenderFemaleClick(Sender: TObject);
    procedure PanelOKClick(Sender: TObject);
  private
    FBackgroundBitmap: TGuiCustomPixelMap;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  DAV_GuiCommon;

procedure TFormSurvey.FormCreate(Sender: TObject);
begin
  // create background bitmap
  FBackgroundBitmap := TGuiPixelMapMemory.Create;
  FormResize(Self);
end;

procedure TFormSurvey.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBackgroundBitmap);
end;

procedure TFormSurvey.FormPaint(Sender: TObject);
begin
  if Assigned(FBackgroundBitmap) then
    FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFormSurvey.FormResize(Sender: TObject);
var
  x, y: Integer;
  s: array [0 .. 1] of Single;
  h, hr: Single;
  ScnLn: PPixel32Array;
begin
  if Assigned(FBackgroundBitmap) then
    with FBackgroundBitmap do
    begin
      SetSize(ClientWidth, ClientHeight);
      s[0] := 0;
      s[1] := 0;
      hr := 1 / Height;
      for y := 0 to Height - 1 do
      begin
        ScnLn := ScanLine[y];
        h := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
        for x := 0 to Width - 1 do
        begin
          s[1] := 0.97 * s[0] + 0.03 * Random;
          s[0] := s[1];

          ScnLn[x].B := Round($9D - $34 * (s[1] - h));
          ScnLn[x].G := Round($AE - $48 * (s[1] - h));
          ScnLn[x].R := Round($BD - $50 * (s[1] - h));
        end;
      end;
    end;
end;

procedure TFormSurvey.LabelGenderMaleClick(Sender: TObject);
begin
  LEDGenderMale.Brightness_Percent := 100;
  LEDGenderFemale.Brightness_Percent := 10;
end;

procedure TFormSurvey.PanelOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormSurvey.LabelGenderFemaleClick(Sender: TObject);
begin
  LEDGenderMale.Brightness_Percent := 10;
  LEDGenderFemale.Brightness_Percent := 100;
end;

end.