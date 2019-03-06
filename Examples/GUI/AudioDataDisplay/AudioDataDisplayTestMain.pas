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

unit AudioDataDisplayTestMain;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, DAV_Classes, DAV_AudioData, DAV_GuiAudioDataDisplay,
  DAV_GuiPixelMap;

type
  TFmAudioDataDisplay = class(TForm)
    ADC: TAudioDataCollection32;
    ADD1: TGuiAudioDataDisplay;
    ADD2: TGuiAudioDataDisplay;
    ADD3: TGuiAudioDataDisplay;
    ADD4: TGuiAudioDataDisplay;
    Axis: TTabSheet;
    CbTransparent: TCheckBox;
    GuiAudioDataDisplay1: TGuiAudioDataDisplay;
    LbLineWidth: TLabel;
    PC: TPageControl;
    TbLineWidth: TTrackBar;
    TsBasic: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure TbLineWidthChange(Sender: TObject);
  private
    FBackground: TGuiCustomPixelMap;
  end;

var
  FmAudioDataDisplay: TFmAudioDataDisplay;

implementation

uses
  DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmAudioDataDisplay.CbTransparentClick(Sender: TObject);
begin
  ADD1.Transparent := CbTransparent.Checked;
  ADD2.Transparent := CbTransparent.Checked;
  ADD3.Transparent := CbTransparent.Checked;
  ADD4.Transparent := CbTransparent.Checked;
end;

procedure TFmAudioDataDisplay.FormCreate(Sender: TObject);
begin
  FBackground := TGuiPixelMapMemory.Create;

  ADC.GenerateWhiteNoise(1);
  ADD1.Transparent := True;
  ADD2.Transparent := True;
  ADD3.Transparent := True;
  ADD4.Transparent := True;
end;

procedure TFmAudioDataDisplay.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBackground);
end;

procedure TFmAudioDataDisplay.FormPaint(Sender: TObject);
begin
  if Assigned(FBackground) then
    FBackground.PaintTo(Canvas);
end;

procedure TFmAudioDataDisplay.FormResize(Sender: TObject);
var
  x, y: Integer;
  s: array [0 .. 1] of Single;
  h, hr: Single;
  ScnLn: PPixel32Array;
begin
  with FBackground do
  begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    hr := 1 / Height;
    for y := 0 to Height - 1 do
    begin
      ScnLn := Scanline[y];
      h := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
      for x := 0 to Width - 1 do
      begin
        s[1] := 0.97 * s[0] + 0.03 * Random;
        s[0] := s[1];

        ScnLn[x].B := Round($70 - $34 * (s[1] - h));
        ScnLn[x].G := Round($84 - $48 * (s[1] - h));
        ScnLn[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmAudioDataDisplay.TbLineWidthChange(Sender: TObject);
begin
  ADD1.LineWidth := TbLineWidth.Position;
  ADD2.LineWidth := TbLineWidth.Position;
  ADD3.LineWidth := TbLineWidth.Position;
  ADD4.LineWidth := TbLineWidth.Position;
end;

end.
