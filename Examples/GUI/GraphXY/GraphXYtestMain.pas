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

unit GraphXYtestMain;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiCustomControl, DAV_GuiGraphXY, DAV_GuiPixelMap;

type
  TFmGraphXY = class(TForm)
    GraphXYA: TGuiGraphXY;
    GraphXYB: TGuiGraphXY;
    GraphXYC: TGuiGraphXY;
    GraphXYD: TGuiGraphXY;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBackground: TGuiCustomPixelMap;
  public
    function SimpleFunctionEvaluate(Sender: TObject; X: Double): Double;
  end;

var
  FmGraphXY: TFmGraphXY;

implementation

uses
  DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmGraphXY.FormCreate(Sender: TObject);
begin
  FBackground := TGuiPixelMapMemory.Create;
  TGuiGraphXYFunctionSeries(GraphXYA[0].Series).OnEvaluate :=
    SimpleFunctionEvaluate;
  TGuiGraphXYFunctionSeries(GraphXYB[0].Series).OnEvaluate :=
    SimpleFunctionEvaluate;
  TGuiGraphXYFunctionSeries(GraphXYC[0].Series).OnEvaluate :=
    SimpleFunctionEvaluate;
  TGuiGraphXYFunctionSeries(GraphXYD[0].Series).OnEvaluate :=
    SimpleFunctionEvaluate;
end;

procedure TFmGraphXY.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBackground);
end;

procedure TFmGraphXY.FormPaint(Sender: TObject);
begin
  if Assigned(FBackground) then
    FBackground.PaintTo(Canvas);
end;

procedure TFmGraphXY.FormResize(Sender: TObject);
var
  X, y: Integer;
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
    for y := 0 to Height - 1 do
    begin
      ScnLn := Scanline[y];
      h := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
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

procedure TFmGraphXY.FormShow(Sender: TObject);
begin
  GraphXYA.UpdateGraph;
  GraphXYB.UpdateGraph;
  GraphXYC.UpdateGraph;
  GraphXYD.UpdateGraph;
end;

function TFmGraphXY.SimpleFunctionEvaluate(Sender: TObject; X: Double): Double;
begin
  Result := X * Sqr(X) * 0.1;
end;

end.
