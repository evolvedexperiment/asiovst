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

unit DAV_GuiVectorPolygon;

interface

{$I ..\DAV_Compiler.inc}

uses
  Graphics, Types, Classes, SysUtils, DAV_Common, DAV_GuiCommon,
  DAV_GuiFixedPoint, DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixel;

type
  TGuiPixelFilledPolygon = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiPolygon;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiPolygon read GetGeometricShape;
  end;

  TGuiPixelFramePolygon = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiPolygon;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiPolygon read GetGeometricShape;
  end;

implementation

uses
  DAV_GuiBlend;

{ TGuiPixelFilledPolygon }

constructor TGuiPixelFilledPolygon.Create;
begin
  inherited;
  FGeometricShape := TGuiPolygon.Create;
end;

procedure TGuiPixelFilledPolygon.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y: Integer;
  XRange: array [0 .. 1] of Integer;
  YRange: array [0 .. 1] of Integer;
  ScnLne: PPixel32Array;
  PixelColor32: TPixel32;
begin
  PixelColor32 := ConvertColor(Color);
  PixelColor32.A := Alpha;
end;

procedure TGuiPixelFilledPolygon.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
var
  X, Y: Integer;
  XRange: array [0 .. 1] of Integer;
  YRange: array [0 .. 1] of Integer;
  XAntiAlias: array [0 .. 1] of Integer;
  ScnLne: PPixel32Array;
  PixelColor32: TPixel32;
  CurrentAlpha: Byte;
begin
  PixelColor32 := ConvertColor(Color);
end;

function TGuiPixelFilledPolygon.GetGeometricShape: TGuiPolygon;
begin
  Result := TGuiPolygon(FGeometricShape);
end;

{ TGuiPixelFramePolygon }

constructor TGuiPixelFramePolygon.Create;
begin
  inherited;
  FGeometricShape := TGuiPolygon.Create;
end;

function TGuiPixelFramePolygon.GetGeometricShape: TGuiPolygon;
begin
  Result := TGuiPolygon(FGeometricShape)
end;

procedure TGuiPixelFramePolygon.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y: Integer;
  XRange: array [0 .. 1] of Integer;
  YRange: array [0 .. 1] of Integer;
  XAntiAlias: array [0 .. 1] of Integer;
  ScnLne: PPixel32Array;
  PixelColor32: TPixel32;
begin
  PixelColor32 := ConvertColor(Color);
  PixelColor32.A := Alpha;
end;

procedure TGuiPixelFramePolygon.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin
  DrawDraftShape(PixelMap);
end;

end.
