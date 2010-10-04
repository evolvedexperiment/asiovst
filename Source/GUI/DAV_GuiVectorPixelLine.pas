unit DAV_GuiVectorPixelLine;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LResources, LMessages,
  {$IFDEF Windows} Windows, {$ENDIF}
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_MemoryUtils, DAV_GuiCommon,
  DAV_GuiFixedPoint, DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixel;

type
  TGuiPixelThinLine = class(TCustomGuiPixelSimplePrimitive)
  private
    function GetGeometricShape: TGuiLine;
  protected
    procedure DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiLine read GetGeometricShape;
  end;

  TGuiPixelLine = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiLine;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiLine read GetGeometricShape;
  end;

implementation

uses
  Math, DAV_GuiBlend;

{ TGuiPixelThinLine }

constructor TGuiPixelThinLine.Create;
begin
 inherited;
 FGeometricShape := TGuiLine.Create;
end;

destructor TGuiPixelThinLine.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

function TGuiPixelThinLine.GetGeometricShape: TGuiLine;
begin
 Result := TGuiLine(FGeometricShape);
end;

procedure TGuiPixelThinLine.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  x, y, x2, y2 : Integer;
  t, dx, dy    : Integer;
  incx, incy   : Integer;
  pdx, pdy     : Integer;
  ddx, ddy     : Integer;
  es, el, err  : Integer;
  DataPointer  : PPixel32Array;
  PixelColor32 : TPixel32;
begin
 with GeometricShape do
  begin
   x := ConvertFromFixed24Dot8PointToInteger(XA);
   y := ConvertFromFixed24Dot8PointToInteger(YA);
   x2 := ConvertFromFixed24Dot8PointToInteger(XB);
   y2 := ConvertFromFixed24Dot8PointToInteger(YB);
  end;

 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;
 DataPointer := PixelMap.DataPointer;

 // ensure the line goes always from left to right
 if x2 > x then
  begin
   Exchange32(x, x2);
   Exchange32(y, y2);
  end;

 if y = y2 then
  begin
   // check whether the y-coordinate is outside the pixel map
   if (y < 0) or (y >= PixelMap.Height) then Exit;

   // limit x-coordinate to values inside the pixel map
   x := Limit(x, 0, PixelMap.Width - 1);
   x2 := Limit(x2, 0, PixelMap.Width - 1);

   // check if all coordinates are equal
   if x = x2 then
    try
     BlendPixelInplace(PixelColor32, DataPointer[y2 * PixelMap.Width + x2]);
     Exit;
    finally
     EMMS;
    end;

   // draw horizontal line
   BlendPixelLine(PixelColor32, @DataPointer[y2 * PixelMap.Width + x], x2 - x)
  end else
 if x = x2 then
  begin
   // check whether the x-coordinate is outside the pixel map
   if (x < 0) or (x >= PixelMap.Width) then Exit;

   // limit y-coordinate to values inside the pixel map
   y := Limit(y, 0, PixelMap.Height - 1);
   y2 := Limit(y2, 0, PixelMap.Height - 1);

   // check if all coordinates are equal
   if y = y2 then
    try
     BlendPixelInplace(PixelColor32, DataPointer[y2 * PixelMap.Width + x2]);
     Exit;
    finally
     EMMS;
    end;

   // draw vertical line
   PixelMap.VerticalLine(x, y, y2, PixelColor32);
  end
 else
  try
   // calculate length in x and y coordinates
   dx := x2 - x;
   dy := y2 - y;

   // check whether x-coordinate is outside the pixel map
   if (x < 0) then
    begin
     // check if line needs to be drawn at all
     if x2 < 0 then Exit;

     // calculate new left offset
     y := Round(y + (-x / dx) * dy);
     x := 0;

     // recalculate length in x and y coordinates
     dx := x2;
     dy := y2 - y;
    end else
   if (x >= PixelMap.Width) then
    begin
     // check if line needs to be drawn at all
     if x2 >= PixelMap.Width then Exit;

     // calculate new left offset
     y := Round(y + ((PixelMap.Width - 1 - x) / dx) * dy);
     x := PixelMap.Width - 1;

     // recalculate length in x and y coordinates
     dx := x2 - x;
     dy := y2 - y;
    end;

   if (x2 < 0) then
    begin
     // calculate new left offset
     y2 := Round(y2 + (-x2 / dx) * dy);
     x2 := 0;

     // recalculate length in x and y coordinates
     dx :=    - x;
     dy := y2 - y;
    end else
   if (x2 >= PixelMap.Width) then
    begin
     // calculate new left offset
     y2 := Round(y2 + ((PixelMap.Width - 1 - x2) / dx) * dy);
     x2 := PixelMap.Width - 1;

     // recalculate length in x and y coordinates
     dx := x2 - x;
     dy := y2 - y;
    end;

   // check whether y-coordinate is outside the pixel map
   if (y < 0) then
    begin
     // check if line needs to be drawn at all
     if y2 < 0 then Exit;

     // calculate new left offset
     x := Round(x + (-y / dy) * dx);
     y := 0;

     // recalculate length in x and y coordinates
     dx := x2 - x;
     dy := y2;
    end else
   if (y >= PixelMap.Height) then
    begin
     // check if line needs to be drawn at all
     if y2 >= PixelMap.Height then Exit;

     // calculate new left offset
     x := Round(x + ((PixelMap.Height - 1 - y) / dy) * dx);
     y := PixelMap.Height - 1;

     // recalculate length in x and y coordinates
     dx := x2 - x;
     dy := y2 - y;
    end;

   if (y2 < 0) then
    begin
     // recalculate new length in x and y coordinates
     dx := Round(x2 + (-y2 / dy) * dx) - x;
     dy :=                               - y;
    end else
   if (y2 >= PixelMap.Height) then
    begin
     // recalculate new length in x and y coordinates
     dx := Round(x2 + ((PixelMap.Height - 1 - y2) / dy) * dx) - x;
     dy := (PixelMap.Height - 1) - y;
    end;

   incx := Sign(dx);
   incy := Sign(dy);
   if (dx < 0) then dx := -dx;
   if (dy < 0) then dy := -dy;

   if (dx > dy) then
    begin
     pdx := incx;
     pdy := 0;
     ddx := incx;
     ddy := incy;
     es  := dy;
     el  := dx;
    end
   else
    begin
     pdx := 0;
     pdy := incy;
     ddx := incx;
     ddy := incy;
     es  := dx;
     el  := dy;
    end;

   err := el shr 1;
   BlendPixelInplace(PixelColor32, DataPointer[Y * PixelMap.Width + x]);

   for t := 1 to el - 1 do
    begin
     err := err - es;
     if (err < 0) then
      begin
       err := err + el;
       x := x + ddx;
       y := y + ddy;
      end
     else
      begin
       x := x + pdx;
       y := y + pdy;
      end;
     BlendPixelInplace(PixelColor32, DataPointer[Y * PixelMap.Width + x]);
    end;
  finally
   EMMS;
  end;
end;

procedure TGuiPixelThinLine.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin
 DrawFloatingPoint(PixelMap);
end;

procedure TGuiPixelThinLine.DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
var
  FromX, ToX   : Double;
  FromY, ToY   : Double;
  dx, dy, t    : Double;
  Gradient     : Double;
  FltEnd       : Double;
  gap          : Double;
  Inter        : Double;
  Index        : Integer;
  IntEnd       : Integer;
  xpxl1, ypxl1 : Integer;
  xpxl2, ypxl2 : Integer;
  DataPointer  : PPixel32Array;
  PixelColor32 : TPixel32;
const
  COne255th : Double = 1 / 255;
begin
 with GeometricShape do
  begin
   FromX := XA.Fixed * COne255th;
   FromY := YA.Fixed * COne255th;
   ToX := XB.Fixed * COne255th;
   ToY := YB.Fixed * COne255th;
  end;

 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 DataPointer := PixelMap.DataPointer;

 dx := ToX - FromX;
 dy := ToY - FromY;
 if Abs(dx) > Abs(dy) then
  begin
   // draw lines from left to right
   if ToX < FromX then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;

   Gradient := dy / dx;
   IntEnd   := Round(FromX);
   FltEnd   := FromY + gradient * (IntEnd - FromX);
   gap      := 1 - Frac(FromX + 0.5);
   xpxl1    := IntEnd;
   ypxl1    := Trunc(FltEnd);

   if (xpxl1 >= 0) and (xpxl1 < PixelMap.Width) then
    begin
     if (Trunc(ypxl1) >= 0) and (Trunc(ypxl1) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl1) * PixelMap.Width + xpxl1], Round($FF * (1 - Frac(FltEnd)) * gap));
     if (Trunc(ypxl1 + 1) >= 0) and (Trunc(ypxl1 + 1) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl1 + 1) * PixelMap.Width + xpxl1], Round($FF * Frac(FltEnd) * gap));
    end;
   Inter := FltEnd + gradient;

   IntEnd := Round(ToX);
   FltEnd := ToY + gradient * (IntEnd - ToX);
   gap := Frac(ToX + 0.5);
   xpxl2 := IntEnd;
   ypxl2 := Trunc(FltEnd);

   if (xpxl2 >= 0) and (xpxl2 < PixelMap.Width) then
    begin
     if (Trunc(ypxl2) >= 0) and (Trunc(ypxl2) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl2) * PixelMap.Width + xpxl2], Round($FF * (1 - Frac(FltEnd)) * gap)); // weight = (1 - Frac(FltEnd)) * gap
     if (Trunc(ypxl2 + 1) >= 0) and (Trunc(ypxl2 + 1) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl2 + 1) * PixelMap.Width + xpxl2], Round($FF * (Frac(FltEnd) * gap)));
    end;

   for Index := xpxl1 + 1 to xpxl2 - 1 do
    begin

     if (Index >= 0) and (Index < PixelMap.Width) then
      begin
       if (Trunc(Inter) >= 0) and (Trunc(Inter) < PixelMap.Height)
        then CombinePixelInplace(PixelColor32, TPixel32(DataPointer[Trunc(Inter) * PixelMap.Width + Index]), Round((1 - Frac(Inter)) * 255));
       if (Trunc(Inter + 1) >= 0) and (Trunc(Inter + 1) < PixelMap.Height)
        then CombinePixelInplace(PixelColor32, TPixel32(DataPointer[Trunc(Inter + 1) * PixelMap.Width + Index]), Round(Frac(Inter) * 255));
      end;

     Inter := Inter + gradient;
    end;
  end
 else
  begin
   if ToY < FromY then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dx / dy;
   IntEnd   := Round(FromY);
   FltEnd   := FromX + gradient * (IntEnd - FromY);
   gap      := 1 - Frac(FromY + 0.5);
   ypxl1    := IntEnd;
   xpxl1    := Trunc(FltEnd);

   if (xpxl1 >= 0) and (xpxl1 < PixelMap.Width) then
    begin
     if (Trunc(ypxl1) >= 0) and (Trunc(ypxl1) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl1) * PixelMap.Width + xpxl1], Round($FF * (1 - Frac(FltEnd)) * gap));
     if (Trunc(ypxl1 + 1) >= 0) and (Trunc(ypxl1 + 1) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl1 + 1) * PixelMap.Width + xpxl1], Round($FF * Frac(FltEnd) * gap));
    end;
   Inter := FltEnd + gradient;

   IntEnd := Round(ToY);
   FltEnd := ToX + gradient * (IntEnd - ToY);
   gap := Frac(ToY + 0.5);
   ypxl2 := IntEnd;
   xpxl2 := Trunc(FltEnd);

   if (xpxl2 >= 0) and (xpxl2 < PixelMap.Width) then
    begin
     if (Trunc(ypxl2) >= 0) and (Trunc(ypxl2) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl2) * PixelMap.Width + xpxl2], Round($FF * (1 - Frac(FltEnd)) * gap));
     if (Trunc(ypxl2 + 1) >= 0) and (Trunc(ypxl2 + 1) < PixelMap.Height)
      then CombinePixelInplace(PixelColor32, DataPointer[Trunc(ypxl2 + 1) * PixelMap.Width + xpxl2], Round($FF * Frac(FltEnd) * gap));
    end;

   for Index := ypxl1 + 1 to ypxl2 - 1 do
    begin
     if (Index >= 0) and (Index < PixelMap.Height) then
      begin
       if (Trunc(Inter) >= 0) and (Trunc(Inter) < PixelMap.Width)
        then CombinePixelInplace(PixelColor32, DataPointer[Index * PixelMap.Width + Trunc(Inter)], Round($FF * (1 - Frac(Inter))));
       if (Trunc(Inter + 1) >= 0) and (Trunc(Inter + 1) < PixelMap.Width)
        then CombinePixelInplace(PixelColor32, DataPointer[Index * PixelMap.Width + Trunc(Inter + 1)], Round($FF * Frac(Inter)));
      end;
     Inter := Inter + Gradient;
    end;
  end;
end;


{ TGuiPixelLine }

constructor TGuiPixelLine.Create;
begin
 inherited;
 FGeometricShape := TGuiLine.Create;
end;

destructor TGuiPixelLine.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

function TGuiPixelLine.GetGeometricShape: TGuiLine;
begin
 Result := TGuiLine(FGeometricShape);
end;

procedure TGuiPixelLine.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiPixelLine.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin

end;


{
procedure TGuiCustomPixelMap.Line(FromX, FromY, ToX, ToY: TFixedPoint; Color: TPixel32);
var
  Index        : Integer;
  dx, dy, t    : TFixedPoint;
  Gradient     : Single;
  FltEnd       : TFixedPoint;
  IntEnd       : Integer;
  gap          : TFixedPoint;
  xpxl1, ypxl1 : Integer;
  xpxl2, ypxl2 : Integer;
  Inter        : TFixedPoint;
  Offset       : TFixedPoint;
begin
 dx := ToX - FromX;
 dy := ToY - FromY;
 if Abs(dx) > Abs(dy) then
  begin
   if ToX < FromX then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dy / dx;
   Offset   := Round(Gradient * CFixedPointOne);
   IntEnd   := (FromX + CFixedPointHalf) shr 16;
   FltEnd   := FromY + Round(Gradient * ((FromX + CFixedPointHalf) and $FFFF0000 - FromX));
   gap      := CFixedPointOne - ((FromX + CFixedPointHalf) and $FFFF);
   xpxl1    := IntEnd;
   ypxl1    := FltEnd and $FFFF0000;

   FDataPointer[ (ypxl1                   shr 16) * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl1 + CFixedPointOne) shr 16) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap
   Inter := FltEnd + Offset;

   IntEnd := (ToX + CFixedPointHalf) shr 16;
   FltEnd := ToY + Round(Gradient * ((ToX + CFixedPointHalf) and $FFFF0000 - ToX));
   gap := (ToX + CFixedPointHalf) and $FFFF;
   xpxl2 := IntEnd;
   ypxl2 := FltEnd and $FFFF0000;
   FDataPointer[ (ypxl2                   shr 16) * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl2 + CFixedPointOne) shr 16) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := xpxl1 + 1 to xpxl2 - 1 do
    begin
     FDataPointer[ (Inter                   shr 16) * Width + Index] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[((Inter + CFixedPointOne) shr 16) * Width + Index] := Color; // weight = Frac(Inter)
     Inter := Inter + Offset;
    end;
  end
 else
  begin
   if ToY < FromY then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dx / dy;
   Offset   := Round(Gradient * CFixedPointOne);
   IntEnd   := (FromY + CFixedPointHalf) shr 16;
   FltEnd   := FromX + Round(Gradient * ((FromY + CFixedPointHalf) and $FFFF0000 - FromY));
   gap      := CFixedPointOne - ((FromY + CFixedPointHalf) and $FFFF);
   ypxl1    := IntEnd;
   xpxl1    := FltEnd and $FFFF0000;

   FDataPointer[ (ypxl1                   shr 16) * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl1 + CFixedPointOne) shr 16) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap
   Inter := FltEnd + Offset;

   IntEnd := (ToY + CFixedPointHalf) shr 16;
   FltEnd := ToX + Round(Gradient * ((ToY + CFixedPointHalf) and $FFFF0000 - ToY));
   gap := (ToY + CFixedPointHalf) and $FFFF;
   ypxl2 := IntEnd;
   xpxl2 := FltEnd and $FFFF0000;
   FDataPointer[ (ypxl2                   shr 16) * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[((ypxl2 + CFixedPointOne) shr 16) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := ypxl1 + 1 to ypxl2 - 1 do
    begin
     FDataPointer[ (Inter                   shr 16) * Width + Index] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[((Inter + CFixedPointOne) shr 16) * Width + Index] := Color; // weight = Frac(Inter)
     Inter := Inter + Offset;
    end;
(*
*)
  end;
(*
var
  x, y, t      : TFixedPoint;
  dx, dy, xgap : TFixedPoint;
  xend, yend   : TFixedPoint;
  xpxl1, ypxl1 : TFixedPoint;
  xpxl2, ypxl2 : TFixedPoint;
  intery       : TFixedPoint;
  Gradient     : TFixedPoint;
begin
 dx := ToX - FromX;
 dy := ToY - FromY;
 if Abs(dx) < Abs(dy) then
  begin
   t := FromX;
   FromX := FromY;
   FromY := t;
   t := ToX;
   ToX := ToY;
   ToY := t;
   t := dx;
   dx := dy;
   dy := t;
  end;
 if ToX < FromX then
  begin
   t := FromX;
   FromX := ToX;
   ToX := t;
   t := FromY;
   FromY := ToY;
   ToY := t;
  end;
 Gradient := round((dy / dx) * CFixedPointOne);

 // handle first endpoint
 xend := (FromX + CFixedPointHalf) and $FFFF0000;
 yend := FromY + gradient * (xend - FromX);
 xgap := CFixedPointOne - (FromX + CFixedPointHalf) and $FFFF;
 xpxl1 := xend;  // this will be used in the main loop
 ypxl1 := yend and $FFFF0000;

 FDataPointer[(ypxl1 shr 16) * Width + (xpxl1 shr 16)] := Color; // weight = rfpart(yend) * xgap)
 FDataPointer[((ypxl1 + CFixedPointOne) shr 16) * Width + (xpxl1 shr 16)] := Color; // weight = fpart(yend) * xgap
 intery := yend + gradient; // first y-intersection for the main loop

 // handle second endpoint
 xend := (ToX + CFixedPointHalf) and $FFFF0000;
 yend := ToY + gradient * (xend - ToX);
 xgap := CFixedPointOne - (ToX + CFixedPointHalf) and $FFFF;
 xpxl2 := xend;  // this will be used in the main loop
 ypxl2 := yend and $FFFF0000;

// FDataPointer[(ypxl2 shr 16) * Width + (xpxl2 shr 16)] := Color; // weight = rfpart (yend) * xgap)
// FDataPointer[((ypxl2 + CFixedPointOne) shr 16) * Width + (xpxl2 shr 16)] := Color; // weight = fpart (yend) * xgap

 // main loop
 x := xpxl1;
 while x < xpxl2 do
  begin
   FDataPointer[(intery shr 16) * Width + (x shr 16)] := Color; // weight = rfpart (yend) * xgap)
   FDataPointer[((intery + CFixedPointOne) shr 16) * Width + (x shr 16)] := Color; // weight = fpart (yend) * xgap
   intery := intery + gradient;
   x := x + CFixedPointOne;
  end;
*)
end;

procedure TGuiCustomPixelMap.Line(FromX, FromY, ToX, ToY: Double; Color: TPixel32);
var
  Index        : Integer;
  dx, dy, t    : Double;
  Gradient     : Double;
  FltEnd       : Double;
  IntEnd       : Integer;
  gap          : Double;
  xpxl1, ypxl1 : Integer;
  xpxl2, ypxl2 : Integer;
  Inter        : Double;
begin
 dx := ToX - FromX;
 dy := ToY - FromY;
 if Abs(dx) > Abs(dy) then
  begin
   if ToX < FromX then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dy / dx;
   IntEnd   := Round(FromX);
   FltEnd   := FromY + gradient * (IntEnd - FromX);
   gap      := 1 - Frac(FromX + 0.5);
   xpxl1    := IntEnd;
   ypxl1    := Trunc(FltEnd);

   FDataPointer[Trunc(ypxl1)     * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl1 + 1) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap
   Inter := FltEnd + gradient;

   IntEnd := Round(ToX);
   FltEnd := ToY + gradient * (IntEnd - ToX);
   gap := Frac(ToX + 0.5);
   xpxl2 := IntEnd;
   ypxl2 := Trunc(FltEnd);
   FDataPointer[Trunc(ypxl2)     * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl2 + 1) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := xpxl1 + 1 to xpxl2 - 1 do
    begin
     CombineMemory(TPixel32(Color), TPixel32(FDataPointer[Trunc(Inter)     * Width + Index]), Round((1 - Frac(Inter)) * 255));
     CombineMemory(TPixel32(Color), TPixel32(FDataPointer[Trunc(Inter + 1) * Width + Index]), Round(Frac(Inter) * 255));
(*
     FDataPointer[Trunc(Inter)     * Width + Index] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[Trunc(Inter + 1) * Width + Index] := Color; // weight = Frac(Inter)
*)
     Inter := Inter + gradient;
    end;
  end
 else
  begin
   if ToY < FromY then
    begin
     t := FromX;
     FromX := ToX;
     ToX := t;
     t := FromY;
     FromY := ToY;
     ToY := t;
    end;
   Gradient := dx / dy;
   IntEnd   := Round(FromY);
   FltEnd   := FromX + gradient * (IntEnd - FromY);
   gap      := 1 - Frac(FromY + 0.5);
   ypxl1    := IntEnd;
   xpxl1    := Trunc(FltEnd);

   FDataPointer[Trunc(ypxl1)     * Width + xpxl1] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl1 + 1) * Width + xpxl1] := Color; // weight = Frac(FltEnd) * gap)
   Inter := FltEnd + gradient;

   IntEnd := Round(ToY);
   FltEnd := ToX + gradient * (IntEnd - ToY);
   gap := Frac(ToY + 0.5);
   ypxl2 := IntEnd;
   xpxl2 := Trunc(FltEnd);
   FDataPointer[Trunc(ypxl2)     * Width + xpxl2] := Color; // weight = (1 - Frac(FltEnd)) * gap
   FDataPointer[Trunc(ypxl2 + 1) * Width + xpxl2] := Color; // weight = Frac(FltEnd) * gap

   for Index := ypxl1 + 1 to ypxl2 - 1 do
    begin
     FDataPointer[Index * Width + Trunc(Inter)] := Color; // weight = (1 - Frac(Inter))
     FDataPointer[Index * Width + Trunc(Inter + 1)] := Color; // weight = Frac(Inter)
     Inter := Inter + Gradient;
    end;
  end;

(*
 Line(FloatingToFixedPoint(FromX), FloatingToFixedPoint(FromY),
   FloatingToFixedPoint(ToX), FloatingToFixedPoint(ToY), Color);
*)
end;
}

end.
