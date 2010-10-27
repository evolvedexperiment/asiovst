unit DAV_GuiVectorPixelRectangle;

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
  Graphics, Classes, SysUtils, DAV_Common, DAV_GuiCommon, DAV_GuiFixedPoint,
  DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixel;

type
  TGuiPixelFilledRectangle = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiRectangle read GetGeometricShape;
  end;

  TGuiPixelFrameRectangle = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiRectangle read GetGeometricShape;
  end;

  TGuiPixelFilledRoundedRectangle = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiRoundedRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiRoundedRectangle read GetGeometricShape;
  end;

  TGuiPixelFrameRoundedRectangle = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiRoundedRectangle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiRoundedRectangle read GetGeometricShape;
  end;


implementation

uses
  DAV_GuiBlend;

{ TGuiPixelFilledRectangle }

constructor TGuiPixelFilledRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRectangle.Create;
end;

destructor TGuiPixelFilledRectangle.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

procedure TGuiPixelFilledRectangle.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  XRange       : array [0..1] of Integer;
  YRange       : array [0..1] of Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   with GeometricShape do
    begin
     XRange[0] := FixedFloor(Left);
     XRange[1] := FixedCeil(Right);

     // check whether the bitmap needs to be drawn at all
     if (XRange[0] >= Width) or (XRange[1] < 0) or (XRange[0] >= XRange[1])
      then Exit;

     YRange[0] := FixedRound(Top);
     YRange[1] := FixedRound(Bottom);

     // check whether the bitmap needs to be drawn at all
     if (YRange[0] >= Height) or (YRange[1] < 0) or (YRange[0] >= YRange[1])
      then Exit;
    end;

   // eventually limit range
   if XRange[0] < 0 then XRange[0] := 0;
   if XRange[1] >= Width then XRange[1] := Width - 1;
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   for Y := YRange[0] to YRange[1] do
    begin
     ScnLne := PixelMap.Scanline[Y];
     for X := XRange[0] to XRange[1] - 1
      do BlendPixelInplace(PixelColor32, ScnLne[X]);
    end;
   EMMS;
  end;
end;

procedure TGuiPixelFilledRectangle.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  XRange       : array [0..1] of Integer;
  YRange       : array [0..1] of Integer;
  XAntiAlias   : array [0..1] of Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
  CurrentAlpha : Byte;
begin
 PixelColor32 := ConvertColor(Color);

 with PixelMap do
  begin
   with GeometricShape do
    begin
     XRange[0] := FixedFloor(Left);
     XRange[1] := FixedCeil(Right);

     // check whether the bitmap needs to be drawn at all
     if (XRange[0] >= Width) or (XRange[1] < 0) or (XRange[0] >= XRange[1])
      then Exit;

     YRange[0] := FixedFloor(Top);
     YRange[1] := FixedCeil(Bottom);

     // check whether the bitmap needs to be drawn at all
     if (YRange[0] >= Height) or (YRange[1] < 0) or (YRange[0] >= YRange[1])
      then Exit;
    end;

   // eventually limit range
   if XRange[0] < 0 then
    begin
     XRange[0] := 0;
     XAntiAlias[0] := 0;
    end else XAntiAlias[0] := 1;
   if XRange[1] >= Width then
    begin
     XRange[1] := Width - 1;
     XAntiAlias[1] := 0;
    end else XAntiAlias[1] := 1;

   if YRange[0] < 0
    then YRange[0] := 0
    else
     begin
      // draw first scanline
      ScnLne := PixelMap.Scanline[YRange[0]];
      CurrentAlpha := Fixed24Dot8Mul($FF - GeometricShape.Top.Frac, Alpha);

      // draw first pixel of this first scanline
      if XAntiAlias[0] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul($FF - GeometricShape.Left.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);
       end;

      // draw middle pixels of this first scanline
      PixelColor32.A := CurrentAlpha;
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this first scanline
      if XAntiAlias[1] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul(GeometricShape.Right.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
       end;

      // increase Y-Range (not to include this first scanline)
      Inc(YRange[0]);
     end;

   if YRange[1] >= Height
    then YRange[1] := Height - 1
    else
     begin
      // draw last scanline
      ScnLne := PixelMap.Scanline[YRange[1]];
      CurrentAlpha := Fixed24Dot8Mul(GeometricShape.Bottom.Frac, Alpha);

      // draw first pixel of this last scanline
      if XAntiAlias[0] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul($FF - GeometricShape.Left.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);
       end;

      // draw middle pixels of this last scanline
      PixelColor32.A := CurrentAlpha;
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this last scanline
      if XAntiAlias[1] <> 0 then
       begin
        PixelColor32.A := Fixed24Dot8Mul(GeometricShape.Right.Frac, CurrentAlpha);
        BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
       end;

      // increase Y-Range (not to include this last scanline)
      Dec(YRange[1]);
     end;

   for Y := YRange[0] to YRange[1] do
    begin
     ScnLne := PixelMap.Scanline[Y];

     // draw first pixel of this scanline
     if XAntiAlias[0] <> 0 then
      begin
       PixelColor32.A := Fixed24Dot8Mul($FF - GeometricShape.Left.Frac, Alpha);
       BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);
      end;

     // draw middle pixels of this scanline
     PixelColor32.A := Alpha;
     for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
      do BlendPixelInplace(PixelColor32, ScnLne[X]);

     // draw last pixel of this scanline
     if XAntiAlias[1] <> 0 then
      begin
       PixelColor32.A := Fixed24Dot8Mul(GeometricShape.Right.Frac, Alpha);
       BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
      end;
    end;
   EMMS;
  end;
end;

function TGuiPixelFilledRectangle.GetGeometricShape: TGuiRectangle;
begin
 Result := TGuiRectangle(FGeometricShape);
end;


{ TGuiPixelFrameRectangle }

constructor TGuiPixelFrameRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRectangle.Create;
end;

destructor TGuiPixelFrameRectangle.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

function TGuiPixelFrameRectangle.GetGeometricShape: TGuiRectangle;
begin
 Result := TGuiRectangle(FGeometricShape)
end;

procedure TGuiPixelFrameRectangle.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  XRange       : array [0..1] of Integer;
  YRange       : array [0..1] of Integer;
  XAntiAlias   : array [0..1] of Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
   with GeometricShape do
    begin
     XRange[0] := FixedFloor(Left);
     XRange[1] := FixedCeil(Right);

     // check whether the bitmap needs to be drawn at all
     if (XRange[0] >= Width) or (XRange[1] < 0) or (XRange[0] >= XRange[1])
      then Exit;

     YRange[0] := FixedFloor(Top);
     YRange[1] := FixedCeil(Bottom);

     // check whether the bitmap needs to be drawn at all
     if (YRange[0] >= Height) or (YRange[1] < 0) or (YRange[0] >= YRange[1])
      then Exit;
    end;

   // eventually limit range
   if XRange[0] < 0 then
    begin
     XRange[0] := 0;
     XAntiAlias[0] := 0;
    end else XAntiAlias[0] := 1;
   if XRange[1] >= Width then
    begin
     XRange[1] := Width - 1;
     XAntiAlias[1] := 0;
    end else XAntiAlias[1] := 1;

   if YRange[0] < 0
    then YRange[0] := 0
    else
     begin
      // draw first scanline
      ScnLne := PixelMap.Scanline[YRange[0]];

      // draw first pixel of this first scanline
      if XAntiAlias[0] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);

      // draw middle pixels of this first scanline
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this first scanline
      if XAntiAlias[1] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);

      // increase Y-Range (not to include this first scanline)
      Inc(YRange[0]);
     end;

   if YRange[1] >= Height
    then YRange[1] := Height - 1
    else
     begin
      // draw last scanline
      ScnLne := PixelMap.Scanline[YRange[1]];

      // draw first pixel of this last scanline
      if XAntiAlias[0] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);

      // draw middle pixels of this last scanline
      for X := XRange[0] + XAntiAlias[0] to XRange[1] - XAntiAlias[1]
       do BlendPixelInplace(PixelColor32, ScnLne[X]);

      // draw last pixel of this last scanline
      if XAntiAlias[1] <> 0
       then BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);

      // increase Y-Range (not to include this last scanline)
      Dec(YRange[1]);
     end;

   for Y := YRange[0] to YRange[1] do
    begin
     ScnLne := PixelMap.Scanline[Y];

     // draw first pixel of this scanline
     if XAntiAlias[0] <> 0
      then BlendPixelInplace(PixelColor32, ScnLne[XRange[0]]);

     // draw last pixel of this scanline
     if XAntiAlias[1] <> 0
      then BlendPixelInplace(PixelColor32, ScnLne[XRange[1]]);
    end;
   EMMS;
  end;
end;

procedure TGuiPixelFrameRectangle.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin
 DrawDraftShape(PixelMap);
end;


{ TGuiPixelFilledRoundedRectangle }

constructor TGuiPixelFilledRoundedRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRoundedRectangle.Create;
end;

destructor TGuiPixelFilledRoundedRectangle.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

procedure TGuiPixelFilledRoundedRectangle.DrawDraftShape(
  PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  XStart            : Single;
  SqrDist, SqrYDist : Single;
  SqrRadMinusOne    : Single;
  Temp              : Single;

  XRange       : array [0..1] of Integer;
  YRange       : array [0..1] of Integer;
  Radius       : Integer;
  ScnLne       : array [0..1] of PPixel32Array;
  PixelColor32 : TPixel32;

begin
 PixelColor32 := ConvertColor(Color);
 PixelColor32.A := Alpha;

 with PixelMap do
  begin
(*
   with GeometricShape do
    begin
     XRange[0] := FixedFloor(Left);
     XRange[1] := FixedCeil(Right);

     // check whether the bitmap needs to be drawn at all
     if (XRange[0] >= Width) or (XRange[1] < 0) or (XRange[0] >= XRange[1])
      then Exit;

     YRange[0] := FixedRound(Top);
     YRange[1] := FixedRound(Bottom);

     // check whether the bitmap needs to be drawn at all
     if (YRange[0] >= Height) or (YRange[1] < 0) or (YRange[0] >= YRange[1])
      then Exit;

     Radius := FixedRound(BorderRadius);
    end;

   // draw circle
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   for Y := 0 to Round(Radius) - 1  do
    begin
     SqrYDist := Sqr(Y - (Radius - 1));
     XStart := Sqr(Radius) - SqrYDist;
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;
     ScnLne[0] := Scanline[Y];
     ScnLne[1] := Scanline[Height - 1 - Y];

     for X := Round((Radius - 1) - XStart) to Round((Width - 1) - (Radius - 1) + XStart) do
      begin
       // calculate squared distance
       if X < (Radius - 1)
        then SqrDist := Sqr(X - (Radius - 1)) + SqrYDist else

       if X > (Width - 1) - (Radius - 1)
        then SqrDist := Sqr(X - (Width - 1) + (Radius - 1)) + SqrYDist
        else SqrDist := SqrYDist;

       if SqrDist < SqrRadMinusOne
        then CombColor := PixelColor32;
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][X]);
       EMMS;
      end;
    end;

   for Y := Round(Radius) to Height - 1 - Round(Radius) do
    begin
     ScnLne[0] := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       // check whether position is a border
       if (Y < BorderWidth - 1) or (Y > Height - 1 - BorderWidth + 1)
        then CombColor := BorderColor else

       // check whether position is an upper half border
       if (Y < BorderWidth) then
        begin
         Temp := BorderWidth - Y;
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end else

       // check whether position is a lower half border
       if (Y > Height - 1 - BorderWidth) then
        begin
         Temp := Y - (Height - 1 - BorderWidth);
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end
       else CombColor := PanelColor;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;
*)
  end;
end;

procedure TGuiPixelFilledRoundedRectangle.DrawFixedPoint(
  PixelMap: TGuiCustomPixelMap);
(*
var
  X, Y              : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  PanelColor        : TPixel32;
  BorderColor       : TPixel32;
  CombColor         : TPixel32;
  Radius            : Single;
  XStart            : Single;
  BorderWidth       : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  SqrDist, SqrYDist : Single;
  SqrRadMinusOne    : Single;
  Temp              : Single;

const
  CBlack : TPixel32 = (ARGB : $FF000000);
*)
begin
 with PixelMap do
  begin
(*
   PanelColor := ConvertColor(FPanelColor);
   if FBorderVisible
    then BorderColor := ConvertColor(FLineColor)
    else BorderColor := PanelColor;

   // draw circle
   Radius := Min(Min(FRoundRadius, 0.5 * Width), 0.5 * Height) + 1;
   BorderWidth := Max(FBorderWidth, 2);

   RadMinusBorderOne := BranchlessClipPositive(Radius - BorderWidth);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - BorderWidth - 1));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   for Y := 0 to Round(Radius) - 1  do
    begin
     SqrYDist := Sqr(Y - (Radius - 1));
     XStart := Sqr(Radius) - SqrYDist;
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;
     ScnLne[0] := Scanline[Y];
     ScnLne[1] := Scanline[Height - 1 - Y];

     for X := Round((Radius - 1) - XStart) to Round((Width - 1) - (Radius - 1) + XStart) do
      begin
       // calculate squared distance
       if X < (Radius - 1)
        then SqrDist := Sqr(X - (Radius - 1)) + SqrYDist else

       if X > (Width - 1) - (Radius - 1)
        then SqrDist := Sqr(X - (Width - 1) + (Radius - 1)) + SqrYDist
        else SqrDist := SqrYDist;

       if SqrDist < SqrRadMinusBorder
        then CombColor := PanelColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp * $FF));
        end else
       if SqrDist < SqrRadMinusOne
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][X]);
       EMMS;
      end;
    end;

   for Y := Round(Radius) to Height - 1 - Round(Radius) do
    begin
     ScnLne[0] := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       // check whether position is a border
       if (Y < BorderWidth - 1) or (Y > Height - 1 - BorderWidth + 1)
        then CombColor := BorderColor else

       // check whether position is an upper half border
       if (Y < BorderWidth) then
        begin
         Temp := BorderWidth - Y;
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end else

       // check whether position is a lower half border
       if (Y > Height - 1 - BorderWidth) then
        begin
         Temp := Y - (Height - 1 - BorderWidth);
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, PanelColor, Round(Temp * $FF));
        end
       else CombColor := PanelColor;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;
*)
  end;
end;

function TGuiPixelFilledRoundedRectangle.GetGeometricShape: TGuiRoundedRectangle;
begin
 Result := TGuiRoundedRectangle(FGeometricShape);
end;


{ TGuiPixelFrameRoundedRectangle }

constructor TGuiPixelFrameRoundedRectangle.Create;
begin
 inherited;
 FGeometricShape := TGuiRoundedRectangle.Create;
end;

destructor TGuiPixelFrameRoundedRectangle.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

procedure TGuiPixelFrameRoundedRectangle.DrawDraftShape(
  PixelMap: TGuiCustomPixelMap);
begin
 inherited;

end;

procedure TGuiPixelFrameRoundedRectangle.DrawFixedPoint(
  PixelMap: TGuiCustomPixelMap);
begin
 inherited;

end;

function TGuiPixelFrameRoundedRectangle.GetGeometricShape: TGuiRoundedRectangle;
begin
 Result := TGuiRoundedRectangle(FGeometricShape);
end;

end.
