unit DAV_GuiVectorPixelCircle;

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
  TGuiPixelFilledCircle = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiCircle;
  protected
    procedure DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiCircle read GetGeometricShape;
  end;

  TGuiPixelFilledCircleSector = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiCircleSector;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiCircleSector read GetGeometricShape;
  end;

  TGuiPixelFrameCircle = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiCircle;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiCircle read GetGeometricShape;
  end;

  TGuiPixelFrameCircleSector = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiCircleSector;
  protected
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GeometricShape: TGuiCircleSector read GetGeometricShape;
  end;

implementation

uses
  DAV_GuiBlend, DAV_Complex, DAV_Approximations;

{ TGuiPixelFilledCircle }

constructor TGuiPixelFilledCircle.Create;
begin
 inherited;
 FGeometricShape := TGuiCircle.Create;
end;

destructor TGuiPixelFilledCircle.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

function TGuiPixelFilledCircle.GetGeometricShape: TGuiCircle;
begin
 Result := TGuiCircle(FGeometricShape);
end;

procedure TGuiPixelFilledCircle.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
  Radius       : TFixed24Dot8Point;
  CenterX      : TFixed24Dot8Point;
  CenterY      : TFixed24Dot8Point;
  XStart       : TFixed24Dot8Point;
  YRange       : array [0..1] of Integer;
  XRange       : array [0..1] of Integer;
  SqrYDist     : TFixed24Dot8Point;
begin
 with PixelMap do
  begin
   // transfer the color data to local variables
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   // transfer the geometric shape data to local variables
   Radius.Fixed := GeometricShape.Radius.Fixed + CFixed24Dot8One.Fixed;
   CenterX := GeometricShape.CenterX;
   CenterY := GeometricShape.CenterY;

   // calculate affected scanlines
   YRange[0] := FixedRound(FixedSub(CenterY, Radius));
   YRange[1] := FixedRound(FixedAdd(CenterY, Radius));

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if FixedRound(FixedSub(CenterX, Radius)) >= Width then Exit;
   if FixedRound(FixedAdd(CenterX, Radius)) < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := FixedSqr(FixedSub(ConvertToFixed24Dot8Point(Y), CenterY));

     XStart.Fixed := FixedSqr(Radius).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // calculate affected pixels within this scanline
     XRange[0] := FixedRound(FixedSub(CenterX, XStart));
     XRange[1] := FixedRound(FixedAdd(CenterX, XStart));

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     for X := XRange[0] to XRange[1]
      do BlendPixelInplace(PixelColor32, ScnLne[X]);
    end;
  end;
 EMMS;
end;

procedure TGuiPixelFilledCircle.DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
var
  X, Y           : Integer;
  ScnLne         : PPixel32Array;
  PixelColor32   : TPixel32;
  CombColor      : TPixel32;
  Radius         : Single;
  Center         : TComplexSingle;
  XStart         : Single;
  YRange         : array [0..1] of Integer;
  XRange         : array [0..1] of Integer;
  SqrYDist       : Single;
  SqrDist        : Single;
  SqrRadMinusOne : Single;
begin
 with PixelMap do
  begin
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   // transfer the GeometricShape data to local variables
   Radius := ConvertFromFixed24Dot8Point(GeometricShape.Radius) + 1;
   Center.Re := ConvertFromFixed24Dot8Point(GeometricShape.CenterX);
   Center.Im := ConvertFromFixed24Dot8Point(GeometricShape.CenterY);

   // calculate affected scanlines
   YRange[0] := Round(Center.Im - Radius);
   YRange[1] := Round(Center.Im + Radius);

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if Center.Re - Radius >= Width then Exit;
   if Center.Re + Radius < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(Radius) - SqrYDist;
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;

     // calculate affected pixels within this scanline
     XRange[0] := Round(Center.Re - XStart);
     XRange[1] := Round(Center.Re + XStart);

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     for X := XRange[0] to XRange[1] do
      begin
       // calculate squared distance
       SqrDist := Sqr(X - Center.Re) + SqrYDist;

       CombColor := PixelColor32;
       if SqrDist >= SqrRadMinusOne
        then CombColor.A := Round(CombColor.A * (Radius - FastSqrtBab2(SqrDist)));

       BlendPixelInplace(CombColor, ScnLne[X]);
       EMMS;
      end;

    end;
  end;
end;

procedure TGuiPixelFilledCircle.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
var
  X, Y           : Integer;
  ScnLne         : PPixel32Array;
  PixelColor32   : TPixel32;
  CombColor      : TPixel32;
  YRange         : array [0..1] of Integer;
  XRange         : array [0..1] of Integer;
  Radius         : TFixed24Dot8Point;
  CenterX        : TFixed24Dot8Point;
  CenterY        : TFixed24Dot8Point;
  XStart         : TFixed24Dot8Point;
  SqrYDist       : TFixed24Dot8Point;
  SqrDist        : TFixed24Dot8Point;
  SqrRadMinusOne : TFixed24Dot8Point;
  PixelLineCount : Integer;
begin
 with PixelMap do
  begin
   // transfer the GeometricShape data to local variables
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   Radius.Fixed := GeometricShape.Radius.Fixed + CFixed24Dot8One.Fixed;
   CenterX := GeometricShape.CenterX;
   CenterY := GeometricShape.CenterY;

   // calculate affected scanlines
   YRange[0] := FixedRound(FixedSub(CenterY, Radius));
   YRange[1] := FixedRound(FixedAdd(CenterY, Radius));

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if FixedRound(FixedSub(CenterX, Radius)) >= Width then Exit;
   if FixedRound(FixedAdd(CenterX, Radius)) < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   if Radius.Fixed > 0
    then SqrRadMinusOne := FixedSqr(FixedSub(Radius, CFixed24Dot8One))
    else SqrRadMinusOne.Fixed := 0;

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := FixedSqr(FixedSub(ConvertToFixed24Dot8Point(Y), CenterY));

     XStart.Fixed := FixedSqr(Radius).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // calculate affected pixels within this scanline
     XRange[0] := FixedRound(FixedSub(CenterX, XStart));
     XRange[1] := FixedRound(FixedAdd(CenterX, XStart));

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     X := XRange[0];
     while X <= XRange[1] do
      begin
       // calculate squared distance
       SqrDist.Fixed := X shl 8 - CenterX.Fixed;
       SqrDist.Fixed := FixedSqr(SqrDist).Fixed + SqrYDist.Fixed;
       CombColor := PixelColor32;
       if SqrDist.Fixed >= SqrRadMinusOne.Fixed then
        begin
         SqrDist.Fixed := Radius.Fixed - FixedSqrt(SqrDist).Fixed;
         if SqrDist.Fixed < $FF
          then CombColor.A := ((SqrDist.Fixed * CombColor.A + $7F) shr 8);
         BlendPixelInplace(CombColor, ScnLne[X]);
         Inc(X);
        end
       else
        begin
         {$IFDEF Simple}
         BlendPixelInplace(CombColor, ScnLne[X]);
         Inc(X);
         {$ELSE}
         SqrDist.Fixed := CenterX.Fixed - ConvertToFixed24Dot8Point(X).Fixed;
         PixelLineCount := FixedRound(FixedAdd(SqrDist, SqrDist)) + 1;
         if X + PixelLineCount > XRange[1]
          then PixelLineCount := (XRange[1] - X);

         if PixelLineCount <= 0 then
          begin
           BlendPixelInplace(CombColor, ScnLne[X]);
           Inc(X);
           Continue;
          end;

         BlendPixelLine(CombColor, @ScnLne[X], PixelLineCount);
         X := X + PixelLineCount;
         {$ENDIF}
        end;
      end;
    end;
   EMMS;
  end;
end;


{ TGuiPixelFilledCircleSector }

constructor TGuiPixelFilledCircleSector.Create;
begin
 inherited;
 FGeometricShape := TGuiCircleSector.Create;
end;

destructor TGuiPixelFilledCircleSector.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

procedure TGuiPixelFilledCircleSector.DrawDraftShape(
  PixelMap: TGuiCustomPixelMap);
begin
 inherited;

end;

procedure TGuiPixelFilledCircleSector.DrawFixedPoint(
  PixelMap: TGuiCustomPixelMap);
begin
 inherited;

end;

function TGuiPixelFilledCircleSector.GetGeometricShape: TGuiCircleSector;
begin
 Result := TGuiCircleSector(FGeometricShape);
end;


{ TGuiPixelFrameCircle }

constructor TGuiPixelFrameCircle.Create;
begin
 inherited;
 FGeometricShape := TGuiCircle.Create;
end;

destructor TGuiPixelFrameCircle.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

function TGuiPixelFrameCircle.GetGeometricShape: TGuiCircle;
begin
 Result := TGuiCircle(FGeometricShape)
end;

procedure TGuiPixelFrameCircle.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y          : Integer;
  ScnLne        : PPixel32Array;
  PixelColor32  : TPixel32;
  SingleRadius  : Single;
  Center        : TComplexSingle;
  XStart        : Single;
  YRange        : array [0..1] of Integer;
  XRange        : array [0..1] of Integer;
  SqrYDist      : Single;
  SqrDist       : Single;
  SqrMinusWidth : Single;
begin
 with PixelMap do
  begin
   // transfer the GeometricShape data to local variables
   with GeometricShape do
    begin
     PixelColor32 := ConvertColor(Color);
     PixelColor32.A := Alpha;
     SingleRadius := ConvertFromFixed24Dot8Point(Radius) + 1;
     Center.Re := ConvertFromFixed24Dot8Point(CenterX);
     Center.Im := ConvertFromFixed24Dot8Point(CenterY);

     // inner radius
     SqrMinusWidth := Sqr(BranchlessClipPositive(SingleRadius - Self.Width.Fixed * CFixed24Dot8ToFloat));
    end;

   // calculate affected scanlines
   YRange[0] := Round(Center.Im - SingleRadius);
   YRange[1] := Round(Center.Im + SingleRadius);

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if Center.Re - SingleRadius >= Width then Exit;
   if Center.Re + SingleRadius < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(SingleRadius) - SqrYDist;
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;

     // calculate affected pixels within this scanline
     XRange[0] := Round(Center.Re - XStart);
     XRange[1] := Round(Center.Re + XStart);

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     for X := XRange[0] to XRange[1] do
      begin
       // calculate squared distance
       SqrDist := Sqr(X - Center.Re) + SqrYDist;

       if SqrDist >= SqrMinusWidth
        then BlendPixelInplace(PixelColor32, ScnLne[X]);
      end;
     EMMS;
    end;
  end;
end;

procedure TGuiPixelFrameCircle.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin
 DrawDraftShape(PixelMap);
end;


{ TGuiPixelFrameCircleSector }

constructor TGuiPixelFrameCircleSector.Create;
begin
 inherited;
 FGeometricShape := TGuiCircleSector.Create;
end;

destructor TGuiPixelFrameCircleSector.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

procedure TGuiPixelFrameCircleSector.DrawDraftShape(
  PixelMap: TGuiCustomPixelMap);
begin
 inherited;

end;

procedure TGuiPixelFrameCircleSector.DrawFixedPoint(
  PixelMap: TGuiCustomPixelMap);
begin
 inherited;

end;

function TGuiPixelFrameCircleSector.GetGeometricShape: TGuiCircleSector;
begin
 Result := TGuiCircleSector(FGeometricShape);
end;

end.
