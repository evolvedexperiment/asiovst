unit DAV_GuiVectorPixel;

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
  DAV_GuiBlend, DAV_GuiPixelMap, DAV_GuiVector;

type
  TCustomGuiPixelPrimitives = class(TPersistent)
  protected
    FPrimitive : TGuiCustomPrimitive;
  public
    procedure Draw(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); virtual; abstract;
  end;

  TGuiPixelCircle = class(TCustomGuiPixelPrimitives)
  public
    procedure Draw(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); override;
  private
    function GetPrimitive: TGuiCircle;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Primitive: TGuiCircle read GetPrimitive;
  end;

implementation

uses
  Math, DAV_Complex, DAV_Approximations, DAV_GuiFixedPoint;

{ TGuiPixelCircle }

function TGuiPixelCircle.GetPrimitive: TGuiCircle;
begin
 Result := TGuiCircle(FPrimitive);
end;

constructor TGuiPixelCircle.Create;
begin
 inherited;
 FPrimitive := TGuiCircle.Create;
end;

destructor TGuiPixelCircle.Destroy;
begin
 FreeAndNil(FPrimitive);
 inherited;
end;

procedure TGuiPixelCircle.DrawDraft(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
  SingleRadius : Single;
  Center       : TComplexSingle;
  XStart       : Single;
  YRange       : array [0..1] of Integer;
  XRange       : array [0..1] of Integer;
  SqrYDist     : Single;
begin
 with PixelMap do
  begin
   // transfer the primitive data to local variables
   with Primitive do
    begin
     PixelColor32 := ConvertColor(Color);
     PixelColor32.A := Alpha;
     SingleRadius := Radius.Value + Radius.Fractal + 1;
     Center.Re := CenterX.Value + CenterX.Fractal;
     Center.Im := CenterY.Value + CenterY.Fractal;
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
     for X := XRange[0] to XRange[1]
      do BlendPixelInplace(PixelColor32, ScnLne[X]);
     EMMS;
    end;
  end;
end;

procedure TGuiPixelCircle.Draw(PixelMap: TGuiCustomPixelMap);
var
  X, Y           : Integer;
  ScnLne         : PPixel32Array;
  PixelColor32   : TPixel32;
  CombColor      : TPixel32;
  SingleRadius   : Single;
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
   // transfer the primitive data to local variables
   with Primitive do
    begin
     PixelColor32 := ConvertColor(Color);
     PixelColor32.A := Alpha;
     SingleRadius := Radius.Value + Radius.Fractal + 1;
     Center.Re := CenterX.Value + CenterX.Fractal;
     Center.Im := CenterY.Value + CenterY.Fractal;
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

   SqrRadMinusOne := Sqr(BranchlessClipPositive(SingleRadius - 1));

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

       CombColor := PixelColor32;
       if SqrDist >= SqrRadMinusOne
        then CombColor.A := Round(CombColor.A * (SingleRadius - FastSqrtBab2(SqrDist)));

       BlendPixelInplace(CombColor, ScnLne[X]);
       EMMS;
      end;

    end;
  end;
end;

procedure TGuiPixelCircle.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
var
  X, Y           : Integer;
  ScnLne         : PPixel32Array;
  PixelColor32   : TPixel32;
  CombColor      : TPixel32;
  YRange         : array [0..1] of Integer;
  XRange         : array [0..1] of Integer;
  Radius         : TFixed16Dot16Point;
  CenterX        : TFixed16Dot16Point;
  CenterY        : TFixed16Dot16Point;
  XStart         : TFixed16Dot16Point;
  SqrYDist       : TFixed16Dot16Point;
  SqrDist        : TFixed16Dot16Point;
  SqrRadMinusOne : TFixed16Dot16Point;
begin
 with PixelMap do
  begin
   // transfer the primitive data to local variables
   PixelColor32 := ConvertColor(Primitive.Color);
   PixelColor32.A := Primitive.Alpha;
   Radius  := Fixed16Dot16Point(Primitive.Radius.Value + Primitive.Radius.Fractal + 1);
   CenterX := Fixed16Dot16Point(Primitive.CenterX.Value + Primitive.CenterX.Fractal);
   CenterY := Fixed16Dot16Point(Primitive.CenterY.Value + Primitive.CenterY.Fractal);

   // calculate affected scanlines
   YRange[0] := (CenterY.Fixed - Radius.Fixed + $7FFF) shr 16;
   YRange[1] := (CenterY.Fixed + Radius.Fixed + $7FFF) shr 16;

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if (CenterX.Fixed - Radius.Fixed + $7FFF) shr 16 >= Width then Exit;
   if (CenterX.Fixed + Radius.Fixed + $7FFF) shr 16 < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   if Radius.Fixed > 0
    then SqrRadMinusOne := FixedSqr(FixedSub(Radius, CFixedOne))
    else SqrRadMinusOne.Fixed := 0;

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := FixedSqr(FixedSub(Fixed16Dot16Point(Y), CenterY));

     XStart := FixedSub(FixedSqr(Radius), SqrYDist);
     if XStart.Fixed <= 0
      then Continue
      else XStart := FixedSub(FixedSqrtHighResolution(XStart), CFixedHalf);

     // calculate affected pixels within this scanline
     XRange[0] := FixedRound(FixedSub(CenterX, XStart));
     XRange[1] := FixedRound(FixedAdd(CenterX, XStart));

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     for X := XRange[0] to XRange[1] do
      begin
       // calculate squared distance
       SqrDist := FixedAdd(FixedSqr(FixedSub(Fixed16Dot16Point(X), CenterX)), SqrYDist);

       CombColor := PixelColor32;

       if SqrDist.Fixed >= SqrRadMinusOne.Fixed
        then CombColor.A := FixedRound(FixedMul(FixedSub(Radius, FixedSqrtHighResolution(SqrDist)), Fixed16Dot16Point(CombColor.A)));

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;
   EMMS;
  end;
end;

end.
