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
  DAV_GuiBlend, DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiFixedPoint;

type
  TGuiFillEvent = procedure(Sender: TObject; const X, Y: Integer;
    var Pixel: TPixel32);

  TCustomGuiPixelPrimitive = class(TPersistent)
  protected
    FGeometricShape : TGuiCustomGeometricShape;
    FOnChanged      : TNotifyEvent;
    procedure Changed; virtual;
  public
    procedure Draw(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); virtual; abstract;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TCustomGuiPixelSimplePrimitive = class(TCustomGuiPixelPrimitive)
  private
    FColor: TColor;
    FAlpha: Byte;
    procedure SetAlpha(const Value: Byte);
    procedure SetColor(const Value: TColor);
  protected
    procedure AlphaChanged; virtual;
    procedure ColorChanged; virtual;
  public
    property Color: TColor read FColor write SetColor;
    property Alpha: Byte read FAlpha write SetAlpha;
  end;

  TCustomGuiPixelFramePrimitive = class(TCustomGuiPixelSimplePrimitive)
  private
    FWidth    : TFixed24Dot8Point;
    procedure SetWidth(const Value: TFixed24Dot8Point);
  protected
    procedure WidthChanged; virtual;
  public
    property Width: TFixed24Dot8Point read FWidth write SetWidth;
  end;

  TCustomGuiPixelFillPrimitive = class(TCustomGuiPixelSimplePrimitive)
  private
    FOnGetFillColor : TGuiFillEvent;
  public
    property OnGetFillColor: TGuiFillEvent read FOnGetFillColor write FOnGetFillColor;
  end;

  TGuiPixelFilledRectangle = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiRectangle;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Draw(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); override;

    property GeometricShape: TGuiRectangle read GetGeometricShape;
  end;

  TGuiPixelFilledCircle = class(TCustomGuiPixelFillPrimitive)
  public
    procedure Draw(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); override;
  private
    function GetGeometricShape: TGuiCircle;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property GeometricShape: TGuiCircle read GetGeometricShape;
  end;

  TGuiPixelFilledEllipse = class(TCustomGuiPixelFillPrimitive)
  public
    procedure Draw(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); override;
  private
    function GetGeometricShape: TGuiEllipse;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property GeometricShape: TGuiEllipse read GetGeometricShape;
  end;

  TGuiPixelLine = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiLine;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Draw(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); override;

    property GeometricShape: TGuiLine read GetGeometricShape;
  end;

  TGuiPixelFrameRectangle = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiRectangle;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Draw(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); override;

    property GeometricShape: TGuiRectangle read GetGeometricShape;
  end;

  TGuiPixelFrameCircle = class(TCustomGuiPixelFramePrimitive)
  private
    function GetGeometricShape: TGuiCircle;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Draw(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraft(PixelMap: TGuiCustomPixelMap); override;

    property GeometricShape: TGuiCircle read GetGeometricShape;
  end;

implementation

uses
  Math, DAV_Complex, DAV_Approximations;


{ TCustomGuiPixelPrimitive }

procedure TCustomGuiPixelPrimitive.Changed;
begin
 if Assigned(FOnChanged)
  then FOnChanged(Self);
end;


{ TCustomGuiPixelSimplePrimitive }

procedure TCustomGuiPixelSimplePrimitive.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TCustomGuiPixelSimplePrimitive.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;

procedure TCustomGuiPixelSimplePrimitive.AlphaChanged;
begin
 Changed;
end;

procedure TCustomGuiPixelSimplePrimitive.ColorChanged;
begin
 Changed;
end;



{ TCustomGuiPixelFramePrimitive }

procedure TCustomGuiPixelFramePrimitive.WidthChanged;
begin
 Changed;
end;

procedure TCustomGuiPixelFramePrimitive.SetWidth(
  const Value: TFixed24Dot8Point);
begin
 if FWidth.Fixed <> Value.Fixed then
  begin
   FWidth.Fixed := Value.Fixed;
   WidthChanged;
  end;
end;


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

procedure TGuiPixelFilledCircle.DrawDraft(PixelMap: TGuiCustomPixelMap);
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
   // transfer the GeometricShape data to local variables
   with GeometricShape do
    begin
     PixelColor32 := ConvertColor(Color);
     PixelColor32.A := Alpha;
     SingleRadius := ConvertFromFixed24Dot8Point(Radius) + 1;
     Center.Re := ConvertFromFixed24Dot8Point(CenterX);
     Center.Im := ConvertFromFixed24Dot8Point(CenterY);
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

procedure TGuiPixelFilledCircle.Draw(PixelMap: TGuiCustomPixelMap);
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
begin
 with PixelMap do
  begin
   // transfer the GeometricShape data to local variables
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   Radius  := FixedAdd(GeometricShape.Radius, CFixed24Dot8One);
   CenterX := GeometricShape.CenterX;
   CenterY := GeometricShape.CenterY;

   // calculate affected scanlines
   YRange[0] := (CenterY.Fixed - Radius.Fixed + $7F) shr 8;
   YRange[1] := (CenterY.Fixed + Radius.Fixed + $7F) shr 8;

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if (CenterX.Fixed - Radius.Fixed + $7F) shr 8 >= Width then Exit;
   if (CenterX.Fixed + Radius.Fixed + $7F) shr 8 < 0 then Exit;

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

     XStart := FixedSub(FixedSqr(Radius), SqrYDist);
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // calculate affected pixels within this scanline
     XRange[0] := (CenterX.Fixed - XStart.Fixed + $7F) shr 8;
     XRange[1] := (CenterX.Fixed + XStart.Fixed + $7F) shr 8;

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     for X := XRange[0] to XRange[1] do
      begin
       // calculate squared distance
       SqrDist.Fixed := X shl 8 - CenterX.Fixed;
       SqrDist.Fixed := FixedSqr(SqrDist).Fixed + SqrYDist.Fixed;
       CombColor := PixelColor32;
       if SqrDist.Fixed >= SqrRadMinusOne.Fixed then
        begin
         SqrDist.Fixed := Radius.Fixed - FixedSqrt(SqrDist).Fixed;
         CombColor.A := ((SqrDist.Fixed * CombColor.A + $7F) shr 8);
        end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;
   EMMS;
  end;
end;


{ TGuiPixelFilledEllipse }

constructor TGuiPixelFilledEllipse.Create;
begin
 inherited;
 FGeometricShape := TGuiEllipse.Create;
end;

destructor TGuiPixelFilledEllipse.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

function TGuiPixelFilledEllipse.GetGeometricShape: TGuiEllipse;
begin
 Result := TGuiEllipse(FGeometricShape);
end;

procedure TGuiPixelFilledEllipse.Draw(PixelMap: TGuiCustomPixelMap);
var
  X, Y           : Integer;
  ScnLne         : PPixel32Array;
  PixelColor32   : TPixel32;
  CombColor      : TPixel32;
  Rad            : TComplexSingle;
  Center         : TComplexSingle;
  XStart         : Single;
  YRange         : array [0..1] of Integer;
  XRange         : array [0..1] of Integer;
  SqrYDist       : Single;
  SqrDist        : Single;
  SqrRadMinusOne : TComplexSingle;
  SqrRadRatio    : Single;
begin
 with PixelMap do
  begin
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   // transfer the GeometricShape data to local variables
   with GeometricShape do
    begin
     Rad.Re := ConvertFromFixed24Dot8Point(RadiusX) + 1;
     Rad.Im := ConvertFromFixed24Dot8Point(RadiusY) + 1;
     if (Rad.Re <= 0) or (Rad.Im <= 0) then Exit;
     SqrRadRatio := Sqr(Rad.Re / Rad.Im);

     Center.Re := ConvertFromFixed24Dot8Point(CenterX);
     Center.Im := ConvertFromFixed24Dot8Point(CenterY);
    end;

   // calculate affected scanlines
   YRange[0] := Round(Center.Im - Rad.Im);
   YRange[1] := Round(Center.Im + Rad.Im);

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if Center.Re - Rad.Re >= Width then Exit;
   if Center.Re + Rad.Re < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   SqrRadMinusOne.Re := Sqr(BranchlessClipPositive(Rad.Re - 1));
   SqrRadMinusOne.Im := Sqr(BranchlessClipPositive(Rad.Im - 1));

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(Rad.Re) - SqrYDist * SqrRadRatio;
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

(*
       XStart := SqrRadMinusOne.Re - SqrYDist * (SqrRadMinusOne.Re / SqrRadMinusOne.Im);
       if XStart <= 0
        then
        else XStart := Sqrt(XStart) - 0.5;

       if X < XStart
        then CombColor.ARGB := $FFFFFFFF;

(*
       if SqrDist >= SqrRadMinusOne
        then CombColor.A := Round(CombColor.A * (Rad.Re - FastSqrtBab2(SqrDist)));
*)

       BlendPixelInplace(CombColor, ScnLne[X]);
       EMMS;
      end;

    end;
  end;
end;

procedure TGuiPixelFilledEllipse.DrawDraft(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
  Rad          : TComplexSingle;
  Center       : TComplexSingle;
  SqrRadRatio  : Single;
  XStart       : Single;
  YRange       : array [0..1] of Integer;
  XRange       : array [0..1] of Integer;
  SqrYDist     : Single;
begin
 with PixelMap do
  begin
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   // transfer the GeometricShape data to local variables
   with GeometricShape do
    begin
     Rad.Re := ConvertFromFixed24Dot8Point(RadiusX) + 1;
     Rad.Im := ConvertFromFixed24Dot8Point(RadiusY) + 1;
     if (Rad.Re <= 0) or (Rad.Im <= 0) then Exit;
     SqrRadRatio := Sqr(Rad.Re / Rad.Im);

     Center.Re := ConvertFromFixed24Dot8Point(CenterX);
     Center.Im := ConvertFromFixed24Dot8Point(CenterY);
    end;

   // calculate affected scanlines
   YRange[0] := Round(Center.Im - Rad.Im);
   YRange[1] := Round(Center.Im + Rad.Im);

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if Center.Re - Rad.Re >= Width then Exit;
   if Center.Re + Rad.Re < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(Rad.Re) - SqrYDist * SqrRadRatio;
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

procedure TGuiPixelFilledEllipse.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin

end;


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

procedure TGuiPixelFilledRectangle.Draw(PixelMap: TGuiCustomPixelMap);
begin
 DrawFixedPoint(PixelMap);
end;

procedure TGuiPixelFilledRectangle.DrawDraft(PixelMap: TGuiCustomPixelMap);
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


{ TGuiPixelLine }

constructor TGuiPixelLine.Create;
begin
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

procedure TGuiPixelLine.Draw(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiPixelLine.DrawDraft(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiPixelLine.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin

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

procedure TGuiPixelFrameRectangle.Draw(PixelMap: TGuiCustomPixelMap);
begin
 DrawFixedPoint(PixelMap);
end;

procedure TGuiPixelFrameRectangle.DrawDraft(PixelMap: TGuiCustomPixelMap);
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
 DrawDraft(PixelMap);
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

procedure TGuiPixelFrameCircle.Draw(PixelMap: TGuiCustomPixelMap);
begin
 DrawDraft(PixelMap);
end;

procedure TGuiPixelFrameCircle.DrawDraft(PixelMap: TGuiCustomPixelMap);
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
 DrawDraft(PixelMap);
end;

end.
