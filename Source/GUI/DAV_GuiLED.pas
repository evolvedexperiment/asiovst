unit DAV_GuiLED;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Graphics, Forms, Messages, SysUtils, Controls, DAV_GuiBaseControl;

type
  TCustomGuiLED = class(TCustomGuiBaseAntialiasedControl)
  private
    FLEDColor   : TColor;
    FOnChange   : TNotifyEvent;
    FBrightness : Single;
    FUniformity: Single;
    procedure SetLEDColor(const Value: TColor);
    procedure SetBrightness(const Value: Single);
    procedure SetUniformity(const Value: Single);
    function GetUniformity: Single;
  protected
    procedure RenderLEDToBitmap24(const Bitmap: TBitmap); virtual;
    procedure RenderLEDToBitmap32(const Bitmap: TBitmap); virtual;
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure UpdateBuffer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;
    property LineWidth;
    property LEDColor: TColor read FLEDColor write SetLEDColor default clBlack;
    property Brightness_Percent: Single read FBrightness write SetBrightness;
    property Uniformity_Percent: Single read GetUniformity write SetUniformity;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiLED = class(TCustomGuiLED)
  published
    property Anchors;
    property AntiAlias;
    property Color;
    property Enabled;
    property LEDColor;
    property LineColor;
    property LineWidth;
    property ParentColor;
    property Transparent;
    property Visible;
    property OnChange;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
  end;

implementation

uses
  ExtCtrls, Math, DAV_Common, DAV_Complex, DAV_GuiCommon;

resourcestring
  RCStrWrongPixelFormat = 'Wrong pixel format!';

function RadToDeg(const Radians: Extended): Extended;  { Degrees := Radians * 180 / PI }
const
  DegPi : Double = (180 / PI);
begin
  Result := Radians * DegPi;
end;

function RelativeAngle(X1, Y1, X2, Y2: Integer): Single;
const
  MulFak = 180 / Pi;
begin
  Result := ArcTan2(X2 - X1, Y1 - Y2) * MulFak;
end;

function SafeAngle(Angle: Single): Single;
begin
  while Angle < 0 do Angle := Angle + 360;
  while Angle >= 360 do Angle := Angle - 360;
  Result := Angle;
end;

{ This function solves for x in the equation "x is y% of z". }
function SolveForX(Y, Z: Longint): Longint;
begin
  Result := round( Z * (Y * 0.01) );//tt
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
  if Z = 0 then Result := 0 else Result := round( (X * 100.0) / Z ); //t
end;


constructor TCustomGuiLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLEDColor    := clBlack;
  FLineColor   := clRed;
  FLineWidth   := 1;
  FBrightness  := 100;
  FUniformity  := 0.4;
  FBuffer.PixelFormat := pf32bit
end;

destructor TCustomGuiLED.Destroy;
begin
  inherited Destroy;
end;

function TCustomGuiLED.GetUniformity: Single;
begin
 Result := 100 * (1 - sqrt(FUniformity));
end;

procedure TCustomGuiLED.SettingsChanged(Sender: TObject);
begin
 Invalidate;
end;

procedure TCustomGuiLED.SetUniformity(const Value: Single);
begin
 if FUniformity <> Value then
  begin
   FUniformity := Sqr(1 - Limit(0.01 * Value, 0, 1));
   Invalidate;
  end;
end;

procedure TCustomGuiLED.RenderLEDToBitmap24(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Rad      : Single;
  XStart   : Single;
  BW       : Single;
  Center   : TPointFloat;
  Line     : PRGB24Array;
  LEDColor : TRGB24;
  Scale    : Single;
  Bright   : Single;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;
   Assert(Bitmap.PixelFormat = pf24bit); 

   LEDColor.R := $FF and FLEDColor;
   LEDColor.G := $FF and (FLEDColor shr 8);
   LEDColor.B := $FF and (FLEDColor shr 16);
   Bright := 0.3 + 0.007 * FBrightness;

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - FLineWidth div 2;
   if Rad <= 0 then Exit;
   BW := 1 - FLineWidth * OversamplingFactor / Rad;

   Center.x := 0.5 * Width;
   Center.y := 0.5 * Height;
   Pen.Color := FLineColor;
   Brush.Color := FLEDColor;

   {$IFNDEF FPC}
   for I := 0 to round(2 * Rad) do
    begin
     XStart := Sqrt(abs(Sqr(rad) - Sqr(Rad - i)));
     Line := Scanline[round(Center.y - (Rad - i))];
     for Steps := Round(Center.x - XStart) to Round(Center.x + XStart) do
      begin
       Scale := Bright * (1 - FUniformity * Math.Max(0, (Sqr(steps - Center.x) + Sqr(Rad - i)) / Sqr(Rad)));

       if Sqr(steps - Center.x) + Sqr(Rad - i) > Sqr(BW * Rad)
        then Scale := 0.4 * Scale;
       Line[Steps].B := Round(Scale * LEDColor.B);
       Line[Steps].G := Round(Scale * LEDColor.G);
       Line[Steps].R := Round(Scale * LEDColor.R);
//       assert(Integer(@(Line[Steps])) and 1 <> 1);
      end;
    end;
   {$ENDIF}
  end;
end;

procedure TCustomGuiLED.RenderLEDToBitmap32(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Rad      : Single;
  XStart   : Single;
  BW       : Single;
  Center   : TPointFloat;
  Line     : PRGB32Array;
  LEDColor : TRGB32;
  Scale    : Single;
  Bright   : Single;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   LEDColor.A := $FF;
   LEDColor.R := $FF and FLEDColor;
   LEDColor.G := $FF and (FLEDColor shr 8);
   LEDColor.B := $FF and (FLEDColor shr 16);
   Bright := 0.3 + 0.007 * FBrightness;

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - 0.5 * FLineWidth;
   if Rad <= 0 then Exit;
   BW := 1 - FLineWidth * OversamplingFactor / Rad;

   Center.x := 0.5 * Width;
   Center.y := 0.5 * Height;
   Pen.Color := FLineColor;
   Brush.Color := FLEDColor;

   {$IFNDEF FPC}
   for i := 0 to Round(2 * Rad) do
    begin
     XStart := Sqrt(abs(Sqr(Rad) - Sqr(Rad - i)));
     Line := Scanline[round(Center.y - (Rad - i))];
     for steps := round(Center.x - XStart) to round(Center.x + XStart) do
      begin
       Scale := Bright * (1 - FUniformity * Math.Max(0, (Sqr(Steps - Center.x) + Sqr(Rad - i)) / Sqr(rad)));

       if Sqr(steps - Center.x) + Sqr(Rad - i) > Sqr(BW * Rad)
        then Scale := 0.4 * Scale;
       Line[steps].B := round(Scale * LEDColor.B);
       Line[steps].G := round(Scale * LEDColor.G);
       Line[steps].R := round(Scale * LEDColor.R);
      end;
    end;
   {$ENDIF}
  end;
end;

procedure TCustomGuiLED.SetBrightness(const Value: Single);
begin
 if FBrightness <> Value then
  begin
   FBrightness := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiLED.UpdateBuffer;
var
  Bmp : TBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    try
     if AntiAlias = gaaNone then
      begin
       Assert(FBuffer.Width = Width);
       Assert(FBuffer.Height = Height);

       // draw background
       {$IFNDEF FPC}
       if FTransparent
        then DrawParentImage(FBuffer.Canvas)
        else
       {$ENDIF}
        begin
         Brush.Color := Self.Color;
         FillRect(ClipRect);
        end;

       // render bitmap depending on bit depth
       case FBuffer.PixelFormat of
        pf24bit : RenderLEDToBitmap24(FBuffer);
        pf32bit : RenderLEDToBitmap32(FBuffer);
        else raise Exception.Create(RCStrWrongPixelFormat);
       end;

      end
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := OversamplingFactor * FBuffer.Width;
         Height      := OversamplingFactor * FBuffer.Height;
         {$IFNDEF FPC}
         if FTransparent then
          begin
           DrawParentImage(Bmp.Canvas);
           UpsampleBitmap(Bmp);
          end
         else
         {$ENDIF}
          with Bmp.Canvas do
           begin
            Brush.Color := Self.Color;
            FillRect(ClipRect);
           end;

         // render bitmap depending on bit depth
         case FBuffer.PixelFormat of
          pf24bit : RenderLEDToBitmap24(Bmp);
          pf32bit : RenderLEDToBitmap32(Bmp);
          else raise Exception.Create(RCStrWrongPixelFormat);
         end;

         DownsampleBitmap(Bmp);
         FBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;
    finally
     Unlock;
    end;
   end;
end;

procedure TCustomGuiLED.SetLEDColor(const Value: TColor);
begin
 if (Value <> FLEDColor) then
  begin
   FLEDColor := Value;
   Invalidate;
  end;
end;

end.
