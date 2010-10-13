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
  {$IFDEF FPC} LCLIntf, LCLType, LResources, LMessages, FPImage, IntfGraphics,
  {$ELSE} Windows, {$ENDIF} Classes, Graphics, Forms, Messages, SysUtils,
  Controls, DAV_GuiCommon, DAV_GuiPixelMap;

type
  TParentControl = class(TWinControl);

  TCustomGuiLED = class(TGraphicControl)
  private
    FPixelMap     : TGuiCustomPixelMap;
    FOnPaint      : TNotifyEvent;

    FLEDColor     : TColor;
    FBrightness   : Single;
    FUniformity   : Single;
    FBorderFactor : Single;
    FBorderWidth  : Single;
    FTransparent  : Boolean;
    FOnChange     : TNotifyEvent;
    function GetUniformity: Single;
    function GetBorderStrength: Single;
    procedure SetLEDColor(const Value: TColor);
    procedure SetBrightness(const Value: Single);
    procedure SetUniformity(const Value: Single);
    procedure SetBorderStrength(const Value: Single);
    procedure SetTransparent(const Value: Boolean);
  protected
    {$IFNDEF FPC}
    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave : TNotifyEvent;
    FOnMouseEnter : TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$ELSE}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message: TLmEraseBkgnd); message LM_ERASEBKGND;
    {$ENDIF}

    procedure RenderLED(const PixelMap: TGuiCustomPixelMap);
    procedure SetBorderWidth(const Value: Single);

    procedure Paint; override;
    procedure ResizeBuffer; virtual;
    procedure UpdateBuffer; virtual;
    procedure CopyParentImage(PixelMap: TGuiCustomPixelMap); virtual;

    procedure BorderStrengthChanged; virtual;
    procedure BrightnessChanged; virtual;
    procedure LEDColorChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure TransparentChanged; virtual;
    procedure UniformityChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resize; override;
    procedure Loaded; override;

    property BorderStrength_Percent: Single read GetBorderStrength write SetBorderStrength;
    property Brightness_Percent: Single read FBrightness write SetBrightness;
    property LEDColor: TColor read FLEDColor write SetLEDColor default clBlack;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Uniformity_Percent: Single read GetUniformity write SetUniformity;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiLED = class(TCustomGuiLED)
  published
    property BorderStrength_Percent;
    property Brightness_Percent;
    property LEDColor;
    property BorderWidth;
    property Uniformity_Percent;
    property Transparent;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
    {$IFDEF Delphi6_Up}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$IFDEF DELPHI8_Up}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    {$ENDIF}
  end;

implementation

uses
  ExtCtrls, Math, DAV_Math, DAV_Common, DAV_Complex, DAV_Approximations, DAV_GuiBlend;

procedure CopyParentImage(Control: TControl; PixelMap: TGuiCustomPixelMap);
var
  I         : Integer;
  SubCount  : Integer;
  SaveIndex : Integer;
  Pnt       : TPoint;
  R, SelfR  : TRect;
  CtlR      : TRect;
  ParentDC  : HDC;
  CompDC    : HDC;
  CtrlCnvs  : TControlCanvas;
  Bmp       : TBitmap;
const
  Gray : TPixel32 = (ARGB : $7F700F7F);
begin
 if (Control = nil) or (Control.Parent = nil) then Exit;
 SubCount := Control.Parent.ControlCount;
 // set
 {$IFDEF WIN32}
 with Control.Parent
  do ControlState := ControlState + [csPaintCopy];
 try
 {$ENDIF}

  with Control do
   begin
    SelfR := Bounds(Left, Top, Width, Height);
    Pnt.X := -Left;
    Pnt.Y := -Top;
   end;

(*
  Bmp := TBitmap.Create;
  with Bmp do
   try
    Width := Control.Width;
    Height := Control.Height;
    PixelFormat := pf32bit;

    // Copy parent control image
    SaveIndex := SaveDC(Canvas.Handle);
    try
     SetViewportOrgEx(Canvas.Handle, Pnt.X, Pnt.Y, nil);
     IntersectClipRect(Canvas.Handle, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
     with TParentControl(Control.Parent) do
      begin
       Perform(WM_ERASEBKGND, Canvas.Handle, 0);
       PaintWindow(Canvas.Handle);
      end;
     PixelMap.Draw(Bmp);
    finally
     RestoreDC(Canvas.Handle, SaveIndex);
    end;
   finally
    Free;
   end;
*)

(*
  CtrlCnvs := TControlCanvas.Create;
  try
   CtrlCnvs.Handle := Control.Parent.Handle;
   PixelMap.Draw(CtrlCnvs);
  finally
   CtrlCnvs.Free;
  end;
*)


(*
  ParentDC := GetDC(Control.Parent.Handle);
  try
   CompDC := CreateCompatibleDC(CompDC);
   try
//    PixelMap.D
//    GetDIBits()
     Bitmap := CreateDIBSection(DeviceContext, FBitmapInfo,
       DIB_RGB_COLORS, Buffer, 0, 0);
   finally
    DeleteDC(CompDC);
   end;
  finally
   ReleaseDC(Control.Parent.Handle, ParentDC);
  end;
*)
//  GetDIBits(



  Bmp := TBitmap.Create;
  with Bmp do
   try
    Width := Control.Width;
    Height := Control.Height;
    PixelFormat := pf32bit;

    // Copy parent control image
    SaveIndex := SaveDC(Canvas.Handle);
    try
     SetViewportOrgEx(Canvas.Handle, Pnt.X, Pnt.Y, nil);
     IntersectClipRect(Canvas.Handle, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
     with TParentControl(Control.Parent) do
      begin
       Perform(WM_ERASEBKGND, Canvas.Handle, 0);
       PaintWindow(Canvas.Handle);
      end;
    finally
     RestoreDC(Canvas.Handle, SaveIndex);
    end;

    // Copy images of graphic controls
    for I := 0 to SubCount - 1 do
     begin
      if Control.Parent.Controls[I] = Control then Break else
       if (Control.Parent.Controls[I] <> nil) and
          (Control.Parent.Controls[I] is TGraphicControl)
        then
         with TGraphicControl(Control.Parent.Controls[I]) do
          begin
           CtlR := Bounds(Left, Top, Width, Height);
           if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
            begin
             {$IFDEF WIN32}
             ControlState := ControlState + [csPaintCopy];
             {$ENDIF}
             SaveIndex := SaveDC(Canvas.Handle);
             try
              SaveIndex := SaveDC(Canvas.Handle);
              SetViewportOrgEx(Canvas.Handle, Left + Pnt.X, Top + Pnt.Y, nil);
              IntersectClipRect(Canvas.Handle, 0, 0, Width, Height);
              Perform(WM_PAINT, Canvas.Handle, 0);
             finally
              RestoreDC(Handle, SaveIndex);
              {$IFDEF WIN32}
              ControlState := ControlState - [csPaintCopy];
              {$ENDIF}
             end;
            end;
          end;
     end;
    PixelMap.Draw(Bmp);
   finally
    Free;
   end;

 {$IFDEF WIN32}
 finally
   with Control.Parent do ControlState := ControlState - [csPaintCopy];
 end;
 {$ENDIF}
end;


{ TCustomGuiLED }

constructor TCustomGuiLED.Create(AOwner: TComponent);
begin
 inherited;
 FPixelMap := TGuiPixelMapMemory.Create;

 FLEDColor    := clRed;
 FBorderWidth := 1.5;
 FBrightness  := 100;
 FUniformity  := 0.4;

 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
end;

destructor TCustomGuiLED.Destroy;
begin
 FreeAndNil(FPixelMap);
 inherited;
end;

procedure TCustomGuiLED.Resize;
begin
 inherited Resize;
 ResizeBuffer;
end;

procedure TCustomGuiLED.ResizeBuffer;
begin
 if (Width > 0) and (Height > 0) then
  begin
   FPixelMap.Width := Width;
   FPixelMap.Height := Height;
   UpdateBuffer;
   Invalidate;
  end;
end;

procedure TCustomGuiLED.Loaded;
begin
 inherited;
 ResizeBuffer;
end;

procedure TCustomGuiLED.Paint;
begin
 FPixelMap.PaintTo(Canvas);
 if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TCustomGuiLED.CopyParentImage(PixelMap: TGuiCustomPixelMap);
var
  I         : Integer;
  SubCount  : Integer;
  SaveIndex : Integer;
  Pnt       : TPoint;
  R, SelfR  : TRect;
  CtlR      : TRect;
  ParentDC  : HDC;
  CompDC    : HDC;
  CtrlCnvs  : TControlCanvas;
  Bmp       : TBitmap;
const
  Gray : TPixel32 = (ARGB : $7F700F7F);
begin
 if (Parent = nil) then Exit;
 SubCount := Parent.ControlCount;
 // set
 {$IFDEF WIN32}
 with Parent
  do ControlState := ControlState + [csPaintCopy];
 try
 {$ENDIF}

  SelfR := Bounds(Left, Top, Width, Height);
  Pnt.X := -Left;
  Pnt.Y := -Top;

  Bmp := TBitmap.Create;
  with Bmp do
   try
    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf32bit;

    // Copy parent control image
    SaveIndex := SaveDC(Canvas.Handle);
    try
     SetViewportOrgEx(Canvas.Handle, Pnt.X, Pnt.Y, nil);
     IntersectClipRect(Canvas.Handle, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
     with TParentControl(Parent) do
      begin
       Perform(WM_ERASEBKGND, Canvas.Handle, 0);
       PaintWindow(Canvas.Handle);
      end;
    finally
     RestoreDC(Canvas.Handle, SaveIndex);
    end;

    // Copy images of graphic controls
    for I := 0 to SubCount - 1 do
     begin
      if Parent.Controls[I] = Self then Break else
       if (Parent.Controls[I] <> nil) and
          (Parent.Controls[I] is TGraphicControl)
        then
         with TGraphicControl(Parent.Controls[I]) do
          begin
           CtlR := Bounds(Left, Top, Width, Height);
           if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
            begin
             {$IFDEF WIN32}
             ControlState := ControlState + [csPaintCopy];
             {$ENDIF}
             SaveIndex := SaveDC(Canvas.Handle);
             try
              SaveIndex := SaveDC(Canvas.Handle);
              SetViewportOrgEx(Canvas.Handle, Left + Pnt.X, Top + Pnt.Y, nil);
              IntersectClipRect(Canvas.Handle, 0, 0, Width, Height);
              Perform(WM_PAINT, Canvas.Handle, 0);
             finally
              RestoreDC(Handle, SaveIndex);
              {$IFDEF WIN32}
              ControlState := ControlState - [csPaintCopy];
              {$ENDIF}
             end;
            end;
          end;
     end;
    PixelMap.Draw(Bmp);
   finally
    Free;
   end;

 {$IFDEF WIN32}
 finally
   with Parent do ControlState := ControlState - [csPaintCopy];
 end;
 {$ENDIF}
end;

procedure TCustomGuiLED.BorderStrengthChanged;
begin
 UpdateBuffer;
 Invalidate;
end;

procedure TCustomGuiLED.BrightnessChanged;
begin
 UpdateBuffer;
 Invalidate;
end;

procedure TCustomGuiLED.LEDColorChanged;
begin
 UpdateBuffer;
 Invalidate;
end;

procedure TCustomGuiLED.BorderWidthChanged;
begin
 UpdateBuffer;
 Invalidate;
end;

procedure TCustomGuiLED.TransparentChanged;
begin
 UpdateBuffer;
 Invalidate;
end;

procedure TCustomGuiLED.UniformityChanged;
begin
 UpdateBuffer;
 Invalidate;
end;


procedure TCustomGuiLED.SetBorderStrength(const Value: Single);
begin
 if BorderStrength_Percent <> Value then
  begin
   FBorderFactor := 1 - Limit(0.01 * Value, 0, 1);
   BorderStrengthChanged;
  end;
end;

procedure TCustomGuiLED.SetBrightness(const Value: Single);
begin
 if FBrightness <> Value then
  begin
   FBrightness := Value;
   BrightnessChanged;
  end;
end;

procedure TCustomGuiLED.SetLEDColor(const Value: TColor);
begin
 if (Value <> FLEDColor) then
  begin
   FLEDColor := Value;
   LEDColorChanged;
  end;
end;

procedure TCustomGuiLED.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiLED.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiLED.SetUniformity(const Value: Single);
begin
 if Uniformity_Percent <> Value then
  begin
   FUniformity := Sqr(1 - Limit(0.01 * Value, 0, 1));
   UniformityChanged;
  end;
end;

procedure TCustomGuiLED.UpdateBuffer;
begin
 if FTransparent
  then CopyParentImage(FPixelMap)
  else FPixelMap.Clear(Color);
 RenderLED(FPixelMap);
end;

procedure TCustomGuiLED.RenderLED(const PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLne            : PPixel32Array;
  LEDColor          : TPixel32;
  CombColor         : TPixel32;
  Radius            : Single;
  XStart            : Single;
  RadMinusBorderOne : Single;
  Scale             : Single;
  Center            : TComplexSingle;
  SqrYDist          : Single;
  SqrDist           : Single;
  Bright            : Single;
  SqrRadMinusOne    : Single;
  SqrRadMinusBorder : Single;
  ReciSqrRad        : Single;
  Temp              : Single;
  CombAlpha         : Integer;

const
  CBlack : TPixel32 = (ARGB : $FF000000);
begin
 with PixelMap do
  begin
   LEDColor := ConvertColor(FLEDColor);
   Bright := 0.3 + 0.007 * FBrightness;

   // draw circle
   Radius := 0.5 * Math.Min(Width, Height) - 3;
   if Radius <= 0 then Exit;

   ReciSqrRad := 1 / Sqr(Radius);
   RadMinusBorderOne := BranchlessClipPositive(Radius - FBorderWidth + 1);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - FBorderWidth));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   Center.Re := 0.5 * Width;
   Center.Im := 0.5 * Height;

   for Y := Round(Center.Im - Radius) to Round(Center.Im + Radius) do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(Radius) - SqrYDist;
     if XStart < 0
      then Continue
      else XStart := Sqrt(XStart) - 0.4999999;

     ScnLne := Scanline[Y];
     for X := Round(Center.Re - XStart) to Round(Center.Re + XStart) do
      begin
       // calculate squared distance
       SqrDist := Sqr(X - Center.Re) + SqrYDist;
       Scale := Bright * (1 - FUniformity * SqrDist * ReciSqrRad);

       if FBorderWidth > 1.5 then
        begin
         if SqrDist <= SqrRadMinusBorder
          then CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF))
          else
         if SqrDist <= Sqr(RadMinusBorderOne) then
          begin
           Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
           Scale := (Temp + (1 - Temp) * FBorderFactor) * Scale;

           CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF));
          end else
         if SqrDist < SqrRadMinusOne
          then CombColor := CombinePixel(LEDColor, CBlack, Round(FBorderFactor * Scale * $FF))
          else
           begin
            Scale := FBorderFactor * Scale;
            CombColor := CBlack;
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            CombinePixelInplace(LEDColor, CombColor, Round(Scale * $FF));
            CombColor.A := CombAlpha;
           end;
        end
       else
        begin
         if SqrDist < SqrRadMinusOne
          then CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF))
          else
           begin
            CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF));
            EMMS;
            Temp := FastSqrtBab2(SqrDist) - (Radius - 1);
            Scale := (Temp + (1 - Temp) * FBorderFactor) * 0.5 * FBorderWidth * Scale;
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            CombColor := CombinePixel(CBlack, CombColor, Round((1 - Scale) * $FF));
            CombColor.A := CombAlpha;

 (*
            Scale := FBorderFactor * Scale;
            CombColor := CBlack;
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            CombinePixelInplace(LEDColor, CombColor, Round(Scale * $FF));
            CombColor.A := CombAlpha;
            BlendPixelInplace(CombColor, ScnLne[X]);

 (*
            Scale := FBorderFactor * Scale;
            Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            Scale := (Temp + (1 - Temp) * FBorderFactor) * Scale;
            CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF));
            CombinePixelInplace(LEDColor, CombColor, Round(Scale * $FF));
            CombColor.A := CombAlpha;
            BlendPixelInplace(CombColor, ScnLne[X]);
 *)
           end;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       EMMS;
      end;
    end;
  end;
end;

function TCustomGuiLED.GetBorderStrength: Single;
begin
 Result := 100 * (1 - FBorderFactor);
end;

function TCustomGuiLED.GetUniformity: Single;
begin
 Result := 100 * (1 - Sqrt(FUniformity));
end;

{$IFNDEF FPC}
{$IFNDEF COMPILER10_UP}
procedure TCustomGuiLED.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomGuiLED.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;
{$ENDIF}

{$ELSE}
procedure TBufferedGraphicControl.WMEraseBkgnd(var Message: TLmEraseBkgnd);
begin
  Message.Result := 0;
end;
{$ENDIF}

procedure TCustomGuiLED.CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 UpdateBuffer;
 Invalidate;
end;

procedure TCustomGuiLED.CMEnabledChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 UpdateBuffer;
 Invalidate;
end;

procedure TCustomGuiLED.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
 Message.Result := 0;
end;

end.
