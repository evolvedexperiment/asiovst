unit DAV_GuiPanel;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Messages, Controls, Graphics, ExtCtrls, DAV_GuiBaseControl,
  DAV_GuiPixelMap;

type
  TCustomGuiPanel = class(TCustomPanel)
  private
    FBorderVisible  : Boolean;
    FPanelColor     : TColor;
    FOwnerDraw      : Boolean;
    FRoundRadius    : Single;
    FTransparent    : Boolean;
    FBorderWidth    : Single;
    FLineColor      : TColor;
    FPixelMap       : TGuiPixelMapMemory;
    FBitmapChanged  : Boolean;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetBorderVisible(const Value: Boolean);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetLineColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetPanelColor(const Value: TColor);
    procedure SetRoundRadius(const Value: Single);
    procedure SetTransparent (const Value: Boolean);
    procedure SetBitmapChanged(const Value: Boolean);
    procedure LineColorChanged;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure PaintBitmap; virtual;
    procedure OwnerDrawChanged; virtual;
    procedure RenderPanel(PixelMap: TGuiCustomPixelMap);
    procedure RenderPanelNew(const PixelMap: TGuiCustomPixelMap);
    procedure CopyParentImage(PixelMap: TGuiCustomPixelMap); virtual;

    property BitmapChanged: Boolean read FBitmapChanged write SetBitmapChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;

    property BorderVisible: Boolean read FBorderVisible write SetBorderVisible default True;
    property PanelColor: TColor read FPanelColor write SetPanelColor default clBtnShadow;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default True;
    property LineColor: TColor read FLineColor write SetLineColor default clBtnHighlight;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property Radius: Single read FRoundRadius write SetRoundRadius;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiPanel = class(TCustomGuiPanel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderVisible;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property OwnerDraw;
    property Hint;
    property LineColor;
    property BorderWidth;
    property PanelColor;
    property ParentBiDiMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Radius;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager;
    property Visible;
    {$IFNDEF FPC}
    property Transparent;
    property OnCanResize;
    {$ENDIF}
    property OnEndDock;
    property OnStartDock;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  Types, SysUtils, Math, DAV_Common, DAV_Math, DAV_Complex, DAV_Approximations,
  DAV_GuiCommon, DAV_GuiBlend;

{ TCustomGuiPanel }

constructor TCustomGuiPanel.Create (AOwner: TComponent);
begin
 inherited Create(AOwner);
 FPanelColor          := clBtnHighlight;
 FLineColor           := clBtnShadow;
 FBorderWidth         := 2;
 FRoundRadius         := 2;
 FBorderVisible       := True;
 FOwnerDraw           := True;
 FPixelMap            := TGuiPixelMapMemory.Create;
 FBitmapChanged       := True;
 FTransparent         := False;
 ParentColor          := True;
 ControlStyle         := ControlStyle + [csAcceptsControls, csOpaque];
 SetBounds(0, 0, 185, 41);
end;

destructor TCustomGuiPanel.Destroy;
begin
 FreeAndNil(FPixelMap);
 inherited;
end;

procedure TCustomGuiPanel.SetOwnerDraw(const Value: Boolean);
begin
 if FOwnerDraw <> Value then
  begin
   FOwnerDraw := Value;
   OwnerDrawChanged;
  end;
end;

procedure TCustomGuiPanel.SetLineColor(const Value: TColor);
begin
 if FLineColor <> Value then
  begin
   FLineColor := Value;
   LineColorChanged;
  end;
end;

procedure TCustomGuiPanel.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   if FOwnerDraw then BitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.SetPanelColor(const Value: TColor);
begin
 if FPanelColor <> Value then
  begin
   FPanelColor := Value;
   if FOwnerDraw then BitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.OwnerDrawChanged;
begin
 if FOwnerDraw
  then BitmapChanged := True
  else Invalidate;
end;

procedure TCustomGuiPanel.LineColorChanged;
begin
 if FOwnerDraw
  then BitmapChanged := True;
end;

procedure TCustomGuiPanel.PaintBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FPixelMap do
   begin
    {$IFNDEF FPC}if FTransparent then CopyParentImage(FPixelMap) else {$ENDIF}
    Clear(Color);
    RenderPanelNew(FPixelMap);
   end;
end;

procedure TCustomGuiPanel.Paint;
begin
 if not FOwnerDraw
  then inherited
  else
   begin
    if FBitmapChanged then
     begin
      FBitmapChanged := False;
      PaintBitmap;
     end;
    FPixelMap.PaintTo(Canvas);
   end;
end;

procedure TCustomGuiPanel.RenderPanelNew(const PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLne            : PPixel32Array;
  PanelColor        : TPixel32;
  BorderColor       : TPixel32;
  CombColor         : TPixel32;
  Radius            : Single;
  XStart            : Single;
  BorderWidth       : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  Scale             : Single;
  Center            : TComplexSingle;
  SqrDist           : Single;
  Bright            : Single;
  SqrRadMinusOne    : Single;
  ReciSqrRad        : Single;
  Temp              : Single;
  BorderFactor      : Single;
  CombAlpha         : Integer;

const
  CBlack : TPixel32 = (ARGB : $FF000000);
begin
 with PixelMap do
  begin
   PanelColor := ConvertColor(FPanelColor);
   BorderColor := ConvertColor(FLineColor);

   // draw circle
   Radius := Min(FRoundRadius, 0.5 * Width);
   Radius := Min(Radius, 0.5 * Height);
   BorderWidth := Max(FBorderWidth + 1, 2);

   ReciSqrRad := 1 / Sqr(Radius);
   RadMinusBorderOne := BranchlessClipPositive(Radius - BorderWidth + 1);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - BorderWidth));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   Center.Re := Radius;
   Center.Im := Radius;

   for Y := 0 to Round(Radius) - 1  do
    begin
     XStart := Sqr(Radius) - Sqr(Y - Radius);
     if XStart < 0
      then Continue
      else XStart := Sqrt(XStart) - 0.4999999;
     ScnLne := Scanline[Y];

     for X := Round(Radius - XStart) to Round(Width - Radius + XStart) do
      begin
       // calculate squared distance
       if X < Radius
        then SqrDist := Sqr(X - Radius) + Sqr(Y - Radius) else
       if X > Width - Radius
        then SqrDist := Sqr(X - Width + Radius) + Sqr(Y - Radius)
        else SqrDist := Sqr(Y - Radius);

       if SqrDist <= SqrRadMinusBorder
        then CombColor := PanelColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombineRegister(BorderColor, PanelColor, Round($FF - Temp * $FF));
        end else
       if SqrDist < SqrRadMinusOne
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       BlendMemory(CombColor, ScnLne[X]);
       EMMS;
      end;
    end;

   for Y := Round(Radius) to Round(Height - Radius) do
    begin
     ScnLne := Scanline[Y];
     for X := 1 to Width - 1 do
      begin
       // check whether position is a border
       if (Y < BorderWidth - 1) or (Y > Height - BorderWidth + 1)
        then CombColor := BorderColor else

       // check whether position is an upper half border
       if (Y < BorderWidth) then
        begin
         Temp := BorderWidth - Y;
         if (X < BorderWidth - 1) or (X > Width - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF));
          end else
         if (X > Width - BorderWidth) then
          begin
           Temp := Temp + (X - Width + BorderWidth) * (1 - Temp);
           CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF))
          end
         else CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF));
        end else

       // check whether position is a lower half border
       if (Y > Height - BorderWidth) then
        begin
         Temp := Y - (Height - BorderWidth);
         if (X < BorderWidth - 1) or (X > Width - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF));
          end else
         if (X > Width - BorderWidth) then
          begin
           Temp := Temp + (X - Width + BorderWidth) * (1 - Temp);
           CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF))
          end
         else CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF));
        end else
       if (X > Width - BorderWidth) then
        begin
         Temp := X - (Width - BorderWidth);
         CombColor := CombineRegister(BorderColor, PanelColor, Round(Temp * $FF));
        end
       else CombColor := PanelColor;

       BlendMemory(CombColor, ScnLne[X]);
       EMMS;
      end;
    end;

   for Y := Round(Height - Radius) + 1 to Round(Height)- 1 do
    begin
     XStart := Sqr(Radius) - Sqr(Y - Height + Radius);
     if XStart < 0
      then Continue
      else XStart := Sqrt(XStart) - 0.4999999;
     ScnLne := Scanline[Y];

     for X := Round(Radius - XStart) to Round(Width - Radius + XStart) do
      begin
       // calculate squared distance
       if X < Radius
        then SqrDist := Sqr(X - Radius) + Sqr(Y - Height + Radius) else
       if X > Width - Radius
        then SqrDist := Sqr(X - Width + Radius) + Sqr(Y - Height + Radius)
        else SqrDist := Sqr(Y - Height + Radius);

       if SqrDist <= SqrRadMinusBorder
        then CombColor := PanelColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombineRegister(BorderColor, PanelColor, Round($FF - Temp * $FF));
        end else
       if SqrDist < SqrRadMinusOne
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       BlendMemory(CombColor, ScnLne[X]);
       EMMS;
      end;
    end;
  end;
end;

procedure TCustomGuiPanel.RenderPanel(PixelMap: TGuiCustomPixelMap);
var
  Val, Off : TComplexDouble;
  Steps, i : Integer;
  tmp      : Single;
  rad      : Integer;
  PtsArray : Array of TPoint;
begin
 with PixelMap do
  begin
{   Lock;

   Brush.Style := bsClear;
   Brush.Color := FPanelColor;
   Pen.Width   := FOSFactor * FLineWidth;
   Pen.Color   := FLineColor;

   case FRoundRadius of
    0, 1 : begin
            Brush.Color := FLineColor;
            FrameRect(ClipRect);
           end;
(*
       2 : begin
            with ClipRect do
             Polygon([Point(Left  + 1, Bottom - 2), Point(Left     , Bottom - 3),
                      Point(Left     , Top    + 2), Point(Left  + 2, Top       ),
                      Point(Right - 3, Top       ), Point(Right - 1, Top    + 2),
                      Point(Right - 2, Top    + 1), Point(Right - 1, Top    + 2),
                      Point(Right - 1, Bottom - 2), Point(Right - 3, Bottom - 1),
                      Point(Left  + 2, Bottom - 1), Point(Left,      Bottom - 3)]);
           end;
*)
    else
     begin
      rad := FOSFactor * FRoundRadius;
      Steps := Round(2 / arcsin(1 / FRoundRadius)) + 1;
      if Steps > 1 then
       begin
        SetLength(PtsArray, Steps + 4);
        Val.Im := 0; Val.Re := -1;
        Val.Re := Val.Re * rad; Val.Im := Val.Im * rad;

        GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
        PtsArray[0] := Point(Round(Linewidth div 2), Round(Linewidth div 2 + rad));

        // upper left corner
        for i := 1 to Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(Linewidth div 2 + rad + Val.Re), Round(Linewidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 4] := Point(Linewidth div 2 + rad, Linewidth div 2 + 0);

        // upper right corner
        for i := Steps div 4 to Steps div 2 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 1] := Point(Round(ClipRect.Right - rad - (Linewidth + 1) div 2 + Val.Re), Round(Linewidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 2 + 1] := Point(ClipRect.Right - (Linewidth + 1) div 2, Linewidth div 2 + rad);

        // lower right corner
        for i := Steps div 2 to 3 * Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 2] := Point(Round(ClipRect.Right - rad - (Linewidth + 1) div 2 + Val.Re), Round(ClipRect.Bottom - (Linewidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[3 * Steps div 4 + 2] := Point(ClipRect.Right - rad - (Linewidth + 1) div 2, ClipRect.Bottom - (Linewidth + 1) div 2);

        // lower left corner
        for i := 3 * Steps div 4 to Steps - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(Linewidth div 2 + rad + Val.Re), Round(ClipRect.Bottom - (Linewidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[Steps + 3] := Point(Linewidth div 2, rad + Linewidth div 2);

        PolyGon(PtsArray);
        if FLineColor <> FPanelColor
         then PolyLine(PtsArray);
       end;
     end;
   end;

   Unlock;
}
  end;
end;

procedure TCustomGuiPanel.Resize;
begin
 inherited;
 if FPixelMap.Width <> Width then
  begin
   FPixelMap.Width := Width;
   FBitmapChanged := True;
  end;
 if FPixelMap.Height <> Height then
  begin
   FPixelMap.Height := Height;
   FBitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.SetRoundRadius(const Value: Single);
begin
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   if FOwnerDraw then BitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 if FOwnerDraw then BitmapChanged := True;
end;

procedure TCustomGuiPanel.CopyParentImage(PixelMap: TGuiCustomPixelMap);
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

procedure TCustomGuiPanel.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   if FOwnerDraw then BitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.SetBitmapChanged(const Value: Boolean);
begin
 FBitmapChanged := Value;
 if FBitmapChanged
  then Invalidate;
end;

procedure TCustomGuiPanel.SetBorderVisible(const Value: Boolean);
begin
 if FBorderVisible <> Value then
  begin
   FBorderVisible := Value;
   if FOwnerDraw then BitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.AssignTo(Dest: TPersistent);
begin
 if Dest is TBitmap
  then (Dest as TBitmap).Canvas.Assign(Canvas)
  else inherited;
end;

end.
