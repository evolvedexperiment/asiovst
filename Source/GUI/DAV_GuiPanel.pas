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
    FBorderVisible : Boolean;
    FPanelColor    : TColor;
    FOwnerDraw     : Boolean;
    FRoundRadius   : Single;
    FTransparent   : Boolean;
    FBorderWidth   : Single;
    FLineColor     : TColor;
    FBuffer        : TGuiPixelMapMemory;
    FBufferChanged : Boolean;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMChanged(var Message: TMessage); message CM_CHANGED;

    procedure SetBorderVisible(const Value: Boolean);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetLineColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetPanelColor(const Value: TColor);
    procedure SetRoundRadius(const Value: Single);
    procedure SetTransparent (const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure PaintBitmap; virtual;
    procedure Loaded; override;

    procedure OwnerDrawChanged; virtual;
    procedure LineColorChanged; virtual;
    procedure BorderVisibleChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure PanelColorChanged; virtual;
    procedure RoundRadiusChanged; virtual;
    procedure TransparentChanged; virtual;

    procedure RenderPanel(PixelMap: TGuiCustomPixelMap);
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
 FPanelColor    := clBtnHighlight;
 FLineColor     := clBtnShadow;
 FBorderWidth   := 2;
 FRoundRadius   := 2;
 FBorderVisible := True;
 FOwnerDraw     := True;
 FBuffer        := TGuiPixelMapMemory.Create;
 FBufferChanged := True;
 FTransparent   := False;
 ParentColor    := True;
 ControlStyle   := ControlStyle + [csAcceptsControls, csOpaque];
 DoubleBuffered := True;
 SetBounds(0, 0, 185, 41);
end;

destructor TCustomGuiPanel.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

procedure TCustomGuiPanel.AssignTo(Dest: TPersistent);
begin
 if Dest is TBitmap then
  begin
   (Dest as TBitmap).Canvas.Assign(Canvas);
  end else
 if Dest is TCustomGuiPanel then
  with TCustomGuiPanel(Dest) do
   begin
    FBorderVisible := Self.FBorderVisible;
    FPanelColor    := Self.FPanelColor;
    FOwnerDraw     := Self.FOwnerDraw;
    FRoundRadius   := Self.FRoundRadius;
    FTransparent   := Self.FTransparent;
    FBorderWidth   := Self.FBorderWidth;
    FLineColor     := Self.FLineColor;
    FBuffer        := Self.FBuffer;
    FBufferChanged := Self.FBufferChanged;
   end else inherited;
end;

procedure TCustomGuiPanel.Loaded;
begin
 inherited;
 Resize;
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
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiPanel.SetBorderVisible(const Value: Boolean);
begin
 if FBorderVisible <> Value then
  begin
   FBorderVisible := Value;
   BorderVisibleChanged;
  end;
end;

procedure TCustomGuiPanel.SetPanelColor(const Value: TColor);
begin
 if FPanelColor <> Value then
  begin
   FPanelColor := Value;
   PanelColorChanged;
  end;
end;

procedure TCustomGuiPanel.SetRoundRadius(const Value: Single);
begin
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   RoundRadiusChanged;
  end;
end;

procedure TCustomGuiPanel.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;


procedure TCustomGuiPanel.OwnerDrawChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.LineColorChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.BorderWidthChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.BorderVisibleChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.RoundRadiusChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.PanelColorChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.TransparentChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.PaintBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer do
   begin
    {$IFNDEF FPC}
    if FTransparent
     then FBuffer.CopyParentImage(Self) else
    {$ENDIF}
    Clear(Color);
    RenderPanel(FBuffer);
   end;
end;

procedure TCustomGuiPanel.Paint;
begin
 if not FOwnerDraw
  then inherited
  else
   begin
    if FBufferChanged then
     begin
      FBufferChanged := False;
      PaintBitmap;
     end;
    FBuffer.PaintTo(Canvas);
   end;
end;

procedure TCustomGuiPanel.RenderPanel(PixelMap: TGuiCustomPixelMap);
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
begin
 with PixelMap do
  begin
   PanelColor := ConvertColor(FPanelColor);
   if FBorderVisible
    then BorderColor := ConvertColor(FLineColor)
    else BorderColor := PanelColor;

   // draw circle
   Radius := Min(Min(FRoundRadius, 0.5 * Width), 0.5 * Height) + 1;
   BorderWidth := Max(FBorderWidth, 1);

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
  end;
end;

procedure TCustomGuiPanel.Resize;
begin
 inherited;

 if Assigned(FBuffer) then
  if FBuffer.Width <> Width then
   begin
    FBuffer.Width := Width;
    FBufferChanged := True;
   end;
  if FBuffer.Height <> Height then
   begin
    FBuffer.Height := Height;
    FBufferChanged := True;
   end;
end;

procedure TCustomGuiPanel.CMChanged(var Message: TMessage);
begin
 inherited;
 if FOwnerDraw
  then FBufferChanged := True;
 Invalidate;
end;

procedure TCustomGuiPanel.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 if FOwnerDraw
  then FBufferChanged := True;
 Invalidate;
end;

end.
