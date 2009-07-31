unit DAV_GuiGroup;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Forms, Classes, Graphics, Controls, StdCtrls,
  DAV_GuiCommon, DAV_GuiBaseControl;

type
  TCustomGuiGroup = class(TCustomGroupBox)
  private
    FAntiAlias   : TGuiAntiAlias;
    FAutoFocus   : Boolean;
    FCaption     : string;
    FLineColor   : TColor;
    FLineWidth   : Integer;
    FOSFactor    : Integer;
    FOwnerDraw   : Boolean;
    FRoundRadius : Integer;
    FTransparent : Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure DrawParentImage(Dest: TCanvas);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetCaption(const Value: string);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineWidth(const Value: Integer);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetRoundRadius(Value: Integer);
    procedure SetTransparent (const Value: Boolean);
    procedure WMMove(var Message: {$IFDEF FPC}TLMMove{$ELSE}TWMMove{$ENDIF}); message WM_MOVE;
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure RenderGroupToBitmap(Bitmap: TBitmap); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default True;
    property Caption: string read FCaption write SetCaption;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default True;
    property LineColor: TColor read FLineColor write SetLineColor default clBtnShadow;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property Radius: Integer read FRoundRadius write SetRoundRadius default 2;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TCustomGuiGroupA = class(TCustomGuiGroup)
  protected
    procedure RenderGroupToBitmap(Bitmap: TBitmap); override;
  end;

  TCustomGuiGroupB = class(TCustomGuiGroup)
  private
    FHeaderMinWidth : Integer;
    FOffset         : Integer;
    procedure SetHeaderMinWidth(const Value: Integer);
    procedure SetOffset(const Value: Integer);
  protected
    procedure RenderGroupToBitmap(Bitmap: TBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    property HeaderMinWidth: Integer read FHeaderMinWidth write SetHeaderMinWidth default 32;
    property Offset: Integer read FOffset write SetOffset default 0;
  end;

  TGuiGroupA = class(TCustomGuiGroupA)
  published
    property Align;
    property Anchors;
    property AntiAlias;
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
    property Font;
    property HelpContext;
    property Hint;
    property LineColor;
    property LineWidth;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Radius;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TGuiGroupB = class(TCustomGuiGroupB)
  published
    property Align;
    property Anchors;
    property AntiAlias;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Offset;
    property OwnerDraw;
    property Font;
    property HeaderMinWidth;
    property HelpContext;
    property Hint;
    property LineColor;
    property LineWidth;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Radius;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TGuiGroup = class(TGuiGroupB)
  end;

implementation

uses
  Math, DAV_Common, DAV_Complex;

{ TCustomGuiGroup }

constructor TCustomGuiGroup.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle    := ControlStyle + [csOpaque, //csReplicatable,
                                    csAcceptsControls];
 FOwnerDraw      := True;
 FOSFactor       := 1;
 FRoundRadius    := 2;
 FCaption        := 'Group'; //Name;
 FLineColor      := clBtnShadow;
 FLineWidth      := 1;
end;

procedure TCustomGuiGroup.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   case FAntiAlias of
         gaaNone : FOSFactor :=  1;
     gaaLinear2x : FOSFactor :=  2;
     gaaLinear4x : FOSFactor :=  4;
     gaaLinear8x : FOSFactor :=  8;
    gaaLinear16x : FOSFactor := 16;
   end;
   Invalidate;
  end;
end;

procedure TCustomGuiGroup.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiGroup.SetLineColor(const Value: TColor);
begin
 if FLineColor <> Value then
  begin
   FLineColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiGroup.SetLineWidth(const Value: Integer);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiGroup.SetRoundRadius(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   Invalidate;
  end;
end;


procedure TCustomGuiGroup.Click;
begin
 if FAutoFocus then SetFocus;
 inherited;
end;

procedure TCustomGuiGroup.CMDialogChar(var Message: TCMDialogChar);
begin
 with Message do
  if IsAccel(Message.CharCode, Caption) and CanFocus then
   begin
    SetFocus;
    Result := 1;
   end;
end;

procedure TCustomGuiGroup.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TCustomGuiGroup.CMParentColorChanged(var Message: TWMNoParams);
begin
 inherited;
 Invalidate;
end;

procedure TCustomGuiGroup.CMSysColorChange(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TCustomGuiGroup.DrawParentImage(Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: THandle;
  Position: TPoint;
begin
  if Parent = nil then Exit;
  DC := Dest.Handle;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, Position);
  SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
  IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
  Parent.Perform(WM_ERASEBKGND, Longint(DC), 0);
  Parent.Perform(WM_PAINT, Longint(DC), 0);
  RestoreDC(DC, SaveIndex);
end;

procedure TCustomGuiGroup.Paint;
var
  BorderRect : TRect;
  Buffer     : TBitmap;
begin
 BorderRect := ClientRect;

 if not FOwnerDraw or (Width <= 0) or (Height <= 0) then
  begin
   inherited;
   Exit;
  end;

 Buffer := TBitmap.Create;
 try
  with Buffer, Canvas do
   begin
    Lock;
    PixelFormat := pf32bit;
    Width  := FOSFactor * Self.Width;
    Height := FOSFactor * Self.Height;
    Brush.Style := bsSolid;
    Brush.Color := Self.Color;

    case FAntiAlias of
     gaaNone     :
      begin
       {$IFNDEF FPC}if FTransparent then DrawParentImage(Canvas) else {$ENDIF}
       FillRect(ClipRect);
       RenderGroupToBitmap(Buffer);
      end;
     gaaLinear2x :
      begin
       {$IFNDEF FPC}
       if FTransparent then
        begin
         DrawParentImage(Canvas);
         Upsample2xBitmap32(Buffer);
        end else
       {$ENDIF}
       FillRect(ClipRect);
       RenderGroupToBitmap(Buffer);
       Downsample2xBitmap32(Buffer);
      end;
     gaaLinear4x :
      begin
       {$IFNDEF FPC}
       if FTransparent then
        begin
         DrawParentImage(Canvas);
         Upsample4xBitmap32(Buffer);
        end else
       {$ENDIF}
       FillRect(ClipRect);
       RenderGroupToBitmap(Buffer);
       Downsample4xBitmap32(Buffer);
      end;
     gaaLinear8x :
      begin
       {$IFNDEF FPC}
       if FTransparent then
        begin
         DrawParentImage(Canvas);
         Upsample2xBitmap32(Buffer);
         Upsample4xBitmap32(Buffer);
        end else
       {$ENDIF}
       FillRect(ClipRect);
       RenderGroupToBitmap(Buffer);
       Downsample2xBitmap32(Buffer);
       Downsample4xBitmap32(Buffer);
      end;
     gaaLinear16x :
      begin
       {$IFNDEF FPC}
       if FTransparent then
        begin
         DrawParentImage(Canvas);
         Upsample4xBitmap32(Buffer);
         Upsample4xBitmap32(Buffer);
        end else
       {$ENDIF}
       FillRect(ClipRect);
       RenderGroupToBitmap(Buffer);
       Downsample4xBitmap32(Buffer);
       Downsample4xBitmap32(Buffer);
      end;
    end;
   end;
  Self.Canvas.Draw(0, 0, Buffer);
 finally
  FreeAndNil(Buffer);
 end;
end;

procedure TCustomGuiGroup.SetOwnerDraw(const Value: Boolean);
begin
 if FOwnerDraw <> Value then
  begin
   FOwnerDraw := Value;
   RecreateWnd;
   Invalidate;
  end;
end;

procedure TCustomGuiGroup.SetTransparent(const Value: Boolean);
begin
 FTransparent := Value;
 Invalidate;
end;

procedure TCustomGuiGroup.WMMove(var Message: TWMMove);
begin
 inherited;
 if FTransparent then Invalidate;
end;

{ TCustomGuiGroupA }

procedure TCustomGuiGroupA.RenderGroupToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexSingle;
  Steps, i : Integer;
  LineOffs : array[0..1] of Integer;
  PntArray : array of TPoint;
//  rct      : TRect;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;

   Brush.Style := bsSolid;
   Brush.Color := Color;
   Pen.Width   := FOSFactor * FLineWidth;
   Pen.Color   := FLineColor;
   Font.Assign(Self.Font);
   Font.Size := FOSFactor * Font.Size;
   TextSize := TextExtent(FCaption);

   case FRoundRadius of
    0, 1 : begin
            FrameRect(ClipRect);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
           end;
       2 : begin
            LineOffs[0] := Round(Linewidth div 2);
            LineOffs[1] := Round((Linewidth + 1) div 2);
            with ClipRect do
             begin
              PntArray[ 0] := Point(Left  + 1 + LineOffs[0], Bottom - 1 - LineOffs[1]);
              PntArray[ 1] := Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1]);
              PntArray[ 2] := Point(Left      + LineOffs[0], Top    + 2 + LineOffs[0]);
              PntArray[ 3] := Point(Left  + 2 + LineOffs[0], Top        + LineOffs[0]);
              PntArray[ 4] := Point(Right - 2 - LineOffs[1], Top        + LineOffs[0]);
              PntArray[ 5] := Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]);
              PntArray[ 6] := Point(Right - 1 - LineOffs[1], Top    + 1 + LineOffs[0]);
              PntArray[ 7] := Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]);
              PntArray[ 8] := Point(Right     - LineOffs[1], Bottom - 2 - LineOffs[1]);
              PntArray[ 9] := Point(Right - 2 - LineOffs[1], Bottom     - LineOffs[1]);
              PntArray[10] := Point(Left  + 2 + LineOffs[0], Bottom     - LineOffs[1]);
              PntArray[11] := Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1]);
             end;
            PolyLine(PntArray);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
            // MoveTo(TextSize.cx + 12, 1);
            // LineTo(TextSize.cx + 12, TextSize.cy + 3);
           end;
    else
     begin
      rad := FOSFactor * FRoundRadius;
      Steps := Round(2 / arcsin(1 / rad)) + 1;
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

(*
        // Draw inner text
        //////////////////

        Brush.Color   := FLineColor;
        SetLength(PtsArray, Steps div 2 + 5);
        Val.Re := -rad; Val.Im := 0;

        GetSinCos(2 * Pi / (Steps div 2 - 1), Off.Im, Off.Re);
        rct := Rect(Linewidth div 2, Linewidth div 2, max(TextSize.cx + 10, FOSFactor * FHeaderMinWidth) - (Linewidth + 1) div 2, TextSize.cy + 5 - (Linewidth + 1) div 2);
        PtsArray[0] := Point(Round(rct.Left), Round(rct.Top + rad));

        // upper left corner
        for i := 1 to (Steps div 4) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(rct.Left + rad + Val.Re), Round(rct.Top + rad + Val.Im));
         end;
        PtsArray[Steps div 4    ] := Point(rct.Left + rad, rct.Top);
        PtsArray[Steps div 4 + 1] := Point(rct.Right, rct.Top);
        PtsArray[Steps div 4 + 2] := Point(rct.Right, rct.Bottom - rad);

        Val.Re := rad; Val.Im := 0;

        // lower right corner
        for i := (Steps div 4) to (Steps div 2) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(rct.Right - rad + Val.Re), Round(rct.Bottom - rad + Val.Im));
         end;

        PtsArray[Steps div 2 + 3] := Point(rct.Right - rad, rct.Bottom);
        PtsArray[Steps div 2 + 4] := Point(rct.Left, rct.Bottom);

        Polygon(PtsArray);
*)
      end;
     end;
   end;

   Brush.Style := bsClear;
   TextOut(6, 2, FCaption);
   Unlock;
  end;
end;

{ TCustomGuiGroupB }

constructor TCustomGuiGroupB.Create(AOwner: TComponent);
begin
 inherited;
 FOffset         := 0;
 FHeaderMinWidth := 32;
end;

procedure TCustomGuiGroupB.SetHeaderMinWidth(const Value: Integer);
begin
 if FHeaderMinWidth <> Value then
  begin
   FHeaderMinWidth := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiGroupB.SetOffset(const Value: Integer);
begin
 if FOffset <> Value then
  begin
   FOffset := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiGroupB.RenderGroupToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexSingle;
  Steps, i : Integer;
  LineOffs : array[0..1] of Integer;
  rct      : TRect;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;

   Brush.Style := bsSolid;
   Brush.Color := Color;
   Pen.Width   := FOSFactor * FLineWidth;
   Pen.Color   := FLineColor;
   Font.Assign(Self.Font);
   Font.Size   := FOSFactor * Font.Size;
   TextSize    := TextExtent(FCaption);
   TextSize.cx := 2 * FOSFactor * (FLineWidth + FOffset) + TextSize.cx;
   if TextSize.cx < FHeaderMinWidth
    then TextSize.cx := TextSize.cx + FOSFactor * FHeaderMinWidth; 

   case FRoundRadius of
    0, 1 : begin
            FrameRect(ClipRect);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
           end;
       2 : begin
            LineOffs[0] := Round(Linewidth div 2);
            LineOffs[1] := Round((Linewidth + 1) div 2);
            with ClipRect do
             PolyLine([Point(Left  + 1 + LineOffs[0], Bottom - 1 - LineOffs[1]),
                       Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1]),
                       Point(Left      + LineOffs[0], Top    + 2 + LineOffs[0]),
                       Point(Left  + 2 + LineOffs[0], Top        + LineOffs[0]),
                       Point(Right - 2 - LineOffs[1], Top        + LineOffs[0]),
                       Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]),
                       Point(Right - 1 - LineOffs[1], Top    + 1 + LineOffs[0]),
                       Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]),
                       Point(Right     - LineOffs[1], Bottom - 2 - LineOffs[1]),
                       Point(Right - 2 - LineOffs[1], Bottom     - LineOffs[1]),
                       Point(Left  + 2 + LineOffs[0], Bottom     - LineOffs[1]),
                       Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1])]);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
            // MoveTo(TextSize.cx + 12, 1);
            // LineTo(TextSize.cx + 12, TextSize.cy + 3);
           end;
    else
     begin
      rad := FOSFactor * FRoundRadius;
      Steps := Round(2 / arcsin(1 / rad)) + 1;
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

        // Draw inner text
        //////////////////

        Brush.Color   := FLineColor;
        SetLength(PtsArray, Steps div 2 + 5);
        Val.Re := -rad; Val.Im := 0;

        GetSinCos(2 * Pi / (Steps div 2 - 1), Off.Im, Off.Re);
        rct := Rect(Linewidth div 2, Linewidth div 2, TextSize.cx + 10 - (Linewidth + 1) div 2, TextSize.cy + 5 - (Linewidth + 1) div 2);
        PtsArray[0] := Point(Round(rct.Left), Round(rct.Top + rad));

        // upper left corner
        for i := 1 to (Steps div 4) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(rct.Left + rad + Val.Re), Round(rct.Top + rad + Val.Im));
         end;
        PtsArray[Steps div 4    ] := Point(rct.Left + rad, rct.Top);
        PtsArray[Steps div 4 + 1] := Point(rct.Right, rct.Top);
        PtsArray[Steps div 4 + 2] := Point(rct.Right, rct.Bottom - rad);

        Val.Re := rad; Val.Im := 0;

        // lower right corner
        for i := (Steps div 4) to (Steps div 2) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(rct.Right - rad + Val.Re), Round(rct.Bottom - rad + Val.Im));
         end;

        PtsArray[Steps div 2 + 3] := Point(rct.Right - rad, rct.Bottom);
        PtsArray[Steps div 2 + 4] := Point(rct.Left, rct.Bottom);

        Polygon(PtsArray);
      end;
     end;
   end;

   Brush.Style := bsClear;
   TextOut(FOSFactor * (FLineWidth + FOffset), 2, FCaption);
   Unlock;
  end;
end;


(*

{ TMFControlsCustomGroupBox }

procedure TMFControlsCustomGroupBox.Paint;
var
  memoryBitmap   : TBitmap;
  borderRect,
  textBounds     : TRect;
  textSize       : TSize;
  textHeightHalf : Integer;
  format         : UINT;
begin

 memoryBitmap := TBitmap.Create; // create memory-bitmap to draw flicker-free
 try
  with memoryBitmap do
   begin
    Height           := ClientRect.Bottom;
    Width            := ClientRect.Right;
    Canvas.Font      := Self.Font;

    if Caption = '' then
     begin
      textSize.cx    := 0;
      textSize.cy    := Canvas.TextHeight('J');
      textHeightHalf := (textSize.cy div 2);
     end
    else
     begin
      textSize       := Canvas.TextExtent(Caption);
      textHeightHalf := (textSize.cy div 2);
     end;
   end;

  with ClientRect do
   if not (fBorder = brMFStyle) then
    begin
     {$IFDEF MFC_COMPILER_4_UP}
     if BidiMode = bdRightToLeft
      then textBounds := Rect(Right - 10 - textSize.cx, Top, Right - 10 , Top + textSize.cy)
      else textBounds := Rect(Left + 10, Top, Left + 10 + textSize.cx, Top + textSize.cy);
     {$ELSE}
     textBounds := Rect(Left + 10, Top, Left + 10 + textSize.cx, Top + textSize.cy);
    {$ENDIF}
     textBounds := Rect(Left + 10, Top, Right - 10, Top + textSize.cy);
    end
   else
    textBounds := Rect((Left + Right - textSize.cx) div 2, Top,
                       (Left + Right + textSize.cx) div 2, ClientRect.Top + textSize.cy);

  // Draw Background
  if FTransparent //or (Border=brFullRound)
   then DrawParentImage(Self, memoryBitmap.Canvas)
   else
    begin
     memoryBitmap.Canvas.Brush.Color := Self.Color;
     memoryBitmap.Canvas.FillRect(ClientRect);
    end;

  // Draw Border
  memoryBitmap.Canvas.Pen.Color := FBorderColor;
  case fBorder of
    brFull:
      {$IFDEF MFC_COMPILER_4_UP}
      if BidiMode = bdRightToLeft then
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Right - 15 - textSize.cx, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left, ClientRect.Bottom-1), Point(ClientRect.Right-1, ClientRect.Bottom-1),
          Point(ClientRect.Right-1, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Right - 7 , ClientRect.Top + textHeightHalf)])
      else
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Left + 5, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left, ClientRect.Bottom-1), Point(ClientRect.Right-1, ClientRect.Bottom-1),
          Point(ClientRect.Right-1, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf)]);
      {$ELSE}
      memoryBitmap.Canvas.Polyline([Point(ClientRect.Left + 5, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left, ClientRect.Bottom-1), Point(ClientRect.Right-1, ClientRect.Bottom-1),
        Point(ClientRect.Right-1, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf)]);
      {$ENDIF}
    brFullRound:
     with memoryBitmap do
      {$IFDEF MFC_COMPILER_4_UP}
      if BidiMode = bdRightToLeft then
        Canvas.Polyline([
          Point(ClientRect.Right - 15 - textSize.cx, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left  +  2, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left  +  1, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Left      , ClientRect.Top + textHeightHalf + 2),
          Point(ClientRect.Left      , ClientRect.Bottom - 3), Point(ClientRect.Left  + 1, ClientRect.Bottom - 2),
          Point(ClientRect.Left  +  2, ClientRect.Bottom - 1), Point(ClientRect.Right - 3, ClientRect.Bottom - 1),
          Point(ClientRect.Right -  2, ClientRect.Bottom - 2), Point(ClientRect.Right - 1, ClientRect.Bottom - 3),
          Point(ClientRect.Right -  1, ClientRect.Top + textHeightHalf + 2),
          Point(ClientRect.Right -  2, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Right -  3, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Right -  7, ClientRect.Top + textHeightHalf)])
      else
        Canvas.Polyline([
          Point(ClientRect.Left + 5 , ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left + 2 , ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left + 1 , ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Left     , ClientRect.Top + textHeightHalf + 2),
          Point(ClientRect.Left     , ClientRect.Bottom - 3), Point(ClientRect.Left + 1, ClientRect.Bottom-2),
          Point(ClientRect.Left  + 2, ClientRect.Bottom - 1), Point(ClientRect.Right-3, ClientRect.Bottom-1),
          Point(ClientRect.Right - 2, ClientRect.Bottom  - 2),  Point(ClientRect.Right-1, ClientRect.Bottom-3),
          Point(ClientRect.Right - 1, ClientRect.Top + textHeightHalf+2),
          Point(ClientRect.Right - 2, ClientRect.Top + textHeightHalf+1),
          Point(ClientRect.Right - 3, ClientRect.Top + textHeightHalf),
          Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf)]);
      {$ELSE}
      Canvas.Polyline([
        Point(ClientRect.Left + 5, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left, ClientRect.Bottom-1), Point(ClientRect.Right-1, ClientRect.Bottom-1),
        Point(ClientRect.Right-1, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf)]);
{
       Canvas.Brush.Color := Self.Color;
       Canvas.Pen.Color := Self.Color;
       Canvas.FillRect(Rect(ClientRect.Left+1,ClientRect.Top + textHeightHalf+2,ClientRect.Right-1,ClientRect.Bottom-2));
       Canvas.MoveTo(ClientRect.Left+2,ClientRect.Top + textHeightHalf+1);
       Canvas.LineTo(ClientRect.Right-2,ClientRect.Top + textHeightHalf+1);
       Canvas.MoveTo(ClientRect.Left+2,ClientRect.Bottom-2);
       Canvas.LineTo(ClientRect.Right-2,ClientRect.Bottom-2);
}
     {$ENDIF}
    brOnlyTopLine:
      {$IFDEF MFC_COMPILER_4_UP}
      if BidiMode = bdRightToLeft then
       begin
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Right - 5, ClientRect.Top + textHeightHalf), Point(ClientRect.Right, ClientRect.Top + textHeightHalf)]);
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Left  + 1, ClientRect.Top + textHeightHalf), Point(ClientRect.Right - 12 - textSize.cx, ClientRect.Top + textHeightHalf)]);
       end
      else
       begin
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Left  + 5, ClientRect.Top + textHeightHalf), Point(ClientRect.Left, ClientRect.Top + textHeightHalf)]);
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Right - 1, ClientRect.Top + textHeightHalf), Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf)]);
       end;
      {$ELSE}
       begin
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Left + 5, ClientRect.Top + textHeightHalf), Point(ClientRect.Left, ClientRect.Top + textHeightHalf)]);
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Right-1, ClientRect.Top + textHeightHalf), Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf)]);
       end;
      {$ENDIF}
    brASCII:
      {$IFDEF MFC_COMPILER_4_UP}
      if BidiMode = bdRightToLeft then
       begin
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Right - 15 - textSize.cx, ClientRect.Top + textHeightHalf - 1),
          Point(ClientRect.Left, ClientRect.Top + textHeightHalf - 1),
          Point(ClientRect.Left, ClientRect.Bottom - 1),
          Point(ClientRect.Right - 1, ClientRect.Bottom - 1),
          Point(ClientRect.Right - 1, ClientRect.Top + textHeightHalf - 1),
          Point(ClientRect.Right - 9 , ClientRect.Top + textHeightHalf - 1)]);
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Right - 15 - textSize.cx, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Left + 2, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Left + 2, ClientRect.Bottom - 3),
          Point(ClientRect.Right - 3, ClientRect.Bottom - 3),
          Point(ClientRect.Right - 3, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Right - 9 , ClientRect.Top + textHeightHalf + 1)]);
        memoryBitmap.Canvas.MoveTo(ClientRect.Left + 3, ClientRect.Top + textHeightHalf + 1);
        memoryBitmap.Canvas.LineTo(ClientRect.Left + 3, ClientRect.Bottom - 2);
        memoryBitmap.Canvas.MoveTo(ClientRect.Left + 1, ClientRect.Top + textHeightHalf);
        memoryBitmap.Canvas.LineTo(ClientRect.Left + 1, ClientRect.Bottom - 1);
        memoryBitmap.Canvas.MoveTo(ClientRect.Right - 4, ClientRect.Top + textHeightHalf + 1);
        memoryBitmap.Canvas.LineTo(ClientRect.Right - 4, ClientRect.Bottom - 2);
        memoryBitmap.Canvas.MoveTo(ClientRect.Right - 2, ClientRect.Top + textHeightHalf);
        memoryBitmap.Canvas.LineTo(ClientRect.Right - 2, ClientRect.Bottom - 1);
       end
      else
       begin
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Left + 5, ClientRect.Top + textHeightHalf - 1),
          Point(ClientRect.Left, ClientRect.Top + textHeightHalf - 1),
          Point(ClientRect.Left, ClientRect.Bottom - 1),
          Point(ClientRect.Right - 1, ClientRect.Bottom - 1),
          Point(ClientRect.Right - 1, ClientRect.Top + textHeightHalf - 1),
          Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf - 1)]);
        memoryBitmap.Canvas.Polyline([Point(ClientRect.Left + 5, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Left + 4, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Left + 4, ClientRect.Bottom - 3),
          Point(ClientRect.Right - 5, ClientRect.Bottom - 3),
          Point(ClientRect.Right - 5, ClientRect.Top + textHeightHalf + 1),
          Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf + 1)]);
        memoryBitmap.Canvas.MoveTo(ClientRect.Left + 3, ClientRect.Top + textHeightHalf + 1);
        memoryBitmap.Canvas.LineTo(ClientRect.Left + 3, ClientRect.Bottom - 2);
        memoryBitmap.Canvas.MoveTo(ClientRect.Left + 1, ClientRect.Top + textHeightHalf);
        memoryBitmap.Canvas.LineTo(ClientRect.Left + 1, ClientRect.Bottom - 1);
        memoryBitmap.Canvas.MoveTo(ClientRect.Right - 4, ClientRect.Top + textHeightHalf + 1);
        memoryBitmap.Canvas.LineTo(ClientRect.Right - 4, ClientRect.Bottom - 2);
        memoryBitmap.Canvas.MoveTo(ClientRect.Right - 2, ClientRect.Top + textHeightHalf);
        memoryBitmap.Canvas.LineTo(ClientRect.Right - 2, ClientRect.Bottom - 1);
       end;
      {$ELSE}
      memoryBitmap.Canvas.Polyline([Point(ClientRect.Left + 5, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left, ClientRect.Bottom-1), Point(ClientRect.Right-1, ClientRect.Bottom-1),
        Point(ClientRect.Right-1, ClientRect.Top + textHeightHalf),
        Point(ClientRect.Left + 12 + textSize.cx, ClientRect.Top + textHeightHalf)]);
      {$ENDIF}
    brMFStyle:
      begin
       with memoryBitmap do
        begin
         Canvas.Polyline([Point((ClientRect.Left + ClientRect.Right - textSize.cx) div 2, ClientRect.Top + textHeightHalf - 1),
           Point(ClientRect.Left, ClientRect.Top + textHeightHalf - 1),
           Point(ClientRect.Left, ClientRect.Bottom - 1),
           Point(ClientRect.Right - 1, ClientRect.Bottom - 1),
           Point(ClientRect.Right - 1, ClientRect.Top + textHeightHalf - 1),
           Point((ClientRect.Left + ClientRect.Right + textSize.cx) div 2 - 1, ClientRect.Top + textHeightHalf - 1)]);
         Canvas.Polyline([Point((ClientRect.Left + ClientRect.Right - textSize.cx) div 2, ClientRect.Top + textHeightHalf + 1),
           Point(ClientRect.Left + 4, ClientRect.Top + textHeightHalf + 1),
           Point(ClientRect.Left + 4, ClientRect.Bottom - 3),
           Point(ClientRect.Right - 5, ClientRect.Bottom - 3),
           Point(ClientRect.Right - 5, ClientRect.Top + textHeightHalf + 1),
           Point((ClientRect.Left + ClientRect.Right + textSize.cx) div 2 - 1, ClientRect.Top + textHeightHalf + 1)]);
         Canvas.MoveTo(ClientRect.Left + 3, ClientRect.Top + textHeightHalf + 1);
         Canvas.LineTo(ClientRect.Left + 3, ClientRect.Bottom - 2);
         Canvas.MoveTo(ClientRect.Left + 1, ClientRect.Top + textHeightHalf);
         Canvas.LineTo(ClientRect.Left + 1, ClientRect.Bottom - 1);
         Canvas.MoveTo(ClientRect.Right - 4, ClientRect.Top + textHeightHalf + 1);
         Canvas.LineTo(ClientRect.Right - 4, ClientRect.Bottom - 2);
         Canvas.MoveTo(ClientRect.Right - 2, ClientRect.Top + textHeightHalf);
         Canvas.LineTo(ClientRect.Right - 2, ClientRect.Bottom - 1);

         // Draw Text
         Canvas.Brush.Style := bsSolid;
         Canvas.Font.Color  := Canvas.Brush.Color;
         Canvas.Brush.Color := FBorderColor;
         DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format);
         // Copy memoryBitmap to screen
        end;
       canvas.CopyRect(ClientRect, memoryBitmap.Canvas, ClientRect);
       Exit;
      end;
     brNone : Exit;
  end;

    // Draw Text
  memoryBitmap.Canvas.Brush.Style := bsClear;
  if not Enabled then
   begin
    OffsetRect(textBounds, 1, 1);
    memoryBitmap.Canvas.Font.Color := fDisabledHighlightColor;
    DrawText(memoryBitmap.Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format);
    OffsetRect(textBounds, -1, -1);
    memoryBitmap.Canvas.Font.Color := fDisabledShadowColor;
    DrawText(memoryBitmap.Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format);
   end else
  if fMFStyle
   then DrawTextMF(memoryBitmap.Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format)
   else DrawText(memoryBitmap.Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format);

  // Copy memoryBitmap to screen
  canvas.CopyRect(ClientRect, memoryBitmap.canvas, ClientRect);
 finally
  memoryBitmap.free; // delete the bitmap
 end;
end;
*)

end.
