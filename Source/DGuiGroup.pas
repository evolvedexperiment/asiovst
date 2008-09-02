unit DGuiGroup;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Controls, DGuiBaseControl, Graphics;

type
  TCustomGuiGroup = class(TCustomGuiBaseControl)
  private
    fCaption        : string;
    fRoundRadius    : Integer;
    fHeaderMinWidth : Integer;
    fAntiAlias      : TGuiAntiAlias;
    fOSFactor       : Integer;
    procedure RenderGroupToBitmap(Bitmap: TBitmap);
    procedure SetCaption(const Value: string);
    procedure SetRoundRadius(Value: Integer);
    procedure SetHeaderMinWidth(const Value: Integer);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
  protected
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); override;
    property AntiAlias: TGuiAntiAlias read fAntiAlias write SetAntiAlias default gaaNone;
    property Caption: string read fCaption write SetCaption;
    property Radius: Integer read fRoundRadius write SetRoundRadius default 2;
    property HeaderMinWidth: Integer read fHeaderMinWidth write SetHeaderMinWidth default 32;
    property LineColor default clBtnShadow;
  end;

  TGuiGroup = class(TCustomGuiGroup)
  published
    property Align;
    property Anchors;
    property AntiAlias;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HeaderMinWidth;
    property LineColor;
    property LineWidth;
    property PopupMenu;
    property Radius;
    property ShowHint;
    property Transparent;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
  end;

implementation

uses
  Math, DAVDCommon, DAVDComplex;

{ TCustomGuiGroup }

constructor TCustomGuiGroup.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle    := ControlStyle + [csFramed, csOpaque, csReplicatable,
                                    csAcceptsControls];
 fRoundRadius    := 2;
 fHeaderMinWidth := 32;
 fCaption        := 'Group'; //Name;
 fLineColor      := clBtnShadow;
end;

procedure TCustomGuiGroup.RenderGroupToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexDouble;
  Steps, i : Integer;
  rct      : TRect;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : Array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;
   Brush.Style := bsClear;
   Brush.Color := fLineColor;
   Pen.Width   := fOSFactor * fLineWidth;
   Pen.Color   := fLineColor;
   Font.Assign(Self.Font);
   Font.Size := fOSFactor * Font.Size;
   TextSize := TextExtent(fCaption);

   case fRoundRadius of
    0, 1 : begin
            FrameRect(ClipRect);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
           end;
       2 : begin
            with ClipRect do
             PolyLine([Point(Left  + 1, Bottom - 2), Point(Left     , Bottom - 3),
                       Point(Left     , Top    + 2), Point(Left  + 2, Top       ),
                       Point(Right - 3, Top       ), Point(Right - 1, Top    + 2),
                       Point(Right - 2, Top    + 1), Point(Right - 1, Top    + 2),
                       Point(Right - 1, Bottom - 2), Point(Right - 3, Bottom - 1),
                       Point(Left  + 2, Bottom - 1), Point(Left,      Bottom - 3)]);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
            // MoveTo(TextSize.cx + 12, 1);
            // LineTo(TextSize.cx + 12, TextSize.cy + 3);
           end;
    else
     begin
      rad := fOSFactor * fRoundRadius;
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

        PolyLine(PtsArray);

        // Draw inner text
        //////////////////

        SetLength(PtsArray, Steps div 2 + 5);
        Val.Re := -rad; Val.Im := 0;

        GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
        rct := Rect(Linewidth div 2, Linewidth div 2, max(TextSize.cx + 10, fOSFactor * fHeaderMinWidth) - (Linewidth + 1) div 2, TextSize.cy + 5 - (Linewidth + 1) div 2);
        PtsArray[0] := Point(Round(rct.Left), Round(rct.Top + rad));

        // upper left corner
        for i := 1 to Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(rct.Left + rad + Val.Re), Round(rct.Top + rad + Val.Im));
         end;
        PtsArray[Steps div 4] := Point(rct.Left + rad, rct.Top);
        PtsArray[Steps div 4  + 1 ] := Point(rct.Right, rct.Top);
        PtsArray[Steps div 4  + 2 ] := Point(rct.Right, rct.Bottom - rad);

        Val.Re := rad; Val.Im := 0;

        // lower right corner
        for i := Steps div 4 to Steps div 2 - 1 do
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

   TextOut(6, 2, fCaption);
   Unlock;
  end;
end;

procedure TCustomGuiGroup.RedrawBuffer(doBufferFlip: Boolean);
var
  Bmp : TBitmap;
begin
 if (Width > 0) and (Height > 0) then with fBuffer.Canvas do
  begin
   Lock;
   Brush.Style := bsSolid;
   Brush.Color := Self.Color;
   case fAntiAlias of
    gaaNone     :
     begin
      {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else {$ENDIF}
      FillRect(ClipRect);
      RenderGroupToBitmap(fBuffer);
     end;
    gaaLinear2x :
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width  := fOSFactor * fBuffer.Width;
        Height := fOSFactor * fBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        Canvas.FillRect(Canvas.ClipRect);
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample2xBitmap(Bmp);
         end else
        {$ENDIF}
        FillRect(ClipRect);
        RenderGroupToBitmap(Bmp);
        Downsample2xBitmap(Bmp);
        Draw(0, 0, Bmp);
       finally
        Free;
       end;
     end;
    gaaLinear4x :
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width  := fOSFactor * fBuffer.Width;
        Height := fOSFactor * fBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        Canvas.FillRect(Canvas.ClipRect);
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample4xBitmap(Bmp);
         end else
        {$ENDIF}
        RenderGroupToBitmap(Bmp);
        Downsample4xBitmap(Bmp);
        Draw(0, 0, Bmp);
       finally
        Free;
       end;
     end;
    gaaLinear8x :
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width  := fOSFactor * fBuffer.Width;
        Height := fOSFactor * fBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        Canvas.FillRect(Canvas.ClipRect);
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample2xBitmap(Bmp);
          Upsample4xBitmap(Bmp);
         end else
        {$ENDIF}
        RenderGroupToBitmap(Bmp);
        Downsample2xBitmap(Bmp);
        Downsample4xBitmap(Bmp);
        Draw(0, 0, Bmp);
       finally
        Free;
       end;
     end;
    gaaLinear16x :
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width  := fOSFactor * fBuffer.Width;
        Height := fOSFactor * fBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        Canvas.FillRect(Canvas.ClipRect);
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample4xBitmap(Bmp);
          Upsample4xBitmap(Bmp);
         end else
        {$ENDIF}
        RenderGroupToBitmap(Bmp);
        Downsample4xBitmap(Bmp);
        Downsample4xBitmap(Bmp);
        Draw(0, 0, Bmp);
       finally
        Free;
       end;
     end;
   end;
  end;

 if doBufferFlip then Invalidate;
end;

procedure TCustomGuiGroup.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if fAntiAlias <> Value then
  begin
   fAntiAlias := Value;
   case fAntiAlias of
         gaaNone : fOSFactor :=  1;
     gaaLinear2x : fOSFactor :=  2;
     gaaLinear4x : fOSFactor :=  4;
     gaaLinear8x : fOSFactor :=  8;
    gaaLinear16x : fOSFactor := 16;
   end;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiGroup.SetCaption(const Value: string);
begin
 if fCaption <> Value then
  begin
   fCaption := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiGroup.SetHeaderMinWidth(const Value: Integer);
begin
 if fHeaderMinWidth <> Value then
  begin
   fHeaderMinWidth := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiGroup.SetRoundRadius(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if fRoundRadius <> Value then
  begin
   fRoundRadius := Value;
   RedrawBuffer(True);
  end;
end;

end.
