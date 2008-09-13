unit DAVGuiButton;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Controls, DAVGuiBaseControl, Graphics;

type
  TCustomGuiButton = class(TCustomGuiBaseControl)
  private
    fRoundRadius  : Integer;
    fAlignment    : TAlignment;
    fAntiAlias    : TGuiAntiAlias;
    fCaption      : string;
    fOSFactor     : Integer;
    fButtonColor  : TColor;
    procedure SetRoundRadius(Value: Integer);
    procedure RenderButtonToBitmap(Bitmap: TBitmap);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetButtonColor(const Value: TColor);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
  protected
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); override;
    property AntiAlias: TGuiAntiAlias read fAntiAlias write SetAntiAlias default gaaNone;
    property Alignment: TAlignment read fAlignment write SetAlignment;
    property Caption: string read fCaption write SetCaption;
    property ButtonColor: TColor read fButtonColor write SetButtonColor default clBtnShadow;
    property Radius: Integer read fRoundRadius write SetRoundRadius default 2;
    property LineColor default clBtnHighlight;
  end;

  TGuiButton = class(TCustomGuiButton)
  published
    property Align;
    property Anchors;
    property AntiAlias;
    property Alignment;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property LineColor;
    property LineWidth;
    property ButtonColor;
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
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math, DAV_Common, DAV_Complex;

{ TCustomGuiButton }

constructor TCustomGuiButton.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle  := ControlStyle + [csFramed, csOpaque, csReplicatable,
                                  csAcceptsControls];
 fAlignment    := taCenter;
 fCaption      := 'empty';
 fRoundRadius  := 2;
 fLineColor    := clBtnHighlight;
 fButtonColor  := clBtnShadow;
end;

procedure TCustomGuiButton.RenderButtonToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexDouble;
  Steps, i : Integer;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : Array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;
   Font.Assign(Self.Font);
   Font.Size := fOSFactor * Font.Size;

   Brush.Style := bsClear;
   Brush.Color := fButtonColor;
   Pen.Width   := fOSFactor * fLineWidth;
   Pen.Color   := fLineColor;
   
   case fRoundRadius of
    0, 1 : FillRect(ClipRect);
       2 : begin
            with ClipRect do
             Polygon([Point(Left  + 1, Bottom - 2), Point(Left     , Bottom - 3),
                      Point(Left     , Top    + 2), Point(Left  + 2, Top       ),
                      Point(Right - 3, Top       ), Point(Right - 1, Top    + 2),
                      Point(Right - 2, Top    + 1), Point(Right - 1, Top    + 2),
                      Point(Right - 1, Bottom - 2), Point(Right - 3, Bottom - 1),
                      Point(Left  + 2, Bottom - 1), Point(Left,      Bottom - 3)]);
           end;
    else
     begin
      rad := fOSFactor * fRoundRadius;
      Steps := Round(2 / arcsin(1 / fRoundRadius)) + 1;
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
        if fLineColor <> fButtonColor
         then PolyLine(PtsArray);
       end;
     end;
   end;

   TextSize := TextExtent(fCaption);
   case fAlignment of
     taLeftJustify : TextOut(0, (Bitmap.Height - TextSize.cy) div 2, fCaption);
    taRightJustify : TextOut(Bitmap.Width - TextSize.cx, (Bitmap.Height - TextSize.cy) div 2, fCaption);
          taCenter : TextOut((Bitmap.Width - TextSize.cx) div 2, (Bitmap.Height - TextSize.cy) div 2, fCaption);
   end;

   Unlock;
  end;
end;

procedure TCustomGuiButton.RedrawBuffer(doBufferFlip: Boolean);
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
      RenderButtonToBitmap(fBuffer);
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
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample2xBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        FillRect(ClipRect);
        RenderButtonToBitmap(Bmp);
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
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample4xBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderButtonToBitmap(Bmp);
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
          Upsample4xBitmap(Bmp);
          Upsample2xBitmap(Bmp);
         end else
        {$ENDIF}
        RenderButtonToBitmap(Bmp);
        Downsample4xBitmap(Bmp);
        Downsample2xBitmap(Bmp);
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
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample4xBitmap(Bmp);
          Upsample4xBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderButtonToBitmap(Bmp);
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

procedure TCustomGuiButton.SetAlignment(const Value: TAlignment);
begin
 if fAlignment <> Value then
  begin
   fAlignment := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiButton.SetAntiAlias(const Value: TGuiAntiAlias);
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

procedure TCustomGuiButton.SetButtonColor(const Value: TColor);
begin
 if fButtonColor <> Value then
  begin
   fButtonColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiButton.SetCaption(const Value: string);
begin
 if fCaption <> Value then
  begin
   fCaption := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiButton.SetRoundRadius(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if fRoundRadius <> Value then
  begin
   fRoundRadius := Value;
   RedrawBuffer(True);
  end;
end;

end.
