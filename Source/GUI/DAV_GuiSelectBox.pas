unit DAV_GuiSelectBox;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Controls, DAV_GuiBaseControl, Graphics;

type
  TCustomGuiSelectBox = class(TCustomGuiBaseControl)
  private
    fAlignment        : TAlignment;
    fAntiAlias        : TGuiAntiAlias;
    fArrowColor       : TColor;
    fArrowWidth       : Integer;
    fArrowButtonWidth : Integer;
    fButtonColor      : TColor;
    fItemIndex        : Integer;
    fItems            : TStringList;
    fOnChange         : TNotifyEvent;
    fOSFactor         : Integer;
    fRoundRadius      : Integer;
    fSelectBoxColor   : TColor;
    procedure RenderSelectBoxToBitmap(Bitmap: TBitmap);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetArrowColor(const Value: TColor);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(const Value: TStringList);
    procedure SetRoundRadius(Value: Integer);
    procedure SetSelectBoxColor(const Value: TColor);
    procedure ButtonWidthChanged;
    procedure SetArrowWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
  protected
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Alignment: TAlignment read fAlignment write SetAlignment default taCenter;
    property AntiAlias: TGuiAntiAlias read fAntiAlias write SetAntiAlias default gaaNone;
    property ArrowColor: TColor read fArrowColor write SetArrowColor default clBtnHighlight;
    property ArrowWidth: Integer read fArrowWidth write SetArrowWidth default 2;
    property ButtonColor: TColor read fButtonColor write SetButtonColor default clBtnShadow;
    property ItemIndex: Integer read fItemIndex write SetItemIndex;
    property Items: TStringList read fItems write SetItems;
    property LineColor default clBtnHighlight;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property Radius: Integer read fRoundRadius write SetRoundRadius default 2;
    property SelectBoxColor: TColor read fSelectBoxColor write SetSelectBoxColor default clBtnShadow;
  end;

  TGuiSelectBox = class(TCustomGuiSelectBox)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AntiAlias;
    property ArrowColor;
    property ArrowWidth;
    property ButtonColor;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;
    property Items;
    property LineColor;
    property LineWidth;
    property SelectBoxColor;
    property PopupMenu;
    property Radius;
    property ShowHint;
    property Transparent;
    property Visible;

    property OnCanResize;
    property OnChange;
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
  Math, SysUtils, DAV_Common, DAV_Complex;

{ TCustomGuiSelectBox }

procedure TCustomGuiSelectBox.Clear;
begin
 fItems.Clear;
 fItemIndex := -1;
end;

constructor TCustomGuiSelectBox.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle      := ControlStyle + [csFramed, csOpaque, csReplicatable,
                                      csAcceptsControls];
 fRoundRadius      := 2;
 fLineColor        := clBtnHighlight;
 fSelectBoxColor   := clBtnHighlight;
 fArrowColor       := clBtnHighlight;
 fSelectBoxColor   := clBtnShadow;
 fButtonColor      := clBtnShadow;
 fArrowWidth       := 2;
 fItemIndex        := -1;
 fOSFactor         := 1;
 fAlignment        := taCenter;
 fItems            := TStringList.Create;
 ButtonWidthChanged;
end;

destructor TCustomGuiSelectBox.Destroy;
begin
 FreeAndNil(fItems);
 inherited;
end;

procedure TCustomGuiSelectBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
  case fAlignment of
   taLeftJustify :
    begin
     if (x > Width - fArrowButtonWidth) then
      begin
       if fItemIndex < fItems.Count - 1 then ItemIndex := ItemIndex + 1 else
       if fItemIndex >= fItems.Count - 1 then ItemIndex := 0;
      end else
     if (x > Width - 2 * fArrowButtonWidth) then
      begin
       if fItemIndex > 0 then ItemIndex := ItemIndex - 1 else
       if fItemIndex = 0 then ItemIndex := fItems.Count - 1;
      end;
    end;
   taCenter :
    begin
     if (x < fArrowButtonWidth) then
      begin
       if fItemIndex > 0 then ItemIndex := ItemIndex - 1 else
       if fItemIndex = 0 then ItemIndex := fItems.Count - 1;
      end else
     if (x > Width - fArrowButtonWidth) then
      begin
       if fItemIndex < fItems.Count - 1 then ItemIndex := ItemIndex + 1 else
       if fItemIndex >= fItems.Count - 1 then ItemIndex := 0;
      end;
    end;
   taRightJustify :
    begin
     if (x < fArrowButtonWidth) then
      begin
       if fItemIndex > 0 then ItemIndex := ItemIndex - 1 else
       if fItemIndex = 0 then ItemIndex := fItems.Count - 1;
      end else
     if (x < 2 * fArrowButtonWidth) then
      begin
       if fItemIndex < fItems.Count - 1 then ItemIndex := ItemIndex + 1 else
       if fItemIndex >= fItems.Count - 1 then ItemIndex := 0;
      end;
    end;
  end;
 inherited;
end;

procedure TCustomGuiSelectBox.RenderSelectBoxToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexDouble;
  Steps, i : Integer;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  ArrowPos : TPoint;
  PtsArray : Array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;
   Font.Assign(Self.Font);
   Font.Size   := fOSFactor * Font.Size;
   Brush.Style := bsClear;
   Brush.Color := fSelectBoxColor;
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
        if fLineColor <> fSelectBoxColor
         then PolyLine(PtsArray);
       end;
     end;
   end;

   case fAlignment of
    taLeftJustify :
     begin
      rad := fArrowButtonWidth * fOSFactor;

      Brush.Color := fArrowColor;
      with ArrowPos do
       begin
        Pen.Width := fOSFactor * fArrowWidth;
        y := Bitmap.Height div 2;
        x := Bitmap.Width - rad;
        Polygon([Point(x, y - fOSFactor * 4),
                 Point(x, y + fOSFactor * 4),
                 Point(x + 8 * fOSFactor, y)]);

        x := x - rad;
        Polygon([Point(x + 8 * fOSFactor, y - fOSFactor * 4),
                 Point(x + 8 * fOSFactor, y + fOSFactor * 4),
                 Point(x, y)]);
       end;

      if fItemIndex >= 0 then
       begin
        Brush.Style := bsClear;
        TextSize := TextExtent(fItems[fItemIndex]);
        TextOut(rad, (Bitmap.Height - TextSize.cy) div 2, fItems[fItemIndex]);
       end;

     end;
    taCenter :
     begin
      rad := fArrowButtonWidth * fOSFactor;
      MoveTo(rad, 0);
      LineTo(rad, Bitmap.Height);
      MoveTo(Bitmap.Width - 1 - rad, 0);
      LineTo(Bitmap.Width - 1 - rad, Bitmap.Height);

      Brush.Color := fArrowColor;
      with ArrowPos do
       begin
        x := rad div 2;
        y := Bitmap.Height div 2;
        Pen.Width := fOSFactor * fArrowWidth;
        Polygon([Point(x + 2 * fOSFactor, y - fOSFactor * 4),
                 Point(x + 2 * fOSFactor, y + fOSFactor * 4),
                 Point(x - 2 * fOSFactor, y)]);

        x := Bitmap.Width - 1 - rad div 2;
        Polygon([Point(x - 2 * fOSFactor, y - fOSFactor * 4),
                 Point(x - 2 * fOSFactor, y + fOSFactor * 4),
                 Point(x + 2 * fOSFactor, y)]);
       end;

      if fItemIndex >= 0 then
       begin
        Brush.Style := bsClear;
        TextSize := TextExtent(fItems[fItemIndex]);
        TextOut((Bitmap.Width - TextSize.cx) div 2,
                (Bitmap.Height - TextSize.cy) div 2, fItems[fItemIndex]);
       end;
     end;
    taRightJustify :
     begin
      rad := fArrowButtonWidth * fOSFactor;

      Brush.Color := fArrowColor;
      with ArrowPos do
       begin
        Pen.Width := fOSFactor * fArrowWidth;
        y := Bitmap.Height div 2;
        x := rad;
        Polygon([Point(x, y - fOSFactor * 4),
                 Point(x, y + fOSFactor * 4),
                 Point(x - 8 * fOSFactor, y)]);

        x := x + rad;
        Polygon([Point(x - 8 * fOSFactor, y - fOSFactor * 4),
                 Point(x - 8 * fOSFactor, y + fOSFactor * 4),
                 Point(x, y)]);
       end;

      if fItemIndex >= 0 then
       begin
        Brush.Style := bsClear;
        TextSize := TextExtent(fItems[fItemIndex]);
        TextOut(Bitmap.Width - rad - TextSize.cx, (Bitmap.Height - TextSize.cy) div 2, fItems[fItemIndex]);
       end;

     end;
   end;
   Unlock;
  end;
end;

procedure TCustomGuiSelectBox.RedrawBuffer(doBufferFlip: Boolean);
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
      RenderSelectBoxToBitmap(fBuffer);
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
        RenderSelectBoxToBitmap(Bmp);
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
//          Upsample2xBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderSelectBoxToBitmap(Bmp);
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
        Canvas.FillRect(Canvas.ClipRect);
        RenderSelectBoxToBitmap(Bmp);
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
        Canvas.FillRect(Canvas.ClipRect);
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample4xBitmap(Bmp);
          Upsample4xBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderSelectBoxToBitmap(Bmp);
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

procedure TCustomGuiSelectBox.SetAlignment(const Value: TAlignment);
begin
 if fAlignment <> Value then
  begin
   fAlignment := Value;
   ButtonWidthChanged;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.ButtonWidthChanged;
begin
 case fAlignment of
   taLeftJustify : fArrowButtonWidth := 12 + (fLineWidth div 2);
        taCenter : fArrowButtonWidth := Max((fRoundRadius + 2), abs(Font.Height)) + fLineWidth div 2;
  taRightJustify : fArrowButtonWidth := 12 + (fLineWidth div 2);
 end;
end;

procedure TCustomGuiSelectBox.SetAntiAlias(const Value: TGuiAntiAlias);
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
   ButtonWidthChanged;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetArrowColor(const Value: TColor);
begin
 if fArrowColor <> Value then
  begin
   fArrowColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetArrowWidth(const Value: Integer);
begin
 if fArrowWidth <> Value then
  begin
   fArrowWidth := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetButtonColor(const Value: TColor);
begin
 if fButtonColor <> Value then
  begin
   fButtonColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetItemIndex(Value: Integer);
begin
 if Value < -1 then Value := -1 else
 if Value >= fItems.Count then Value := fItems.Count - 1;
 if fItemIndex <> Value then
  begin
   fItemIndex := Value;
   if assigned(fOnChange)
    then fOnChange(Self);
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetItems(const Value: TStringList);
begin
 if Assigned(FItems)
  then FItems.Assign(Value)
  else FItems := Value;
 fItemIndex := - 1;
 RedrawBuffer(True);
end;

procedure TCustomGuiSelectBox.SetSelectBoxColor(const Value: TColor);
begin
 if fSelectBoxColor <> Value then
  begin
   fSelectBoxColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetRoundRadius(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if fRoundRadius <> Value then
  begin
   fRoundRadius := Value;
   ButtonWidthChanged;
   RedrawBuffer(True);
  end;
end;

end.
