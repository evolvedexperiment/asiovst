unit DAV_GuiSelectBox;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Controls, Graphics, Menus, DAV_GuiBaseControl;

type
  TCustomGuiSelectBox = class(TCustomGuiBaseControl)
  private
    FAlignment        : TAlignment;
    FAntiAlias        : TGuiAntiAlias;
    FArrowColor       : TColor;
    FArrowWidth       : Integer;
    FArrowButtonWidth : Integer;
    FButtonColor      : TColor;
    FItemIndex        : Integer;
    FItems            : TStringList;
    FOnChange         : TNotifyEvent;
    FOSFactor         : Integer;
    FRoundRadius      : Integer;
    FPopupMenu        : TPopupMenu;
    FSelectBoxColor   : TColor;
    procedure ButtonWidthChanged;
    procedure RenderSelectBoxToBitmap(const Bitmap: TBitmap);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetArrowColor(const Value: TColor);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(const Value: TStringList);
    procedure SetRoundRadius(Value: Integer);
    procedure SetSelectBoxColor(const Value: TColor);
    procedure MenuItemClick(Sender: TObject);
  protected
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBtnHighlight;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 2;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnShadow;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Items: TStringList read FItems write SetItems;
    property LineColor default clBtnHighlight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Radius: Integer read FRoundRadius write SetRoundRadius default 2;
    property SelectBoxColor: TColor read FSelectBoxColor write SetSelectBoxColor default clBtnShadow;
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
    property Radius;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property Transparent;
    property OnCanResize;
    {$ENDIF}
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
 FItems.Clear;
 FItemIndex := -1;
end;

constructor TCustomGuiSelectBox.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle      := ControlStyle + [csFramed, csOpaque, csReplicatable,
                                      csAcceptsControls];
 FRoundRadius      := 2;
 FLineColor        := clBtnHighlight;
 FSelectBoxColor   := clBtnHighlight;
 FArrowColor       := clBtnHighlight;
 FSelectBoxColor   := clBtnShadow;
 FButtonColor      := clBtnShadow;
 FArrowWidth       := 2;
 FItemIndex        := -1;
 FOSFactor         := 1;
 FAlignment        := taCenter;
 FItems            := TStringList.Create;
 ButtonWidthChanged;
end;

destructor TCustomGuiSelectBox.Destroy;
begin
 FreeAndNil(FItems);
 if assigned(FPopupMenu)
  then FreeAndNil(FPopupMenu);
 inherited;
end;

procedure TCustomGuiSelectBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i  : Integer;
  MI : TMenuItem;
begin
 if Button = mbLeft then
  case FAlignment of
   taLeftJustify :
    begin
     if (x > Width - FArrowButtonWidth) then
      begin
       if FItemIndex < FItems.Count - 1 then ItemIndex := ItemIndex + 1 else
       if FItemIndex >= FItems.Count - 1 then ItemIndex := 0;
      end else
     if (x > Width - 2 * FArrowButtonWidth) then
      begin
       if FItemIndex > 0 then ItemIndex := ItemIndex - 1 else
       if FItemIndex = 0 then ItemIndex := FItems.Count - 1;
      end;
    end;
   taCenter :
    begin
     if (x < FArrowButtonWidth) then
      begin
       if FItemIndex > 0 then ItemIndex := ItemIndex - 1 else
       if FItemIndex = 0 then ItemIndex := FItems.Count - 1;
      end else
     if (x > Width - FArrowButtonWidth) then
      begin
       if FItemIndex < FItems.Count - 1 then ItemIndex := ItemIndex + 1 else
       if FItemIndex >= FItems.Count - 1 then ItemIndex := 0;
      end;
    end;
   taRightJustify :
    begin
     if (x < FArrowButtonWidth) then
      begin
       if FItemIndex > 0 then ItemIndex := ItemIndex - 1 else
       if FItemIndex = 0 then ItemIndex := FItems.Count - 1;
      end else
     if (x < 2 * FArrowButtonWidth) then
      begin
       if FItemIndex < FItems.Count - 1 then ItemIndex := ItemIndex + 1 else
       if FItemIndex >= FItems.Count - 1 then ItemIndex := 0;
      end;
    end;
  end else
 if Button = mbRight then
  begin
   if assigned(FPopupMenu)
    then FPopupMenu.Items.Clear
    else FPopupMenu := TPopupMenu.Create(Self);
   for i := 0 to FItems.Count - 1 do
    begin
     MI := TMenuItem.Create(FPopupMenu);
     MI.Caption   := FItems[i];
     MI.RadioItem := True;
     MI.Checked   := i = ItemIndex;
     MI.OnClick   := MenuItemClick;
     MI.Tag       := i;
     FPopupMenu.Items.Add(MI);
    end;
   FPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
 inherited;
end;

procedure TCustomGuiSelectBox.MenuItemClick(Sender: TObject);
begin
 assert(Sender is TMenuItem);
 with TMenuItem(Sender) do
  begin
   ItemIndex := Tag;
  end;
end;

procedure TCustomGuiSelectBox.RenderSelectBoxToBitmap(const Bitmap: TBitmap);
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
   Font.Size   := FOSFactor * Font.Size;
   Brush.Style := bsClear;
   Brush.Color := FSelectBoxColor;
   Pen.Width   := FOSFactor * fLineWidth;
   Pen.Color   := FLineColor;

   case FRoundRadius of
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
        if FLineColor <> FSelectBoxColor
         then PolyLine(PtsArray);
       end;
     end;
   end;

   case FAlignment of
    taLeftJustify :
     begin
      rad := FArrowButtonWidth * FOSFactor;

      Brush.Color := FArrowColor;
      with ArrowPos do
       begin
        Pen.Width := FOSFactor * FArrowWidth;
        y := Bitmap.Height div 2;
        x := Bitmap.Width - rad;
        Polygon([Point(x, y - FOSFactor * 4),
                 Point(x, y + FOSFactor * 4),
                 Point(x + 8 * FOSFactor, y)]);

        x := x - rad;
        Polygon([Point(x + 8 * FOSFactor, y - FOSFactor * 4),
                 Point(x + 8 * FOSFactor, y + FOSFactor * 4),
                 Point(x, y)]);
       end;

      if FItemIndex >= 0 then
       begin
        Brush.Style := bsClear;
        TextSize := TextExtent(FItems[FItemIndex]);
        TextOut(rad, (Bitmap.Height - TextSize.cy) div 2, FItems[FItemIndex]);
       end;

     end;
    taCenter :
     begin
      rad := FArrowButtonWidth * FOSFactor;
      MoveTo(rad, 0);
      LineTo(rad, Bitmap.Height);
      MoveTo(Bitmap.Width - 1 - rad, 0);
      LineTo(Bitmap.Width - 1 - rad, Bitmap.Height);

      Brush.Color := FArrowColor;
      with ArrowPos do
       begin
        x := rad div 2;
        y := Bitmap.Height div 2;
        Pen.Width := FOSFactor * FArrowWidth;
        Polygon([Point(x + 2 * FOSFactor, y - FOSFactor * 4),
                 Point(x + 2 * FOSFactor, y + FOSFactor * 4),
                 Point(x - 2 * FOSFactor, y)]);

        x := Bitmap.Width - 1 - rad div 2;
        Polygon([Point(x - 2 * FOSFactor, y - FOSFactor * 4),
                 Point(x - 2 * FOSFactor, y + FOSFactor * 4),
                 Point(x + 2 * FOSFactor, y)]);
       end;

      if FItemIndex >= 0 then
       begin
        Brush.Style := bsClear;
        TextSize := TextExtent(FItems[FItemIndex]);
        TextOut((Bitmap.Width - TextSize.cx) div 2,
                (Bitmap.Height - TextSize.cy) div 2, FItems[FItemIndex]);
       end;
     end;
    taRightJustify :
     begin
      rad := FArrowButtonWidth * FOSFactor;

      Brush.Color := FArrowColor;
      with ArrowPos do
       begin
        Pen.Width := FOSFactor * FArrowWidth;
        y := Bitmap.Height div 2;
        x := rad;
        Polygon([Point(x, y - FOSFactor * 4),
                 Point(x, y + FOSFactor * 4),
                 Point(x - 8 * FOSFactor, y)]);

        x := x + rad;
        Polygon([Point(x - 8 * FOSFactor, y - FOSFactor * 4),
                 Point(x - 8 * FOSFactor, y + FOSFactor * 4),
                 Point(x, y)]);
       end;

      if FItemIndex >= 0 then
       begin
        Brush.Style := bsClear;
        TextSize := TextExtent(FItems[FItemIndex]);
        TextOut(Bitmap.Width - rad - TextSize.cx, (Bitmap.Height - TextSize.cy) div 2, FItems[FItemIndex]);
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
   case FAntiAlias of
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
        Width  := FOSFactor * fBuffer.Width;
        Height := FOSFactor * fBuffer.Height;
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
        Width  := FOSFactor * fBuffer.Width;
        Height := FOSFactor * fBuffer.Height;
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
        Width  := FOSFactor * fBuffer.Width;
        Height := FOSFactor * fBuffer.Height;
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
        Width  := FOSFactor * fBuffer.Width;
        Height := FOSFactor * fBuffer.Height;
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
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   ButtonWidthChanged;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.ButtonWidthChanged;
begin
 case FAlignment of
   taLeftJustify : FArrowButtonWidth := 12 + (fLineWidth div 2);
        taCenter : FArrowButtonWidth := Max((FRoundRadius + 2), abs(Font.Height)) + fLineWidth div 2;
  taRightJustify : FArrowButtonWidth := 12 + (fLineWidth div 2);
 end;
end;

procedure TCustomGuiSelectBox.SetAntiAlias(const Value: TGuiAntiAlias);
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
   ButtonWidthChanged;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetArrowColor(const Value: TColor);
begin
 if FArrowColor <> Value then
  begin
   FArrowColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetArrowWidth(const Value: Integer);
begin
 if FArrowWidth <> Value then
  begin
   FArrowWidth := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetItemIndex(Value: Integer);
begin
 if Value < -1 then Value := -1 else
 if Value >= FItems.Count then Value := FItems.Count - 1;
 if FItemIndex <> Value then
  begin
   FItemIndex := Value;
   if assigned(FOnChange)
    then FOnChange(Self);
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetItems(const Value: TStringList);
begin
 if Assigned(FItems)
  then FItems.Assign(Value)
  else FItems := Value;
 FItemIndex := - 1;
 RedrawBuffer(True);
end;

procedure TCustomGuiSelectBox.SetSelectBoxColor(const Value: TColor);
begin
 if FSelectBoxColor <> Value then
  begin
   FSelectBoxColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiSelectBox.SetRoundRadius(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   ButtonWidthChanged;
   RedrawBuffer(True);
  end;
end;

end.
