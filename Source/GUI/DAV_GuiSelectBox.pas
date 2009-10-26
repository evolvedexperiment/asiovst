unit DAV_GuiSelectBox;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Controls, Graphics, Menus, DAV_GuiBaseControl;

type
  TCustomGuiSelectBox = class(TCustomGuiBaseAntialiasedControl)
  private
    FAlignment        : TAlignment;
    FAlternate        : Boolean;
    FArrowColor       : TColor;
    FArrowWidth       : Integer;
    FArrowButtonWidth : Integer;
    FButtonColor      : TColor;
    FItemIndex        : Integer;
    FItems            : TStringList;
    FOnChange         : TNotifyEvent;
    FRoundRadius      : Integer;
    FPopupMenu        : TPopupMenu;
    FSelectBoxColor   : TColor;
    procedure RenderSelectBoxToBitmap(const Bitmap: TBitmap);
    procedure MenuItemClick(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAlternate(const Value: Boolean);
    procedure SetArrowColor(const Value: TColor);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(const Value: TStringList);
    procedure SetRoundRadius(Value: Integer);
    procedure SetSelectBoxColor(const Value: TColor);
  protected
    procedure AlignmentChanged; virtual;
    procedure AlternateChanged; virtual;
    procedure ArrowColorChanged; virtual;
    procedure ArrowWidthChanged; virtual;
    procedure ButtonColorChanged; virtual;
    procedure ButtonWidthChanged; virtual;
    procedure ItemIndexChanged; virtual;
    procedure RoundRadiusChanged; virtual;
    procedure SelectBoxColorChanged; virtual;
    procedure UpdateBuffer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBtnHighlight;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 1;
    property Alternate: Boolean read FAlternate write SetAlternate default False; 
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
    property Alternate;
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
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Radius;
    property SelectBoxColor;
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
  Math, SysUtils, DAV_Math, DAV_Complex;

{ TCustomGuiSelectBox }

procedure TCustomGuiSelectBox.Clear;
begin
 FItems.Clear;
 FItemIndex := -1;
end;

constructor TCustomGuiSelectBox.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle    := ControlStyle + [csFramed, csOpaque, csReplicatable,
                                    csAcceptsControls];
 FRoundRadius    := 2;
 FLineColor      := clBtnHighlight;
 FSelectBoxColor := clBtnHighlight;
 FArrowColor     := clBtnHighlight;
 FSelectBoxColor := clBtnShadow;
 FButtonColor    := clBtnShadow;
 FArrowWidth     := 1;
 FAlternate      := False;
 FItemIndex      := -1;
 FAlignment      := taCenter;
 FItems          := TStringList.Create;
 ButtonWidthChanged;
end;

destructor TCustomGuiSelectBox.Destroy;
begin
 FreeAndNil(FItems);
 if Assigned(FPopupMenu)
  then FreeAndNil(FPopupMenu);
 inherited;
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
  Offsets  : Array [0..1] of Integer;
begin
 with Bitmap.Canvas do
  begin
   Lock;
   Font.Assign(Self.Font);
   Font.Size   := OversamplingFactor * Font.Size;
   Brush.Style := bsClear;
   Brush.Color := FSelectBoxColor;
   Pen.Width   := OversamplingFactor * FLineWidth;
   Pen.Color   := FLineColor;
   Offsets[0]  := Pen.Width div 2;
   Offsets[1]  := (Pen.Width - 1) div 2;

   case FRoundRadius of
    0, 1 : with ClipRect
            do Rectangle(Left + Offsets[0], Top + Offsets[0],
                         Right - Offsets[1], Bottom - Offsets[1]);
    else if not Alternate
     then with ClipRect
           do RoundRect(Left + Offsets[0], Top + Offsets[0],
                        Right - Offsets[1], Bottom - Offsets[1],
                        FRoundRadius * OversamplingFactor, FRoundRadius * OversamplingFactor)

(*        with ClipRect do Polygon(
             [Point(Left  + Offsets[0]     + Pen.Width, Top    + Offsets[0]),
              Point(Right - Offsets[1] - 1 - Pen.Width, Top    + Offsets[0]),
              Point(Right - Offsets[1] - 1            , Top    + Offsets[0]     + Pen.Width),
              Point(Right - Offsets[1] - 1            , Bottom - Offsets[1] - 1 - Pen.Width),
              Point(Right - Offsets[1] - 1 - Pen.Width, Bottom - Offsets[1] - 1),
              Point(Left  + Offsets[0]     + Pen.Width, Bottom - Offsets[1] - 1),
              Point(Left  + Offsets[0]                , Bottom - Offsets[1] - 1 - Pen.Width),
              Point(Left  + Offsets[0]                , Top    + Offsets[1]     + Pen.Width)]);
*)
    else
     begin
      rad := OversamplingFactor * FRoundRadius;
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
      rad := FArrowButtonWidth * OversamplingFactor;

      Brush.Color := FArrowColor;
      with ArrowPos do
       begin
        Pen.Width := OversamplingFactor * FArrowWidth;
        y := Bitmap.Height div 2;
        x := Bitmap.Width - rad;
        Polygon([Point(x, y - OversamplingFactor * 4),
                 Point(x, y + OversamplingFactor * 4),
                 Point(x + 8 * OversamplingFactor, y)]);

        x := x - rad;
        Polygon([Point(x + 8 * OversamplingFactor, y - OversamplingFactor * 4),
                 Point(x + 8 * OversamplingFactor, y + OversamplingFactor * 4),
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
      rad := FArrowButtonWidth * OversamplingFactor;
      MoveTo(rad, 0);
      LineTo(rad, Bitmap.Height);
      MoveTo(Bitmap.Width - 1 - rad, 0);
      LineTo(Bitmap.Width - 1 - rad, Bitmap.Height);

      Brush.Color := FArrowColor;
      with ArrowPos do
       begin
        x := rad div 2;
        y := Bitmap.Height div 2;
        Pen.Width := OversamplingFactor * FArrowWidth;
        Polygon([Point(x + 2 * OversamplingFactor, y - OversamplingFactor * 4),
                 Point(x + 2 * OversamplingFactor, y + OversamplingFactor * 4),
                 Point(x - 2 * OversamplingFactor, y)]);

        x := Bitmap.Width - 1 - rad div 2;
        Polygon([Point(x - 2 * OversamplingFactor, y - OversamplingFactor * 4),
                 Point(x - 2 * OversamplingFactor, y + OversamplingFactor * 4),
                 Point(x + 2 * OversamplingFactor, y)]);
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
      rad := FArrowButtonWidth * OversamplingFactor;

      Brush.Color := FArrowColor;
      with ArrowPos do
       begin
        Pen.Width := OversamplingFactor * FArrowWidth;
        y := Bitmap.Height div 2;
        x := rad;
        Polygon([Point(x, y - OversamplingFactor * 4),
                 Point(x, y + OversamplingFactor * 4),
                 Point(x - 8 * OversamplingFactor, y)]);

        x := x + rad;
        Polygon([Point(x - 8 * OversamplingFactor, y - OversamplingFactor * 4),
                 Point(x - 8 * OversamplingFactor, y + OversamplingFactor * 4),
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

procedure TCustomGuiSelectBox.UpdateBuffer;
var
  Bmp : TBitmap;
begin
 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   Brush.Style := bsSolid;
   Brush.Color := Self.Color;
   if AntiAlias = gaaNone then
    begin
     {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else {$ENDIF}
     FillRect(ClipRect);
     RenderSelectBoxToBitmap(FBuffer);
    end
   else
    begin
     Bmp := TBitmap.Create;
     with Bmp do
      try
       PixelFormat := pf32bit;
       Width  := OversamplingFactor * FBuffer.Width;
       Height := OversamplingFactor * FBuffer.Height;
       Canvas.Brush.Style := bsSolid;
       Canvas.Brush.Color := Self.Color;
       {$IFNDEF FPC}
       if FTransparent then
        begin
         CopyParentImage(Self, Bmp.Canvas);
//         DrawParentImage(Bmp.Canvas);
         UpsampleBitmap(Bmp);
        end else
       {$ENDIF}
       Canvas.FillRect(Canvas.ClipRect);
       RenderSelectBoxToBitmap(Bmp);
       DownsampleBitmap(Bmp);
       Draw(0, 0, Bmp);
      finally
       FreeAndNil(Bmp);
      end;
    end;
  end;
end;

procedure TCustomGuiSelectBox.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiSelectBox.AlignmentChanged;
begin
 ButtonWidthChanged;
 Invalidate;
end;

procedure TCustomGuiSelectBox.SetAlternate(const Value: Boolean);
begin
 if Alternate <> Value then
  begin
   FAlternate := Value;
   AlternateChanged;
  end;
end;

procedure TCustomGuiSelectBox.AlternateChanged;
begin
 Invalidate;
end;

procedure TCustomGuiSelectBox.ButtonWidthChanged;
begin
 case FAlignment of
   taLeftJustify : FArrowButtonWidth := 12 + (FLineWidth div 2) + FArrowWidth;
        taCenter : FArrowButtonWidth := Max(Max(FArrowWidth, FRoundRadius div 2) + 4, abs(Font.Height)) + FLineWidth div 2;
  taRightJustify : FArrowButtonWidth := 12 + (FLineWidth div 2) + FArrowWidth;
 end;
 Inc(FArrowButtonWidth, FArrowWidth);
end;

procedure TCustomGuiSelectBox.SetArrowColor(const Value: TColor);
begin
 if FArrowColor <> Value then
  begin
   FArrowColor := Value;
   ArrowColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.ArrowColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiSelectBox.SetArrowWidth(const Value: Integer);
begin
 if FArrowWidth <> Value then
  begin
   FArrowWidth := Value;
   ArrowWidthChanged;
  end;
end;

procedure TCustomGuiSelectBox.ArrowWidthChanged;
begin
 ButtonWidthChanged;
 Invalidate;
end;

procedure TCustomGuiSelectBox.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   ButtonColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.ButtonColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiSelectBox.SetItemIndex(Value: Integer);
begin
 if Value < -1 then Value := -1 else
 if Value >= FItems.Count then Value := FItems.Count - 1;
 if FItemIndex <> Value then
  begin
   FItemIndex := Value;
   ItemIndexChanged;
  end;
end;

procedure TCustomGuiSelectBox.ItemIndexChanged;
begin
 if assigned(FOnChange)
  then FOnChange(Self);
 Invalidate;
end;

procedure TCustomGuiSelectBox.SetItems(const Value: TStringList);
begin
 if Assigned(FItems)
  then FItems.Assign(Value)
  else FItems := Value;
 FItemIndex := - 1;
 Invalidate;
end;

procedure TCustomGuiSelectBox.SetSelectBoxColor(const Value: TColor);
begin
 if FSelectBoxColor <> Value then
  begin
   FSelectBoxColor := Value;
   SelectBoxColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.SelectBoxColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiSelectBox.SetRoundRadius(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   RoundRadiusChanged;
  end;
end;

procedure TCustomGuiSelectBox.RoundRadiusChanged;
begin
 ButtonWidthChanged;
 Invalidate;
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
     inherited;
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
     inherited;
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
     inherited;
    end;
  end else
 if Button = mbRight then
  begin
   if Assigned(FPopupMenu)
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
   inherited;
   FPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end else inherited;
end;

procedure TCustomGuiSelectBox.MenuItemClick(Sender: TObject);
begin
 assert(Sender is TMenuItem);
 with TMenuItem(Sender) do
  begin
   ItemIndex := Tag;
  end;
end;

end.
