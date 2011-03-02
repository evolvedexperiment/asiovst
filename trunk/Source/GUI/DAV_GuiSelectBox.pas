unit DAV_GuiSelectBox;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Controls, Graphics, Menus, DAV_GuiPixelMap,
  DAV_GuiCustomControl, DAV_GuiFont, DAV_GuiShadow;

type
  TCustomGuiSelectBox = class(TGuiCustomControl)
  private
    FAlignment        : TAlignment;
    FAlternate        : Boolean;
    FArrowColor       : TColor;
    FArrowWidth       : Integer;
    FArrowButtonWidth : Integer;
    FButtonColor      : TColor;
    FBorderColor      : TColor;
    FBorderWidth      : Single;
    FBorderRadius     : Single;
    FItemIndex        : Integer;
    FItems            : TStrings;
    FGuiFont          : TGuiOversampledGDIFont;
    FOnChange         : TNotifyEvent;
    FPopupMenu        : TPopupMenu;
    FSelectBoxColor   : TColor;
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
    procedure MenuItemClick(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAlternate(const Value: Boolean);
    procedure SetArrowColor(const Value: TColor);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetBorderRadius(Value: Single);
    procedure SetSelectBoxColor(const Value: TColor);
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetShadow(const Value: TGUIShadow);
  protected
    procedure AlignmentChanged; virtual;
    procedure AlternateChanged; virtual;
    procedure ArrowColorChanged; virtual;
    procedure ArrowWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure ButtonColorChanged; virtual;
    procedure ButtonWidthChanged; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure ItemIndexChanged; virtual;
    procedure SelectBoxColorChanged; virtual;
    procedure RenderSelectBox(PixelMap: TGuiCustomPixelMap);
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
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnHighlight;
    property BorderRadius: Single read FBorderRadius write SetBorderRadius;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnShadow;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Items: TStrings read FItems write SetItems;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property SelectBoxColor: TColor read FSelectBoxColor write SetSelectBoxColor default clBtnShadow;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiSelectBox = class(TCustomGuiSelectBox)
  published
    property Align;
    property Alignment;
    property Alternate;
    property Anchors;
    property ArrowColor;
    property ArrowWidth;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property ButtonColor;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property ItemIndex;
    property Items;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
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
  Math, SysUtils, DAV_Approximations, DAV_Common, DAV_Math, DAV_Complex,
  DAV_GuiCommon, DAV_GuiBlend;

{ TCustomGuiSelectBox }

procedure TCustomGuiSelectBox.Clear;
begin
 FItems.Clear;
 FItemIndex := -1;
end;

constructor TCustomGuiSelectBox.Create(AOwner: TComponent);
begin
 inherited;
 FArrowColor     := clBtnHighlight;
 FArrowWidth     := 1;
 FAlternate      := False;
 FAlignment      := taCenter;
 FBorderRadius   := 2;
 FBorderColor    := clBtnHighlight;
 FButtonColor    := clBtnShadow;
 FSelectBoxColor := clBtnHighlight;
 FSelectBoxColor := clBtnShadow;

 // create GUI font
 FGuiFont := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 // create items
 FItems          := TStringList.Create;
 FItemIndex      := -1;

 ButtonWidthChanged;
end;

destructor TCustomGuiSelectBox.Destroy;
begin
 FreeAndNil(FItems);

 FreeAndNil(FGuiFont);

 if Assigned(FPopupMenu)
  then FreeAndNil(FPopupMenu);

 inherited;
end;

function TCustomGuiSelectBox.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TCustomGuiSelectBox.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TCustomGuiSelectBox.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.RenderSelectBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  ButtonColor       : TPixel32;
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

  Val, Off          : TComplexDouble;
  Steps, i          : Integer;
  tmp               : Single;
  rad               : Integer;
  TextSize          : TSize;
  ArrowPos          : TPoint;
  PtsArray          : array of TPoint;
  Offsets           : array [0..1] of Integer;
begin
 with PixelMap do
  begin
   ButtonColor := ConvertColor(FButtonColor);
   if FBorderWidth = 0
    then BorderColor := ButtonColor
    else BorderColor := ConvertColor(FBorderColor);

   // draw circle
   Radius := Min(Min(FBorderRadius, 0.5 * Width), 0.5 * Height) + 1;
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
        then CombColor := ButtonColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombinePixel(BorderColor, ButtonColor, Round($FF - Temp * $FF));
        end else
       if SqrDist < SqrRadMinusOne
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       // lines
       if (X = FArrowButtonWidth) or (X = Width - 1 - FArrowButtonWidth)
        then CombColor := BorderColor;


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
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
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
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end
       else CombColor := ButtonColor;


       // lines
       if (X = FArrowButtonWidth) or (X = Width - 1 - FArrowButtonWidth)
        then CombColor := BorderColor;


       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;

   rad := FArrowButtonWidth;

   case FAlignment of
    taLeftJustify :
     begin

(*
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

*)
      if FItemIndex >= 0 then
       begin
        TextSize := FGuiFont.TextExtent(FItems[FItemIndex]);
        FGuiFont.TextOut(FItems[FItemIndex], PixelMap, rad, (Height - TextSize.cy) div 2);
       end;

     end;
    taCenter :
     begin
(*
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
*)

      if FItemIndex >= 0 then
       begin
        TextSize := FGuiFont.TextExtent(FItems[FItemIndex]);
        FGuiFont.TextOut(FItems[FItemIndex], PixelMap,
          (Width - TextSize.cx) div 2, (Height - TextSize.cy) div 2);
       end;
     end;
    taRightJustify :
     begin
(*
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
*)

      if FItemIndex >= 0 then
       begin
        TextSize := FGuiFont.TextExtent(FItems[FItemIndex]);
        FGuiFont.TextOut(FItems[FItemIndex], PixelMap,
          Width - rad - TextSize.cx, (Height - TextSize.cy) div 2);
       end;

     end;
   end;
  end;
end;

procedure TCustomGuiSelectBox.UpdateBuffer;
begin
 inherited;

 RenderSelectBox(FBuffer);
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
 BufferChanged;
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
 BufferChanged;
end;

procedure TCustomGuiSelectBox.ButtonWidthChanged;
begin
 case FAlignment of
   taLeftJustify : FArrowButtonWidth := 12 + Round(0.5 * FBorderWidth) + FArrowWidth;
        taCenter : FArrowButtonWidth := Max(Max(FArrowWidth, Round(0.5 * FBorderRadius)) + 4, abs(Font.Height)) + Round(0.5 * FBorderWidth);
  taRightJustify : FArrowButtonWidth := 12 + Round(0.5 * FBorderWidth) + FArrowWidth;
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
 BufferChanged;
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
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
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
 BufferChanged;
end;

procedure TCustomGuiSelectBox.BorderColorChanged;
begin
 BufferChanged;
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
 if Assigned(FOnChange)
  then FOnChange(Self);
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetItems(const Value: TStrings);
begin
 if Assigned(FItems)
  then FItems.Assign(Value)
  else FItems := Value;
 FItemIndex := - 1;
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TCustomGuiSelectBox.SetSelectBoxColor(const Value: TColor);
begin
 if FSelectBoxColor <> Value then
  begin
   FSelectBoxColor := Value;
   SelectBoxColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TCustomGuiSelectBox.SelectBoxColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetBorderRadius(Value: Single);
begin
 if Value < 0 then Value := 0;

 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiSelectBox.BorderRadiusChanged;
begin
 ButtonWidthChanged;
 BufferChanged;
end;

procedure TCustomGuiSelectBox.BorderWidthChanged;
begin
 BufferChanged;
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
