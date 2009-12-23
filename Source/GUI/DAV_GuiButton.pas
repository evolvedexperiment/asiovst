unit DAV_GuiButton;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Controls, Graphics, DAV_GuiCommon, DAV_GuiBaseControl;

type
  TCustomGuiButton = class(TCustomGuiBaseControl)
  private
    FRoundRadius  : Integer;
    FAlignment    : TAlignment;
    FAntiAlias    : TGuiAntiAlias;
    FCaption      : string;
    FOSFactor     : Integer;
    FButtonColor  : TColor;
    FShadow       : TGUIShadow;
    procedure SetRoundRadius(Value: Integer);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetButtonColor(const Value: TColor);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetShadow(const Value: TGUIShadow);
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);
  protected
    procedure UpdateBuffer; override;
    procedure AntiAliasChanged; virtual;
    procedure RenderButtonToBitmap(const Bitmap: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Caption: string read FCaption write SetCaption;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnShadow;
    property Radius: Integer read FRoundRadius write SetRoundRadius default 2;
    property LineColor default clBtnHighlight;
    property Shadow: TGUIShadow read FShadow write SetShadow;
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
    property Shadow;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property Transparent;
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
  Math, SysUtils, DAV_Math, DAV_Complex;

{ TCustomGuiButton }

constructor TCustomGuiButton.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle  := ControlStyle + [csFramed, csOpaque, csReplicatable,
                                  csAcceptsControls];

 FShadow       := TGUIShadow.Create;

 FAlignment    := taCenter;
 FCaption      := 'empty';
 FRoundRadius  := 2;
 FOSFactor     := 1;
 FLineColor    := clBtnHighlight;
 FButtonColor  := clBtnShadow;
end;

destructor TCustomGuiButton.Destroy;
begin
 FreeAndNil(FShadow);
 inherited;
end;

procedure TCustomGuiButton.RenderButtonToBitmap(const Bitmap: TBitmap);
{$IFNDEF FPC}
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
   Font.Size := FOSFactor * Font.Size;

   Brush.Style := bsClear;
   Brush.Color := FButtonColor;
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
        if FLineColor <> FButtonColor
         then PolyLine(PtsArray);
       end;
     end;
   end;

   TextSize := TextExtent(FCaption);

   if FShadow.Visible then
    begin
     Font.Color := FShadow.Color;
     case FAlignment of
       taLeftJustify : TextOut(FOSFactor * FShadow.Offset.X, (Bitmap.Height - TextSize.cy) div 2 + FOSFactor * FShadow.Offset.Y, FCaption);
      taRightJustify : TextOut(FOSFactor * FShadow.Offset.X + Bitmap.Width - TextSize.cx, (Bitmap.Height - TextSize.cy) div 2 + FOSFactor * FShadow.Offset.Y, FCaption);
            taCenter : TextOut(FOSFactor * FShadow.Offset.X + (Bitmap.Width - TextSize.cx) div 2, (Bitmap.Height - TextSize.cy) div 2 + FOSFactor * FShadow.Offset.Y, FCaption);
     end;
     Font.Color := Self.Font.Color;
    end;

   case FAlignment of
     taLeftJustify : TextOut(0, (Bitmap.Height - TextSize.cy) div 2, FCaption);
    taRightJustify : TextOut(Bitmap.Width - TextSize.cx, (Bitmap.Height - TextSize.cy) div 2, FCaption);
          taCenter : TextOut((Bitmap.Width - TextSize.cx) div 2, (Bitmap.Height - TextSize.cy) div 2, FCaption);
   end;

   Unlock;
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TCustomGuiButton.UpsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Upsample2xBitmap32(Bitmap);
   gaaLinear3x: Upsample3xBitmap32(Bitmap);
   gaaLinear4x: Upsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TCustomGuiButton.DownsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x : Downsample2xBitmap32(Bitmap);
   gaaLinear3x : Downsample3xBitmap32(Bitmap);
   gaaLinear4x : Downsample4xBitmap32(Bitmap);
   gaaLinear8x : begin
                  Downsample4xBitmap32(Bitmap);
                  Downsample2xBitmap32(Bitmap);
                 end;
  gaaLinear16x : begin
                  Downsample4xBitmap32(Bitmap);
                  Downsample4xBitmap32(Bitmap);
                 end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TCustomGuiButton.UpdateBuffer;
var
  Bmp : TBitmap;
begin
 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   Brush.Style := bsSolid;
   Brush.Color := Self.Color;
   case FAntiAlias of
    gaaNone     :
     begin
      {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);
      RenderButtonToBitmap(FBuffer);
     end;
    else
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width  := FOSFactor * FBuffer.Width;
        Height := FOSFactor * FBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        {$IFNDEF FPC}
        if FTransparent then
         begin
          CopyParentImage(Self, Bmp.Canvas);
          UpsampleBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        FillRect(ClipRect);
        RenderButtonToBitmap(Bmp);
        DownsampleBitmap(Bmp);
        Draw(0, 0, Bmp);
       finally
        Free;
       end;
     end;
   end;
  end;
end;

procedure TCustomGuiButton.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiButton.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TCustomGuiButton.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 Invalidate;
end;

procedure TCustomGuiButton.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiButton.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiButton.SetRoundRadius(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiButton.SetShadow(const Value: TGUIShadow);
begin
 FShadow.Assign(Value);
end;

end.
