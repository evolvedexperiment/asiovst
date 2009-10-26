unit DAV_GuiPanel;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Messages, Controls, Graphics, ExtCtrls, DAV_GuiBaseControl;

type
  TCustomGuiPanel = class(TCustomPanel)
  private
    FAntiAlias      : TGuiAntiAlias;
    FBorderVisible  : Boolean;
    FPanelColor     : TColor;
    FOwnerDraw      : Boolean;
    FOSFactor       : Integer;
    FRoundRadius    : Integer;
    FTransparent    : Boolean;
    FLineWidth      : Integer;
    FLineColor      : TColor;
    FBitmap         : TBitmap;
    FBitmapChanged  : Boolean;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged (var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure RenderPanelToBitmap(Bitmap: TBitmap);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetBorderVisible(const Value: Boolean);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetLineColor(const Value: TColor);
    procedure SetLinewidth(const Value: Integer);
    procedure SetPanelColor(const Value: TColor);
    procedure SetRoundRadius(const Value: Integer);
    procedure SetTransparent (const Value: Boolean);
    procedure PaintBitmap;
    procedure SetBitmapChanged(const Value: Boolean);
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);
  protected
    procedure Paint; override;
    procedure Resize; override;
    property BitmapChanged: Boolean read FBitmapChanged write SetBitmapChanged; 
  public
    constructor Create (AOwner: TComponent); override;
    procedure AssignTo(Dest: TPersistent); override;

    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property BorderVisible: Boolean read FBorderVisible write SetBorderVisible default True;
    property PanelColor: TColor read FPanelColor write SetPanelColor default clBtnShadow;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default True;
    property LineColor: TColor read FLineColor write SetLineColor default clBtnHighlight;
    property Linewidth: Integer read FLineWidth write SetLinewidth default 2;
    property Radius: Integer read FRoundRadius write SetRoundRadius default 2;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiPanel = class(TCustomGuiPanel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AntiAlias;
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
    property Font;
    property Hint;
    property LineColor;
    property Linewidth;
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
  Types, StdCtrls, SysUtils, Math, DAV_Math, DAV_Complex, DAV_GuiCommon;

{ TCustomGuiPanel }

constructor TCustomGuiPanel.Create (AOwner: TComponent);
begin
 inherited Create(AOwner);
 ParentFont           := True;
 FPanelColor          := clBtnHighlight;
 FLineColor           := clBtnShadow;
 FLineWidth           := 2;
 FRoundRadius         := 2;
 FBorderVisible       := True;
 FOwnerDraw           := True;
 FOSFactor            := 1;
 FAntiAlias           := gaaNone;
 FBitmap              := TBitmap.Create;
 FBitmap.PixelFormat  := pf32bit;
 FBitmapChanged       := True;
 ParentColor          := True;
 ControlStyle         := ControlStyle + [csAcceptsControls, csOpaque];
 SetBounds(0, 0, 185, 41);
end;

procedure TCustomGuiPanel.SetOwnerDraw(const Value: Boolean);
begin
 if FOwnerDraw <> Value then
  begin
   FOwnerDraw := Value;
   if FOwnerDraw
    then BitmapChanged := True
    else Invalidate;
  end;
end;

procedure TCustomGuiPanel.SetLineColor(const Value: TColor);
begin
 if FLineColor <> Value then
  begin
   FLineColor := Value;
   if FOwnerDraw then BitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.SetLinewidth(const Value: Integer);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
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

procedure TCustomGuiPanel.UpsampleBitmap(Bitmap: TBitmap);
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

procedure TCustomGuiPanel.DownsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Downsample2xBitmap32(Bitmap);
   gaaLinear3x: Downsample3xBitmap32(Bitmap);
   gaaLinear4x: Downsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TCustomGuiPanel.PaintBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBitmap do
   begin
    Canvas.Lock;
    try

     with Canvas.Brush do
      begin
       Style := bsSolid;
       Color := Self.Color;
      end;
      
     Width  := FOSFactor * Self.ClientRect.Right;
     Height := FOSFactor * Self.ClientRect.Bottom;
     case FAntiAlias of
      gaaNone     :
       begin
        {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, Canvas) else {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPanelToBitmap(FBitmap);
       end;
      else
       begin
        {$IFNDEF FPC}
        if FTransparent then
         begin
          CopyParentImage(Self, FBitmap.Canvas);
          UpsampleBitmap(FBitmap);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPanelToBitmap(FBitmap);
        DownsampleBitmap(FBitmap);
       end;
     end;
    finally
     Canvas.Unlock;
    end;
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
    Canvas.Draw(0, 0, FBitmap);
   end;
end;

procedure TCustomGuiPanel.RenderPanelToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexDouble;
  Steps, i : Integer;
  tmp      : Single;
  rad      : Integer;
  PtsArray : Array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;
   Font.Assign(Self.Font);
   Font.Size := FOSFactor * Font.Size;

   Brush.Style := bsClear;
   Brush.Color := FPanelColor;
   Pen.Width   := FOSFactor * FLineWidth;
   Pen.Color   := FLineColor;

   case FRoundRadius of
    0, 1 : begin
            Brush.Color := FLineColor;
            FrameRect(ClipRect);
           end;
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
        if FLineColor <> FPanelColor
         then PolyLine(PtsArray);
       end;
     end;
   end;

(*
   // Draw Text
   Canvas.Font := Self.Font;
   Canvas.Brush.Style := bsClear;
   if not Enabled then
    begin
     OffsetRect(textBounds, 1, 1);
     Canvas.Font.Color := fDisabledHighlightColor;
     DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format);
     OffsetRect(textBounds, -1, -1);
     Canvas.Font.Color := fDisabledShadowColor;
     DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format);
    end
   else DrawText(Canvas.Handle, PChar(Caption), Length(Caption), textBounds, format);
*)

   Unlock;
  end;
end;

procedure TCustomGuiPanel.Resize;
begin
 inherited;
 if FBitmap.Width <> Width then
  begin
   FBitmap.Width := Width;
   FBitmapChanged := True;
  end;
 if FBitmap.Height <> Height then
  begin
   FBitmap.Height := Height;
   FBitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.SetRoundRadius(const Value: Integer);
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

procedure TCustomGuiPanel.CMTextChanged(var Message: TWmNoParams);
begin
 inherited;
 if FOwnerDraw then BitmapChanged := True;
end;

procedure TCustomGuiPanel.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   if FOwnerDraw then BitmapChanged := True;
  end;
end;

procedure TCustomGuiPanel.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   case FAntiAlias of
         gaaNone : FOSFactor :=  1;
     gaaLinear2x : FOSFactor :=  2;
     gaaLinear3x : FOSFactor :=  3;
     gaaLinear4x : FOSFactor :=  4;
     gaaLinear8x : FOSFactor :=  8;
    gaaLinear16x : FOSFactor := 16;
   end;
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
