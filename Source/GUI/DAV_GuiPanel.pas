unit DAV_GuiPanel;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Messages, Controls, Graphics, ExtCtrls, DAV_GuiBaseControl;

type
  TCustomGuiPanel = class(TCustomPanel)
  private
    fAntiAlias              : TGuiAntiAlias;
    fBorderVisible          : Boolean;
    fPanelColor             : TColor;
    fOwnerDraw              : Boolean;
    fOSFactor               : Integer;
    fRoundRadius            : Integer;
    fTransparent            : Boolean;
    fLineWidth              : Integer;
    fLineColor              : TColor;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged (var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure DrawParentImage(Dest: TCanvas);
    procedure RenderPanelToBitmap(Bitmap: TBitmap);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetBorderVisible(const Value: Boolean);
    procedure SetOwnerDraw(const Value: Boolean);
    procedure SetLineColor(const Value: TColor);
    procedure SetLinewidth(const Value: Integer);
    procedure SetPanelColor(const Value: TColor);
    procedure SetRoundRadius(const Value: Integer);
    procedure SetTransparent (const Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;

    property AntiAlias: TGuiAntiAlias read fAntiAlias write SetAntiAlias default gaaNone;
    property BorderVisible: Boolean read fBorderVisible write SetBorderVisible default True;
    property PanelColor: TColor read fPanelColor write SetPanelColor default clBtnShadow;
    property OwnerDraw: Boolean read fOwnerDraw write SetOwnerDraw default True;
    property LineColor: TColor read fLineColor write SetLineColor default clBtnHighlight;
    property Linewidth: Integer read fLinewidth write SetLinewidth default 2;
    property Radius: Integer read fRoundRadius write SetRoundRadius default 2;
    property Transparent: Boolean read fTransparent write SetTransparent default False;
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
    property Transparent;
    property UseDockManager;
    property Visible;

    property OnEndDock;
    property OnStartDock;
    property OnCanResize;
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
  SysUtils, Math, DAV_Common, DAV_Complex, Types;

{ TCustomGuiPanel }

constructor TCustomGuiPanel.Create (AOwner: TComponent);
begin
 inherited Create(AOwner);
 ParentFont      := True;
 fPanelColor     := clBtnHighlight;
 fLineColor      := clBtnShadow;
 fLineWidth      := 2;
 fRoundRadius    := 2;
 fBorderVisible  := True;
 fOwnerDraw      := True;
 fOSFactor       := 1;
 fAntiAlias      := gaaNone;
 ParentColor     := True;
 ControlStyle    := ControlStyle + [csAcceptsControls, csOpaque];
 SetBounds(0, 0, 185, 41);
end;

destructor TCustomGuiPanel.Destroy;
begin
 inherited;
end;

procedure TCustomGuiPanel.SetOwnerDraw(const Value: Boolean);
begin
 if fOwnerDraw <> Value then
  begin
   fOwnerDraw := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiPanel.SetLineColor(const Value: TColor);
begin
 if fLineColor <> Value then
  begin
   fLineColor := Value;
   if fOwnerDraw then Invalidate;
  end;
end;

procedure TCustomGuiPanel.SetLinewidth(const Value: Integer);
begin
 if fLinewidth <> Value then
  begin
   fLinewidth := Value;
   if fOwnerDraw then Invalidate;
  end;
end;

procedure TCustomGuiPanel.SetPanelColor(const Value: TColor);
begin
 if fPanelColor <> Value then
  begin
   fPanelColor := Value;
   if fOwnerDraw then Invalidate;
  end;
end;

procedure TCustomGuiPanel.DrawParentImage(Dest: TCanvas);
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

procedure TCustomGuiPanel.Paint;
var
(*
  textBounds : TRect;
  format     : UINT;
*)
  Bmp        : TBitmap;
begin
 if not fOwnerDraw then
  begin
   inherited;
   exit;
  end;
(*
 textBounds := ClientRect;
 format := DT_SINGLELINE or DT_VCENTER;
 case Alignment of
  taLeftJustify  : format := format or DT_LEFT;
        taCenter : format := format or DT_CENTER;
  taRightJustify : format := format or DT_RIGHT;
 end;
*)

 if (Width > 0) and (Height > 0) then
  begin
   Bmp := TBitmap.Create;
   with Bmp do
    try
     PixelFormat := pf32bit;
     Canvas.Lock;
     Canvas.Brush.Style := bsSolid;
     Canvas.Brush.Color := PanelColor;
     Width  := fOSFactor * Self.ClientRect.Right;
     Height := fOSFactor * Self.ClientRect.Bottom;
     case fAntiAlias of
      gaaNone     :
       begin
        {$IFNDEF FPC}if fTransparent then DrawParentImage(Canvas) else {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPanelToBitmap(Bmp);
       end;
      gaaLinear2x :
       begin
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          UpsampleBitmap2x(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPanelToBitmap(Bmp);
        DownsampleBitmap2x(Bmp);
       end;
      gaaLinear4x :
       begin
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          UpsampleBitmap4x(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPanelToBitmap(Bmp);
        DownsampleBitmap4x(Bmp);
       end;
      gaaLinear8x :
       begin
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          UpsampleBitmap4x(Bmp);
          UpsampleBitmap2x(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPanelToBitmap(Bmp);
        DownsampleBitmap4x(Bmp);
        DownsampleBitmap2x(Bmp);
       end;
      gaaLinear16x :
       begin
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          UpsampleBitmap4x(Bmp);
          UpsampleBitmap4x(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderPanelToBitmap(Bmp);
        DownsampleBitmap4x(Bmp);
        DownsampleBitmap4x(Bmp);
       end;
     end;
    finally
     Self.Canvas.Draw(0, 0, Bmp);
     FreeAndNil(Bmp)
    end;
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
   Font.Size := fOSFactor * Font.Size;

   Brush.Style := bsClear;
   Brush.Color := fPanelColor;
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
        if fLineColor <> fPanelColor
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

procedure TCustomGuiPanel.SetRoundRadius(const Value: Integer);
begin
 if fRoundRadius <> Value then
  begin
   fRoundRadius := Value;
   if fOwnerDraw then Invalidate;
  end;
end;

procedure TCustomGuiPanel.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 if fOwnerDraw then Invalidate;
end;

procedure TCustomGuiPanel.CMTextChanged(var Message: TWmNoParams);
begin
 inherited;
 if fOwnerDraw then Invalidate;
end;

procedure TCustomGuiPanel.SetTransparent(const Value: Boolean);
begin
 if fTransparent <> Value then
  begin
   fTransparent := Value;
   if OwnerDraw then Invalidate;
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
     gaaLinear4x : FOSFactor :=  4;
     gaaLinear8x : FOSFactor :=  8;
    gaaLinear16x : FOSFactor := 16;
   end;
   if fOwnerDraw then Invalidate;
  end;
end;

procedure TCustomGuiPanel.SetBorderVisible(const Value: Boolean);
begin
 if fBorderVisible <> Value then
  begin
   fBorderVisible := Value;
   if fOwnerDraw then Invalidate;
  end;
end;

procedure TCustomGuiPanel.AssignTo(Dest: TPersistent);
begin
 if Dest is TBitmap
  then (Dest as TBitmap).Canvas.Assign(Canvas)
  else inherited;
end;


end.
