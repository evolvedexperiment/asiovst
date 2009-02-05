unit DAV_GuiLevelMeter;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DAV_Common, DAV_GuiBaseControl;

type
  TGuiLevelDirection = (ldirHorizontal, ldirHorizontalInverted, ldmVertical, ldmVerticalInverted);
  TGuiShowClipping = (scNo, scTopLeft, scBottomRight);

  TCustomGuiLevelMeter = class(TGuiBaseControl)
  private
    FBarWidthPercentage : Single;
    FClippingBoxSize    : Integer;
    FClippingFillColor  : TColor;
    FClippingFillStyle  : TBrushStyle;
    FClippingLineColor  : TColor;
    FClippingLineStyle  : TPenStyle;
    FClippingLineWidth  : Integer;
    FFillColor          : TColor;
    FFillStyle          : TBrushStyle;
    FLevelDirection     : TGuiLevelDirection;
    FLineStyle          : TPenStyle;
    FMaximumPeakLevel   : Single;
    FMaxPeakLineColor   : TColor;
    FMaxPeakLineStyle   : TPenStyle;
    FMaxPeakLineWidth   : Integer;
    FPeakLevel          : Single;
    FShowClipping       : TGuiShowClipping;
    FShowMaximum        : Boolean;

    procedure SetBarWidthPercentage(const Value: Single);
    procedure SetClippingBoxSize(const Value: Integer);
    procedure SetClippingFillColor(const Value: TColor);
    procedure SetClippingFillStyle(const Value: TBrushStyle);
    procedure SetClippingLineColor(const Value: TColor);
    procedure SetClippingLineStyle(const Value: TPenStyle);
    procedure SetClippingLineWidth(const Value: Integer);
    procedure SetFillColor(const Value: TColor);
    procedure SetFillStyle(const Value: TBrushStyle);
    procedure SetLevelDirection(const Value: TGuiLevelDirection);
    procedure SetLineStyle(const Value: TPenStyle);
    procedure SetMaxPeakLineColor(const Value: TColor);
    procedure SetMaxPeakLineStyle(const Value: TPenStyle);
    procedure SetMaxPeakLineWidth(const Value: Integer);
    procedure SetShowClipping(const Value: TGuiShowClipping);
    procedure SetShowMaximum(const Value: Boolean);
    procedure SetMaximumPeakLevel(const Value: Single);
    procedure SetPeakLevel(const Value: Single);
  protected
    procedure DrawGauge(GaugeRect: TRect); virtual;
    procedure DrawMaxPeakLine(x1, y1, x2, y2: Integer); virtual;
    procedure DrawClipIndicator(ClipIndRect: TRect); virtual;

    procedure DrawSingleBarH (BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure DrawSingleBarHI(BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure DrawSingleBarV (BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure DrawSingleBarVI(BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    property FillColor: TColor read FFillColor write SetFillColor default clGreen;
    property FillStyle: TBrushStyle read FFillStyle write SetFillStyle default bsSolid;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psSolid;

    property MaxPeakLineColor: TColor read FMaxPeakLineColor write SetMaxPeakLineColor default clBlue;
    property MaxPeakLineStyle: TPenStyle read FMaxPeakLineStyle write SetMaxPeakLineStyle default psSolid;
    property MaxPeakLineWidth: Integer read FMaxPeakLineWidth write SetMaxPeakLineWidth default 1;

    property ClippingLineColor: TColor read FClippingLineColor write SetClippingLineColor default clBlack;
    property ClippingLineStyle: TPenStyle read FClippingLineStyle write SetClippingLineStyle default psSolid;
    property ClippingLineWidth: Integer read FClippingLineWidth write SetClippingLineWidth default 1;
    property ClippingFillColor: TColor read FClippingFillColor write SetClippingFillColor default clRed;
    property ClippingFillStyle: TBrushStyle read FClippingFillStyle write SetClippingFillStyle default bsSolid;
    property ClippingBoxSize: Integer read FClippingBoxSize write SetClippingBoxSize default 5;

    property PeakLevel: Single read FPeakLevel write SetPeakLevel;
    property MaximumPeakLevel: Single read FMaximumPeakLevel write SetMaximumPeakLevel;
    property ShowMaximum: Boolean read FShowMaximum write SetShowMaximum default True;
    property ShowClipping: TGuiShowClipping read FShowClipping write SetShowClipping default scTopLeft;

    property LevelDirection: TGuiLevelDirection read FLevelDirection write SetLevelDirection default ldmVertical;
    property BarWidthPercentage: Single read FBarWidthPercentage write SetBarWidthPercentage;
  end;

  TGuiLevelMeter = class(TCustomGuiLevelMeter)
  published
    property Anchors;
    property BarWidthPercentage;
    property ClippingBoxSize;
    property ClippingFillColor;
    property ClippingFillStyle;
    property ClippingLineColor;
    property ClippingLineStyle;
    property ClippingLineWidth;
    property Color;
    property FillColor;
    property FillStyle;
    property LevelDirection;
    property LineColor;
    property LineStyle;
    property LineWidth;
    property MaximumPeakLevel;
    property MaxPeakLineColor;
    property MaxPeakLineStyle;
    property MaxPeakLineWidth;
    property PeakLevel;
    property ShowClipping;
    property ShowMaximum;
    property Transparent;
  end;

  TCustomGuiColorLevelMeter = class(TGuiBaseControl)
  private
    FLevelDirection   : TGuiLevelDirection;
    FPeakLevel        : Single;
    FLower            : Single;
    FUpper            : Single;
    FBorderColor      : TColor;
    FContrastLuminanz : Single;

    procedure SetLevelDirection(const Value: TGuiLevelDirection);
    procedure SetPeakLevel(const Value: Single);
    procedure SetLower(const Value: Single);
    procedure SetUpper(const Value: Single);
    procedure SetBorderColor(const Value: TColor);
    procedure DrawVertical(ClipRect: TRect);
    procedure SetContrastLuminanz(const Value: Single);
  protected
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;

    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property PeakLevel: Single read FPeakLevel write SetPeakLevel;
    property Lower: Single read FLower write SetLower;
    property Upper: Single read FUpper write SetUpper;
    property ContrastLuminanz: Single read FContrastLuminanz write SetContrastLuminanz;
    property LevelDirection: TGuiLevelDirection read FLevelDirection write SetLevelDirection default ldmVertical;
  end;

  TGuiColorLevelMeter = class(TCustomGuiColorLevelMeter)
  published
    property LineWidth;
    property Anchors;
    property BorderColor;
    property Color;
    property ContrastLuminanz;
    property LevelDirection;
    property PeakLevel;
    property Lower;
    property Upper;
    property Transparent;
  end;

implementation

uses
  SysUtils, Math, DAV_GuiCommon;

{ TCustomGuiLevelMeter }

constructor TCustomGuiLevelMeter.Create(AOwner: TComponent);
begin
  inherited;

  FFillColor         := clGreen;
  FFillStyle         := bsSolid;
  FLineStyle         := psSolid;
  FMaxPeakLineColor      := clBlue;
  FMaxPeakLineStyle      := psSolid;
  FMaxPeakLineWidth      := 1;

  FClippingLineColor := clBlack;
  FClippingLineStyle := psSolid;
  FClippingLineWidth := 1;
  FClippingFillColor := clRed;
  FClippingFillStyle := bsSolid;
  FClippingBoxSize   := 5;

  FShowMaximum       := True;
  FShowClipping      := scTopLeft;

  FLevelDirection    := ldmVertical;

  FBarWidthPercentage :=  0.8;
end;

destructor TCustomGuiLevelMeter.Destroy;
begin
  inherited;
end;

procedure TCustomGuiLevelMeter.DrawClipIndicator(ClipIndRect: TRect);
begin
  with FBuffer.Canvas do
   begin
    Pen.Color := FClippingLineColor;
    Pen.Width := FClippingLineWidth;
    Pen.Style := FClippingLineStyle;
    Brush.Color := FClippingFillColor;
    Brush.Style := FClippingFillStyle;
    Rectangle(ClipIndRect);
   end;
end;

procedure TCustomGuiLevelMeter.DrawGauge(GaugeRect: TRect);
begin
  with FBuffer.Canvas do
   begin
    Pen.Color := fLineColor;
    Pen.Width := fLineWidth;
    Pen.Style := FLineStyle;
    Brush.Color := FFillColor;
    Brush.Style := FFillStyle;

    Rectangle(GaugeRect);
   end;
end;

procedure TCustomGuiLevelMeter.DrawMaxPeakLine(x1, y1, x2, y2: Integer);
begin
  with FBuffer.Canvas do
  begin
    Pen.Color := FMaxPeakLineColor;
    Pen.Width := FMaxPeakLineWidth;
    Pen.Style := FMaxPeakLineStyle;

    MoveTo(x1, y1);
    LineTo(x2, y2);
  end;
end;


procedure TCustomGuiLevelMeter.DrawSingleBarH(BarRect: TRect; Peak, MaxPeak: Single);
var
  ClipIndRect  : TRect;
  GaugeRect    : TRect;
  tmpRect      : TRect;
  tmp          : Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left + FClippingBoxSize + 1, BarRect.Top, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Left + FClippingBoxSize, BarRect.Bottom); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right-FClippingBoxSize-1, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Right - FClippingBoxSize, BarRect.Top, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Right := Round((tmpRect.Right-tmpRect.Left)*tmp + tmpRect.Left);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp := min(1, MaxPeak);
    GaugeRect.Right := Round((GaugeRect.Right-GaugeRect.Left)*tmp + GaugeRect.Left);
    DrawMaxPeakLine(GaugeRect.Right, GaugeRect.Top, GaugeRect.Right, GaugeRect.Bottom);
  end;
  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.DrawSingleBarHI(BarRect: TRect; Peak, MaxPeak: Single);
var ClipIndRect, GaugeRect, tmpRect: TRect; tmp: Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left+FClippingBoxSize+1, BarRect.Top, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Left+FClippingBoxSize, BarRect.Bottom); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right-FClippingBoxSize-1, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Right-FClippingBoxSize, BarRect.Top, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Left := Round(tmpRect.Right-(tmpRect.Right-tmpRect.Left)*tmp);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp := min(1, MaxPeak);
    GaugeRect.Left := Round(GaugeRect.Right-(GaugeRect.Right-GaugeRect.Left)*tmp);
    DrawMaxPeakLine(GaugeRect.Left, GaugeRect.Top, GaugeRect.Left, GaugeRect.Bottom);
  end;

  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.DrawSingleBarV(BarRect: TRect; Peak, MaxPeak: Single);
var
  ClipIndRect,
  GaugeRect,
  tmpRect      : TRect;
  tmp          : Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left, BarRect.Top+FClippingBoxSize+1, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Top+FClippingBoxSize); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Bottom-FClippingBoxSize-1);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Bottom-FClippingBoxSize, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Top := Round(tmpRect.Bottom-(tmpRect.Bottom-tmpRect.Top)*tmp);
  DrawGauge(tmpRect);

  if FShowMaximum then
   begin
    tmp := min(1, MaxPeak);
    GaugeRect.Top := Round(GaugeRect.Bottom-(GaugeRect.Bottom-GaugeRect.Top)*tmp);
    DrawMaxPeakLine(GaugeRect.Left, GaugeRect.Top, GaugeRect.Right, GaugeRect.Top);
   end;

  if (FShowClipping <> scNo) and
     (MaxPeak > 1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.DrawSingleBarVI(BarRect: TRect; Peak, MaxPeak: Single);
var
  ClipIndRect,
  GaugeRect,
  tmpRect      : TRect;
  tmp          : Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left, BarRect.Top+FClippingBoxSize+1, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Top+FClippingBoxSize); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Bottom-FClippingBoxSize-1);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Bottom-FClippingBoxSize, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Bottom := Round((tmpRect.Bottom-tmpRect.Top)*tmp + tmpRect.Top);
  DrawGauge(tmpRect);

  if FShowMaximum then
   begin
    tmp := min(1, MaxPeak);
    GaugeRect.Bottom := Round((GaugeRect.Bottom - GaugeRect.Top) * tmp + GaugeRect.Top);
    DrawMaxPeakLine(GaugeRect.Left, GaugeRect.Bottom, GaugeRect.Right, GaugeRect.Bottom);
   end;

  if (FShowClipping <> scNo) and
     (MaxPeak > 1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.RedrawBuffer(doBufferFlip: Boolean);
var
  CurrentPeak, CurrentMax: Single;
  i: Integer;
  DestBarRect: TRect;
  SplitSize: Single;
  BarPadding: Single;
begin
  if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    Brush.Color := Self.Color;

    {$IFNDEF FPC}if FTransparent then DrawParentImage(FBuffer.Canvas) else{$ENDIF}
      FillRect(FBuffer.Canvas.ClipRect);

    if FLevelDirection in [ldirHorizontal, ldirHorizontalInverted]
     then SplitSize   := Height
     else SplitSize   := Width;

    BarPadding := (1 - FBarWidthPercentage) * SplitSize * 0.5;

    if FLevelDirection in [ldirHorizontal, ldirHorizontalInverted]
     then DestBarRect := Rect(0, Round(SplitSize * i + BarPadding), Width, Round(SplitSize * (i + 1) - BarPadding))
     else DestBarRect := Rect(Round(SplitSize * i + BarPadding), 0, Round(SplitSize * (i + 1) - BarPadding), Height);

    case FLevelDirection of
      ldirHorizontal:         DrawSingleBarH (DestBarRect, FPeakLevel, FMaximumPeakLevel);
      ldirHorizontalInverted: DrawSingleBarHI(DestBarRect, FPeakLevel, FMaximumPeakLevel);
      ldmVertical:            DrawSingleBarV (DestBarRect, FPeakLevel, FMaximumPeakLevel);
      ldmVerticalInverted:    DrawSingleBarVI(DestBarRect, FPeakLevel, FMaximumPeakLevel);
    end;

    UnLock;
   end;

  if doBufferFlip then Invalidate;
end;

procedure TCustomGuiLevelMeter.SetBarWidthPercentage(const Value: Single);
begin
  if FBarWidthPercentage <> Value then
   begin
    FBarWidthPercentage := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingFillColor(const Value: TColor);
begin
  if FClippingFillColor <> Value then
   begin
    FClippingFillColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingFillStyle(const Value: TBrushStyle);
begin
  if FClippingFillStyle <> Value then
   begin
    FClippingFillStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingLineColor(const Value: TColor);
begin
  if FClippingLineColor <> Value then
   begin
    FClippingLineColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingLineStyle(const Value: TPenStyle);
begin
  if FClippingLineStyle <> Value then
   begin
    FClippingLineStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingLineWidth(const Value: Integer);
begin
  if FClippingLineWidth <> Value then
   begin
    FClippingLineWidth := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingBoxSize(const Value: Integer);
begin
  if FClippingBoxSize <> Value then
   begin
    FClippingBoxSize := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
   begin
    FFillColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetFillStyle(const Value: TBrushStyle);
begin
  if FFillStyle <> Value then
   begin
    FFillStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetLevelDirection(const Value: TGuiLevelDirection);
begin
  if FLevelDirection <> Value then
   begin
    FLevelDirection := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetLineStyle(const Value: TPenStyle);
begin
  if FLineStyle <> Value then
   begin
    FLineStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetMaxPeakLineColor(const Value: TColor);
begin
  if FMaxPeakLineColor <> Value then
   begin
    FMaxPeakLineColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetMaxPeakLineStyle(const Value: TPenStyle);
begin
  if FMaxPeakLineStyle <> Value then
   begin
    FMaxPeakLineStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetMaxPeakLineWidth(const Value: Integer);
begin
  if FMaxPeakLineWidth <> Value then
   begin
    FMaxPeakLineWidth := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetPeakLevel(const Value: Single);
begin
 if PeakLevel <> Value then
  begin
   FPeakLevel := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLevelMeter.SetShowClipping(const Value: TGuiShowClipping);
begin
  if FShowClipping <> Value then
   begin
    FShowClipping := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetShowMaximum(const Value: Boolean);
begin
  if FShowMaximum <> Value then
   begin
    FShowMaximum := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetMaximumPeakLevel(const Value: Single);
begin
 if MaximumPeakLevel <> Value then
  begin
   FMaximumPeakLevel := Value;
   RedrawBuffer(True);
  end;
end;


{ TCustomGuiColorLevelMeter }

constructor TCustomGuiColorLevelMeter.Create(AOwner: TComponent);
begin
 inherited;
 FLevelDirection := ldmVertical;
 FPeakLevel := 0;
 FLower := 0;
 FUpper := 1;
 FContrastLuminanz := 0.3;
 FBorderColor := clWindowFrame;
end;

procedure TCustomGuiColorLevelMeter.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiColorLevelMeter.SetContrastLuminanz(const Value: Single);
begin
 if FContrastLuminanz <> Value then
  begin
   FContrastLuminanz := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiColorLevelMeter.SetLevelDirection(
  const Value: TGuiLevelDirection);
begin
 if LevelDirection <> Value then
  begin
   FLevelDirection := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiColorLevelMeter.SetLower(const Value: Single);
begin
 if Lower <> Value then
  begin
   FLower := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiColorLevelMeter.SetPeakLevel(const Value: Single);
begin
 if PeakLevel <> Value then
  begin
   FPeakLevel := Value;
   RedrawBuffer(True);
   Invalidate;
  end;
end;

procedure TCustomGuiColorLevelMeter.SetUpper(const Value: Single);
begin
 if Upper <> Value then
  begin
   FUpper := Value;
   RedrawBuffer(True);
   Invalidate;
  end;
end;

procedure TCustomGuiColorLevelMeter.DrawVertical(ClipRect: TRect);
var
  x, y    : Integer;
  Line    : TRGB24Array;
  H, S, L : Single;
  PeakRel : Single;
begin
 PeakRel := 1 - (FPeakLevel - FLower) / (FUpper - FLower);
 with FBuffer, Canvas do
  begin
   y := ClipRect.Top;
   Pen.Width := FLineWidth;
   while y < ClipRect.Bottom do
    begin
     H := Y / (ClipRect.Bottom - ClipRect.Top);
     if H > PeakRel
      then S := 1
      else S := 0;
     H := 0.66 * (1 - sqr(1 - H));
     L := FContrastLuminanz *(1 + (y div FLineWidth) mod 2);
     Pen.Color := HLSToRGB(H, L, S);
     MoveTo(ClipRect.Left, y);
     LineTo(ClipRect.Right, y);
     inc(Y, FLineWidth);
    end;
  end;
end;

procedure TCustomGuiColorLevelMeter.RedrawBuffer(doBufferFlip: Boolean);
var
  R    : TRect;
begin
 with FBuffer.Canvas do
  begin
   R := ClipRect;
   Brush.Color := FBorderColor;
   Brush.Style := bsSolid;
   InflateRect(R, -1, -1);
   case FLevelDirection of
    ldmVertical : DrawVertical(R);
   end;
   InflateRect(R, 1, 1);
   Brush.Style := bsSolid;
   FrameRect(R);
  end;

 inherited;
end;

end.
