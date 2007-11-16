unit DGuiLevelMeter;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DGuiBaseControl, DAVDCommon, DDspEnvelopeFollower;

type
  TGuiLevelDirection = (ldirHorizontal, ldirHorizontalInverted, ldmVertical, ldmVerticalInverted);
  TGuiShowClipping = (scNo, scTopLeft, scBottomRight);


  TGuiLevelMeter = class(TGuiBaseControl)
  private
    FShowMaximum        : Boolean;
    FMaxLineWidth       : Integer;
    FClippingLineWidth  : Integer;
    FBarWidthPercentage : Single;
    FClippingFillStyle  : TBrushStyle;
    FFillStyle          : TBrushStyle;
    FFillColor          : TColor;
    FMaxLineColor       : TColor;
    FClippingFillColor  : TColor;
    FClippingLineColor  : TColor;
    FLevelDirection     : TGuiLevelDirection;
    FShowClipping       : TGuiShowClipping;
    FMaxLineStyle       : TPenStyle;
    FLineStyle          : TPenStyle;
    FClippingLineStyle  : TPenStyle;
    FClippingBoxSize    : Integer;
    FPeakEnvFollower    : TDspEnvelopeFollower;
    FMaxPeakEnvFollower : TDspEnvelopeFollower;
    FMaximumTimeFactor  : Single;
    FLastMaxPeaks       : TAVDSingleDynArray;
    FLastMinPeaks       : TAVDSingleDynArray;

    procedure SetBarWidthPercentage(const Value: Single);
    procedure SetClippingFillColor(const Value: TColor);
    procedure SetClippingFillStyle(const Value: TBrushStyle);
    procedure SetClippingLineColor(const Value: TColor);
    procedure SetClippingLineStyle(const Value: TPenStyle);
    procedure SetClippingLineWidth(const Value: Integer);
    procedure SetClippingBoxSize(const Value: Integer);
    procedure SetDisplayChannels(const Value: Integer);
    procedure SetFillColor(const Value: TColor);
    procedure SetFillStyle(const Value: TBrushStyle);
    procedure SetLevelAttack(const Value: Single);
    procedure SetLevelRelease(const Value: Single);
    procedure SetMaximumTimeFactor(const Value: Single);
    procedure SetLevelDirection(const Value: TGuiLevelDirection);
    procedure SetLineStyle(const Value: TPenStyle);
    procedure SetMaxLineColor(const Value: TColor);
    procedure SetMaxLineStyle(const Value: TPenStyle);
    procedure SetMaxLineWidth(const Value: Integer);
    procedure SetShowClipping(const Value: TGuiShowClipping);
    procedure SetShowMaximum(const Value: Boolean);

    function GetLevelAttack: Single;
    function GetLevelRelease: Single;
    function GetDisplayChannels: Integer;
  protected
    procedure DrawGauge(GaugeRect: TRect); virtual;
    procedure DrawMaxLine(x1, y1, x2, y2: Integer); virtual;
    procedure DrawClipIndicator(ClipIndRect: TRect); virtual;

    procedure DrawSingleBarH (BarRect: TRect; Peak, MaxPeak: single); virtual;
    procedure DrawSingleBarHI(BarRect: TRect; Peak, MaxPeak: single); virtual;
    procedure DrawSingleBarV (BarRect: TRect; Peak, MaxPeak: single); virtual;
    procedure DrawSingleBarVI(BarRect: TRect; Peak, MaxPeak: single); virtual;
    procedure SetRedrawInterval(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure RedrawBuffer(doBufferFlip: Boolean); override;
    procedure ResetPeaks;

    procedure ProcessBufferIndirect(NewWaveData: TAVDArrayOfSingleDynArray; Channels, SampleFrames: Integer);
    procedure ProcessBuffer(NewWaveData: TAVDSingleDynArray; InpLen: Integer = -1); overload;
    procedure ProcessBuffer(NewWaveData: TAVDArrayOfSingleDynArray; InpLen: Integer = -1); overload;
  published
    property Transparent;
    property LineWidth;
    property LineColor;
    property Color;
    property RedrawInterval;

    property FillColor: TColor read FFillColor write SetFillColor default clGreen;
    property FillStyle: TBrushStyle read FFillStyle write SetFillStyle default bsSolid;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psSolid;

    property MaxLineColor: TColor read FMaxLineColor write SetMaxLineColor default clBlue;
    property MaxLineStyle: TPenStyle read FMaxLineStyle write SetMaxLineStyle default psSolid;
    property MaxLineWidth: Integer read FMaxLineWidth write SetMaxLineWidth default 1;

    property ClippingLineColor: TColor read FClippingLineColor write SetClippingLineColor default clBlack;
    property ClippingLineStyle: TPenStyle read FClippingLineStyle write SetClippingLineStyle default psSolid;
    property ClippingLineWidth: Integer read FClippingLineWidth write SetClippingLineWidth default 1;
    property ClippingFillColor: TColor read FClippingFillColor write SetClippingFillColor default clRed;
    property ClippingFillStyle: TBrushStyle read FClippingFillStyle write SetClippingFillStyle default bsSolid;
    property ClippingBoxSize: Integer read FClippingBoxSize write SetClippingBoxSize default 5;

    property ShowMaximum: Boolean read FShowMaximum write SetShowMaximum default true;
    property ShowClipping: TGuiShowClipping read FShowClipping write SetShowClipping default scTopLeft;

    property LevelAttack: Single read GetLevelAttack write SetLevelAttack;
    property LevelRelease: Single read GetLevelRelease write SetLevelRelease;

    property LevelDirection: TGuiLevelDirection read FLevelDirection write SetLevelDirection default ldmVertical;
    property DisplayChannels: Integer read GetDisplayChannels write SetDisplayChannels default 2;
    property BarWidthPercentage: Single read FBarWidthPercentage write SetBarWidthPercentage;
    property MaximumTimeFactor: single read FMaximumTimeFactor write SetMaximumTimeFactor;
  end;
  
implementation

uses SysUtils, Math,
    {$IFDEF PUREPASCAL}DAVDBufferMathPascal{$ELSE}DAVDBufferMathAsm,
  DateUtils{$ENDIF};

{ TGuiLevelMeter }

constructor TGuiLevelMeter.Create(AOwner: TComponent);
begin    
  inherited;

  FPeakEnvFollower:=TDspEnvelopeFollower.Create(self);
  FMaxPeakEnvFollower:=TDspEnvelopeFollower.Create(self);

  FFillColor         := clGreen;
  FFillStyle         := bsSolid;
  FLineStyle         := psSolid;
  FMaxLineColor      := clBlue;
  FMaxLineStyle      := psSolid;
  FMaxLineWidth      := 1;

  FClippingLineColor := clBlack;
  FClippingLineStyle := psSolid;
  FClippingLineWidth := 1;
  FClippingFillColor := clRed;
  FClippingFillStyle := bsSolid;
  FClippingBoxSize   := 5;

  FShowMaximum       := true;
  FShowClipping      := scTopLeft;

  FLevelDirection    := ldmVertical;
  FPeakEnvFollower.Channels := 2;
  FMaxPeakEnvFollower.Channels := 2;

  FBarWidthPercentage:= 0.8;

  ResetPeaks;

  FMaximumTimeFactor:=3;
  RedrawInterval := 30;
  LevelAttack    := 0;
  LevelRelease   := 0;
  FMaxPeakEnvFollower.Attack:=0;
  FMaxPeakEnvFollower.Release:=1;
end;

destructor TGuiLevelMeter.Destroy;
begin
  FPeakEnvFollower.Free;
  FMaxPeakEnvFollower.Free;
  SetLength(FLastMaxPeaks, 0);
  SetLength(FLastMinPeaks, 0);
  inherited;
end;

procedure TGuiLevelMeter.ResetPeaks;
begin
  setlength(FLastMaxPeaks, FPeakEnvFollower.Channels);
  setlength(FLastMinPeaks, FPeakEnvFollower.Channels);
  FillChar(FLastMaxPeaks[0], SizeOf(Single)*FPeakEnvFollower.Channels, 0);
  FillChar(FLastMinPeaks[0], SizeOf(Single)*FPeakEnvFollower.Channels, 0);
  RedrawBuffer(true);
end;


procedure TGuiLevelMeter.SetLevelAttack(const Value: Single);
begin
  FPeakEnvFollower.Attack := Value;
end;

procedure TGuiLevelMeter.SetLevelRelease(const Value: Single);
begin
  FPeakEnvFollower.Release := Value;
end;

function TGuiLevelMeter.GetLevelAttack: Single;
begin
  Result := FPeakEnvFollower.Attack
end;

function TGuiLevelMeter.GetLevelRelease: Single;
begin
  Result := FPeakEnvFollower.Release
end;

procedure TGuiLevelMeter.DrawClipIndicator(ClipIndRect: TRect);
begin
  with fBuffer.Canvas do
  begin
    pen.Color:=FClippingLineColor;
    pen.Width:=FClippingLineWidth;
    pen.Style:=FClippingLineStyle;
    brush.Color:=FClippingFillColor;
    brush.Style:=FClippingFillStyle;
    Rectangle(ClipIndRect);
  end;
end;

procedure TGuiLevelMeter.DrawGauge(GaugeRect: TRect);
begin
  with fBuffer.Canvas do
  begin
    pen.Color:=fLineColor;
    pen.Width:=fLineWidth;
    pen.Style:=FLineStyle;
    brush.Color:=FFillColor;
    brush.Style:=FFillStyle;
    
    Rectangle(GaugeRect);
  end;
end;

procedure TGuiLevelMeter.DrawMaxLine(x1, y1, x2, y2: Integer);
begin
  with fBuffer.Canvas do
  begin
    pen.Color:=FMaxLineColor;
    pen.Width:=FMaxLineWidth;
    pen.Style:=FMaxLineStyle;

    moveto(x1, y1);
    lineto(x2, y2);
  end;
end;


procedure TGuiLevelMeter.DrawSingleBarH(BarRect: TRect; Peak, MaxPeak: single);
var ClipIndRect, GaugeRect, tmpRect: TRect; tmp: single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect  :=BarRect;
                         ClipIndRect:=Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect  :=Rect(BarRect.Left+FClippingBoxSize+1, BarRect.Top, BarRect.Right, BarRect.Bottom);
                         ClipIndRect:=Rect(BarRect.Left, BarRect.Top, BarRect.Left+FClippingBoxSize, BarRect.Bottom); end;
    scBottomRight: begin GaugeRect  :=Rect(BarRect.Left, BarRect.Top, BarRect.Right-FClippingBoxSize-1, BarRect.Bottom);
                         ClipIndRect:=Rect(BarRect.Right-FClippingBoxSize, BarRect.Top, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp:=min(1, Peak);
  tmpRect:=GaugeRect;
  tmpRect.Right:=Round((tmpRect.Right-tmpRect.Left)*tmp + tmpRect.Left);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp:=min(1, MaxPeak);
    GaugeRect.Right:=Round((GaugeRect.Right-GaugeRect.Left)*tmp + GaugeRect.Left);
    DrawMaxLine(GaugeRect.Right, GaugeRect.Top, GaugeRect.Right, GaugeRect.Bottom);
  end;
  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TGuiLevelMeter.DrawSingleBarHI(BarRect: TRect; Peak, MaxPeak: single);
var ClipIndRect, GaugeRect, tmpRect: TRect; tmp: single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect  :=BarRect;
                         ClipIndRect:=Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect  :=Rect(BarRect.Left+FClippingBoxSize+1, BarRect.Top, BarRect.Right, BarRect.Bottom);
                         ClipIndRect:=Rect(BarRect.Left, BarRect.Top, BarRect.Left+FClippingBoxSize, BarRect.Bottom); end;
    scBottomRight: begin GaugeRect  :=Rect(BarRect.Left, BarRect.Top, BarRect.Right-FClippingBoxSize-1, BarRect.Bottom);
                         ClipIndRect:=Rect(BarRect.Right-FClippingBoxSize, BarRect.Top, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp:=min(1, Peak);
  tmpRect:=GaugeRect;
  tmpRect.Left:=Round(tmpRect.Right-(tmpRect.Right-tmpRect.Left)*tmp);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp:=min(1, MaxPeak);
    GaugeRect.Left:=Round(GaugeRect.Right-(GaugeRect.Right-GaugeRect.Left)*tmp);
    DrawMaxLine(GaugeRect.Left, GaugeRect.Top, GaugeRect.Left, GaugeRect.Bottom);
  end;

  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TGuiLevelMeter.DrawSingleBarV(BarRect: TRect; Peak, MaxPeak: single);
var ClipIndRect, GaugeRect, tmpRect: TRect; tmp: single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect  :=BarRect;
                         ClipIndRect:=Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect  :=Rect(BarRect.Left, BarRect.Top+FClippingBoxSize+1, BarRect.Right, BarRect.Bottom);
                         ClipIndRect:=Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Top+FClippingBoxSize); end;
    scBottomRight: begin GaugeRect  :=Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Bottom-FClippingBoxSize-1);
                         ClipIndRect:=Rect(BarRect.Left, BarRect.Bottom-FClippingBoxSize, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp:=min(1, Peak);
  tmpRect:=GaugeRect;
  tmpRect.Top:=Round(tmpRect.Bottom-(tmpRect.Bottom-tmpRect.Top)*tmp);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp:=min(1, MaxPeak);
    GaugeRect.Top:=Round(GaugeRect.Bottom-(GaugeRect.Bottom-GaugeRect.Top)*tmp);
    DrawMaxLine(GaugeRect.Left, GaugeRect.Top, GaugeRect.Right, GaugeRect.Top);
  end;

  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TGuiLevelMeter.DrawSingleBarVI(BarRect: TRect; Peak, MaxPeak: single);
var ClipIndRect, GaugeRect, tmpRect: TRect; tmp: single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect  :=BarRect;
                         ClipIndRect:=Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect  :=Rect(BarRect.Left, BarRect.Top+FClippingBoxSize+1, BarRect.Right, BarRect.Bottom);
                         ClipIndRect:=Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Top+FClippingBoxSize); end;
    scBottomRight: begin GaugeRect  :=Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Bottom-FClippingBoxSize-1);
                         ClipIndRect:=Rect(BarRect.Left, BarRect.Bottom-FClippingBoxSize, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp:=min(1, Peak);
  tmpRect:=GaugeRect;
  tmpRect.Bottom:=Round((tmpRect.Bottom-tmpRect.Top)*tmp + tmpRect.Top);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp:=min(1, MaxPeak);
    GaugeRect.Bottom:=Round((GaugeRect.Bottom-GaugeRect.Top)*tmp + GaugeRect.Top);
    DrawMaxLine(GaugeRect.Left, GaugeRect.Bottom, GaugeRect.Right, GaugeRect.Bottom);
  end;

  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TGuiLevelMeter.RedrawBuffer(doBufferFlip: Boolean);
var CurrentPeak, CurrentMax: single;
    i: integer;
    DestBarRect: TRect;
    SplitSize: single;
    BarPadding: single;
begin
  if (Width>0) and (Height>0) then
  with fBuffer.Canvas do
  begin
    Lock;
    Brush.Color:=Self.Color;

    {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
      FillRect(fBuffer.Canvas.ClipRect);

    if FLevelDirection in [ldirHorizontal, ldirHorizontalInverted] then
      SplitSize   := Height / FPeakEnvFollower.Channels
    else
      SplitSize   := Width / FPeakEnvFollower.Channels;

    BarPadding := (1-FBarWidthPercentage)*SplitSize / 2;

    for i:=0 to FPeakEnvFollower.Channels-1 do
    begin
      CurrentPeak := (FLastMaxPeaks[i]-FLastMinPeaks[i])/2;
      FLastMaxPeaks[i] := 0;
      FLastMinPeaks[i] := 0;

      FPeakEnvFollower.ProcessS(CurrentPeak, i);
      CurrentMax := CurrentPeak;
      FMaxPeakEnvFollower.ProcessS(CurrentMax, i);

      if FLevelDirection in [ldirHorizontal, ldirHorizontalInverted] then
        DestBarRect := Rect(0, Round(splitsize * i + BarPadding), width, Round(splitsize * (i+1)-BarPadding))
      else
        DestBarRect := Rect(Round(splitsize * i+BarPadding), 0, Round(splitsize * (i+1)-BarPadding), height);

      case FLevelDirection of
        ldirHorizontal:         DrawSingleBarH (DestBarRect, CurrentPeak , CurrentMax);
        ldirHorizontalInverted: DrawSingleBarHI(DestBarRect, CurrentPeak , CurrentMax);
        ldmVertical:            DrawSingleBarV (DestBarRect, CurrentPeak , CurrentMax);
        ldmVerticalInverted:    DrawSingleBarVI(DestBarRect, CurrentPeak , CurrentMax);
      end;
    end;

    UnLock;
  end;

  if doBufferFlip then Invalidate;
end;

procedure TGuiLevelMeter.SetBarWidthPercentage(const Value: Single);
begin
  if FBarWidthPercentage <> Value then
  begin
    FBarWidthPercentage := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetClippingFillColor(const Value: TColor);
begin
  if FClippingFillColor <> Value then
  begin
    FClippingFillColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetClippingFillStyle(const Value: TBrushStyle);
begin
  if FClippingFillStyle <> Value then
  begin
    FClippingFillStyle := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetClippingLineColor(const Value: TColor);
begin
  if FClippingLineColor <> Value then
  begin
    FClippingLineColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetClippingLineStyle(const Value: TPenStyle);
begin
  if FClippingLineStyle <> Value then
  begin
    FClippingLineStyle := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetClippingLineWidth(const Value: Integer);
begin
  if FClippingLineWidth <> Value then
  begin
    FClippingLineWidth := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetClippingBoxSize(const Value: Integer);
begin
  if FClippingBoxSize <> Value then
  begin
    FClippingBoxSize := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetDisplayChannels(const Value: Integer);
begin
  if FPeakEnvFollower.Channels <> Value then
  begin
    FPeakEnvFollower.Channels := Value;
    FMaxPeakEnvFollower.Channels := Value;
    ResetPeaks;
  end;
end;

function TGuiLevelMeter.GetDisplayChannels: Integer;
begin
  Result := FPeakEnvFollower.Channels;
end;

procedure TGuiLevelMeter.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetFillStyle(const Value: TBrushStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetLevelDirection(const Value: TGuiLevelDirection);
begin
  if FLevelDirection <> Value then
  begin
    FLevelDirection := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetLineStyle(const Value: TPenStyle);
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetMaxLineColor(const Value: TColor);
begin
  if FMaxLineColor <> Value then
  begin
    FMaxLineColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetMaxLineStyle(const Value: TPenStyle);
begin
  if FMaxLineStyle <> Value then
  begin
    FMaxLineStyle := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetMaxLineWidth(const Value: Integer);
begin
  if FMaxLineWidth <> Value then
  begin
    FMaxLineWidth := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetShowClipping(const Value: TGuiShowClipping);
begin
  if FShowClipping <> Value then
  begin
    FShowClipping := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetShowMaximum(const Value: Boolean);
begin
  if FShowMaximum <> Value then
  begin
    FShowMaximum := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiLevelMeter.SetRedrawInterval(Value: Integer);
begin
  if Value<1 then
    raise Exception.Create('RedrawInterval must greater than 0');

  FPeakEnvFollower.SampleRate := 1000 / Value;
  FMaxPeakEnvFollower.SampleRate := FMaximumTimeFactor * FPeakEnvFollower.SampleRate;

  inherited;
end;

procedure TGuiLevelMeter.SetMaximumTimeFactor(const Value: Single);
begin
  if FMaximumTimeFactor <> value then
  begin
     FMaximumTimeFactor:=value;
     FMaxPeakEnvFollower.SampleRate := FMaximumTimeFactor * FPeakEnvFollower.SampleRate;
  end;
end;

procedure TGuiLevelMeter.ProcessBufferIndirect(NewWaveData: TAVDArrayOfSingleDynArray; Channels, SampleFrames: Integer);
var tmp: TAVDArrayOfSingleDynArray; i: integer;
begin
  SetLength(tmp,Channels, SampleFrames);
  for i:=0 to Channels-1 do
    move(NewWaveData[i,0], tmp[i,0], SampleFrames*SizeOf(Single));

  ProcessBuffer(tmp, SampleFrames);
end;

procedure TGuiLevelMeter.ProcessBuffer(NewWaveData: TAVDSingleDynArray; InpLen: Integer);
var tmp: TAVDArrayOfSingleDynArray;
begin
  SetLength(tmp,1);
  tmp[0] := NewWaveData;
  ProcessBuffer(tmp, InpLen);
end;

procedure TGuiLevelMeter.ProcessBuffer(NewWaveData: TAVDArrayOfSingleDynArray; InpLen: Integer);
var minPeak, maxPeak: TAVDSingleDynArray;
    i: integer;
begin
  SetLength(minPeak, FPeakEnvFollower.Channels);
  SetLength(maxPeak, FPeakEnvFollower.Channels);

  GetPeaks(NewWaveData, minPeak, maxPeak, FPeakEnvFollower.Channels, InpLen);
  for i:=0 to FPeakEnvFollower.Channels-1 do
  begin
    if maxPeak[i]>FLastMaxPeaks[i] then FLastMaxPeaks[i] := maxPeak[i]
    else if minPeak[i]<FLastMinPeaks[i] then FLastMinPeaks[i] := minPeak[i];
  end;
  fTimerMustRedraw:=true;
end;

end.
