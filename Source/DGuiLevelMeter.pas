unit DGuiLevelMeter;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DGuiBaseControl, DAVDCommon, DDspEnvelopeFollower;

type
  TGuiLevelDirection = (ldirHorizontal, ldmVertical);
  TGuiShowClipping = (scNo, scTop, scBottom);
  
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
    FMaxEnvFollower     : TDspEnvelopeFollower;
    FMaximumTimeFactor  : Single;

    FLastPeaks: TAVDSingleDynArray;
    FLastMaxPeaks: TAVDSingleDynArray;

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
    procedure SetMaximumTimeFactor(const Value: Single);
    procedure SetLevelAttack(const Value: Single);
    procedure SetLevelRelease(const Value: Single);
    procedure SetLevelDirection(const Value: TGuiLevelDirection);
    procedure SetLineStyle(const Value: TPenStyle);
    procedure SetMaxLineColor(const Value: TColor);
    procedure SetMaxLineStyle(const Value: TPenStyle);
    procedure SetMaxLineWidth(const Value: Integer);
    procedure SetShowClipping(const Value: TGuiShowClipping);
    procedure SetShowMaximum(const Value: Boolean);
    procedure SetSampleRate(const Value: Single);

    function GetSampleRate: Single;
    function GetLevelAttack: Single;
    function GetLevelRelease: Single;
    function GetDisplayChannels: Integer;
  protected
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
    property ShowClipping: TGuiShowClipping read FShowClipping write SetShowClipping default scTop;

    property SampleRate: single read GetSampleRate write SetSampleRate;
    property MaximumTimeFactor: Single read FMaximumTimeFactor write SetMaximumTimeFactor;
    property LevelAttack: Single read GetLevelAttack write SetLevelAttack;
    property LevelRelease: Single read GetLevelRelease write SetLevelRelease;

    property LevelDirection: TGuiLevelDirection read FLevelDirection write SetLevelDirection default ldmVertical;
    property DisplayChannels: Integer read GetDisplayChannels write SetDisplayChannels default 2;
    property BarWidthPercentage: Single read FBarWidthPercentage write SetBarWidthPercentage;
  end;
  
implementation

uses SysUtils,
    {$IFDEF PUREPASCAL}DAVDBufferMathPascal{$ELSE}DAVDBufferMathAsm{$ENDIF};

{ TGuiLevelMeter }

constructor TGuiLevelMeter.Create(AOwner: TComponent);
begin    
  inherited;

  FPeakEnvFollower:=TDspEnvelopeFollower.Create(self);
  FMaxEnvFollower:=TDspEnvelopeFollower.Create(self);

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
  FShowClipping      := scTop;

  FLevelDirection    := ldmVertical;
  FPeakEnvFollower.Channels := 2;
  FMaxEnvFollower.Channels := 2;

  FMaximumTimeFactor := 10;
  FBarWidthPercentage:= 0.8;

  ResetPeaks;

  fRedrawTimer.Interval := 30;
  SampleRate         := 44100;
  LevelAttack        := 0;
  LevelRelease       := 0;
end;

destructor TGuiLevelMeter.Destroy;
begin
  FMaxEnvFollower.Free;
  FPeakEnvFollower.Free;
  SetLength(FLastPeaks, 0);
  SetLength(FLastMaxPeaks, 0);
  inherited;
end;

procedure TGuiLevelMeter.ResetPeaks;
begin
  setlength(FLastPeaks, FPeakEnvFollower.Channels);
  setlength(FLastMaxPeaks, FPeakEnvFollower.Channels);
  FillChar(FLastPeaks[0], SizeOf(Single)*FPeakEnvFollower.Channels, 0);
  FillChar(FLastMaxPeaks[0], SizeOf(Single)*FPeakEnvFollower.Channels, 0);
  RedrawBuffer(true);
end;


procedure TGuiLevelMeter.SetLevelAttack(const Value: Single);
begin
  FPeakEnvFollower.Attack := Value;
  FMaxEnvFollower.Attack := Value;
end;

procedure TGuiLevelMeter.SetLevelRelease(const Value: Single);
begin
  FPeakEnvFollower.Release := Value;
  FMaxEnvFollower.Release := Value;
end;

function TGuiLevelMeter.GetLevelAttack: Single;
begin
  Result := FPeakEnvFollower.Attack
end;

function TGuiLevelMeter.GetLevelRelease: Single;
begin
  Result := FPeakEnvFollower.Release
end;

procedure TGuiLevelMeter.SetMaximumTimeFactor(const Value: Single);
begin
  FMaximumTimeFactor := Value;
  FMaxEnvFollower.SampleRate := FMaximumTimeFactor * FPeakEnvFollower.SampleRate;
end;

procedure TGuiLevelMeter.SetSampleRate(const Value: Single);
begin
  FPeakEnvFollower.SampleRate := Value;
  FMaxEnvFollower.SampleRate := FMaximumTimeFactor * FPeakEnvFollower.SampleRate;
end;

function TGuiLevelMeter.GetSampleRate: Single;
begin
  Result := FPeakEnvFollower.SampleRate
end;

procedure TGuiLevelMeter.RedrawBuffer(doBufferFlip: Boolean);
begin
  if (Width>0) and (Height>0) then
  begin
    fBuffer.Canvas.Lock;
    fBuffer.Canvas.Brush.Color:=Self.Color;

    {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
      fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);

//    fBuffer.Canvas.TextOut(0,0, FloatToStr(fLastPeaks[0]));
    fBuffer.Canvas.Rectangle(0,0,width, round(height*fLastPeaks[0]));

    fBuffer.Canvas.UnLock;
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
    FMaxEnvFollower.Channels := Value;
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

  inherited;
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
var //tmpbuf: TAVDArrayOfSingleDynArray;
    dummy: TAVDSingleDynArray;
begin
  SetLength(dummy, FPeakEnvFollower.Channels);
  FPeakEnvFollower.ProcessSAA(NewWaveData);
  GetPeaks(NewWaveData, dummy, FLastPeaks, FPeakEnvFollower.Channels, InpLen);
//  tmpbuf := FMaxEnvFollower.ProcessSAA(NewWaveData, InpLen);
//  GetPeaks(tmpbuf, dummy, FLastMaxPeaks, FPeakEnvFollower.Channels, InpLen);

  fTimerMustRedraw:=true;
end;

end.
