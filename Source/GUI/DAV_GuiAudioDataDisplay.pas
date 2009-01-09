unit DAV_GuiAudioDataDisplay;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Controls, Graphics, DAV_GuiBaseControl, DAV_Common, DAV_AudioData;

type
  TGuiNormalizationType = (ntNone, ntPerChannel, ntOverallChannels);
  TGuiWaveDrawMode = (wdmSolid, wdmPoints, wdmOutline, wdmSimple);

  TCustomGuiAudioDataDisplay = class(TBufferedGraphicControl)
  private
    FAntiAlias            : TGuiAntiAlias;
    FAudioData            : TCustomAudioDataCollection;
    FDisplayedChannel     : Integer;
    FLineColor            : TColor;
    FLineWidth            : Integer;
    FMedianColor          : TColor;
    FMedianLineWidth      : Integer;
    FMedianVisible        : Boolean;
    FNormalizationType    : TGuiNormalizationType;
    FOSFactor             : Integer;
    FWaveDrawMode         : TGuiWaveDrawMode;
    FHalfHeight           : Integer;
    FTransparent          : Boolean;
    FWaveVPadding         : Integer;

    function  GetChannelCount: Integer;
    function  GetSampleFrames: Integer;
    procedure DrawChannelData(Bitmap: TBitmap; Channel: Integer);
    procedure SetAudioData(const Value: TCustomAudioDataCollection);
    procedure SetDisplayedChannel(Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineWidth(const Value: Integer);
    procedure SetMedianColor(Value: TColor);
    procedure SetMedianLineWidth(Value: Integer);
    procedure SetMedianVisible(Value: Boolean);
    procedure SetNormalizationType(Value: TGuiNormalizationType);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWaveDrawMode(Value: TGuiWaveDrawMode);
    procedure SetWaveVPadding(Value: Integer);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure RenderDisplayToBitmap(Bitmap: TBitmap);
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);
  protected
    procedure Resize; override;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
    procedure Loaded; override;
(*
    procedure DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
    procedure DrawMedian(YOffset: Integer);
    procedure DrawGraphs;
    procedure DrawSingleWave(YOffset, HalfHeight, Channel: Integer);
*)

    property SampleFrames: Integer read GetSampleFrames;
    property ChannelCount: Integer read GetChannelCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property AudioData: TCustomAudioDataCollection read FAudioData write SetAudioData;
    property DisplayedChannel: Integer read FDisplayedChannel write SetDisplayedChannel default -1;
    property WaveVPadding: Integer read FWaveVPadding write SetWaveVPadding default 3;

    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    {$IFNDEF FPC}
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    {$ENDIF}
    property MedianVisible: Boolean read FMedianVisible write SetMedianVisible default True;
    property MedianColor: TColor read FMedianColor write SetMedianColor default clRed;
    property MedianLineWidth: Integer read FMedianLineWidth write SetMedianLineWidth default 1;
    property NormalizationType: TGuiNormalizationType read FNormalizationType write SetNormalizationType default ntNone;
    property WaveDrawMode: TGuiWaveDrawMode read FWaveDrawMode write SetWaveDrawMode default wdmSolid;
  end;

  TGuiAudioDataDisplay = class(TCustomGuiAudioDataDisplay)
  published
    property Align;
    property Anchors;
    property AntiAlias;
    property AudioData;
    property Color;
    property Constraints;
    property DisplayedChannel;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property MedianColor;
    property MedianLineWidth;
    property MedianVisible;
    property NormalizationType;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property WaveDrawMode;
    property WaveVPadding;

    {$IFNDEF FPC}
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
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

uses
  Math, SysUtils;

constructor TCustomGuiAudioDataDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNormalizationType := ntNone;
  FDisplayedChannel  := -1;
  FWaveVPadding      := 3;
  FMedianVisible     := True;
  FMedianColor       := clRed;
  FMedianLineWidth   := 1;
  FOSFactor          :=  1; 
  FWaveDrawMode      := wdmSolid;
end;

destructor TCustomGuiAudioDataDisplay.Destroy;
begin
  inherited;
end;

function TCustomGuiAudioDataDisplay.GetSampleFrames: Integer;
begin
 if assigned(FAudioData)
  then result := FAudioData.SampleFrames
  else result := 0;
end;

procedure TCustomGuiAudioDataDisplay.Loaded;
begin
 inherited;
 FHalfHeight := Height div 2;
end;

function TCustomGuiAudioDataDisplay.GetChannelCount: Integer;
begin
 if assigned(FAudioData)
  then result := FAudioData.Channels.Count
  else result := 0;
end;

procedure TCustomGuiAudioDataDisplay.SetAntiAlias(const Value: TGuiAntiAlias);
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
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetAudioData(const Value: TCustomAudioDataCollection);
begin
 if FAudioData <> Value then
  begin
   FAudioData := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetDisplayedChannel(Value: Integer);
begin
 if Value < -1 then Value := -1;
 if FDisplayedChannel <> Value then
  begin
   FDisplayedChannel := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetLineColor(const Value: TColor);
begin
 if FLineColor <> Value then
  begin
   FLineColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetLineWidth(const Value: Integer);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetMedianVisible(Value: Boolean);
begin
 if FMedianVisible <> Value then
  begin
   FMedianVisible := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetMedianColor(Value: TColor);
begin
 if FMedianColor <> Value then
  begin
   FMedianColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetMedianLineWidth(Value: Integer);
begin
 if FMedianLineWidth <> Value then
  begin
   FMedianLineWidth := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetWaveVPadding(Value: Integer);
begin
 if FWaveVPadding <> Value then
  begin
   FWaveVPadding := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetNormalizationType(Value: TGuiNormalizationType);
begin
 if FNormalizationType <> Value then
  begin
   FNormalizationType := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetWaveDrawMode(Value: TGuiWaveDrawMode);
begin
 if FWaveDrawMode <> Value then
  begin
   FWaveDrawMode := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.RedrawBuffer(doBufferFlip: Boolean);
begin
 inherited;
 Invalidate;
end;

procedure TCustomGuiAudioDataDisplay.Resize;
begin
 inherited;
 FHalfHeight := Height div 2;
end;

// Drawing stuff

procedure TCustomGuiAudioDataDisplay.UpsampleBitmap(Bitmap: TBitmap);
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

procedure TCustomGuiAudioDataDisplay.DownsampleBitmap(Bitmap: TBitmap);
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

procedure TCustomGuiAudioDataDisplay.Paint;
var
  Bmp: TBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with fBuffer.Canvas do
   begin
    Lock;
    Brush.Assign(Canvas.Brush);

    case FAntiAlias of
     gaaNone     :
      begin
       // draw background
       {$IFNDEF FPC}
       if FTransparent
        then DrawParentImage(fBuffer.Canvas)
        else
       {$ENDIF}
        begin
         Brush.Color := Self.Color;
         FillRect(ClipRect);
        end;
       RenderDisplayToBitmap(fBuffer);
      end;
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSFactor * fBuffer.Width;
         Height      := FOSFactor * fBuffer.Height;
         {$IFNDEF FPC}
         if FTransparent then
          begin
           DrawParentImage(Bmp.Canvas);
           UpsampleBitmap(Bmp);
          end
         else
         {$ENDIF}
          with Bmp.Canvas do
           begin
            Brush.Color := Self.Color;
            FillRect(ClipRect);
           end;
         Bmp.Canvas.FillRect(ClipRect);
         RenderDisplayToBitmap(Bmp);
         DownsampleBitmap(Bmp);
         fBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;
    end;
    Unlock;
   end;
 inherited;
end;

procedure TCustomGuiAudioDataDisplay.RenderDisplayToBitmap(Bitmap: TBitmap);
var
  ch: Integer;
begin
 if (ChannelCount > 0) and (ChannelCount > FDisplayedChannel) then
  if FDisplayedChannel >= 0 then DrawChannelData(Bitmap, FDisplayedChannel) else
   for ch := 0 to ChannelCount - 1 do DrawChannelData(Bitmap, ch);
end;

procedure TCustomGuiAudioDataDisplay.DrawChannelData(Bitmap: TBitmap; Channel: Integer);
var
  PixelPerSample     : Single;
  MinVal, MaxVal     : Single;
  Sample             : Cardinal;
  HlfHght            : Integer;
  XPixelPosAsSingle  : Single;
  XPixelPosAsInt, o  : Integer;
begin
 with Bitmap.Canvas do
  begin
   if SampleFrames = 0 then exit;
   HlfHght := FOSFactor * FHalfHeight;
   PixelPerSample := FOSFactor * Self.Width / SampleFrames;
   Pen.Width := FOSFactor * FLineWidth;
   Pen.Color := FLineColor;

   if (FAudioData.Channels.Items[Channel] is TAudioChannel32) then
    with TAudioChannel32(FAudioData.Channels.Items[Channel]) do
     begin
      if SampleCount = 0 then Exit;
      MinVal := ChannelDataPointer^[0];
      MaxVal := MinVal;

      MoveTo(0, Round(MinVal * HlfHght + HlfHght));
      Sample            := 1;
      XPixelPosAsInt    := 0;
      XPixelPosAsSingle := 0;

      while Sample < SampleCount do
       begin
        // check for minimum and maximum
        if ChannelDataPointer^[Sample] > MaxVal then MaxVal := ChannelDataPointer^[Sample] else
        if ChannelDataPointer^[Sample] < MinVal then MinVal := ChannelDataPointer^[Sample];

        XPixelPosAsSingle := XPixelPosAsSingle + PixelPerSample;
        if XPixelPosAsSingle > XPixelPosAsInt then
         begin
          XPixelPosAsInt := Round(XPixelPosAsSingle);
          if MinVal = MaxVal then LineTo(XPixelPosAsInt, HlfHght * Round((1 + MinVal))) else
           begin
            o := PenPos.Y - HlfHght;
            if abs(o - MinVal * HlfHght) < abs(o - MaxVal * HlfHght)
             then
              begin
               LineTo(XPixelPosAsInt, Round((1 + MinVal) * HlfHght));
               LineTo(XPixelPosAsInt, Round((1 + MaxVal) * HlfHght));
              end
             else
              begin
               LineTo(XPixelPosAsInt, Round((1 + MaxVal) * HlfHght));
               LineTo(XPixelPosAsInt, Round((1 + MinVal) * HlfHght));
              end;
           end;
          MinVal := ChannelDataPointer^[Sample];
          MaxVal := MinVal;
         end;
        inc(Sample);
       end;

(*
      // I have no idea what this was for ?!
      if MinVal = MaxVal then LineTo(Self.Width, HlfHght * Round((1 + MinVal))) else
       begin
        o := PenPos.Y - HlfHght;
        if abs(o - MinVal * HlfHght) < abs(o - MaxVal * HlfHght)
         then
          begin
           LineTo(Self.Width, Round((1 + MinVal) * HlfHght));
           LineTo(Self.Width, Round((1 + MaxVal) * HlfHght));
          end
         else
          begin
           LineTo(Self.Width, Round((1 + MaxVal) * HlfHght));
           LineTo(Self.Width, Round((1 + MinVal) * HlfHght));
          end;
       end;
*)
     end;
  end;
end;

(*
procedure TCustomGuiAudioDataDisplay.DrawMedian(YOffset: Integer);
begin
  with fBuffer.Canvas do
   begin
    Pen.Width := FMedianLineWidth;
    Pen.Color := FMedianColor;

    MoveTo(0, YOffset);
    LineTo(Width, YOffset);
   end;
end;

procedure TCustomGuiAudioDataDisplay.DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
var
  LastCenter: Integer;
begin
  with fBuffer.Canvas do
  begin
    case FWaveDrawMode of
      wdmPoints:
        begin
         Pixels[NewMax.X, NewMax.Y] := Pen.Color;
         Pixels[NewMin.X, NewMin.Y] := Pen.Color;
        end;

      wdmOutline:
        begin
         if OldMaxPos.Y = OldMinPos.Y then
          begin
           MoveTo(NewMin.X, NewMin.Y);
           LineTo(OldMinPos.X, OldMinPos.Y);
           if NewMax.Y <> NewMin.Y then LineTo(NewMax.X, NewMax.Y);
          end
         else if NewMax.Y = NewMin.Y then
          begin
           MoveTo(OldMinPos.X, OldMinPos.Y);
           LineTo(NewMax.X, NewMax.Y);
           LineTo(OldMaxPos.X, OldMaxPos.Y);
          end
         else
          begin
           MoveTo(NewMin.X, NewMin.Y);
           LineTo(OldMinPos.X, OldMinPos.Y);
           MoveTo(NewMax.X, NewMax.Y);
           LineTo(OldMaxPos.X, OldMaxPos.Y);
          end;
        end;
      wdmSimple:
        begin
         LineTo(OldMaxPos.X, NewMin.Y);
         LineTo(NewMax.X, NewMax.Y);
        end;

      else
        begin
         LastCenter := (OldMaxPos.Y + OldMinPos.Y) div 2;
         if abs(NewMax.Y - LastCenter) < abs(NewMin.Y - LastCenter) then
          begin
           LineTo(NewMax.X, NewMax.Y);
           if NewMin.Y <> NewMax.Y then LineTo(NewMin.X, NewMin.Y);
          end
         else
          begin
           LineTo(NewMin.X, NewMin.Y);
           if NewMin.Y <> NewMax.Y then LineTo(NewMax.X, NewMax.Y);
          end;
        end
    end
  end;

  OldMaxPos := NewMax;
  OldMinPos := NewMin;
end;

procedure TCustomGuiAudioDataDisplay.DrawSingleWave(YOffset, HalfHeight, Channel: Integer);
var
  SampleWidth, COffset  : Single;
  MinSample, MaxSample  : Single;
  OldMaxPos             : TPoint;
  OldMinPos             : TPoint;
  COffsetRounded, i     : Integer;
begin
  with fBuffer.Canvas do
   begin
    Pen.Width := FLineWidth;
    Pen.Color := FLineColor;

    SampleWidth := (Width - 1) / (WaveLength - 1);

    MinSample := fWaveData[Channel][0];
    MaxSample := MinSample;

    COffset := 0;
    COffsetRounded := 1;
    i := 1;
    while i<Length(fWavedata[Channel]) do
     begin
      COffset := COffset+SampleWidth;
      if (COffset > COffsetRounded) or (i = Length(fWavedata[Channel]) - 1) then
       begin
        if COffsetRounded = 1 then
         begin
          OldMaxPos := Point(0, Round(YOffset - MaxSample*fNormalizationFactors[Channel]*HalfHeight));
          OldMinPos := Point(0, Round(YOffset - MinSample*fNormalizationFactors[Channel]*HalfHeight));
          MoveTo((OldMinPos.X + OldMaxPos.X) div 2,
                 (OldMinPos.Y + OldMaxPos.Y) div 2);
         end;

        COffsetRounded := ceil(COffset);
        DrawSamples(
          OldMaxPos,
          OldMinPos,
          Point(COffsetRounded - 1, Round(YOffset - MaxSample*fNormalizationFactors[Channel]*HalfHeight)),
          Point(COffsetRounded - 1, Round(YOffset - MinSample*fNormalizationFactors[Channel]*HalfHeight)));

        MaxSample := fWaveData[Channel][i];
        MinSample := MaxSample;
       end
      else
       begin
        if fWaveData[Channel][i] > MaxSample
         then MaxSample := fWaveData[Channel][i] else
        if fWaveData[Channel][i] < MinSample
         then MinSample := fWaveData[Channel][i];
       end;

      inc(i);
    end;
  end;
end;

procedure TCustomGuiAudioDataDisplay.DrawGraphs;
var
  YOffset, i: Integer;
begin
  with fBuffer.Canvas do
   for i := 0 to fDisplayChannels - 1 do
    begin
     YOffset := (FWaveVPadding + FHalfHeight) * (i * 2 + 1);

     if fNormalizationFactors[i] > 0
      then DrawSingleWave(YOffset, FHalfHeight, i);

     if FMedianVisible then DrawMedian(YOffset);
    end;
end;
*)

end.
