unit DGuiAudioDataDisplay;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DGuiBaseControl, DAVDCommon, DAudioData;

type
  TGuiNormalizationType = (ntNone, ntPerChannel, ntOverallChannels);
  TGuiWaveDrawMode = (wdmSolid, wdmPoints, wdmOutline, wdmSimple);

  TCustomGuiAudioDataDisplay = class(TBufferedGraphicControl)
  private
    fAudioData            : TCustomAudioData;
    fDisplayedChannel     : Integer;
    fLineColor            : TColor;
    fLineWidth            : Integer;
    fMedianColor          : TColor;
    fMedianLineWidth      : Integer;
    fMedianVisible        : Boolean;
    fNormalizationType    : TGuiNormalizationType;
    fWaveDrawMode         : TGuiWaveDrawMode;
    fHalfHeight           : Integer;
    fWaveVPadding         : Integer;
    fTransparent          : Boolean;

    function  GetChannelCount: Integer;
    function  GetSampleFrames: Integer;
    procedure DrawChannelData(Channel: Integer);
    procedure SetAudioData(const Value: TCustomAudioData);
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
  protected
    procedure Resize; override;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
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

    property AudioData: TCustomAudioData read fAudioData write SetAudioData;
    property DisplayedChannel: Integer read fDisplayedChannel write SetDisplayedChannel default -1;
    property WaveVPadding: Integer read fWaveVPadding write SetWaveVPadding default 3;

    property LineWidth: Integer read fLineWidth write SetLineWidth default 1;
    property LineColor: TColor read fLineColor write SetLineColor default clBlack;
    {$IFNDEF FPC}
    property Transparent: Boolean read fTransparent write SetTransparent default False;
    {$ENDIF}
    property MedianVisible: Boolean read fMedianVisible write SetMedianVisible default True;
    property MedianColor: TColor read fMedianColor write SetMedianColor default clRed;
    property MedianLineWidth: Integer read fMedianLineWidth write SetMedianLineWidth default 1;
    property NormalizationType: TGuiNormalizationType read fNormalizationType write SetNormalizationType default ntNone;
    property WaveDrawMode: TGuiWaveDrawMode read fWaveDrawMode write SetWaveDrawMode default wdmSolid;
  end;

  TGuiAudioDataDisplay = class(TCustomGuiAudioDataDisplay)
  published
    property Align;
    property Anchors;
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

    property OnCanResize;
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
  Math;

constructor TCustomGuiAudioDataDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fNormalizationType := ntNone;
  fDisplayedChannel  := -1;
  fWaveVPadding      := 3;
  fMedianVisible     := True;
  fMedianColor       := clRed;
  fMedianLineWidth   := 1;
  fWaveDrawMode      := wdmSolid;
end;

destructor TCustomGuiAudioDataDisplay.Destroy;
begin
  inherited;
end;

function TCustomGuiAudioDataDisplay.GetSampleFrames: Integer;
begin
 if assigned(fAudioData)
  then result := fAudioData.SampleFrames
  else result := 0;
end;

function TCustomGuiAudioDataDisplay.GetChannelCount: Integer;
begin
 if assigned(fAudioData)
  then result := fAudioData.Channels.Count
  else result := 0;
end;

procedure TCustomGuiAudioDataDisplay.SetAudioData(const Value: TCustomAudioData);
begin
 if fAudioData <> Value then
  begin
   fAudioData := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetDisplayedChannel(Value: Integer);
begin
 if Value < -1 then Value := -1;
 if fDisplayedChannel <> Value then
  begin
   fDisplayedChannel := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetLineColor(const Value: TColor);
begin
 if fLineColor <> Value then
  begin
   fLineColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetLineWidth(const Value: Integer);
begin
 if fLineWidth <> Value then
  begin
   fLineWidth := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetMedianVisible(Value: Boolean);
begin
 if fMedianVisible <> Value then
  begin
   fMedianVisible := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetMedianColor(Value: TColor);
begin
 if fMedianColor <> Value then
  begin
   fMedianColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetMedianLineWidth(Value: Integer);
begin
 if fMedianLineWidth <> Value then
  begin
   fMedianLineWidth := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetWaveVPadding(Value: Integer);
begin
 if fWaveVPadding <> Value then
  begin
   fWaveVPadding := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetNormalizationType(Value: TGuiNormalizationType);
begin
 if fNormalizationType <> Value then
  begin
   fNormalizationType := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetTransparent(const Value: Boolean);
begin
 if fTransparent <> Value then
  begin
   fTransparent := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetWaveDrawMode(Value: TGuiWaveDrawMode);
begin
 if fWaveDrawMode <> Value then
  begin
   fWaveDrawMode := Value;
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
 fHalfHeight := Height div 2;
end;

// Drawing stuff

procedure TCustomGuiAudioDataDisplay.Paint;
var
  ch: Integer;
begin
 if (Width > 0) and (Height > 0) then
  begin
   fBuffer.Canvas.Lock;
   fBuffer.Canvas.Brush.Assign(Canvas.Brush);

   {$IFNDEF FPC}
   if fTransparent
    then DrawParentImage(fBuffer.Canvas)
    else {$ENDIF} fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);

   if (ChannelCount > 0) and (ChannelCount > fDisplayedChannel) then
    if fDisplayedChannel >= 0 then DrawChannelData(fDisplayedChannel) else
     for ch := 0 to ChannelCount - 1 do DrawChannelData(ch);

   fBuffer.Canvas.Unlock;
  end;
 inherited;
end;


procedure TCustomGuiAudioDataDisplay.DrawChannelData(Channel: Integer);
var
  PixelPerSample     : Single;
  MinVal, MaxVal     : Single;
  Sample             : Cardinal;
  XPixelPosAsSingle  : Single;
  XPixelPosAsInt, o  : Integer;
begin
 with fBuffer.Canvas do
  begin
   Pen.Width := fLineWidth;
   Pen.Color := fLineColor;
   PixelPerSample := Self.Width / SampleFrames;

   if (fAudioData.Channels.Items[Channel] is TAudioChannel32) then
    with TAudioChannel32(fAudioData.Channels.Items[Channel]) do
     begin
      MinVal := ChannelDataPointer^[0];
      MaxVal := MinVal;

      MoveTo(0, Round(MinVal * fHalfHeight + fHalfHeight));
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
          if MinVal = MaxVal then LineTo(XPixelPosAsInt, Round((1 + MinVal) * fHalfHeight)) else
           begin
            o := fBuffer.Canvas.PenPos.Y - fHalfHeight;
            if abs(o - MinVal * fHalfHeight) < abs(o - MaxVal * fHalfHeight)
             then
              begin
               LineTo(XPixelPosAsInt, Round((1 + MinVal) * fHalfHeight));
               LineTo(XPixelPosAsInt, Round((1 + MaxVal) * fHalfHeight));
              end
             else
              begin
               LineTo(XPixelPosAsInt, Round((1 + MaxVal) * fHalfHeight));
               LineTo(XPixelPosAsInt, Round((1 + MinVal) * fHalfHeight));
              end;
           end;
          MinVal := ChannelDataPointer^[Sample];
          MaxVal := MinVal;
         end;
        inc(Sample);
       end;

      if MinVal = MaxVal then LineTo(Self.Width, Round((1 + MinVal) * fHalfHeight)) else
       begin
        o := fBuffer.Canvas.PenPos.Y - fHalfHeight;
        if abs(o - MinVal * fHalfHeight) < abs(o - MaxVal * fHalfHeight)
         then
          begin
           LineTo(Self.Width, Round((1 + MinVal) * fHalfHeight));
           LineTo(Self.Width, Round((1 + MaxVal) * fHalfHeight));
          end
         else
          begin
           LineTo(Self.Width, Round((1 + MaxVal) * fHalfHeight));
           LineTo(Self.Width, Round((1 + MinVal) * fHalfHeight));
          end;
       end;
     end;
  end;
end;

(*
procedure TCustomGuiAudioDataDisplay.DrawMedian(YOffset: Integer);
begin
  with fBuffer.Canvas do
   begin
    Pen.Width := fMedianLineWidth;
    Pen.Color := fMedianColor;

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
    case fWaveDrawMode of
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
    Pen.Width := fLineWidth;
    Pen.Color := fLineColor;

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
     YOffset := (fWaveVPadding + fHalfHeight) * (i * 2 + 1);

     if fNormalizationFactors[i] > 0
      then DrawSingleWave(YOffset, fHalfHeight, i);

     if fMedianVisible then DrawMedian(YOffset);
    end;
end;

procedure TCustomGuiAudioDataDisplay.RedrawBuffer(doBufferFlip: Boolean);
var
  i           : Integer;
  MaxAmp, Amp : Single;
begin
  if (Width > 0) and (Height > 0) then
   begin
    fBuffer.Canvas.Lock;
    fBuffer.Canvas.Brush.Color := Self.Color;

    {$IFNDEF FPC}
    if fTransparent
     then DrawParentImage(fBuffer.Canvas)
     else {$ENDIF} fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);

    MaxAmp := 0;
    if fDisplayChannels < 1 then exit;
    for i := 0 to fDisplayChannels - 1 do
      if i >= Length(fWaveData)
       then fNormalizationFactors[i] := 0 else
      if Length(fWaveData[i]) < 1 then fNormalizationFactors[i] := 0 else
       begin
        Amp := GetMaxAmp(i);
        MaxAmp := Max(MaxAmp, Amp);
        if Amp = 0
         then fNormalizationFactors[i] := 0
         else fNormalizationFactors[i] := 1 / Amp;
      end;

    if fNormalizationType = ntNone then
     begin
      for i := 0 to fDisplayChannels - 1 do
        if fNormalizationFactors[i] > 0 then
          fNormalizationFactors[i] := 1;

     end
    else if (fNormalizationType = ntOverallChannels) and (MaxAmp > 0) then
     begin
      for i := 0 to fDisplayChannels - 1 do
        if fNormalizationFactors[i] > 0 then
          fNormalizationFactors[i] := 1 / MaxAmp;
     end;

    DrawGraphs;
    fBuffer.Canvas.UnLock;
   end;

  if doBufferFlip then Invalidate;
end;
*)

end.
