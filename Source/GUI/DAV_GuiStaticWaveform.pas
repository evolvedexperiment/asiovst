unit DAV_GuiStaticWaveform;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DAV_GuiBaseControl, DAV_Common;

type
  TGuiNormalizationType = (ntNone, ntPerChannel, ntOverallChannels);
  TGuiWaveDrawMode = (wdmSolid, wdmPoints, wdmOutline, wdmSimple);

  TCustomGuiStaticWaveform = class(TCustomGuiBaseMouseControl)
  private
    fNormalizationType    : TGuiNormalizationType;
    fNormalizationFactors : TAVDSingleDynArray;
    fWaveHalfHeight       : Integer;
    fWaveData             : TAVDArrayOfSingleDynArray;
    fWaveVPadding         : Integer;
    fDisplayChannels      : Integer;
    fMedianVisible        : Boolean;
    fMedianColor          : TColor;
    fMedianLineWidth      : Integer;
    fWaveDrawMode         : TGuiWaveDrawMode;

    procedure SetNormalizationType(Value: TGuiNormalizationType);
    function  GetWaveLength: Integer;
    function  GetWaveChannels: Integer;
    procedure SetWaveVPadding(Value: Integer);
    procedure SetDisplayChannels(Value: Integer);

    procedure SetMedianVisible(Value: Boolean);
    procedure SetMedianColor(Value: TColor);
    procedure SetMedianLineWidth(Value: Integer);
    procedure SetWaveDrawMode(Value: TGuiWaveDrawMode);
    procedure SetWaveLength(const Value: Integer);
    procedure SetWaveChannels(const Value: Integer);
  protected
    procedure DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
    procedure ResizeBuffer; override;
    procedure DrawMedian(YOffset: Integer);
    procedure DrawGraphs;
    procedure DrawSingleWave(YOffset, HalfHeight, Channel: Integer);
    function  GetMaxAmp(Channel: Integer): single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;

    procedure SetWaveForm(NewWaveData: TAVDSingleDynArray;  DoRedrawBuffer: Boolean = False; DoFlipBuffer: Boolean = False); overload;
    procedure SetWaveForm(NewWaveData: TAVDArrayOfSingleDynArray; DoRedrawBuffer: Boolean = False; DoFlipBuffer: Boolean = False);overload;
    procedure ClearWaveForm(DoRedrawBuffer: Boolean = False; DoFlipBuffer: Boolean = False);

    property Wavedata: TAVDArrayOfSingleDynArray read fWaveData;
    property WaveLength: Integer read GetWaveLength write SetWaveLength;
    property WaveChannels: Integer read GetWaveChannels write SetWaveChannels;
    property DisplayChannels: Integer read fDisplayChannels write SetDisplayChannels default 2;
    property WaveVPadding: Integer read fWaveVPadding write SetWaveVPadding default 3;

    property MedianVisible: Boolean read fMedianVisible write SetMedianVisible default True;
    property MedianColor: TColor read fMedianColor write SetMedianColor default clRed;
    property MedianLineWidth: Integer read fMedianLineWidth write SetMedianLineWidth default 1;
    property NormalizationType: TGuiNormalizationType read fNormalizationType write SetNormalizationType default ntNone;
    property WaveDrawMode: TGuiWaveDrawMode read fWaveDrawMode write SetWaveDrawMode default wdmSolid;
  end;

  TGuiStaticWaveform = class(TCustomGuiStaticWaveform)
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DisplayChannels;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property LineColor;
    property LineWidth;
    property MedianColor;
    property MedianLineWidth;
    property MedianVisible;
    property NormalizationType;
    property PopupMenu;
    property ShowHint;
    property Transparent;
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
    property OnDragMouseMove;
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

uses Math;

constructor TCustomGuiStaticWaveform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fNormalizationType := ntNone;
  
  fDisplayChannels := 2;
  fWaveVPadding    := 3;
  fMedianVisible   := True;
  fMedianColor     := clRed;
  fMedianLineWidth := 1;
  fWaveDrawMode    := wdmSolid;

  SetLength(fNormalizationFactors, fDisplayChannels); // !IMPORTANT
  ClearWaveForm;
end;

destructor TCustomGuiStaticWaveform.Destroy;
begin
  ClearWaveForm;
  inherited;
end;

procedure TCustomGuiStaticWaveform.ClearWaveForm(DoRedrawBuffer, DoFlipBuffer: Boolean);
begin
  SetLength(fWaveData, 0, 0);

  if DoRedrawBuffer then RedrawBuffer(DoFlipBuffer);
end;

function TCustomGuiStaticWaveform.GetWaveLength: Integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Length(fWaveData) - 1
   do result := Max(result, Length(fWavedata[i]));
end;

function TCustomGuiStaticWaveform.GetWaveChannels: Integer;
begin
  result := Length(fWavedata);
end;

procedure TCustomGuiStaticWaveform.SetDisplayChannels(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if fDisplayChannels <> Value then
   begin
    fDisplayChannels := Value;
    ResizeBuffer;
   end;
end;


procedure TCustomGuiStaticWaveform.SetMedianVisible(Value: Boolean);
begin
  if fMedianVisible <> Value then
  begin
    fMedianVisible := Value;
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiStaticWaveform.SetMedianColor(Value: TColor);
begin
  if fMedianColor <> Value then
  begin
    fMedianColor := Value;
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiStaticWaveform.SetMedianLineWidth(Value: Integer);
begin
  if fMedianLineWidth <> Value then
  begin
    fMedianLineWidth := Value;
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiStaticWaveform.SetWaveVPadding(Value: Integer);
begin
  if fWaveVPadding <> Value then
  begin
    fWaveVPadding := Value;
    ResizeBuffer;
  end;
end;

procedure TCustomGuiStaticWaveform.SetNormalizationType(Value: TGuiNormalizationType);
begin
  if fNormalizationType <> Value then
  begin
    fNormalizationType := Value;
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiStaticWaveform.SetWaveChannels(const Value: Integer);
begin
 if Value <> WaveChannels then
  begin
   SetLength(fWaveData, Value);
  end;
end;

procedure TCustomGuiStaticWaveform.SetWaveDrawMode(Value: TGuiWaveDrawMode);
begin
  if fWaveDrawMode <> Value then
   begin
    fWaveDrawMode := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiStaticWaveform.ResizeBuffer;
begin
  fWaveHalfHeight := Height div (2 * fDisplayChannels) - fWaveVPadding;
  SetLength(fNormalizationFactors, fDisplayChannels);

  inherited;
end;

procedure TCustomGuiStaticWaveform.SetWaveForm(NewWaveData: TAVDSingleDynArray; DoRedrawBuffer, DoFlipBuffer: Boolean);
begin
  ClearWaveForm;
  SetLength(fWaveData, 1);
  fWaveData[0] := NewWaveData;

  if DoRedrawBuffer then RedrawBuffer(DoFlipBuffer);
end;


procedure TCustomGuiStaticWaveform.SetWaveForm(NewWaveData: TAVDArrayOfSingleDynArray; DoRedrawBuffer, DoFlipBuffer: Boolean);
var
  i, len: Integer;
begin
  if Length(NewWaveData) < 1
   then ClearWaveForm(DoRedrawBuffer, DoFlipBuffer)
   else
    begin
     ClearWaveForm;
     SetLength(fWaveData, Length(NewWaveData));
     for i := 0 to Length(NewWaveData) - 1 do
      begin
       len := Length(NewWaveData[i]);
       SetLength(fWaveData[i], len);
       Move(NewWaveData[i][0], fWaveData[i][0], len * SizeOf(Single));
      end;

     if DoRedrawBuffer then RedrawBuffer(DoFlipBuffer);
    end;
end;

procedure TCustomGuiStaticWaveform.SetWaveLength(const Value: Integer);
var
  ch : Integer;
begin
 if Value <> WaveLength then
  begin
   for ch := 0 to Length(fWavedata) - 1 do
   SetLength(fWavedata[ch], Value);
  end;
end;

function TCustomGuiStaticWaveform.GetMaxAmp(Channel: Integer):single;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(fWaveData[Channel]) - 1
   do Result := max(Result, abs(fWaveData[Channel][i]));
end;

procedure TCustomGuiStaticWaveform.DrawMedian(YOffset: Integer);
begin
  with fBuffer.Canvas do
   begin
    Pen.Width := fMedianLineWidth;
    Pen.Color := fMedianColor;

    MoveTo(0, YOffset);
    LineTo(width, YOffset);
   end;
end;

procedure TCustomGuiStaticWaveform.DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
var
  LastCenter: Integer;
begin
  with fBuffer.Canvas do
  begin
    case fWaveDrawMode of
      wdmPoints: begin
             Pixels[NewMax.X, NewMax.Y] := Pen.Color;
             Pixels[NewMin.X, NewMin.Y] := Pen.Color;
           end;

      wdmOutline: begin
             if OldMaxPos.Y = OldMinPos.Y then
             begin
               MoveTo(NewMin.X, NewMin.Y);
               LineTo(OldMinPos.X, OldMinPos.Y);
               if NewMax.Y <> NewMin.Y then LineTo(NewMax.X, NewMax.Y);
             end else if NewMax.Y=NewMin.Y then
             begin
               MoveTo(OldMinPos.X, OldMinPos.Y);
               LineTo(NewMax.X, NewMax.Y);
               LineTo(OldMaxPos.X, OldMaxPos.Y);
             end else begin
               MoveTo(NewMin.X, NewMin.Y);
               LineTo(OldMinPos.X, OldMinPos.Y);
               MoveTo(NewMax.X, NewMax.Y);
               LineTo(OldMaxPos.X, OldMaxPos.Y);
             end;
           end;
      wdmSimple: begin
                  LineTo(OldMaxPos.X, NewMin.Y);
                  LineTo(NewMax.X, NewMax.Y);
                 end;

      else begin
             LastCenter := (OldMaxPos.Y + OldMinPos.Y) div 2;
             if abs(NewMax.Y - LastCenter) < abs(NewMin.Y - LastCenter) then
             begin
               LineTo(NewMax.X, NewMax.Y);
               if NewMin.Y <> NewMax.Y then LineTo(NewMin.X, NewMin.Y);
             end else begin
               LineTo(NewMin.X, NewMin.Y);
               if NewMin.Y <> NewMax.Y then LineTo(NewMax.X, NewMax.Y);
             end;
           end
    end
  end;

  OldMaxPos := NewMax;
  OldMinPos := NewMin;
end;

procedure TCustomGuiStaticWaveform.DrawSingleWave(YOffset, HalfHeight, Channel: Integer);
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

    SampleWidth := (width - 1) / (WaveLength - 1);

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
          OldMaxPos := Point(0, round(YOffset - MaxSample*fNormalizationFactors[Channel]*HalfHeight));
          OldMinPos := Point(0, round(YOffset - MinSample*fNormalizationFactors[Channel]*HalfHeight));
          MoveTo((OldMinPos.X+OldMaxPos.X) div 2, (OldMinPos.Y+OldMaxPos.Y) div 2);
         end;

        COffsetRounded := ceil(COffset);
        DrawSamples(
          OldMaxPos,
          OldMinPos,
          Point(COffsetRounded - 1, round(YOffset - MaxSample*fNormalizationFactors[Channel]*HalfHeight)),
          Point(COffsetRounded - 1, round(YOffset - MinSample*fNormalizationFactors[Channel]*HalfHeight)));

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

procedure TCustomGuiStaticWaveform.DrawGraphs;
var
  YOffset, i: Integer;
begin
  with fBuffer.Canvas do
   for i := 0 to fDisplayChannels - 1 do
    begin
     YOffset := (fWaveVPadding + fWaveHalfHeight) * (i * 2 + 1);

     if fNormalizationFactors[i] > 0
      then DrawSingleWave(YOffset, fWaveHalfHeight, i);

     if fMedianVisible then DrawMedian(YOffset);
    end;
end;

procedure TCustomGuiStaticWaveform.RedrawBuffer(doBufferFlip: Boolean);
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

end.
