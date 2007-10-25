unit DGuiWaveform;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DGuiBaseControl, DAVDCommon;

type
  TGuiNormalizationType = (ntNone, ntPerChannel, ntOverallChannels);
  TGuiWaveDrawMode = (wdmSolid, wdmPoints, wdmOutline);

  TGuiStaticWaveform = class(TGuiBaseControl)
  private
    fNormalizationType:    TGuiNormalizationType;
    fNormalizationFactors: TAVDSingleDynArray;
    fWaveHalfHeight:  Integer;
    fWaveData:        TArrayOfSingleDynArray;
    fWaveVPadding:    Integer;
    fDisplayChannels: Integer;
    fMedianVisible:   Boolean;
    fMedianColor:     TColor;
    fMedianLineWidth: Integer;
    fWaveDrawMode:    TGuiWaveDrawMode;

    procedure SetNormalizationType(Value: TGuiNormalizationType);
    function  GetWaveLength: Integer;
    function  GetWaveChannels: Integer;
    procedure SetWaveVPadding(Value: Integer);
    procedure SetDisplayChannels(Value: Integer);

    procedure SetMedianVisible(Value: Boolean);
    procedure SetMedianColor(Value: TColor);
    procedure SetMedianLineWidth(Value: Integer);
    procedure SetWaveDrawMode(Value: TGuiWaveDrawMode);
  protected
    procedure DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
    procedure ResizeBuffer; override;
    procedure DrawMedian(YOffset: integer);
    procedure DrawGraphs;
    procedure DrawSingleWave(YOffset, HalfHeight, Channel: integer);
    function  GetMaxAmp(Channel: integer): single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   RedrawBuffer(doBufferFlip: Boolean); override;

    procedure SetWaveForm(NewWaveData: TAVDSingleDynArray;  DoRedrawBuffer: Boolean = false; DoFlipBuffer: Boolean = false); overload;
    procedure SetWaveForm(NewWaveData: TArrayOfSingleDynArray; DoRedrawBuffer: Boolean = false; DoFlipBuffer: Boolean = false);overload;
    procedure ClearWaveForm(DoRedrawBuffer: Boolean = false; DoFlipBuffer: Boolean = false);

    property Wavedata: TArrayOfSingleDynArray read fWaveData;
    property WaveLength: Integer read GetWaveLength;
    property WaveChannels: Integer read GetWaveChannels;
  published
    property Transparent;
    property LineWidth;
    property LineColor;
    
    property DisplayChannels: integer read fDisplayChannels write SetDisplayChannels;
    property WaveVPadding: Integer read fWaveVPadding write SetWaveVPadding default 3;

    property MedianVisible: Boolean read fMedianVisible write SetMedianVisible default true;
    property MedianColor: TColor read fMedianColor write SetMedianColor default clRed;
    property MedianLineWidth: Integer read fMedianLineWidth write SetMedianLineWidth default 1;
    property NormalizationType: TGuiNormalizationType read fNormalizationType write SetNormalizationType default ntNone;
    property WaveDrawMode: TGuiWaveDrawMode read fWaveDrawMode write SetWaveDrawMode default wdmSolid;
  end;

implementation

uses Math;

constructor TGuiStaticWaveform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fNormalizationType := ntNone;
  
  fDisplayChannels := 1;
  fWaveVPadding    := 3;
  fMedianVisible   := true;
  fMedianColor     := clRed;
  fMedianLineWidth := 1;
  fWaveDrawMode    := wdmSolid;

  SetLength(fNormalizationFactors,fDisplayChannels); // !IMPORTANT
  ClearWaveForm;
end;

destructor TGuiStaticWaveform.Destroy;
begin
  ClearWaveForm;
  inherited;
end;

procedure TGuiStaticWaveform.ClearWaveForm(DoRedrawBuffer, DoFlipBuffer: Boolean);
var i: integer;
begin
  for i:=0 to Length(fWaveData)-1 do SetLength(fWaveData[i],0);
  SetLength(fWaveData,0);

  if DoRedrawBuffer then RedrawBuffer(DoFlipBuffer);
end;

function TGuiStaticWaveform.GetWaveLength: Integer;
var i: integer;
begin
  result := 0;
  for i:=0 to Length(fWaveData)-1 do
    result:=Max(result, Length(fWavedata[i]));
end;

function TGuiStaticWaveform.GetWaveChannels: Integer;
begin
  result:=Length(fWavedata);
end;

procedure TGuiStaticWaveform.SetDisplayChannels(Value: Integer);
begin
  if Value < 1 then Value:=1;
  if fDisplayChannels<>Value then
  begin
    fDisplayChannels := Value;
    ResizeBuffer;
  end;
end;


procedure TGuiStaticWaveform.SetMedianVisible(Value: Boolean);
begin
  if fMedianVisible<>Value then
  begin
    fMedianVisible := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiStaticWaveform.SetMedianColor(Value: TColor);
begin
  if fMedianColor<>Value then
  begin
    fMedianColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiStaticWaveform.SetMedianLineWidth(Value: Integer);
begin
  if fMedianLineWidth<>Value then
  begin
    fMedianLineWidth := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiStaticWaveform.SetWaveVPadding(Value: Integer);
begin
  if fWaveVPadding<>Value then
  begin
    fWaveVPadding := Value;
    ResizeBuffer;
  end;
end;

procedure TGuiStaticWaveform.SetNormalizationType(Value: TGuiNormalizationType);
begin
  if fNormalizationType<>Value then
  begin
    fNormalizationType := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiStaticWaveform.SetWaveDrawMode(Value: TGuiWaveDrawMode);
begin
  if fWaveDrawMode<>Value then
  begin
    fWaveDrawMode := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiStaticWaveform.ResizeBuffer;
begin
  fWaveHalfHeight := Height div (2*fDisplayChannels) - fWaveVPadding;
  SetLength(fNormalizationFactors,fDisplayChannels);

  inherited;
end;

procedure TGuiStaticWaveform.SetWaveForm(NewWaveData: TAVDSingleDynArray; DoRedrawBuffer, DoFlipBuffer: Boolean);
var len: integer;
begin
  ClearWaveForm;
  SetLength(fWaveData,1);

  len:=Length(NewWaveData);
  SetLength(fWaveData[0], len);
  move(NewWaveData[0],fWaveData[0][0], len * SizeOf(Single));

  if DoRedrawBuffer then RedrawBuffer(DoFlipBuffer);
end;


procedure TGuiStaticWaveform.SetWaveForm(NewWaveData: TArrayOfSingleDynArray; DoRedrawBuffer, DoFlipBuffer: Boolean);
var i, len: integer;
begin
  if Length(NewWaveData) < 1 then
    ClearWaveForm(DoRedrawBuffer, DoFlipBuffer)
  else begin
    ClearWaveForm;
    SetLength(fWaveData,Length(NewWaveData));
    for i := 0 to Length(NewWaveData)-1 do
    begin
      len:=Length(NewWaveData[i]);
      SetLength(fWaveData[i], len);
      move(NewWaveData[i][0],fWaveData[i][0], len * SizeOf(Single));
    end;

    if DoRedrawBuffer then RedrawBuffer(DoFlipBuffer);
  end;
end;

function TGuiStaticWaveform.GetMaxAmp(Channel: integer):single;
var i: integer;
begin
  Result:=0;
  for i:=0 to length(fWaveData[Channel])-1 do Result:=max(Result,abs(fWaveData[Channel][i]));
end;

procedure TGuiStaticWaveform.DrawMedian(YOffset: integer);
begin
  with fBuffer.Canvas do
  begin
    Pen.Width:=fMedianLineWidth;
    Pen.Color:=fMedianColor;

    MoveTo(0,YOffset);
    LineTo(width,YOffset);
  end;
end;

procedure TGuiStaticWaveform.DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
var LastCenter: Integer;
begin
  LastCenter:=(OldMaxPos.Y+OldMinPos.Y) div 2;

  with fBuffer.Canvas do
  begin
    case fWaveDrawMode of
      wdmPoints: begin
             Pixels[NewMax.X, NewMax.Y] := Pen.Color;
             Pixels[NewMin.X, NewMin.Y] := Pen.Color;
           end;

      wdmOutline: begin
             if OldMaxPos.Y=OldMinPos.Y then
             begin
               MoveTo(NewMin.X, NewMin.Y);
               LineTo(OldMinPos.X, OldMinPos.Y);
               if NewMax.Y<>NewMin.Y then LineTo(NewMax.X, NewMax.Y);
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

      else begin
             if abs(NewMax.Y-LastCenter) > abs(NewMin.Y-LastCenter) then
             begin
               LineTo(NewMax.X, NewMax.Y);
             end else begin
               if NewMin.Y<>NewMax.Y then LineTo(NewMax.X, NewMax.Y);
               LineTo(NewMin.X, NewMin.Y);
             end;
           end
    end
  end;

  OldMaxPos:=NewMax;
  OldMinPos:=NewMin;
end;

procedure TGuiStaticWaveform.DrawSingleWave(YOffset, HalfHeight, Channel: integer);
var SampleWidth, COffset, MinSample, MaxSample: single;
  OldMaxPos: TPoint;
  OldMinPos: TPoint;
  COffsetRounded, i: integer;
begin
  with fBuffer.Canvas do
  begin
    Pen.Width:=fLineWidth;
    Pen.Color:=fLineColor;

    SampleWidth:=(width-1) / (WaveLength-1);

    MinSample := fWaveData[Channel][0];
    MaxSample := MinSample;

    COffset:=0;
    COffsetRounded:=0;
    i:=1;
    while i<Length(fWavedata[Channel]) do
    begin
      COffset:=COffset+SampleWidth;
      if (COffset > COffsetRounded) or (i = Length(fWavedata[Channel])-1) then
      begin
        if COffsetRounded=1 then
        begin
          OldMaxPos := Point(0, round(YOffset-MaxSample*fNormalizationFactors[Channel]*HalfHeight));
          OldMinPos := Point(0, round(YOffset-MinSample*fNormalizationFactors[Channel]*HalfHeight));
          MoveTo((OldMinPos.X+OldMaxPos.X) div 2, (OldMinPos.Y+OldMaxPos.Y) div 2);
        end;

        COffsetRounded := ceil(COffset);
        DrawSamples(
          OldMaxPos,
          OldMinPos,
          Point(COffsetRounded-1, round(YOffset-MaxSample*fNormalizationFactors[Channel]*HalfHeight)),
          Point(COffsetRounded-1, round(YOffset-MinSample*fNormalizationFactors[Channel]*HalfHeight)));

        MaxSample := fWaveData[Channel][i];
        MinSample := MaxSample;
      end else begin
        if fWaveData[Channel][i] > MaxSample then
          MaxSample := fWaveData[Channel][i]
        else if fWaveData[Channel][i] < MinSample then
          MinSample := fWaveData[Channel][i];
      end;

      inc(i);
    end;
  end;
end;

procedure TGuiStaticWaveform.DrawGraphs;
var YOffset, i: integer;
begin
  with fBuffer.Canvas do for i:=0 to fDisplayChannels-1 do
  begin
    YOffset := (fWaveVPadding+fWaveHalfHeight) * (i*2+1);

    if fNormalizationFactors[i]>0 then DrawSingleWave(YOffset, fWaveHalfHeight, i);

    if fMedianVisible then DrawMedian(YOffset);
  end;
end;

procedure TGuiStaticWaveform.RedrawBuffer(doBufferFlip: Boolean);
var i: integer; MaxAmp, Amp: single;
begin
  fBuffer.Canvas.Lock;
  fBuffer.Canvas.Brush.Color:=Self.Color;

  {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
      fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);

  MaxAmp := 0;
  if fDisplayChannels<1 then exit;
  for i:=0 to fDisplayChannels-1 do
    if i>=length(fWaveData) then
      fNormalizationFactors[i] := 0
    else if length(fWaveData[i])<1 then
      fNormalizationFactors[i] := 0
    else begin
      Amp := GetMaxAmp(i);
      MaxAmp := Max(MaxAmp, Amp);
      if Amp = 0 then
        fNormalizationFactors[i]:=0
      else
        fNormalizationFactors[i] := 1/Amp;
    end;

  if fNormalizationType = ntNone then
  begin
    for i:=0 to fDisplayChannels-1 do
      if fNormalizationFactors[i]>0 then
        fNormalizationFactors[i] := 1;

  end else if (fNormalizationType = ntOverallChannels) and (MaxAmp > 0) then
  begin 
    for i:=0 to fDisplayChannels-1 do
      if fNormalizationFactors[i]>0 then
        fNormalizationFactors[i] := 1/MaxAmp;
  end;

  DrawGraphs;
  fBuffer.Canvas.UnLock;
  if doBufferFlip then Invalidate;
end;




end.
