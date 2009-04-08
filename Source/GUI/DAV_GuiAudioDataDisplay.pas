unit DAV_GuiAudioDataDisplay;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Controls, Graphics, DAV_GuiCommon, DAV_GuiBaseControl,
  DAV_Common, DAV_AudioData, DAV_GuiAudioDataDisplayCursor,
  DAV_GuiAudioDataDisplayAxis;

type
  TGuiNormalizationType = (ntNone, ntPerChannel, ntOverallChannels);
  TGuiWaveDrawMode = (wdmLine, wdmSolid, wdmPoints, wdmOutline);

  TCustomGuiAudioDataDisplay = class(TCustomControl)
  private
    FAntiAlias            : TGuiAntiAlias;
    FAudioDataCollection  : TCustomAudioDataCollection;
    FBuffer               : TBitmap;
    FDisplayedChannel     : Integer;
    FHalfHeight           : Integer;
    FLineColor            : TColor;
    FLineWidth            : Integer;
    FOSFactor             : Integer;
    FSolidColor           : TColor;
    FTransparent          : Boolean;
    FWaveDrawMode         : TGuiWaveDrawMode;
    FCursor               : TGuiAudioDataDisplayCursor;
    FXAxis                : TGuiAudioDataDisplayXAxis;

    function  GetChannelCount: Integer;
    function  GetSampleFrames: Integer;
    procedure DrawChannelData(Bitmap: TBitmap; Channel: Integer);
    procedure SetAudioDataCollection(const Value: TCustomAudioDataCollection);
    procedure SetDisplayedChannel(Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineWidth(const Value: Integer);
    procedure SetSolidColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWaveDrawMode(Value: TGuiWaveDrawMode);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure RenderDisplayToBitmap(Bitmap: TBitmap);
    procedure RenderCursorsToBitmap(Bitmap: TBitmap);
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);
    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}
    procedure SetCursor(const Value: TGuiAudioDataDisplayCursor);
    procedure CursorChangedHandler(Sender: TObject);
    procedure SetXAxis(const Value: TGuiAudioDataDisplayXAxis);
    procedure AxisChangedHandler(Sender: TObject);
    procedure AudioDataCollectionChanged;
  protected
    procedure Resize; override;
//    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
    procedure Loaded; override;
    procedure RenderBuffer; virtual;

    property SampleFrames: Integer read GetSampleFrames;
    property ChannelCount: Integer read GetChannelCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property AudioDataCollection: TCustomAudioDataCollection read FAudioDataCollection write SetAudioDataCollection;
    property DisplayedChannel: Integer read FDisplayedChannel write SetDisplayedChannel default -1;
    property Cursor: TGuiAudioDataDisplayCursor read FCursor write SetCursor;
    property SolidColor: TColor read FSolidColor write SetSolidColor default clRed;

    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    {$IFNDEF FPC}
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    {$ENDIF}
    property WaveDrawMode: TGuiWaveDrawMode read FWaveDrawMode write SetWaveDrawMode default wdmLine;

    property XAxis: TGuiAudioDataDisplayXAxis read FXAxis write SetXAxis;
  end;

  TGuiAudioDataDisplay = class(TCustomGuiAudioDataDisplay)
  published
    property Align;
    property Anchors;
    property AntiAlias;
    property AudioDataCollection;
    property Color;
    property Constraints;
    property Cursor;
    property DisplayedChannel;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property PopupMenu;
    property LineColor;
    property LineWidth;
    property ShowHint;
    property Visible;
    property WaveDrawMode;
    property XAxis;


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
//    property OnPaint;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

uses
  Math, SysUtils;

constructor TCustomGuiAudioDataDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle        := ControlStyle + [csOpaque];
  FLineColor          := clBlack;
  FSolidColor         := clRed;
  FDisplayedChannel   := -1;
  FOSFactor           := 1;
  FWaveDrawMode       := wdmLine;
  FBuffer             := TBitmap.Create;
  FBuffer.PixelFormat := pf24bit;

  FCursor             := TGuiAudioDataDisplayCursor.Create;
  FCursor.OnChanged   := CursorChangedHandler;

  FXAxis              := TGuiAudioDataDisplayXAxis.Create;
  FXAxis.OnChanged    := AxisChangedHandler;
end;

destructor TCustomGuiAudioDataDisplay.Destroy;
begin
 FreeAndNil(FCursor);
 FreeAndNil(FBuffer);
 FreeAndNil(FXAxis);
 inherited;
end;

procedure TCustomGuiAudioDataDisplay.CursorChangedHandler(Sender: TObject);
begin
 Invalidate;
end;

procedure TCustomGuiAudioDataDisplay.AxisChangedHandler(Sender: TObject);
begin
 Invalidate;
end;

{$IFNDEF FPC}
procedure TCustomGuiAudioDataDisplay.DrawParentImage(Dest: TCanvas);
var
  SaveIndex : Integer;
  DC        : THandle;
  Position  : TPoint;
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
{$ENDIF}

function TCustomGuiAudioDataDisplay.GetSampleFrames: Integer;
begin
 if assigned(FAudioDataCollection)
  then result := FAudioDataCollection.SampleFrames
  else result := 0;
end;

procedure TCustomGuiAudioDataDisplay.Loaded;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
 FHalfHeight    := Height div 2;
end;

function TCustomGuiAudioDataDisplay.GetChannelCount: Integer;
begin
 if assigned(FAudioDataCollection)
  then result := FAudioDataCollection.Channels.Count
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

procedure TCustomGuiAudioDataDisplay.SetAudioDataCollection(const Value: TCustomAudioDataCollection);
begin
 if FAudioDataCollection <> Value then
  begin
   FAudioDataCollection := Value;
   AudioDataCollectionChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.AudioDataCollectionChanged;
begin
 if assigned(FAudioDataCollection) then
  begin
   FXAxis.SetBounds(0, FAudioDataCollection.SampleFrames - 1);
  end;
 Invalidate;
end;

procedure TCustomGuiAudioDataDisplay.SetCursor(const Value: TGuiAudioDataDisplayCursor);
begin
 FCursor.Assign(Value);
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

procedure TCustomGuiAudioDataDisplay.SetSolidColor(const Value: TColor);
begin
 if FSolidColor <> Value then
  begin
   FSolidColor := Value;
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

procedure TCustomGuiAudioDataDisplay.SetXAxis(
  const Value: TGuiAudioDataDisplayXAxis);
begin
 FXAxis.Assign(Value);
end;

(*
procedure TCustomGuiAudioDataDisplay.RedrawBuffer(doBufferFlip: Boolean);
begin
 inherited;
 Invalidate;
end;
*)

procedure TCustomGuiAudioDataDisplay.Resize;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
 FHalfHeight    := Height div 2;
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
begin
 RenderBuffer;
 Canvas.Draw(0, 0, FBuffer);
 inherited;
end;

procedure TCustomGuiAudioDataDisplay.RenderBuffer;
var
  Bmp: TBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    Brush.Assign(Canvas.Brush);

    case FAntiAlias of
     gaaNone:
      begin
       // draw background
       {$IFNDEF FPC}
       if FTransparent
        then DrawParentImage(FBuffer.Canvas)
        else
       {$ENDIF}
        begin
         Brush.Color := Self.Color;
         FillRect(ClipRect);
        end;
       RenderDisplayToBitmap(FBuffer);
      end;
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSFactor * FBuffer.Width;
         Height      := FOSFactor * FBuffer.Height;
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
         FBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;
    end;
    Unlock;
   end;
end;

procedure TCustomGuiAudioDataDisplay.RenderDisplayToBitmap(Bitmap: TBitmap);
var
  ch: Integer;
begin
 if (ChannelCount > 0) and (ChannelCount > FDisplayedChannel) then
  if FDisplayedChannel >= 0
   then DrawChannelData(Bitmap, FDisplayedChannel)
   else
    for ch := 0 to ChannelCount - 1 do DrawChannelData(Bitmap, ch);
 RenderCursorsToBitmap(Bitmap);
end;

procedure TCustomGuiAudioDataDisplay.RenderCursorsToBitmap(Bitmap: TBitmap);
begin
 // yet todo
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
   PixelPerSample := FOSFactor * Self.Width / (SampleFrames - 1);
   Pen.Width := FOSFactor * FLineWidth;
   Pen.Color := FLineColor;

   if (FAudioDataCollection.Channels.Items[Channel] is TAudioChannel32) then
    with TAudioChannel32(FAudioDataCollection.Channels.Items[Channel]) do
     begin
      if SampleCount = 0 then Exit;
      Sample := Cardinal(FXAxis.SampleLower);
      MinVal := ChannelDataPointer^[Sample];
      MaxVal := MinVal;

      MoveTo(0, Round((1 - MinVal) * HlfHght));
      XPixelPosAsInt    := 0;
      XPixelPosAsSingle := 0;
      inc(Sample);

      while Sample < SampleCount do
       begin
        // check for minimum and maximum
        if ChannelDataPointer^[Sample] > MaxVal then MaxVal := ChannelDataPointer^[Sample] else
        if ChannelDataPointer^[Sample] < MinVal then MinVal := ChannelDataPointer^[Sample];

        XPixelPosAsSingle := XPixelPosAsSingle + PixelPerSample;
        if XPixelPosAsSingle > XPixelPosAsInt then
         begin
          XPixelPosAsInt := Round(XPixelPosAsSingle + 0.5);
          if MinVal = MaxVal then LineTo(XPixelPosAsInt, HlfHght * Round((1 - MinVal))) else
           begin
            o := PenPos.Y - HlfHght;
            if abs(o - MinVal * HlfHght) > abs(o - MaxVal * HlfHght)
             then
              begin
               LineTo(XPixelPosAsInt, Round((1 - MinVal) * HlfHght));
               LineTo(XPixelPosAsInt, Round((1 - MaxVal) * HlfHght));
              end
             else
              begin
               LineTo(XPixelPosAsInt, Round((1 - MaxVal) * HlfHght));
               LineTo(XPixelPosAsInt, Round((1 - MinVal) * HlfHght));
              end;
           end;
          MinVal := ChannelDataPointer^[Sample];
          MaxVal := MinVal;
         end;
        inc(Sample);
       end;
     end;
  end;
end;

end.
