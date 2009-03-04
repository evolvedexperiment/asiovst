unit DAV_GuiEQGraph;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls;

const
  fkt = 0.35355339;

type
  TGuiEQGraph = class;
  TFrequencyAxisLabelStyle = (flsNone, flsTop, flsBottom);
  TdBLabelStyle = (dlsNone, dlsLeft, dlsRight);

  TGetFilterGainEvent = function(Sender: TObject; const Frequency: Single): Single of object;
  TGuiEQGraph = class(TCustomControl)
  private
    FAutoColor               : Boolean;
    FBuffer                  : TBitmap;
    FChartBuffer             : TBitmap;
    FChartColor              : TColor;
    FBorderRadius            : Integer;
    FBorderWidth             : Integer;
    FdBLabelStyle            : TdBLabelStyle;
    FFreqLabelStyle          : TFrequencyAxisLabelStyle;
    FGraphColorDark          : TColor;
    FGraphColorLight         : TColor;
    FMaxGain                 : Single;
    FChartBufferNeedsRepaint : Boolean;
    FBufferNeedsRepaint      : Boolean;
    FOnPaint                 : TNotifyEvent;
    FOnGetFilterGain         : TGetFilterGainEvent;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetChartColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetdBLabelStyle(const Value: TdBLabelStyle);
    procedure SetFrequencyAxisLabelStyle(const Value: TFrequencyAxisLabelStyle);
    procedure SetGraphColorDark(const Value: TColor);
    procedure SetGraphColorLight(const Value: TColor);
    procedure SetMaxGain(const Value: Single);
    procedure SetBorderWidth(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure RepaintBuffer; virtual;
    procedure RepaintChartBuffer; virtual;
    procedure MaxGainChanged; virtual;
    {$IFDEF FPC}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
    property ChartBufferNeedsRepaint: Boolean read FChartBufferNeedsRepaint write FChartBufferNeedsRepaint;
    property BufferNeedsRepaint: Boolean read FBufferNeedsRepaint write FBufferNeedsRepaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoColor: Boolean read FAutoColor write SetAutoColor default true;
    property GraphColorDark: TColor read FGraphColorDark write SetGraphColorDark default $303030;
    property GraphColorLight: TColor read FGraphColorLight write SetGraphColorLight default $606060;
    property ColorChart: TColor read FChartColor write SetChartColor;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property MaxGain : Single read FMaxGain write SetMaxGain;
    property FrequencyAxisLabelStyle: TFrequencyAxisLabelStyle read FFreqLabelStyle write SeTFrequencyAxisLabelStyle default flsNone;
    property dBLabelStyle: TdBLabelStyle read FdBLabelStyle write SetdBLabelStyle;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnGetFilterGain: TGetFilterGainEvent read FOnGetFilterGain write FOnGetFilterGain;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
  end;

implementation

uses
  Math, DAV_Common;

{ TGuiEQGraph }

constructor TGuiEQGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle      := [csAcceptsControls, csCaptureMouse, csClickEvents,
                       csDoubleClicks, csReplicatable, csOpaque];
 TabStop          := False; // Ensure we're not a tab-stop
 FMaxGain         := 15;
 FAutoColor       := False;
 FBuffer          := TBitmap.Create;
 FChartBuffer     := TBitmap.Create;
 FGraphColorLight := $606060;
 FGraphColorDark  := $303030;
 FFreqLabelStyle  := flsNone;
 FdBLabelStyle    := dlsNone;
 FBorderWidth     := 1;
 Color            := clBtnFace;
 FChartColor      := Color;
end;

destructor TGuiEQGraph.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FChartBuffer);
 inherited Destroy;
end;

{$IFNDEF FPC}
procedure TGuiEQGraph.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TGuiEQGraph.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 FChartBuffer.Canvas.Font.Assign(Font);
 FBuffer.Canvas.Font.Assign(Font);
 ChartBufferNeedsRepaint := True;
end;

procedure TGuiEQGraph.Paint;
begin
 inherited;
 if FChartBufferNeedsRepaint then RepaintChartBuffer;
 if FBufferNeedsRepaint then RepaintBuffer;
 Canvas.Draw(0, 0, FBuffer);
 if assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TGuiEQGraph.SetChartColor(const Value: TColor);
begin
 if not FAutoColor and (FChartColor <> Value) then
  begin
   FChartColor := Value;
   ChartBufferNeedsRepaint := True;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   ChartBufferNeedsRepaint := True;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   ChartBufferNeedsRepaint := True;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
   if FAutoColor then
    begin
(*
     FChartColor32 := Lighten(Color32(Color),60);
     FChartColor := WinColor(FChartColor32);
*)
     RepaintChartBuffer;
    end;
  end;
end;

procedure TGuiEQGraph.RepaintBuffer;
begin
 with FBuffer, Canvas do
  begin
   Lock;
   FBufferNeedsRepaint := False;
   Draw(0, 0, FChartBuffer);
   Unlock;
  end;
end;

procedure TGuiEQGraph.RepaintChartBuffer;
var
  i, j, w, h : Integer;
  rct        : TRect;
  HHOffs     : Integer;
  HalfHeight : Single;
  NormFactor : Single;
  Temp       : array [0..1] of Single;
  Wdth       : Integer;
begin
 if ((csLoading in ComponentState) and not (csDesigning in ComponentState))
   or (csDestroying in ComponentState) then exit;
 with FChartBuffer, Canvas do
  if FChartBufferNeedsRepaint or (csDesigning in ComponentState) then
   begin
    Lock;

    FChartBufferNeedsRepaint := False;
    {$IFDEF Delphi10_Up}
    with Margins
     do rct := Rect(Left, Top, Width - Right, Height - Bottom);
    {$ELSE}
    rct := Rect(0, 0, Width, Height);
    {$ENDIF}

    Brush.Color := Color;
    Brush.Style := bsSolid;
    FillRect(ClientRect);
    Brush.Color := FChartColor;

    Pen.Width := 1;
    InflateRect(rct, -1, -1);

    // calculate the real half height of the visible area (without the margin)
    HalfHeight := (rct.Bottom - rct.Top) * 0.5;
    NormFactor := HalfHeight / FMaxGain;

    // add top margin to half height as offset
    HHOffs := round(rct.Top + HalfHeight);
    Wdth := round(rct.Right - rct.Left);

    // draw middle line
    Pen.Color := FGraphColorDark;
    MoveTo(rct.Left, HHOffs);
    LineTo(rct.Right, HHOffs);

    Pen.Color := FGraphColorLight;
    if FMaxGain <=  5 then w := 1 else
    if FMaxGain <= 10 then w := 5 else
    if FMaxGain >= 180 then w := 45 else w := 10;
    i := w;
    while i * NormFactor < HalfHeight do
     begin
      MoveTo(rct.Left,  round(rct.Top + HalfHeight - i * NormFactor));
      LineTo(rct.Right, round(rct.Top + HalfHeight - i * NormFactor));

      MoveTo(rct.Left,  round(rct.Top + HalfHeight + i * NormFactor));
      LineTo(rct.Right, round(rct.Top + HalfHeight + i * NormFactor));

      inc(i, w);
     end;

    i := 10; j := 3;
    while j * i < 20000 do
     begin
      w := round(rct.Left + FreqLogToLinear(j * i)    * Wdth);
      MoveTo(w, rct.Top); LineTo(w, rct.Bottom);
      inc(j);
      if j >= 10 then
       begin
        i := i * 10;
        j := 1;
       end;
     end;

    Font.Assign(Self.Font);
    case FFreqLabelStyle of
     flsBottom :
      begin
       h := rct.Bottom + Font.Height - 2 - FBorderWidth div 2;
       TextOut(Round(rct.Left + FreqLogToLinear(30)    * Wdth) - 12, h, '30Hz');
       TextOut(Round(rct.Left + FreqLogToLinear(50)    * Wdth) - 12, h, '50Hz');
       TextOut(Round(rct.Left + FreqLogToLinear(100)   * Wdth) - 12, h, '100Hz');
       TextOut(Round(rct.Left + FreqLogToLinear(200)   * Wdth) - 12, h, '200Hz');
       TextOut(Round(rct.Left + FreqLogToLinear(500)   * Wdth) - 12, h, '500Hz');
       TextOut(Round(rct.Left + FreqLogToLinear(1000)  * Wdth) - 10, h, '1kHz');
       TextOut(Round(rct.Left + FreqLogToLinear(2000)  * Wdth) - 10, h, '2kHz');
       TextOut(Round(rct.Left + FreqLogToLinear(5000)  * Wdth) - 10, h, '5kHz');
       TextOut(Round(rct.Left + FreqLogToLinear(10000) * Wdth) - 12, h, '10kHz');
      end;
     flsTop:
      begin
       TextOut(Round(rct.Left + FreqLogToLinear(100) * Wdth) - 12, Round(rct.Top),'100Hz');
       TextOut(Round(rct.Left + FreqLogToLinear(1000) * Wdth) - 10, Round(rct.Top),'1kHz');
      end;
    end;

    case FdBLabelStyle of
     dlsLeft:
      begin
       if FMaxGain <=  5 then w := 1 else
       if FMaxGain <= 10 then w := 5 else 
       if FMaxGain >= 180 then w := 45 else w := 10;
       i := w;
       while i * NormFactor < HalfHeight do
        begin
         TextOut(rct.Left, round(rct.Top + HalfHeight - i * NormFactor - 5) + Font.Height div 2 + 2, '+' + IntToStr(i) + 'dB');
         TextOut(rct.Left, round(rct.Top + HalfHeight + i * NormFactor - 5) + Font.Height div 2 + 2, '-' + IntToStr(i) + 'dB');
         inc(i,w);
        end;
      end;
     dlsRight:
      begin
       if FMaxGain <=  5 then w := 1 else
       if FMaxGain <= 10 then w := 5 else 
       if FMaxGain >= 180 then w := 45 else w := 10;
       i := w;
       while i * NormFactor < HalfHeight do
        begin
         TextOut(Round(rct.Right - 27), round(HalfHeight - i * NormFactor - 5), '+' + IntToStr(i) + 'dB');
         TextOut(Round(rct.Right - 27), round(HalfHeight + i * NormFactor - 5), '-' + IntToStr(i) + 'dB');
         inc(i, w);
        end;
      end;
    end;

    Pen.Color := GraphColorDark;
    Pen.Width := 2;
    if assigned(FOnGetFilterGain) then
     begin
      MoveTo(rct.Left, round(HHOffs - FOnGetFilterGain(Self, 20) * NormFactor));
      Temp[0] := 1 / (rct.Right - rct.Left);
      for w := rct.Left + 1 to rct.Right - 1
       do LineTo(w, round(HHOffs - FOnGetFilterGain(Self, FreqLinearToLog((w - rct.Left) * Temp[0])) * NormFactor));
     end;


    InflateRect(rct, 1, 1);

    if FBorderWidth > 0 then
     begin
      Pen.Color := Font.Color;
      Pen.Width := FBorderWidth;
      Brush.Style := bsClear;
      RoundRect(rct.Left, rct.Top, rct.Right, rct.Bottom,
        FBorderRadius, FBorderRadius);
     end;

    FBufferNeedsRepaint := True;
    Unlock;
   end;
end;

procedure TGuiEQGraph.Resize;
begin
 inherited;
 FBuffer.Canvas.Brush.Color := Self.Color;
 FBuffer.Width := Self.Width;
 FBuffer.Height := Self.Height;
 FChartBuffer.Canvas.Brush.Color := Self.Color;
 FChartBuffer.Width := Self.Width;
 FChartBuffer.Height := Self.Height;
 ChartBufferNeedsRepaint := True;
end;

procedure TGuiEQGraph.SetMaxGain(const Value: Single);
begin
 if FMaxGain <> Value then
  begin
   FMaxGain := Value;
   MaxGainChanged;
  end;
end;

procedure TGuiEQGraph.MaxGainChanged;
begin
 RepaintChartBuffer;
end;

procedure TGuiEQGraph.SetGraphColorDark(const Value: TColor);
begin
 if FGraphColorDark <> Value then
  begin
   FGraphColorDark := Value;
   RepaintChartBuffer;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetGraphColorLight(const Value: TColor);
begin
 if FGraphColorLight <> Value then
  begin
   FGraphColorLight := Value;
   RepaintChartBuffer;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetFrequencyAxisLabelStyle(const Value: TFrequencyAxisLabelStyle);
begin
 if FFreqLabelStyle <> Value then
  begin
   FFreqLabelStyle := Value;
   RepaintChartBuffer;
   Invalidate;
  end;
end;

procedure TGuiEQGraph.SetdBLabelStyle(const Value: TdBLabelStyle);
begin
 FdBLabelStyle := Value;
 RepaintChartBuffer;
 Invalidate;
end;

end.
