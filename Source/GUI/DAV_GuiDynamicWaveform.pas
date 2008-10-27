unit DAV_GuiDynamicWaveform;

interface

uses
  DAV_GuiStaticWaveform, DAV_Common, Classes, Controls;

type
  TGuiWaveProcessMode = (wpmScroll, wpmReplace, wpmStretch);

  TCustomGuiDynamicWaveform = class(TCustomGuiStaticWaveform)
  private
    fWaveProcessMode: TGuiWaveProcessMode;
    fInternalBufferSize: Integer;
    fInternalBuffer: TDAVArrayOfSingleDynArray;
    fInternalBufferChannels: Integer;

    procedure SetInternalBufferSize(const Value: Integer);
    procedure SetInternalBufferChannels(const Value: Integer);
  protected
    procedure SetRedrawInterval(Value: Integer); override;
    procedure UpdateInternalBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessBufferIndirect(NewWaveData: TDAVArrayOfSingleDynArray; Channels, SampleFrames: Integer);
    procedure ProcessBuffer(NewWaveData: TDAVSingleDynArray; InpLen: Integer = -1); overload;
    procedure ProcessBuffer(NewWaveData: TDAVArrayOfSingleDynArray; InpLen: Integer = -1); overload;
  published
    property RedrawInterval;

    property InternalBufferSize: Integer read fInternalBufferSize write SetInternalBufferSize default 512;
    property InternalBufferChannels: Integer read fInternalBufferChannels write SetInternalBufferChannels default 2;
    property WaveProcessMode: TGuiWaveProcessMode read fWaveProcessMode write fWaveProcessMode default wpmScroll;
  end;

  TGuiDynamicWaveform = class(TCustomGuiDynamicWaveform)
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
    property InternalBufferChannels;
    property InternalBufferSize;
    property LineColor;
    property LineWidth;
    property MedianColor;
    property MedianLineWidth;
    property MedianVisible;
    property NormalizationType;
    property PopupMenu;
    property RedrawInterval;
    property ShowHint;
    property Visible;
    property WaveDrawMode;
    property WaveProcessMode;
    property WaveVPadding;
    {$IFNDEF FPC}
    property Transparent;
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
    property OnDragMouseMove;
  end;

implementation

uses SysUtils, Math, dialogs, DAV_GuiBaseControl;

constructor TCustomGuiDynamicWaveform.Create(AOwner: TComponent);
begin
  inherited;

  fRedrawTimer.Interval   := 25;
  fWaveProcessMode        := wpmScroll;
  fInternalBufferChannels := 2;
  fInternalBufferSize     := 512;
  UpdateInternalBuffer;
end;

destructor TCustomGuiDynamicWaveform.Destroy;
begin
  fInternalBufferChannels := 0;
  fInternalBufferSize     := 0;
  UpdateInternalBuffer;
  inherited;
end;

procedure TCustomGuiDynamicWaveform.ProcessBufferIndirect(NewWaveData: TDAVArrayOfSingleDynArray; Channels, SampleFrames: Integer);
var
  tmp : TDAVArrayOfSingleDynArray;
  i   : Integer;
begin
  SetLength(tmp,Channels, SampleFrames);
  for i := 0 to Channels - 1
   do Move(NewWaveData[i, 0], tmp[i, 0], SampleFrames * SizeOf(Single));

  ProcessBuffer(tmp, SampleFrames);
end;

procedure TCustomGuiDynamicWaveform.ProcessBuffer(NewWaveData: TDAVSingleDynArray; InpLen: Integer);
var
  tmp: TDAVArrayOfSingleDynArray;
begin
  SetLength(tmp, 1);
  tmp[0] := NewWaveData;
  ProcessBuffer(tmp, InpLen);
end;

procedure TCustomGuiDynamicWaveform.ProcessBuffer(NewWaveData: TDAVArrayOfSingleDynArray; InpLen: Integer);
var
  nOffset,
  Amount, i,
  tmpLen      : Integer;
  stepw,pos,
  frac        : Single;
  InputBuffer : TDAVSingleDynArray;
begin
  if InpLen<1 then
  begin
    for i := 0 to fInternalBufferChannels-1 do
      if i < Length(NewWaveData) then
        InpLen := max(Length(NewWaveData[i]), InpLen);

    if InpLen<1 then exit;
  end;

  for i := 0 to fInternalBufferChannels-1 do
  begin
    tmpLen := 0;
    if i < Length(NewWaveData) then
    begin
      InputBuffer := NewWaveData[i];
      tmpLen := length(InputBuffer);
    end;

    SetLength(InputBuffer, InpLen);
    if tmpLen < InpLen then
      FillChar(InputBuffer[tmpLen], (InpLen - tmpLen) * SizeOf(Single), 0);

    case fWaveProcessMode of
      wpmScroll:
        if InpLen >= fInternalBufferSize then
        begin

          // copy part of input into full working buffer
          nOffset := InpLen - fInternalBufferSize;
          Move(InputBuffer[nOffset], fInternalBuffer[i, 0], fInternalBufferSize * SizeOf(Single));
        end else begin

          // copy full input buffer into part of working buffer
          nOffset := fInternalBufferSize - InpLen;
          Move(fInternalBuffer[i, InpLen], fInternalBuffer[i, 0], nOffset * SizeOf(Single));
          Move(InputBuffer[0], fInternalBuffer[i, nOffset], InpLen * SizeOf(Single));
        end;
      wpmReplace:
        begin
          Amount := min(InpLen, fInternalBufferSize);
          Move(InputBuffer[0], fInternalBuffer[i, 0], Amount * SizeOf(Single));

          if Amount < fInternalBufferSize then
          begin
            fillchar(fInternalBuffer[i, Amount], (fInternalBufferSize - Amount) * SizeOf(Single), 0);
          end;
        end; 

      wpmStretch:
        begin
          stepw := (InpLen - 1) / (fInternalBufferSize - 1);
          fInternalBuffer[i, 0] := InputBuffer[0];

          for nOffset := 1 to fInternalBufferSize - 1 do
           begin
            pos := stepw * nOffset;
            frac := pos - trunc(pos);

            fInternalBuffer[i, nOffset] := InputBuffer[floor(pos)] * frac + InputBuffer[ceil(pos)] * (1 - frac);
           end;
        end;
    end;
  end;
  if not fTimerMustRedraw then SetWaveForm(fInternalBuffer);
  fTimerMustRedraw := true;
end;

procedure TCustomGuiDynamicWaveform.UpdateInternalBuffer;
var
  i : Integer;
begin
  for i := fInternalBufferChannels to Length(fInternalBuffer) - 1
   do SetLength(fInternalBuffer[i], 0);
  SetLength(fInternalBuffer, fInternalBufferChannels, fInternalBufferSize);

  if fInternalBufferChannels > 0 then
   for i := 0 to Length(fInternalBuffer) - 1 do
      FillChar(fInternalBuffer[i][0], SizeOf(Single) * fInternalBufferSize, 0);
end;

procedure TCustomGuiDynamicWaveform.SetInternalBufferChannels(const Value: Integer);
begin
  if Value < 1
   then raise Exception.Create('InternalBufferChannels must greater than 0');

 if fInternalBufferChannels <> Value then
  begin
   fInternalBufferChannels := Value;
   UpdateInternalBuffer;
  end;
end;

procedure TCustomGuiDynamicWaveform.SetInternalBufferSize(const Value: Integer);
begin
  if Value < 1
   then raise Exception.Create('InternalBufferSize must greater than 0');
  if fInternalBufferSize <> Value then
   begin
    fInternalBufferSize := Value;
    UpdateInternalBuffer;
   end;
end;

procedure TCustomGuiDynamicWaveform.SetRedrawInterval(Value: Integer);
begin
  if Value < 1
   then raise Exception.Create('RedrawInterval must greater than 0');

  inherited;
end;

end.
