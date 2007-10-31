unit DGuiDynamicWaveform;

interface

uses DGuiStaticWaveform, DAVDCommon, Classes, Controls;

type
  TGuiWaveProcessMode = (wpmScroll, wpmReplace, wpmStretch);

  TGuiDynamicWaveform = class(TGuiStaticWaveform)
  private
    fWaveProcessMode: TGuiWaveProcessMode;
    fInternalBufferSize: Integer;
    fInternalBuffer: TArrayOfSingleDynArray;
    fInternalBufferChannels: Integer;

    procedure SetInternalBufferSize(const Value: Integer);
    procedure SetInternalBufferChannels(const Value: Integer);
  protected
    procedure SetRedrawInterval(Value: Integer); override;
    procedure UpdateInternalBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessBufferIndirect(NewWaveData: TArrayOfSingleDynArray; Channels, SampleFrames: Integer);
    procedure ProcessBuffer(NewWaveData: TAVDSingleDynArray; InpLen: Integer = -1); overload;
    procedure ProcessBuffer(NewWaveData: TArrayOfSingleDynArray; InpLen: Integer = -1); overload;
  published
    property RedrawInterval;

    property InternalBufferSize: Integer read fInternalBufferSize write SetInternalBufferSize default 512;
    property InternalBufferChannels: Integer read fInternalBufferChannels write SetInternalBufferChannels default 2;
    property WaveProcessMode: TGuiWaveProcessMode read fWaveProcessMode write fWaveProcessMode default wpmScroll;
  end;

implementation

uses SysUtils, Math, dialogs, DGuiBaseControl;

constructor TGuiDynamicWaveform.Create(AOwner: TComponent);
begin
  inherited;

  fRedrawTimer.Interval   := 25;
  fWaveProcessMode        := wpmScroll;
  fInternalBufferChannels := 2;
  fInternalBufferSize     := 512;
  UpdateInternalBuffer;
end;

destructor TGuiDynamicWaveform.Destroy;
begin
  fInternalBufferChannels := 0;
  fInternalBufferSize     := 0;
  UpdateInternalBuffer;
  inherited;
end;

procedure TGuiDynamicWaveform.ProcessBufferIndirect(NewWaveData: TArrayOfSingleDynArray; Channels, SampleFrames: Integer);
var tmp: TArrayOfSingleDynArray; i: integer;
begin
  SetLength(tmp,Channels);
  for i:=0 to Channels-1 do
  begin
    SetLength(tmp[i],SampleFrames);
    move(NewWaveData[i,0], tmp[i,0], SampleFrames*SizeOf(Single));
  end;

  ProcessBuffer(tmp, SampleFrames);
end;

procedure TGuiDynamicWaveform.ProcessBuffer(NewWaveData: TAVDSingleDynArray; InpLen: Integer);
var tmp: TArrayOfSingleDynArray;
begin
  SetLength(tmp,1);
  tmp[0] := NewWaveData;
  ProcessBuffer(tmp, InpLen);
end;

procedure TGuiDynamicWaveform.ProcessBuffer(NewWaveData: TArrayOfSingleDynArray; InpLen: Integer);
var nOffset, Amount, i, tmpLen: integer;
    stepw,pos, frac: single;
    InputBuffer: TAVDSingleDynArray;
begin 
  if InpLen<1 then
  begin
    for i:=0 to fInternalBufferChannels-1 do
      if i < Length(NewWaveData) then
        InpLen := max(Length(NewWaveData[i]), InpLen);

    if InpLen<1 then exit;
  end;

  for i:=0 to fInternalBufferChannels-1 do
  begin
    tmpLen := 0;
    if i < Length(NewWaveData) then
    begin
      InputBuffer:=NewWaveData[i];
      tmpLen:=length(InputBuffer);
    end;
    
    setlength(InputBuffer, InpLen);
    if tmpLen<InpLen then
      FillChar(InputBuffer[tmpLen], (InpLen-tmpLen)*SizeOf(single), 0);

    case fWaveProcessMode of
      wpmScroll:
        if InpLen>=fInternalBufferSize then
        begin

          // copy part of input into full working buffer
          nOffset := InpLen - fInternalBufferSize;
          move(InputBuffer[nOffset],fInternalBuffer[i,0],fInternalBufferSize * SizeOf(Single));
        end else begin
          
          // copy full input buffer into part of working buffer
          nOffset := fInternalBufferSize-InpLen;
          move(fInternalBuffer[i,InpLen],fInternalBuffer[i,0],nOffset * SizeOf(Single));
          move(InputBuffer[0],fInternalBuffer[i,nOffset],InpLen * SizeOf(Single));
        end;
      wpmReplace:
        begin
          Amount := min(InpLen, fInternalBufferSize);
          move(InputBuffer[0],fInternalBuffer[i,0],Amount * SizeOf(Single));

          if Amount < fInternalBufferSize then
          begin
            fillchar(fInternalBuffer[i,Amount], (fInternalBufferSize-Amount) * SizeOf(Single), 0);
          end;
        end; 

      wpmStretch:
        begin
          stepw:=(InpLen-1)/(fInternalBufferSize-1);
          fInternalBuffer[i,0] := InputBuffer[0];

          for nOffset:=1 to fInternalBufferSize-1 do
          begin
            pos := stepw*nOffset;
            frac := pos - trunc(pos);

            fInternalBuffer[i,nOffset] := InputBuffer[floor(pos)]*frac+InputBuffer[ceil(pos)]*(1-frac);
          end;
        end;
    end;
  end;
  if not fTimerMustRedraw then SetWaveForm(fInternalBuffer);
  fTimerMustRedraw:=true;
end;

procedure TGuiDynamicWaveform.UpdateInternalBuffer;
var i: integer;
begin
  for i:=fInternalBufferChannels to Length(fInternalBuffer)-1 do SetLength(fInternalBuffer[i],0);
  SetLength(fInternalBuffer, fInternalBufferChannels);

  if fInternalBufferChannels>0 then
    for i:=0 to Length(fInternalBuffer)-1 do
    begin
      SetLength(fInternalBuffer[i],fInternalBufferSize);
      FillChar(fInternalBuffer[i][0],SizeOf(single)*fInternalBufferSize,0);
    end;
end;

procedure TGuiDynamicWaveform.SetInternalBufferChannels(const Value: Integer);
begin
  if Value<1 then
    raise Exception.Create('InternalBufferChannels must greater than 0')

  else if fInternalBufferChannels<>Value then
  begin
    fInternalBufferChannels := Value;
    UpdateInternalBuffer;
  end;
end;

procedure TGuiDynamicWaveform.SetInternalBufferSize(const Value: Integer);
begin
  if Value<1 then
    raise Exception.Create('InternalBufferSize must greater than 0')

  else if fInternalBufferSize<>Value then
  begin
    fInternalBufferSize := Value;
    UpdateInternalBuffer;
  end;
end;

procedure TGuiDynamicWaveform.SetRedrawInterval(Value: Integer);
begin
  if Value<1 then
    raise Exception.Create('RedrawInterval must greater than 0');

  inherited;
end;

end.
