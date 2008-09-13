unit DAV_DspVoice;

interface

{$I ASIOVST.INC}

uses
  Classes, DAV_Common, DAV_ProcessingComponent;

type
  TDspVoice = class;

  TDspOnVoiceNoteOff = procedure(Sender: TDspVoice; var CanGoOff: Boolean) of object;

  TDspVoiceProcessingMode = (pmDspQueue, pmUser);

  TDspVoiceInfo = class(TObject)
  public
    InitialMidiEvent: TAVDMidiEvent;
    NoteNr: Byte;
    Velocity: single;
    Offset: LongInt;

    constructor Create(MidiEvent: TAVDMidiEvent);
  end;

  TDspVoiceTrailingType = (vttAutomatic, vttManually);
  TDspVoice = class(TDataModule)
  protected
    fEnabled:         Boolean;
    fSampleRate:      Single;
    fChannels:        Integer;

    fIsVoiceNoteOn:   Boolean;
    fTrailingSamples: Integer;
    fTrailingType:    TDspVoiceTrailingType;

    fOffTrailingCounter: array of Integer;

    fVoiceInfo:       TDspVoiceInfo;
    FDspQueueList:    TAVDProcessingComponentList;
    FDspDirectProcessItem: TAVDProcessingComponent;

    fProcessS:   TDspBaseProcessFuncS;
    fProcessD:   TDspBaseProcessFuncD;
    fProcessSA:  TDspBaseProcessFuncSA;
    fProcessDA:  TDspBaseProcessFuncDA;
    fProcessSAA: TDspBaseProcessFuncSAA;
    fProcessDAA: TDspBaseProcessFuncDAA;

    fDefaultProcessS:   TDspBaseProcessFuncS;
    fDefaultProcessD:   TDspBaseProcessFuncD;
    fDefaultProcessSA:  TDspBaseProcessFuncSA;
    fDefaultProcessDA:  TDspBaseProcessFuncDA;
    fDefaultProcessSAA: TDspBaseProcessFuncSAA;
    fDefaultProcessDAA: TDspBaseProcessFuncDAA;

    fUserProcessS:   TDspBaseProcessFuncS;
    fUserProcessD:   TDspBaseProcessFuncD;
    fUserProcessSA:  TDspBaseProcessFuncSA;
    fUserProcessDA:  TDspBaseProcessFuncDA;
    fUserProcessSAA: TDspBaseProcessFuncSAA;
    fUserProcessDAA: TDspBaseProcessFuncDAA;

    FVoiceProcessingMode: TDspVoiceProcessingMode;

    fOnVoiceNoteOff: TDspOnVoiceNoteOff;

    procedure UpdateProcessingFunctions; virtual;
    procedure InitializeTrailing; virtual;

    procedure SetEnabled(const Value: Boolean);   virtual;
    procedure SetSampleRate(const Value: Single); virtual;
    procedure SetChannels(const Value: Integer);  virtual;

    // placeholders for unset processing procedures
    procedure ProcessBasic      (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;

    // for enabled = false
    procedure ProcessSilence    (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessSilence    (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessSilence    (var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;

    // for dsp direct processing mode
    procedure ProcessDspItem    (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessDspItem    (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessDspItem    (var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;

    procedure SetUserProcessD   (const Value: TDspBaseProcessFuncD);
    procedure SetUserProcessDA  (const Value: TDspBaseProcessFuncDA);
    procedure SetUserProcessDAA (const Value: TDspBaseProcessFuncDAA);
    procedure SetUserProcessS   (const Value: TDspBaseProcessFuncS);
    procedure SetUserProcessSA  (const Value: TDspBaseProcessFuncSA);
    procedure SetUserProcessSAA (const Value: TDspBaseProcessFuncSAA);

    procedure SetDspDirectProcessItem(v: TAVDProcessingComponent); virtual;

    procedure SetVoiceProcessingMode(const Value: TDspVoiceProcessingMode);

    function GetIsAlive: Boolean;

    procedure SetTrailingSamples(const Value: Integer);  virtual;
    procedure SetTrailingType(const Value: TDspVoiceTrailingType); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; VoiceInfo: TDspVoiceInfo); reintroduce; overload;
    destructor Destroy; override;

    procedure RegisterDSPItem(item: TAVDProcessingComponent);
    procedure UnRegisterDSPItem(item: TAVDProcessingComponent);

    procedure Init;  virtual;
    procedure Reset; virtual;

    procedure ProcessMidiEvent(MidiEvent: TAVDMidiEvent; var FilterEvent: Boolean); virtual;

    procedure VoiceNoteOff; virtual;
    procedure DecrementTrailing(DecrementTrailing: Integer = 0; Channel: integer = -1);

    procedure UpdateTrailingSamples; virtual;

    property IsAlive:        Boolean read GetIsAlive;
    property IsVoiceNoteOn:  Boolean read fIsVoiceNoteOn;

    property ProcessS:   TDspBaseProcessFuncS   read fProcessS;
    property ProcessD:   TDspBaseProcessFuncD   read fProcessD;
    property ProcessSA:  TDspBaseProcessFuncSA  read fProcessSA;
    property ProcessDA:  TDspBaseProcessFuncDA  read fProcessDA;
    property ProcessSAA: TDspBaseProcessFuncSAA read fProcessSAA;
    property ProcessDAA: TDspBaseProcessFuncDAA read fProcessDAA;

    property VoiceInfo: TDspVoiceInfo read fVoiceInfo;

    property Channels:   Integer read fChannels   write SetChannels   default 2;
    property SampleRate: Single  read fSampleRate write SetSampleRate;
  published
    property Enabled: Boolean read fEnabled write SetEnabled default true;

    property DspDirectProcessItem: TAVDProcessingComponent read fDspDirectProcessItem write SetDspDirectProcessItem default nil;

    property VoiceProcessingMode: TDspVoiceProcessingMode read FVoiceProcessingMode write SetVoiceProcessingMode;

    property OnProcessS:   TDspBaseProcessFuncS   read fUserProcessS   write SetUserProcessS;
    property OnProcessD:   TDspBaseProcessFuncD   read fUserProcessD   write SetUserProcessD;
    property OnProcessSA:  TDspBaseProcessFuncSA  read fUserProcessSA  write SetUserProcessSA;
    property OnProcessDA:  TDspBaseProcessFuncDA  read fUserProcessDA  write SetUserProcessDA;
    property OnProcessSAA: TDspBaseProcessFuncSAA read fUserProcessSAA write SetUserProcessSAA;
    property OnProcessDAA: TDspBaseProcessFuncDAA read fUserProcessDAA write SetUserProcessDAA;

    property OnVoiceNoteOff: TDspOnVoiceNoteOff read fOnVoiceNoteOff write fOnVoiceNoteOff;

    property TrailingType: TDspVoiceTrailingType read fTrailingType write SetTrailingType;
    property TrailingSamples: Integer read fTrailingSamples write SetTrailingSamples;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Forms, SysUtils
  {$IFDEF PUREPASCAL},DAV_BufferMathPascal{$ELSE},DAV_BufferMathAsm{$ENDIF};

constructor TDspVoiceInfo.Create(MidiEvent: TAVDMidiEvent);
begin
  inherited Create;
  NoteNr := MidiEvent.MidiData[1];

  Velocity := MidiEvent.MidiData[2] / 127;
  Offset := MidiEvent.DeltaFrames;
  InitialMidiEvent := MidiEvent;
end;



{ TDspVoice }

constructor TDspVoice.Create(AOwner: TComponent);
begin
  fDefaultProcessS   := ProcessSilence;
  fDefaultProcessD   := ProcessBasic;
  fDefaultProcessSA  := ProcessBasic;
  fDefaultProcessDA  := ProcessBasic;
  fDefaultProcessSAA := ProcessBasic;
  fDefaultProcessDAA := ProcessBasic;

  fUserProcessS   := nil;
  fUserProcessD   := nil;
  fUserProcessSA  := nil;
  fUserProcessDA  := nil;
  fUserProcessSAA := nil;
  fUserProcessDAA := nil;

  FDspQueueList   := TAVDProcessingComponentList.Create;
  fVoiceInfo      := nil;
  fEnabled        := true;
  fIsVoiceNoteOn  := true;

  FDspDirectProcessItem:=nil;
  fTrailingType := vttAutomatic;
  
  inherited Create(AOwner);

  Init;

  UpdateProcessingFunctions;
end;

constructor TDspVoice.Create(AOwner: TComponent; VoiceInfo: TDspVoiceInfo);
begin
  Create(AOwner);
  fVoiceInfo:=VoiceInfo;
end;

destructor TDspVoice.Destroy;
begin
  FDspQueueList.Free;
  if assigned(fVoiceInfo) then fVoiceInfo.free;
  inherited;
end;

procedure TDspVoice.Init;
begin

end;

procedure TDspVoice.Reset;
begin
  if (FVoiceProcessingMode = pmDspQueue) and Assigned(FDspDirectProcessItem) then
    FDspDirectProcessItem.ResetQueue;
end;

procedure TDspVoice.RegisterDSPItem(item: TAVDProcessingComponent);
begin
  with FDspQueueList do
  begin
    if IndexOf(item)<0 then
    begin
      Add(item);
      if (FVoiceProcessingMode = pmDspQueue) and not Assigned(FDspDirectProcessItem) then
      begin
        FDspDirectProcessItem:=item;
        UpdateProcessingFunctions;
      end;
    end;  
    Item.SampleRate:=FSampleRate;
    Item.Channels:=fChannels;  
  end;
end;

procedure TDspVoice.UnRegisterDSPItem(item: TAVDProcessingComponent);
begin
 with FDspQueueList do
  if IndexOf(item)>=0
   then Remove(item);

  if FDspDirectProcessItem=item then
  begin
    if FDspQueueList.Count>0 then FDspDirectProcessItem:=FDspQueueList.Items[0]
    else FDspDirectProcessItem:=nil;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetEnabled(const Value: Boolean);
begin
  if fEnabled<>value then
  begin
    fEnabled := value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetChannels(const Value: Integer);
begin
  if fChannels<>Value then
  begin
    fChannels := Value;
    FDspQueueList.SetChannels(fChannels);
    InitializeTrailing;
  end;
end;

procedure TDspVoice.SetSampleRate(const Value: Single);
begin
  if fSampleRate<>Value then
  begin
    fSampleRate := Value;
    FDspQueueList.SetSampleRate(fSampleRate);
  end;
end;

procedure TDspVoice.InitializeTrailing;
var i: integer;
begin
  setlength(fOffTrailingCounter, fChannels);
  for i:=fChannels-1 downto 0 do
    fOffTrailingCounter[i] := fTrailingSamples;
end;

procedure TDspVoice.UpdateProcessingFunctions;
begin
  if fEnabled then
  begin
    if FVoiceProcessingMode = pmUser then
    begin
      if assigned(fUserProcessS)   then fProcessS   := fUserProcessS   else fProcessS   := fDefaultProcessS;
      if assigned(fUserProcessD)   then fProcessD   := fUserProcessD   else fProcessD   := fDefaultProcessD;
      if assigned(fUserProcessSA)  then fProcessSA  := fUserProcessSA  else fProcessSA  := fDefaultProcessSA;
      if assigned(fUserProcessDA)  then fProcessDA  := fUserProcessDA  else fProcessDA  := fDefaultProcessDA;
      if assigned(fUserProcessSAA) then fProcessSAA := fUserProcessSAA else fProcessSAA := fDefaultProcessSAA;
      if assigned(fUserProcessDAA) then fProcessDAA := fUserProcessDAA else fProcessDAA := fDefaultProcessDAA;
    end else if assigned(FDspDirectProcessItem) then begin
      fProcessS   := ProcessDspItem;
      fProcessD   := ProcessDspItem;
      fProcessSA  := ProcessDspItem;
      fProcessDA  := ProcessDspItem;
      fProcessSAA := ProcessDspItem;
      fProcessDAA := ProcessDspItem;
    end else begin
      fProcessS   := ProcessSilence;
      fProcessD   := ProcessSilence;
      fProcessSA  := ProcessSilence;
      fProcessDA  := ProcessSilence;
      fProcessSAA := ProcessSilence;
      fProcessDAA := ProcessSilence;
    end;
  end else begin
    fProcessS   := ProcessSilence;
    fProcessD   := ProcessSilence;
    fProcessSA  := ProcessSilence;
    fProcessDA  := ProcessSilence;
    fProcessSAA := ProcessSilence;
    fProcessDAA := ProcessSilence;
  end;
end;

procedure TDspVoice.SetVoiceProcessingMode(const Value: TDspVoiceProcessingMode);
begin
  // no check for changes here!!!
  FVoiceProcessingMode := Value;
  UpdateProcessingFunctions;
end;

procedure TDspVoice.SetDspDirectProcessItem(v: TAVDProcessingComponent);
begin
  if v<>FDspDirectProcessItem then
  begin
    if v=nil then
    begin
      FDspDirectProcessItem:=v;
      UpdateProcessingFunctions;
    end else if FDspQueueList.IndexOf(v)>=0 then
    begin
      FDspDirectProcessItem:=v;
      SetVoiceProcessingMode(pmDspQueue);
    end else
      raise Exception.Create('DspDirectProcessItem has to be the first item of a queue');
  end;
end;




procedure TDspVoice.SetUserProcessD(const Value: TDspBaseProcessFuncD);
begin
  if @FUserProcessD<>@Value then
  begin
    FUserProcessD := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessDA(const Value: TDspBaseProcessFuncDA);
begin
  if @FUserProcessDA<>@Value then
  begin
    FUserProcessDA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessDAA(const Value: TDspBaseProcessFuncDAA);
begin
  if @FUserProcessDAA<>@Value then
  begin
    FUserProcessDAA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessS(const Value: TDspBaseProcessFuncS);
begin
  if @FUserProcessS<>@Value then
  begin
    FUserProcessS := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessSA(const Value: TDspBaseProcessFuncSA);
begin
  if @FUserProcessSA<>@Value then
  begin
    FUserProcessSA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetUserProcessSAA(const Value: TDspBaseProcessFuncSAA);
begin
  if @FUserProcessSAA<>@Value then
  begin
    FUserProcessSAA := Value;
    UpdateProcessingFunctions;
  end;
end;

procedure TDspVoice.SetTrailingSamples(const Value: Integer);
begin
  if fTrailingSamples<>Value then
  begin
    fTrailingSamples := Value;
    fTrailingType := vttManually;
  end;
end;

procedure TDspVoice.SetTrailingType(const Value: TDspVoiceTrailingType);
begin
  if fTrailingType<>Value then
  begin
    fTrailingType := Value;
    UpdateTrailingSamples;
  end;
end;



procedure TDspVoice.ProcessSilence(var Data: Single; const channel: integer);
begin
  Data := 0;
end;

procedure TDspVoice.ProcessSilence(var Data: Double; const channel: integer);
begin
  Data := 0;
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Double), 0);
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);
end;

procedure TDspVoice.ProcessSilence(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);
end;




procedure TDspVoice.ProcessBasic(var Data: Double; const channel: integer);
var tmp: single;
begin
  tmp := Data;
  fProcessS(tmp, channel);
  Data := tmp;
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do fProcessS(ProcessBuffer[i], channel);
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do fProcessD(ProcessBuffer[i], channel);
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to fChannels - 1
  do fProcessSA(ProcessBuffer[i], i, SampleFrames);
end;

procedure TDspVoice.ProcessBasic(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to fChannels - 1
  do fProcessDA(ProcessBuffer[i], i, SampleFrames);
end;





procedure TDspVoice.ProcessDspItem(var Data: Single; const channel: integer);
begin
  FDspDirectProcessItem.ProcessQueueS(Data, channel);
end;

procedure TDspVoice.ProcessDspItem(var Data: Double; const channel: integer);
begin
  FDspDirectProcessItem.ProcessQueueD(Data, channel);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueSA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueDA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
end;

procedure TDspVoice.ProcessDspItem(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
begin
  FDspDirectProcessItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
end;



procedure TDspVoice.UpdateTrailingSamples;
begin
  if fTrailingType=vttManually then exit;

  fTrailingSamples := FDspQueueList.TrailingSamplesQueue;
end;

procedure TDspVoice.ProcessMidiEvent(MidiEvent: TAVDMidiEvent; var FilterEvent: Boolean);
begin
  FDspQueueList.ProcessMidiEventQueue(MidiEvent, FilterEvent);
end;

procedure TDspVoice.VoiceNoteOff;
var CanGoOff: Boolean;
begin
  CanGoOff := true;
  if Assigned(fOnVoiceNoteOff) then fOnVoiceNoteOff(self, CanGoOff);
  if CanGoOff then
  begin
    FDspQueueList.NoteOffQueue;
    InitializeTrailing;
    fIsVoiceNoteOn:=false;
  end;
end;

procedure TDspVoice.DecrementTrailing(DecrementTrailing: Integer = 0; Channel: integer = -1);
var i: Integer;
begin
  if not fIsVoiceNoteOn and (DecrementTrailing>0) then
    if Channel>=0 then
      fOffTrailingCounter[Channel] := fOffTrailingCounter[Channel] - DecrementTrailing
    else for i:=fChannels-1 downto 0 do
      fOffTrailingCounter[i] := fOffTrailingCounter[i] - DecrementTrailing;
end;

function TDspVoice.GetIsAlive: Boolean;
var i: integer;
begin
  Result:=true;
  if fIsVoiceNoteOn then exit;

  for i:=fChannels-1 downto 0 do
    if fOffTrailingCounter[i]>0 then exit;

  Result:=false;
end;

end.
