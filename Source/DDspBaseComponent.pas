unit DDspBaseComponent;

interface

{$I ASIOVST.inc}

uses Classes, DAVDCommon, Contnrs, DAVDProcessingComponent;

type
  TDspQueueList = TComponentList;

  TDspBaseComponent = class(TAVDProcessingComponent)
  protected
    fNextDspQueueItem: TDspBaseComponent;
    fPrevDspQueueItem: TDspBaseComponent;

    fStdProcessS:   TDspBaseProcessFuncS;
    fStdProcessD:   TDspBaseProcessFuncD;
    fStdProcessSA:  TDspBaseProcessFuncSA;
    fStdProcessDA:  TDspBaseProcessFuncDA;
    fStdProcessSAA: TDspBaseProcessFuncSAA;
    fStdProcessDAA: TDspBaseProcessFuncDAA;

    fStdProcessQueueS:   TDspBaseProcessFuncS;
    fStdProcessQueueD:   TDspBaseProcessFuncD;
    fStdProcessQueueSA:  TDspBaseProcessFuncSA;
    fStdProcessQueueDA:  TDspBaseProcessFuncDA;
    fStdProcessQueueSAA: TDspBaseProcessFuncSAA;
    fStdProcessQueueDAA: TDspBaseProcessFuncDAA;
    
    function  GetTrailingSamplesQueue: integer; override;

    procedure SetBypass(const Value: Boolean); override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure SetSampleRate(const Value: Single); override;
    procedure SetChannels(const Value: Integer); override;
    procedure SetTrailingSamples(const Value: Integer); override;

    procedure SetNextDspQueueItem(const Value: TDspBaseComponent); virtual;
    procedure SampleRateChanged; virtual;
    procedure ChannelsChanged; virtual;
    procedure UpdateParameters; virtual;
    procedure TrailingSamplesChanged; virtual;
    procedure BeforeDestroy; virtual;

    procedure RegisterInOwner(item: TDspBaseComponent);
    procedure UnRegisterInOwner(item: TDspBaseComponent);
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; UseSampleRate: Integer); reintroduce; overload;
    destructor Destroy; override;

    procedure Init; override;       // called automaticaly in constructor
    procedure Reset; override;      // called manualy
    procedure ResetQueue; override;

    procedure NoteOff; override;
    procedure NoteOffQueue; override;

    function  GetFollowingItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetPreviousItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback
    function  GetQueueItems(var items: TDspQueueList): boolean; virtual; // Returns false on loopback

    
    procedure ProcessSilence    (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var Data: Single; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var Data: Single; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBypass     (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessBasic      (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessQueueBasic (var Data: Double; const channel: integer); overload; virtual;
    procedure ProcessQueueBypass(var Data: Double; const channel: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer); overload; virtual;

    procedure ProcessSilence    (var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBypass     (var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessBasic      (var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBasic (var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;
    procedure ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer); overload; virtual;

    procedure ProcessMidiEvent(MidiEvent: TAVDMidiEvent; var FilterEvent: Boolean); override;
    procedure ProcessMidiEventQueue(MidiEvent: TAVDMidiEvent; var FilterEvent: Boolean); override;

    property PrevDspQueueItem: TDspBaseComponent read fPrevDspQueueItem write fPrevDspQueueItem;
  published
    property Enabled: Boolean                    read fEnabled          write SetEnabled    default true;
    property Bypass: Boolean                     read fBypass           write SetBypass     default false;
    property Channels: Integer                   read fChannels         write SetChannels   default 2;
    property SampleRate: Single                  read fSampleRate       write SetSampleRate;
    property NextDspQueueItem: TDspBaseComponent read fNextDspQueueItem write SetNextDspQueueItem;
  end;

implementation

uses Sysutils, Math, DVSTModuleWithDsp, DDspVoice
  {$IFDEF PUREPASCAL},DAVDBufferMathPascal{$ELSE},DAVDBufferMathAsm{$ENDIF};

constructor TDspBaseComponent.Create(AOwner: TComponent);
begin
  inherited;
  fNextDspQueueItem := nil;
  fPrevDspQueueItem := nil;
  fEnabled          := true;
  fBypass           := false;
  fSampleRate       := 44100;
  fChannels         := 2;
  fTrailingSamples  := 0;

  fStdProcessS   := ProcessBypass;
  fStdProcessD   := ProcessBasic;
  fStdProcessSA  := ProcessBasic;
  fStdProcessDA  := ProcessBasic;
  fStdProcessSAA := ProcessBasic;
  fStdProcessDAA := ProcessBasic;

  fStdProcessQueueS  := ProcessQueueBasic;
  fStdProcessQueueD  := ProcessQueueBasic;
  fStdProcessQueueSA := ProcessQueueBasic;
  fStdProcessQueueDA := ProcessQueueBasic;
  fStdProcessQueueSAA:= ProcessQueueBasic;
  fStdProcessQueueDAA:= ProcessQueueBasic;

  Init;

  SetEnabled(fEnabled);
  SetBypass(fBypass);

  RegisterInOwner(self);
end;

constructor TDspBaseComponent.Create(AOwner: TComponent; UseSampleRate: Integer);
begin
  Create(AOwner);
  SetSampleRate(UseSampleRate);
end;

destructor TDspBaseComponent.Destroy;
begin
  BeforeDestroy;

  if assigned(fNextDspQueueItem) then
  begin
    fNextDspQueueItem.PrevDspQueueItem := fPrevDspQueueItem;
    if not assigned(fPrevDspQueueItem) then RegisterInOwner(fNextDspQueueItem);
  end;
  if assigned(fPrevDspQueueItem) then fPrevDspQueueItem.NextDspQueueItem := fNextDspQueueItem;

  UnRegisterInOwner(self);
  inherited;
end;

procedure TDspBaseComponent.BeforeDestroy;
begin end;

procedure TDspBaseComponent.RegisterInOwner(item: TDspBaseComponent);
begin
  if      Owner is TDspVSTModule then (Owner as TDspVSTModule).RegisterDSPItem(item)
  else if Owner is TDspVoice     then (Owner as TDspVoice    ).RegisterDSPItem(item);
end;

procedure TDspBaseComponent.UnRegisterInOwner(item: TDspBaseComponent);
begin
  if      Owner is TDspVSTModule then (Owner as TDspVSTModule).UnRegisterDSPItem(item)
  else if Owner is TDspVoice     then (Owner as TDspVoice    ).UnRegisterDSPItem(item);
end;

procedure TDspBaseComponent.Init;  begin end;
procedure TDspBaseComponent.Reset; begin end;
procedure TDspBaseComponent.UpdateParameters; begin end;
procedure TDspBaseComponent.NoteOff; begin end;

procedure TDspBaseComponent.SampleRateChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ChannelsChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.TrailingSamplesChanged;
begin
  UpdateParameters;
end;

procedure TDspBaseComponent.ResetQueue;
begin
  Reset;
  if assigned(fNextDspQueueItem) then fNextDspQueueItem.ResetQueue;
end;


function TDspBaseComponent.GetFollowingItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(fNextDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=fNextDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Add(fNextDspQueueItem);
    Result:=Result and fNextDspQueueItem.GetFollowingItems(items);
  end;
end;

function TDspBaseComponent.GetPreviousItems(var items: TDspQueueList): boolean;
var i: integer;
begin
  Result:=true;
  if not assigned(items) then
    items:=TDspQueueList.Create(false);

  if assigned(fPrevDspQueueItem) then
  begin
    for i:=items.Count-1 downto 0 do
      if items[i]=fPrevDspQueueItem then
      begin
        Result:=false;
        exit;
      end;

    items.Insert(0, fPrevDspQueueItem);
    Result:=Result and fNextDspQueueItem.GetPreviousItems(items);
  end;
end;

function TDspBaseComponent.GetQueueItems(var items: TDspQueueList): boolean;
var mypos: integer;
begin
  result:=GetPreviousItems(items);
  if not result then exit;

  mypos:=items.Count;

  result:=result and GetFollowingItems(items);
  if not result then exit;
  items.Insert(mypos,self);
end;

procedure TDspBaseComponent.SetNextDspQueueItem(const Value: TDspBaseComponent);
var x: TDspQueueList; backup: TDspBaseComponent;
begin
  if (Value<>self) and (fNextDspQueueItem<>Value) then
  begin
    backup := fNextDspQueueItem;
    fNextDspQueueItem := Value;
    if Value<>nil then
    begin
      x:=nil;
      if (fPrevDspQueueItem=Value) or not (GetQueueItems(x)) then
      begin
        fNextDspQueueItem := backup;
        raise Exception.Create('Processing queue loopback');
        exit;
      end else begin
        if not assigned(fNextDspQueueItem.PrevDspQueueItem) then
          UnRegisterInOwner(fNextDspQueueItem);

        fNextDspQueueItem.PrevDspQueueItem:=self;
      end;
    end;

    // Important
    SetEnabled(fEnabled);
    SetBypass(fBypass);
  end;
end;

procedure TDspBaseComponent.SetSampleRate(const Value: Single);
begin
  if (fSampleRate<>Value) and (Value>0) then
  begin
    fSampleRate := Value;
    SampleRateChanged;
    
    if assigned(fNextDspQueueItem) then fNextDspQueueItem.SampleRate:=fSampleRate;
  end;
end;

procedure TDspBaseComponent.SetChannels(const Value: Integer);
begin
  if (fChannels<>Value) and (Value>0) then
  begin
    fChannels := Value;
    ChannelsChanged;

    if assigned(fNextDspQueueItem) then fNextDspQueueItem.Channels:=fChannels;
  end;
end;

procedure TDspBaseComponent.SetTrailingSamples(const Value: Integer);
begin
  if (fTrailingSamples<>Value) and (Value>=0) then
  begin
    fTrailingSamples := Value;
    TrailingSamplesChanged;

     if Owner is TDspVoice then
      (Owner as TDspVoice).UpdateTrailingSamples;
  end;
end;

procedure TDspBaseComponent.SetBypass(const Value: Boolean);
begin
  fBypass := Value;
  if not fEnabled then exit;

  if Value then
  begin
    // bypass everything
    fProcessS  := ProcessBypass;
    fProcessD  := ProcessBypass;
    fProcessSA := ProcessBypass;
    fProcessDA := ProcessBypass;
    fProcessSAA:= ProcessBypass;
    fProcessDAA:= ProcessBypass;

    if assigned(fNextDspQueueItem) then
    begin
      // ignore this item and call directly the next item
      fProcessQueueS  := ProcessQueueBypass;
      fProcessQueueD  := ProcessQueueBypass;
      fProcessQueueSA := ProcessQueueBypass;
      fProcessQueueDA := ProcessQueueBypass;
      fProcessQueueSAA:= ProcessQueueBypass;
      fProcessQueueDAA:= ProcessQueueBypass;
    end else begin
      // bypass this item and there is no next item, so don't call it
      fProcessQueueS  := ProcessBypass;
      fProcessQueueD  := ProcessBypass;
      fProcessQueueSA := ProcessBypass;
      fProcessQueueDA := ProcessBypass;
      fProcessQueueSAA:= ProcessBypass;
      fProcessQueueDAA:= ProcessBypass;
    end;
  end else begin
    // process item
    fProcessS  := fStdProcessS;
    fProcessD  := fStdProcessD;
    fProcessSA := fStdProcessSA;
    fProcessDA := fStdProcessDA;
    fProcessSAA:= fStdProcessSAA;
    fProcessDAA:= fStdProcessDAA;

    if assigned(fNextDspQueueItem) then
    begin
      // process this item and pass output to the next
      fProcessQueueS  := fStdProcessQueueS;
      fProcessQueueD  := fStdProcessQueueD;
      fProcessQueueSA := fStdProcessQueueSA;
      fProcessQueueDA := fStdProcessQueueDA;
      fProcessQueueSAA:= fStdProcessQueueSAA;
      fProcessQueueDAA:= fStdProcessQueueDAA;
    end else begin
      // only process this item
      fProcessQueueS  := fStdProcessS;
      fProcessQueueD  := fStdProcessD;
      fProcessQueueSA := fStdProcessSA;
      fProcessQueueDA := fStdProcessDA;
      fProcessQueueSAA:= fStdProcessSAA;
      fProcessQueueDAA:= fStdProcessDAA;
    end;
  end;
end;

procedure TDspBaseComponent.SetEnabled(const Value: Boolean);
begin
  if Value then
  begin
    // enable everything
    fProcessS  := fStdProcessS;
    fProcessD  := fStdProcessD;
    fProcessSA := fStdProcessSA;
    fProcessDA := fStdProcessDA;
    fProcessSAA:= fStdProcessSAA;
    fProcessDAA:= fStdProcessDAA;

    if assigned(fNextDspQueueItem) then
    begin
      // process this item and pass output to the next
      fProcessQueueS  := fStdProcessQueueS;
      fProcessQueueD  := fStdProcessQueueD;
      fProcessQueueSA := fStdProcessQueueSA;
      fProcessQueueDA := fStdProcessQueueDA;
      fProcessQueueSAA:= fStdProcessQueueSAA;
      fProcessQueueDAA:= fStdProcessQueueDAA;
    end else begin
      // only Process this item
      fProcessQueueS  := fStdProcessS;
      fProcessQueueD  := fStdProcessD;
      fProcessQueueSA := fStdProcessSA;
      fProcessQueueDA := fStdProcessDA;
      fProcessQueueSAA:= fStdProcessSAA;
      fProcessQueueDAA:= fStdProcessDAA;
    end;
  end else begin
    // disable everything
    fProcessS  := ProcessSilence;
    fProcessD  := ProcessSilence;
    fProcessSA := ProcessSilence;
    fProcessDA := ProcessSilence;
    fProcessSAA:= ProcessSilence;
    fProcessDAA:= ProcessSilence;

    // disable this item and don't process the next one
    fProcessQueueS  := ProcessSilence;
    fProcessQueueD  := ProcessSilence;
    fProcessQueueSA := ProcessSilence;
    fProcessQueueDA := ProcessSilence;
    fProcessQueueSAA:= ProcessSilence;
    fProcessQueueDAA:= ProcessSilence;
  end;
  fEnabled := Value;
  if fEnabled and fBypass then SetBypass(true);
end;



procedure TDspBaseComponent.ProcessSilence(var Data: Single; const channel: integer);
begin
  Data := 0;
end;

procedure TDspBaseComponent.ProcessSilence(var Data: Double; const channel: integer);
begin
  Data := 0;
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Single), 0);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
begin
 FillChar(ProcessBuffer[0], SampleFrames * SizeOf(Double), 0);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);
end;

procedure TDspBaseComponent.ProcessSilence(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
begin
  ClearArrays(ProcessBuffer, fChannels, SampleFrames);
end;





procedure TDspBaseComponent.ProcessBypass(var Data: Single; const channel: integer);
begin
 // Do nothing
end;

procedure TDspBaseComponent.ProcessBypass(var Data: Double; const channel: integer);
begin
 // Do nothing
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
begin
 // Do nothing with the buffer
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
begin
 // Do nothing with the buffer
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
begin
 // Do nothing with the buffer
end;

procedure TDspBaseComponent.ProcessBypass(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
begin
 // Do nothing with the buffer
end;






procedure TDspBaseComponent.ProcessBasic(var Data: Double; const channel: integer);
var tmp: single;
begin
  tmp := Data;
  fProcessS(tmp, channel);
  Data := tmp;
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do fProcessS(ProcessBuffer[i], channel);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
var i: integer;
begin
 for i := 0 to SampleFrames - 1
  do fProcessD(ProcessBuffer[i], channel);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to fChannels - 1
  do fProcessSA(ProcessBuffer[i], i, SampleFrames);
end;

procedure TDspBaseComponent.ProcessBasic(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
var i: integer;
begin
 for i := 0 to fChannels - 1
  do fProcessDA(ProcessBuffer[i], i, SampleFrames);
end;




procedure TDspBaseComponent.ProcessQueueBasic(var Data: Single; const channel: integer);
begin
 fProcessS(Data, channel);
 fNextDspQueueItem.ProcessQueueS(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var Data: Double; const channel: integer);
begin
 fProcessD(Data, channel);
 fNextDspQueueItem.ProcessQueueD(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
begin
 fProcessSA(ProcessBuffer, channel, SampleFrames);
 fNextDspQueueItem.ProcessQueueSA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
begin
 fProcessDA(ProcessBuffer, channel, SampleFrames);
 fNextDspQueueItem.ProcessQueueDA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
begin
 fNextDspQueueItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBasic(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
begin
 fNextDspQueueItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
end;




procedure TDspBaseComponent.ProcessQueueBypass(var Data: Single; const channel: integer);
begin
 fNextDspQueueItem.ProcessQueueS(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var Data: Double; const channel: integer);
begin
 fNextDspQueueItem.ProcessQueueD(Data, channel);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDSingleDynArray; const channel, SampleFrames: integer);
begin
 fNextDspQueueItem.ProcessQueueSA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDDoubleDynArray; const channel, SampleFrames: integer);
begin
 fNextDspQueueItem.ProcessQueueDA(ProcessBuffer, channel, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfSingleDynArray; const SampleFrames: integer);
begin
 fNextDspQueueItem.ProcessQueueSAA(ProcessBuffer, SampleFrames);
end;

procedure TDspBaseComponent.ProcessQueueBypass(var ProcessBuffer: TAVDArrayOfDoubleDynArray; const SampleFrames: integer);
begin
 fNextDspQueueItem.ProcessQueueDAA(ProcessBuffer, SampleFrames);
end;

procedure TDspBaseComponent.ProcessMidiEvent(MidiEvent: TAVDMidiEvent; var FilterEvent: Boolean);
begin end;

procedure TDspBaseComponent.ProcessMidiEventQueue(MidiEvent: TAVDMidiEvent; var FilterEvent: Boolean);
begin
  FilterEvent:=false;
  ProcessMidiEvent(MidiEvent, FilterEvent);

  if not FilterEvent and assigned(fNextDspQueueItem) then
    fNextDspQueueItem.ProcessMidiEventQueue(MidiEvent, FilterEvent);
end;

function TDspBaseComponent.GetTrailingSamplesQueue: integer;
begin
  result:=fTrailingSamples;
  if assigned(fNextDspQueueItem) then result:=max(result, fNextDspQueueItem.TrailingSamplesQueue);
end;

procedure TDspBaseComponent.NoteOffQueue;
begin
  NoteOff;
  if assigned(fNextDspQueueItem) then fNextDspQueueItem.NoteOffQueue;
end;

end.
