unit DVSTBasicModule;

// This unit implements the basic VST-Plugin <--> Host communications

interface

{$I ASIOVST.INC}

uses Classes, DVSTEffect;

type
  TBasicVSTModule = class({$IFDEF UseDelphi}TDataModule{$ELSE}TComponent{$ENDIF})
  protected
    FEffect: TVSTEffect;
    FAudioMaster: TAudioMasterCallbackFunc;
    {$IFNDEF UseDelphi}
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    {$ENDIF}

    function GetEffect: PVSTEffect; virtual;

    procedure SetAudioMaster(const AM :TAudioMasterCallbackFunc); virtual;

    function  GetMasterVersion: Integer; virtual;
    function  GetCurrentUniqueID: Integer; virtual;
    procedure MasterIdle; virtual;
    function  IsInputConnected(input: Integer): Boolean; virtual;
    function  IsOutputConnected(output: Integer): Boolean; virtual;

    procedure WantEvents(filter: Integer);  // filter is currently ignored, midi channel data only (default) virtual void wantEvents (long filter = 1); default is 1 for this!
    function  GetTimeInfo(filter: Integer): PVstTimeInfo; virtual;  // returns const VstTimeInfo* (or 0 if not supported) filter should contain a mask indicating which fields are requested (see valid masks in aeffectx.h), as some items may require extensive conversions
    procedure SetTimeInfo(filter: Integer; ti: PVstTimeInfo); virtual;
    function  TempoAt(pos: Integer): Integer; virtual; // returns tempo (in bpm * 10000) at sample frame location <pos>
    function  SendVstEventsToHost(events: PVstEvents): Boolean;  // True: success

    function  GetNumAutomatableParameters: Integer; virtual;
    procedure SetParameterAutomated(Index: Integer; Value: Single); virtual;
    function  GetParameterQuantization: Integer; virtual; // returns the Integer Value for +1.0 representation, or 1 if full single float precision is maintained in automation. parameter Index in <Value> (-1: all, any)

    function  GetInputLatency: Integer; virtual;
    function  GetOutputLatency: Integer; virtual;
    function  GetPreviousPlug(input: Integer): PVSTEffect; virtual;  // input can be -1 in which case the first found is returned
    function  GetNextPlug(output: Integer): PVSTEffect; virtual;     // output can be -1 in which case the first found is returned

    function  WillProcessReplacing: Integer; virtual; // returns 0: not implemented, 1: replacing, 2: accumulating
    function  GetCurrentProcessLevel: Integer; virtual;  // returns: 0: not supported, 1: currently in user thread (gui) 2: currently in audio thread or irq (where Process is called) 3: currently in 'sequencer' thread or irq (midi, timer etc) 4: currently offline Processing and thus in user thread other: not defined, but probably pre-empting user thread.
    function  GetAutomationState: Integer; virtual;  // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

    function  OfflineRead(offline: PVstOfflineTask; option: TVstOfflineOption; readSource: Boolean): Boolean; virtual;
    function  OfflineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): Boolean; virtual;
    function  OfflineStart(ptr: PVstAudioFile; numAudioFiles: Integer; numNewAudioFiles: Integer): Boolean; virtual;
    function  OfflineGetCurrentPass: Integer; virtual;
    function  OfflineGetCurrentMetaPass: Integer; virtual;

    procedure SetOutputSampleRate(samplerate: Single); virtual;

    function  GetHostVendorString(Text: pchar): Boolean; virtual;  // fills <Text> with a string identifying the vendor (max 64 char)
    function  GetHostProductString(Text: pchar): Boolean; virtual; // fills <Text> with a string with product name (max 64 char)
    function  GetHostVendorVersion: Integer; virtual;  // returns vendor-specific version
    function  HostVendorSpecific(lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer; virtual;  // no definition
    function  CanHostDo(Text: pchar): Integer; virtual;  // see 'hostCanDos' in audioeffectx.cpp returns 0: don't know (default), 1: yes, -1: no
    function  GetHostLanguage: Integer; virtual;   // returns VstHostLanguage
    function  OpenWindow(aWindow: PVstWindow): pointer; virtual;  // create new window
    function  CloseWindow(aWindow: PVstWindow): Boolean; virtual; // close a newly created window
    function  GetDirectory: pointer; virtual;  // get the plug's directory, FSSpec on mac, else char*

    function  UpdateDisplay: Boolean; virtual; // something has changed, update 'multi-fx' display returns True if supported
    function  IOChanged: Boolean; virtual;   // tell host numInputs and/or numOutputs and/or numParameters has changed
    function  NeedIdle: Boolean; virtual;    // plug needs idle calls (outside its editor window)
    function  SizeWindow(width, height: Integer): Boolean; virtual;

    function  BeginEdit(Index: Integer): Boolean; virtual;  // to be called before a setParameterAutomated with mouse move (one per Mouse Down)
    function  EndEdit(Index: Integer): Boolean; virtual;    // to be called after a setParameterAutomated (on Mouse Up)
    
    function  OpenFileSelector(ptr: PVstFileSelect): Boolean; virtual;
    function  CloseFileSelector(ptr: PVstFileSelect): Boolean;
    function  GetChunkFile(nativePath: pointer): Boolean;

    function  UpdateSampleRate: Double; virtual;  // gets and returns sample rate from host (may issue setSampleRate() )
    function  UpdateBlockSize: Integer; virtual;  // same for block size

    function  GetInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function  GetOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); virtual; abstract;
    procedure HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); virtual; abstract;
    procedure HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer); virtual; abstract;

    function  HostCallGetParameter(Index: Integer): Single; virtual; abstract;
    procedure HostCallSetParameter(Index: Integer; Value: Single); virtual; abstract;

    procedure HostCallDispatchEffect(opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single); virtual; abstract;

    function HostCallOpen                      (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallClose                     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetProgramm               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetProgramm               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetProgramName            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetProgramName            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetParamLabel             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetParamDisplay           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetParamName              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetVu                     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetSampleRate             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetBlockSize              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallMainsChanged              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditGetRect               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditOpen                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditClose                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditDraw                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditMouse                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditKey                   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditIdle                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditTop                   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditSleep                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallIdentify                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetChunk                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetChunk                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallProcessEvents             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallCanBeAutomated            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallString2Parameter          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetNumProgramCategories   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetProgramNameIndexed     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallCopyProgram               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallConnectInput              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallConnectOutput             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetInputProperties        (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetOutputProperties       (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetPlugCategory           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetCurrentPosition        (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetDestinationBuffer      (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallOfflineNotify             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallOfflinePrepare            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallOfflineRun                (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallProcessVarIo              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetSpeakerArrangement     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetBlockSizeAndSampleRate (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetBypass                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetEffectName             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetErrorText              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetVendorString           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetProductString          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetVendorVersion          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallVendorSpecific            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallCanDo                     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetTailSize               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallIdle                      (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetIcon                   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetViewPosition           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetParameterProperties    (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallKeysRequired              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetVstVersion             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditKeyDown               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEditKeyUp                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetEditKnobMode           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetMidiProgramName        (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetCurrentMidiProgram     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetMidiProgramCategory    (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallHasMidiProgramsChanged    (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetMidiKeyName            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallBeginSetProgram           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallEndSetProgram             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetSpeakerArrangement     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallShellGetNextPlugin        (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallStartProcess              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallStopProcess               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetTotalSampleToProcess   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetPanLaw                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallBeginLoadBank             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallBeginLoadProgram          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallSetProcessPrecision       (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetNumMidiInputChannels   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    function HostCallGetNumMidiOutputChannels  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;

    property Effect: PVSTEffect read GetEffect;
    property AudioMaster: TAudioMasterCallbackFunc read FAudioMaster write SetAudioMaster;
    {$IFNDEF UseDelphi}
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    {$ENDIF}
  end;

implementation

uses
  Sysutils, DVSTEffectFunctions;

{ TBasicVSTModule }
constructor TBasicVSTModule.Create(AOwner: TComponent);
begin
  {$IFDEF UseDelphi} inherited CreateNew(AOwner); {$ENDIF}
  with FEffect do
  begin
   vObject         := Self;
   magic           := 'PtsV';
   EffectFlags     := [effFlagsCanReplacing];
   reservedForHost := nil;
   resvd2          := 0;
   user            := nil;
   uniqueID        := FourCharToLong('N', 'o', 'E', 'f');
   ioRatio         := 1;
   numParams       := 0;
   numPrograms     := 0;
   numInputs       := 2;
   numOutputs      := 2;

   dispatcher             := @DispatchEffectFunc;
   Process                := @ProcessFunc;
   setParameter           := @setParameterFunc;
   GetParameter           := @getParameterFunc;
   ProcessReplacing       := ProcessReplacingFunc;
   ProcessDoubleReplacing := ProcessDoubleReplacingFunc;
  end;
end;

function TBasicVSTModule.GetEffect: PVSTEffect;
begin
 Result := @FEffect;
end;

procedure TBasicVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
begin
  FAudioMaster := AM;
  if FAudioMaster(nil, audioMasterVersion, 0, 0, nil, 0) = 0 then
    raise exception.Create('AudioMaster Error');
end;



// ------------------------------------------------------------------
// Calls to the Host
// ------------------------------------------------------------------
function TBasicVSTModule.GetMasterVersion: Integer;
var vers: Integer;
begin
  vers := 1;
  if Assigned(FAudioMaster) then
  begin
    vers := FAudioMaster(@FEffect, audioMasterVersion, 0, 0, nil, 0);
    if vers = 0 then vers := 1;
  end;

  Result := vers;
end;

function TBasicVSTModule.GetCurrentUniqueId: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterCurrentId, 0, 0, nil, 0)
  else
    Result := 0;
end;

procedure TBasicVSTModule.MasterIdle;
begin
  if Assigned(FAudioMaster) then FAudioMaster(@FEffect, audioMasterIdle, 0, 0, nil, 0);
end;

function TBasicVSTModule.IsInputConnected(input: Integer): Boolean;
var ret: Integer;
begin
  ret := 0;
  if Assigned(FAudioMaster) then ret := FAudioMaster(@FEffect, audioMasterPinConnected, input, 0, nil, 0);

  Result := (ret = 0);
end;

function TBasicVSTModule.IsOutputConnected(output: Integer): Boolean;
var ret: Integer;
begin
  ret := 0;
  if Assigned(FAudioMaster) then
    ret := FAudioMaster(@FEffect, audioMasterPinConnected, output, 1, nil, 0);

  Result := (ret = 0);
end;

procedure TBasicVSTModule.WantEvents(filter: Integer);
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterWantMidi, 0, filter, nil, 0);
end;

function TBasicVSTModule.GetTimeInfo(filter: Integer): PVstTimeInfo;
begin
  if Assigned(FAudioMaster) then
    Result := PVstTimeInfo(FAudioMaster (@FEffect, audioMasterGetTime, 0, filter, nil, 0))
  else
    Result := nil;
end;

procedure TBasicVSTModule.SetTimeInfo(filter: Integer; ti: PVstTimeInfo);
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterSetTime, 0, filter, ti, 0);
end;

function TBasicVSTModule.TempoAt(pos: Integer): Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterTempoAt, 0, pos, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.SendVstEventsToHost(events: PVstEvents): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterProcessEvents, 0, 0, events, 0) = 1
  else
    Result := False;
end;

function TBasicVSTModule.GetNumAutomatableParameters: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetNumAutomatableParameters, 0, 0, nil, 0)
  else
    Result := 0;
end;

procedure TBasicVSTModule.SetParameterAutomated(Index: Integer; Value: Single);
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterAutomate, Index, 0, nil, Value);
end;

function TBasicVSTModule.GetParameterQuantization: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetParameterQuantization, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.GetInputLatency: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetInputLatency, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.GetOutputLatency: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetOutputLatency, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.GetPreviousPlug(input: Integer): PVSTEffect;
begin
  if Assigned(FAudioMaster) then
    Result := PVSTEffect(FAudioMaster(@FEffect, audioMasterGetPreviousPlug, 0, 0, nil, 0))
  else
    Result := nil;
end;

function TBasicVSTModule.GetNextPlug(output: Integer): PVSTEffect;
begin
  if Assigned(FAudioMaster) then
    Result := PVSTEffect(FAudioMaster(@FEffect, audioMasterGetNextPlug, 0, 0, nil, 0))
  else
    Result := nil;
end;

function TBasicVSTModule.WillProcessReplacing: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterWillReplaceOrAccumulate, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.GetCurrentProcessLevel: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetCurrentProcessLevel, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.GetAutomationState: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetAutomationState, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.OfflineRead(Offline: PVstOfflineTask; Option: TVstOfflineOption; readSource: Boolean): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineRead, Integer(readSource), Integer(option), offline, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.OfflineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineWrite, 0, Integer(option), offline, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.OfflineStart(ptr: PVstAudioFile; numAudioFiles: Integer; numNewAudioFiles: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineStart, numNewAudioFiles, numAudioFiles, ptr, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.OfflineGetCurrentPass: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := Integer(FAudioMaster(@FEffect, audioMasterOfflineGetCurrentPass, 0, 0, nil, 0) <> 0)
  else
    Result := 0;
end;

function TBasicVSTModule.OfflineGetCurrentMetaPass: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := Integer(FAudioMaster(@FEffect, audioMasterOfflineGetCurrentMetaPass, 0, 0, nil, 0) <> 0)
  else
    Result := 0;
end;

procedure TBasicVSTModule.SetOutputSampleRate(sampleRate: Single);
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterSetOutputSampleRate, 0, 0, nil, sampleRate);
end;

function TBasicVSTModule.GetHostVendorString(Text: pchar): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterGetVendorString, 0, 0, Text, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.GetHostProductString(Text: pchar): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterGetProductString, 0, 0, Text, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.GetHostVendorVersion: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetVendorVersion, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.HostVendorSpecific(lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterVendorSpecific, lArg1, lArg2, ptrArg, floatArg);
end;

function TBasicVSTModule.CanHostDo(Text: pchar): Integer;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterCanDo, 0, 0, Text, 0);
end;

function TBasicVSTModule.GetHostLanguage: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetLanguage, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.OpenWindow(aWindow: PVstWindow): pointer;
begin
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterOpenWindow, 0, 0, aWindow, 0))
  else
    Result := nil;
end;

function TBasicVSTModule.CloseWindow(aWindow: PVstWindow): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterCloseWindow, 0, 0, aWindow, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.GetDirectory: pointer;
begin
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterGetDirectory, 0, 0, nil, 0))
  else
    Result := nil;
end;

function TBasicVSTModule.UpdateDisplay: Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterUpdateDisplay, 0, 0, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.IOChanged: Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterIOChanged, 0, 0, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.NeedIdle: Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterNeedIdle, 0, 0, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.SizeWindow(width, height: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterSizeWindow, width, height, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.BeginEdit(Index: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterBeginEdit, Index, 0, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.EndEdit(Index: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterEndEdit, Index, 0, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.OpenFileSelector(ptr: PVstFileSelect): Boolean;
begin
  if Assigned(FAudioMaster) and (ptr <> nil) then
    Result := (FAudioMaster(@FEffect, audioMasterOpenFileSelector, 0, 0, ptr, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.CloseFileSelector(ptr: PVstFileSelect): Boolean;
begin
  if Assigned(FAudioMaster) and (ptr <> nil) then
    Result := (FAudioMaster(@FEffect, audioMasterCloseFileSelector, 0, 0, ptr, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.GetChunkFile(nativePath: pointer): Boolean;
begin
  if Assigned(FAudioMaster) and (nativePath <> nil) then
    Result := (FAudioMaster(@FEffect, audioMasterGetChunkFile, 0, 0, nativePath, 0) <> 0)
  else
    Result := False;
end;


function TBasicVSTModule.UpdateSampleRate: Double;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetSampleRate, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.UpdateBlockSize: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetBlockSize, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.GetInputSpeakerArrangement: PVstSpeakerArrangement;
begin
  if Assigned(FAudioMaster) then
    Result := PVstSpeakerArrangement(FAudioMaster(@FEffect, audioMasterGetInputSpeakerArrangement, 0, 0, nil, 0))
  else
    Result := nil;
end;

function TBasicVSTModule.GetOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
  if Assigned(FAudioMaster) then
    Result := PVstSpeakerArrangement(FAudioMaster(@FEffect, audioMasterGetOutputSpeakerArrangement, 0, 0, nil, 0))
  else
    Result := nil;
end;



// ------------------------------------------------------------------
// Calls from the host
// ------------------------------------------------------------------

function TBasicVSTModule.HostCallOpen(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallClose(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProgramm(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramm(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProgramName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamLabel(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamDisplay(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVu(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSampleRate(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSize(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallMainsChanged(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditGetRect(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditOpen(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditClose(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditDraw(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditMouse(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKey(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditIdle(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditTop(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditSleep(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdentify(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := FourCharToLong('N', 'v', 'E', 'f'); end;

function TBasicVSTModule.HostCallGetChunk(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetChunk(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessEvents(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanBeAutomated(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallString2Parameter(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumProgramCategories(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramNameIndexed(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallCopyProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallConnectInput(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallConnectOutput(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetInputProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetOutputProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetPlugCategory(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentPosition(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetDestinationBuffer(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineNotify(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflinePrepare(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineRun(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessVarIo(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSpeakerArrangement(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSizeAndSampleRate(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBypass(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetEffectName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetErrorText(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorString(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProductString(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorVersion(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallVendorSpecific(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanDo(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetTailSize(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdle(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetIcon(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetViewPosition(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParameterProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallKeysRequired(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVstVersion(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyDown(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyUp(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetEditKnobMode(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentMidiProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramCategory(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallHasMidiProgramsChanged(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiKeyName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginSetProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEndSetProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetSpeakerArrangement(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallShellGetNextPlugin(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallStartProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallStopProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetTotalSampleToProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetPanLaw(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadBank(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadProgram(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProcessPrecision(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiInputChannels(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiOutputChannels(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin Result := 0; end;


end.
