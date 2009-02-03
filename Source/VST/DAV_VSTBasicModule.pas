unit DAV_VSTBasicModule;

// This unit implements the basic VST-Plugin <--> Host communications

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, {$IFDEF Delphi5} Forms, {$ENDIF} DAV_Common, DAV_VSTEffect;

type
  TBasicVSTModule = class({$IFDEF UseDelphi}TDataModule{$ELSE}TComponent{$ENDIF})
  protected
    FEffect      : TVSTEffect;
    FAudioMaster : TAudioMasterCallbackFunc;
    {$IFNDEF UseDelphi}
    FOnCreate    : TNotifyEvent;
    FOnDestroy   : TNotifyEvent;
    {$ENDIF}

    function GetEffect: PVSTEffect; virtual;

    procedure SetAudioMaster(const AM: TAudioMasterCallbackFunc); virtual;

    function  GetMasterVersion: Integer; virtual;
    function  GetCurrentUniqueID: TChunkName; virtual;
    procedure MasterIdle; virtual;
    function  IsInputConnected(const Input: Integer): Boolean; virtual;
    function  IsOutputConnected(const Output: Integer): Boolean; virtual;

    procedure WantEvents(const Filter: Integer);  // filter is currently ignored, midi channel data only (default) virtual void wantEvents (long filter = 1); default is 1 for this!
    function  GetTimeInfo(const Filter: Integer): PVstTimeInfo; virtual;  // returns const VstTimeInfo* (or 0 if not supported) filter should contain a mask indicating which fields are requested (see valid masks in aeffectx.h), as some items may require extensive conversions
    procedure SetTimeInfo(const Filter: Integer; var VstTimeInfo: PVstTimeInfo); virtual;
    function  TempoAt(const pos: Integer): Integer; virtual; // returns tempo (in bpm * 10000) at sample frame location <pos>
    function  SendVstEventsToHost(var Events: TVstEvents): Boolean;  // True: success

    function  GetNumAutomatableParameters: Integer; virtual;
    procedure SetParameterAutomated(Index: Integer; const Value: Single); virtual;
    function  GetParameterQuantization: Integer; virtual; // returns the Integer Value for +1.0 representation, or 1 if full single float precision is maintained in automation. parameter Index in <Value> (-1: all, any)

    function  GetInputLatency: Integer; virtual;
    function  GetOutputLatency: Integer; virtual;
    function  GetPreviousPlug(const Input: Integer): PVSTEffect; virtual;  // input can be -1 in which case the first found is returned
    function  GetNextPlug(const Output: Integer): PVSTEffect; virtual;     // output can be -1 in which case the first found is returned

    function  WillProcessReplacing: Integer; virtual; // returns 0: not implemented, 1: replacing, 2: accumulating
    function  GetCurrentProcessLevel: Integer; virtual;  // returns: 0: not supported, 1: currently in user thread (gui) 2: currently in audio thread or irq (where Process is called) 3: currently in 'sequencer' thread or irq (midi, timer etc) 4: currently offline Processing and thus in user thread other: not defined, but probably pre-empting user thread.
    function  GetAutomationState: Integer; virtual;  // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

    function  OfflineRead(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption; const ReadSource: Boolean): Boolean; virtual;
    function  OfflineWrite(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption): Boolean; virtual;
    function  OfflineStart(var AudioFile: TVstAudioFile; const numAudioFiles, numNewAudioFiles: Integer): Boolean; virtual;
    function  OfflineGetCurrentPass: Integer; virtual;
    function  OfflineGetCurrentMetaPass: Integer; virtual;

    procedure SetOutputSampleRate(const Samplerate: Single); virtual;

    function  GetHostVendorString(const Text: PAnsiChar): Boolean; virtual;  // fills <Text> with a string identifying the vendor (max 64 char)
    function  GetHostProductString(const Text: PAnsiChar): Boolean; virtual; // fills <Text> with a string with product name (max 64 char)
    function  GetHostVendorVersion: Integer; virtual;  // returns vendor-specific version
    function  HostVendorSpecific(const Arg1, Arg2: Integer; const ptrArg: pointer; const floatArg: Single): Integer; virtual;  // no definition
    function  GetCanHostDo(Text: string): Integer; virtual;  // see 'hostCanDos' in audioeffectx.cpp returns 0: don't know (default), 1: yes, -1: no
    function  GetHostLanguage: Integer; virtual;   // returns VstHostLanguage
    function  OpenWindow(const aWindow: PVstWindow): pointer; virtual;  // create new window
    function  CloseWindow(const aWindow: PVstWindow): Boolean; virtual; // close a newly created window
    function  GetDirectory: Pointer; virtual;  // get the plug's directory, FSSpec on mac, else char*

    function  UpdateDisplay: Boolean; virtual; // something has changed, update 'multi-fx' display returns True if supported
    function  IOChanged: Boolean; virtual;   // tell host numInputs and/or numOutputs and/or numParameters has changed
    function  NeedIdle: Boolean; virtual;    // plug needs idle calls (outside its editor window)
    function  SizeWindow(const Width, Height: Integer): Boolean; virtual;

    function  BeginEdit(const Index: Integer): Boolean; virtual; // to be called before a setParameterAutomated with mouse move (one per Mouse Down)
    function  EndEdit(const Index: Integer): Boolean; virtual;   // to be called after a setParameterAutomated (on Mouse Up)

    function  OpenFileSelector(var VstFileSelect: TVstFileSelect): Boolean; virtual;
    function  CloseFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
    function  GetChunkFile(const NativePath: Pointer): Boolean;

    // HostCalls, protected methods that can be overwritten, but shall remain
    // hidden, since the user should not be able to call them directly!
    function HostCallOpen                      (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallClose                     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetProgramm               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetProgramm               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetProgramName            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetProgramName            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetParamLabel             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetParamDisplay           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetParamName              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetVu                     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetSampleRate             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetBlockSize              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallMainsChanged              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditGetRect               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditOpen                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditClose                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditDraw                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditMouse                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditKey                   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditIdle                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditTop                   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditSleep                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallIdentify                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetChunk                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetChunk                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallProcessEvents             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallCanBeAutomated            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallString2Parameter          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetNumProgramCategories   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetProgramNameIndexed     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallCopyProgram               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallConnectInput              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallConnectOutput             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetInputProperties        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetOutputProperties       (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetPlugCategory           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetCurrentPosition        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetDestinationBuffer      (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallOfflineNotify             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallOfflinePrepare            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallOfflineRun                (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallProcessVarIo              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetSpeakerArrangement     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetBlockSizeAndSampleRate (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetBypass                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetEffectName             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetErrorText              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetVendorString           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetProductString          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetVendorVersion          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallVendorSpecific            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallCanDo                     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetTailSize               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallIdle                      (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetIcon                   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetViewPosition           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetParameterProperties    (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallKeysRequired              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetVstVersion             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditKeyDown               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEditKeyUp                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetEditKnobMode           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetMidiProgramName        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetCurrentMidiProgram     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetMidiProgramCategory    (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallHasMidiProgramsChanged    (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetMidiKeyName            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallBeginSetProgram           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallEndSetProgram             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetSpeakerArrangement     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallShellGetNextPlugin        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallStartProcess              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallStopProcess               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetTotalSampleToProcess   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetPanLaw                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallBeginLoadBank             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallBeginLoadProgram          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallSetProcessPrecision       (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetNumMidiInputChannels   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;
    function HostCallGetNumMidiOutputChannels  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; virtual;

    procedure HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); virtual; abstract;
    procedure HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); virtual; abstract;
    procedure HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer); virtual; abstract;

    procedure HostCallDispatchEffect(const Opcode: TDispatcherOpcode; const Index, Value: Integer; const ptr: pointer; const opt: Single); virtual; abstract;

    function  UpdateSampleRate: Double; virtual;  // gets and returns sample rate from host (may issue setSampleRate() )
    function  UpdateBlockSize: Integer; virtual;  // same for block size

    function  GetInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function  GetOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    class function GetStaticDescription: string; virtual;
    function  HostCallGetParameter(const Index: Integer): Single; virtual; abstract;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); virtual; abstract;

    property Effect: PVSTEffect read GetEffect;
    property AudioMaster: TAudioMasterCallbackFunc read FAudioMaster write SetAudioMaster;
    {$IFNDEF UseDelphi}
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    {$ENDIF}
  end;

function  DispatchEffectFunc(Effect: PVSTEffect; OpCode : TDispatcherOpCode; const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; cdecl;
function  GetParameterFunc(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
procedure SetParameterFunc(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
procedure ProcessFunc(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure ProcessReplacingFunc(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
procedure ProcessDoubleReplacingFunc(const Effect: PVSTEffect; const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;

implementation

uses
  Sysutils;

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
   resvd2          := nil;
   user            := Self;
   uniqueID        := 'fEoN';
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
    raise Exception.Create('AudioMaster Error');
end;



// ------------------------------------------------------------------
// Calls to the Host
// ------------------------------------------------------------------
function TBasicVSTModule.GetMasterVersion: Integer;
var
  vers: Integer;
begin
 vers := 1;
 if Assigned(FAudioMaster) then
  begin
   vers := FAudioMaster(@FEffect, audioMasterVersion, 0, 0, nil, 0);
   if vers = 0 then vers := 1;
  end;

 Result := vers;
end;

function TBasicVSTModule.GetCurrentUniqueId: TChunkName;
begin
 if Assigned(FAudioMaster)
  then Result := TChunkName(FAudioMaster(@FEffect, audioMasterCurrentId, 0, 0, nil, 0))
  else Result := #0#0#0#0;
end;

procedure TBasicVSTModule.MasterIdle;
begin
 if Assigned(FAudioMaster) then FAudioMaster(@FEffect, audioMasterIdle, 0, 0, nil, 0);
end;

function TBasicVSTModule.IsInputConnected(const Input: Integer): Boolean;
var
  ret: Integer;
begin
 ret := 0;
 if Assigned(FAudioMaster)
  then ret := FAudioMaster(@FEffect, audioMasterPinConnected, input, 0, nil, 0);

 Result := (ret = 0);
end;

function TBasicVSTModule.IsOutputConnected(const Output: Integer): Boolean;
var
  ret: Integer;
begin
 ret := 0;
 if Assigned(FAudioMaster)
  then ret := FAudioMaster(@FEffect, audioMasterPinConnected, output, 1, nil, 0);

 Result := (ret = 0);
end;

procedure TBasicVSTModule.WantEvents(const Filter: Integer);
begin
 if Assigned(FAudioMaster)
  then FAudioMaster(@FEffect, audioMasterWantMidi, 0, filter, nil, 0);
end;

function TBasicVSTModule.GetTimeInfo(const Filter: Integer): PVstTimeInfo;
begin
 if Assigned(FAudioMaster)
  then Result := PVstTimeInfo(FAudioMaster (@FEffect, audioMasterGetTime, 0, filter, nil, 0))
  else Result := nil;
end;

procedure TBasicVSTModule.SetTimeInfo(const Filter: Integer; var VstTimeInfo: PVstTimeInfo);
begin
 if Assigned(FAudioMaster)
  then FAudioMaster(@FEffect, audioMasterSetTime, 0, Filter, @VstTimeInfo, 0);
end;

function TBasicVSTModule.TempoAt(const Pos: Integer): Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterTempoAt, 0, pos, nil, 0)
  else Result := 0;
end;

function TBasicVSTModule.SendVstEventsToHost(var Events: TVstEvents): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterProcessEvents, 0, 0, @Events, 0) = 1
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

procedure TBasicVSTModule.SetParameterAutomated(Index: Integer; const Value: Single);
begin
  if Assigned(FAudioMaster)
   then FAudioMaster(@FEffect, audioMasterAutomate, Index, 0, nil, Value);
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

function TBasicVSTModule.GetPreviousPlug(const Input: Integer): PVSTEffect;
begin
  if Assigned(FAudioMaster) then
    Result := PVSTEffect(FAudioMaster(@FEffect, audioMasterGetPreviousPlug, 0, 0, nil, 0))
  else
    Result := nil;
end;

class function TBasicVSTModule.GetStaticDescription: string;
begin
 result := 'Delphi ASIO & VST Package Plugin';
end;

function TBasicVSTModule.GetNextPlug(const Output: Integer): PVSTEffect;
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

function TBasicVSTModule.OfflineRead(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption; const ReadSource: Boolean): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineRead, Integer(ReadSource), Integer(Option), @Offline, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.OfflineWrite(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineWrite, 0, Integer(Option), @Offline, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.OfflineStart(var AudioFile: TVstAudioFile; const numAudioFiles, numNewAudioFiles: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOfflineStart, numNewAudioFiles, numAudioFiles, @AudioFile, 0) <> 0)
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

procedure TBasicVSTModule.SetOutputSampleRate(const SampleRate: Single);
begin
  if Assigned(FAudioMaster) then
    FAudioMaster(@FEffect, audioMasterSetOutputSampleRate, 0, 0, nil, SampleRate);
end;

function TBasicVSTModule.GetHostVendorString(const Text: PAnsiChar): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterGetVendorString, 0, 0, Text, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.GetHostProductString(const Text: PAnsiChar): Boolean;
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

function TBasicVSTModule.HostVendorSpecific(const Arg1, Arg2: Integer; const ptrArg: Pointer; const floatArg: Single): Integer;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterVendorSpecific, Arg1, Arg2, ptrArg, floatArg);
end;

function TBasicVSTModule.GetCanHostDo(Text: string): Integer;
begin
  Result := 0;
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterCanDo, 0, 0, PChar(Text), 0);
end;

function TBasicVSTModule.GetHostLanguage: Integer;
begin
  if Assigned(FAudioMaster) then
    Result := FAudioMaster(@FEffect, audioMasterGetLanguage, 0, 0, nil, 0)
  else
    Result := 0;
end;

function TBasicVSTModule.OpenWindow(const aWindow: PVstWindow): pointer;
begin
  if Assigned(FAudioMaster) then
    Result := pointer(FAudioMaster(@FEffect, audioMasterOpenWindow, 0, 0, aWindow, 0))
  else
    Result := nil;
end;

function TBasicVSTModule.CloseWindow(const aWindow: PVstWindow): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterCloseWindow, 0, 0, aWindow, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.GetDirectory: Pointer;
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

function TBasicVSTModule.SizeWindow(const Width, Height: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterSizeWindow, Width, Height, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.BeginEdit(const Index: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterBeginEdit, Index, 0, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.EndEdit(const Index: Integer): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterEndEdit, Index, 0, nil, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.OpenFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterOpenFileSelector, 0, 0, @VstFileSelect, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.CloseFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
begin
  if Assigned(FAudioMaster) then
    Result := (FAudioMaster(@FEffect, audioMasterCloseFileSelector, 0, 0, @VstFileSelect, 0) <> 0)
  else
    Result := False;
end;

function TBasicVSTModule.GetChunkFile(const NativePath: Pointer): Boolean;
begin
  if Assigned(FAudioMaster) and (nativePath <> nil) then
    Result := (FAudioMaster(@FEffect, audioMasterGetChunkFile, 0, 0, NativePath, 0) <> 0)
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

function TBasicVSTModule.HostCallOpen(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallClose(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProgramm(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramm(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProgramName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamLabel(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamDisplay(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVu(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSampleRate(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSize(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallMainsChanged(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditGetRect(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditOpen(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditClose(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditDraw(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditMouse(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKey(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditIdle(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditTop(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditSleep(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdentify(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  ChunkName : TChunkName;
begin
 ChunkName := 'fEvN';
 Result := Integer(ChunkName);
end;

function TBasicVSTModule.HostCallGetChunk(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetChunk(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessEvents(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanBeAutomated(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallString2Parameter(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumProgramCategories(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramNameIndexed(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallCopyProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallConnectInput(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallConnectOutput(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetInputProperties(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetOutputProperties(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetPlugCategory(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentPosition(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetDestinationBuffer(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineNotify(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflinePrepare(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineRun(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessVarIo(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSpeakerArrangement(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSizeAndSampleRate(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBypass(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetEffectName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetErrorText(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorString(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProductString(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorVersion(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallVendorSpecific(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanDo(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetTailSize(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdle(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetIcon(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetViewPosition(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParameterProperties(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallKeysRequired(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVstVersion(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyDown(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyUp(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetEditKnobMode(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentMidiProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramCategory(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallHasMidiProgramsChanged(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiKeyName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginSetProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallEndSetProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetSpeakerArrangement(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallShellGetNextPlugin(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallStartProcess(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallStopProcess(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetTotalSampleToProcess(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetPanLaw(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadBank(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadProgram(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProcessPrecision(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiInputChannels(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiOutputChannels(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin Result := 0; end;


// effect functions

function DispatchEffectFunc(Effect: PVSTEffect; OpCode : TDispatcherOpCode; const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; cdecl;
begin
 if assigned(Effect) and (TObject(Effect^.vObject) is TBasicVSTModule) then
  with TBasicVSTModule(Effect^.vObject) do
   begin
    HostCallDispatchEffect(OpCode, Index, Value, ptr, opt);

    case OpCode of
     effOpen:                      Result := HostCallOpen(Index, Value, ptr, opt);
     effClose:                     try
                                    HostCallClose(Index, Value, ptr, opt);
                                    Effect^.vObject := nil;
                                    Free;
                                    Result := 1;
                                   except
                                    Result := 0;
                                   end;
     effSetProgram:                Result := HostCallSetProgramm(Index, Value, ptr, opt);
     effGetProgram:                Result := HostCallGetProgramm(Index, Value, ptr, opt);
     effSetProgramName:            Result := HostCallSetProgramName(Index, Value, ptr, opt);
     effGetProgramName:            Result := HostCallGetProgramName(Index, Value, ptr, opt);
     effGetParamLabel:             Result := HostCallGetParamLabel(Index, Value, ptr, opt);
     effGetParamDisplay:           Result := HostCallGetParamDisplay(Index, Value, ptr, opt);
     effGetParamName:              Result := HostCallGetParamName(Index, Value, ptr, opt);
     effGetVu:                     Result := HostCallGetVu(Index, Value, ptr, opt);
     effSetSampleRate:             Result := HostCallSetSampleRate(Index, Value, ptr, opt);
     effSetBlockSize:              Result := HostCallSetBlockSize(Index, Value, ptr, opt);
     effMainsChanged:              Result := HostCallMainsChanged(Index, Value, ptr, opt);
     effEditGetRect:               Result := HostCallEditGetRect(Index, Value, ptr, opt);
     effEditOpen:                  Result := HostCallEditOpen(Index, Value, ptr, opt);
     effEditClose:                 Result := HostCallEditClose(Index, Value, ptr, opt);
     effEditDraw:                  Result := HostCallEditDraw(Index, Value, ptr, opt);
     effEditMouse:                 Result := HostCallEditMouse(Index, Value, ptr, opt);
     effEditKey:                   Result := HostCallEditKey(Index, Value, ptr, opt);
     effEditIdle:                  Result := HostCallEditIdle(Index, Value, ptr, opt);
     effEditTop:                   Result := HostCallEditTop(Index, Value, ptr, opt);
     effEditSleep:                 Result := HostCallEditSleep(Index, Value, ptr, opt);
     effIdentify:                  Result := HostCallIdentify(Index, Value, ptr, opt);
     effGetChunk:                  Result := HostCallGetChunk(Index, Value, ptr, opt);
     effSetChunk:                  Result := HostCallSetChunk(Index, Value, ptr, opt);
     effProcessEvents:             Result := HostCallProcessEvents(Index, Value, ptr, opt);
     effCanBeAutomated:            Result := HostCallCanBeAutomated(Index, Value, ptr, opt);
     effString2Parameter:          Result := HostCallString2Parameter(Index, Value, ptr, opt);
     effGetNumProgramCategories:   Result := HostCallGetNumProgramCategories(Index, Value, ptr, opt);
     effGetProgramNameIndexed:     Result := HostCallGetProgramNameIndexed(Index, Value, ptr, opt);
     effCopyProgram:               Result := HostCallCopyProgram(Index, Value, ptr, opt);
     effConnectInput:              Result := HostCallConnectInput(Index, Value, ptr, opt);
     effConnectOutput:             Result := HostCallConnectOutput(Index, Value, ptr, opt);
     effGetInputProperties:        Result := HostCallGetInputProperties(Index, Value, ptr, opt);
     effGetOutputProperties:       Result := HostCallGetOutputProperties(Index, Value, ptr, opt);
     effGetPlugCategory:           Result := HostCallGetPlugCategory(Index, Value, ptr, opt);
     effGetCurrentPosition:        Result := HostCallGetCurrentPosition(Index, Value, ptr, opt);
     effGetDestinationBuffer:      Result := HostCallGetDestinationBuffer(Index, Value, ptr, opt);
     effOfflineNotify:             Result := HostCallOfflineNotify(Index, Value, ptr, opt);
     effOfflinePrepare:            Result := HostCallOfflinePrepare(Index, Value, ptr, opt);
     effOfflineRun:                Result := HostCallOfflineRun(Index, Value, ptr, opt);
     effProcessVarIo:              Result := HostCallProcessVarIo(Index, Value, ptr, opt);
     effSetSpeakerArrangement:     Result := HostCallSetSpeakerArrangement(Index, Value, ptr, opt);
     effSetBlockSizeAndSampleRate: Result := HostCallSetBlockSizeAndSampleRate(Index, Value, ptr, opt);
     effSetBypass:                 Result := HostCallSetBypass(Index, Value, ptr, opt);
     effGetEffectName:             Result := HostCallGetEffectName(Index, Value, ptr, opt);
     effGetErrorText:              Result := HostCallGetErrorText(Index, Value, ptr, opt);
     effGetVendorString:           Result := HostCallGetVendorString(Index, Value, ptr, opt);
     effGetProductString:          Result := HostCallGetProductString(Index, Value, ptr, opt);
     effGetVendorVersion:          Result := HostCallGetVendorVersion(Index, Value, ptr, opt);
     effVendorSpecific:            Result := HostCallVendorSpecific(Index, Value, ptr, opt);
     effCanDo:                     Result := HostCallCanDo(Index, Value, ptr, opt);
     effGetTailSize:               Result := HostCallGetTailSize(Index, Value, ptr, opt);
     effIdle:                      Result := HostCallIdle(Index, Value, ptr, opt);
     effGetIcon:                   Result := HostCallGetIcon(Index, Value, ptr, opt);
     effSetViewPosition:           Result := HostCallSetViewPosition(Index, Value, ptr, opt);
     effGetParameterProperties:    Result := HostCallGetParameterProperties(Index, Value, ptr, opt);
     effKeysRequired:              Result := HostCallKeysRequired(Index, Value, ptr, opt);
     effGetVstVersion:             Result := HostCallGetVstVersion(Index, Value, ptr, opt);
     effEditKeyDown:               Result := HostCallEditKeyDown(Index, Value, ptr, opt);
     effEditKeyUp:                 Result := HostCallEditKeyUp(Index, Value, ptr, opt);
     effSetEditKnobMode:           Result := HostCallSetEditKnobMode(Index, Value, ptr, opt);
     effGetMidiProgramName:        Result := HostCallGetMidiProgramName(Index, Value, ptr, opt);
     effGetCurrentMidiProgram:     Result := HostCallGetCurrentMidiProgram(Index, Value, ptr, opt);
     effGetMidiProgramCategory:    Result := HostCallGetMidiProgramCategory(Index, Value, ptr, opt);
     effHasMidiProgramsChanged:    Result := HostCallHasMidiProgramsChanged(Index, Value, ptr, opt);
     effGetMidiKeyName:            Result := HostCallGetMidiKeyName(Index, Value, ptr, opt);
     effBeginSetProgram:           Result := HostCallBeginSetProgram(Index, Value, ptr, opt);
     effEndSetProgram:             Result := HostCallEndSetProgram(Index, Value, ptr, opt);
     effGetSpeakerArrangement:     Result := HostCallGetSpeakerArrangement(Index, Value, ptr, opt);
     effShellGetNextPlugin:        Result := HostCallShellGetNextPlugin(Index, Value, ptr, opt);
     effStartProcess:              Result := HostCallStartProcess(Index, Value, ptr, opt);
     effStopProcess:               Result := HostCallStopProcess(Index, Value, ptr, opt);
     effSetTotalSampleToProcess:   Result := HostCallSetTotalSampleToProcess(Index, Value, ptr, opt);
     effSetPanLaw:                 Result := HostCallSetPanLaw(Index, Value, ptr, opt);
     effBeginLoadBank:             Result := HostCallBeginLoadBank(Index, Value, ptr, opt);
     effBeginLoadProgram:          Result := HostCallBeginLoadProgram(Index, Value, ptr, opt);
     effSetProcessPrecision:       Result := HostCallSetProcessPrecision(Index, Value, ptr, opt);
     effGetNumMidiInputChannels:   Result := HostCallGetNumMidiInputChannels(Index, Value, ptr, opt);
     effGetNumMidiOutputChannels:  Result := HostCallGetNumMidiOutputChannels(Index, Value, ptr, opt);
     else
       try
         raise Exception.Create('Unknown OpCode');
       except
         Result := 0;
       end;
     end;
   end
 else Result := 0;
end;

function GetParameterFunc(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
 assert(assigned(Effect));
 if TObject(Effect^.vObject) is TBasicVSTModule
  then Result := TBasicVSTModule(Effect^.vObject).HostCallGetParameter(Index)
  else Result := 0;
end;

procedure SetParameterFunc(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
 assert(assigned(Effect));
 if TObject(Effect^.vObject) is TBasicVSTModule
  then TBasicVSTModule(Effect^.vObject).HostCallSetParameter(Index, Value);
end;

procedure ProcessFunc(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
begin
 assert(assigned(Effect));
 if TObject(Effect^.vObject) is TBasicVSTModule
  then TBasicVSTModule(Effect^.vObject).HostCallProcess(Inputs, Outputs, SampleFrames);
end;

procedure ProcessReplacingFunc(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
begin
 assert(assigned(Effect));
 if TObject(Effect^.vObject) is TBasicVSTModule
  then TBasicVSTModule(Effect^.vObject).HostCallProcessReplacing(Inputs, Outputs, SampleFrames);
end;

procedure ProcessDoubleReplacingFunc(const Effect: PVSTEffect; const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;
begin
 assert(assigned(Effect));
 if TObject(Effect^.vObject) is TBasicVSTModule
  then TBasicVSTModule(Effect^.vObject).HostCallProcessDoubleReplacing(Inputs, Outputs, SampleFrames);
end;

function GetParameterFuncDummy(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
end;

procedure SetParameterFuncDummy(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
end;

procedure ProcessFuncDummy(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
begin
end;

procedure ProcessReplacingFuncDummy(const Effect: PVSTEffect; const Inputs, Outputs: PPSingle; const SampleFrames: Integer); cdecl;
begin
end;

procedure ProcessDoubleReplacingFuncDummy(const Effect: PVSTEffect; const Inputs, Outputs: PPDouble; const SampleFrames: Integer); cdecl;
begin
end;

end.
