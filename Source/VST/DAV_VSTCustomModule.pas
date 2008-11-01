unit DAV_VSTCustomModule;

interface

{$I ..\ASIOVST.INC}

uses
  {$IFDEF FPC}LCLIntf, LMessages, Controls, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Forms, DAV_Common, DAV_VSTEffect, DAV_VSTChannels,
  DAV_VSTBasicModule, DAV_VSTShellPlugins, DAV_VSTOfflineTask;

type
//  TChannelPropertyFlags = set of (cpfIsActive, cpfIsStereo, cpfUseSpeaker);

  TProcessAudioEvent     = procedure(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer) of object;
  TProcessDoubleEvent    = procedure(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer) of object;
  TGetVUEvent            = procedure(var VU: Single) of object;
  TBlockSizeChangeEvent  = procedure(Sender: TObject; const BlockSize: Integer) of object;
  TSampleRateChangeEvent = procedure(Sender: TObject; const SampleRate: Single) of object;
  TOnDispatcherEvent     = procedure(Sender: TObject; const opCode: TDispatcherOpcode) of object;
  TOfflineNotifyEvent    = procedure(Sender: TObject; const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean) of object;
  TOfflinePrepareEvent   = procedure(Sender: TObject; const OfflineTasks: array of TVstOfflineTask) of object;
  TOfflineRunEvent       = procedure(Sender: TObject; const OfflineTasks: array of TVstOfflineTask) of object;
  TVSTKeyEvent           = procedure(Sender: TObject; var keyCode : TVstKeyCode) of object;
  TProcessVarIOEvent     = procedure(Sender: TObject; const varIo: TVstVariableIo) of object;
  TInOutConnectedEvent   = procedure(Sender: TObject; const Index: Integer; const State: Boolean) of object;
  TSetKnobModeEvent      = procedure(Sender: TObject; val: Integer) of object;
  TSoftBypassEvent       = procedure(Sender: TObject; const isBypass: Boolean) of object;
  TOnSetPanLawEvent      = procedure(Sender: TObject; var vType: Integer; var val: single) of object;
  TGetEditorEvent        = procedure(Sender: TObject; var GUI: TForm; ParentWindow : THandle) of object;
  TOnVendorSpecificEvent = function(Sender: TObject; const lArg1, lArg2: Integer; const ptrArg: pointer; const floatArg: Single): Integer of object;
  TOnCanDoEvent          = function(Sender: TObject; const CanDoText: string): Integer of object;
  TOnCheckKey            = function(Sender: TObject; Key: Char): Boolean of object;
  TOnEditClose           = procedure(Sender: TObject; var DestroyForm: Boolean) of object;

  TOnGetChannelPropertiesEvent = function(Sender: TObject; const Index: Integer; var vLabel: ShortString; var shortLabel: ShortString; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean of object;

  TCustomVSTModule = class(TBasicVSTModule)
  private
    FAbout                  : string;
    FVersion                : string;

    FOnEditClose            : TOnEditClose;
    FOnEditIdle             : TNotifyEvent;
    FOnEditTop              : TNotifyEvent;
    FOnEditSleep            : TNotifyEvent;

    FOnInitialize           : TNotifyEvent;
    FOnResume               : TNotifyEvent;
    FOnSuspend              : TNotifyEvent;
    FOnGetVUEvent           : TGetVUEvent;
    FBlockSizeChangeEvent   : TBlockSizeChangeEvent;
    FSampleRateChangeEvent  : TSampleRateChangeEvent;
    FOnDispatcher           : TOnDispatcherEvent;
    FProcessPrecisition     : TProcessPrecision;
    FTempo                  : Single;
    FTailSize               : Integer;
    FCanDos                 : TVstCanDos;
    FPlugCategory           : TVstPluginCategory;
    FKeysRequired           : Boolean;

    FOnOfflineNotify        : TOfflineNotifyEvent;
    FOnOfflinePrepare       : TOfflinePrepareEvent;
    FOnOfflineRun           : TOfflineRunEvent;
    FOnProcessVarIO         : TProcessVarIOEvent;
    FOnKeyUp                : TVSTKeyEvent;
    FOnKeyDown              : TVSTKeyEvent;
    FOnSetKnobMode          : TSetKnobModeEvent;
    FOnInConnected          : TInOutConnectedEvent;
    FOnOutConnected         : TInOutConnectedEvent;
    FOnSoftBypass           : TSoftBypassEvent;
    FOnStartProcess         : TNotifyEvent;
    FOnStopProcess          : TNotifyEvent;
    FOnSetPanLaw            : TOnSetPanLawEvent;
    FOnVendorSpecific       : TOnVendorSpecificEvent;
    FOnCanDo                : TOnCanDoEvent;
    FOnGetInputProperties   : TOnGetChannelPropertiesEvent;
    FOnGetOutputProperties  : TOnGetChannelPropertiesEvent;
    FOnCheckKey             : TOnCheckKey;
    FVstShellPlugins        : TCustomVstShellPlugins;
    FCurrentVstShellPlugin  : Integer;
    {$IFDEF Debug} FLog     : TStringList; {$ENDIF}
    {$IFDEF Debug} FTmStmp  : TDateTime; {$ENDIF}

    procedure SetVstShellPlugins(const Value: TCustomVstShellPlugins);
    procedure SetKeysRequired(const Value: Boolean);
    procedure ReadOnlyString(s: string); virtual;
    function GetHostProduct: string;
    function GetHostVendor: string;
  protected
    FTruncateStrings        : Boolean;
    FNumCategories          : Integer;
    FEditorNeedUpdate       : Boolean;
    FEditorForm             : TForm;
    FEditorRect             : ERect;
    FSampleRate             : Single;
    FBlockSize              : Integer;
    FEffectName             : string;
    FVendorName             : string;
    FVersionMajor           : Integer;
    FVersionMinor           : Integer;
    FVersionRelease         : Integer;
    FProductName            : string;
    FHostProduct            : string;
    FHostVendor             : string;
    FInitialDelay           : Integer;
    FOnOpen                 : TNotifyEvent;
    FOnClose                : TNotifyEvent;
    FOnEditOpen             : TGetEditorEvent;
    FOnProcessEx            : TProcessAudioEvent;
    FOnProcessReplacingEx   : TProcessAudioEvent;
    FOnProcessDoublesEx     : TProcessDoubleEvent;

    function GetPluginFlags: TEffFlags; virtual;
    function GetUniqueID: string; virtual;
    function OfflinePrepare(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer; virtual;
    function OfflineRun(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer; virtual; 
    procedure SetAudioMaster(const AM: TAudioMasterCallbackFunc); override;
    procedure SetBlockSize(newValue: Integer); virtual;
    procedure SetInitialDelay(delay: Integer); virtual;
    procedure SetNumInputs(Inputs: Integer); virtual;
    procedure SetNumOutputs(Outputs: Integer); virtual;
    procedure SetPluginFlags(newFlags : TEffFlags); virtual;
    procedure SetSampleRate(newValue: Single); virtual;
    procedure SetUniqueID(Value: string); virtual;
    {$IFDEF UseDelphi}
    procedure ReadState(Reader: TReader); override;
    {$ENDIF}
    procedure SetEffectName(const Value: string);
    procedure SetProductName(const Value: string);
    procedure SetVendorName(const Value: string);
    procedure SetVersionMajor(Value: Integer);
    procedure SetVersionMinor(Value: Integer);
    procedure SetVersionRelease(Value: Integer);
    procedure UpdateVersion;
    procedure SampleRateChanged; virtual;
    procedure BlockSizeChanged; virtual;

    procedure HostCallDispatchEffect(opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single); override;
    procedure HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer); override;

    // HostCalls, protected methods that can be overwritten, but shall remain
    // hidden, since the user should not be able to call them directly!
    function HostCallOpen                      (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallClose                     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetVu                     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetSampleRate             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetBlockSize              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallMainsChanged              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditGetRect               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditOpen                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditClose                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditIdle                  (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditTop                   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditSleep                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallProcessEvents             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallConnectInput              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallConnectOutput             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetInputProperties        (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetOutputProperties       (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetPlugCategory           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallOfflineNotify             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallOfflinePrepare            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallOfflineRun                (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallProcessVarIo              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetBlockSizeAndSampleRate (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetBypass                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetEffectName             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetVendorString           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetProductString          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetVendorVersion          (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallVendorSpecific            (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallCanDo                     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetTailSize               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallKeysRequired              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetVstVersion             (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditKeyDown               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallEditKeyUp                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetEditKnobMode           (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallGetSpeakerArrangement     (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallShellGetNextPlugin        (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallStartProcess              (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallStopProcess               (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetTotalSampleToProcess   (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetPanLaw                 (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;
    function HostCallSetProcessPrecision       (Index, Value: Integer; ptr: pointer; opt: Single): Integer; override;

    function AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean; virtual;   // Allocate memory for a VstSpeakerArrangement containing the given number of channels
    function DeallocateArrangement(var Arrangement: PVstSpeakerArrangement): Boolean; virtual;                      // Delete/free memory for a speaker Arrangement
    function CopySpeaker(copyTo, copyFrom: PVstSpeakerProperties): Boolean; virtual;    // Feed the "to" speaker Properties with the same Values than "from"'s ones. It is assumed here that "to" exists yet, ie this function won't allocate memory for the speaker (this will prevent from having a difference between an Arrangement's number of channels and its actual speakers...)
    function MatchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): Boolean; virtual;    // "to" is deleted, then created and initialized with the same Values as "from" ones ("from" must exist).
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   EditorPostUpdate; virtual;

    function UpdateSampleRate: Double; override;
    function UpdateBlockSize: Integer; override;

    {$IFDEF Debug} property DebugLog: TStringList read fLog; {$ENDIF}

    // Properties
    property EditorForm: TForm read FEditorForm;
    property EditorNeedUpdate: Boolean read FEditorNeedUpdate write FEditorNeedUpdate;

    property Flags: TEffFlags read GetPluginFlags write SetPluginFlags default [effFlagsCanReplacing];
    property CanDo[canDo: string]: Integer read GetCanHostDo;

    property About: string read FAbout write ReadOnlyString stored False;
    property CanDos: TVstCanDos read fCanDos write fCanDos default [];
    property EffectName: string read FEffectName write SetEffectName;
    property HostProduct: string read GetHostProduct stored false;
    property HostVendor: string read GetHostVendor stored false;
    property HostVersion: Integer read GetHostVendorVersion stored false;
    property InitialDelay: Integer read FEffect.initialDelay write SetInitialDelay default 0;
    property IORatio: Single read FEffect.ioRatio write FEffect.ioRatio;
    property KeysRequired: Boolean read FKeysRequired write SetKeysRequired default False;
    property numCategories: Integer read fNumCategories write fNumCategories default 1;
    property numInputs: Integer read FEffect.numInputs write SetNumInputs default 2;
    property numOutputs: Integer read FEffect.numOutputs write SetNumOutputs default 2;
    property OffQualities: Integer read FEffect.offQualities write FEffect.offQualities default 0;
    property PlugCategory: TVstPluginCategory read fPlugCategory write fPlugCategory default vpcUnknown;
    property ProcessPrecisition: TProcessPrecision read FProcessPrecisition write FProcessPrecisition default pp32;
    property ProductName: string read fProductName write SetProductName;
    property RealQualities: Integer read FEffect.realQualities write FEffect.realQualities default 0;
    property SampleRate: Single read fSampleRate write SetSampleRate;
    property ShellPlugins: TCustomVstShellPlugins read FVstShellPlugins write SetVstShellPlugins;
    property TailSize: Integer read FTailSize write FTailSize default 0;
    property Tempo: Single read fTempo;
    property TruncateStrings: Boolean read FTruncateStrings write FTruncateStrings default False;
    property UniqueID: string read GetUniqueID write SetUniqueID;
    property VendorName: string read fVendorName write SetVendorName;
    property Version: string read FVersion write FVersion;
    property VersionMajor: Integer read FVersionMajor write SetVersionMajor default 1;
    property VersionMinor: Integer read FVersionMinor write SetVersionMinor default 0;
    property VersionRelease: Integer read FVersionRelease write SetVersionRelease default 0;

    property OnBlockSizeChange: TBlockSizeChangeEvent read fBlockSizeChangeEvent write fBlockSizeChangeEvent;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnResume: TNotifyEvent read FOnResume write FOnResume;
    property OnSuspend: TNotifyEvent read FOnSuspend write FOnSuspend;
    property OnEditOpen: TGetEditorEvent read FOnEditOpen write FOnEditOpen;
    property OnEditClose: TOnEditClose read FOnEditClose write FOnEditClose;
    property OnEditIdle: TNotifyEvent read FOnEditIdle write FOnEditIdle;
    property OnEditTop: TNotifyEvent read FOnEditTop write FOnEditTop;
    property OnEditSleep: TNotifyEvent read FOnEditSleep write FOnEditSleep;
    property OnSampleRateChange: TSampleRateChangeEvent read fSampleRateChangeEvent write fSampleRateChangeEvent;
    property OnGetVU: TGetVUEvent read FOnGetVUEvent write FOnGetVUEvent;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnDispatcher: TOnDispatcherEvent read FOnDispatcher write FOnDispatcher;
    property OnSoftBypass: TSoftBypassEvent read FOnSoftBypass write FOnSoftBypass;
    property OnInConnected: TInOutConnectedEvent read FOnInConnected write FOnInConnected;
    property OnOutConnected: TInOutConnectedEvent read FOnOutConnected write FOnOutConnected;
    property OnStartProcess: TNotifyEvent read FOnStartProcess write FOnStartProcess;
    property OnStopProcess: TNotifyEvent read FOnStopProcess write FOnStopProcess;
    property OnEditorKeyUp: TVSTKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnEditorKeyDown: TVSTKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnEditorKnobMode: TSetKnobModeEvent read FOnSetKnobMode write FOnSetKnobMode;
    property OnOfflineNotify: TOfflineNotifyEvent read FOnOfflineNotify write FOnOfflineNotify;
    property OnOfflinePrepare: TOfflinePrepareEvent read FOnOfflinePrepare write FOnOfflinePrepare;
    property OnOfflineRun: TOfflineRunEvent read FOnOfflineRun write FOnOfflineRun;
    property OnProcessVarIO: TProcessVarIOEvent read FOnProcessVarIO write FOnProcessVarIO;
    property OnSetPanLaw: TOnSetPanLawEvent read FOnSetPanLaw write FOnSetPanLaw;
    property OnVendorSpecific: TOnVendorSpecificEvent read FOnVendorSpecific write FOnVendorSpecific;
    property OnCanDo: TOnCanDoEvent read FOnCanDo write FOnCanDo;
    property OnCheckKey: TOnCheckKey read FOnCheckKey write FOnCheckKey;
    property OnInputProperties: TOnGetChannelPropertiesEvent read FOnGetInputProperties write FOnGetInputProperties;
    property OnOutputProperties: TOnGetChannelPropertiesEvent read FOnGetOutputProperties write FOnGetOutputProperties;

    property OnProcess: TProcessAudioEvent read FOnProcessEx write FOnProcessEx;
    property OnProcessReplacing: TProcessAudioEvent read FOnProcessReplacingEx write FOnProcessReplacingEx;
    property OnProcessDoubleReplacing: TProcessDoubleEvent read FOnProcessDoublesEx write FOnProcessDoublesEx;
  end;


implementation

{$IFDEF FPC} {$DEFINE PUREPASCAL} {$ENDIF}

uses
  SysUtils, Math,
  {$IFDEF PUREPASCAL}DAV_BufferMathPascal{$ELSE}DAV_BufferMathAsm{$ENDIF};

constructor TCustomVSTModule.Create(AOwner: TComponent);
begin
 inherited;
 {$IFDEF Debug}
 FLog := TStringList.Create;
 fTmStmp := Now;
 FLog.Add('Create: ' + TimeToStr(fTmStmp));
 FLog.SaveToFile('Debug.log');
 {$ENDIF}
 Randomize;
 FTruncateStrings    := False;
 FVersion            := '0.0';
 FAbout              := 'VST Plugin Template by Christian Budde, Tobybear & MyCo';
 FHostProduct        := '';
 FProcessPrecisition := pp32;
 FKeysRequired       := False;
 FSampleRate         := 44100;
 FBlockSize          := 1024;
 FEditorForm         := nil;
 FTailSize           := 0;
 FVersionMajor       := 1;
 FVersionMinor       := 0;
 FVersionRelease     := 0;
 UpdateVersion;

 FVstShellPlugins := TCustomVstShellPlugins.Create(Self);
 FCurrentVstShellPlugin := 0;
 FNumCategories := 1;
end;

procedure TCustomVSTModule.HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
var
  Ins     : TDAVArrayOfSingleDynArray absolute Inputs;
  Outs    : TDAVArrayOfSingleDynArray absolute Outputs;
  OutsTmp : TDAVArrayOfSingleDynArray;
  i, j    : Integer;
begin
 if Assigned(FOnProcessEx)
  then FOnProcessEx(Ins, Outs, SampleFrames)
  else if Assigned(FOnProcessReplacingEx) then
   begin
    SetLength(OutsTmp, FEffect.NumOutputs, SampleFrames);
    ClearArrays(OutsTmp, FEffect.NumOutputs, SampleFrames);
    FOnProcessReplacingEx(Ins, OutsTmp, SampleFrames);
    for i := 0 to FEffect.NumOutputs - 1 do
     for j := 0 to SampleFrames - 1
      do Outs[i, j] := Outs[i, j] + OutsTmp[i, j];
   end;
end;

procedure TCustomVSTModule.HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
var
  Ins  : TDAVArrayOfSingleDynArray absolute Inputs;
  Outs : TDAVArrayOfSingleDynArray absolute Outputs;
begin
  if Assigned(FOnProcessReplacingEx)
   then FOnProcessReplacingEx(Ins, Outs, SampleFrames);
end;

procedure TCustomVSTModule.HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer);
var
  Ins  : TDAVArrayOfDoubleDynArray absolute Inputs;
  Outs : TDAVArrayOfDoubleDynArray absolute Outputs;
begin
  if Assigned(FOnProcessDoublesEx) then FOnProcessDoublesEx(Ins, Outs,SampleFrames);
end;

destructor TCustomVSTModule.Destroy;
begin
 try
  if Assigned(FEditorForm) then FreeAndNil(FEditorForm);
  if Assigned(FVstShellPlugins) then FreeAndNil(FVstShellPlugins);
  {$IFDEF Debug} if assigned(FLog) then FLog.SaveToFile('Debug.log'); {$ENDIF}
 finally
  {$IFDEF Debug} if assigned(FLog) then FLog.Free; {$ENDIF}
  inherited;
 end;
end;

Procedure TCustomVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
var
  rUID : TChunkName;
  i    : Integer;
  sUID : string;
  hv   : boolean;
begin
 inherited;
 hv := (HostProduct <> 'WaveLab') {or (shortstring(temp)<>'energyXT')};
 if hv then hv := (CanDo['shellCategory'] = 1);

 if (PlugCategory = vpcShell) and hv then
  begin
   rUID := getCurrentUniqueId;
   if (Integer(rUID) > 0) then
    begin
     for i := 0 to ShellPlugins.Count - 1 do
      if rUID = ShellPlugins[i].UniqueID then Break;
     if i < ShellPlugins.Count then
      with ShellPlugins[i] do
       if (rUID = UniqueID) then
        begin
         FEffect.uniqueID := rUID;
         if NumInputs   >= 0 then FEffect.numInputs := NumInputs;
         if NumOutputs  >= 0 then FEffect.numOutputs := NumOutputs;
         if NumPrograms >= 0 then FEffect.numPrograms := NumPrograms;
         if NumParams   >= 0 then FEffect.numParams := NumParams;
         fPlugCategory := PlugCategory;
         if Assigned(OnInstanciate) then
          begin
           sUID := UniqueID;
           OnInstanciate(Self, sUID);
          end;
         IOChanged;
        end;
    end;
  end
 else
  if (PlugCategory = vpcShell)
   then PlugCategory := vpcUnknown;
end;

procedure TCustomVSTModule.HostCallDispatchEffect(opcode: TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single);
begin
 if Assigned(FOnDispatcher) then FOnDispatcher(Self, Opcode);

 {$IFDEF Debug}
 if not (opcode in [effIdle, effEditIdle])
  then FLog.Add(TimeToStr(Now - fTmStmp) +
                ' Opcode: ' + opcode2String(opcode) +
                ' Value: ' + IntToStr(Value));
 FLog.SaveToFile('Debug.log');
 {$ENDIF}
end;

procedure TCustomVSTModule.ReadOnlyString(s: string); begin end;

function TCustomVSTModule.HostCallOpen(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallOpen'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnOpen) then FOnOpen(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallClose(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallClose'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnClose) then FOnClose(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallGetVu(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var s: single;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetVu'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnGetVUEvent) then
  begin
   s := 0;
   FOnGetVUEvent(s);
   Result := round(s * 32767);
  end else Result := 0;
end;

function TCustomVSTModule.HostCallSetSampleRate(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallSetSampleRate'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 setSampleRate(opt);
 Result := 1;
end;

function TCustomVSTModule.HostCallSetBlockSize(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallSetBlockSize'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 setBlockSize(Value);
 Result := 0;
end;

function TCustomVSTModule.HostCallMainsChanged(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallMainsChanged'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if (Value = 0) then
  if Assigned(FOnSuspend)
   then FOnSuspend(Self)
   else
    begin
     if Assigned(FOnResume) then FOnResume(Self);
     wantEvents(1);
    end;

  Result := 0;
end;

function TCustomVSTModule.HostCallEditGetRect(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallEditGetRect'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 PPERect(ptr)^ := @FEditorRect;
 FEditorRect.top := 0;
 FEditorRect.left := 0;

 if Assigned(FEditorForm) then
  begin
   FEditorRect.bottom := FEditorForm.ClientHeight;
   FEditorRect.right := FEditorForm.ClientWidth;
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallEditOpen(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallEditOpen'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := 0;
 if (effFlagsHasEditor in FEffect.EffectFlags) then
  begin
   if Assigned(FOnEditOpen) then FOnEditOpen(Self, FEditorForm, THandle(ptr));
   if Assigned(FEditorForm) then
   try
    Result := 1;
    with FEditorForm do
     begin
      {$IFNDEF FPC}
      ParentWindow := HWnd(ptr);
      {$ELSE}
      Parent := FindOwnerControl(THandle(ptr));
      SetBounds(0, 0, 300, 100);
      {$ENDIF}
      Visible := True;
      BorderStyle := bsNone;
      SetBounds(0, 0, Width, Height);
      Invalidate;
     end;
   except
   end;
  end;
end;

function TCustomVSTModule.HostCallEditClose(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  DestroyForm: Boolean;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallEditClose'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if (effFlagsHasEditor in FEffect.EffectFlags) then
  begin
   DestroyForm := True;
   if Assigned(FOnEditClose) then FOnEditClose(Self, DestroyForm);
   if DestroyForm and Assigned(FEditorForm) then FreeAndNil(FEditorForm);
  end;

 Result := 0;
end;

function TCustomVSTModule.HostCallEditIdle(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 if (effFlagsHasEditor in FEffect.EffectFlags) and FEditorNeedUpdate and Assigned(FEditorForm)then
  begin
   if Assigned(FOnEditIdle) then FOnEditIdle(Self);
   FEditorNeedUpdate := False;
  end;

 Result := 0;
end;

function TCustomVSTModule.HostCallEditTop(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallEditTop'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnEditTop) then FOnEditTop(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallEditSleep(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallEditSleep'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnEditSleep) then FOnEditSleep(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallProcessEvents(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallProcessEvents'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := 1;
end;

function TCustomVSTModule.HostCallConnectInput(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallConnectInput'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnInConnected) then FOnInConnected(Self,Index,(Value <> 0));
 Result := 1;
end;

function TCustomVSTModule.HostCallConnectOutput(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallConnectOutput'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnOutConnected) then FOnOutConnected(Self,Index,(Value <> 0));
 Result := 1;
end;

function TCustomVSTModule.HostCallGetInputProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  str1  : string[63];
  str2  : string[7];
  sat   : TVstSpeakerArrangementType;
  cpf   : TVstPinPropertiesFlags;
begin
 Result := 0;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetInputProperties'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if (Index < FEffect.numInputs) then
  with PVstPinProperties(ptr)^ do
   begin
    str1 := 'Input #' + IntToStr(Index + 1);
    str2 := 'In' + IntToStr(Index + 1);
    sat := satStereo;
    cpf := [vppIsActive, vppIsStereo];

    if Assigned(FOnGetInputProperties)
     then Result := Integer(FOnGetInputProperties(Self, Index, str1, str2, sat, cpf))
     else Result := 1;

    StrPCopy(Caption, str1); // set name of input channel:
    StrPCopy(ShortLabel, str2); // set name of input channel:
    if vppIsActive in cpf then Flags := [vppIsActive] else Flags := [];
    if vppIsStereo in cpf then Flags := Flags + [vppIsStereo];
    if vppUseSpeaker in cpf then Flags := Flags + [vppUseSpeaker];
  end;
end;

function TCustomVSTModule.HostCallGetOutputProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  str1  : string[63];
  str2  : string[7];
  sat   : TVSTSpeakerArrangementType;
  cpf   : TVstPinPropertiesFlags;
begin
 Result := 0;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetOutputProperties'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if (Index < FEffect.numOutputs) then
  with PVstPinProperties(ptr)^ do
   begin
    str1 := 'Output #' + IntToStr(Index + 1);
    str2 := 'Out' + IntToStr(Index + 1);
    sat := satStereo;
    cpf := [vppIsActive, vppIsStereo];

    if Assigned(FOnGetOutputProperties)
     then Result := Integer(FOnGetOutputProperties(Self, index, str1, str2, sat, cpf))
     else Result := 1;

    StrPCopy(Caption, str1); // set name of input channel:
    StrPCopy(shortLabel, str2); // set name of input channel:
    if vppIsActive in cpf then Flags := [vppIsActive] else Flags := [];
    if vppIsStereo in cpf then Flags := Flags + [vppIsStereo];
    if vppUseSpeaker in cpf then Flags := Flags + [vppUseSpeaker];
   end;
end;

function TCustomVSTModule.HostCallGetPlugCategory(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetPlugCategory'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := Integer(FPlugCategory);
end;

function TCustomVSTModule.HostCallOfflineNotify(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallOfflineNotify'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnOfflineNotify) then
  begin
   FOnOfflineNotify(Self, PVstAudioFile(ptr)^, Value, (Index <> 0));
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.OfflinePrepare(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer;
var
  VSTOfflineTasks : array of TVstOfflineTask;
  i               : Integer;
begin
 if Assigned(FOnOfflinePrepare) then
  begin
   SetLength(VSTOfflineTasks, Count);
   for i := 0 to Count - 1 do
    begin
     VSTOfflineTasks[i] := TVstOfflineTask.Create(OfflineTaskStartPointer);
     inc(OfflineTaskStartPointer);
    end;
   FOnOfflinePrepare(Self, VSTOfflineTasks);
   for i := 0 to Count - 1 do FreeAndNil(VSTOfflineTasks[i]);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.OfflineRun(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer;
var
  VSTOfflineTasks : array of TVstOfflineTask;
  i               : Integer;
begin
 if Assigned(FOnOfflinePrepare) then
  begin
   SetLength(VSTOfflineTasks, Count);
   for i := 0 to Count - 1 do
    begin
     VSTOfflineTasks[i] := TVstOfflineTask.Create(OfflineTaskStartPointer);
     inc(OfflineTaskStartPointer);
    end;
   FOnOfflineRun(Self, VSTOfflineTasks);
   for i := 0 to Count - 1 do FreeAndNil(VSTOfflineTasks[i]);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallOfflinePrepare(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallOfflinePrepare'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := OfflinePrepare(PVstOfflineTaskRecord(ptr), Value);
end;

function TCustomVSTModule.HostCallOfflineRun(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallOfflineRun'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := OfflineRun(PVstOfflineTaskRecord(ptr), Value);
end;

function TCustomVSTModule.HostCallProcessVarIo(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallProcessVarIo'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnProcessVarIO) then
  begin
   FOnProcessVarIO(Self, PVstVariableIo(ptr)^);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallSetBlockSizeAndSampleRate(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallSetBlockSizeAndSampleRate: Blocksize ' + IntToStr(Value) + ' Samplerate ' + FloatToStr(opt)); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if fSampleRate <> opt then
  begin
   fSampleRate := opt;
   SampleRateChanged;
  end;
 if fBlockSize <> Value then
  begin
   fBlockSize := Value;
   BlockSizeChanged;
  end;
 Result := 1;
end;

function TCustomVSTModule.HostCallSetBypass(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  {$IFDEF Debug}
  if Value <> 0
   then FLog.Add('SoftBypass: On')
   else FLog.Add('SoftBypass: Off');
  FLog.SaveToFile('Debug.log');
  {$ENDIF}
  if Assigned(FOnSoftBypass) then
   begin
    FOnSoftBypass(Self, Value <> 0);
    Result := 1;
   end else Result := 0;
end;

function TCustomVSTModule.HostCallGetEffectName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetEffectName'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 StrPCopy(ptr, FEffectName);
 Result := 1;
end;

function TCustomVSTModule.HostCallGetVendorString(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetVendorString'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 StrPCopy(ptr, fVendorName);
 Result := 1;
end;

function TCustomVSTModule.HostCallGetProductString(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetProductString'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if fProductName <> ''
  then StrPCopy(ptr, fProductName)
  else StrPCopy(ptr, FEffectName);
 Result := 1;
end;

function TCustomVSTModule.HostCallGetVendorVersion(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetVendorVersion'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := FEffect.Version;
end;

function TCustomVSTModule.HostCallVendorSpecific(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallVendorSpecific'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnVendorSpecific)
  then Result := FOnVendorSpecific(Self, Index, Value, ptr, opt)
  else Result := 0;
end;

function TCustomVSTModule.HostCallCanDo(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 Result := 0;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallCanDo'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if StrComp(ptr, 'receiveVstEvents')      = 0 then Result := 2 * Integer(vcdReceiveVstEvents      in fCanDos)-1 else
 if StrComp(ptr, 'receiveVstMidiEvent')   = 0 then Result := 2 * Integer(vcdReceiveVstMidiEvent   in fCanDos)-1 else
 if StrComp(ptr, 'receiveVstTimeInfo')    = 0 then Result := 2 * Integer(vcdReceiveVstTimeInfo    in fCanDos)-1 else
 if StrComp(ptr, 'sendVstEvents')         = 0 then Result := 2 * Integer(vcdSendVstEvents         in fCanDos)-1 else
 if StrComp(ptr, 'sendVstMidiEvent')      = 0 then Result := 2 * Integer(vcdSendVstMidiEvent      in fCanDos)-1 else
 if StrComp(ptr, 'sendVstTimeInfo')       = 0 then Result := 2 * Integer(vcdSendVstTimeInfo       in fCanDos)-1 else
 if StrComp(ptr, 'offline')               = 0 then Result := 2 * Integer(vcdOffline               in fCanDos)-1 else
 if StrComp(ptr, 'plugAsChannelInsert')   = 0 then Result := 2 * Integer(vcdPlugAsChannelInsert   in fCanDos)-1 else
 if StrComp(ptr, 'plugAsSend')            = 0 then Result := 2 * Integer(vcdPlugAsSend            in fCanDos)-1 else
 if StrComp(ptr, 'mixDryWet')             = 0 then Result := 2 * Integer(vcdMixDryWet             in fCanDos)-1 else
 if StrComp(ptr, 'noRealTime')            = 0 then Result := 2 * Integer(vcdNoRealTime            in fCanDos)-1 else
 if StrComp(ptr, 'multipass')             = 0 then Result := 2 * Integer(vcdMultipass             in fCanDos)-1 else
 if StrComp(ptr, 'metapass')              = 0 then Result := 2 * Integer(vcdMetapass              in fCanDos)-1 else
 if StrComp(ptr, '1in1out')               = 0 then Result := 2 * Integer(vcd1in1out               in fCanDos)-1 else
 if StrComp(ptr, '1in2out')               = 0 then Result := 2 * Integer(vcd1in2out               in fCanDos)-1 else
 if StrComp(ptr, '2in1out')               = 0 then Result := 2 * Integer(vcd2in1out               in fCanDos)-1 else
 if StrComp(ptr, '2in2out')               = 0 then Result := 2 * Integer(vcd2in2out               in fCanDos)-1 else
 if StrComp(ptr, '2in4out')               = 0 then Result := 2 * Integer(vcd2in4out               in fCanDos)-1 else
 if StrComp(ptr, '4in2out')               = 0 then Result := 2 * Integer(vcd4in2out               in fCanDos)-1 else
 if StrComp(ptr, '4in4out')               = 0 then Result := 2 * Integer(vcd4in4out               in fCanDos)-1 else
 if StrComp(ptr, '4in8out')               = 0 then Result := 2 * Integer(vcd4in8out               in fCanDos)-1 else
 if StrComp(ptr, '8in4out')               = 0 then Result := 2 * Integer(vcd8in4out               in fCanDos)-1 else
 if StrComp(ptr, '8in8out')               = 0 then Result := 2 * Integer(vcd8in8out               in fCanDos)-1 else
 if StrComp(ptr, 'midiProgramNames')      = 0 then Result := 2 * Integer(vcdMidiProgramNames      in fCanDos)-1 else
 if StrComp(ptr, 'conformsToWindowRules') = 0 then Result := 2 * Integer(vcdConformsToWindowRules in fCanDos)-1 else
 if StrComp(ptr, 'LiveWithoutToolbar')    = 0 then Result := 2 * Integer(vcdLiveWithoutToolbar    in fCanDos)-1 else
 if StrComp(ptr, 'bypass')                = 0 then Result := 2 * Integer(vcdBypass                in fCanDos)-1 else
 if StrComp(ptr, 'hasCockosExtensions')   = 0 then
  if vcdCockosExtension in fCanDos
   then Result := $BEEF0000
   else Result := 0;
 if Assigned(FOnCanDo) then FOnCanDo(Self,pchar(ptr));
end;

function TCustomVSTModule.HostCallGetTailSize(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetTailSize'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := fTailSize;
end;



function TCustomVSTModule.HostCallKeysRequired(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallKeysRequired'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := Integer(not fKeysRequired); // reversed to keep v1 compatibility
end;

function TCustomVSTModule.HostCallGetVstVersion(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetVstVersion'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := 2400;
end;

function TCustomVSTModule.HostCallEditKeyDown(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  keyCode : TVstKeyCode;
  a,b     : Integer;
  Hndl    : THandle;
begin
 Result := 0;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallEditKeyDown'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if fKeysRequired then
  try
   keyCode.character := Index;
   keyCode.virt := Value;
   keyCode.modifier := Round(opt);
   if Assigned(EditorForm) then
    begin
     a := KeyCodeToInteger(keyCode);
     if Assigned(EditorForm.ActiveControl)
      then Hndl := EditorForm.ActiveControl.Handle
      else Hndl := EditorForm.Handle;

     {$IFNDEF FPC}
     if keyCode.virt = 0 then b := 0 else b := KF_EXTENDED;
     if mkAlternate in TVSTModifierKeys(keyCode.modifier)
      then SendMessage(Hndl, WM_KEYDOWN, a,b)
      else SendMessage(Hndl, WM_SYSKEYDOWN, a,KF_ALTDOWN);
     SendMessage(Hndl,WM_CHAR, a, b);
     {$ELSE}
     if keyCode.virt = 0 then b := 0 else b := $100;
     if mkAlternate in TVSTModifierKeys(Integer(keyCode.modifier))
      then SendMessage(Hndl, LM_KEYDOWN, a,b)
      else SendMessage(Hndl, LM_SYSKEYDOWN, a, $2000);
     SendMessage(Hndl,LM_CHAR, a, b);
     {$ENDIF}

     if Assigned(FOnKeyDown) then FOnKeyDown(Self, keyCode);
     if Assigned(FOnCheckKey) then
      if FOnCheckKey(Self, Char(a))
       then Result := 1
       else Result := -1
      else Result := -1;
    end;
  except
   Result := -1;
  end
 else Result := -1;
end;

function TCustomVSTModule.HostCallEditKeyUp(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var
  keyCode : TVstKeyCode;
  a, b    : Integer;
  Hndl    : THandle;
begin
 Result := 0;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallEditKeyDown'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if fKeysRequired then
  try
   keyCode.character := Index;
   keyCode.virt := Value;
   keyCode.modifier := Round(opt);

   if Assigned(EditorForm) then
    begin
     a := KeyCodeToInteger(keyCode);
     if Assigned(EditorForm.ActiveControl)
      then Hndl := EditorForm.ActiveControl.Handle
      else Hndl := EditorForm.Handle;

     {$IFNDEF FPC}
     if keyCode.virt=0 then b := 0 else b := KF_EXTENDED;
     if mkAlternate in TVSTModifierKeys(keyCode.modifier)
      then SendMessage(Hndl, WM_KEYUP, a, b)
      else SendMessage(Hndl, WM_SYSKEYUP, a, KF_ALTDOWN);
     {$ELSE}
     if keyCode.virt = 0 then b := 0 else b := $100;
     if mkAlternate in TVSTModifierKeys(Integer(keyCode.modifier))
      then SendMessage(Hndl, LM_KEYUP, a,b)
      else SendMessage(Hndl, LM_SYSKEYUP, a, $2000);

     SendMessage(Hndl,LM_CHAR, a, b);
     {$ENDIF}

     if Assigned(FOnKeyUp) then FOnKeyUp(Self, keyCode);
     if Assigned(FOnCheckKey) then
      if FOnCheckKey(Self, Char(a))
       then Result := 1
       else Result := -1
      else Result := -1;
    end;
  except
   Result := -1;
  end else Result := -1;
end;

function TCustomVSTModule.HostCallSetEditKnobMode(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallSetEditKnobMode'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnSetKnobMode) then
  begin
   FOnSetKnobMode(Self, Value);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallGetSpeakerArrangement(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallGetSpeakerArrangement'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 PVstSpeakerArrangement(Value) := nil;
 PVstSpeakerArrangement(ptr) := nil;
 Result := 0;
end;

function TCustomVSTModule.HostCallShellGetNextPlugin(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallShellGetNextPlugin'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if FCurrentVstShellPlugin < FVstShellPlugins.Count then
  begin
   StrPCopy(pchar(ptr),FVstShellPlugins[FCurrentVstShellPlugin].DisplayName);
   Result := Integer(FVstShellPlugins[FCurrentVstShellPlugin].UniqueID);
   Inc(FCurrentVstShellPlugin);
  end
 else
  begin
   Result := 0;
   FCurrentVstShellPlugin := 0;
  end;
end;

function TCustomVSTModule.HostCallStartProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 Result := 1;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallStartProcess'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnStartProcess)
  then FOnStartProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.HostCallStopProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 Result := 1;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallStopProcess'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnStopProcess)
  then FOnStopProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.HostCallSetTotalSampleToProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallSetTotalSampleToProcess'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := Value;
end;

function TCustomVSTModule.HostCallSetPanLaw(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 Result := 1;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallSetPanLaw'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FOnSetPanLaw)
  then FOnSetPanLaw(Self, Value, opt)
  else Result := 0;
end;

function TCustomVSTModule.HostCallSetProcessPrecision(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('HostCallSetProcessPrecision'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := Integer(fProcessPrecisition); // [value]: @see VstProcessPrecision  @see AudioEffectX::setProcessPrecision
end;

function TCustomVSTModule.GetUniqueID: string;
begin
 Result := FEffect.UniqueID[3] +
           FEffect.UniqueID[2] +
           FEffect.UniqueID[1] +
           FEffect.UniqueID[0];
end;

procedure TCustomVSTModule.SetUniqueID(Value: string);
var
  i : Integer;
begin
 for i := 1 to 4 do
  if i <= Length(Value)
   then FEffect.uniqueID[4 - i] := Value[i]
   else FEffect.uniqueID[4 - i] := #0;
end;

procedure TCustomVSTModule.SetSampleRate(newValue: Single);
begin
 if fSampleRate <> newValue then
  begin
   fSampleRate := newValue;
   SampleRateChanged;
  end;
end;

procedure TCustomVSTModule.SetBlockSize(newValue: Integer);
begin
 if fBlockSize <> newValue then
  begin
   fBlockSize := newValue;
   BlockSizeChanged;
  end;
end;

procedure TCustomVSTModule.SetNumInputs(Inputs: Integer);
begin
 FEffect.numInputs := Inputs;
 IOChanged;
end;

procedure TCustomVSTModule.SetNumOutputs(Outputs: Integer);
begin
 FEffect.numOutputs := Outputs;
 IOChanged;
end;

procedure TCustomVSTModule.SetPluginFlags(newFlags : TEffFlags);
begin
 FEffect.EffectFlags := newFlags;
end;

function TCustomVSTModule.GetPluginFlags: TEffFlags;
begin
 Result := FEffect.EffectFlags;
end;

procedure TCustomVSTModule.SetInitialDelay(delay: Integer);
begin
 if FInitialDelay <> delay then
  begin
   FInitialDelay := delay;
   FEffect.initialDelay := FInitialDelay;

   if HostProduct <> 'energyXT' then IOChanged;
  end;
end;

procedure TCustomVSTModule.EditorPostUpdate;
begin
 FEditorNeedUpdate := True;
end;

{$IFDEF UseDelphi}
procedure TCustomVSTModule.ReadState(Reader: TReader);
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('Before ReadState'); {$ENDIF}
 {$IFDEF Debug} if assigned(FLog) then FLog.SaveToFile('Debug.log'); {$ENDIF}
 inherited;
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('After ReadState'); {$ENDIF}
 {$IFDEF Debug} if assigned(FLog) then FLog.SaveToFile('Debug.log'); {$ENDIF}

 if Assigned(FOnInitialize) then FOnInitialize(Self);
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('End ReadState'); {$ENDIF}
 {$IFDEF Debug} if assigned(FLog) then FLog.SaveToFile('Debug.log'); {$ENDIF}
end;
{$ENDIF}

function TCustomVSTModule.UpdateSampleRate: Double;
begin
  {$IFDEF Debug} if assigned(FLog) then FLog.Add('Update Samplerate'); FLog.SaveToFile('Debug.log'); {$ENDIF}

  Result := inherited UpdateSampleRate;
  if (Result>0) and (Result<>FSampleRate) then
  begin
    fSampleRate := Result;
    SampleRateChanged;
  end;
end;

function TCustomVSTModule.UpdateBlockSize: Integer;
begin
 {$IFDEF Debug} if assigned(FLog) then FLog.Add('UpdateBlockSize'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 Result := inherited UpdateBlockSize;
 if (Result > 0) and (Result <> FBlockSize) then
  begin
   FBlockSize := Result;
   BlockSizeChanged;
  end;
end;

function TCustomVSTModule.AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean;
var
  size : Integer;
begin
 if Assigned(Arrangement) then
  begin
   FreeMem(Arrangement);
   Arrangement := nil;
  end;

 size := SizeOf(Integer) + SizeOf(Integer) + (nbChannels) * SizeOf(TVstSpeakerProperties);
 GetMem(Arrangement, size);
 if not Assigned(Arrangement) then
  begin
   Result := False;
   Exit;
  end;

 FillChar(Arrangement^, size, 0);
 Arrangement^.numChannels := nbChannels;
 Result := True;
end;

function TCustomVSTModule.DeallocateArrangement(var Arrangement: PVstSpeakerArrangement): Boolean;
begin
 if Assigned(Arrangement) then
  begin
   FreeMem(Arrangement);
   Arrangement := nil;
  end;
 Result := True;
end;

function TCustomVSTModule.CopySpeaker(copyTo, copyFrom: PVstSpeakerProperties): Boolean;
begin
  // We assume here that "to" exists yet, ie this function won't
  // allocate memory for the speaker (this will prevent from having
  // a difference between an Arrangement's number of channels and
  // its actual speakers...)
 if (copyFrom = nil) or (copyTo = nil) then
  begin
   Result := False;
   Exit;
  end;

 StrCopy(copyTo^.name, copyFrom^.name);
 copyTo^.vType := copyFrom^.vType;
 copyTo^.Azimuth := copyFrom^.Azimuth;
 copyTo^.Elevation := copyFrom^.Elevation;
 copyTo^.Radius := copyFrom^.Radius;
 copyTo^.Reserved := copyFrom^.Reserved;
 Move(copyTo^.Future, copyFrom^.future, 28);
 Result := True;
end;

function TCustomVSTModule.MatchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): Boolean;
var i: Integer;
begin
 if matchFrom = nil then Result := False else
 if not deallocateArrangement(matchTo) or
    not allocateArrangement(matchTo, matchFrom^.numChannels)
  then Result := False
  else
   begin
    matchTo^.vType := matchFrom^.vType;
    for i := 0 to matchTo^.numChannels-1 do
     begin
      if not copySpeaker(@(matchTo^.speakers[i]), @(matchFrom^.speakers[i])) then
       begin
        Result := False;
        Exit;
       end;
     end;
    Result := False;
   end;
end;


procedure TCustomVSTModule.BlockSizeChanged;
begin
  if Assigned(FBlockSizeChangeEvent) then FBlockSizeChangeEvent(Self, FBlockSize);
end;

procedure TCustomVSTModule.SampleRateChanged;
begin
  if Assigned(FSampleRateChangeEvent) then FSampleRateChangeEvent(Self,fSampleRate);
end;

{$WARNINGS ON}

procedure TCustomVSTModule.SetVstShellPlugins(const Value: TCustomVstShellPlugins);
begin
 FVstShellPlugins.Assign(Value);
end;

procedure TCustomVSTModule.SetKeysRequired(const Value: Boolean);
begin
 FKeysRequired := Value;
 updateDisplay;
end;

procedure TCustomVSTModule.SetEffectName(const Value: string);
begin
 FEffectName := Value;
 if FTruncateStrings and (Length(FEffectName) > 32)
  then SetLength(FEffectName, 32);
end;

procedure TCustomVSTModule.SetVendorName(const Value: string);
begin
 FVendorName := Value;
 if FTruncateStrings and (Length(FVendorName) > 64)
  then SetLength(FVendorName, 64);
end;

procedure TCustomVSTModule.SetProductName(const Value: string);
begin
 FProductName := Value;
 if FTruncateStrings and (Length(FProductName) > 64)
  then SetLength(FProductName, 64);
end;

function TCustomVSTModule.GetHostProduct: string;
var
  Text : PChar;
begin
 if (FHostProduct = '') or (FHostProduct = 'Unknown') then
  begin
   // allocate and zero memory (256 byte, which is more than necessary, but
   // just to be sure and in case of host ignoring the specs)
   GetMem(Text, 256);
   FillChar(Text^, 256, 0);
   try
    if GetHostProductString(Text)
     then Result := StrPas(Text)
     else Result := 'Unknown';
    if TruncateStrings and (Length(Result) > 64)
     then SetLength(Result, 64);
    FHostProduct := Result;
   finally
    // dispose memory
    Dispose(Text);
   end;
  end
 else Result := FHostProduct;
end;

function TCustomVSTModule.GetHostVendor: string;
var
  Text : PChar;
begin
 if (FHostVendor = '') or (FHostVendor = 'Unknown') then
  begin
   // allocate and zero memory (256 byte, which is more than necessary, but
   // just to be sure and in case of host ignoring the specs)
   GetMem(Text, 256);
   FillChar(Text^, 256, 0);
   try
    if GetHostVendorString(Text)
     then Result := StrPas(Text)
     else Result := 'Unknown';
    if TruncateStrings and (Length(Result) > 64)
     then SetLength(Result, 64);
    FHostVendor := Result;
   finally
    // dispose memory
    Dispose(Text);
   end;
  end
 else Result := FHostVendor;
end;

procedure TCustomVSTModule.SetVersionMajor(Value: Integer);
begin
  FVersionMajor := Value;
  UpdateVersion;
end;

procedure TCustomVSTModule.SetVersionMinor(Value: Integer);
begin
  FVersionMinor := Value;
  UpdateVersion;
end;

procedure TCustomVSTModule.SetVersionRelease(Value: Integer);
begin
  FVersionRelease := Value;
  UpdateVersion;
end;

procedure TCustomVSTModule.UpdateVersion;
begin
  FEffect.version := (FVersionMajor shl 16) + (FVersionMinor shl 8) + FVersionRelease;
end;

end.
