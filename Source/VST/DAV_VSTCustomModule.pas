unit DAV_VSTCustomModule;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LCLType, LMessages, Controls, {$ELSE} Windows, Messages,
  {$ENDIF} Classes, Forms, DAV_Common, DAV_VSTEffect, DAV_VSTChannels,
  DAV_VSTBasicModule, DAV_VSTShellPlugins, DAV_VSTOfflineTask;

type
//  TChannelPropertyFlags = set of (cpfIsActive, cpfIsStereo, cpfUseSpeaker);

  TProcessAudioEvent       = procedure(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer) of object;
  TProcessDoubleEvent      = procedure(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer) of object;
  TGetVUEvent              = procedure(var VU: Single) of object;
  TBlockSizeChangeEvent    = procedure(Sender: TObject; const BlockSize: Integer) of object;
  TSampleRateChangeEvent   = procedure(Sender: TObject; const SampleRate: Single) of object;
  TOnDispatcherEvent       = procedure(Sender: TObject; const opCode: TDispatcherOpcode) of object;
  TOfflineNotifyEvent      = procedure(Sender: TObject; const AudioFile: TVstAudioFile; const numAudioFiles: Integer; const start: Boolean) of object;
  TOfflinePrepareEvent     = procedure(Sender: TObject; const OfflineTasks: array of TVstOfflineTask) of object;
  TOfflineRunEvent         = procedure(Sender: TObject; const OfflineTasks: array of TVstOfflineTask) of object;
  TSpeakerArrangementEvent = procedure(Sender: TObject; const Input, Output: TVstSpeakerArrangement) of object;
  TVSTKeyEvent             = procedure(Sender: TObject; var keyCode : TVstKeyCode) of object;
  TProcessVarIOEvent       = procedure(Sender: TObject; const varIo: TVstVariableIo) of object;
  TInOutConnectedEvent     = procedure(Sender: TObject; const Index: Integer; const State: Boolean) of object;
  TSetKnobModeEvent        = procedure(Sender: TObject; val: Integer) of object;
  TSoftBypassEvent         = procedure(Sender: TObject; const isBypass: Boolean) of object;
  TOnSetPanLawEvent        = procedure(Sender: TObject; const LawType: TVstPanLawType; const Value: Single) of object;
  TGetEditorEvent          = procedure(Sender: TObject; var GUI: TForm; ParentWindow : THandle) of object;
  TOnVendorSpecificEvent   = function(Sender: TObject; const Index, Value: Integer; const Ptr: pointer; const Float: Single): Integer of object;
  TOnCanDoEvent            = function(Sender: TObject; const CanDoText: string): Integer of object;
  TOnCheckKey              = function(Sender: TObject; Key: Char): Boolean of object;
  TOnEditClose             = procedure(Sender: TObject; var DestroyForm: Boolean) of object;

  TOnGetChannelPropertiesEvent = function(Sender: TObject; const Index: Integer; var VstPinProperties: TVstPinProperties): Boolean of object;

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
    FOnSpeakerArrangementChanged: TSpeakerArrangementEvent;
    {$IFDEF Debug}
    FLog                    : TStringList;
    FTmStmp                 : TDateTime;
    procedure AddLogMessage(const Text: string);
    {$ENDIF}
    procedure SetNumInputs(const Inputs: Integer);
    procedure SetNumOutputs(const Outputs: Integer);
    procedure SetVstShellPlugins(const Value: TCustomVstShellPlugins);
    procedure SetKeysRequired(const Value: Boolean);
    procedure ReadOnlyString(s: string); virtual;
    function GetHostProduct: string;
    function GetHostVendor: string;
  protected
    FBlockSize              : Integer;
    FEditorForm             : TForm;
    FEditorNeedUpdate       : Boolean;
    FEditorRect             : ERect;
    FEffectName             : string;
    FHostProduct            : string;
    FHostVendor             : string;
    FInitialDelay           : Integer;
    FNumCategories          : Integer;
    FOnClose                : TNotifyEvent;
    FOnEditOpen             : TGetEditorEvent;
    FOnOpen                 : TNotifyEvent;
    FOnProcessDoublesEx     : TProcessDoubleEvent;
    FOnProcessEx            : TProcessAudioEvent;
    FOnProcessReplacingEx   : TProcessAudioEvent;
    FProductName            : string;
    FSampleRate             : Single;
    FTruncateStrings        : Boolean;
    FVendorName             : string;
    FVersionMajor           : Integer;
    FVersionMinor           : Integer;
    FVersionRelease         : Integer;

    function GetPluginFlags: TEffFlags; virtual;
    function GetUniqueID: string; virtual;
    function OfflinePrepare(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer; virtual;
    function OfflineRun(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer; virtual;
    procedure SetAudioMaster(const AM: TAudioMasterCallbackFunc); override;
    procedure SetBlockSize(const Value: Integer); virtual;
    procedure SetInitialDelay(const Delay: Integer); virtual;
    procedure SetPluginFlags(newFlags : TEffFlags); virtual;
    procedure SetSampleRate(const Value: Single); virtual;
    procedure SetUniqueID(const Value: string); virtual;
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
    procedure NumInputsChanged; virtual;
    procedure NumOutputsChanged; virtual;

    procedure HostCallDispatchEffect(const opcode : TDispatcherOpcode; const Index, Value: Integer; const ptr: pointer; const opt: Single); override;
    procedure HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer); override;

    // HostCalls, protected methods that can be overwritten, but shall remain
    // hidden, since the user should not be able to call them directly!
    function HostCallOpen                      (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallClose                     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetVu                     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetSampleRate             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetBlockSize              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallMainsChanged              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditGetRect               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditOpen                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditClose                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditIdle                  (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditTop                   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditSleep                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallProcessEvents             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallConnectInput              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallConnectOutput             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetInputProperties        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetOutputProperties       (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetPlugCategory           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallOfflineNotify             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallOfflinePrepare            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallOfflineRun                (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallProcessVarIo              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetBlockSizeAndSampleRate (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetBypass                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetEffectName             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetVendorString           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetProductString          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetVendorVersion          (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallVendorSpecific            (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallCanDo                     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetTailSize               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallKeysRequired              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetVstVersion             (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditKeyDown               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallEditKeyUp                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetEditKnobMode           (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallGetSpeakerArrangement     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallShellGetNextPlugin        (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallStartProcess              (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallStopProcess               (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetTotalSampleToProcess   (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetPanLaw                 (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetProcessPrecision       (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;
    function HostCallSetSpeakerArrangement     (const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer; override;

    function GetSpeakerArrangement(var InputArrangement, OutputArrangement: PVstSpeakerArrangement): Boolean; virtual;
    function AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean; virtual;   // Allocate memory for a VstSpeakerArrangement containing the given number of channels
    function DeallocateArrangement(var Arrangement: PVstSpeakerArrangement): Boolean; virtual;                      // Delete/free memory for a speaker Arrangement
    function CopySpeaker(copyTo, copyFrom: PVstSpeakerProperties): Boolean; virtual;    // Feed the "to" speaker Properties with the same Values than "from"'s ones. It is assumed here that "to" exists yet, ie this function won't allocate memory for the speaker (this will prevent from having a difference between an Arrangement's number of channels and its actual speakers...)
    function MatchArrangement(var MatchTo, MatchFrom: PVstSpeakerArrangement): Boolean; virtual;    // "to" is deleted, then created and initialized with the same Values as "from" ones ("from" must exist).
    procedure SetSpeakerArrangement(const Input, Output: TVstSpeakerArrangement);
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
    property BlockSize: Integer read FBlockSize write SetBlockSize default 1024;
    property CanDos: TVstCanDos read FCanDos write FCanDos default [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out];
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
    property SampleRate: Single read FSampleRate write SetSampleRate;
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
    property OnSpeakerArrangementChanged: TSpeakerArrangementEvent read FOnSpeakerArrangementChanged write FOnSpeakerArrangementChanged;
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
 FTmStmp := Now;
 FLog.Add('Create: ' + TimeToStr(FTmStmp));
 FLog.SaveToFile('Debug.log');
 {$ENDIF}
 Randomize;
 FTruncateStrings    := False;
 FVersion            := '1.0';
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
 FCanDos             := [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out];
 UpdateVersion;

 FVstShellPlugins := TCustomVstShellPlugins.Create(Self);
 FCurrentVstShellPlugin := 0;
 FNumCategories := 1;
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

{$IFDEF Debug}
procedure TCustomVSTModule.AddLogMessage(const Text: string);
begin
 if assigned(FLog) then
  begin
   FLog.Add(TimeToStr(Now - FTmStmp) + ' | ' + Text);
   FLog.SaveToFile('Debug.log');
  end;
end;
{$ENDIF}

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

procedure TCustomVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
var
  rUID : TChunkName;
  i    : Integer;
  sUID : string;
  hv   : boolean;
begin
 inherited;
 hv := (Pos('WaveLab', HostProduct) < 0) {or (shortstring(temp)<>'energyXT')};
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

procedure TCustomVSTModule.HostCallDispatchEffect(const Opcode: TDispatcherOpcode; const Index, Value: Integer; const ptr: pointer; const opt: Single);
begin
 if Assigned(FOnDispatcher) then FOnDispatcher(Self, Opcode);

 {$IFDEF Debug}
 if not (Opcode in [effIdle, effEditIdle])
  then AddLogMessage(' Opcode: ' + Opcode2String(Opcode) +
                     ' Index: ' + IntToStr(Index) +
                     ' Value: ' + IntToStr(Value) +
                     ' Single: ' + FloatToStr(opt));
 {$ENDIF}
end;

procedure TCustomVSTModule.ReadOnlyString(s: string); begin end;

function TCustomVSTModule.HostCallOpen(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallOpen'); {$ENDIF}
 Result := inherited HostCallOpen(Index, Value, ptr, opt);
 if Assigned(FOnOpen) then FOnOpen(Self);
end;

function TCustomVSTModule.HostCallClose(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallClose'); {$ENDIF}
 if Assigned(FOnClose) then FOnClose(Self);
 Result := inherited HostCallClose(Index, Value, ptr, opt);
end;

function TCustomVSTModule.HostCallGetVu(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  s: Single;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetVu'); {$ENDIF}
 if Assigned(FOnGetVUEvent) then
  begin
   s := 0;
   FOnGetVUEvent(s);
   Result := round(s * 32767);
  end else Result := 0;
end;

function TCustomVSTModule.HostCallSetSampleRate(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallSetSampleRate (' + FloatToStr(Opt) + ')'); {$ENDIF}
 SetSampleRate(opt);
 Result := 1;
end;

function TCustomVSTModule.HostCallSetBlockSize(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallSetBlockSize (' + IntToStr(Value) + ')'); {$ENDIF}
 setBlockSize(Value);
 Result := 0;
end;

function TCustomVSTModule.HostCallMainsChanged(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallMainsChanged (' + IntToStr(Value) + ')'); {$ENDIF}
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

function TCustomVSTModule.HostCallEditGetRect(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallEditGetRect'); {$ENDIF}
 Result := 0;
 if assigned(ptr) then
  begin
   PPERect(ptr)^ := @FEditorRect;
   if assigned(ptr) then
    begin
     FEditorRect.Top := 0;
     FEditorRect.Left := 0;

     if Assigned(FEditorForm) then
      begin
       FEditorRect.Bottom := FEditorForm.ClientHeight;
       FEditorRect.Right := FEditorForm.ClientWidth;
       Result := 1;
      end;
    end;
  end;
end;

function TCustomVSTModule.HostCallEditOpen(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallEditOpen'); {$ENDIF}
 Result := 0;
 if (effFlagsHasEditor in FEffect.EffectFlags) and assigned(ptr) then
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
      Parent := TWinControl.CreateParented(HWnd(ptr));
(*
      ParentWindow := HWnd(ptr);
      Parent := FindOwnerControl(THandle(ptr));
      SetBounds(0, 0, 300, 100);
*)
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

function TCustomVSTModule.HostCallEditClose(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  DestroyForm: Boolean;
begin
 {$IFDEF Debug} AddLogMessage('HostCallEditClose'); {$ENDIF}
 if (effFlagsHasEditor in FEffect.EffectFlags) then
  begin
   DestroyForm := True;
   if Assigned(FOnEditClose) then FOnEditClose(Self, DestroyForm);
   if DestroyForm and Assigned(FEditorForm) then FreeAndNil(FEditorForm);
  end;

 Result := 0;
end;

function TCustomVSTModule.HostCallEditIdle(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 if (effFlagsHasEditor in FEffect.EffectFlags) and FEditorNeedUpdate and Assigned(FEditorForm)then
  begin
   if Assigned(FOnEditIdle) then FOnEditIdle(Self);
   FEditorNeedUpdate := False;
  end;

 Result := 0;
end;

function TCustomVSTModule.HostCallEditTop(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallEditTop'); {$ENDIF}
 if Assigned(FOnEditTop) then FOnEditTop(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallEditSleep(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallEditSleep'); {$ENDIF}
 if Assigned(FOnEditSleep) then FOnEditSleep(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallProcessEvents(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallProcessEvents'); {$ENDIF}
 Result := 1;
end;

function TCustomVSTModule.HostCallConnectInput(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallConnectInput'); {$ENDIF}
 if Assigned(FOnInConnected) then FOnInConnected(Self, Index, (Value <> 0));
 Result := 1;
end;

function TCustomVSTModule.HostCallConnectOutput(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallConnectOutput'); {$ENDIF}
 if Assigned(FOnOutConnected) then FOnOutConnected(Self, Index, (Value <> 0));
 Result := 1;
end;

function TCustomVSTModule.HostCallGetInputProperties(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  cpf : TVstPinPropertiesFlags;
  str : string;
begin
 Result := 0;
 {$IFDEF Debug} AddLogMessage('HostCallGetInputProperties'); {$ENDIF}
 if (Index < FEffect.numInputs) then
  with PVstPinProperties(ptr)^ do
   begin
    str := 'Input #' + IntToStr(Index + 1) + #0;
    Move(str[1], Caption, Length(str));
    str := 'In' + IntToStr(Index + 1) + #0;
    Move(str[1], ShortLabel, Length(str));

    case numInputs of
     1: ArrangementType := satMono;
     2: ArrangementType := satStereo;
     4: ArrangementType := sat40Music;
    end;
    cpf := [vppIsActive];
    if numInputs = 2 then cpf := cpf + [vppIsStereo];

    if vppIsActive in cpf then Flags := [vppIsActive] else Flags := [];
    if vppIsStereo in cpf then Flags := Flags + [vppIsStereo];
    if vppUseSpeaker in cpf then Flags := Flags + [vppUseSpeaker];

    if Assigned(FOnGetInputProperties)
     then Result := Integer(FOnGetInputProperties(Self, Index, PVstPinProperties(ptr)^))
     else Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetOutputProperties(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  cpf : TVstPinPropertiesFlags;
  str : string;
begin
 Result := 0;
 {$IFDEF Debug} AddLogMessage('HostCallGetOutputProperties'); {$ENDIF}
 if (Index < FEffect.numOutputs) then
  with PVstPinProperties(ptr)^ do
   begin
    str := 'Output #' + IntToStr(Index + 1) + #0;
    Move(str[1], Caption, Length(str));
    str := 'Out' + IntToStr(Index + 1) + #0;
    Move(str[1], ShortLabel, Length(str));

    case numOutputs of
     1: ArrangementType := satMono;
     2: ArrangementType := satStereo;
     4: ArrangementType := sat40Music;
    end;
    cpf := [vppIsActive];
    if numOutputs = 2 then cpf := cpf + [vppIsStereo];

    if vppIsActive in cpf then Flags := [vppIsActive] else Flags := [];
    if vppIsStereo in cpf then Flags := Flags + [vppIsStereo];
    if vppUseSpeaker in cpf then Flags := Flags + [vppUseSpeaker];

    if Assigned(FOnGetOutputProperties)
     then Result := Integer(FOnGetOutputProperties(Self, index, PVstPinProperties(ptr)^))
     else Result := 1;
   end;
end;

function TCustomVSTModule.HostCallGetPlugCategory(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetPlugCategory'); {$ENDIF}
 Result := Integer(FPlugCategory);
end;

function TCustomVSTModule.HostCallOfflineNotify(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallOfflineNotify'); {$ENDIF}
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

function TCustomVSTModule.HostCallOfflinePrepare(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallOfflinePrepare'); {$ENDIF}
 Result := OfflinePrepare(PVstOfflineTaskRecord(ptr), Value);
end;

function TCustomVSTModule.HostCallOfflineRun(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallOfflineRun'); {$ENDIF}
 Result := OfflineRun(PVstOfflineTaskRecord(ptr), Value);
end;

function TCustomVSTModule.HostCallProcessVarIo(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallProcessVarIo'); {$ENDIF}
 if Assigned(FOnProcessVarIO) then
  begin
   FOnProcessVarIO(Self, PVstVariableIo(ptr)^);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallSetBlockSizeAndSampleRate(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallSetBlockSizeAndSampleRate: Blocksize ' + IntToStr(Value) + ' Samplerate ' + FloatToStr(opt)); {$ENDIF}
 if FSampleRate <> opt then
  begin
   FSampleRate := opt;
   SampleRateChanged;
  end;
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
 Result := 1;
end;

function TCustomVSTModule.HostCallSetBypass(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
  {$IFDEF Debug}
  if Value <> 0
   then AddLogMessage('HostCallSetBypass: On')
   else AddLogMessage('HostCallSetBypass: Off');
  {$ENDIF}
  if Assigned(FOnSoftBypass) then
   begin
    FOnSoftBypass(Self, Value <> 0);
    Result := 1;
   end else Result := 0;
end;

function TCustomVSTModule.HostCallGetEffectName(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetEffectName'); {$ENDIF}
 Result := 0;
 if Assigned(ptr) then
  begin
   StrPCopy(ptr, FEffectName);
   Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetVendorString(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetVendorString'); {$ENDIF}
 Result := 0;
 if Assigned(ptr) then
  begin
   StrPCopy(ptr, FVendorName);
   Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetProductString(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetProductString'); {$ENDIF}
 Result := 0;
 if Assigned(ptr) then
  begin
   if FProductName <> ''
    then StrPCopy(ptr, FProductName)
    else StrPCopy(ptr, FEffectName);
   Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetVendorVersion(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetVendorVersion'); {$ENDIF}
 Result := FEffect.Version;
end;

function TCustomVSTModule.HostCallVendorSpecific(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallVendorSpecific'); {$ENDIF}
 if Assigned(FOnVendorSpecific)
  then Result := FOnVendorSpecific(Self, Index, Value, ptr, opt)
  else Result := 0;
end;

function TCustomVSTModule.HostCallCanDo(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := 0;
 {$IFDEF Debug} AddLogMessage('HostCallCanDo (' + StrPas(PChar(ptr)) + ')'); {$ENDIF}
 if StrComp(ptr, 'receiveVstEvents')      = 0 then Result := 2 * Integer(vcdReceiveVstEvents      in FCanDos) - 1 else
 if StrComp(ptr, 'receiveVstMidiEvent')   = 0 then Result := 2 * Integer(vcdReceiveVstMidiEvent   in FCanDos) - 1 else
 if StrComp(ptr, 'receiveVstTimeInfo')    = 0 then Result := 2 * Integer(vcdReceiveVstTimeInfo    in FCanDos) - 1 else
 if StrComp(ptr, 'sendVstEvents')         = 0 then Result := 2 * Integer(vcdSendVstEvents         in FCanDos) - 1 else
 if StrComp(ptr, 'sendVstMidiEvent')      = 0 then Result := 2 * Integer(vcdSendVstMidiEvent      in FCanDos) - 1 else
 if StrComp(ptr, 'sendVstTimeInfo')       = 0 then Result := 2 * Integer(vcdSendVstTimeInfo       in FCanDos) - 1 else
 if StrComp(ptr, 'offline')               = 0 then Result := 2 * Integer(vcdOffline               in FCanDos) - 1 else
 if StrComp(ptr, 'plugAsChannelInsert')   = 0 then Result := 2 * Integer(vcdPlugAsChannelInsert   in FCanDos) - 1 else
 if StrComp(ptr, 'plugAsSend')            = 0 then Result := 2 * Integer(vcdPlugAsSend            in FCanDos) - 1 else
 if StrComp(ptr, 'mixDryWet')             = 0 then Result := 2 * Integer(vcdMixDryWet             in FCanDos) - 1 else
 if StrComp(ptr, 'noRealTime')            = 0 then Result := 2 * Integer(vcdNoRealTime            in FCanDos) - 1 else
 if StrComp(ptr, 'multipass')             = 0 then Result := 2 * Integer(vcdMultipass             in FCanDos) - 1 else
 if StrComp(ptr, 'metapass')              = 0 then Result := 2 * Integer(vcdMetapass              in FCanDos) - 1 else
 if StrComp(ptr, '1in1out')               = 0 then Result := 2 * Integer(vcd1in1out               in FCanDos) - 1 else
 if StrComp(ptr, '1in2out')               = 0 then Result := 2 * Integer(vcd1in2out               in FCanDos) - 1 else
 if StrComp(ptr, '2in1out')               = 0 then Result := 2 * Integer(vcd2in1out               in FCanDos) - 1 else
 if StrComp(ptr, '2in2out')               = 0 then Result := 2 * Integer(vcd2in2out               in FCanDos) - 1 else
 if StrComp(ptr, '2in4out')               = 0 then Result := 2 * Integer(vcd2in4out               in FCanDos) - 1 else
 if StrComp(ptr, '4in2out')               = 0 then Result := 2 * Integer(vcd4in2out               in FCanDos) - 1 else
 if StrComp(ptr, '4in4out')               = 0 then Result := 2 * Integer(vcd4in4out               in FCanDos) - 1 else
 if StrComp(ptr, '4in8out')               = 0 then Result := 2 * Integer(vcd4in8out               in FCanDos) - 1 else
 if StrComp(ptr, '8in4out')               = 0 then Result := 2 * Integer(vcd8in4out               in FCanDos) - 1 else
 if StrComp(ptr, '8in8out')               = 0 then Result := 2 * Integer(vcd8in8out               in FCanDos) - 1 else
 if StrComp(ptr, 'midiProgramNames')      = 0 then Result := 2 * Integer(vcdMidiProgramNames      in FCanDos) - 1 else
 if StrComp(ptr, 'conformsToWindowRules') = 0 then Result := 2 * Integer(vcdConformsToWindowRules in FCanDos) - 1 else
 if StrComp(ptr, 'LiveWithoutToolbar')    = 0 then Result := 2 * Integer(vcdLiveWithoutToolbar    in FCanDos) - 1 else
 if StrComp(ptr, 'bypass')                = 0 then Result := 2 * Integer(vcdBypass                in FCanDos) - 1 else
 if StrComp(ptr, 'hasCockosExtensions')   = 0 then
  if vcdCockosExtension in FCanDos
   then Result := Integer($BEEF0000)
   else Result := 0;
 if Assigned(FOnCanDo) then FOnCanDo(Self, PAnsiChar(ptr));
end;

function TCustomVSTModule.HostCallGetTailSize(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetTailSize'); {$ENDIF}
 Result := FTailSize;
end;

function TCustomVSTModule.HostCallKeysRequired(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallKeysRequired'); {$ENDIF}
 Result := Integer(not FKeysRequired); // reversed to keep v1 compatibility
end;

function TCustomVSTModule.HostCallGetVstVersion(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetVstVersion'); {$ENDIF}
 Result := 2400;
end;

function TCustomVSTModule.HostCallEditKeyDown(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  keyCode : TVstKeyCode;
  a, b    : Integer;
  Hndl    : THandle;
begin
 Result := 0;
 {$IFDEF Debug} AddLogMessage('HostCallEditKeyDown'); {$ENDIF}
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

function TCustomVSTModule.HostCallEditKeyUp(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
var
  keyCode : TVstKeyCode;
  a, b    : Integer;
  Hndl    : THandle;
begin
 Result := 0;
 {$IFDEF Debug} AddLogMessage('HostCallEditKeyDown'); {$ENDIF}
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

function TCustomVSTModule.HostCallSetEditKnobMode(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallSetEditKnobMode (' + IntToStr(Value) + ')'); {$ENDIF}
 if Assigned(FOnSetKnobMode) then
  begin
   FOnSetKnobMode(Self, Value);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallGetSpeakerArrangement(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallGetSpeakerArrangement'); {$ENDIF}
 Result := Integer(GetSpeakerArrangement(PVstSpeakerArrangement(Pointer(Value)^), PVstSpeakerArrangement(ptr^)));
end;

function TCustomVSTModule.HostCallShellGetNextPlugin(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallShellGetNextPlugin'); {$ENDIF}
 if (FCurrentVstShellPlugin < FVstShellPlugins.Count) and assigned(Ptr) then
  begin
   StrPCopy(Ptr, FVstShellPlugins[FCurrentVstShellPlugin].DisplayName);
   Result := Integer(FVstShellPlugins[FCurrentVstShellPlugin].UniqueID);
   Inc(FCurrentVstShellPlugin);
  end
 else
  begin
   Result := 0;
   FCurrentVstShellPlugin := 0;
  end;
end;

function TCustomVSTModule.HostCallStartProcess(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := 1;
 {$IFDEF Debug} AddLogMessage('HostCallStartProcess'); {$ENDIF}
 if Assigned(FOnStartProcess)
  then FOnStartProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.HostCallStopProcess(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := 1;
 {$IFDEF Debug} AddLogMessage('HostCallStopProcess'); {$ENDIF}
 if Assigned(FOnStopProcess)
  then FOnStopProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.HostCallSetTotalSampleToProcess(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallSetTotalSampleToProcess'); {$ENDIF}
 Result := Value;
end;

function TCustomVSTModule.HostCallSetPanLaw(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 Result := 1;
 {$IFDEF Debug} AddLogMessage('HostCallSetPanLaw'); {$ENDIF}
 if Assigned(FOnSetPanLaw)
  then FOnSetPanLaw(Self, TVstPanLawType(Value), Opt)
  else Result := 0;
end;

function TCustomVSTModule.HostCallSetProcessPrecision(const Index, Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 {$IFDEF Debug} AddLogMessage('HostCallSetProcessPrecision'); {$ENDIF}
 Result := Integer(fProcessPrecisition); // [value]: @see VstProcessPrecision  @see AudioEffectX::setProcessPrecision
end;

function TCustomVSTModule.GetUniqueID: string;
begin
 Result := FEffect.UniqueID[3] +
           FEffect.UniqueID[2] +
           FEffect.UniqueID[1] +
           FEffect.UniqueID[0];
end;

procedure TCustomVSTModule.SetUniqueID(const Value: string);
var
  i : Integer;
begin
 for i := 1 to 4 do
  if i <= Length(Value)
   then FEffect.uniqueID[4 - i] := AnsiChar(Value[i])
   else FEffect.uniqueID[4 - i] := #0;
end;

procedure TCustomVSTModule.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomVSTModule.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
end;

procedure TCustomVSTModule.NumInputsChanged;
begin
 IOChanged;
end;

procedure TCustomVSTModule.NumOutputsChanged;
begin
 IOChanged;
end;

procedure TCustomVSTModule.SetNumInputs(const Inputs: Integer);
begin
 if FEffect.numInputs <> Inputs then
  begin
   FEffect.numInputs := Inputs;
   NumInputsChanged;
  end;
end;

procedure TCustomVSTModule.SetNumOutputs(const Outputs: Integer);
begin
 if FEffect.numOutputs <> Outputs then
  begin
   FEffect.numOutputs := Outputs;
   NumOutputsChanged;
  end;
end;

procedure TCustomVSTModule.SetPluginFlags(newFlags : TEffFlags);
begin
 FEffect.EffectFlags := newFlags;
end;

function TCustomVSTModule.GetPluginFlags: TEffFlags;
begin
 Result := FEffect.EffectFlags;
end;

function TCustomVSTModule.GetSpeakerArrangement(var InputArrangement,
  OutputArrangement: PVstSpeakerArrangement): Boolean;
begin
 {$IFDEF Debug} AddLogMessage('GetSpeakerArrangement'); {$ENDIF}
 result := False;
end;

procedure TCustomVSTModule.SetInitialDelay(const Delay: Integer);
begin
 if FInitialDelay <> Delay then
  begin
   FInitialDelay := Delay;
   FEffect.initialDelay := Delay;

   if Pos('WaveLab', HostProduct) > 0 then
    begin
     IOChanged;
    end else
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
 {$IFDEF Debug} AddLogMessage('Before ReadState'); {$ENDIF}
 inherited;
 {$IFDEF Debug} AddLogMessage('After ReadState'); {$ENDIF}

 if Assigned(FOnInitialize) then FOnInitialize(Self);
 {$IFDEF Debug} AddLogMessage('End ReadState'); {$ENDIF}
end;
{$ENDIF}

function TCustomVSTModule.UpdateSampleRate: Double;
begin
  {$IFDEF Debug} AddLogMessage('Update Samplerate'); {$ENDIF}

  Result := inherited UpdateSampleRate;
  if (Result>0) and (Result<>FSampleRate) then
  begin
    FSampleRate := Result;
    SampleRateChanged;
  end;
end;

function TCustomVSTModule.UpdateBlockSize: Integer;
begin
 {$IFDEF Debug} AddLogMessage('UpdateBlockSize'); {$ENDIF}
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
 Result := False;
 if (copyFrom = nil) or (copyTo = nil)
  then Exit;

 copyTo^ := copyFrom^;
 Result := True;
end;

function TCustomVSTModule.MatchArrangement(var MatchTo, MatchFrom: PVstSpeakerArrangement): Boolean;
var
  i: Integer;
begin
 if matchFrom = nil then Result := False else
 if not DeallocateArrangement(matchTo) or
    not AllocateArrangement(matchTo, matchFrom^.numChannels)
  then Result := False
  else
   begin
    matchTo^.ArrangementType := matchFrom^.ArrangementType;
    for i := 0 to matchTo^.numChannels - 1 do
     begin
      if not CopySpeaker(@(MatchTo.speakers[i]), @(matchFrom^.speakers[i])) then
       begin
        Result := False;
        Exit;
       end;
     end;
    Result := False;
   end;
end;

procedure TCustomVSTModule.SetSpeakerArrangement(const Input, Output: TVstSpeakerArrangement);
begin
 if assigned(FOnSpeakerArrangementChanged)
  then FOnSpeakerArrangementChanged(Self, Input, Output);
end;

function TCustomVSTModule.HostCallSetSpeakerArrangement(const Index,
  Value: Integer; const ptr: pointer; const opt: Single): Integer;
begin
 result := 0;
 if assigned(ptr) and (Value <> 0) then
  begin
   SetSpeakerArrangement(PVstSpeakerArrangement(Value)^, PVstSpeakerArrangement(Ptr)^);
   result := 1;
  end;
end;

procedure TCustomVSTModule.BlockSizeChanged;
begin
  if Assigned(FBlockSizeChangeEvent) then FBlockSizeChangeEvent(Self, FBlockSize);
end;

procedure TCustomVSTModule.SampleRateChanged;
begin
 if Assigned(FSampleRateChangeEvent) then FSampleRateChangeEvent(Self,FSampleRate);
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
  Text : PAnsiChar;
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
  Text : PAnsiChar;
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
 if FVersionRelease < 100
  then FEffect.version := 1000 * FVersionMajor + 100 * FVersionMinor + FVersionRelease
  else FEffect.version := (FVersionMajor shl 16) + (FVersionMinor shl 8) + FVersionRelease;
end;

end.
