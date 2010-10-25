unit DAV_VSTCustomModule;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LCLType, LMessages, Controls,
  {$IFDEF Windows} Windows, {$ENDIF} {$IFDEF DARWIN} MacOSAll,{$ENDIF}
  {$ELSE} Windows, Messages, {$ENDIF} Classes, Forms,
  DAV_Types, DAV_VSTEffect, DAV_VSTChannels, DAV_VSTBasicModule,
  DAV_VSTShellPlugins, DAV_VSTOfflineTask;

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
  TOnVendorSpecificEvent   = function(Sender: TObject; const Index: Integer; const Value: TVstIntPtr; const Ptr: pointer; const Float: Single): Integer of object;
  TOnCanDoEvent            = function(Sender: TObject; const CanDoText: AnsiString): Integer of object;
  TOnCheckKey              = function(Sender: TObject; Key: Char): Boolean of object;
  TOnEditClose             = procedure(Sender: TObject; var DestroyForm: Boolean) of object;

  TOnGetChannelPropertiesEvent = function(Sender: TObject; const Index: Integer; var VstPinProperties: TVstPinProperties): Boolean of object;

  TCustomVSTModule = class(TBasicVSTModule)
  private
    FAbout                  : string;
    FVersion                : AnsiString;

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
(*
    function GetEditorFormClassName: string;
    procedure SetEditorFormClassName(Value: string);
*)
    procedure SetNumInputs(const Inputs: Integer);
    procedure SetNumOutputs(const Outputs: Integer);
    procedure SetVstShellPlugins(const Value: TCustomVstShellPlugins);
    procedure SetKeysRequired(const Value: Boolean);
    procedure ReadOnlyString(s: string); virtual;
    procedure SetOnProcessEx(const Value: TProcessAudioEvent);
    procedure SetOnProcess32ReplacingEx(const Value: TProcessAudioEvent);
    procedure SetOnProcess64ReplacingEx(const Value: TProcessDoubleEvent);
    procedure SetSampleRate(const Value: Single);
    procedure SetPluginFlags(newFlags : TEffFlags);
    procedure SetInitialDelay(const Delay: Integer);
    procedure SetBlockSize(const Value: Integer);
    procedure VersionMajorChanged;
    procedure VersionMinorChanged;
    procedure VersionReleaseChanged;
  protected
    FBlockSize              : Integer;
    FEditorForm             : TForm;
    FEditorNeedUpdate       : Boolean;
    FEditorRect             : ERect;
    FEditorFormClass        : TFormClass;
    FEffectName             : AnsiString;
    FInitialDelay           : Integer;
    FNumCategories          : Integer;
    FOnClose                : TNotifyEvent;
    FOnEditOpen             : TGetEditorEvent;
    FOnOpen                 : TNotifyEvent;
    FOnProcess64ReplacingEx     : TProcessDoubleEvent;
    FOnProcessEx            : TProcessAudioEvent;
    FOnProcess32ReplacingEx   : TProcessAudioEvent;
    FProductName            : AnsiString;
    FSampleRate             : Single;

    FHostProduct            : AnsiString;
    FHostVendor             : AnsiString;
    FTruncateStrings        : Boolean;

    FVendorName             : AnsiString;
    FVersionMajor           : Integer;
    FVersionMinor           : Integer;
    FVersionRelease         : Integer;
//    FParentWindowProxy      : TWinControl;
    function GetPluginFlags: TEffFlags; virtual;
    function GetUniqueID: AnsiString; virtual;
    function OfflinePrepare(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer; virtual;
    function OfflineRun(OfflineTaskStartPointer: PVstOfflineTaskRecord; Count: Integer): Integer; virtual;
    procedure SetAudioMaster(const AM: TAudioMasterCallbackFunc); override;
    procedure SetUniqueID(const Value: AnsiString); virtual;
    {$IFDEF UseDelphi}
    procedure ReadState(Reader: TReader); override;
    {$ENDIF}
    function GetHostProduct: AnsiString;
    function GetHostVendor: AnsiString;
    procedure SetEffectName(const Value: AnsiString);
    procedure SetProductName(const Value: AnsiString);
    procedure SetVendorName(const Value: AnsiString);
    procedure SetVersionMajor(Value: Integer);
    procedure SetVersionMinor(Value: Integer);
    procedure SetVersionRelease(Value: Integer);
    procedure UpdateVersion;
    procedure SampleRateChanged; virtual;
    procedure BlockSizeChanged; virtual;
    procedure NumInputsChanged; virtual;
    procedure NumOutputsChanged; virtual;
    procedure EffectFlagsChanged; virtual;
    procedure OnProcess64ReplacingExChanged; virtual;
    procedure OnProcessExChanged; virtual;
    procedure OnProcess32ReplacingExChanged; virtual;
    procedure InitialDelayChanged; virtual;

    function HostCallDispatchEffect(const opcode : TDispatcherOpcode; const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    procedure HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcess32Replacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcess64Replacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer); override;

    // HostCalls, protected methods that can be overwritten, but shall remain
    // hidden, since the user should not be able to call them directly!
    function HostCallOpen                      (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallClose                     (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetVu                     (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetSampleRate             (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetBlockSize              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallMainsChanged              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditGetRect               (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditOpen                  (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditClose                 (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditIdle                  (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditTop                   (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditSleep                 (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallProcessEvents             (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallConnectInput              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallConnectOutput             (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetInputProperties        (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetOutputProperties       (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetPlugCategory           (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallOfflineNotify             (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallOfflinePrepare            (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallOfflineRun                (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallProcessVarIo              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetBlockSizeAndSampleRate (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetBypass                 (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetEffectName             (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetVendorString           (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetProductString          (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetVendorVersion          (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallVendorSpecific            (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallCanDo                     (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetTailSize               (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallKeysRequired              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetVstVersion             (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditKeyDown               (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallEditKeyUp                 (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetEditKnobMode           (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallGetSpeakerArrangement     (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallShellGetNextPlugin        (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallStartProcess              (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallStopProcess               (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetTotalSampleToProcess   (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetPanLaw                 (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetProcessPrecision       (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;
    function HostCallSetSpeakerArrangement     (const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr; override;

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

    // Properties
    property About: string read FAbout write ReadOnlyString stored False;
    property BlockSize: Integer read FBlockSize write SetBlockSize default 1024;
    property CanDo[canDo: string]: Integer read GetCanHostDo;
    property CanDos: TVstCanDos read FCanDos write FCanDos default [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out];
    property EditorForm: TForm read FEditorForm;
    property EditorFormClass: TFormClass read FEditorFormClass write FEditorFormClass;
//    property EditorFormClassName: string read GetEditorFormClassName write SetEditorFormClassName;
    property EditorNeedUpdate: Boolean read FEditorNeedUpdate write FEditorNeedUpdate;
    property EffectName: AnsiString read FEffectName write SetEffectName;
    property Flags: TEffFlags read GetPluginFlags write SetPluginFlags default [effFlagsCanReplacing];
    property HostProduct: AnsiString read GetHostProduct stored False;
    property HostVendor: AnsiString read GetHostVendor stored False;
    property HostVersion: Integer read GetHostVendorVersion stored False;
    property InitialDelay: Integer read FEffect.initialDelay write SetInitialDelay default 0;
    property IORatio: Single read FEffect.ioRatio write FEffect.ioRatio;
    property KeysRequired: Boolean read FKeysRequired write SetKeysRequired default False;
    property numCategories: Integer read fNumCategories write fNumCategories default 1;
    property numInputs: Integer read FEffect.numInputs write SetNumInputs default 2;
    property numOutputs: Integer read FEffect.numOutputs write SetNumOutputs default 2;
    property OffQualities: Integer read FEffect.offQualities write FEffect.offQualities default 0;
    property PlugCategory: TVstPluginCategory read fPlugCategory write fPlugCategory default vpcUnknown;
    property ProcessPrecisition: TProcessPrecision read FProcessPrecisition write FProcessPrecisition default pp32;
    property ProductName: AnsiString read FProductName write SetProductName;
    property RealQualities: Integer read FEffect.realQualities write FEffect.realQualities default 0;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property ShellPlugins: TCustomVstShellPlugins read FVstShellPlugins write SetVstShellPlugins;
    property TailSize: Integer read FTailSize write FTailSize default 0;
    property Tempo: Single read fTempo;
    property TruncateStrings: Boolean read FTruncateStrings write FTruncateStrings default False;
    property UniqueID: AnsiString read GetUniqueID write SetUniqueID;
    property VendorName: AnsiString read FVendorName write SetVendorName;
    property Version: AnsiString read FVersion write FVersion;
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

    property OnProcess: TProcessAudioEvent read FOnProcessEx write SetOnProcessEx;
    property OnProcessReplacing: TProcessAudioEvent read FOnProcess32ReplacingEx write SetOnProcess32ReplacingEx;
    property OnProcessDoubleReplacing: TProcessDoubleEvent read FOnProcess64ReplacingEx write SetOnProcess64ReplacingEx;
  end;


implementation

{$IFDEF FPC} {$DEFINE PUREPASCAL} {$ENDIF}

uses
  {$IFDEF DELPHI14_UP}AnsiStrings, {$ENDIF} SysUtils, Math,
  {$IFDEF PUREPASCAL}DAV_BufferMathPascal{$ELSE}DAV_BufferMathAsm{$ENDIF};

constructor TCustomVSTModule.Create(AOwner: TComponent);
begin
 inherited;
 Randomize;
 FVersion            := '1.0';
 FAbout              := 'VST Plugin Template by Christian Budde, Tobybear & MyCo';
 FProcessPrecisition := pp32;
 FKeysRequired       := False;
 FSampleRate         := 44100;
 FBlockSize          := 1024;
 FEditorForm         := nil;
 FTailSize           := 0;
 FHostProduct        := '';
 FHostVendor         := '';
 FTruncateStrings    := False;
 FVersionMajor       := 1;
 FVersionMinor       := 0;
 FVersionRelease     := 0;
 FCanDos             := [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out];
 UpdateVersion;

 {$IFDEF DebugLog} AddLogMessage('After Update Version'); {$ENDIF}

 FVstShellPlugins := TCustomVstShellPlugins.Create(Self);
 FCurrentVstShellPlugin := 0;
 FNumCategories := 1;
end;

destructor TCustomVSTModule.Destroy;
begin
 try
  if Assigned(FEditorForm) then FreeAndNil(FEditorForm);
  if Assigned(FVstShellPlugins) then FreeAndNil(FVstShellPlugins);
 finally
  inherited;
 end;
end;

(*
function TCustomVSTModule.GetEditorFormClassName: string;
begin
 Result := FEditorFormClass.ClassName;
end;

procedure TCustomVSTModule.SetEditorFormClassName(Value: string);
begin

 FEditorFormClass :=
end;
*)

procedure TCustomVSTModule.HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
var
  Ins     : TDAVArrayOfSingleDynArray absolute Inputs;
  Outs    : TDAVArrayOfSingleDynArray absolute Outputs;
  OutsTmp : TDAVArrayOfSingleDynArray;
  i, j    : Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallProcess'); {$ENDIF}
 if Assigned(FOnProcessEx)
  then FOnProcessEx(Ins, Outs, SampleFrames)
  else if Assigned(FOnProcess32ReplacingEx) then
   begin
    SetLength(OutsTmp, FEffect.NumOutputs, SampleFrames);
    ClearArrays(OutsTmp, FEffect.NumOutputs, SampleFrames);
    FOnProcess32ReplacingEx(Ins, OutsTmp, SampleFrames);
    for i := 0 to FEffect.NumOutputs - 1 do
     for j := 0 to SampleFrames - 1
      do Outs[i, j] := Outs[i, j] + OutsTmp[i, j];
   end;
end;

procedure TCustomVSTModule.HostCallProcess32Replacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
var
  Ins  : TDAVArrayOfSingleDynArray absolute Inputs;
  Outs : TDAVArrayOfSingleDynArray absolute Outputs;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallProcess32Replacing'); {$ENDIF}
 if Assigned(FOnProcess32ReplacingEx)
  then FOnProcess32ReplacingEx(Ins, Outs, SampleFrames);
end;

procedure TCustomVSTModule.HostCallProcess64Replacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer);
var
  Ins  : TDAVArrayOfDoubleDynArray absolute Inputs;
  Outs : TDAVArrayOfDoubleDynArray absolute Outputs;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallProcess64Replacing'); {$ENDIF}
 if Assigned(FOnProcess64ReplacingEx) then FOnProcess64ReplacingEx(Ins, Outs,SampleFrames);
end;

function TCustomVSTModule.GetHostProduct: AnsiString;
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

function TCustomVSTModule.GetHostVendor: AnsiString;
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

procedure TCustomVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
var
  rUID : TChunkName;
  i    : Integer;
  hv   : Boolean;
begin
 {$IFDEF DebugLog} AddLogMessage('TCustomVSTModule.SetAudioMaster'); {$ENDIF}

 inherited;

 {$IFDEF DELPHI14_UP}
 hv := (AnsiStrings.AnsiPos('WaveLab', HostProduct) < 0) {or (shortstring(temp) <> 'energyXT')};
 {$ELSE}
 hv := (Pos('WaveLab', HostProduct) < 0) {or (shortstring(temp) <> 'energyXT')};
 {$ENDIF}

 {$IFDEF DebugLog} AddLogMessage('After check for Wavelab in HostProduct'); {$ENDIF}

 if hv then hv := (CanDo['shellCategory'] = 1);

 if (PlugCategory = vpcShell) and hv then
  begin
   rUID := getCurrentUniqueId;

   {$IFDEF DebugLog} AddLogMessage('After get current ID'); {$ENDIF}

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
         FPlugCategory := PlugCategory;
         if Assigned(OnInstanciate) then
          begin
           System.Move(UniqueID[1], rUID, Min(4, Length(UniqueID)));
           OnInstanciate(Self, rUID);
          end;
         IOChanged;
        end;
    end;
  end
 else
  if (PlugCategory = vpcShell)
   then PlugCategory := vpcUnknown;
end;

function TCustomVSTModule.HostCallDispatchEffect(const Opcode: TDispatcherOpcode; const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := inherited HostCallDispatchEffect(Opcode, Index, Value, ptr, opt);
 if Assigned(FOnDispatcher) then FOnDispatcher(Self, Opcode);

 {$IFDEF DebugLog}
 if not (Opcode in [effIdle, effEditIdle])
  then AddLogMessage(' Opcode: ' + Opcode2String(Opcode) +
                     ' Index: ' + IntToStr(Index) +
                     ' Value: ' + IntToStr(Value) +
                     ' Single: ' + FloatToStr(opt));
 {$ENDIF}
end;

procedure TCustomVSTModule.ReadOnlyString(s: string); begin end;

function TCustomVSTModule.HostCallOpen(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallOpen'); {$ENDIF}
 Result := inherited HostCallOpen(Index, Value, ptr, opt);
 if Assigned(FOnOpen) then FOnOpen(Self);
end;

function TCustomVSTModule.HostCallClose(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallClose'); {$ENDIF}
 if Assigned(FOnClose) then FOnClose(Self);
 Result := inherited HostCallClose(Index, Value, ptr, opt);
end;

function TCustomVSTModule.HostCallGetVu(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  s: Single;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetVu'); {$ENDIF}
 if Assigned(FOnGetVUEvent) then
  begin
   s := 0;
   FOnGetVUEvent(s);
   Result := round(s * 32767);
  end else Result := 0;
end;

function TCustomVSTModule.HostCallSetSampleRate(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallSetSampleRate (' + FloatToStr(Opt) + ')'); {$ENDIF}
 SetSampleRate(opt);
 Result := 1;
end;

function TCustomVSTModule.HostCallSetBlockSize(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallSetBlockSize (' + IntToStr(Value) + ')'); {$ENDIF}
 setBlockSize(Value);
 Result := 0;
end;

function TCustomVSTModule.HostCallMainsChanged(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallMainsChanged (' + IntToStr(Value) + ')'); {$ENDIF}
 if (Value = 0) then
  if Assigned(FOnSuspend)
   then FOnSuspend(Self)
   else
    begin
     if Assigned(FOnResume) then FOnResume(Self);
     WantEvents(1);
    end;

  Result := 0;
end;

function TCustomVSTModule.HostCallEditGetRect(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallEditGetRect'); {$ENDIF}
 Result := 0;
 if Assigned(ptr) then
  begin
   PPERect(ptr)^ := @FEditorRect;
   if Assigned(ptr) then
    begin
     FEditorRect.Top := 0;
     FEditorRect.Left := 0;

     if Assigned(FEditorForm) then
      begin
       FEditorRect.Bottom := Max(100, FEditorForm.ClientHeight);
       FEditorRect.Right := Max(100, FEditorForm.ClientWidth);
       Result := 1;
      end;
    end;
  end;
end;

function TCustomVSTModule.HostCallEditOpen(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
{$IFDEF DARWIN}
var
  ViewRef : HIViewRef;
{$ENDIF}
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallEditOpen'); {$ENDIF}
 Result := 0;
 if (effFlagsHasEditor in FEffect.EffectFlags) and Assigned(ptr) then
  begin
   // if an editor form is assigned create the form and assign it
   if Assigned(FEditorFormClass)
    then FEditorForm := FEditorFormClass.Create(Self);

   // the below code is about to be removed soon!
   if Assigned(FOnEditOpen)
    then FOnEditOpen(Self, FEditorForm, THandle(ptr));

   if Assigned(FEditorForm) then
    try
     Result := 1;
     with FEditorForm do
      begin
       ParentWindow := HWnd(ptr);
       Visible := True;
       BorderStyle := bsNone;
       SetBounds(0, 0, Width, Height);
       Invalidate;
      end;
    except
    end;
  end;
end;

function TCustomVSTModule.HostCallEditClose(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  DestroyForm: Boolean;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallEditClose'); {$ENDIF}
 if (effFlagsHasEditor in FEffect.EffectFlags) then
  begin
   DestroyForm := True;
   if Assigned(FOnEditClose) then FOnEditClose(Self, DestroyForm);
   if DestroyForm and Assigned(FEditorForm) then FreeAndNil(FEditorForm);
  end;

 Result := 0;
end;

function TCustomVSTModule.HostCallEditIdle(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 if (effFlagsHasEditor in FEffect.EffectFlags) and FEditorNeedUpdate and Assigned(FEditorForm)then
  begin
   if Assigned(FOnEditIdle) then FOnEditIdle(Self);
   FEditorNeedUpdate := False;
  end;

 Result := 0;
end;

function TCustomVSTModule.HostCallEditTop(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallEditTop'); {$ENDIF}
 if Assigned(FOnEditTop) then FOnEditTop(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallEditSleep(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallEditSleep'); {$ENDIF}
 if Assigned(FOnEditSleep) then FOnEditSleep(Self);
 Result := 0;
end;

function TCustomVSTModule.HostCallProcessEvents(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallProcessEvents'); {$ENDIF}
 Result := 1;
end;

function TCustomVSTModule.HostCallConnectInput(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallConnectInput'); {$ENDIF}
 if Assigned(FOnInConnected) then FOnInConnected(Self, Index, (Value <> 0));
 Result := 1;
end;

function TCustomVSTModule.HostCallConnectOutput(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallConnectOutput'); {$ENDIF}
 if Assigned(FOnOutConnected) then FOnOutConnected(Self, Index, (Value <> 0));
 Result := 1;
end;

function TCustomVSTModule.HostCallGetInputProperties(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  cpf : TVstPinPropertiesFlags;
  str : string;
begin
 Result := 0;
 {$IFDEF DebugLog} AddLogMessage('HostCallGetInputProperties'); {$ENDIF}
 if (Index < FEffect.numInputs) then
  with PVstPinProperties(ptr)^ do
   begin
    str := 'Input #' + IntToStr(Index + 1) + #0;
    System.Move(str[1], Caption, Length(str));
    str := 'In' + IntToStr(Index + 1) + #0;
    System.Move(str[1], ShortLabel, Length(str));

    case numInputs of
     1: ArrangementType := satMono;
     2: ArrangementType := satStereo;
     4: ArrangementType := sat40Music;
    end;
    cpf := [vppIsActive];
    if numInputs = 2 then Include(cpf, vppIsStereo);

    if vppIsActive in cpf then Flags := [vppIsActive] else Flags := [];
    if vppIsStereo in cpf then Include(Flags, vppIsStereo);
    if vppUseSpeaker in cpf then Include(Flags, vppUseSpeaker);

    if Assigned(FOnGetInputProperties)
     then Result := Integer(FOnGetInputProperties(Self, Index, PVstPinProperties(ptr)^))
     else Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetOutputProperties(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  cpf : TVstPinPropertiesFlags;
  str : string;
begin
 Result := 0;
 {$IFDEF DebugLog} AddLogMessage('HostCallGetOutputProperties'); {$ENDIF}
 if (Index < FEffect.numOutputs) then
  with PVstPinProperties(ptr)^ do
   begin
    str := 'Output #' + IntToStr(Index + 1) + #0;
    System.Move(str[1], Caption, Length(str));
    str := 'Out' + IntToStr(Index + 1) + #0;
    System.Move(str[1], ShortLabel, Length(str));

    case numOutputs of
     1: ArrangementType := satMono;
     2: ArrangementType := satStereo;
     4: ArrangementType := sat40Music;
    end;
    cpf := [vppIsActive];
    if numOutputs = 2 then Include(cpf, vppIsStereo);

    if vppIsActive in cpf then Flags := [vppIsActive] else Flags := [];
    if vppIsStereo in cpf then Include(Flags, vppIsStereo);
    if vppUseSpeaker in cpf then Include(Flags, vppUseSpeaker);

    if Assigned(FOnGetOutputProperties)
     then Result := Integer(FOnGetOutputProperties(Self, Index, PVstPinProperties(ptr)^))
     else Result := 1;
   end;
end;

function TCustomVSTModule.HostCallGetPlugCategory(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetPlugCategory'); {$ENDIF}
 Result := Integer(FPlugCategory);
end;

function TCustomVSTModule.HostCallOfflineNotify(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallOfflineNotify'); {$ENDIF}
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

function TCustomVSTModule.HostCallOfflinePrepare(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallOfflinePrepare'); {$ENDIF}
 Result := OfflinePrepare(PVstOfflineTaskRecord(ptr), Value);
end;

function TCustomVSTModule.HostCallOfflineRun(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallOfflineRun'); {$ENDIF}
 Result := OfflineRun(PVstOfflineTaskRecord(ptr), Value);
end;

function TCustomVSTModule.HostCallProcessVarIo(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallProcessVarIo'); {$ENDIF}
 if Assigned(FOnProcessVarIO) then
  begin
   FOnProcessVarIO(Self, PVstVariableIo(ptr)^);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallSetBlockSizeAndSampleRate(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallSetBlockSizeAndSampleRate: Blocksize ' + IntToStr(Value) + ' Samplerate ' + FloatToStr(opt)); {$ENDIF}
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

function TCustomVSTModule.HostCallSetBypass(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
  {$IFDEF DebugLog}
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

function TCustomVSTModule.HostCallGetEffectName(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetEffectName'); {$ENDIF}
 Result := 0;
 if Assigned(ptr) then
  begin
   StrPCopy(ptr, FEffectName);
   Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetVendorString(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetVendorString'); {$ENDIF}
 Result := 0;
 if Assigned(ptr) then
  begin
   StrPCopy(ptr, FVendorName);
   Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetProductString(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetProductString'); {$ENDIF}
 Result := 0;
 if Assigned(ptr) then
  begin
   if FProductName <> ''
    then StrPCopy(ptr, FProductName)
    else StrPCopy(ptr, FEffectName);
   Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetVendorVersion(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetVendorVersion'); {$ENDIF}
 Result := FEffect.Version;
end;

function TCustomVSTModule.HostCallVendorSpecific(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallVendorSpecific'); {$ENDIF}
 if Assigned(FOnVendorSpecific)
  then Result := FOnVendorSpecific(Self, Index, Value, ptr, opt)
  else Result := 0;
end;

function TCustomVSTModule.HostCallCanDo(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := 0;
 {$IFDEF DebugLog} AddLogMessage('HostCallCanDo (' + StrPas(PChar(ptr)) + ')'); {$ENDIF}
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

function TCustomVSTModule.HostCallGetTailSize(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetTailSize'); {$ENDIF}
 Result := FTailSize;
end;

function TCustomVSTModule.HostCallKeysRequired(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallKeysRequired'); {$ENDIF}
 Result := Integer(not FKeysRequired); // reversed to keep v1 compatibility
end;

function TCustomVSTModule.HostCallGetVstVersion(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetVstVersion'); {$ENDIF}
 Result := 2400;
end;

function TCustomVSTModule.HostCallEditKeyDown(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  keyCode : TVstKeyCode;
  a, b    : Integer;
  Hndl    : THandle;
begin
 Result := 0;
 {$IFDEF DebugLog} AddLogMessage('HostCallEditKeyDown'); {$ENDIF}
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

function TCustomVSTModule.HostCallEditKeyUp(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
var
  keyCode : TVstKeyCode;
  a, b    : Integer;
  Hndl    : THandle;
begin
 Result := 0;
 {$IFDEF DebugLog} AddLogMessage('HostCallEditKeyDown'); {$ENDIF}
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

function TCustomVSTModule.HostCallSetEditKnobMode(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallSetEditKnobMode (' + IntToStr(Value) + ')'); {$ENDIF}
 if Assigned(FOnSetKnobMode) then
  begin
   FOnSetKnobMode(Self, Value);
   Result := 1;
  end else Result := 0;
end;

function TCustomVSTModule.HostCallGetSpeakerArrangement(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallGetSpeakerArrangement'); {$ENDIF}
 Result := Integer(GetSpeakerArrangement(PVstSpeakerArrangement(Pointer(Value)^), PVstSpeakerArrangement(ptr^)));
end;

function TCustomVSTModule.HostCallShellGetNextPlugin(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallShellGetNextPlugin'); {$ENDIF}
 if (FCurrentVstShellPlugin < FVstShellPlugins.Count) and Assigned(Ptr) then
  begin
   StrPCopy(Ptr, FVstShellPlugins[FCurrentVstShellPlugin].DisplayName);
   Result := TVstIntPtr(FVstShellPlugins[FCurrentVstShellPlugin].UniqueID);
   Inc(FCurrentVstShellPlugin);
  end
 else
  begin
   Result := 0;
   FCurrentVstShellPlugin := 0;
  end;
end;

function TCustomVSTModule.HostCallStartProcess(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := 1;
 {$IFDEF DebugLog} AddLogMessage('HostCallStartProcess'); {$ENDIF}
 if Assigned(FOnStartProcess)
  then FOnStartProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.HostCallStopProcess(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := 1;
 {$IFDEF DebugLog} AddLogMessage('HostCallStopProcess'); {$ENDIF}
 if Assigned(FOnStopProcess)
  then FOnStopProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.HostCallSetTotalSampleToProcess(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallSetTotalSampleToProcess'); {$ENDIF}
 Result := Value;
end;

function TCustomVSTModule.HostCallSetPanLaw(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 Result := 1;
 {$IFDEF DebugLog} AddLogMessage('HostCallSetPanLaw'); {$ENDIF}
 if Assigned(FOnSetPanLaw)
  then FOnSetPanLaw(Self, TVstPanLawType(Value), Opt)
  else Result := 0;
end;

function TCustomVSTModule.HostCallSetProcessPrecision(const Index: Integer; const Value: TVstIntPtr; const ptr: pointer; const opt: Single): TVstIntPtr;
begin
 {$IFDEF DebugLog} AddLogMessage('HostCallSetProcessPrecision'); {$ENDIF}
 Result := Integer(fProcessPrecisition); // [value]: @see VstProcessPrecision  @see AudioEffectX::setProcessPrecision
end;

function TCustomVSTModule.GetUniqueID: AnsiString;
begin
 Result := FEffect.UniqueID[3] +
           FEffect.UniqueID[2] +
           FEffect.UniqueID[1] +
           FEffect.UniqueID[0];
end;

procedure TCustomVSTModule.SetUniqueID(const Value: AnsiString);
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

procedure TCustomVSTModule.SetOnProcess64ReplacingEx(
  const Value: TProcessDoubleEvent);
begin
 if @FOnProcess64ReplacingEx <> @Value then
  begin
   FOnProcess64ReplacingEx := Value;
   OnProcess64ReplacingExChanged;
  end;
end;

procedure TCustomVSTModule.SetOnProcessEx(const Value: TProcessAudioEvent);
begin
 if @FOnProcessEx <> @Value then
  begin
   FOnProcessEx := Value;
   OnProcessExChanged;
  end;
end;

procedure TCustomVSTModule.SetOnProcess32ReplacingEx(
  const Value: TProcessAudioEvent);
begin
 if @FOnProcess32ReplacingEx <> @Value then
  begin
   FOnProcess32ReplacingEx := Value;
   OnProcess32ReplacingExChanged;
  end;
end;

procedure TCustomVSTModule.SetPluginFlags(newFlags : TEffFlags);
begin
 if FEffect.EffectFlags <> newFlags then
  begin
   FEffect.EffectFlags := newFlags;
   EffectFlagsChanged;
  end;
end;

procedure TCustomVSTModule.OnProcess64ReplacingExChanged;
begin
 // nothing todo here
end;

procedure TCustomVSTModule.OnProcessExChanged;
begin
 // nothing todo here
end;

procedure TCustomVSTModule.OnProcess32ReplacingExChanged;
begin
 // nothing todo here
end;

procedure TCustomVSTModule.EffectFlagsChanged;
begin
 // nothing todo here
end;

function TCustomVSTModule.GetPluginFlags: TEffFlags;
begin
 Result := FEffect.EffectFlags;
end;

function TCustomVSTModule.GetSpeakerArrangement(var InputArrangement,
  OutputArrangement: PVstSpeakerArrangement): Boolean;
begin
 {$IFDEF DebugLog} AddLogMessage('GetSpeakerArrangement'); {$ENDIF}
 result := False;
end;

procedure TCustomVSTModule.SetInitialDelay(const Delay: Integer);
begin
 if FInitialDelay <> Delay then
  begin
   FInitialDelay := Delay;
   InitialDelayChanged;
  end;
end;

procedure TCustomVSTModule.InitialDelayChanged;
begin
 FEffect.InitialDelay := FInitialDelay;

 {$IFDEF DELPHI14_UP}
 if AnsiStrings.AnsiPos('WaveLab', HostProduct) > 0
 {$ELSE}
 if Pos('WaveLab', HostProduct) > 0
 {$ENDIF}
  then IOChanged else
 if HostProduct <> 'energyXT' then IOChanged;
end;

procedure TCustomVSTModule.EditorPostUpdate;
begin
 FEditorNeedUpdate := True;
end;

{$IFDEF UseDelphi}
procedure TCustomVSTModule.ReadState(Reader: TReader);
begin
 {$IFDEF DebugLog} AddLogMessage('Before ReadState'); {$ENDIF}
 inherited;
 {$IFDEF DebugLog} AddLogMessage('After ReadState'); {$ENDIF}

 if Assigned(FOnInitialize) then FOnInitialize(Self);
 {$IFDEF DebugLog} AddLogMessage('End ReadState'); {$ENDIF}
end;
{$ENDIF}

function TCustomVSTModule.UpdateSampleRate: Double;
begin
  {$IFDEF DebugLog} AddLogMessage('Update Samplerate'); {$ENDIF}

  Result := inherited UpdateSampleRate;
  if (Result>0) and (Result<>FSampleRate) then
  begin
    FSampleRate := Result;
    SampleRateChanged;
  end;
end;

function TCustomVSTModule.UpdateBlockSize: Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('UpdateBlockSize'); {$ENDIF}
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
 if Assigned(FOnSpeakerArrangementChanged)
  then FOnSpeakerArrangementChanged(Self, Input, Output);
end;

function TCustomVSTModule.HostCallSetSpeakerArrangement(const Index: Integer;
  const Value: TVstIntPtr; const ptr: Pointer; const opt: Single): TVstIntPtr;
begin
 Result := 0;
 if Assigned(ptr) and (Value <> 0) then
  begin
   SetSpeakerArrangement(PVstSpeakerArrangement(Value)^, PVstSpeakerArrangement(Ptr)^);
   Result := 1;
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

procedure TCustomVSTModule.SetEffectName(const Value: AnsiString);
begin
 FEffectName := Value;
 if FTruncateStrings and (Length(FEffectName) > 32)
  then SetLength(FEffectName, 32);
end;

procedure TCustomVSTModule.SetVendorName(const Value: AnsiString);
begin
 FVendorName := Value;
 if FTruncateStrings and (Length(FVendorName) > 64)
  then SetLength(FVendorName, 64);
end;

procedure TCustomVSTModule.SetProductName(const Value: AnsiString);
begin
 FProductName := Value;
 if FTruncateStrings and (Length(FProductName) > 64)
  then SetLength(FProductName, 64);
end;

procedure TCustomVSTModule.SetVersionMajor(Value: Integer);
begin
 if FVersionMajor <> Value then
  begin
   FVersionMajor := Value;
   VersionMajorChanged;
  end;
end;

procedure TCustomVSTModule.SetVersionMinor(Value: Integer);
begin
 if FVersionMinor <> Value then
  begin
   FVersionMinor := Value;
   VersionMinorChanged;
  end;
end;

procedure TCustomVSTModule.SetVersionRelease(Value: Integer);
begin
 if FVersionRelease <> Value then
  begin
   FVersionRelease := Value;
   VersionReleaseChanged;
  end;
end;

procedure TCustomVSTModule.VersionMajorChanged;
begin
 UpdateVersion;
end;

procedure TCustomVSTModule.VersionMinorChanged;
begin
 UpdateVersion;
end;

procedure TCustomVSTModule.VersionReleaseChanged;
begin
 UpdateVersion;
end;

procedure TCustomVSTModule.UpdateVersion;
begin
 if FVersionRelease < 100
  then FEffect.version := 1000 * FVersionMajor + 100 * FVersionMinor + FVersionRelease
  else FEffect.version := (FVersionMajor shl 16) + (FVersionMinor shl 8) + FVersionRelease;
end;

end.
