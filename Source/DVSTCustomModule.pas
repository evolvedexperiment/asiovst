unit DVSTCustomModule;

interface

{$I ASIOVST.INC}

uses
  {$IFDEF FPC}LCLIntf, LMessages, Controls, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Forms, DVSTEffect, DVSTShellPlugins, DVSTBasicModule, DAVDCommon;

type
  TVstCanDo = (vcdSendVstEvents,       vcdSendVstMidiEvent,      vcdSendVstTimeInfo, vcdReceiveVstEvents,
               vcdReceiveVstMidiEvent, vcdReceiveVstTimeInfo,    vcdOffline,         vcdPlugAsChannelInsert,
               vcdPlugAsSend,          vcdMixDryWet,             vcdNoRealTime,      vcdMultipass,
               vcdMetapass,            vcd1in1out,               vcd1in2out,         vcd2in1out,
               vcd2in2out,             vcd2in4out,               vcd4in2out,         vcd4in4out,
               vcd4in8out,             vcd8in4out,               vcd8in8out,         vcdMidiProgramNames,
               vcdLiveWithoutToolbar,  vcdConformsToWindowRules, vcdBypass);
               
  TVstCanDos = set of TVstCanDo;

  TChannelPropertyFlags = set of (cpfIsActive, cpfIsStereo, cpfUseSpeaker);

  TProcessAudioEvent     = procedure(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer) of object;
  TProcessDoubleEvent    = procedure(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer) of object;
  TGetVUEvent            = procedure(var VU:Single) of object;
  TBlockSizeChangeEvent  = procedure(Sender: TObject; const BlockSize: Integer) of object;
  TSampleRateChangeEvent = procedure(Sender: TObject; const SampleRate: Single) of object;
  TOnDispatcherEvent     = procedure(Sender: TObject; opCode: TDispatcherOpcode) of object;
  TOfflineNotifyEvent    = procedure(Sender: TObject; AudioFile: TVstAudioFile; numAudioFiles: Integer; start: Boolean) of object;
  TOfflinePrepareEvent   = procedure(Sender: TObject; OfflineTask: TVstOfflineTask; count: Integer) of object;
  TOfflineRunEvent       = procedure(Sender: TObject; OfflineTask: TVstOfflineTask; count: Integer) of object;
  TVSTKeyEvent           = procedure(Sender: TObject; var keyCode : TVstKeyCode) of object;
  TProcessVarIOEvent     = procedure(Sender: TObject; varIo: TVstVariableIo) of object;
  TInOutConnectedEvent   = procedure(Sender: TObject; Index: Integer; State: Boolean) of object;
  TSetKnobModeEvent      = procedure(Sender: TObject; val: Integer) of object;
  TSoftBypassEvent       = procedure(Sender: TObject; isBypass: Boolean) of object;
  TOnSetPanLawEvent      = procedure(Sender: TObject; var vType: Integer; var val: single) of object;
  TGetEditorEvent        = procedure(Sender: TObject; var GUI: TForm; ParentWindow : THandle) of object;
  TOnVendorSpecificEvent = function(Sender: TObject; lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer of object;
  TOnCanDoEvent          = function(Sender: TObject; CanDoText: String): Integer of object;
  TOnCheckKey            = function(Sender: TObject; Key: Char): Boolean of object;

  TOnGetChannelPropertiesEvent = function(Sender: TObject; var vLabel: ShortString; var shortLabel: ShortString; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags:TChannelPropertyFlags): Integer of object;

  TCustomVSTModule = class(TBasicVSTModule)
  private
    FAbout                  : string;
    FVersion                : string;
    FEditorRect             : ERect;

    FOnEditClose            : TNotifyEvent;
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
    FNumCategories          : Integer;
    FEditorNeedUpdate       : Boolean;
    FEditorForm             : TForm;
    FSampleRate             : Single;
    FBlockSize              : Integer;
    FEffectName             : string;
    FVendorName             : string;
    FVersionMajor           : Integer;
    FVersionMinor           : Integer;
    FVersionRelease         : Integer;
    FProductName            : string;
    FHostProduct            : string;
    FInitialDelay           : Integer;
    FOnOpen                 : TNotifyEvent;
    FOnClose                : TNotifyEvent;
    FOnEditOpen             : TGetEditorEvent;
    FOnProcessEx            : TProcessAudioEvent;
    FOnProcessReplacingEx   : TProcessAudioEvent;
    FOnProcessDoublesEx     : TProcessDoubleEvent;


    procedure SetAudioMaster(const AM :TAudioMasterCallbackFunc); override;
    procedure SetNumInputs(Inputs: Integer); virtual;
    procedure SetNumOutputs(Outputs: Integer); virtual;
    procedure SetSampleRate(newValue: Single); virtual;
    procedure SetBlockSize(newValue: Integer); virtual;
    function GetUniqueID:string; virtual;
    procedure SetUniqueID(fID:string); virtual;
    procedure SetPluginFlags(newFlags : TEffFlags); virtual;
    function GetPluginFlags: TEffFlags; virtual;
    procedure SetInitialDelay(delay: Integer); virtual;
    {$IFDEF UseDelphi}
    procedure ReadState(Reader: TReader); override;
    {$ENDIF}
    procedure SetEffectName(const Value: string);
    procedure SetVersionMajor(Value: Integer);
    procedure SetVersionMinor(Value: Integer);
    procedure SetVersionRelease(Value: Integer);
    procedure UpdateVersion;
    procedure SampleRateChanged; virtual;
    procedure BlockSizeChanged; virtual;

    procedure SetBlockSizeAndSampleRate(aBlockSize: Integer; aSampleRate: Single); virtual;

    function AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean; virtual;   // Allocate memory for a VstSpeakerArrangement containing the given number of channels
    function DeallocateArrangement(var Arrangement: PVstSpeakerArrangement): Boolean; virtual;                      // Delete/free memory for a speaker Arrangement
    function CopySpeaker(copyTo, copyFrom: PVstSpeakerProperties): Boolean; virtual;    // Feed the "to" speaker Properties with the same Values than "from"'s ones. It is assumed here that "to" exists yet, ie this function won't allocate memory for the speaker (this will prevent from having a difference between an Arrangement's number of channels and its actual speakers...)
    function MatchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): Boolean; virtual;    // "to" is deleted, then created and initialized with the same Values as "from" ones ("from" must exist).
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   EditorPostUpdate; virtual;

    procedure HostCallDispatchEffect(opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single); override;
    procedure HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer); override;
    procedure HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer); override;

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

    function UpdateSampleRate: Double; override;
    function UpdateBlockSize: Integer; override;


    // Properties
    property EditorForm: TForm read FEditorForm;
    property EditorNeedUpdate: Boolean read FEditorNeedUpdate write FEditorNeedUpdate;

    property Flags: TEffFlags read GetPluginFlags write SetPluginFlags;

    property SampleRate: Single read fSampleRate write SetSampleRate;
    property numInputs: Integer read FEffect.numInputs write SetNumInputs default 2;
    property numOutputs: Integer read FEffect.numOutputs write SetNumOutputs default 2;
    property InitialDelay: Integer read FEffect.initialDelay write SetInitialDelay default 0;
    property RealQualities: Integer read FEffect.realQualities write FEffect.realQualities default 0;
    property OffQualities: Integer read FEffect.offQualities write FEffect.offQualities default 0;
    property IORatio: Integer read FEffect.ioRatio write FEffect.ioRatio default 1;
    property About: string read FAbout write ReadOnlyString stored False;
    property Version: string read FVersion write FVersion;
    property UniqueID: string read GetUniqueID write setUniqueID;
    property numCategories: Integer read fNumCategories write fNumCategories default 1;
    property EffectName: string read FEffectName write SetEffectName;
    property ProductName: string read fProductName write fProductName;
    property VendorName: string read fVendorName write fVendorName;
    property VersionMajor: Integer read FVersionMajor write SetVersionMajor default 1;
    property VersionMinor: Integer read FVersionMinor write SetVersionMinor default 0;
    property VersionRelease: Integer read FVersionRelease write SetVersionRelease default 0;
    property PlugCategory: TVstPluginCategory read fPlugCategory write fPlugCategory default vpcUnknown;
    property ProcessPrecisition: TProcessPrecision read FProcessPrecisition write FProcessPrecisition default pp32;
    property KeysRequired: Boolean read FKeysRequired write SetKeysRequired default False;
    property Tempo: Single read fTempo;
    property ShellPlugins: TCustomVstShellPlugins read FVstShellPlugins write SetVstShellPlugins;
    property TailSize: Integer read FTailSize write FTailSize default 0;
    property CanDos: TVstCanDos read fCanDos write fCanDos;
    property HostProduct: string read GetHostProduct stored false;
    property HostVendor: string read GetHostVendor stored false;
    property HostVersion: Integer read GetHostVendorVersion stored false;

    property OnBlockSizeChange: TBlockSizeChangeEvent read fBlockSizeChangeEvent write fBlockSizeChangeEvent;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnResume: TNotifyEvent read FOnResume write FOnResume;
    property OnSuspend: TNotifyEvent read FOnSuspend write FOnSuspend;
    property OnEditOpen: TGetEditorEvent read FOnEditOpen write FOnEditOpen;
    property OnEditClose: TNotifyEvent read FOnEditClose write FOnEditClose;
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

uses SysUtils, Math, {$IFDEF PUREPASCAL}DAVDBufferMathPascal{$ELSE}DAVDBufferMathAsm{$ENDIF};


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
 FVersion := '0.0';
 FAbout := 'VST Plugin Template by Christian Budde, Tobybear & MyCo';
 FProcessPrecisition := pp32;
 FKeysRequired := False;
 FTailSize := 0;
 FVersionMajor := 1;
 FVersionMinor := 0;
 FVersionRelease := 0;
 UpdateVersion;
 FEditorForm := nil;


 FSampleRate := 44100;
 FBlockSize := 1024;
 FVstShellPlugins := TCustomVstShellPlugins.Create(Self);
 FCurrentVstShellPlugin := 0;
 FNumCategories := 1;
end;


{$IFDEF CONVERT_TO_DYNARRAY}
  procedure TCustomVSTModule.HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
  var Ins  : TAVDArrayOfSingleDynArray absolute Inputs;
      Outs : TAVDArrayOfSingleDynArray absolute Outputs;
      OutsTmp, tmpI, tmpO: TAVDArrayOfSingleDynArray;
  begin
    if Assigned(FOnProcessEx) then
    begin
      CreateArrayCopy(Ins,tmpI, FEffect.NumOutputs, SampleFrames);
      CreateArrayCopy(Outs,tmpO, FEffect.NumOutputs, SampleFrames);

      FOnProcessEx(tmpI, tmpO, SampleFrames);

      CopyArrays(tmpO, Outs, FEffect.NumOutputs, SampleFrames);
      CopyArrays(tmpI, Ins, FEffect.NumOutputs, SampleFrames);
    end else if Assigned(FOnProcessReplacingEx) then
    begin
      CreateArrayCopy(Ins,tmpI, FEffect.NumOutputs, SampleFrames);
      CreateEmptyArray(OutsTmp, FEffect.NumOutputs, SampleFrames);

      FOnProcessReplacingEx(tmpI, OutsTmp, SampleFrames);

      AddArrays(OutsTmp, Outs, Outs, FEffect.NumOutputs, SampleFrames);
      CopyArrays(tmpI, Ins, FEffect.NumOutputs, SampleFrames);
    end;
  end;

{$ELSE}
  procedure TCustomVSTModule.HostCallProcess(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
  var Ins  : TAVDArrayOfSingleDynArray absolute Inputs;
      Outs : TAVDArrayOfSingleDynArray absolute Outputs;
      OutsTmp: TAVDArrayOfSingleDynArray;
  begin
    if Assigned(FOnProcessEx) then FOnProcessEx(Ins, Outs, SampleFrames)
    else if Assigned(FOnProcessReplacingEx) then
    begin
      CreateEmptyArray(OutsTmp, FEffect.NumOutputs, SampleFrames);

      FOnProcessReplacingEx(Ins, OutsTmp, SampleFrames);

      AddArrays(Outs, OutsTmp, Outs, FEffect.NumOutputs, SampleFrames);
    end;
  end;

{$ENDIF}

{$IFDEF CONVERT_TO_DYNARRAY}
  procedure TCustomVSTModule.HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
  var Ins  : TAVDArrayOfSingleDynArray absolute Inputs;
      Outs : TAVDArrayOfSingleDynArray absolute Outputs;
      tmpI, tmpO: TAVDArrayOfSingleDynArray;
  begin
    if Assigned(FOnProcessReplacingEx) then
    begin
      CreateArrayCopy(Ins,tmpI, FEffect.NumOutputs, SampleFrames);
      CreateArrayCopy(Outs,tmpO, FEffect.NumOutputs, SampleFrames);

      FOnProcessReplacingEx(tmpI, tmpO, SampleFrames);

      CopyArrays(tmpO, Outs, FEffect.NumOutputs, SampleFrames);
      CopyArrays(tmpI, Ins, FEffect.NumOutputs, SampleFrames);
    end;
  end;
{$ELSE}
  procedure TCustomVSTModule.HostCallProcessReplacing(const Inputs, Outputs: PPSingle; const SampleFrames: Integer);
  var Ins  : TAVDArrayOfSingleDynArray absolute Inputs;
      Outs : TAVDArrayOfSingleDynArray absolute Outputs;
  begin
    if Assigned(FOnProcessReplacingEx) then FOnProcessReplacingEx(Ins,Outs,SampleFrames);
  end;
{$ENDIF}



{$IFDEF CONVERT_TO_DYNARRAY}
  procedure TCustomVSTModule.HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer);
  var Ins  : TAVDArrayOfDoubleDynArray absolute Inputs;
      Outs : TAVDArrayOfDoubleDynArray absolute Outputs;
      tmpI, tmpO: TAVDArrayOfDoubleDynArray;
  begin
    if Assigned(FOnProcessDoublesEx) then
    begin
      CreateArrayCopy(Ins,tmpI, FEffect.NumOutputs, SampleFrames);
      CreateArrayCopy(Outs,tmpO, FEffect.NumOutputs, SampleFrames);

      FOnProcessDoublesEx(tmpI, tmpO, SampleFrames);

      CopyArrays(tmpO, Outs, FEffect.NumOutputs, SampleFrames);
      CopyArrays(tmpI, Ins, FEffect.NumOutputs, SampleFrames);
    end;
  end;
{$ELSE}
  procedure TCustomVSTModule.HostCallProcessDoubleReplacing(const Inputs, Outputs: PPDouble; const SampleFrames: Integer);
  var Ins  : TAVDArrayOfDoubleDynArray absolute Inputs;
      Outs : TAVDArrayOfDoubleDynArray absolute Outputs;
  begin
    if Assigned(FOnProcessDoublesEx) then FOnProcessDoublesEx(Ins,Outs,SampleFrames);
  end;
{$ENDIF}




destructor TCustomVSTModule.Destroy;
begin
 try
  if Assigned(FEditorForm) then FreeAndNil(FEditorForm);
  if Assigned(FVstShellPlugins) then FreeAndNil(FVstShellPlugins);
  {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}
 finally
  {$IFDEF Debug} FLog.Free; {$ENDIF}
  inherited;
 end;
end;

Procedure TCustomVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
var rUID : Integer;
    i,j  : Integer;
    sUID : string;
    hv   : boolean;
begin
 inherited;
 hv := (HostProduct<>'WaveLab') {or (shortstring(temp)<>'energyXT')};
 if hv then hv := (canHostDo('shellCategory')=1);

 if (PlugCategory=vpcShell) and hv then
  begin
   rUID := getCurrentUniqueId;
   if (rUID>0) then
    begin
     for i := 0 to ShellPlugins.Count-1 do
      if rUID=ShellPlugins[i].UID then Break;
     if i<ShellPlugins.Count then
      if (rUID=ShellPlugins[i].UID) then
       begin
        FEffect.uniqueID := rUID;
        if ShellPlugins[i].NumInputs>=0 then FEffect.numInputs := ShellPlugins[i].NumInputs;
        if ShellPlugins[i].NumOutputs>=0 then FEffect.numOutputs := ShellPlugins[i].NumOutputs;
        if ShellPlugins[i].NumPrograms>=0 then FEffect.numPrograms := ShellPlugins[i].NumPrograms;
        if ShellPlugins[i].NumParams>=0 then FEffect.numParams := ShellPlugins[i].NumParams;
        fPlugCategory := ShellPlugins[i].PlugCategory;
        if Assigned(ShellPlugins[i].OnInstanciate) then
         begin
          sUID := '';
          for j := 3 downto 0 do sUID := sUID + char(rUID shr (j * 8));
          ShellPlugins[i].OnInstanciate(Self,sUID);
         end;
        IOChanged;
       end;
    end;
  end
 else
  if (PlugCategory=vpcShell)
   then PlugCategory := vpcUnknown;
end;

procedure TCustomVSTModule.HostCallDispatchEffect(opcode: TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single);
begin
 if Assigned(FOnDispatcher) then FOnDispatcher(Self,opcode);

 {$IFDEF Debug}
 if not (opcode in [effIdle, effEditIdle]) then
  FLog.Add(TimeToStr(Now - fTmStmp)+' Opcode: '+opcode2String(opcode)+' Value: '+IntToStr(Value));
 FLog.SaveToFile('Debug.log');
 {$ENDIF}
end;

procedure TCustomVSTModule.ReadOnlyString(s: string);
begin end;



function TCustomVSTModule.HostCallOpen(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  {$IFDEF Debug} FLog.Add('Host: ' + HostProduct); {$ENDIF}
  if Assigned(FOnOpen) then FOnOpen(Self);  
  Result := 0;
end;

function TCustomVSTModule.HostCallClose(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnClose) then FOnClose(Self);
  Result := 0;
end;

function TCustomVSTModule.HostCallGetVu(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var s: single;
begin
  if Assigned(FOnGetVUEvent) then
  begin
    s := 0;
    FOnGetVUEvent(s);
    Result := round(s * 32767);
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallSetSampleRate(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  setSampleRate(opt);
  Result := 1;
end;

function TCustomVSTModule.HostCallSetBlockSize(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  setBlockSize(Value);
  Result := 0;
end;

function TCustomVSTModule.HostCallMainsChanged(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if (Value = 0) then
  begin
    if Assigned(FOnSuspend) then FOnSuspend(Self);
  end else begin
    if Assigned(FOnResume) then FOnResume(Self);
    wantEvents(1);;
  end;

  Result := 0;
end;

function TCustomVSTModule.HostCallEditGetRect(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  PPERect(ptr)^ := @FEditorRect;
  FEditorRect.top := 0;
  FEditorRect.left := 0;

  if Assigned(FEditorForm) then
  begin
    FEditorRect.bottom := FEditorForm.ClientHeight;
    FEditorRect.right := FEditorForm.ClientWidth;
    Result := 1;
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallEditOpen(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
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
begin
  if (effFlagsHasEditor in FEffect.EffectFlags) then
  begin
    if Assigned(FOnEditClose) then FOnEditClose(Self);
    if Assigned(FEditorForm) then FreeAndNil(FEditorForm);
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
  if Assigned(FOnEditTop) then FOnEditTop(Self);
  Result := 0;
end;

function TCustomVSTModule.HostCallEditSleep(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnEditSleep) then FOnEditSleep(Self);
  Result := 0;
end;



function TCustomVSTModule.HostCallProcessEvents(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := 1;
end;



function TCustomVSTModule.HostCallConnectInput(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnInConnected) then FOnInConnected(Self,Index,(Value <> 0));
  Result := 1;
end;

function TCustomVSTModule.HostCallConnectOutput(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnOutConnected) then FOnOutConnected(Self,Index,(Value <> 0));
  Result := 1;
end;

function TCustomVSTModule.HostCallGetInputProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var str1  : string[63];
    str2  : string[7];
    sat   : TVstSpeakerArrangementType;
    cpf   : TChannelPropertyFlags;
begin
  Result := 0;
  if (Index < FEffect.numInputs) then
  begin
    str1 := 'Input #' + IntToStr(Index + 1);
    str2 := 'In' + IntToStr(Index + 1);
    sat := satStereo;
    cpf := [cpfIsActive,cpfIsStereo];

    if Assigned(FOnGetInputProperties) then FOnGetInputProperties(Self,str1,str2,sat,cpf);

    StrPCopy(PVstPinProperties(ptr)^.Caption, str1); // set name of input channel:
    StrPCopy(PVstPinProperties(ptr)^.ShortLabel, str2); // set name of input channel:
    if cpfIsActive in cpf then PVstPinProperties(ptr)^.Flags := [vppIsActive] else PVstPinProperties(ptr)^.Flags := [];
    if cpfIsStereo in cpf then PVstPinProperties(ptr)^.Flags := PVstPinProperties(ptr)^.Flags + [vppIsStereo];
    if cpfUseSpeaker in cpf then PVstPinProperties(ptr)^.Flags := PVstPinProperties(ptr)^.Flags + [vppUseSpeaker];

  Result := 1;
 end;
end;

function TCustomVSTModule.HostCallGetOutputProperties(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var str1  : string[63];
    str2  : string[7];
    sat   : TVSTSpeakerArrangementType;
    cpf   : TChannelPropertyFlags;
begin
  Result := 0;
  if (Index < FEffect.numOutputs) then
  begin
    str1 := 'Output #' + IntToStr(Index + 1);
    str2 := 'Out' + IntToStr(Index + 1);
    sat := satStereo;
    cpf := [cpfIsActive,cpfIsStereo];

    if Assigned(FOnGetOutputProperties) then FOnGetOutputProperties(Self,str1,str2,sat,cpf);

    StrPCopy(PVstPinProperties(ptr)^.Caption, str1); // set name of input channel:
    StrPCopy(PVstPinProperties(ptr)^.shortLabel, str2); // set name of input channel:
    if cpfIsActive in cpf then PVstPinProperties(ptr)^.Flags := [vppIsActive] else PVstPinProperties(ptr)^.Flags := [];
    if cpfIsStereo in cpf then PVstPinProperties(ptr)^.Flags := PVstPinProperties(ptr)^.Flags + [vppIsStereo];
    if cpfUseSpeaker in cpf then PVstPinProperties(ptr)^.Flags := PVstPinProperties(ptr)^.Flags + [vppUseSpeaker];
    Result := 1;
  end;
end;

function TCustomVSTModule.HostCallGetPlugCategory(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := Integer(FPlugCategory);
end;

function TCustomVSTModule.HostCallOfflineNotify(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnOfflineNotify) then
  begin
    FOnOfflineNotify(Self, PVstAudioFile(ptr)^, Value, (Index <> 0));
    Result := 1;
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallOfflinePrepare(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnOfflinePrepare) then
  begin
    FOnOfflinePrepare(Self, PVstOfflineTask(vcdOffline)^, Value);
    Result := 1;
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallOfflineRun(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnOfflineRun) then
  begin
    FOnOfflineRun(Self, PVstOfflineTask(vcdOffline)^, Value);
    Result := 1;
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallProcessVarIo(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnProcessVarIO) then
  begin
    FOnProcessVarIO(Self, PVstVariableIo(ptr)^);
    Result := 1;
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallSetBlockSizeAndSampleRate(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  SetBlockSizeAndSampleRate(Value, opt);
  Result := 1;
end;

function TCustomVSTModule.HostCallSetBypass(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  {$IFDEF Debug} if Value <> 0 then FLog.Add('SoftBypass: On') else FLog.Add('SoftBypass: Off'); FLog.SaveToFile('Debug.log'); {$ENDIF}
  if Assigned(FOnSoftBypass) then
  begin
    FOnSoftBypass(Self, Value <> 0);
    Result := 1;
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallGetEffectName(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  StrPCopy(ptr, FEffectName);
  Result := 1;
end;

function TCustomVSTModule.HostCallGetVendorString(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  StrPCopy(ptr, fVendorName);
  Result := 1;
end;

function TCustomVSTModule.HostCallGetProductString(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if fProductName <> '' then
    StrPCopy(ptr, fProductName)
  else
    StrPCopy(ptr, FEffectName);

  Result := 1;
end;

function TCustomVSTModule.HostCallGetVendorVersion(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := FEffect.Version;
end;

function TCustomVSTModule.HostCallVendorSpecific(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnVendorSpecific) then
    Result := FOnVendorSpecific(Self, Index, Value, ptr, opt)
  else
    Result := 0;
end;

function TCustomVSTModule.HostCallCanDo(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := 0;
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
  if StrComp(ptr, 'bypass')                = 0 then Result := 2 * Integer(vcdBypass                in fCanDos)-1;
  if Assigned(FOnCanDo) then FOnCanDo(Self,pchar(ptr));
end;

function TCustomVSTModule.HostCallGetTailSize(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := fTailSize;
end;



function TCustomVSTModule.HostCallKeysRequired(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := Integer(not fKeysRequired); // reversed to keep v1 compatibility
end;

function TCustomVSTModule.HostCallGetVstVersion(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := 2400;
end;

function TCustomVSTModule.HostCallEditKeyDown(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var keyCode : TVstKeyCode;
    a,b: integer;
    Hndl: THandle;
begin
  Result := 0;
  if fKeysRequired then
  try
    keyCode.character := Index;
    keyCode.virt := Value;
    keyCode.modifier := Round(opt);
    if Assigned(EditorForm) then
    begin
      a := KeyCodeToInteger(keyCode);
      if Assigned(EditorForm.ActiveControl) then
        Hndl := EditorForm.ActiveControl.Handle
      else
        Hndl := EditorForm.Handle;

      {$IFNDEF FPC}
      if keyCode.virt=0 then b := 0 else b := KF_EXTENDED;
      if (keyCode.modifier and MODIFIER_ALTERNATE)<>0 then
        SendMessage(Hndl, WM_KEYDOWN, a,b)
      else
        SendMessage(Hndl, WM_SYSKEYDOWN, a,KF_ALTDOWN);
      SendMessage(Hndl,WM_CHAR, a, b);
      {$ELSE}
      if keyCode.virt=0 then b := 0 else b := $100;
      if (keyCode.modifier and MODIFIER_ALTERNATE)<>0 then
        SendMessage(Hndl, LM_KEYDOWN, a,b)
      else
        SendMessage(Hndl, LM_SYSKEYDOWN, a, $2000);
      SendMessage(Hndl,LM_CHAR, a, b);
      {$ENDIF}

      if Assigned(FOnKeyDown) then FOnKeyDown(Self, keyCode);
      if Assigned(FOnCheckKey) then
        if FOnCheckKey(Self, Char(a)) then
          Result := 1
        else
          Result := -1
      else
        Result := -1;
    end;
  except
    Result := -1;
  end else
    Result := -1;
end;

function TCustomVSTModule.HostCallEditKeyUp(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var keyCode : TVstKeyCode;
    a,b: integer;
    Hndl: THandle;
begin
  Result := 0;
  if fKeysRequired then
  try
    keyCode.character := Index;
    keyCode.virt := Value;
    keyCode.modifier := Round(opt);

    if Assigned(EditorForm) then
    begin
      a := KeyCodeToInteger(keyCode);
      if Assigned(EditorForm.ActiveControl) then
        Hndl := EditorForm.ActiveControl.Handle
      else
        Hndl := EditorForm.Handle;
      {$IFNDEF FPC}
      if keyCode.virt=0 then b := 0 else b := KF_EXTENDED;
      if (keyCode.modifier and MODIFIER_ALTERNATE)<>0 then
        SendMessage(Hndl, WM_KEYUP, a, b)
      else
        SendMessage(Hndl, WM_SYSKEYUP, a, KF_ALTDOWN);
      {$ELSE}
      if keyCode.virt=0 then b := 0 else b := $100;
      if (keyCode.modifier and MODIFIER_ALTERNATE)<>0 then
        SendMessage(Hndl, LM_KEYUP, a,b)
      else
        SendMessage(Hndl, LM_SYSKEYUP, a, $2000);

      SendMessage(Hndl,LM_CHAR, a, b);
      {$ENDIF}

      if Assigned(FOnKeyUp) then FOnKeyUp(Self, keyCode);
      if Assigned(FOnCheckKey) then
        if FOnCheckKey(Self, Char(a)) then
          Result := 1
        else
          Result := -1
      else
        Result := -1;
    end;
  except
    Result := -1;
  end else
    Result := -1;
end;

function TCustomVSTModule.HostCallSetEditKnobMode(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if Assigned(FOnSetKnobMode) then
  begin
    FOnSetKnobMode(Self, Value);
    Result := 1;
  end else
    Result := 0;
end;

function TCustomVSTModule.HostCallGetSpeakerArrangement(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  PVstSpeakerArrangement(Value) := nil;
  PVstSpeakerArrangement(ptr) := nil;
  Result := 0;
end;

function TCustomVSTModule.HostCallShellGetNextPlugin(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  if FCurrentVstShellPlugin<FVstShellPlugins.Count then
  begin
    StrPCopy(pchar(ptr),FVstShellPlugins[FCurrentVstShellPlugin].DisplayName);
    Result := FVstShellPlugins[FCurrentVstShellPlugin].UID;
    Inc(FCurrentVstShellPlugin);
  end else begin
    Result := 0;
    FCurrentVstShellPlugin := 0;
  end;
end;

function TCustomVSTModule.HostCallStartProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := 1;
  if Assigned(FOnStartProcess) then
    FOnStartProcess(Self)
  else
    Result := 0;
end;

function TCustomVSTModule.HostCallStopProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := 1;
  if Assigned(FOnStopProcess) then
    FOnStopProcess(Self)
  else
    Result := 0;
end;

function TCustomVSTModule.HostCallSetTotalSampleToProcess(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := Value;
end;

function TCustomVSTModule.HostCallSetPanLaw(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := 1;
  if Assigned(FOnSetPanLaw) then
    FOnSetPanLaw(Self, Value, opt)
  else
    Result := 0;
end;

function TCustomVSTModule.HostCallSetProcessPrecision(Index, Value: Integer; ptr: pointer; opt: Single): Integer;
begin
  Result := Integer(fProcessPrecisition); //< [value]: @see VstProcessPrecision  @see AudioEffectX::setProcessPrecision
end;

function TCustomVSTModule.GetUniqueID:string;
var i : Integer;
begin
 Result := '';
 for i := 3 downto 0
  do Result := Result + char(FEffect.uniqueID shr (i * 8));
end;

procedure TCustomVSTModule.SetUniqueID(fID:string);
begin
 FEffect.uniqueID := FourCharToLong(fID[1], fID[2], fID[3], fID[4])
end;

procedure TCustomVSTModule.SetSampleRate(newValue: Single);
begin
 if fSampleRate<>newValue then
  begin
   fSampleRate := newValue;
   SampleRateChanged;
  end;
end;

procedure TCustomVSTModule.SetBlockSize(newValue: Integer);
begin
 if fBlockSize<>newValue then
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
 {$IFDEF Debug} FLog.Add('Before ReadState'); {$ENDIF}
 {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}
 inherited;
 {$IFDEF Debug} FLog.Add('After ReadState'); {$ENDIF}
 {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}

 if Assigned(FOnInitialize) then FOnInitialize(Self);
 {$IFDEF Debug} FLog.Add('End ReadState'); {$ENDIF}
 {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}
end;
{$ENDIF}

function TCustomVSTModule.UpdateSampleRate: Double;
begin
  {$IFDEF Debug} FLog.Add('Update Samplerate'); FLog.SaveToFile('Debug.log'); {$ENDIF}

  Result := inherited UpdateSampleRate;
  if (Result>0) and (Result<>FSampleRate) then
  begin
    fSampleRate := Result;
    SampleRateChanged;
  end;
end;

function TCustomVSTModule.UpdateBlockSize: Integer;
begin
  Result := inherited UpdateBlockSize;
  if (Result>0) and (Result<>FBlockSize) then
  begin
    FBlockSize := Result;
    BlockSizeChanged;
  end;
end;

procedure TCustomVSTModule.SetBlockSizeAndSampleRate(aBlockSize: Integer; aSampleRate: Single);
begin
 {$IFDEF Debug} FLog.Add('Set BlockSize/Samplerate: Blocksize ' + IntToStr(aBlockSize) + ' Samplerate ' + FloatToStr(aSampleRate)); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if fSampleRate<>aSampleRate then
  begin
   fSampleRate := aSampleRate;
   SampleRateChanged;
  end;
 if fBlockSize<>aBlockSize then
  begin
   fBlockSize := aBlockSize;
   BlockSizeChanged;
  end;
end;

function TCustomVSTModule.AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean;
var size : Integer;
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
  if Assigned(fSampleRateChangeEvent) then fSampleRateChangeEvent(Self,fSampleRate);
end;

procedure TCustomVSTModule.SampleRateChanged;
begin
  if Assigned(fSampleRateChangeEvent) then fSampleRateChangeEvent(Self,fSampleRate);
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
end;

function TCustomVSTModule.GetHostProduct: string;
var Text : pchar;
begin
 if (FHostProduct = '') or (FHostProduct = 'Unknown') then
  begin
   Getmem(Text, 64);
   try
    if GetHostProductString(Text)
     then Result := shortstring(Text)
     else Result := 'Unknown';
    if Result = 'Unknown' then
    try
     Result := shortstring(Text);
    except
     Result := 'Unknown';
    end;  
   finally
    FreeMem(Text);
    FHostProduct := Result;
   end
  end
 else Result := FHostProduct;
end;

function TCustomVSTModule.GetHostVendor: string;
var Text : pchar;
begin
 Getmem(Text, 64);
 try
  if GetHostVendorString(Text)
   then Result := 'Unknown'
   else Result := shortstring(Text);
 finally
  FreeMem(Text);
 end;
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
