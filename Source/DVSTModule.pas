unit DVSTModule;

interface

{$I ASIOVST.INC}
{$DEFINE UseDelphi}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows,
  Messages, {$ENDIF} SysUtils, Forms, Classes, DDSPBase, DVSTEffect;

{$DEFINE Debug}

{$IFDEF FPC} {$DEFINE Debug} {$ENDIF}
{$IFNDEF FPC}{$IFDEF DELPHI6_UP} {$DEFINE CPU_Detection} {$ENDIF} {$ENDIF}

type
  TChannelPropertyFlags = set of (cpfIsActive, cpfIsStereo, cpfUseSpeaker);
  TParameterChangeEvent = procedure(Sender: TObject; const Index: Integer; var Value: Single) of object;
  TBlockSizeChangeEvent = procedure(Sender: TObject; const BlockSize: Integer) of object;
  TSampleRateChangeEvent = procedure(Sender: TObject; const SampleRate: Single) of object;
  TProcessAudioEvent = procedure(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer) of object;
  TProcessDoubleEvent = procedure(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer) of object;
  TOnDispatcherEvent = procedure (Sender: TObject; opCode: TDispatcherOpcode) of object;
  TGetVUEvent = procedure(var VU:Single) of object;
  TGetEditorEvent = procedure(Sender: TObject; var GUI: TForm) of object;
  TChunkEvent = procedure(Sender: TObject; const Index : Integer; const isPreset : Boolean) of object;
  TGetChunkParameterEvent = function(Sender: TObject; const Index: Integer): Single of object;
  TProcessMidiEvent = procedure(Sender: TObject; MidiEvent: TVstMidiEvent) of object;
  TSoftBypassEvent = procedure(Sender: TObject; onOff: Boolean) of object;
  TInOutConnectedEvent = procedure(Sender: TObject; Index: Integer; State: Boolean) of object;
  TSetKnobModeEvent = procedure (Sender: TObject; val: Integer) of object;
  TVSTKeyEvent = procedure(Sender: TObject; var keyCode : TVstKeyCode) of object;
  TOfflineNotifyEvent = procedure (Sender: TObject; AudioFile: TVstAudioFile; numAudioFiles: Integer; start: Boolean) of object;
  TOfflinePrepareEvent = procedure (Sender: TObject; OfflineTask: TVstOfflineTask; count: Integer) of object;
  TOfflineRunEvent = procedure (Sender: TObject; OfflineTask: TVstOfflineTask; count: Integer) of object;
  TProcessVarIOEvent = procedure (Sender: TObject; varIo: TVstVariableIo) of object;
  TOnSetPanLawEvent = procedure (Sender: TObject; var vType: Integer; var val: single) of object;
  TOnBeginLoadBankEvent = procedure (Sender: TObject; PatchChunkInfo: TVstPatchChunkInfo) of object;
  TOnBeginLoadProgramEvent = procedure (Sender: TObject; PatchChunkInfo: TVstPatchChunkInfo) of object;
  TOnVendorSpecificEvent = function(Sender: TObject; lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer of object;
  TOnCanDoEvent = function(Sender: TObject; CanDoText: String): Integer of object;
  TOnGetChannelPropertiesEvent = function(Sender: TObject; var vLabel: ShortString; var shortLabel: ShortString; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags:TChannelPropertyFlags): Integer of object;
  TOnCheckKey = function(Sender: TObject; Key: Char): Boolean of object;
  TUIDInstantiateEvent = procedure(Sender: TObject; UID: string) of object;

  TVstCanDo = (vcdSendVstEvents, vcdSendVstMidiEvent, vcdSendVstTimeInfo,
   vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdReceiveVstTimeInfo,
   vcdOffline, vcdPlugAsChannelInsert, vcdPlugAsSend, vcdMixDryWet,
   vcdNoRealTime, vcdMultipass, vcdMetapass, vcd1in1out, vcd1in2out, vcd2in1out,
   vcd2in2out, vcd2in4out, vcd4in2out, vcd4in4out, vcd4in8out, vcd8in4out,
   vcd8in8out, vcdMidiProgramNames, vcdLiveWithoutToolbar, vcdConformsToWindowRules,
   vcdBypass);
  TVstCanDos = set of TVstCanDo;

  TProcessingMode = (pmNormal, pmBlockSave, pmCopy, pmMute);

  TCurveType = (ctLinear, ctLogarithmic, ctExponential, ctFrequencyScale);

  {$IFDEF CPU_Detection}
  TCPUVendor = (cvUnknown, cvAMD, cvCentaur, cvCyrix, cvIntel, cvTransmeta,
                cvNexGen, cvRise, cvUMC, cvNSC, cvSiS);

  TCPUInstruction = (isFPU, isTSC, isCX8, isSEP, isCMOV, isMMX, isFXSR, isSSE, isSSE2, isSSE3,
                     isMONITOR, isCX16, isX64, isExMMX, isEx3DNow, is3DNow);

  TCPUInstructions = set of TCPUInstruction;

  TCPU = class(TPersistent)
  private
    FOwner            : TPersistent;
    FVendor           : TCPUVendor;
    FSignature        : Cardinal;
    FEffFamily        : Byte;
    FEffModel         : Byte;
    FCodeL1CacheSize,
    FDataL1CacheSize,
    FL2CacheSize,
    FL3CacheSize      : Word;
    FInstructions     : TCPUInstructions;
    procedure GetCPUInfo;
    procedure GetCPUVendor;
    procedure GetCPUFeatures;
    procedure GetCPUExtendedFeatures;
    procedure GetProcessorCacheInfo;
    procedure GetExtendedProcessorCacheInfo;
    procedure VerifyOSSupportForXMMRegisters;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner : TPersistent);
    destructor Destroy; override;
  published
    property Vendor: TCPUVendor read fVendor;
    property Signature: Cardinal read fSignature;
    property EffFamily: Byte read fEffFamily;
    property EffModel: Byte read fEffModel;
    property CodeL1CacheSize: Word read fCodeL1CacheSize;
    property DataL1CacheSize: Word read fDataL1CacheSize;
    property L2CacheSize: Word read fL2CacheSize;
    property L3CacheSize: Word read fL3CacheSize;
    property Instructions: TCPUInstructions read fInstructions;
  end;

const VendorStr: array[Low(TCPUVendor)..High(TCPUVendor)] of ShortString =
       ('Unknown', 'AMD', 'Centaur (VIA)', 'Cyrix', 'Intel', 'Transmeta',
        'NexGen', 'Rise', 'UMC', 'National Semiconductor', 'SiS');

      InstructionSupportStr: array[Low(TCPUInstruction)..High(TCPUInstruction)] of ShortString =
       ('FPU', 'TSC', 'CX8', 'SEP', 'CMOV', 'MMX', 'FXSR', 'SSE', 'SSE2',
        'SSE3', 'MONITOR', 'CX16', 'X64', 'MMX+', '3DNow!+', '3DNow!');
{$ENDIF}

type
  TCustomVstPrograms = class;
  TCustomVSTModule = class;
  TCustomVstParameterProperties = class;

  TCustomParameterLabelEvent = procedure(Sender: TObject; const Index: Integer; var PreDefined: string) of object;
  TCustomParameterDisplayEvent = procedure(Sender: TObject; const Index: Integer; var PreDefined: string) of object;

  { TCustomVstParameterProperty }

  TCustomVstParameterProperty = class(TCollectionItem)
  private
    FSmoothStates     : Array [0..1] of Single;
    FMin, fMax        : Single;
    FCurve            : TCurveType;
    FCurveFactor      : Single;
    FDisplayName      : string;
    FUnits            : string;
    FSmoothingFactor  : Single;
    FCanBeAutomated   : Boolean;
    FV2Properties     : Boolean;
    FStepFloat        : Single;
    FSmallStepFloat   : Single;
    FLargeStepFloat   : Single;
    FFlags            : TVstParameterPropertiesFlags;
    FMinInteger       : Integer;
    FMaxInteger       : Integer;
    FStepInteger      : Integer;
    FLargeStepInteger : Integer;
    FCC               : Integer;
    FShortLabel       : string[7];

    FVSTModule        : TCustomVSTModule;
    FOnSPC            : TParameterChangeEvent;
    FOnCPL            : TCustomParameterLabelEvent;
    FOnCPD            : TCustomParameterDisplayEvent;
    function Smooth(i: Single): Single;
    function GetShortLabel: string;
    procedure SetShortLabel(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
    procedure SetUnits(AUnits: string);
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
  published
    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;
    property CC: Integer read FCC write FCC default -1;
    property Curve: TCurveType read FCurve write FCurve;
    property DisplayName{$IFNDEF FPC}: string read FDisplayName write SetDisplayName{$ENDIF};
    property Units: string read FUnits write SetUnits;
    property CurveFactor: Single read FCurveFactor write FCurveFactor;
    property SmoothingFactor: Single read FSmoothingFactor write FSmoothingFactor;
    property CanBeAutomated: Boolean read FCanBeAutomated write FCanBeAutomated default true;
    property ReportVST2Properties: Boolean read FV2Properties write FV2Properties default false;
    property StepFloat: Single read FStepFloat write FStepFloat;
    property SmallStepFloat: Single read FSmallStepFloat write FSmallStepFloat;
    property LargeStepFloat: Single read FLargeStepFloat write FLargeStepFloat;
    property Flags: TVstParameterPropertiesFlags read FFlags write FFlags default [];
    property MinInteger: Integer read FMinInteger write FMinInteger default 0;
    property MaxInteger: Integer read FMaxInteger write FMaxInteger default 100;
    property StepInteger: Integer read FStepInteger write FStepInteger default 1;
    property LargeStepInteger: Integer read FLargeStepInteger write FLargeStepInteger default 10;
    property ShortLabel: string read GetShortLabel write SetShortLabel;
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
    property OnParameterChange: TParameterChangeEvent read FOnSPC write FOnSPC;
    property OnCustomParameterLabel: TCustomParameterLabelEvent read FOnCPL write FOnCPL;
    property OnCustomParameterDisplay: TCustomParameterDisplayEvent read FOnCPD write FOnCPD;
  end;

  TCustomVstParameterProperties = class(TOwnedCollection)
  private
    FVSTModule: TCustomVSTModule;
  protected
    function GetItem(Index: Integer): TCustomVstParameterProperty; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomVstParameterProperty); virtual;
    property Items[Index: Integer]: TCustomVstParameterProperty read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure WriteVSTXML; overload;
    procedure WriteVSTXML(FileName : TFileName); overload;
    function Add: TCustomVstParameterProperty;
    function Insert(Index: Integer): TCustomVstParameterProperty;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
  end;

  TCustomVstProgram = class(TCollectionItem)
  private
    FDisplayName      : string;
    FVSTModule        : TCustomVSTModule;
    FOnInitialize     : TNotifyEvent;
    FOnStoreChunk     : TChunkEvent;
    FOnLoadChunk      : TChunkEvent;

    procedure SetParameter(AIndex: Integer; s: Single);
    function GetParameter(AIndex: Integer): Single;
  protected
    FParameter        : array of Single;
    FChunkData        : TMemoryStream;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
    property Parameter[AIndex: Integer]: Single read GetParameter write SetParameter;
    property Chunk: TMemoryStream read fChunkData write fChunkData;
  published
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnLoadChunk: TChunkEvent read FOnLoadChunk write FOnLoadChunk;
    property OnStoreChunk: TChunkEvent read FOnStoreChunk write FOnStoreChunk;
  end;

  TCustomVstPrograms = class(TOwnedCollection)
  private
    FVSTModule: TCustomVSTModule;
  protected
    function GetItem(Index: Integer): TCustomVstProgram;
    procedure SetItem(Index: Integer; const Value: TCustomVstProgram);
    property Items[Index: Integer]: TCustomVstProgram read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCustomVstProgram;
    function Insert(Index: Integer): TCustomVstProgram;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
  end;

  TCustomVstShellPlugin = class(TCollectionItem)
  private
    FDisplayName      : string;
    FNumInputs        : Integer;
    FNumOutputs       : Integer;
    FNumParams        : Integer;
    FNumPrograms      : Integer;
    FPlugCategory     : TVstPluginCategory;
    FVSTModule        : TCustomVSTModule;
    FOnInstanciate    : TUIDInstantiateEvent;
    procedure SetUniqueID(fID: String);
    function GetUniqueID: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
    UID  : Integer;
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
  published
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property numInputs: Integer read FNumInputs write FNumInputs default -1;
    property numOutputs: Integer read FNumOutputs write FNumOutputs default -1;
    property numParams: Integer read FNumParams write FNumParams default -1;
    property numPrograms: Integer read FNumPrograms write FNumPrograms default -1;
    property PlugCategory: TVstPluginCategory read FPlugCategory write FPlugCategory;
    property UniqueID: string read GetUniqueID write SetUniqueID;
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
    property OnInstanciate: TUIDInstantiateEvent read FOnInstanciate write FOnInstanciate;
  end;

  TCustomVstShellPlugins = class(TOwnedCollection)
  private
    FVSTModule: TCustomVSTModule;
    function GetItem(Index: Integer): TCustomVstShellPlugin;
    procedure SetItem(Index: Integer; const Value: TCustomVstShellPlugin);
  protected
    property Items[Index: Integer]: TCustomVstShellPlugin read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCustomVstShellPlugin;
    function Insert(Index: Integer): TCustomVstShellPlugin;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
  end;

  { TCustomVSTModule }

  {$IFDEF UseDelphi}
  TCustomVSTModule = class(TDataModule)
  {$ELSE}
  TCustomVSTModule = class(TComponent)
  {$ENDIF}
  private
    FParameterUpdate        : Boolean;
    FAbout                  : string;
    FVersion                : string;
    {$IFDEF CPU_Detection}
    FCPU                    : TCPU;
    {$ENDIF}

    FEditorRect             : ERect;
    FEditorNeedUpdate       : Boolean;

    FCurProgram             : Integer;
    FVstPrograms            : TCustomVstPrograms;
    FParameter              : array of Single;
    FChunkData              : TMemoryStream;
    FParameterProperties    : TCustomVstParameterProperties;

    FOnEditClose            : TNotifyEvent;
    FOnEditIdle             : TNotifyEvent;
    FOnEditTop              : TNotifyEvent;
    FOnEditSleep            : TNotifyEvent;
    FOnProcess              : TProcessAudioEvent;
    FOnProcessEx            : TProcessAudioEvent;
    FOnProcessReplacing     : TProcessAudioEvent;
    FOnProcessReplacingEx   : TProcessAudioEvent;
    FOnProcessDoubles       : TProcessDoubleEvent;
    FOnProcessDoublesEx     : TProcessDoubleEvent;
    FOnInitialize           : TNotifyEvent;
    FOnResume               : TNotifyEvent;
    FOnSuspend              : TNotifyEvent;
    FOnBeforeProgramChange  : TNotifyEvent;
    FOnAfterProgramChange   : TNotifyEvent;
    FOnParameterSizeFailed  : TNotifyEvent;
    FOnGetVUEvent           : TGetVUEvent;
    FOnParameterChangeEvent : TParameterChangeEvent;
    FBlockSizeChangeEvent   : TBlockSizeChangeEvent;
    FSampleRateChangeEvent  : TSampleRateChangeEvent;
    FOnDispatcher           : TOnDispatcherEvent;
    FProcessPrecisition     : TProcessPrecision;
    FProcessingMode         : TProcessingMode;
    FBlockInBuffer          : TArrayOfSingleDynArray;
    FBlockOutBuffer         : TArrayOfSingleDynArray;
    FBlockPosition          : Integer;
    FBlockModeSize          : Integer;
    FBlockModeOverlap       : Integer;
    FInitialDelay           : Integer;
    FTempo                  : Single;
    FTailSize               : Integer;
    FCanDos                 : TVstCanDos;
    FPlugCategory           : TVstPluginCategory;
    FMidiEvent              : TVstEvents;
    FKeysRequired           : Boolean;

    FOnGetChunkParamEvent   : TGetChunkParameterEvent;
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
    FNumCategories          : Integer;
    FProcessMidi            : TProcessMidiEvent;
    FOnStartProcess         : TNotifyEvent;
    FOnStopProcess          : TNotifyEvent;
    FOnSetPanLaw            : TOnSetPanLawEvent;
    FOnBeginLoadBank        : TOnBeginLoadBankEvent;
    FOnBeginLoadProgram     : TOnBeginLoadProgramEvent;
    FOnBeginSetProgram      : TNotifyEvent;
    FOnEndSetProgram        : TNotifyEvent;
    FOnVendorSpecific       : TOnVendorSpecificEvent;
    FOnCanDo                : TOnCanDoEvent;
    FOnGetInputProperties   : TOnGetChannelPropertiesEvent;
    FOnGetOutputProperties  : TOnGetChannelPropertiesEvent;
    FOnCheckKey             : TOnCheckKey;
    FVstShellPlugins        : TCustomVstShellPlugins;
    FCurrentVstShellPlugin  : Integer;
    {$IFNDEF UseDelphi}
    fOnDestroy              : TNotifyEvent;
    fOnCreate               : TNotifyEvent;
    {$ENDIF}
    {$IFDEF Debug} FLog     : TStringList; {$ENDIF}
    {$IFDEF Debug} FTmStmp  : TDateTime; {$ENDIF}

    procedure SetVstPrograms(const Value: TCustomVstPrograms);
    procedure SetParameterProperties(const Value : TCustomVstParameterProperties);
    procedure setParameterAutomated(Index: Integer; Value: Single);
    procedure SetVstShellPlugins(const Value: TCustomVstShellPlugins);
    procedure SetKeysRequired(const Value: Boolean);
    function EditorOpen(ptr: pointer): Integer; virtual;
    procedure EditorClose; virtual;
    procedure EditorIdle; virtual;
    procedure ReadOnlyString(s: string); virtual;
    procedure FOnBlockSaveProcess(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure FOnBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure FOnProcessCopy(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure FOnProcessMute(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer); overload;
    procedure FOnBlockSaveProcess(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure FOnBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure FOnProcessCopy(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    procedure FOnProcessMute(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer); overload;
    function GetHostProduct: string;
    function GetHostVendor: string;
  protected
    FEffect                 : TVSTEffect;
    FOnOpen                 : TNotifyEvent;
    FOnClose                : TNotifyEvent;
    FOnEditOpen             : TGetEditorEvent;
    FEditorForm             : TForm;
    FAudioMaster            : TAudioMasterCallbackFunc;
    FSampleRate             : Single;
    FBlockSize              : Integer;
    FEffectName             : string;
    FVendorName             : string;
    FVersionMajor           : Integer;
    FVersionMinor           : Integer;
    FVersionRelease         : Integer;
    FProductName            : string;
    FIsHostAutomation       : Boolean;
    FHostProduct            : string;
    function Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
    function VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
    function GetEffect: PVSTEffect;
    function Dispatcher(opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    procedure SetAudioMaster(const AM :TAudioMasterCallbackFunc); virtual;
    procedure setNumInputs(Inputs: Integer); virtual;
    procedure setNumOutputs(Outputs: Integer); virtual;
    procedure SetSampleRate(newValue: Single); virtual;
    procedure SetBlockSize(newValue: Integer); virtual;
    function GetUniqueID:string; virtual;
    procedure SetUniqueID(fID:string); virtual;
    procedure SetProgram(aProgram: Integer); virtual;
    procedure SetCurrentProgramName(AName: string); virtual;
    function GetCurrentProgramName:string; virtual;
    procedure SetPluginFlags(newFlags : TEffFlags); virtual;
    function GetPluginFlags: TEffFlags; virtual;
    procedure Suspend; virtual;
    procedure Resume; virtual;
    procedure setInitialDelay(delay: Integer); virtual;
    procedure SetNumParams(newNum : Integer); virtual;
    procedure SetNumPrograms(newNum : Integer); virtual;
    procedure getParameterLabel(Index: Integer; Text: pchar); virtual;
    procedure getParameterDisplay(Index: Integer; Text: pchar); virtual;
    procedure getParameterName(Index: Integer; Text: pchar); virtual;
    {$IFDEF UseDelphi}
    procedure ReadState(Reader: TReader); override;
    {$ENDIF}
    procedure SetOnProcess(v : TProcessAudioEvent);
    procedure SetOnProcessReplacing(v : TProcessAudioEvent);
    procedure SetOnProcessDoubleReplacing(v : TProcessDoubleEvent);
    procedure SetProcessingMode(v : TProcessingMode);
    procedure PrepareBlockProcessing; virtual;
    procedure SetBlockForcedSize(v: Integer); virtual;
    procedure SetBlockOverlapSize(v: Integer); virtual;
    procedure SetParameter(const Index: Integer; Value: Single); virtual;
    function GetParameter(Index: Integer): Single; virtual;
    function ProcessEvents(events: PVstEvents): Integer; virtual;  // wants no more...else return 1! VstEvents and VstMidiEvents are declared in aeffectx.h
    procedure SetEffectName(const Value: string); virtual;
    procedure SetVersionMajor(Value: Integer);
    procedure SetVersionMinor(Value: Integer);
    procedure SetVersionRelease(Value: Integer);
    procedure UpdateVersion;

    // Host -> Plug
    function CanParameterBeAutomated(Index: Integer): Boolean; virtual;
    function String2parameter(Index: Integer; Text: pchar): Boolean; virtual; // note: implies setParameter. Text==0 is to be
    function GetChannelParameter(channel, Index: Integer): Single; virtual;
    function GetProgramNameIndexed(category, Index: Integer; Text: pchar): Boolean; virtual;
    function CopyProgram(destination: Integer): Boolean; virtual; // expected to check the capability (returns True).

    // Host -> Plug
    function GetInputProperties(Index: Integer; Properties: PVstPinProperties): Boolean; virtual;
    function GetOutputProperties(Index: Integer; Properties: PVstPinProperties): Boolean; virtual;

    // realtime
    procedure WantEvents(filter: Integer);  // filter is currently ignored, midi channel data only (default) virtual void wantEvents (long filter = 1); default is 1 for this!
    function GetTimeInfo(filter: Integer): PVstTimeInfo; virtual;  // returns const VstTimeInfo* (or 0 if not supported) filter should contain a mask indicating which fields are requested (see valid masks in aeffectx.h), as some items may require extensive conversions
    procedure SetTimeInfo(filter: Integer; ti: PVstTimeInfo); virtual;
    function TempoAt(pos: Integer): Integer; virtual; // returns tempo (in bpm * 10000) at sample frame location <pos>
    function SendVstEventsToHost(events: PVstEvents): Boolean;  // True: success

    // Plug -> Host
    function WillProcessReplacing: Integer; virtual; // returns 0: not implemented, 1: replacing, 2: accumulating
    function GetCurrentProcessLevel: Integer; virtual;  // returns: 0: not supported, 1: currently in user thread (gui) 2: currently in audio thread or irq (where Process is called) 3: currently in 'sequencer' thread or irq (midi, timer etc) 4: currently offline Processing and thus in user thread other: not defined, but probably pre-empting user thread.
    function GetAutomationState: Integer; virtual;  // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

    // Host -> Plug
    function ReportCurrentPosition: Integer; virtual;  // for external dsp, see wantAsyncOperation ()
    function ReportDestinationBuffer: PSingle; virtual;  // for external dsp (dma option)

    // offline
    // Plug -> Host
    function OfflineRead(offline: PVstOfflineTask; option: TVstOfflineOption; readSource: Boolean): Boolean; virtual;
    function OfflineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): Boolean; virtual;
    function OfflineStart(ptr: PVstAudioFile; numAudioFiles: Integer; numNewAudioFiles: Integer): Boolean; virtual;
    function OfflineGetCurrentPass: Integer; virtual;
    function OfflineGetCurrentMetaPass: Integer; virtual;

    function OfflineGetNumPasses: Integer; virtual;
    function OfflineGetNumMetaPasses: Integer; virtual;

    // other
    // Plug -> Host
    procedure SetOutputSampleRate(samplerate: Single); virtual;
    function GetInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function GetOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function GetHostVendorString(Text: pchar): Boolean; virtual;  // fills <Text> with a string identifying the vendor (max 64 char)
    function GetHostProductString(Text: pchar): Boolean; virtual; // fills <Text> with a string with product name (max 64 char)
    function GetHostVendorVersion: Integer; virtual;  // returns vendor-specific version
    function HostVendorSpecific(lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer; virtual;  // no definition
    function CanHostDo(Text: pchar): Integer; virtual;  // see 'hostCanDos' in audioeffectx.cpp returns 0: don't know (default), 1: yes, -1: no
    function GetHostLanguage: Integer; virtual;   // returns VstHostLanguage
    function OpenWindow(aWindow: PVstWindow): pointer; virtual;  // create new window
    function CloseWindow(aWindow: PVstWindow): Boolean; virtual; // close a newly created window
    function GetDirectory: pointer; virtual;  // get the plug's directory, FSSpec on mac, else char*

    // Host -> Plug
    function SetSpeakerArrangement(pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean; virtual;
    function GetSpeakerArrangement(var pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean; virtual;
    procedure SetBlockSizeAndSampleRate(aBlockSize: Integer; aSampleRate: Single); virtual;
    function SetBypass(onOff: Boolean): Boolean; virtual; // for 'soft-bypass; Process() still called
    function GetEffectName(AName: pchar): Boolean; virtual;  // name max 32 char
    function GetErrorText(Text: pchar): Boolean; virtual;  // max 256 char
    function GetVendorString(Text: pchar): Boolean; virtual;  // fill Text with a string identifying the vendor (max 64 char)
    function GetProductString(Text: pchar): Boolean; virtual; // fills Text with a string with product name (max 64 char)
    function GetVendorVersion: Integer; virtual;
    function CanDo(Text: pchar): Integer; virtual; // see 'plugCanDos' in audioeffectx.cpp. return Values: 0: don't know (default), 1: yes, -1: no
    function GetIcon: pointer; virtual;  // not yet defined
    function SetViewPosition(x, y: Integer): Boolean; virtual;
    function FxIdle: Integer; virtual;
    function GetParameterProperties(Index: Integer; p: PVstParameterProperties): Boolean; virtual;

    // midi program names, are always defined per channel, valid channels are 0 - 15
    // Host -> Plug
    function GetMidiProgramName(channel: Integer; midiProgramName: PMidiProgramName): Integer; virtual; {return 0;} // Struct will be filled with information for 'thisProgramIndex'. Returns number of used programIndexes. If 0 is returned, no MidiProgramNames supported.
    function GetCurrentMidiProgram(channel: Integer; currentProgram: PMidiProgramName): Integer; virtual; {return -1;} // returns the programIndex of the current program. -1 means not supported. Struct will be filled with information for the current program.
    function GetMidiProgramCategory(channel: Integer; category: PMidiProgramCategory): Integer; virtual; {return 0;} // Struct will be filled with information for 'thisCategoryIndex'. Returns number of used categoryIndexes. If 0 is returned, no MidiProgramCategories supported/ used.
    function HasMidiProgramsChanged(channel: Integer): Boolean; virtual; {return false;} // returns True if the MidiProgramNames, MidiKeyNames or MidiControllerNames had changed on this channel.
    function GetMidiKeyName(channel: Integer; keyName: PMidiKeyName): Boolean; virtual; {return false;} // Struct will be filled with information for 'thisProgramIndex' and 'thisKeyNumber' if keyName is "" the standard name of the key will be displayed. If false is returned, no MidiKeyNames defined for 'thisProgramIndex'.

    // Plug -> Host
    function BeginEdit(Index: Integer): Boolean; virtual;  // to be called before a setParameterAutomated with mouse move (one per Mouse Down)
    function EndEdit(Index: Integer): Boolean; virtual;    // to be called after a setParameterAutomated (on Mouse Up)

    function OpenFileSelector(ptr: PVstFileSelect): Boolean; virtual;
    function CloseFileSelector(ptr: PVstFileSelect): Boolean;
    function GetChunkFile(nativePath: pointer): Boolean;

    // Host -> Plug
    function SetTotalSampleToProcess(Value: Integer): Integer; virtual;   // Called in offline (non RealTime) Process before Process is called, indicates how many sample will be Processed
    function GetNextShellPlugin(const AName: pchar): Integer; virtual;           // This opcode is only called, if Plugin is of type kPlugCategShell. Should return the next plugin's uniqueID. name points to a char buffer of size 64, which is to be filled with the name of the plugin including the terminating zero.
    function StartProcess: Integer; virtual;                              // Called one time before the start of Process call
    function StopProcess: Integer; virtual;                               // Called after the stop of Process call
    function SetPanLaw(var vType: Integer; var val: single): Boolean; virtual;    // Set the Panning Law used by the Host
    function BeginLoadBank(ptr: PVstPatchChunkInfo): Integer; virtual;    // Called before a Bank is loaded. returns -1 if the Bank cannot be loaded, returns 1 if it can be loaded else 0 (for compatibility)
    function BeginLoadProgram(ptr: PVstPatchChunkInfo): Integer; virtual; // Called before a Program is loaded. (called before beginSetProgram) returns -1 if the Program cannot be loaded, returns 1 if it can be loaded else 0 (for compatibility)

    // Tools
    function AllocateArrangement(var Arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean; virtual;   // Allocate memory for a VstSpeakerArrangement containing the given number of channels
    function DeallocateArrangement(var Arrangement: PVstSpeakerArrangement): Boolean; virtual;                      // Delete/free memory for a speaker Arrangement
    function CopySpeaker(copyTo, copyFrom: PVstSpeakerProperties): Boolean; virtual;    // Feed the "to" speaker Properties with the same Values than "from"'s ones. It is assumed here that "to" exists yet, ie this function won't allocate memory for the speaker (this will prevent from having a difference between an Arrangement's number of channels and its actual speakers...)
    function MatchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): Boolean; virtual;    // "to" is deleted, then created and initialized with the same Values as "from" ones ("from" must exist).
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure EditorPostUpdate; virtual;
    procedure Process(Inputs, Outputs: PPSingle; SampleFrames: Integer); virtual;
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: Integer); virtual;
    procedure ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: Integer); virtual;
    function GetChunk(var data: pointer; isPreset: Boolean): Integer; virtual;   // returns byteSize
    function SetChunk(data: pointer; byteSize: Integer; isPreset: Boolean): Integer; virtual;

    // host communication
    function GetMasterVersion: Integer; virtual;
    function GetCurrentUniqueID: Integer; virtual;
    procedure MasterIdle; virtual;
    function IsInputConnected(input: Integer): Boolean; virtual;
    function IsOutputConnected(output: Integer): Boolean; virtual;

    procedure MIDI_Out(b1, b2, b3, b4: byte; offset: Integer = 0);
    procedure MIDI_SendSysEx(Data: array of byte; offset: Integer = 0);
    procedure MIDI_CC(ch, num, val: Integer; offset: Integer = 0);
    procedure MIDI_ChannelAftertouch(ch, val: Integer; offset: Integer = 0);
    procedure MIDI_NoteOff(ch, note, val: Integer; offset: Integer = 0);
    procedure MIDI_NoteOn(ch, note, val: Integer; offset: Integer = 0);
    procedure MIDI_PitchBend(ch, val: Integer; offset: Integer = 0);
    procedure MIDI_PitchBend2(ch, x1, x2: Integer; offset: Integer = 0);
    procedure MIDI_PolyAftertouch(ch, note, val: Integer; offset: Integer = 0);
    procedure MIDI_ProgramChange(ch, val: Integer; offset: Integer = 0);

    // Plug -> Host
    function GetNumAutomatableParameters: Integer; virtual;
    function GetParameterQuantization: Integer; virtual; // returns the Integer Value for +1.0 representation, or 1 if full single float precision is maintained in automation. parameter Index in <Value> (-1: all, any)

    // Plug -> Host
    function IOChanged: Boolean; virtual;   // tell host numInputs and/or numOutputs and/or numParameters has changed
    function NeedIdle: Boolean; virtual;    // plug needs idle calls (outside its editor window)
    function SizeWindow(width, height: Integer): Boolean; virtual;
    function UpdateSampleRate: Double; virtual;  // gets and returns sample rate from host (may issue setSampleRate() )
    function UpdateBlockSize: Integer; virtual;  // same for block size
    function GetInputLatency: Integer; virtual;
    function GetOutputLatency: Integer; virtual;
    function GetPreviousPlug(input: Integer): PVSTEffect; virtual;  // input can be -1 in which case the first found is returned
    function GetNextPlug(output: Integer): PVSTEffect; virtual;     // output can be -1 in which case the first found is returned
    function UpdateDisplay: Boolean; virtual; // something has changed, update 'multi-fx' display returns True if supported

    // Properties
    property EditorForm: TForm read FEditorForm;
    property EditorNeedUpdate: Boolean read FEditorNeedUpdate write FEditorNeedUpdate;

    property AudioMaster: TAudioMasterCallbackFunc read FAudioMaster write SetAudioMaster;
    property Flags: TEffFlags read GetPluginFlags write SetPluginFlags;
    property OnParameterChange: TParameterChangeEvent read FOnParameterChangeEvent write FOnParameterChangeEvent;
    property Effect: PVSTEffect read GetEffect;

    property SampleRate: Single read fSampleRate write SetSampleRate;
    property BlockSize: Integer read fBlockSize write SetBlockSize default 1024;
    property BlockModeSize: Integer read FBlockModeSize write SetBlockForcedSize default 1024;
    property BlockModeOverlap: Integer read FBlockModeOverlap write SetBlockOverlapSize default 0;
    property numInputs: Integer read FEffect.numInputs write SetNumInputs default 2;
    property numOutputs: Integer read FEffect.numOutputs write SetNumOutputs default 2;
    property numParams: Integer read FEffect.numParams write SetNumParams stored false;
    property numPrograms: Integer read FEffect.numPrograms write SetNumPrograms stored false;
    property CurrentProgram: Integer read FCurProgram write SetProgram;
    property CurrentProgramName: string read GetCurrentProgramName write SetCurrentProgramName;
    property InitialDelay: Integer read FEffect.initialDelay write SetInitialDelay default 0;
    property ProcessingMode: TProcessingMode read FProcessingMode write SetProcessingMode default pmNormal;
    property RealQualities: Integer read FEffect.realQualities write FEffect.realQualities default 0;
    property OffQualities: Integer read FEffect.offQualities write FEffect.offQualities default 0;
    property IORatio: Integer read FEffect.ioRatio write FEffect.ioRatio default 1;
    property About: string read FAbout write ReadOnlyString stored False;
    {$IFDEF CPU_Detection} property CPU: TCPU read fCPU; {$ENDIF}
    property Version: string read FVersion write FVersion;
    property UniqueID: string read GetUniqueID write setUniqueID;
    property Parameter[Index: Integer]: Single read getParameter write setParameterAutomated;
    property Chunk: TMemoryStream read fChunkData write fChunkData;
    property Programs: TCustomVstPrograms read FVstPrograms write SetVstPrograms;
    property ParameterProperties: TCustomVstParameterProperties read FParameterProperties write SetParameterProperties;
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

    property OnCreate {$IFNDEF UseDelphi} : TNotifyEvent read fOnCreate write fOnCreate {$ENDIF};
    property OnDestroy {$IFNDEF UseDelphi} : TNotifyEvent read fOnDestroy write fOnDestroy {$ENDIF};
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnResume: TNotifyEvent read FOnResume write FOnResume;
    property OnSuspend: TNotifyEvent read FOnSuspend write FOnSuspend;
    property OnEditOpen: TGetEditorEvent read FOnEditOpen write FOnEditOpen;
    property OnEditClose: TNotifyEvent read FOnEditClose write FOnEditClose;
    property OnEditIdle: TNotifyEvent read FOnEditIdle write FOnEditIdle;
    property OnEditTop: TNotifyEvent read FOnEditTop write FOnEditTop;
    property OnEditSleep: TNotifyEvent read FOnEditSleep write FOnEditSleep;
    property OnParameterSizeFailed: TNotifyEvent read FOnParameterSizeFailed write FOnParameterSizeFailed;
    property OnBlockSizeChange: TBlockSizeChangeEvent read fBlockSizeChangeEvent write fBlockSizeChangeEvent;
    property OnSampleRateChange: TSampleRateChangeEvent read fSampleRateChangeEvent write fSampleRateChangeEvent;
    property OnGetVU: TGetVUEvent read FOnGetVUEvent write FOnGetVUEvent;
    property OnProcess: TProcessAudioEvent read FOnProcess write SetOnProcess;
    property OnProcessReplacing: TProcessAudioEvent read FOnProcessReplacing write SetOnProcessReplacing;
    property OnProcessDoubleReplacing: TProcessDoubleEvent read FOnProcessDoubles write SetOnProcessDoubleReplacing;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnBeforeProgramChange: TNotifyEvent read FOnBeforeProgramChange write FOnBeforeProgramChange;
    property OnAfterProgramChange: TNotifyEvent read FOnAfterProgramChange write FOnAfterProgramChange;
    property OnDispatcher: TOnDispatcherEvent read FOnDispatcher write FOnDispatcher;
    property OnGetChunkParameter: TGetChunkParameterEvent read FOnGetChunkParamEvent write FOnGetChunkParamEvent;
    property OnSoftBypass: TSoftBypassEvent read FOnSoftBypass write FOnSoftBypass;
    property OnProcessMidi: TProcessMidiEvent read fProcessMidi write fProcessMidi;
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
    property OnBeginSetProgram: TNotifyEvent read FOnBeginSetProgram write FOnBeginSetProgram;
    property OnEndSetProgram: TNotifyEvent read FOnEndSetProgram write FOnEndSetProgram;
    property OnVendorSpecific: TOnVendorSpecificEvent read FOnVendorSpecific write FOnVendorSpecific;
    property OnCanDo: TOnCanDoEvent read FOnCanDo write FOnCanDo;
    property OnCheckKey: TOnCheckKey read FOnCheckKey write FOnCheckKey;
    property OnInputProperties: TOnGetChannelPropertiesEvent read FOnGetInputProperties write FOnGetInputProperties;
    property OnOutputProperties: TOnGetChannelPropertiesEvent read FOnGetOutputProperties write FOnGetOutputProperties;
    property OnBeginLoadBank: TOnBeginLoadBankEvent read FOnBeginLoadBank write FOnBeginLoadBank;
    property OnBeginLoadProgram: TOnBeginLoadProgramEvent read FOnBeginLoadProgram write FOnBeginLoadProgram;
    property HostProduct: string read GetHostProduct stored false;
    property HostVendor: string read GetHostVendor stored false;
    property HostVersion: Integer read getHostVendorVersion stored false;
  end;

  TVSTModule = class(TCustomVSTModule)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Flags;
    property About;
    {$IFDEF CPU_Detection} property CPU; {$ENDIF}
    property Version;
    property EffectName;
    property ProductName;
    property VendorName;
    property VersionMajor;
    property VersionMinor;
    property VersionRelease;
    property PlugCategory;
    property Tempo;
    property TailSize;
    property CanDos;
    property SampleRate;
    property BlockSize;
    property numInputs;
    property numOutputs;
    property numParams;
    property numPrograms;
    property numCategories;
    property CurrentProgram;
    property CurrentProgramName;
    property ProcessingMode;
    property BlockModeSize;
    property BlockModeOverlap;
    property InitialDelay;
    property KeysRequired;
    property RealQualities;
    property OffQualities;
    property IORatio;
    property UniqueID;
    property ShellPlugins;
    property Programs;
    property ParameterProperties;
    property OnGetChunkParameter;
    {$IFDEF UseDelphi}
    property OldCreateOrder;
    property OnCreate;
    property OnDestroy;
    {$ENDIF}
    property OnOpen;
    property OnClose;
    property OnEditOpen;
    property OnEditClose;
    property OnEditIdle;
    property OnEditTop;
    property OnEditSleep;
    property OnEditorKeyUp;
    property OnEditorKeyDown;
    property OnEditorKnobMode;
    property OnParameterChange;
    property OnResume;
    property OnSuspend;
    property OnParameterSizeFailed;
    property OnBlockSizeChange;
    property OnSampleRateChange;
    property OnGetVU;
    property OnProcess;
    property OnProcessReplacing;
    property OnProcessDoubleReplacing;
    property OnSoftBypass;
    property OnProcessMidi;
    property OnInConnected;
    property OnOutConnected;
    property OnStartProcess;
    property OnStopProcess;
    property OnOfflineNotify;
    property OnOfflinePrepare;
    property OnOfflineRun;
    property OnProcessVarIO;
    property OnSetPanLaw;
    property OnBeginLoadBank;
    property OnBeginLoadProgram;
    property OnBeginSetProgram;
    property OnEndSetProgram;
    property OnInitialize;
    property OnBeforeProgramChange;
    property OnAfterProgramChange;
    property OnDispatcher;
    property OnVendorSpecific;
    property OnCanDo;
    property OnInputProperties;
    property OnOutputProperties;
    property OnCheckKey;
  end;

  TVstProgram = TCustomVstProgram;
  TVstPrograms = TCustomVstPrograms;
  TVstParameterProperty = TCustomVstParameterProperty;
  TVstParameterProperties = TCustomVstParameterProperties;

function Opcode2String(opcode: TDispatcherOpcode): string;
function DispatchEffectClass(Effect: PVSTEffect; opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single): Integer; cdecl;
function GetParameterClass(Effect: PVSTEffect; Index: Integer): Single; cdecl;
procedure SetParameterClass(Effect: PVSTEffect; Index: Integer; Value: Single); cdecl;
procedure ProcessClass(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
procedure ProcessClassReplacing(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
procedure ProcessClassDoubleReplacing(Effect: PVSTEffect; Inputs, Outputs: PPDouble; SampleFrames: Integer); cdecl;

function KeyCodeToInteger(VKC:TVstKeyCode):Integer;

{$IFDEF FPC}
function InitResourceComponent(Instance: TComponent; RootAncestor: TClass):Boolean;
{$ENDIF}

implementation

uses Controls, Math, RtlConsts;

{$IFDEF CPU_Detection}
type
  TRegisters = record EAX, EBX, ECX, EDX: Cardinal;  end;
  TVendorStr = string[12];
  TCpuFeatures = ( cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
                   cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
                   cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
                   cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA_64, cfPBE,
                   cfSSE3, cf_c1, cf_c2, cfMON, cfDS_CPL, cf_c5, cf_c6, cfEIST,
                   cfTM2, cf_c9, cfCID, cf_c11, cf_c12, cfCX16, cfxTPR, cf_c15,
                   cf_c16, cf_c17, cf_c18, cf_c19, cf_c20, cf_c21, cf_c22, cf_c23,
                   cf_c24, cf_c25, cf_c26, cf_c27, cf_c28, cf_c29, cf_c30, cf_c31);
  TCpuFeatureSet = set of TCpuFeatures;

  TCpuExtendedFeatures = (cefFPU, cefVME, cefDE, cefPSE, cefTSC, cefMSR, cefPAE,
                          cefMCE, cefCX8, cefAPIC, cef_10, cefSEP, cefMTRR,
                          cefPGE, cefMCA, cefCMOV, cefPAT, cefPSE36, cef_18,
                          ceMPC, ceNX, cef_21, cefExMMX, cefMMX, cefFXSR, cef_25,
                          cef_26, cef_27, cef_28, cefLM, cefEx3DNow, cef3DNow);
  TCpuExtendedFeatureSet = set of TCpuExtendedFeatures;

const
  VendorIDString: array[Low(TCPUVendor)..High(TCPUVendor)] of TVendorStr =
  ('', 'AuthenticAMD', 'CentaurHauls', 'CyrixInstead', 'GenuineIntel', 'GenuineTMx86',
   'NexGenDriven', 'RiseRiseRise', 'UMC UMC UMC ', 'Geode by NSC', 'SiS SiS SiS');

  {CPU signatures}

  IntelLowestSEPSupportSignature = $633;
  K7DuronA0Signature = $630;
  C3Samuel2EffModel = 7;
  C3EzraEffModel = 8;
  PMBaniasEffModel = 9;
  PMDothanEffModel = $D;
  P3LowestEffModel = 7;
{$ENDIF}

const maxMidiEvents = 256;

function opcode2String(Opcode: TDispatcherOpcode): string;
begin
 case Opcode of
  effOpen                           : Result:='effOpen';
  effClose                          : Result:='effClose';
  effSetProgram                     : Result:='effSetProgram';
  effGetProgram                     : Result:='effGetProgram';
  effSetProgramName                 : Result:='effSetProgramName';
  effGetProgramName                 : Result:='effGetProgramName';
  effGetParamLabel                  : Result:='effGetParamLabel';
  effGetParamDisplay                : Result:='effGetParamDisplay';
  effGetParamName                   : Result:='effGetParamName';
  effGetVu                          : Result:='effGetVu';
  effSetSampleRate                  : Result:='effSetSampleRate';
  effSetBlockSize                   : Result:='effSetBlockSize';
  effMainsChanged                   : Result:='effMainsChanged';
  effEditGetRect                    : Result:='effEditGetRect';
  effEditOpen                       : Result:='effEditOpen';
  effEditClose                      : Result:='effEditClose';
  effEditDraw                       : Result:='effEditDraw';
  effEditMouse                      : Result:='effEditMouse';
  effEditKey                        : Result:='effEditKey';
  effEditIdle                       : Result:='effEditIdle';
  effEditTop                        : Result:='effEditTop';
  effEditSleep                      : Result:='effEditSleep';
  effIdentify                       : Result:='effIdentify';
  effGetChunk                       : Result:='effGetChunk';
  effSetChunk                       : Result:='effSetChunk';
  effProcessEvents                  : Result:='effProcessEvents';
  effCanBeAutomated                 : Result:='effCanBeAutomated';
  effString2Parameter               : Result:='effString2Parameter';
  effGetNumProgramCategories        : Result:='effGetNumProgramCategories';
  effGetProgramNameIndexed          : Result:='effGetProgramNameIndexed';
  effCopyProgram                    : Result:='effCopyProgram';
  effConnectInput                   : Result:='effConnectInput';
  effConnectOutput                  : Result:='effConnectOutput';
  effGetInputProperties             : Result:='effGetInputProperties';
  effGetOutputProperties            : Result:='effGetOutputProperties';
  effGetPlugCategory                : Result:='effGetPlugCategory';
  effGetCurrentPosition             : Result:='effGetCurrentPosition';
  effGetDestinationBuffer           : Result:='effGetDestinationBuffer';
  effOfflineNotify                  : Result:='effOfflineNotify';
  effOfflinePrepare                 : Result:='effOfflinePrepare';
  effOfflineRun                     : Result:='effOfflineRun';
  effProcessVarIo                   : Result:='effProcessVarIo';
  effSetSpeakerArrangement          : Result:='effSetSpeakerArrangement';
  effSetBlockSizeAndSampleRate      : Result:='effSetBlockSizeAndSampleRate';
  effSetBypass                      : Result:='effSetBypass';
  effGetEffectName                  : Result:='effGetEffectName';
  effGetErrorText                   : Result:='effGetErrorText';
  effGetVendorString                : Result:='effGetVendorString';
  effGetProductString               : Result:='effGetProductString';
  effGetVendorVersion               : Result:='effGetVendorVersion';
  effVendorSpecific                 : Result:='effVendorSpecific';
  effCanDo                          : Result:='effCanDo';
  effGetTailSize                    : Result:='effGetTailSize';
  effIdle                           : Result:='effIdle';
  effGetIcon                        : Result:='effGetIcon';
  effSetViewPosition                : Result:='effSetViewPosition';
  effGetParameterProperties         : Result:='effGetParameterProperties';
  effKeysRequired                   : Result:='effKeysRequired';
  effGetVstVersion                  : Result:='effGetVstVersion';
  effEditKeyDown                    : Result:='effEditKeyDown';
  effEditKeyUp                      : Result:='effEditKeyUp';
  effSetEditKnobMode                : Result:='effSetEditKnobMode';
  effGetMidiProgramName             : Result:='effGetMidiProgramName';
  effGetCurrentMidiProgram          : Result:='effGetCurrentMidiProgram';
  effGetMidiProgramCategory         : Result:='effGetMidiProgramCategory';
  effHasMidiProgramsChanged         : Result:='effHasMidiProgramsChanged';
  effGetMidiKeyName                 : Result:='effGetMidiKeyName';
  effBeginSetProgram                : Result:='effBeginSetProgram';
  effEndSetProgram                  : Result:='effEndSetProgram';
  effGetSpeakerArrangement          : Result:='effGetSpeakerArrangement';
  effShellGetNextPlugin             : Result:='effShellGetNextPlugin';
  effStartProcess                   : Result:='effStartProcess';
  effStopProcess                    : Result:='effStopProcess';
  effSetTotalSampleToProcess        : Result:='effSetTotalSampleToProcess';
  effSetPanLaw                      : Result:='effSetPanLaw';
  effBeginLoadBank                  : Result:='effBeginLoadBank';
  effBeginLoadProgram               : Result:='effBeginLoadProgram';
  effSetProcessPrecision            : Result:='effSetProcessPrecision';
  effGetNumMidiInputChannels        : Result:='effGetNumMidiInputChannels';
  effGetNumMidiOutputChannels       : Result:='effGetNumMidiOutputChannels';
  else Result := 'unkown opcode: ' +IntToStr(Integer(Opcode));
 end;
end;

{ TVSTModule }

{$IFNDEF FPC}
resourcestring
  SResNotFound = 'Resource %s not found';

constructor TVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF UseDelphi}
 inherited Create(AOwner);
 if (ClassType <> TVSTModule) and not (csDesigning in ComponentState) then
  begin
   if not InitInheritedComponent(Self, TCustomVSTModule) then
     raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
   try
    if Assigned(OnCreate) and OldCreateOrder then OnCreate(Self);
   except
    Forms.Application.HandleException(Self);
   end;
  end;
 {$ELSE}
 inherited Create(AOwner);
 if Assigned(OnCreate) then OnCreate(Self);
 {$ENDIF}
end;
{$ELSE}
constructor TVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF UseDelphi}
 inherited Create(AOwner);
 if (ClassType <> TVSTModule) and not (csDesigning in ComponentState) then
  begin
   if not InitInheritedComponent(Self, TCustomVSTModule)
    then raise EStreamError.CreateFmt(SErrNoStreaming, [ClassName]);
   if OldCreateOrder then DoCreate;
  end;
 {$ELSE}
 inherited Create(AOwner);
 {$ENDIF}
end;

function InitResourceComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
begin
//  Result:=InitLazResourceComponent(Instance,RootAncestor);
end;
{$ENDIF}

destructor TVSTModule.Destroy;
begin
 {$IFNDEF UseDelphi}
 if Assigned(fOnDestroy) then fOnDestroy(Self);
 {$ENDIF}
 inherited;
end;

{ TCustomVST1Module }

constructor TCustomVSTModule.Create{$IFDEF UseDelphi}(AOwner: TComponent){$ENDIF};
var i : Integer;
begin
 {$IFDEF UseDelphi} inherited CreateNew(AOwner); {$ENDIF}
 {$IFDEF Debug} FLog:=TStringList.Create; {$ENDIF}
 {$IFDEF Debug} fTmStmp:=Now; {$ENDIF}
 {$IFDEF Debug} FLog.Add('Create '+TimeToStr(fTmStmp)); {$ENDIF}
 {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}
 Randomize;
 FVersion := '0.0';
 FAbout := 'VST Plugin Wizard by Christian Budde & Tobybear';
 FProcessPrecisition := pp32;
 FKeysRequired := False;
 FTailSize := 0;
 FVersionMajor := 1;
 FVersionMinor := 0;
 FVersionRelease := 0;
 UpdateVersion;
 FEditorForm := nil;
 FCurProgram := -1;
 with FEffect do
  begin
   vObject := Self;
   magic := 'PtsV';
   dispatcher := @dispatchEffectClass;
   Process := @ProcessClass;
   ProcessReplacing := ProcessClassReplacing;
   ProcessDoubleReplacing := ProcessClassDoubleReplacing;
   setParameter := @setParameterClass;
   GetParameter := @getParameterClass;
   EffectFlags := [];
   reservedForHost := nil;
   resvd2 := 0;
   user := nil;
   uniqueID := FourCharToLong('N', 'o', 'E', 'f');
   ioRatio:= 1;
   numParams:=0;
   numPrograms:=0;
   numInputs:=2;
   numOutputs:=2;
  end;
 FParameterProperties := TCustomVstParameterProperties.Create(Self);
 FVstPrograms := TCustomVstPrograms.Create(Self);
 FParameterUpdate := False;
 FSampleRate := 44100;
 FBlockSize:=1024;
 FBlockModeSize:=1024;
 FBlockModeOverlap:=0;
 {$IFDEF CPU_Detection}
 FCPU:=TCPU.Create(Self);
 {$ENDIF}
 FProcessingMode:=pmNormal;
 FChunkData:=TMemoryStream.Create;
 FVstShellPlugins := TCustomVstShellPlugins.Create(Self);
 FCurrentVstShellPlugin:=0;
 FNumCategories:=1;
 FMidiEvent.numEvents := 0;
 for i := 0 to maxMidiEvents - 1 do
  begin
   GetMem(FMidiEvent.events[i], sizeof(TVstMidiEvent));
   FillChar(FMidiEvent.events[i]^, sizeof(TVstMidiEvent), 0);
   PVstMidiEvent(FMidiEvent.events[i])^.EventType := etMidi;
   PVstMidiEvent(FMidiEvent.events[i])^.ByteSize := 24;
  end;
end;

destructor TCustomVSTModule.Destroy;
var i : Integer;
begin
 try
  if Assigned(FParameterProperties) then FreeAndNil(FParameterProperties);
  if Assigned(FVstPrograms) then FreeAndNil(FVstPrograms);
  if Assigned(FEditorForm) then FreeAndNil(FEditorForm);
  {$IFDEF CPU_Detection}
  if Assigned(FCPU) then FreeAndNil(FCPU);
  {$ENDIF}
  if Assigned(FChunkData) then FreeAndNil(FChunkData);
  if Assigned(FVstShellPlugins) then FreeAndNil(FVstShellPlugins);
  for i := 0 to maxMidiEvents - 1 do FreeMem(FMidiEvent.events[i]);
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
 FAudioMaster:=AM;
 if FAudioMaster(nil, audioMasterVersion, 0, 0, nil, 0) = 0
  then raise exception.Create('AudioMaster Error');
 hv:=(HostProduct<>'WaveLab') {or (shortstring(temp)<>'energyXT')};
 if hv then hv:=(canHostDo('shellCategory')=1);

 if (PlugCategory=vpcShell) and hv then
  begin
   rUID:=getCurrentUniqueId;
   if (rUID>0) then
    begin
     for i:=0 to ShellPlugins.Count-1 do
      if rUID=ShellPlugins[i].UID then Break;
     if i<ShellPlugins.Count then
      if (rUID=ShellPlugins[i].UID) then
       begin
        FEffect.uniqueID:=rUID;
        if ShellPlugins[i].FNumInputs>=0 then FEffect.numInputs:=ShellPlugins[i].FNumInputs;
        if ShellPlugins[i].FNumOutputs>=0 then FEffect.numOutputs:=ShellPlugins[i].FNumOutputs;
        if ShellPlugins[i].FNumPrograms>=0 then FEffect.numPrograms:=ShellPlugins[i].FNumPrograms;
        if ShellPlugins[i].FNumParams>=0 then FEffect.numParams:=ShellPlugins[i].FNumParams;
        fPlugCategory:=ShellPlugins[i].fPlugCategory;
        if Assigned(ShellPlugins[i].FOnInstanciate) then
         begin
          sUID := '';
          for j := 3 downto 0 do sUID := sUID + char(rUID shr (j * 8));
          ShellPlugins[i].FOnInstanciate(Self,sUID);
         end;
        IOChanged;
       end;
    end;
  end
 else
  if (PlugCategory=vpcShell)
   then PlugCategory:=vpcUnknown;
end;

function TCustomVSTModule.Dispatcher(opcode: TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single): Integer;
var a,b     : Integer;
    keyCode : TVstKeyCode;
    s       : Single;
    Hndl    : THandle;
begin
 if Assigned(FOnDispatcher) then FOnDispatcher(Self,opcode);
 Result := 0;

 {$IFDEF Debug}
 if not (opcode in [effIdle, effEditIdle]) then
  FLog.Add(TimeToStr(Now - fTmStmp)+' Opcode: '+opcode2String(opcode)+' Value: '+IntToStr(Value));
 FLog.SaveToFile('Debug.log');
 {$ENDIF}

 case opcode of
  effOpen            : if Assigned(FOnOpen) then FOnOpen(Self);
  effClose           : if Assigned(FOnClose) then FOnClose(Self);
  effSetProgram      : if (Value < FEffect.numPrograms) and (Value >= 0) and (Value <> FCurProgram)
                        then CurrentProgram:=Value;
  effGetProgram      : Result := FCurProgram;
  effSetProgramName  : if numPrograms>0
                        then Programs[FCurProgram].DisplayName:=string(PChar(ptr));
  effGetProgramName  : if numPrograms>0
                        then StrPCopy(ptr,Programs[FCurProgram].DisplayName)
                        else StrPCopy(ptr,'');
  effGetParamLabel   : GetParameterLabel(Index, ptr);
  effGetParamDisplay : GetParameterDisplay(Index, ptr);
  effGetParamName    : GetParameterName(Index, ptr);
  effSetSampleRate   : begin setSampleRate(opt); Result:=1; end;
  effSetBlockSize    : setBlockSize(Value);
  effMainsChanged    : if (Value = 0) then Suspend else Resume;
  effGetVu           : if Assigned(FOnGetVUEvent)
                        then
                         begin
                          s:=0;
                          FOnGetVUEvent(s);
                          Result := round(s * 32767);
                         end
                        else Result := 0;
  // editor
  effEditGetRect     : begin
                        PPERect(ptr)^:=@FEditorRect;
                        FEditorRect.top := 0;
                        FEditorRect.left := 0;

                        if Assigned(FEditorForm) then
                         begin
                          FEditorRect.bottom := FEditorForm.ClientHeight;
                          FEditorRect.right := FEditorForm.ClientWidth;
                          Result := 1;
                         end
                        else Result := 0;
                       end;
  effEditOpen        : if (effFlagsHasEditor in FEffect.EffectFlags) then Result := EditorOpen(ptr);
  effEditClose       : if (effFlagsHasEditor in FEffect.EffectFlags) then EditorClose;
  effEditIdle        : if (effFlagsHasEditor in FEffect.EffectFlags) then EditorIdle;
  effEditTop         : if Assigned(FOnEditTop) then FOnEditTop(Self);
  effEditSleep       : if Assigned(FOnEditSleep) then FOnEditSleep(Self);
   // new
  effIdentify        : Result := FourCharToLong('N', 'v', 'E', 'f');
  effGetChunk        : Result := GetChunk(pointer(ptr^), (Index <> 0));
  effSetChunk        : Result := setChunk(ptr, Value, (Index <> 0));
  effProcessEvents             : Result := ProcessEvents(ptr);
  effCanBeAutomated            : Result := Integer(canParameterBeAutomated(Index));
  effString2Parameter          : Result := Integer(string2parameter(Index, ptr));
  effGetNumProgramCategories   : Result := fNumCategories;
  effGetProgramNameIndexed     : Result := Integer(getProgramNameIndexed(Value, Index, ptr));
  effCopyProgram               : Result := Integer(copyProgram(Index));
  effConnectInput              : begin
                                  if Assigned(FOnInConnected) then FOnInConnected(Self,Index,(Value <> 0));
                                  Result := 1;
                                 end;
  effConnectOutput             : begin
                                  if Assigned(FOnOutConnected) then FOnOutConnected(Self,Index,(Value <> 0));
                                  Result := 1;
                                 end;
  effGetInputProperties        : Result := Integer(GetInputProperties(Index, ptr));
  effGetOutputProperties       : Result := Integer(GetOutputProperties(Index, ptr));
  effGetPlugCategory           : Result := Integer(FPlugCategory);
  effGetCurrentPosition        : Result := reportCurrentPosition;
  effGetDestinationBuffer      : Result := Integer(reportDestinationBuffer);
  effOfflineNotify             : if Assigned(FOnOfflineNotify)
                                  then begin FOnOfflineNotify(Self, PVstAudioFile(ptr)^, Value, (Index <> 0)); Result := 1; end
                                  else Result := 0;
  effOfflinePrepare            : if Assigned(FOnOfflinePrepare)
                                  then begin FOnOfflinePrepare(Self, PVstOfflineTask(vcdOffline)^, Value); Result := 1; end
                                  else Result := 0;
  effOfflineRun                : if Assigned(FOnOfflineRun)
                                  then begin FOnOfflineRun(Self, PVstOfflineTask(vcdOffline)^, Value); Result := 1; end
                                  else Result := 0;
  effSetSpeakerArrangement     : Result := Integer(setSpeakerArrangement(pointer(Value), ptr));
  effProcessVarIo              : if Assigned(FOnProcessVarIO)
                                  then begin FOnProcessVarIO(Self, PVstVariableIo(ptr)^); Result := 1; end
                                  else Result := 0;
  effSetBlockSizeAndSampleRate : begin
                                  SetBlockSizeAndSampleRate(Value, opt);
                                  Result := 1;
                                 end;
  effSetBypass                 : Result := Integer(SetBypass(Value <> 0));
  effGetEffectName             : Result := Integer(GetEffectName(ptr));
  effGetErrorText              : Result := Integer(GetErrorText(ptr));
  effGetVendorString           : Result := Integer(GetVendorString(ptr));
  effGetProductString          : Result := Integer(GetProductString(ptr));
  effGetVendorVersion          : Result := GetVendorVersion;
  effVendorSpecific            : if Assigned(FOnVendorSpecific) then Result := FOnVendorSpecific(Self, Index, Value, ptr, opt);
  effCanDo                     : Result := canDo(ptr);
  effGetIcon                   : Result := Integer(getIcon);
  effSetViewPosition           : Result := Integer(setViewPosition(Index, Value));
  effGetTailSize               : Result := fTailSize;
  effIdle                      : Result := fxIdle;
  effGetParameterProperties    : Result := Integer(getParameterProperties(Index, ptr));
  effKeysRequired              : Result := Integer(not fKeysRequired); // reversed to keep v1 compatibility
  effGetVstVersion             : Result := 2400;
  effEditKeyDown               : if fKeysRequired then
                                  try
                                   keyCode.character := Index;
                                   keyCode.virt := Value;
                                   keyCode.modifier := Round(opt);
                                   if Assigned(EditorForm) then
                                    begin
                                     a:=KeyCodeToInteger(keyCode);
                                     if Assigned(EditorForm.ActiveControl)
                                      then Hndl:=EditorForm.ActiveControl.Handle
                                      else Hndl:=EditorForm.Handle;
{$IFNDEF FPC}
                                     if keyCode.virt=0 then b:=0 else b:=KF_EXTENDED;
                                     if (keyCode.modifier and MODIFIER_ALTERNATE)<>0
                                      then SendMessage(Hndl, WM_KEYDOWN, a,b)
                                      else SendMessage(Hndl, WM_SYSKEYDOWN, a,KF_ALTDOWN);
                                     SendMessage(Hndl,WM_CHAR, a, b);
{$ELSE}
                                     if keyCode.virt=0 then b:=0 else b:=$100;
                                     if (keyCode.modifier and MODIFIER_ALTERNATE)<>0
                                      then SendMessage(Hndl, LM_KEYDOWN, a,b)
                                      else SendMessage(Hndl, LM_SYSKEYDOWN, a, $2000);
                                     SendMessage(Hndl,LM_CHAR, a, b);
{$ENDIF}
                                     if Assigned(FOnKeyDown) then FOnKeyDown(Self, keyCode);
                                     if Assigned(FOnCheckKey)
                                      then if FOnCheckKey(Self, Char(a))
                                            then Result := 1
                                            else Result := -1
                                      else Result := -1;
                                    end;
                                  except
                                   Result := -1;
                                  end else Result := -1;
  effEditKeyUp                 : if fKeysRequired then
                                  try
                                   keyCode.character := Index;
                                   keyCode.virt := Value;
                                   keyCode.modifier := Round(opt);
                                   if Assigned(EditorForm) then
                                    begin
                                     a:=KeyCodeToInteger(keyCode);
                                     if Assigned(EditorForm.ActiveControl)
                                      then Hndl:=EditorForm.ActiveControl.Handle
                                      else Hndl:=EditorForm.Handle;
{$IFNDEF FPC}
                                     if keyCode.virt=0 then b:=0 else b:=KF_EXTENDED;
                                     if (keyCode.modifier and MODIFIER_ALTERNATE)<>0
                                      then SendMessage(Hndl, WM_KEYUP, a, b)
                                      else SendMessage(Hndl, WM_SYSKEYUP, a, KF_ALTDOWN);
{$ELSE}
                                     if keyCode.virt=0 then b:=0 else b:=$100;
                                     if (keyCode.modifier and MODIFIER_ALTERNATE)<>0
                                      then SendMessage(Hndl, LM_KEYUP, a,b)
                                      else SendMessage(Hndl, LM_SYSKEYUP, a, $2000);
                                     SendMessage(Hndl,LM_CHAR, a, b);
{$ENDIF}
                                     if Assigned(FOnKeyUp) then FOnKeyUp(Self, keyCode);
                                     if Assigned(FOnCheckKey)
                                      then if FOnCheckKey(Self, Char(a))
                                            then Result := 1
                                            else Result := -1
                                      else Result := -1;
                                    end;
                                  except
                                   Result := -1;
                                  end else Result := -1;
  effSetEditKnobMode          : if Assigned(FOnSetKnobMode)
                                 then begin FOnSetKnobMode(Self, Value); Result := 1; end
                                 else Result := 0;
  effGetMidiProgramName       : Result := GetMidiProgramName(Index, PMidiProgramName(ptr));
  effGetCurrentMidiProgram    : Result := GetCurrentMidiProgram(Index, PMidiProgramName(ptr));
  effGetMidiProgramCategory   : Result := GetMidiProgramCategory(Index, PMidiProgramCategory(ptr));
  effHasMidiProgramsChanged   : Result := Integer(hasMidiProgramsChanged(Index));
  effGetMidiKeyName           : Result := Integer(getMidiKeyName(Index, PMidiKeyName(ptr)));
  effBeginSetProgram          : if Assigned(FOnBeginSetProgram)
                                 then begin FOnBeginSetProgram(Self); Result := 1; end
                                 else Result := 0;
  effEndSetProgram          : if Assigned(FOnEndSetProgram)
                                 then begin FOnEndSetProgram(Self); Result := 1; end
                                 else Result := 0;
  effGetSpeakerArrangement    : Result := Integer(getSpeakerArrangement(PVstSpeakerArrangement(Value), PVstSpeakerArrangement(ptr)));
  effSetTotalSampleToProcess  : Result := setTotalSampleToProcess(Value);
  effShellGetNextPlugin       : Result := GetNextShellPlugin(pchar(ptr));
  effStartProcess             : Result := startProcess;
  effStopProcess              : Result := stopProcess ();
  effSetPanLaw                : Result := Integer(setPanLaw(Value, opt));
  effBeginLoadBank            : Result := beginLoadBank(PVstPatchChunkInfo(ptr));
  effBeginLoadProgram         : Result := beginLoadProgram(PVstPatchChunkInfo(ptr));
  effSetProcessPrecision      : Result := Integer(fProcessPrecisition); //< [value]: @see VstProcessPrecision  @see AudioEffectX::setProcessPrecision
  effGetNumMidiInputChannels	: Result := 0; //< [return value]: number of used MIDI input channels (1-15)  @see AudioEffectX::getNumMidiInputChannels
  effGetNumMidiOutputChannels	: Result := 0; //< [return value]: number of used MIDI output channels (1-15)  @see AudioEffectX::getNumMidiOutputChannels
  else
   try
     raise Exception.Create('unknown opcode');
   except
     Result := 0;
   end;
 end;
end;

procedure TCustomVSTModule.SetNumParams(newNum : Integer);
begin
 if Assigned(FParameterProperties)
  then FEffect.numParams:=FParameterProperties.Count
  else FEffect.numParams:=0
end;

procedure TCustomVSTModule.SetNumPrograms(newNum : Integer);
begin
 if Assigned(fVstPrograms)
  then FEffect.numPrograms:=fVstPrograms.Count
  else FEffect.numPrograms:=0
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
 FEffect.uniqueID:=FourCharToLong(fID[1], fID[2], fID[3], fID[4])
end;

procedure TCustomVSTModule.Process(Inputs, Outputs: PPSingle; SampleFrames: Integer);
var Ins  : TArrayOfSingleDynArray absolute Inputs;
    Outs : TArrayOfSingleDynArray absolute Outputs;
    OutsTmp: TArrayOfSingleDynArray;
    i, j: Integer;
begin
// fTempo := TempoAt(0) * 0.0001; // Get current bpm tempo from host
 SetLength(OutsTmp, FEffect.NumOutputs, SampleFrames);
 for j := 0 to FEffect.NumOutputs - 1
  do FillChar(OutsTmp[j, 0], SampleFrames*SizeOf(Single), 0);
 if Assigned(FOnProcessEx) then FOnProcessEx(Ins, OutsTmp, SampleFrames);
 for i := 0 to SampleFrames - 1 do
  for j := 0 to FEffect.NumOutputs - 1 do
   Outs[j, i] := Outs[j, i] + OutsTmp[j, i];
 if FMidiEvent.numEvents > 0 then
  begin
   sendVstEventsToHost(@FMidiEvent);
   FMidiEvent.numEvents := 0;
  end;
end;

procedure TCustomVSTModule.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: Integer);
var Ins  : TArrayOfSingleDynArray absolute Inputs;
    Outs : TArrayOfSingleDynArray absolute Outputs;
begin
// fTempo := TempoAt(0) * 0.0001; // Get current bpm tempo from host
 if Assigned(FOnProcessReplacingEx) then FOnProcessReplacingEx(Ins,Outs,SampleFrames);
 if FMidiEvent.numEvents > 0 then
  begin
   sendVstEventsToHost(@FMidiEvent);
   FMidiEvent.numEvents := 0;
  end;
end;

procedure TCustomVSTModule.ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: Integer);
var Ins  : TArrayOfDoubleDynArray absolute Inputs;
    Outs : TArrayOfDoubleDynArray absolute Outputs;
begin
// fTempo := TempoAt(0) * 0.0001; // Get current bpm tempo from host
 if Assigned(FOnProcessDoublesEx) then FOnProcessDoublesEx(Ins,Outs,SampleFrames);
 if FMidiEvent.numEvents > 0 then
  begin
   sendVstEventsToHost(@FMidiEvent);
   FMidiEvent.numEvents := 0;
  end;
end;

procedure TCustomVSTModule.FOnProcessCopy(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var i,j: Integer;
begin
 j:=numInputs; if numOutputs < numInputs then j := numOutputs;
 for i:=0 to j-1 do Move(Inputs[i,0], Outputs[i,0], SampleFrames * SizeOf(Single));
end;

procedure TCustomVSTModule.FOnProcessMute(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var i : Integer;
begin
 for i := 0 to numOutputs - 1
  do Fillchar(Outputs[i,0], 0, SampleFrames * SizeOf(Single));
end;

procedure TCustomVSTModule.FOnProcessCopy(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var i,j: Integer;
begin
 j := numInputs; if numOutputs < numInputs then j := numOutputs;
 for i := 0 to j - 1 do Move(Inputs[i,0], Outputs[i,0], SampleFrames * SizeOf(Double));
end;

procedure TCustomVSTModule.FOnProcessMute(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var i : Integer;
begin
 for i := 0 to numOutputs - 1
  do Fillchar(Outputs[i,0], 0, SampleFrames * SizeOf(Double));
end;

procedure TCustomVSTModule.SetBlockForcedSize(v: Integer);
begin
 if v>0 then FBlockModeSize:=v;
 FBlockPosition:=FBlockModeOverlap;
 PrepareBlockProcessing;
end;

procedure TCustomVSTModule.SetBlockOverlapSize(v: Integer);
begin
 if v<FBlockModeSize
  then FBlockModeOverlap:=v;
 if (FProcessingMode=pmBlockSave) and (FEffect.InitialDelay<FBlockModeSize-FBlockModeOverlap)
  then SetInitialDelay(FInitialDelay);
end;

procedure TCustomVSTModule.FOnBlockSaveProcess(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;

 repeat
  if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
   begin
    for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));
    for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));

    FBlockPosition:=FBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i := 0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));
    for i := 0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));

    FOnProcess(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);

    for i:=0 to numInputs-1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Single));
//    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,ProcessSizeH],fBlockOutBuffer[i,0],ProcessSizeH*Sizeof(Single));

    CurrentPosition := CurrentPosition + (FBlockModeSize - FBlockPosition);
    FBlockPosition := FBlockModeOverlap;
   end;
 until CurrentPosition >= SampleFrames;
end;

procedure TCustomVSTModule.FOnBlockSaveProcess(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;

 repeat
  if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
   begin
    for i := 0 to numInputs - 1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));
    for i := 0 to numOutputs - 1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));

    FBlockPosition:=FBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));

    FOnProcess(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);

    for i := 0 to numInputs - 1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Double));
//    for i := 0 to numOutputs - 1 do move(fBlockOutBuffer[i,ProcessSizeH],fBlockOutBuffer[i,0],ProcessSizeH*Sizeof(Double));

    CurrentPosition:=CurrentPosition+(FBlockModeSize-FBlockPosition);
    FBlockPosition:=FBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TCustomVSTModule.FOnBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;

 repeat
  if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
   begin
    for i := 0 to numInputs - 1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));
    for i := 0 to numOutputs - 1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));

    FBlockPosition:=FBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Single));

    FOnProcessReplacing(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);

    for i:=0 to numInputs-1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Single));
//    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockModeOverlap],fBlockOutBuffer[i,0],(FBlockModeSize-FBlockModeOverlap)*Sizeof(Single));

    CurrentPosition:=CurrentPosition+(FBlockModeSize-FBlockPosition);
    FBlockPosition:=FBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TCustomVSTModule.FOnBlockSaveProcessReplacing(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;
 repeat
  if FBlockPosition+(SampleFrames-CurrentPosition)<FBlockModeSize then
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));
    FBlockPosition:=FBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,FBlockPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockPosition],Outputs[i,CurrentPosition],(FBlockModeSize-FBlockPosition)*Sizeof(Double));
    FOnProcessReplacing(fBlockInBuffer,fBlockOutBuffer,FBlockModeSize);
    for i:=0 to numInputs-1  do move(fBlockInBuffer[i,(FBlockModeSize-FBlockModeOverlap)],fBlockInBuffer[i,0],FBlockModeOverlap*Sizeof(Double));
//    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,FBlockModeOverlap],fBlockOutBuffer[i,0],(FBlockModeSize-FBlockModeOverlap)*Sizeof(Double));
    CurrentPosition:=CurrentPosition+(FBlockModeSize-FBlockPosition);
    FBlockPosition:=FBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TCustomVSTModule.PrepareBlockProcessing;
var i : Integer;
begin
 if FProcessingMode=pmBlockSave then
  begin
   SetLength(fBlockInBuffer,numInputs);
   SetLength(fBlockOutBuffer,numOutputs);
   for i:=0 to numInputs-1 do SetLength(fBlockInBuffer[i],FBlockModeSize);
   for i:=0 to numOutputs-1 do SetLength(fBlockOutBuffer[i],FBlockModeSize);
   FBlockPosition:=FBlockModeOverlap;
   if (FProcessingMode=pmBlockSave) and (FEffect.InitialDelay<FBlockModeSize-FBlockModeOverlap)
    then SetInitialDelay(FInitialDelay);
  end
 else
  begin
   if Length(fBlockInBuffer)>0 then for i:=0 to Length(fBlockInBuffer)-1 do SetLength(fBlockInBuffer[i],0);
   if Length(fBlockOutBuffer)>0 then for i:=0 to Length(fBlockOutBuffer)-1 do SetLength(fBlockOutBuffer[i],0);
   SetLength(fBlockInBuffer,0);
   SetLength(fBlockOutBuffer,0);
  end;
end;

procedure TCustomVSTModule.SetOnProcess(v : TProcessAudioEvent);
begin
 FOnProcess:=v;
 case FProcessingMode of
  pmNormal: FOnProcessEx:=FOnProcess;
  pmBlockSave:  begin
                 if Assigned(FOnProcessReplacing)
                  then FOnProcessEx:=FOnBlockSaveProcess
                  else FOnProcessEx:=FOnProcess;
//                 PrepareBlockProcessing;
                end;
  pmCopy: FOnProcessEx:=FOnProcessCopy;
  pmMute: FOnProcessEx:=FOnProcessMute;
 end;
end;

procedure TCustomVSTModule.SetOnProcessReplacing(v : TProcessAudioEvent);
begin
 FOnProcessReplacing:=v;
 case FProcessingMode of
  pmNormal: FOnProcessReplacingEx:=FOnProcessReplacing;
  pmBlockSave: begin
                if Assigned(FOnProcessReplacing)
                 then FOnProcessReplacingEx:=FOnBlockSaveProcessReplacing
                 else FOnProcessReplacingEx:=FOnProcessReplacing;
//                PrepareBlockProcessing;
               end;
  pmCopy:   FOnProcessReplacingEx:=FOnProcessCopy;
  pmMute:   FOnProcessReplacingEx:=FOnProcessMute;
 end;
end;

procedure TCustomVSTModule.SetOnProcessDoubleReplacing(v : TProcessDoubleEvent);
begin
 FOnProcessDoubles:=v;
 case FProcessingMode of
  pmNormal: FOnProcessDoublesEx:=FOnProcessDoubles;
  pmBlockSave: begin
                if Assigned(FOnProcessDoubles)
                 then FOnProcessDoublesEx:=FOnBlockSaveProcessReplacing
                 else FOnProcessDoublesEx:=FOnProcessDoubles;
//                PrepareBlockProcessing;
               end;
  pmCopy:   FOnProcessReplacingEx:=FOnProcessCopy;
  pmMute:   FOnProcessReplacingEx:=FOnProcessMute;
 end;
end;

procedure TCustomVSTModule.SetProcessingMode(v : TProcessingMode);
begin
 if v<>fProcessingmode then
  begin
   fProcessingmode:=v;
   case FProcessingMode of
    pmNormal: begin
               FOnProcessEx:=FOnProcess;
               FOnProcessReplacingEx:=FOnProcessReplacing;
              end;
    pmBlockSave: begin
                  if Assigned(FOnProcess)
                   then FOnProcessEx:=FOnBlockSaveProcess
                   else FOnProcessEx:=FOnProcess;
                  if Assigned(FOnProcessReplacing)
                   then FOnProcessReplacingEx:=FOnBlockSaveProcessReplacing
                   else FOnProcessReplacingEx:=FOnProcessReplacing;
                PrepareBlockProcessing;
              end;
    pmCopy:   begin
               FOnProcessEx:=FOnProcessCopy;
               FOnProcessReplacingEx:=FOnProcessCopy;
              end;
    pmMute:   begin
               FOnProcessEx:=FOnProcessMute;
               FOnProcessReplacingEx:=FOnProcessMute;
              end;
   end;
  end;
end;

procedure TCustomVSTModule.SetProgram(aProgram: Integer);
var i: Integer;
begin
 if (aProgram >= 0) and (aProgram < FEffect.numPrograms) and (numPrograms>0) then
  begin
   if Assigned(FOnBeforeProgramChange) then FOnBeforeProgramChange(Self);
   FCurProgram := aProgram;
   if Assigned(FOnAfterProgramChange) then FOnAfterProgramChange(Self);
//   if (effFlagsProgramChunks in FEffect.EffectFlags) then
    try
     for i := 0 to Length(Programs[FCurProgram].FParameter)-1
      do setParameter(i, Programs[FCurProgram].FParameter[i]);
    except
    end;
   FEditorNeedUpdate := True;
  end;
 updateDisplay;
end;

procedure TCustomVSTModule.SetCurrentProgramName(AName: string);
begin
 if (FCurProgram<numPrograms) and (numPrograms>0) then
  begin
   Programs[FCurProgram].DisplayName:=AName;
   FEditorNeedUpdate := True;
  end;
 updateDisplay;
end;

function TCustomVSTModule.GetCurrentProgramName:string;
begin
 if (FCurProgram<numPrograms) and (numPrograms>0) and (FCurProgram>=0)
  then Result:=Programs[FCurProgram].DisplayName
  else Result:='';
end;

procedure TCustomVSTModule.GetParameterLabel(Index: Integer; Text: pchar);
var str : string;
begin
 if (Index >= FEffect.numParams) or (Index>=FParameterProperties.Count)
  then str:='undefined'
  else
   begin
    str := FParameterProperties[Index].Units;
    if Assigned(FParameterProperties[Index].FOnCPL)
     then FParameterProperties[Index].FOnCPL(Self,Index,str);
   end;
 StrPCopy(Text, str)
end;

procedure TCustomVSTModule.GetParameterDisplay(Index: Integer; Text: pchar);
var str : string;
begin
 if (Index >= FEffect.numParams) or (Index>=FParameterProperties.Count)
  then str:='undefined'
  else
   begin
    if (effFlagsProgramChunks in FEffect.EffectFlags)
     then str:=FloatToStr(FOnGetChunkParamEvent(Self,Index))
     else
      if (numPrograms>0)
       then str:=FloatToStrF(Programs[FCurProgram].FParameter[Index],ffGeneral,4,4)
       else str:=FloatToStrF(FParameter[Index],ffGeneral,4,4);
    if Assigned(FParameterProperties[Index].FOnCPD)
     then FParameterProperties[Index].FOnCPD(Self,Index,str);
   end;
 StrPCopy(Text, str)
end;

procedure TCustomVSTModule.GetParameterName(Index: Integer; Text: pchar);
begin
 if (Index >= FEffect.numParams) or (Index>=FParameterProperties.Count)
  then StrPCopy(Text, 'undefined')
  else StrPCopy(Text,FParameterProperties[Index].DisplayName);
end;

function TCustomVSTModule.GetChunk(var data: pointer; isPreset: Boolean): Integer;
var i,j  : Integer;
    tmps : TMemoryStream;
begin
 Result := 0;
 if (numPrograms<=0) then Exit;
 if isPreset then
 begin
  Programs[FCurProgram].Chunk.Position:=0;
  if Assigned(Programs[FCurProgram].FOnStoreChunk)
   then Programs[FCurProgram].FOnStoreChunk(Programs[FCurProgram],FCurProgram,True);
  data := Programs[FCurProgram].Chunk.Memory;
  Result := Programs[FCurProgram].Chunk.Size;
 end else
 begin
  tmps:=TMemoryStream.Create;
  for i := 0 to numPrograms - 1 do
   begin
    Programs[i].Chunk.Position := 0;
    if Assigned(Programs[i].FOnStoreChunk)
     then Programs[i].FOnStoreChunk(Programs[FCurProgram],FCurProgram,False);
    j := Programs[i].Chunk.Size;
    tmps.Write(j, 4);
    tmps.Write(Programs[i].Chunk.Memory^, Programs[i].Chunk.Size);
   end;
  Data := tmps.Memory;
  Result := tmps.Size;
 end;
end;

function TCustomVSTModule.SetChunk(Data: pointer; byteSize: Integer; isPreset: Boolean): Integer;
var i: Integer;
    pi: pInteger;
    pb: pbyte;
begin
 Result := 0;
 if (numPrograms<=0) then Exit;
 if isPreset then
  with Programs[FCurProgram] do
   begin
    Chunk.Clear;
    Chunk.Write(data^, byteSize);
    Chunk.Position:=0;
    Result:=bytesize;
    if Assigned(FOnLoadChunk)
     then FOnLoadChunk(Programs[FCurProgram],FCurProgram,True);
   end
 else
  begin
   pb := data;
   for i := 0 to NumPrograms - 1 do
    begin
     Programs[i].Chunk.Clear;
     pi := pInteger(pb);
     inc(pb, 4);
     Programs[i].Chunk.Write(pb^, pi^);
     Programs[i].Chunk.Position:=0;
     inc(pb, pi^);
     if Assigned(Programs[i].FOnLoadChunk)
      then Programs[i].FOnLoadChunk(Programs[i],i,False);
    end;
   Result := bytesize;
   if Assigned(Programs[CurrentProgram].FOnLoadChunk)
    then Programs[CurrentProgram].FOnLoadChunk(Programs[CurrentProgram],CurrentProgram,False);
  end;
 FEditorNeedUpdate:=True;
end;

procedure TCustomVSTModule.SetSampleRate(newValue: Single);
begin
 if fSampleRate<>newValue then
  begin
   fSampleRate := newValue;
   if Assigned(fSampleRateChangeEvent) then fSampleRateChangeEvent(Self,newValue);
  end;
end;

procedure TCustomVSTModule.SetBlockSize(newValue: Integer);
begin
 if fBlockSize<>newValue then
  begin
   fBlockSize := newValue;
   if Assigned(fBlockSizeChangeEvent) then fBlockSizeChangeEvent(Self,newValue);
  end;
end;

procedure TCustomVSTModule.Suspend;
begin
 if Assigned(FOnSuspend) then FOnSuspend(Self);
end;

procedure TCustomVSTModule.Resume;
begin
 if Assigned(FOnResume) then FOnResume(Self);
 wantEvents(1);
end;

procedure TCustomVSTModule.SetNumInputs(Inputs: Integer);
begin
 FEffect.numInputs := Inputs;
 PrepareBlockProcessing;
 IOChanged;
end;

procedure TCustomVSTModule.SetNumOutputs(Outputs: Integer);
begin
 FEffect.numOutputs := Outputs;
 PrepareBlockProcessing;
 IOChanged;
end;

function TCustomVSTModule.GetMasterVersion: Integer;
var vers: Integer;
begin
 vers := 1;
 if Assigned(FAudioMaster) then
  begin
   vers := FAudioMaster(@FEffect, audioMasterVersion, 0, 0, nil, 0);
   if (vers = 0)
    then vers := 1;
  end;
 Result := vers;
end;

function TCustomVSTModule.GetCurrentUniqueId: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterCurrentId, 0, 0, nil, 0)
  else Result := 0;
end;

procedure TCustomVSTModule.masterIdle;
begin
 if Assigned(FAudioMaster)
  then FAudioMaster(@FEffect, audioMasterIdle, 0, 0, nil, 0);
end;

function TCustomVSTModule.isInputConnected(input: Integer): Boolean;
var ret: Integer;
begin
 ret := 0;
 if Assigned(FAudioMaster)
  then ret := FAudioMaster(@FEffect, audioMasterPinConnected, input, 0, nil, 0);
 Result := (ret = 0);
end;

function TCustomVSTModule.isOutputConnected(output: Integer): Boolean;
var ret: Integer;
begin
 ret := 0;
 if Assigned(FAudioMaster)
  then ret := FAudioMaster(@FEffect, audioMasterPinConnected, output, 1, nil, 0);
 Result := (ret = 0);
end;

// Flags

procedure TCustomVSTModule.SetPluginFlags(newFlags : TEffFlags);
begin
 FEffect.EffectFlags:=newFlags;
end;

function TCustomVSTModule.GetPluginFlags: TEffFlags;
begin
 Result:=FEffect.EffectFlags;
end;

procedure TCustomVSTModule.SetInitialDelay(delay: Integer);
var hst : string;
begin
 if FInitialDelay<>delay then
  begin
   FInitialDelay:=delay;
   if (FProcessingMode=pmBlockSave) and (FInitialDelay<FBlockModeSize-FBlockModeOverlap)
    then FEffect.initialDelay := FBlockModeSize-FBlockModeOverlap
    else FEffect.initialDelay := FInitialDelay;
   hst:=HostProduct;
   if hst<>'energyXT'
    then IOChanged;
  end;
end;

function TCustomVSTModule.GetEffect: PVSTEffect;
begin
 Result := @FEffect;
end;

procedure TCustomVSTModule.SetVstPrograms(const Value: TCustomVstPrograms);
begin
 FVstPrograms.Assign(Value);
end;

procedure TCustomVSTModule.SetParameterProperties(const Value : TCustomVstParameterProperties);
begin
 FVstPrograms.Assign(Value);
end;

function TCustomVSTModule.Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
begin
 if (Index>=numParams) or (Index>=FParameterProperties.Count) then begin Result := 0; Exit; end; 
 Result := (Value - FParameterProperties[Index].min) / (FParameterProperties[Index].max - FParameterProperties[Index].min);
 case FParameterProperties[Index].curve of
  ctLogarithmic: Result := ln(FParameterProperties[Index].curveFactor * Result + 1) / ln(FParameterProperties[Index].curveFactor + 1);
  ctExponential: Result := exp(Result * ln(FParameterProperties[Index].curveFactor + 1)) - 1;
  ctFrequencyScale: if FParameterProperties[Index].min<>0
                     then Result := ln((FParameterProperties[Index].max/FParameterProperties[Index].min)*Result+1)/ln((FParameterProperties[Index].max/FParameterProperties[Index].min))
                     else Result := ln((FParameterProperties[Index].max)*Result+1)/ln((FParameterProperties[Index].max));
  else
 end;
 Result := f_limit(Result, 0, 1);
end;

function TCustomVSTModule.VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
begin
 Result := Value;
 case FParameterProperties[Index].curve of
  ctLogarithmic: Result := (exp(Result * ln(FParameterProperties[Index].curveFactor + 1)) - 1) / FParameterProperties[Index].curveFactor;
  ctExponential: Result := ln(FParameterProperties[Index].curveFactor * Result + 1) / ln(FParameterProperties[Index].curveFactor + 1);
 else
 end;
 Result := FParameterProperties[Index].Smooth(Result * (FParameterProperties[Index].max - FParameterProperties[Index].min) + FParameterProperties[Index].min);
end;

procedure TCustomVSTModule.ReadOnlyString(s: string);
begin end;

procedure TCustomVSTModule.SetParameterAutomated(Index: Integer; Value: Single);
begin
 if (Index>=numParams) or (Index>=FParameterProperties.Count) then Exit;
 setParameter(Index,Value);
 if Assigned(FAudioMaster) and Assigned(FParameterProperties[Index]) then
  if FParameterProperties[Index].CanBeAutomated and not FIsHostAutomation
   then FAudioMaster(@FEffect, audioMasterAutomate, Index, 0, nil, Parameter2VSTParameter(Value,Index));
end;

procedure TCustomVSTModule.SetParameter(const Index: Integer; Value: Single);
begin
 if FParameterUpdate then exit;
 {$IFDEF Debug} FLog.Add(TimeToStr(Now-fTmStmp)+' Set Parameter: Index: '+IntToStr(index) + ' Value: '+FloatToStr(Value)); {$ENDIF}
 FParameterUpdate:=True;
 try
  if (Index >= FEffect.numParams) or (Index < 0) or (Index>=FParameterProperties.Count)
   then raise Exception.Create('Index out of bounds');
  if (effFlagsProgramChunks in FEffect.EffectFlags)
   then
    begin
     if Assigned(ParameterProperties[Index].FOnSPC)
      then FParameterProperties[Index].FOnSPC(Self,Index,Value);
     if Assigned(OnParameterChange)
      then OnParameterChange(Self, Index, Value);
    end
   else
    begin
     if (numPrograms>0) and (FCurProgram>=0)
      then
       begin
        Programs[FCurProgram].FParameter[Index] := Value;
        if Assigned(ParameterProperties[Index].FOnSPC)
         then FParameterProperties[Index].FOnSPC(Self,Index,Programs[FCurProgram].FParameter[Index]);
        if Assigned(OnParameterChange)
         then OnParameterChange(Self, Index, Programs[FCurProgram].FParameter[Index]);
       end
      else
       begin
        FParameter[Index] := Value;
        if Assigned(ParameterProperties[Index].FOnSPC)
         then FParameterProperties[Index].FOnSPC(Self,Index,FParameter[Index]);
        if Assigned(OnParameterChange)
         then OnParameterChange(Self, Index, FParameter[Index]);
       end
    end;
  FEditorNeedUpdate := True;
 finally
  FParameterUpdate := False;
 end;
end;

function TCustomVSTModule.GetParameter(Index: Integer): Single;
begin
 if (effFlagsProgramChunks in FEffect.EffectFlags)
  then Result:=FOnGetChunkParamEvent(Self,Index)
  else
   if numPrograms>0
    then Result:=Programs[FCurProgram].FParameter[Index]
    else Result:=FParameter[Index];
end;

//------------------------------------------------------------------------------
//  TVSTModuleEditor
//------------------------------------------------------------------------------

function TCustomVSTModule.EditorOpen(ptr: Pointer): Integer;
var i,pr : Integer;
begin
 Result := 0;
 if Assigned(FOnEditOpen) then FOnEditOpen(Self, FEditorForm);
 if Assigned(FEditorForm) then
  try
   Result := 1;
   with FEditorForm do
    begin
     {$IFNDEF FPC}
     ParentWindow := HWnd(ptr);
     {$ELSE}
     Handle := Integer(ptr);
     {$ENDIF}
     Visible:=True;
     BorderStyle:=bsNone;
     SetBounds(0, 0, Width, Height);
     Invalidate;
    end;
   pr := min(numParams, FParameterProperties.Count);
   if Assigned(FOnParameterChangeEvent) and (not (effFlagsProgramChunks in FEffect.EffectFlags)) then
    if numPrograms > 0
     then for i := 0 to pr - 1 do FOnParameterChangeEvent(Self, i, Programs[FCurProgram].FParameter[i])
     else for i := 0 to pr - 1 do FOnParameterChangeEvent(Self, i, FParameter[i]);
  except
  end;
end;

procedure TCustomVSTModule.EditorClose;
begin
 if Assigned(FOnEditClose) then FOnEditClose(Self);
 if Assigned(FEditorForm) then FreeAndNil(FEditorForm);
end;

procedure TCustomVSTModule.EditorIdle;
begin
 if FEditorNeedUpdate and Assigned(FEditorForm) then
  begin
   if Assigned(FOnEditIdle) then FOnEditIdle(Self);
   FEditorNeedUpdate := False;
  end;
end;

procedure TCustomVSTModule.EditorPostUpdate;
begin
 FEditorNeedUpdate := True;
end;

{$IFDEF UseDelphi}
procedure TCustomVSTModule.ReadState(Reader: TReader);
var i: Integer;
begin
 {$IFDEF Debug} FLog.Add('Before ReadState'); {$ENDIF}
 {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}
 inherited;
 {$IFDEF Debug} FLog.Add('After ReadState'); {$ENDIF}
 {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}
 for i := 0 to numPrograms - 1
  do if Assigned(Programs[i].FOnInitialize) then Programs[i].FOnInitialize(Programs[i]);
 if numPrograms < 0
  then FCurProgram := -1
  else CurrentProgram := 0;
 if Assigned(FOnInitialize) then FOnInitialize(Self);
 {$IFDEF Debug} FLog.Add('End ReadState'); {$ENDIF}
 {$IFDEF Debug} FLog.SaveToFile('Debug.log'); {$ENDIF}
end;
{$ENDIF}

// Functions

function DispatchEffectClass(Effect: PVSTEffect; opcode : TDispatcherOpcode; Index, Value: Integer; ptr: pointer; opt: Single): Integer; cdecl;
var VSTModule: TCustomVSTModule;
begin
 VSTModule := TCustomVSTModule(Effect^.vObject);
 if (opcode = effClose) then
  try
   VSTModule.Dispatcher(opcode, Index, Value, ptr, opt);
   VSTModule.Free;
   Result := 1;
  except
   Result := 0;
  end
 else Result := VSTModule.Dispatcher(opcode, Index, Value, ptr, opt);
end;

function GetParameterClass(Effect: PVSTEffect; Index: Integer): Single; cdecl;
var VSTModule: TCustomVSTModule;
begin
 VSTModule := TCustomVSTModule(Effect^.vObject);
 if (Index<VSTModule.numParams) and (Index<VSTModule.FParameterProperties.Count)
  then Result := VSTModule.Parameter2VSTParameter(VSTModule.GetParameter(Index),Index)
  else Result := 0;
end;

procedure SetParameterClass(Effect: PVSTEffect; Index: Integer; Value: Single); cdecl;
begin
 with TCustomVSTModule(Effect^.vObject) do
  begin
   {$IFDEF Debug} FLog.Add(TimeToStr(Now-fTmStmp)+'Set Parameter Class: '+FloatToStr(Value)); {$ENDIF}
   if FIsHostAutomation then exit;
   FIsHostAutomation:=True;
   if ((Index>=numParams) or (Index>=FParameterProperties.Count)) and Assigned(OnParameterSizeFailed)
    then OnParameterSizeFailed(TCustomVSTModule(Effect^.vObject))
    else setParameter(Index, VSTParameter2Parameter(Value,Index));
   FIsHostAutomation:=False;
  end;
end;

procedure ProcessClass(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
begin
 TCustomVSTModule(Effect^.vObject).Process(Inputs, Outputs, SampleFrames);
end;

procedure ProcessClassReplacing(Effect: PVSTEffect; Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
begin
 TCustomVSTModule(Effect^.vObject).ProcessReplacing(Inputs, Outputs, SampleFrames);
end;

procedure ProcessClassDoubleReplacing(Effect: PVSTEffect; Inputs, Outputs: PPDouble; SampleFrames: Integer); cdecl;
begin
 TCustomVSTModule(Effect^.vObject).ProcessDoubleReplacing(Inputs, Outputs, SampleFrames);
end;

// TCustomVstParameterProperty

{$IFDEF FPC}
constructor TCustomVstParameterProperty.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstParameterProperty.Create(Collection: TCollection);
{$ENDIF}
var i: Integer;
begin
 inherited;
 FMin         := 0;
 FMax         := 1;
 FMinInteger  := 0;
 FMaxInteger  := 100;
 FStepInteger := 1;
 FCC          := -1;
 FCurve       := ctLinear;
 FFlags       := [];
 FCurveFactor := 1;
 FLargeStepInteger := 10;
 FSmoothingFactor  := 1;
 FCanBeAutomated   := True;
 FV2Properties     := False;
 FDisplayName := 'Parameter '+IntTostr(Collection.Count);
 FVSTModule := (Collection As TCustomVstParameterProperties).VSTModule;
 try
  FVSTModule.FEffect.numParams := Collection.Count;
  if not (effFlagsProgramChunks in fVSTModule.FEffect.EffectFlags) then
   with FVSTModule do
    if (FEffect.numPrograms>0)
     then for i := 0 to FEffect.numPrograms-1 do SetLength(Programs[i].FParameter,Collection.Count)
     else SetLength(FParameter,Collection.Count);
 except
 end;
end;

destructor TCustomVstParameterProperty.Destroy;
var i: Integer;
begin
 try
  if assigned(VSTModule) then
   with FVSTModule do
    if not (effFlagsProgramChunks in FEffect.EffectFlags) then
     if FEffect.numPrograms>0
      then for i:=0 to FEffect.numPrograms-1 do SetLength(Programs[i].FParameter,Collection.Count-1)
      else SetLength(FParameter,Collection.Count-1);
 except
 end;
 inherited;
end;

function TCustomVstParameterProperty.Smooth(i: Single): Single;
begin
 FSmoothStates[0] := FSmoothStates[0] + SmoothingFactor * (i - FSmoothStates[0]);
 FSmoothStates[1] := FSmoothStates[1] + SmoothingFactor * (FSmoothStates[0] - FSmoothStates[1]);
 Result := FSmoothStates[1];
end;

procedure TCustomVstParameterProperty.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstParameterProperty then
  with TCustomVstParameterProperty(Dest) do
   try
    Units := Self.Units;
    DisplayName := Self.DisplayName;
   except
    inherited;
   end
  else inherited;
end;

procedure TCustomVstParameterProperty.SetDisplayName(const AValue: string);
begin
 FDisplayName:=Copy(AValue,1,Math.Min(30,Length(AValue)));
end;

function TCustomVstParameterProperty.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TCustomVstParameterProperty.SetUnits(AUnits: string);
begin
 FUnits := AUnits;
end;

{ TCustomVstParameterProperties }

constructor TCustomVstParameterProperties.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TCustomVstParameterProperty);
 FVSTModule := TCustomVSTModule(AOwner);
end;

destructor TCustomVstParameterProperties.Destroy;
begin
 while Count>0 do Delete(0);
 inherited;
end;

procedure TCustomVstParameterProperties.WriteVSTXML;
{$IFNDEF FPC}
var s : string;
    b : PChar;
{$ENDIF}
begin
 {$IFNDEF FPC}
 GetMem(b,255); GetModuleFileName(Application.Handle,b,255); FreeMem(b);
 s:=b; WriteVSTXML(Copy(s,1,Pos('.dll',s)-1)+'.VSTXML');
 {$ENDIF}
end;

procedure TCustomVstParameterProperties.WriteVSTXML(FileName: TFileName);
var i : Integer;
begin

 with TStringlist.Create do
  try
   SaveToFile(FileName);
   Add('<!-- =========================================================== -->');
   Add('<!-- XML definition of VST parameters ========================== -->');
   Add('<!-- Draft 0.1================================================== -->');
   Add('<!-- Date: '+DateToStr(Now)+'=========================================== -->');
   Add('<!-- =========================================================== -->');
   Add('<VSTPluginProperties>');
   Add('');
   Add(#9+'<VSTParametersStructure>');
   Add(#9+#9+'<!--  Create Global Params================================== -->');
   for i:=0 to Count-1 do
    begin
     Add(#9+#9+'<Param name="'+Items[i].FDisplayName+'"'+#9+
                'shortName="'+Items[i].fShortLabel+'"'+#9+
                'id="'+IntToStr(i)+'"/>');
    end;
   Add(#9+'</VSTParametersStructure>');
   Add('</VSTPluginProperties>');
  finally
   Free;
  end;
end;

function TCustomVstParameterProperties.Add: TCustomVstParameterProperty;
begin
  Result := TCustomVstParameterProperty(inherited Add);
end;

function TCustomVstParameterProperties.GetItem(Index: Integer): TCustomVstParameterProperty;
begin
 Result := TCustomVstParameterProperty(inherited GetItem(Index));
end;

function TCustomVstParameterProperties.Insert(Index: Integer): TCustomVstParameterProperty;
begin
 Result := TCustomVstParameterProperty(inherited Insert(Index));
end;

procedure TCustomVstParameterProperties.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TCustomVstParameterProperties.SetItem(Index: Integer; const Value: TCustomVstParameterProperty);
begin
 inherited SetItem(Index, Value);
end;

// TVstProgram

{$IFDEF FPC}
constructor TCustomVstProgram.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstProgram.Create(Collection: TCollection);
{$ENDIF}
begin
 inherited;
 FDisplayName := 'Init';
 fVSTModule := (Collection As TCustomVstPrograms).VSTModule;
 VSTModule.FEffect.numPrograms:=Collection.Count;
 if not (effFlagsProgramChunks in VSTModule.FEffect.EffectFlags)
  then SetLength(FParameter,VSTModule.numParams)
  else fChunkData:=TMemoryStream.Create;
 if VSTModule.FCurProgram<0 then VSTModule.FCurProgram:=0;
end;

destructor TCustomVstProgram.Destroy;
begin
 try
  SetLength(FParameter,0);
  FreeAndNil(fChunkData);
 finally
  inherited;
 end;
end;

function TCustomVstProgram.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TCustomVstProgram.SetDisplayName(const AValue: string);
begin
 FDisplayName:=copy(AValue,0,50);
end;

procedure TCustomVstProgram.AssignTo(Dest: TPersistent);
var i: Integer;
begin
 if Dest is TCustomVstProgram then
  with TCustomVstProgram(Dest) do
   begin
    if Length(Self.FParameter)>0 then
     begin
      SetLength(TCustomVstProgram(Dest).FParameter,Length(Self.FParameter));
      for i := 0 to Length(Self.FParameter) - 1 do Parameter[i]:=Self.Parameter[i];
     end;
    DisplayName := Self.DisplayName;
   end
  else inherited;
end;

procedure TCustomVstProgram.SetParameter(AIndex: Integer; s: Single);
begin
 if effFlagsProgramChunks in fVSTModule.Flags then exit;
 if (AIndex>=0) and (AIndex<VSTModule.numParams)
  then FParameter[AIndex]:=s
  else //raise exception.Create('Index out of bounds');
end;

function TCustomVstProgram.GetParameter(AIndex: Integer): Single;
begin
 if (AIndex>=0) and (AIndex<VSTModule.numParams)
  then Result:=FParameter[AIndex] else
   begin
    Result := 0;
    // raise exception.Create('Index out of bounds');
   end; 
end;

{ TVstPrograms }

constructor TCustomVstPrograms.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TCustomVstProgram);
 FVSTModule := TCustomVSTModule(AOwner);
end;

destructor TCustomVstPrograms.Destroy;
begin
 while Count>0 do Delete(0);
 inherited;
end;

function TCustomVstPrograms.Add: TCustomVstProgram;
begin
  Result := TCustomVstProgram(inherited Add);
end;

function TCustomVstPrograms.GetItem(Index: Integer): TCustomVstProgram;
begin
 Result := TCustomVstProgram(inherited GetItem(Index));
end;

function TCustomVstPrograms.Insert(Index: Integer): TCustomVstProgram;
begin
 Result := TCustomVstProgram(inherited Insert(Index));
end;

procedure TCustomVstPrograms.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TCustomVstPrograms.SetItem(Index: Integer; const Value: TCustomVstProgram);
begin
 inherited SetItem(Index, Value);
end;

{$IFDEF CPU_Detection}
{$WARNINGS ON}
function IsCPUID_Available: Boolean; register;
asm
  PUSHFD                 {save EFLAGS to stack}
  POP     EAX            {store EFLAGS in EAX}
  MOV     EDX, EAX       {save in EDX for later testing}
  XOR     EAX, $200000;  {flip ID bit in EFLAGS}
  PUSH    EAX            {save new EFLAGS Value on stack}
  POPFD                  {replace current EFLAGS Value}
  PUSHFD                 {get new EFLAGS}
  POP     EAX            {store new EFLAGS in EAX}
  XOR     EAX, EDX       {check if ID bit changed}
  JZ      @exit          {no, CPUID not available}
  MOV     EAX, True      {yes, CPUID is available}
@exit:
end;

function IsFPU_Available: Boolean;
var _FCW, _FSW: Word;
asm
  MOV     EAX, False     {initialize return register}
  MOV     _FSW, $5A5A    {store a non-zero Value}
  FNINIT                 {must use non-wait form}
  FNSTSW  _FSW           {store the status}
  CMP     _FSW, 0        {was the correct status read?}
  JNE     @exit          {no, FPU not available}
  FNSTCW  _FCW           {yes, now save control word}
  MOV     DX, _FCW       {get the control word}
  AND     DX, $103F      {mask the proper status bits}
  CMP     DX, $3F        {is a numeric processor installed?}
  JNE     @exit          {no, FPU not installed}
  MOV     EAX, True      {yes, FPU is installed}
@exit:
end;

procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
asm
  PUSH    EBX                         {save affected registers}
  PUSH    EDI
  MOV     EDI, Registers
  XOR     EBX, EBX                    {clear EBX register}
  XOR     ECX, ECX                    {clear ECX register}
  XOR     EDX, EDX                    {clear EDX register}
  DB $0F, $A2                         {CPUID opcode}
  MOV     TRegisters(EDI).&EAX, EAX   {save EAX register}
  MOV     TRegisters(EDI).&EBX, EBX   {save EBX register}
  MOV     TRegisters(EDI).&ECX, ECX   {save ECX register}
  MOV     TRegisters(EDI).&EDX, EDX   {save EDX register}
  POP     EDI                         {restore registers}
  POP     EBX
end;

constructor TCPU.Create(AOwner : TPersistent);
begin
 inherited Create;
 GetCPUInfo;
end;

destructor TCPU.Destroy;
begin
 inherited;
end;

function TCPU.GetOwner: TPersistent;
begin
 Result:=FOwner;
end;

procedure TCPU.GetCPUVendor;
var  VendorStr: TVendorStr;
     Registers: TRegisters;
begin
 GetCPUID(0, Registers);
 SetLength(VendorStr, 12);
 Move(Registers.EBX, VendorStr[1], 4);
 Move(Registers.EDX, VendorStr[5], 4);
 Move(Registers.ECX, VendorStr[9], 4);
 fVendor := High(TCPUVendor);
 while (VendorStr <> VendorIDString[fVendor]) and (fVendor > Low(TCPUVendor))
  do Dec(fVendor);
end;

procedure TCPU.GetCPUFeatures;
type _Int64 = packed record
       Lo: Longword;
       Hi: Longword;
     end;
var Registers: TRegisters;
    CpuFeatures: TCpuFeatureSet;
begin
 GetCPUID($00000001, Registers);
 fSignature := Registers.EAX;
 fEffFamily := fSignature and $00000F00 shr 8;
 fEffModel := fSignature and $000000F0 shr 4;
 if fEffFamily = $F then
  begin
   fEffFamily := fEffFamily + (fSignature and $0FF00000 shr 20);
   fEffModel := fEffModel + (fSignature and $000F0000 shr 12);
  end;
 Move(Registers.EDX, _Int64(CPUFeatures).Lo, 4);
 Move(Registers.ECX, _Int64(CpuFeatures).Hi, 4);
 if cfFPU in CpuFeatures then Include(fInstructions, isFPU);
 if cfTSC in CPUFeatures then Include(fInstructions, isTSC);
 if cfCX8 in CpuFeatures then Include(fInstructions, isCX8);
 if cfSEP in CpuFeatures then
  begin
   Include(fInstructions, isSEP);
   if (fVendor = cvIntel) and (fSignature and $0FFF3FFF < IntelLowestSEPSupportSignature)
    then Exclude(fInstructions, isSEP);
  end;
 if cfCMOV in CpuFeatures then Include(fInstructions, isCMOV);
 if cfFXSR in CpuFeatures then Include(fInstructions, isFXSR);
 if cfMMX in CpuFeatures then Include(fInstructions, isMMX);
 if cfSSE in CpuFeatures then Include(fInstructions, isSSE);
 if cfSSE2 in CpuFeatures then Include(fInstructions, isSSE2);
 if cfSSE3 in CpuFeatures then Include(fInstructions, isSSE3);
 if (fVendor = cvIntel) and (cfMON in CpuFeatures) then Include(fInstructions, isMONITOR);
 if cfCX16 in CpuFeatures then Include(fInstructions, isCX16);
end;

procedure TCPU.GetCPUExtendedFeatures;
var Registers     : TRegisters;
    CpuExFeatures : TCpuExtendedFeatureSet;
begin
 GetCPUID($80000001, Registers);
 CPUExFeatures := TCPUExtendedFeatureSet(Registers.EDX);
 if cefLM in CpuExFeatures then Include(fInstructions, isX64);
 if cefExMMX in CpuExFeatures then Include(fInstructions, isExMMX);
 if cefEx3DNow in CpuExFeatures then Include(fInstructions, isEx3DNow);
 if cef3DNow in CpuExFeatures then Include(fInstructions, is3DNow);
end;

procedure TCPU.GetProcessorCacheInfo;
type TConfigDescriptor = packed array[0..15] of Byte;
var Registers  : TRegisters;
    i,j        : Integer;
    QueryCount : Byte;
begin
 GetCPUID($00000002, Registers);
 QueryCount := Registers.EAX and $FF;
 for i := 1 to QueryCount do
  begin
   for j := 1 to 15 do
    case TConfigDescriptor(Registers)[j] of
      $06: fCodeL1CacheSize := 8;
      $08: fCodeL1CacheSize := 16;
      $0A: fDataL1CacheSize := 8;
      $0C: fDataL1CacheSize := 16;
      $22: fL3CacheSize := 512;
      $23: fL3CacheSize := 1024;
      $25: fL3CacheSize := 2048;
      $29: fL3CacheSize := 4096;
      $2C: fDataL1CacheSize := 32;
      $30: fCodeL1CacheSize := 32;
      $39: fL2CacheSize := 128;
      $3B: fL2CacheSize := 128;
      $3C: fL2CacheSize := 256;
      $40: if fL2CacheSize <> 0 then fL3CacheSize := 0;
      $41: fL2CacheSize := 128;
      $42: fL2CacheSize := 256;
      $43: fL2CacheSize := 512;
      $44: fL2CacheSize := 1024;
      $45: fL2CacheSize := 2048;
      $60: fDataL1CacheSize := 16;
      $66: fDataL1CacheSize := 8;
      $67: fDataL1CacheSize := 16;
      $68: fDataL1CacheSize := 32;
      $70: if not (fVendor in [cvCyrix, cvNSC]) then fCodeL1CacheSize := 12;
      $71: fCodeL1CacheSize := 16;
      $72: fCodeL1CacheSize := 32;
      $78: fL2CacheSize := 1024;
      $79: fL2CacheSize := 128;
      $7A: fL2CacheSize := 256;
      $7B: fL2CacheSize := 512;
      $7C: fL2CacheSize := 1024;
      $7D: fL2CacheSize := 2048;
      $7F: fL2CacheSize := 512;
      $80: if fVendor in [cvCyrix, cvNSC] then
            begin
             fCodeL1CacheSize := 8;
             fDataL1CacheSize := 8;
            end;
      $82: fL2CacheSize := 256;
      $83: fL2CacheSize := 512;
      $84: fL2CacheSize := 1024;
      $85: fL2CacheSize := 2048;
      $86: fL2CacheSize := 512;
      $87: fL2CacheSize := 1024;
     end;
    if i < QueryCount then GetCPUID(2, Registers);
  end;
end;

procedure TCPU.GetExtendedProcessorCacheInfo;
var Registers: TRegisters;
begin
 GetCPUID($80000005, Registers);
 if not (fVendor in [cvIntel, cvCyrix]) then
  begin
   fCodeL1CacheSize := Registers.EDX shr 24;
   fDataL1CacheSize := Registers.ECX shr 24;
  end;
 GetCPUID($80000006, Registers);
 if (fVendor = cvAMD) and (fSignature and $FFF = K7DuronA0Signature)
  then fL2CacheSize := 64
  else if (fVendor = cvCentaur) and (fEffFamily = 6) and (fEffModel in [C3Samuel2EffModel, C3EzraEffModel])
   then fL2CacheSize := Registers.ECX shr 24
   else fL2CacheSize := Registers.ECX shr 16;
end;

procedure TCPU.VerifyOSSupportForXMMRegisters;
begin
 try
  asm
   DB $0F, $54, $C0
  end
 except
  on E: Exception do
   begin
    Exclude(fInstructions, isSSE);
    Exclude(fInstructions, isSSE2);
    Exclude(fInstructions, isSSE3);
   end;
 end;
end;

procedure TCPU.GetCPUInfo;
var Registers : TRegisters;
    MaxCPUID   : Cardinal;
    MaxExCPUID : Cardinal;
begin
//   FillChar(fCPU, SizeOf(fCPU), 0);
 try
  if not IsCPUID_Available then
   if IsFPU_Available then Include(fInstructions, isFPU) else
  else
   begin
    GetCPUID($00000000, Registers);
    MaxCPUID := Registers.EAX;
    GetCPUVendor;
    if MaxCPUID >= $00000001 then GetCPUFeatures;
    if MaxCPUID >= $00000002 then GetProcessorCacheInfo;
    GetCPUID($80000000, Registers);
    MaxExCPUID := Registers.EAX;
    if MaxExCPUID >= $80000001 then GetCPUExtendedFeatures;
    if isSSE in fInstructions then VerifyOSSupportForXMMRegisters;
    if MaxExCPUID >= $80000006 then GetExtendedProcessorCacheInfo;
    end;
  except
    on E: Exception do raise;
  end;
end;
{$ENDIF}

function TCustomVstParameterProperty.GetShortLabel: string;
begin
 Result:=fShortLabel;
end;

procedure TCustomVstParameterProperty.SetShortLabel(const Value: string);
begin
 fShortLabel:=Value;
end;

function KeyCodeToInteger(VKC:TVstKeyCode):Integer;
begin
 if (VKC.character=0) then
  begin
{$IFNDEF FPC}
   case VKC.virt of
    VKEY_BACK: Result:=VK_BACK;
    VKEY_TAB: Result:=VK_TAB;
    VKEY_CLEAR: Result:=VK_CLEAR;
    VKEY_RETURN: Result:=VK_RETURN;
    VKEY_PAUSE: Result:=VK_PAUSE;
    VKEY_ESCAPE: Result:=VK_ESCAPE;
    VKEY_SPACE: Result:=VK_SPACE;
    VKEY_NEXT: Result:=VK_NEXT;
    VKEY_END: Result:=VK_END;
    VKEY_HOME: Result:=VK_HOME;
    VKEY_LEFT: Result:=VK_LEFT;
    VKEY_UP: Result:=VK_UP;
    VKEY_RIGHT: Result:=VK_RIGHT;
    VKEY_DOWN: Result:=VK_DOWN;
    VKEY_PAGEUP: Result:=VK_UP;
    VKEY_PAGEDOWN: Result:=VK_DOWN;
    VKEY_SELECT: Result:=VK_SELECT;
    VKEY_PRINT: Result:=VK_PRINT;
    VKEY_ENTER: Result:=VK_RETURN;
    VKEY_SNAPSHOT: Result:=VK_SNAPSHOT;
    VKEY_INSERT: Result:=VK_INSERT;
    VKEY_DELETE: Result:=VK_DELETE;
    VKEY_HELP: Result:=VK_HELP;
    VKEY_NUMPAD0: Result:=48; //VK_NUMPAD0;
    VKEY_NUMPAD1: Result:=49; //VK_NUMPAD1;
    VKEY_NUMPAD2: Result:=50; //VK_NUMPAD2;
    VKEY_NUMPAD3: Result:=51; //VK_NUMPAD3;
    VKEY_NUMPAD4: Result:=52; //VK_NUMPAD4;
    VKEY_NUMPAD5: Result:=53; //VK_NUMPAD5;
    VKEY_NUMPAD6: Result:=54; //VK_NUMPAD6;
    VKEY_NUMPAD7: Result:=55; //VK_NUMPAD7;
    VKEY_NUMPAD8: Result:=56; //VK_NUMPAD8;
    VKEY_NUMPAD9: Result:=57; //VK_NUMPAD9;
    VKEY_MULTIPLY: Result:=VK_MULTIPLY;
    VKEY_ADD: Result:=VK_ADD;
    VKEY_SEPARATOR: Result:=VK_SEPARATOR;
    VKEY_SUBTRACT: Result:=VK_SUBTRACT;
    VKEY_DECIMAL: Result:=VK_DECIMAL;
    VKEY_DIVIDE: Result:=VK_DIVIDE;
    VKEY_F1: Result:=VK_F1;
    VKEY_F2: Result:=VK_F2;
    VKEY_F3: Result:=VK_F3;
    VKEY_F4: Result:=VK_F4;
    VKEY_F5: Result:=VK_F5;
    VKEY_F6: Result:=VK_F6;
    VKEY_F7: Result:=VK_F7;
    VKEY_F8: Result:=VK_F8;
    VKEY_F9: Result:=VK_F9;
    VKEY_F10: Result:=VK_F10;
    VKEY_F11: Result:=VK_F11;
    VKEY_F12: Result:=VK_F12;
    VKEY_NUMLOCK: Result:=VK_NUMLOCK;
    VKEY_SCROLL: Result:=VK_SCROLL;
    VKEY_SHIFT: Result:=VK_SHIFT;
    VKEY_CONTROL: Result:=VK_CONTROL;
    VKEY_ALT: Result:=VK_MENU;
    VKEY_EQUALS: Result:=$5D;
    else Result:=VKC.character;
   end;
{$ENDIF}
  end
 else
  begin
   Result:=VKC.character;
   if ((VKC.modifier and MODIFIER_SHIFT)<>0)
    then Dec(Result,32);
//   if Result=1 then Result:=VK_BACK;
(*   ShowMessage({'Character: '+Char(VKC.character)+#13#10+}
               'Integer: '+IntToStr(VKC.character)+#13#10+
               'Virt: '+IntToStr(VKC.virt)+#13#10+
               'Mod: '+IntToStr(VKC.modifier)); *)
  end;
end;

procedure TCustomVSTModule.MIDI_Out(b1, b2, b3, b4: byte; offset: Integer);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := b1;
   MidiData[1] := b2;
   MidiData[2] := b3;
   MidiData[3] := b4;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_CC(ch, num, val: Integer; offset: Integer = 0);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $B0 + ch;
   MidiData[1] := num;
   MidiData[2] := val;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_ChannelAftertouch(ch, val: Integer; offset: Integer = 0);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $D0 + ch;
   MidiData[1] := val;
   MidiData[2] := 0;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_NoteOff(ch, note, val: Integer; offset: Integer = 0);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $80 + ch;
   MidiData[1] := note;
   MidiData[2] := val;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_NoteOn(ch, note, val: Integer; offset: Integer = 0);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $90 + ch;
   MidiData[1] := note;
   MidiData[2] := val;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_PitchBend(ch, val: Integer; offset: Integer = 0);
var a, b: Integer;
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   a := (val div 128) + 64;
   b := (val div 128);
   b := val - b * 128;
   MidiData[0] := $E0 + ch;
   MidiData[1] := b;
   MidiData[2] := a;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_PitchBend2(ch, x1, x2: Integer; offset: Integer = 0);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $E0 + ch;
   MidiData[1] := x1;
   MidiData[2] := x2;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_PolyAftertouch(ch, note, val: Integer; offset: Integer = 0);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $A0 + ch;
   MidiData[1] := note;
   MidiData[2] := val;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_ProgramChange(ch, val: Integer; offset: Integer = 0);
begin
 with PVstMidiEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etMidi;
   MidiData[0] := $D0 + ch;
   MidiData[1] := val;
   MidiData[2] := 0;
   DeltaFrames := offset;
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_SendSysEx(Data: array of byte; offset: Integer);
begin
 with PVstMidiSysexEvent(FMidiEvent.events[FMidiEvent.numEvents])^ do
  begin
   EventType := etSysEx;
   DeltaFrames := offset;
   dumpBytes := Length(Data);
   GetMem(sysexDump, dumpBytes);
   Move(Data[0], sysexDump^, dumpBytes);
   if FMidiEvent.numEvents < maxMidiEvents - 1 then inc(FMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.wantEvents(filter: Integer);
begin
 if Assigned(FAudioMaster) then FAudioMaster(@FEffect, audioMasterWantMidi, 0, filter, nil, 0);
end;

function TCustomVSTModule.GetTimeInfo(filter: Integer): PVstTimeInfo;
begin
 if Assigned(FAudioMaster)
  then Result := PVstTimeInfo(FAudioMaster (@FEffect, audioMasterGetTime, 0, filter, nil, 0))
  else Result := nil;
end;

procedure TCustomVSTModule.SetTimeInfo(filter: Integer; ti: PVstTimeInfo);
begin
 if Assigned(FAudioMaster)
  then FAudioMaster(@FEffect, audioMasterSetTime, 0, filter, ti, 0);
end;

function TCustomVSTModule.tempoAt(pos: Integer): Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterTempoAt, 0, pos, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.sendVstEventsToHost(events: PVstEvents): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterProcessEvents, 0, 0, events, 0) = 1
  else Result := False;
end;

{ parameters }

function TCustomVSTModule.GetNumAutomatableParameters: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetNumAutomatableParameters, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.GetParameterQuantization: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetParameterQuantization, 0, 0, nil, 0)
  else Result := 0;
end;

{ configuration }

function TCustomVSTModule.IOChanged: Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterIOChanged, 0, 0, nil, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.needIdle: Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterNeedIdle, 0, 0, nil, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.sizeWindow(width, height: Integer): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterSizeWindow, width, height, nil, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.UpdateSampleRate: Double;
var i : Integer;
begin
 {$IFDEF Debug} FLog.Add('Update Samplerate'); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if Assigned(FAudioMaster) then
  begin
   i := FAudioMaster(@FEffect, audioMasterGetSampleRate, 0, 0, nil, 0);
   if (i > 0) then
    begin
     fSampleRate := i;
     if Assigned(OnSampleRateChange) then OnSampleRateChange(Self, sampleRate);
    end;
  end;
 Result := sampleRate;
end;

function TCustomVSTModule.UpdateBlockSize: Integer;
var i : Integer;
begin
 if Assigned(FAudioMaster) then
  begin
   i := FAudioMaster(@FEffect, audioMasterGetBlockSize, 0, 0, nil, 0);
   if (i > 0) then
    begin
     FBlockSize := i;
     if Assigned(OnBlockSizeChange) then OnBlockSizeChange(Self, fBlockSize);
    end;
  end;
 Result := BlockSize;
end;

function TCustomVSTModule.GetInputLatency: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetInputLatency, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.GetOutputLatency: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetOutputLatency, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.GetPreviousPlug(input: Integer): PVSTEffect;
begin
 if Assigned(FAudioMaster)
  then Result := PVSTEffect(FAudioMaster(@FEffect, audioMasterGetPreviousPlug, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.GetNextPlug(output: Integer): PVSTEffect;
begin
 if Assigned(FAudioMaster)
  then Result := PVSTEffect(FAudioMaster(@FEffect, audioMasterGetNextPlug, 0, 0, nil, 0))
  else Result := nil;
end;

{ configuration }

function TCustomVSTModule.willProcessReplacing: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterWillReplaceOrAccumulate, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.GetCurrentProcessLevel: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetCurrentProcessLevel, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.GetAutomationState: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetAutomationState, 0, 0, nil, 0)
  else Result := 0;
end;

{ offline }

function TCustomVSTModule.OfflineRead(Offline: PVstOfflineTask; Option: TVstOfflineOption; readSource: Boolean): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterOfflineRead, Integer(readSource), Integer(option), offline, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.OfflineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterOfflineWrite, 0, Integer(option), offline, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.OfflineStart(ptr: PVstAudioFile; numAudioFiles: Integer; numNewAudioFiles: Integer): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterOfflineStart, numNewAudioFiles, numAudioFiles, ptr, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.OfflineGetCurrentPass: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := Integer(FAudioMaster(@FEffect, audioMasterOfflineGetCurrentPass, 0, 0, nil, 0) <> 0)
  else Result := 0;
end;

function TCustomVSTModule.OfflineGetCurrentMetaPass: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := Integer(FAudioMaster(@FEffect, audioMasterOfflineGetCurrentMetaPass, 0, 0, nil, 0) <> 0)
  else Result := 0;
end;

procedure TCustomVSTModule.SetOutputSampleRate(sampleRate: Single);
begin
 if Assigned(FAudioMaster)
  then FAudioMaster(@FEffect, audioMasterSetOutputSampleRate, 0, 0, nil, sampleRate);
end;

function TCustomVSTModule.GetSpeakerArrangement(var pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean;
begin
 pluginInput := nil;
 pluginOutput := nil;
 Result := False;
end;

function TCustomVSTModule.GetHostVendorString(Text: pchar): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterGetVendorString, 0, 0, Text, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.GetHostProductString(Text: pchar): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterGetProductString, 0, 0, Text, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.GetHostVendorVersion: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetVendorVersion, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.HostVendorSpecific(lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer;
begin
 Result := 0;
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterVendorSpecific, lArg1, lArg2, ptrArg, floatArg);
end;

function TCustomVSTModule.CanHostDo(Text: pchar): Integer;
begin
 Result := 0;
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterCanDo, 0, 0, Text, 0);
end;

function TCustomVSTModule.GetHostLanguage: Integer;
begin
 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, audioMasterGetLanguage, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.OpenWindow(aWindow: PVstWindow): pointer;
begin
 if Assigned(FAudioMaster)
  then Result := pointer(FAudioMaster(@FEffect, audioMasterOpenWindow, 0, 0, aWindow, 0))
  else Result := nil;
end;

function TCustomVSTModule.CloseWindow(aWindow: PVstWindow): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterCloseWindow, 0, 0, aWindow, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.GetDirectory: pointer;
begin
 if Assigned(FAudioMaster)
  then Result := pointer(FAudioMaster(@FEffect, audioMasterGetDirectory, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.UpdateDisplay: Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterUpdateDisplay, 0, 0, nil, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.ProcessEvents(events: PVstEvents): Integer;
var i: Integer;
begin
 for i := 0 to events^.numEvents - 1 do
  if (events^.events[i]^.EventType = etMidi) then
   if Assigned(fProcessMidi) then fProcessMidi(Self, PVstMidiEvent(events^.events[i])^);
 Result := 1;
end;

function TCustomVSTModule.CanParameterBeAutomated(Index: Integer): Boolean;
begin
 if Index < ParameterProperties.Count
  then Result := ParameterProperties[Index].CanBeAutomated
  else Result := True
end;

function TCustomVSTModule.String2parameter(Index: Integer; Text: pchar): Boolean;
var tmp : string;
begin
 Result := False;
 if Text <> nil then
  try
   tmp := Text;
   Parameter[Index] := StrtoFloat(tmp);
   Result := True;
  except
  end;
end;

function TCustomVSTModule.GetChannelParameter(channel, Index: Integer): Single;
begin
 Result := 0;
end;

function TCustomVSTModule.GetProgramNameIndexed(category, Index: Integer; Text: pchar): Boolean;
begin
 Result := false;
 if (Index < FEffect.numPrograms) and not (Index<0) then
  begin
   StrPCopy(Text,Programs[Index].DisplayName);
   Result := True;
  end;
end;

function TCustomVSTModule.CopyProgram(destination: Integer): Boolean;
begin
 Result := False; //ToDo
end;

function TCustomVSTModule.GetInputProperties(Index: Integer; Properties: PVstPinProperties): Boolean;
var str1  : string[63];
    str2  : string[7];
    sat   : TVstSpeakerArrangementType;
    cpf   : TChannelPropertyFlags;
begin
 Result := false;
 if (Index < FEffect.numInputs) then
 begin
  str1:='Input #' + IntToStr(Index + 1);
  str2:='In' + IntToStr(Index + 1);
  sat:=satStereo;
  cpf:=[cpfIsActive,cpfIsStereo];
  if Assigned(FOnGetInputProperties) then FOnGetInputProperties(Self,str1,str2,sat,cpf);
  StrPCopy(Properties^.Caption, str1); // set name of input channel:
  StrPCopy(Properties^.ShortLabel, str2); // set name of input channel:
  if cpfIsActive in cpf then Properties^.Flags := [vppIsActive] else Properties^.Flags := [];
  if cpfIsStereo in cpf then Properties^.Flags := Properties^.Flags + [vppIsStereo];
  if cpfUseSpeaker in cpf then Properties^.Flags := Properties^.Flags + [vppUseSpeaker];

  Result := True;
 end;
end;

function TCustomVSTModule.GetOutputProperties(Index: Integer; Properties: PVstPinProperties): Boolean;
var str1  : string[63];
    str2  : string[7];
    sat   : TVSTSpeakerArrangementType;
    cpf   : TChannelPropertyFlags;
begin
 Result := false;
 if (Index < FEffect.numOutputs) then
 begin
  str1:='Output #' + IntToStr(Index + 1);
  str2:='Out' + IntToStr(Index + 1);
  sat:=satStereo;
  cpf:=[cpfIsActive,cpfIsStereo];
  if Assigned(FOnGetOutputProperties) then FOnGetOutputProperties(Self,str1,str2,sat,cpf);
  StrPCopy(Properties^.Caption, str1); // set name of input channel:
  StrPCopy(Properties^.shortLabel, str2); // set name of input channel:
  if cpfIsActive in cpf then Properties^.Flags := [vppIsActive] else Properties^.Flags := [];
  if cpfIsStereo in cpf then Properties^.Flags := Properties^.Flags + [vppIsStereo];
  if cpfUseSpeaker in cpf then Properties^.Flags := Properties^.Flags + [vppUseSpeaker];
  Result := True;
 end;
end;

function TCustomVSTModule.reportCurrentPosition: Integer;
begin
 Result := 0;
end;

function TCustomVSTModule.reportDestinationBuffer: PSingle;
begin
 Result := nil;
end;

function TCustomVSTModule.offlineGetNumPasses: Integer;
begin
  Result := 0;
end;

function TCustomVSTModule.offlineGetNumMetaPasses: Integer;
begin
  Result := 0;
end;

function TCustomVSTModule.SetSpeakerArrangement(pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean;
begin
  Result := False;
end;

procedure TCustomVSTModule.SetBlockSizeAndSampleRate(aBlockSize: Integer; aSampleRate: Single);
begin
 {$IFDEF Debug} FLog.Add('Set BlockSize/Samplerate: Blocksize ' + IntToStr(aBlockSize) + ' Samplerate ' + FloatToStr(aSampleRate)); FLog.SaveToFile('Debug.log'); {$ENDIF}
 if fSampleRate<>aSampleRate then
  begin
   fSampleRate := aSampleRate;
   if Assigned(fSampleRateChangeEvent) then fSampleRateChangeEvent(Self,aSampleRate);
  end;
 if fBlockSize<>aBlockSize then
  begin
   fBlockSize := aBlockSize;
   if Assigned(fBlockSizeChangeEvent) then fBlockSizeChangeEvent(Self,aBlockSize);
  end;
end;

function TCustomVSTModule.SetBypass(onOff: Boolean): Boolean;
begin
 if Assigned(FOnSoftBypass) then
  begin
   FOnSoftBypass(Self, onOff);
   Result := True;
  end
 else Result := False;
end;

function TCustomVSTModule.GetEffectName(AName: pchar): Boolean;
begin
 StrPCopy(AName, FEffectName);
 Result := True;
end;

function TCustomVSTModule.GetErrorText(Text: pchar): Boolean;
begin
  Result := False;
end;

function TCustomVSTModule.GetVendorString(Text: pchar): Boolean;
begin
 StrPCopy(Text, fVendorName);
 Result := True;
end;

function TCustomVSTModule.GetProductString(Text: pchar): Boolean;
begin
  if fProductName <> '' then
    StrPCopy(Text, fProductName)
  else
    StrPCopy(Text, FEffectName);
  Result := True;
end;

function TCustomVSTModule.GetVendorVersion: Integer;
begin
 Result := FEffect.Version;
end;

function TCustomVSTModule.canDo(Text: pchar): Integer;
begin
 Result := 0;
 if StrComp(Text, 'receiveVstEvents') = 0 then Result := 2*Integer(vcdReceiveVstEvents in fCanDos)-1 else
 if StrComp(Text, 'receiveVstMidiEvent') = 0 then Result := 2*Integer(vcdReceiveVstMidiEvent in fCanDos)-1 else
 if StrComp(Text, 'receiveVstTimeInfo') = 0 then Result := 2*Integer(vcdReceiveVstTimeInfo in fCanDos)-1 else
 if StrComp(Text, 'sendVstEvents') = 0 then Result := 2*Integer(vcdSendVstEvents in fCanDos)-1 else
 if StrComp(Text, 'sendVstMidiEvent') = 0 then Result := 2*Integer(vcdSendVstMidiEvent in fCanDos)-1 else
 if StrComp(Text, 'sendVstTimeInfo') = 0 then Result := 2*Integer(vcdSendVstTimeInfo in fCanDos)-1 else
 if StrComp(Text, 'offline') = 0 then Result := 2*Integer(vcdOffline in fCanDos)-1 else
 if StrComp(Text, 'plugAsChannelInsert') = 0 then Result := 2*Integer(vcdPlugAsChannelInsert in fCanDos)-1 else
 if StrComp(Text, 'plugAsSend') = 0 then Result := 2*Integer(vcdPlugAsSend in fCanDos)-1 else
 if StrComp(Text, 'mixDryWet') = 0 then Result := 2*Integer(vcdMixDryWet in fCanDos)-1 else
 if StrComp(Text, 'noRealTime') = 0 then Result := 2*Integer(vcdNoRealTime in fCanDos)-1 else
 if StrComp(Text, 'multipass') = 0 then Result := 2*Integer(vcdMultipass in fCanDos)-1 else
 if StrComp(Text, 'metapass') = 0 then Result := 2*Integer(vcdMetapass in fCanDos)-1 else
 if StrComp(Text, '1in1out') = 0 then Result := 2*Integer(vcd1in1out in fCanDos)-1 else
 if StrComp(Text, '1in2out') = 0 then Result := 2*Integer(vcd1in2out in fCanDos)-1 else
 if StrComp(Text, '2in1out') = 0 then Result := 2*Integer(vcd2in1out in fCanDos)-1 else
 if StrComp(Text, '2in2out') = 0 then Result := 2*Integer(vcd2in2out in fCanDos)-1 else
 if StrComp(Text, '2in4out') = 0 then Result := 2*Integer(vcd2in4out in fCanDos)-1 else
 if StrComp(Text, '4in2out') = 0 then Result := 2*Integer(vcd4in2out in fCanDos)-1 else
 if StrComp(Text, '4in4out') = 0 then Result := 2*Integer(vcd4in4out in fCanDos)-1 else
 if StrComp(Text, '4in8out') = 0 then Result := 2*Integer(vcd4in8out in fCanDos)-1 else
 if StrComp(Text, '8in4out') = 0 then Result := 2*Integer(vcd8in4out in fCanDos)-1 else
 if StrComp(Text, '8in8out') = 0 then Result := 2*Integer(vcd8in8out in fCanDos)-1 else
 if StrComp(Text, 'midiProgramNames') = 0 then Result := 2*Integer(vcdMidiProgramNames in fCanDos)-1 else
 if StrComp(Text, 'conformsToWindowRules') = 0 then Result := 2*Integer(vcdConformsToWindowRules in fCanDos)-1 else
 if StrComp(Text, 'LiveWithoutToolbar') = 0 then Result := 2*Integer(vcdLiveWithoutToolbar in fCanDos)-1 else
 if StrComp(Text, 'bypass') = 0 then Result := 2*Integer(vcdBypass in fCanDos)-1;
 if Assigned(FOnCanDo) then FOnCanDo(Self,Text);
end;

function TCustomVSTModule.GetIcon: pointer;
begin
 Result := nil;
end;

function TCustomVSTModule.SetViewPosition(x, y: Integer): Boolean;
begin
 Result := False;
end;

function TCustomVSTModule.fxIdle: Integer;
begin
 Result := 0;
end;

function TCustomVSTModule.GetParameterProperties(Index: Integer; p: PVstParameterProperties): Boolean;
var str: string;
begin
 Result := ParameterProperties[Index].ReportVST2Properties;
 if Result then
  begin
   StrCopy(p^.Caption,@ParameterProperties[Index].DisplayName[1]);
   str:=ParameterProperties[Index].ShortLabel;
   StrCopy(p^.shortLabel,@str);
   p^.minInteger:=ParameterProperties[Index].MinInteger;
   p^.maxInteger:=ParameterProperties[Index].MaxInteger;
   p^.stepInteger:=ParameterProperties[Index].StepInteger;
   p^.largeStepInteger:=ParameterProperties[Index].LargeStepInteger;
   p^.stepFloat:=ParameterProperties[Index].StepFloat;
   p^.largeStepFloat:=ParameterProperties[Index].LargeStepFloat;
   p^.smallStepFloat:=ParameterProperties[Index].SmallStepFloat;
   p^.displayIndex:=0;
   p^.Flags:=ParameterProperties[Index].Flags;
  end;
end;

function TCustomVSTModule.GetMidiProgramName(channel: Integer; midiProgramName: PMidiProgramName): Integer;
//var MPN: TMidiProgramName;
begin
// MPN.thisProgramIndex:=CurrentProgram;
// StrCopy(@MPN.name,@CurrentProgram);
// MPN.midiProgram:=CurrentProgram;
 Result := 0;
end;

function TCustomVSTModule.GetCurrentMidiProgram(channel: Integer; currentProgram: PMidiProgramName): Integer;
begin
 Result := -1;
end;

function TCustomVSTModule.GetMidiProgramCategory(channel: Integer; category: PMidiProgramCategory): Integer;
begin
 Result := 0;
end;

function TCustomVSTModule.hasMidiProgramsChanged(channel: Integer): Boolean;
begin
 Result := False;
end;

function TCustomVSTModule.GetMidiKeyName(channel: Integer; keyName: PMidiKeyName): Boolean;
begin
 Result := False;
end;

function TCustomVSTModule.beginEdit(Index: Integer): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterBeginEdit, Index, 0, nil, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.endEdit(Index: Integer): Boolean;
begin
 if Assigned(FAudioMaster)
  then Result := (FAudioMaster(@FEffect, audioMasterEndEdit, Index, 0, nil, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.openFileSelector(ptr: PVstFileSelect): Boolean;
begin
 if Assigned(FAudioMaster) and (ptr <> nil)
  then Result := (FAudioMaster(@FEffect, audioMasterOpenFileSelector, 0, 0, ptr, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.closeFileSelector(ptr: PVstFileSelect): Boolean;
begin
 if Assigned(FAudioMaster) and (ptr <> nil)
  then Result := (FAudioMaster(@FEffect, audioMasterCloseFileSelector, 0, 0, ptr, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.GetChunkFile(nativePath: pointer): Boolean;
begin
 if Assigned(FAudioMaster) and (nativePath <> nil)
  then Result := (FAudioMaster(@FEffect, audioMasterGetChunkFile, 0, 0, nativePath, 0) <> 0)
  else Result := False;
end;

function TCustomVSTModule.GetInputSpeakerArrangement: PVstSpeakerArrangement;
begin
 if Assigned(FAudioMaster)
  then Result := PVstSpeakerArrangement(FAudioMaster(@FEffect, audioMasterGetInputSpeakerArrangement, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.GetOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
 if Assigned(FAudioMaster)
  then Result := PVstSpeakerArrangement(FAudioMaster(@FEffect, audioMasterGetOutputSpeakerArrangement, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.SetTotalSampleToProcess(Value: Integer): Integer;
begin
 Result := Value;
end;

function TCustomVSTModule.GetNextShellPlugin(const AName: pchar): Integer;
begin
 if FCurrentVstShellPlugin<FVstShellPlugins.Count then
  begin
   StrPCopy(AName,FVstShellPlugins[FCurrentVstShellPlugin].DisplayName);
   Result:=FVstShellPlugins[FCurrentVstShellPlugin].UID;
   Inc(FCurrentVstShellPlugin);
  end
 else
  begin
   Result := 0;
   FCurrentVstShellPlugin := 0;
  end;
end;

function TCustomVSTModule.StartProcess: Integer;
begin
 Result := 1;
 if Assigned(FOnStartProcess)
  then FOnStartProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.StopProcess: Integer;
begin
 Result := 1;
 if Assigned(FOnStopProcess)
  then FOnStopProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.SetPanLaw(var vType: Integer; var val: single): Boolean;
begin
 Result := True;
 if Assigned(FOnSetPanLaw)
  then FOnSetPanLaw(Self, vType, val)
  else Result := false;
end;

function TCustomVSTModule.BeginLoadBank(ptr: PVstPatchChunkInfo): Integer;
begin
 if ptr^.pluginUniqueID<>FEffect.uniqueID
  then Result := -1
  else Result := 0;
 if Assigned(FOnBeginLoadBank)
  then FOnBeginLoadBank(Self, ptr^)
end;

function TCustomVSTModule.BeginLoadProgram(ptr: PVstPatchChunkInfo): Integer;
begin
 if ptr^.pluginUniqueID<>FEffect.uniqueID
  then Result := -1
  else Result := 0;
 if Assigned(FOnBeginLoadProgram)
  then FOnBeginLoadProgram(Self, ptr^)
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

{$WARNINGS ON}

{ TCustomVstShellPlugin }

{$IFDEF FPC}
constructor TCustomVstShellPlugin.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstShellPlugin.Create(Collection: TCollection);
{$ENDIF}
begin
 inherited;
 FDisplayName  := 'Init'; // inherited GetDisplayName;
 FNumInputs    := -1;
 FNumOutputs   := -1;
 FNumPrograms  := -1;
 FNumParams    := -1;
 FPlugCategory := vpcUnknown;
 FVSTModule    := (Collection As TCustomVstShellPlugins).VSTModule;
end;

destructor TCustomVstShellPlugin.Destroy;
begin
 inherited;
end;

procedure TCustomVstShellPlugin.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstShellPlugin then
  with TCustomVstShellPlugin(Dest)
   do DisplayName := Self.DisplayName
  else inherited;
end;

function TCustomVstShellPlugin.GetUniqueID:string;
var i : Integer;
begin
 Result := '';
 for i := 3 downto 0
  do Result := Result + char(UID shr (i * 8));
end;

procedure TCustomVstShellPlugin.SetUniqueID(fID:string);
begin
 UID := FourCharToLong(fID[1], fID[2], fID[3], fID[4])
end;

procedure TCustomVstShellPlugin.SetDisplayName(const AValue: string);
begin
 FDisplayName := Copy(AValue,0,50);
end;

function TCustomVstShellPlugin.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

{ TCustomVstShellPlugins }

constructor TCustomVstShellPlugins.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TCustomVstShellPlugin);
 FVSTModule := TCustomVSTModule(AOwner);
end;

destructor TCustomVstShellPlugins.Destroy;
begin
 while Count>0 do Delete(0);
 inherited;
end;

function TCustomVstShellPlugins.Add: TCustomVstShellPlugin;
begin
 Result := TCustomVstShellPlugin(inherited Add);
end;

function TCustomVstShellPlugins.Insert(Index: Integer): TCustomVstShellPlugin;
begin
 Result := TCustomVstShellPlugin(inherited Insert(Index));
end;

procedure TCustomVstShellPlugins.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TCustomVstShellPlugins.GetItem(Index: Integer): TCustomVstShellPlugin;
begin
 Result := TCustomVstShellPlugin(inherited GetItem(Index));
end;

procedure TCustomVstShellPlugins.SetItem(Index: Integer; const Value: TCustomVstShellPlugin);
begin
 inherited SetItem(Index, Value);
end;

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
 if FHostProduct='' then
  begin
   Getmem(Text, 64);
   try
    if GetHostProductString(Text)
     then Result:=shortstring(Text)
     else Result:='Unknown';
   finally
    FreeMem(Text);
    FHostProduct:=Result;
   end
  end
 else Result:=FHostProduct;
end;

function TCustomVSTModule.GetHostVendor: string;
var Text : pchar;
begin
 Getmem(Text, 64);
 try
  if GetHostVendorString(Text)
   then Result:=shortstring(Text)
   else Result:='Unknown';
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

initialization
//  Set8087CW(Default8087CW or $3F);
{$IFDEF FPC}
  RegisterInitComponentHandler(TCustomVSTModule,@InitResourceComponent);
  RegisterInitComponentHandler(TVSTModule,@InitResourceComponent);
{$ENDIF}

end.
