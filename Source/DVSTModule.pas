unit DVSTModule;

interface

{$I JEDI.INC}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LResources, LCLClasses, LMessages, RtlConsts,
  {$ELSE} Windows, Messages, {$ENDIF} SysUtils, Forms, Classes,
  DDSPBase, DVSTEffect;

{$DEFINE _Debug}

type
  TChannelPropertyFlags = set of (cpfIsActive, cpfIsStereo, cpfUseSpeaker);

  TParameterChangeEvent = procedure(Sender: TObject; const Index: Integer; var Value: Single) of object;
  TBlockSizeChangeEvent = procedure(Sender: TObject; const BlockSize: Integer) of object;
  TSampleRateChangeEvent = procedure(Sender: TObject; const SampleRate: Single) of object;
  TProcessAudioEvent = procedure(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer) of object;
  TProcessDoubleEvent = procedure(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer) of object;
  TOnDispatcherEvent = procedure (Sender: TObject; opCode: Integer) of object;
  TGetVUEvent = procedure(var VU:Single) of object;
  TGetEditorEvent = procedure(Sender: TObject; var GUI: TForm) of object;
  TChunkEvent = procedure(Sender: TObject; const Index : Integer; const isPreset : Boolean) of object;
  TGetChunkParameterEvent = function(Sender: TObject; const Index: Integer): Single of object;
  TProcessMidiEvent = procedure(Sender: TObject; MidiEvent: TVstMidiEvent) of object;
  TSoftBypassEvent = procedure(Sender: TObject; onOff: Boolean) of object;
  TInOutConnectedEvent = procedure(Sender: TObject; Index: Integer; State: Boolean) of object;
  TSetKnobModeEvent = procedure (Sender: TObject; val: integer) of object;
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

  TVstCanDo = (sendVstEvents, sendVstMidiEvent, sendVstTimeInfo,
   receiveVstEvents, receiveVstMidiEvent, receiveVstTimeInfo,
   offline, plugAsChannelInsert, plugAsSend, mixDryWet, noRealTime,
   multipass, metapass, _1in1out, _1in2out, _2in1out, _2in2out,
   _2in4out, _4in2out, _4in4out, _4in8out, _8in4out, _8in8out,
   midiProgramNames, LiveWithoutToolbar, conformsToWindowRules, bypass);
  TVstCanDos = set of TVstCanDo;

  TVstPluginCategory = (cgUnknown, cgEffect, cgSynth, cgAnalysis,
   cgMastering, cgSpacializer, cgRoomFx, cgSurroundFx, cgRestoration,
   cgOfflineProcess, cgShell, cgGenerator);

  TParameterPropertiesFlag = (ppfIsSwitch, ppfUsesIntegerMinMax, ppfUsesFloatStep,
                              ppfUsesIntStep, ppfSupportsDisplayIndex,
                              ppfSupportsDisplayCategory, ppfCanRamp);
  TParameterPropertiesFlags = set of TParameterPropertiesFlag;


  TProcessingMode = (pmNormal, pmBlockSave, pmCopy, pmMute);

  TCurveType = (ctLinear, ctLogarithmic, ctExponential, ctFrequencyScale);

  TCPUVendor = (cvUnknown, cvAMD, cvCentaur, cvCyrix, cvIntel, cvTransmeta,
                cvNexGen, cvRise, cvUMC, cvNSC, cvSiS);

  TCPUInstruction = (isFPU, isTSC, isCX8, isSEP, isCMOV, isMMX, isFXSR, isSSE, isSSE2, isSSE3,
                     isMONITOR, isCX16, isX64, isExMMX, isEx3DNow, is3DNow);

  TCPUInstructions = set of TCPUInstruction;

  {$IFNDEF FPC}
  TCPU = class(TPersistent)
  private
    fVendor           : TCPUVendor;
    fSignature        : Cardinal;
    fEffFamily        : Byte;
    fEffModel         : Byte;
    fCodeL1CacheSize,
    fDataL1CacheSize,
    fL2CacheSize,
    fL3CacheSize      : Word;
    fInstructions     : TCPUInstructions;
    procedure GetCPUInfo;
    procedure GetCPUVendor;
    procedure GetCPUFeatures;
    procedure GetCPUExtendedFeatures;
    procedure GetProcessorCacheInfo;
    procedure GetExtendedProcessorCacheInfo;
    procedure VerifyOSSupportForXMMRegisters;
  public
    constructor Create;
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
    i1, i2            : Single;
    fMin, fMax        : Single;
    fCurve            : TCurveType;
    fCurveFactor      : Single;
    fDisplayName      : string;
    fUnits            : {$IFNDEF FPC}string{$ELSE}ansistring{$ENDIF};
    fSmoothingFactor  : Single;
    fCanBeAutomated   : Boolean;
    fV2Properties     : Boolean;
    fStepFloat        : Single;
    fSmallStepFloat   : Single;
    fLargeStepFloat   : Single;
    fFlags            : TParameterPropertiesFlags;
    fMinInteger       : Integer;
    fMaxInteger       : Integer;
    fStepInteger      : Integer;
    fLargeStepInteger : Integer;
    fCC               : Integer;
    fShortLabel       : string[7];

    fVSTModule        : TCustomVSTModule;
    fOnSPC            : TParameterChangeEvent;
    fOnCPL            : TCustomParameterLabelEvent;
    fOnCPD            : TCustomParameterDisplayEvent;
    function Smooth(i: Single): Single;
    function GetShortLabel: string;
    procedure SetShortLabel(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    
{$IFNDEF FPC}
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
    procedure SetUnits(AUnits: string);
{$ELSE}
    procedure SetDisplayName(const AValue: ansistring); override;
    function GetDisplayName: ansistring; override;
    procedure SetUnits(AUnits: ansistring);
{$ENDIF}
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
  published
    property Min: Single read fMin write fMin;
    property Max: Single read fMax write fMax;
    property CC: Integer read fCC write fCC default -1;
    property Curve: TCurveType read fCurve write fCurve;
    property DisplayName{$IFNDEF FPC}: string read fDisplayName write SetDisplayName{$ENDIF};
    property Units: {$IFNDEF FPC}string{$ELSE}ansistring{$ENDIF} read fUnits write SetUnits;
    property CurveFactor: Single read fCurveFactor write fCurveFactor;
    property SmoothingFactor: Single read fSmoothingFactor write fSmoothingFactor;
    property CanBeAutomated: Boolean read fCanBeAutomated write fCanBeAutomated;
    property ReportVST2Properties: Boolean read fV2Properties write fV2Properties;
    property StepFloat: Single read fStepFloat write fStepFloat;
    property SmallStepFloat: Single read fSmallStepFloat write fSmallStepFloat;
    property LargeStepFloat: Single read fLargeStepFloat write fLargeStepFloat;
    property Flags: TParameterPropertiesFlags read fFlags write fFlags;
    property MinInteger: Integer read fMinInteger write fMinInteger;
    property MaxInteger: Integer read fMaxInteger write fMaxInteger;
    property StepInteger: Integer read fStepInteger write fStepInteger;
    property LargeStepInteger: Integer read fLargeStepInteger write fLargeStepInteger;
    property ShortLabel: string read GetShortLabel write SetShortLabel;
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
    property OnParameterChange: TParameterChangeEvent read fOnSPC write fOnSPC;
    property OnCustomParameterLabel: TCustomParameterLabelEvent read fOnCPL write fOnCPL;
    property OnCustomParameterDisplay: TCustomParameterDisplayEvent read fOnCPD write fOnCPD;
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
    fDisplayName      : string;
    fVSTModule        : TCustomVSTModule;
    fOnInitialize     : TNotifyEvent;
    fOnStoreChunk     : TChunkEvent;
    fOnLoadChunk      : TChunkEvent;

    procedure SetParameter(AIndex: Integer; s: Single);
    function GetParameter(AIndex: Integer): Single;
  protected
    fParameter        : array of Single;
    fChunkData        : TMemoryStream;
    procedure AssignTo(Dest: TPersistent); override;
    {$IFNDEF FPC}
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
    {$ELSE}
    procedure SetDisplayName(const AValue: ansistring); override;
    function GetDisplayName: ansistring; override;
    {$ENDIF}
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    property Parameter[ndx: integer]: Single read GetParameter write SetParameter;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    property Parameter[AIndex: integer]: Single read GetParameter write SetParameter;
    {$ENDIF}
    destructor Destroy; override;
    property Chunk: TMemoryStream read fChunkData write fChunkData;
  published
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
    property OnInitialize: TNotifyEvent read fOnInitialize write fOnInitialize;
    property OnLoadChunk: TChunkEvent read fOnLoadChunk write fOnLoadChunk;
    property OnStoreChunk: TChunkEvent read fOnStoreChunk write fOnStoreChunk;
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
    fDisplayName      : string;
    fNumInputs        : Integer;
    fNumOutputs       : Integer;
    fNumParams        : Integer;
    fNumPrograms      : Integer;
    fPlugCategory     : TVstPluginCategory;
    fVSTModule        : TCustomVSTModule;
    fOnInstanciate    : TUIDInstantiateEvent;
    procedure SetUniqueID(fID: String);
    function GetUniqueID: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    {$IFNDEF FPC}
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
    {$ELSE}
    procedure SetDisplayName(const AValue: ansistring); override;
    function GetDisplayName: ansistring; override;
    {$ENDIF}
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
    property numInputs: Integer read fNumInputs write fNumInputs;
    property numOutputs: Integer read fNumOutputs write fNumOutputs;
    property numParams: Integer read fNumParams write fNumParams;
    property numPrograms: Integer read fNumPrograms write fNumPrograms;
    property PlugCategory: TVstPluginCategory read fPlugCategory write fPlugCategory;
    property UniqueID: string read GetUniqueID write SetUniqueID;
    property VSTModule: TCustomVSTModule read FVSTModule write FVSTModule;
    property OnInstanciate: TUIDInstantiateEvent read fOnInstanciate write fOnInstanciate;
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

  TCustomVSTModule = class(TDataModule)
  private
    fParameterUpdate        : Boolean;
    FAbout                  : string;
    FVersion                : string;
    {$IFDEF DELPHI6_UP}
    fCPU                    : TCPU;
    {$ENDIF}

    fEditorRect             : ERect;
    fEditorNeedUpdate       : Boolean;

    fCurProgram             : Integer;
    fVstPrograms            : TCustomVstPrograms;
    fParameter              : array of Single;
    fChunkData              : TMemoryStream;
    fParameterProperties    : TCustomVstParameterProperties;

    fOnEditClose            : TNotifyEvent;
    fOnEditIdle             : TNotifyEvent;
    fOnEditTop              : TNotifyEvent;
    fOnEditSleep            : TNotifyEvent;
    fOnProcess              : TProcessAudioEvent;
    fOnProcessEx            : TProcessAudioEvent;
    fOnProcessReplacing     : TProcessAudioEvent;
    fOnProcessReplacingEx   : TProcessAudioEvent;
    fOnProcessDoubles       : TProcessDoubleEvent;
    fOnProcessDoublesEx     : TProcessDoubleEvent;
    fOnInitialize           : TNotifyEvent;
    fOnResume               : TNotifyEvent;
    fOnSuspend              : TNotifyEvent;
    fOnBeforeProgramChange  : TNotifyEvent;
    fOnAfterProgramChange   : TNotifyEvent;
    fOnParameterSizeFailed  : TNotifyEvent;
    fOnGetVUEvent           : TGetVUEvent;
    fOnParameterChangeEvent : TParameterChangeEvent;
    fBlockSizeChangeEvent   : TBlockSizeChangeEvent;
    fSampleRateChangeEvent  : TSampleRateChangeEvent;
    fOnDispatcher           : TOnDispatcherEvent;
    fProcessingMode         : TProcessingMode;
    fBlockInBuffer          : TArrayOfSingleDynArray;
    fBlockOutBuffer         : TArrayOfSingleDynArray;
    fBlockPosition          : Integer;
    fBlockModeSize          : Integer;
    fBlockModeOverlap       : Integer;
    fInitialDelay           : Integer;
    fTempo                  : Single;
    fTailSize               : integer;
    fCanDos                 : TVstCanDos;
    fPlugCategory           : TVstPluginCategory;
    fMidiEvent              : TVstEvents;
    fkeysRequired           : Boolean;

    fOnGetChunkParamEvent   : TGetChunkParameterEvent;
    fOnOfflineNotify        : TOfflineNotifyEvent;
    fOnOfflinePrepare       : TOfflinePrepareEvent;
    fOnOfflineRun           : TOfflineRunEvent;
    fOnProcessVarIO         : TProcessVarIOEvent;
    fOnKeyUp                : TVSTKeyEvent;
    fOnKeyDown              : TVSTKeyEvent;
    fOnSetKnobMode          : TSetKnobModeEvent;
    fOnInConnected          : TInOutConnectedEvent;
    fOnOutConnected         : TInOutConnectedEvent;
    fOnSoftBypass           : TSoftBypassEvent;
    fNumCategories          : Integer;
    fProcessMidi            : TProcessMidiEvent;
    fOnStartProcess         : TNotifyEvent;
    fOnStopProcess          : TNotifyEvent;
    fOnSetPanLaw            : TOnSetPanLawEvent;
    fOnBeginLoadBank        : TOnBeginLoadBankEvent;
    fOnBeginLoadProgram     : TOnBeginLoadProgramEvent;
    fOnBeginSetProgram      : TNotifyEvent;
    fOnEndSetProgram        : TNotifyEvent;
    fOnVendorSpecific       : TOnVendorSpecificEvent;
    fOnCanDo                : TOnCanDoEvent;
    fOnGetInputProperties   : TOnGetChannelPropertiesEvent;
    fOnGetOutputProperties  : TOnGetChannelPropertiesEvent;
    fOnCheckKey             : TOnCheckKey;
    fVstShellPlugins        : TCustomVstShellPlugins;
    fCurrentVstShellPlugin  : Integer;
    {$IFDEF Debug} fLog     : TStringList; {$ENDIF}
    {$IFDEF Debug} fTmStmp  : TDateTime; {$ENDIF}

    procedure SetVstPrograms(const Value: TCustomVstPrograms);
    procedure SetParameterProperties(const Value : TCustomVstParameterProperties);
    procedure setParameterAutomated(index: Integer; value: Single);
    procedure SetVstShellPlugins(const Value: TCustomVstShellPlugins);
    procedure SetKeysRequired(const Value: Boolean);
    function EditorOpen(ptr: pointer): Integer; virtual;
    procedure EditorClose; virtual;
    procedure EditorIdle; virtual;
    procedure ReadOnlyString(s: string); virtual;
    procedure fOnBlockSaveProcess(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer); overload;
    procedure fOnBlockSaveProcessReplacing(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer); overload;
    procedure fOnProcessCopy(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer); overload;
    procedure fOnProcessMute(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer); overload;
    procedure fOnBlockSaveProcess(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer); overload;
    procedure fOnBlockSaveProcessReplacing(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer); overload;
    procedure fOnProcessCopy(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer); overload;
    procedure fOnProcessMute(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer); overload;
    function GetHostProduct: string;
    function GetHostVendor: string;
  protected
    fEffect                 : TVSTEffect;
    fOnOpen                 : TNotifyEvent;
    fOnClose                : TNotifyEvent;
    fOnEditOpen             : TGetEditorEvent;
    fEditorForm             : TForm;
    fAudioMaster            : TAudioMasterCallbackFunc;
    fSampleRate             : Single;
    fBlockSize              : Integer;
    fEffectName             : string;
    fVendorName             : string;
    fVersionMajor           : Integer;
    fVersionMinor           : Integer;
    fVersionRelease         : Integer;
    fProductName            : string;
    fIsHostAutomation       : Boolean;
    fHostProduct            : string;
    function Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
    function VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
    function GetEffect: PVSTEffect;
    function Dispatcher(opcode, index, value: Integer; ptr: pointer; opt: Single): Integer; virtual;
    procedure SetAudioMaster(const AM :TAudioMasterCallbackFunc); virtual;
    procedure setNumInputs(inputs: Integer); virtual;
    procedure setNumOutputs(outputs: Integer); virtual;
    procedure SetSampleRate(newvalue: Single); virtual;
    procedure SetBlockSize(newvalue: Integer); virtual;
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
    procedure getParameterLabel(index: Integer; text: pchar); virtual;
    procedure getParameterDisplay(index: Integer; text: pchar); virtual;
    procedure getParameterName(index: Integer; text: pchar); virtual;
    procedure ReadState(Reader: TReader); override;
    procedure SetOnProcess(v : TProcessAudioEvent);
    procedure SetOnProcessReplacing(v : TProcessAudioEvent);
    procedure SetOnProcessDoubleReplacing(v : TProcessDoubleEvent);
    procedure SetProcessingMode(v : TProcessingMode);
    procedure PrepareBlockProcessing; virtual;
    procedure SetBlockForcedSize(v: Integer); virtual;
    procedure SetBlockOverlapSize(v: Integer); virtual;
    procedure setParameter(const index: Integer; value: Single); virtual;
    function getParameter(index: Integer): Single; virtual;
    function ProcessEvents(events: PVstEvents): Integer; virtual;  // wants no more...else return 1! VstEvents and VstMidiEvents are declared in aeffectx.h
    procedure SetEffectName(const Value: string); virtual;
    procedure SetVersionMajor(Value: Integer);
    procedure SetVersionMinor(Value: Integer);
    procedure SetVersionRelease(Value: Integer);
    procedure UpdateVersion;

    // Host -> Plug
    function canParameterBeAutomated(Index: Integer): Boolean; virtual;
    function string2parameter(Index: Integer; text: pchar): Boolean; virtual; // note: implies setParameter. text==0 is to be
    function getChannelParameter(channel, Index: Integer): Single; virtual;
    function getProgramNameIndexed(category, Index: Integer; text: pchar): Boolean; virtual;
    function copyProgram(destination: Integer): Boolean; virtual; // expected to check the capability (returns true).

    // Host -> Plug
    function getInputProperties(Index: Integer; properties: PVstPinProperties): Boolean; virtual;
    function getOutputProperties(Index: Integer; properties: PVstPinProperties): Boolean; virtual;

    // realtime
    procedure wantEvents(filter: Integer);  // filter is currently ignored, midi channel data only (default) virtual void wantEvents (long filter = 1); default is 1 for this!
    function getTimeInfo(filter: Integer): PVstTimeInfo; virtual;  // returns const VstTimeInfo* (or 0 if not supported) filter should contain a mask indicating which fields are requested (see valid masks in aeffectx.h), as some items may require extensive conversions
    procedure setTimeInfo(filter: Integer; ti: PVstTimeInfo); virtual;
    function tempoAt(pos: Integer): Integer; virtual; // returns tempo (in bpm * 10000) at sample frame location <pos>
    function sendVstEventsToHost(events: PVstEvents): Boolean;  // true: success

    // Plug -> Host
    function willProcessReplacing: Integer; virtual; // returns 0: not implemented, 1: replacing, 2: accumulating
    function getCurrentProcessLevel: Integer; virtual;  // returns: 0: not supported, 1: currently in user thread (gui) 2: currently in audio thread or irq (where process is called) 3: currently in 'sequencer' thread or irq (midi, timer etc) 4: currently offline processing and thus in user thread other: not defined, but probably pre-empting user thread.
    function getAutomationState: Integer; virtual;  // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

    // Host -> Plug
    function reportCurrentPosition: Integer; virtual;  // for external dsp, see wantAsyncOperation ()
    function reportDestinationBuffer: PSingle; virtual;  // for external dsp (dma option)

    // offline
    // Plug -> Host
    function offlineRead(offline: PVstOfflineTask; option: TVstOfflineOption; readSource: Boolean): Boolean; virtual;
    function offlineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): Boolean; virtual;
    function offlineStart(ptr: PVstAudioFile; numAudioFiles: Integer; numNewAudioFiles: Integer): Boolean; virtual;
    function offlineGetCurrentPass: Integer; virtual;
    function offlineGetCurrentMetaPass: Integer; virtual;

    function offlineGetNumPasses: Integer; virtual;
    function offlineGetNumMetaPasses: Integer; virtual;

    // other
    // Plug -> Host
    procedure setOutputSampleRate(samplerate: Single); virtual;
    function getInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function getOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function getHostVendorString(text: pchar): Boolean; virtual;  // fills <text> with a string identifying the vendor (max 64 char)
    function getHostProductString(text: pchar): Boolean; virtual; // fills <text> with a string with product name (max 64 char)
    function getHostVendorVersion: Integer; virtual;  // returns vendor-specific version
    function hostVendorSpecific(lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer; virtual;  // no definition
    function canHostDo(text: pchar): Integer; virtual;  // see 'hostCanDos' in audioeffectx.cpp returns 0: don't know (default), 1: yes, -1: no
    function getHostLanguage: Integer; virtual;   // returns VstHostLanguage
    function openWindow(aWindow: PVstWindow): pointer; virtual;  // create new window
    function closeWindow(aWindow: PVstWindow): Boolean; virtual; // close a newly created window
    function getDirectory: pointer; virtual;  // get the plug's directory, FSSpec on mac, else char*

    // Host -> Plug
    function setSpeakerArrangement(pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean; virtual;
    function getSpeakerArrangement(var pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean; virtual;
    procedure setBlockSizeAndSampleRate(aBlockSize: Integer; aSampleRate: Single); virtual;
    function setBypass(onOff: Boolean): Boolean; virtual; // for 'soft-bypass; process() still called
    function getEffectName(AName: pchar): Boolean; virtual;  // name max 32 char
    function getErrorText(text: pchar): Boolean; virtual;  // max 256 char
    function getVendorString(text: pchar): Boolean; virtual;  // fill text with a string identifying the vendor (max 64 char)
    function getProductString(text: pchar): Boolean; virtual; // fills text with a string with product name (max 64 char)
    function getVendorVersion: Integer; virtual;
    function canDo(text: pchar): Integer; virtual; // see 'plugCanDos' in audioeffectx.cpp. return values: 0: don't know (default), 1: yes, -1: no
    function getIcon: pointer; virtual;  // not yet defined
    function setViewPosition(x, y: Integer): Boolean; virtual;
    function fxIdle: Integer; virtual;
    function getParameterProperties(Index: Integer; p: PVstParameterProperties): Boolean; virtual;

    // midi program names, are always defined per channel, valid channels are 0 - 15
    // Host -> Plug
    function getMidiProgramName(channel: Integer; midiProgramName: PMidiProgramName): Integer; virtual; {return 0;} // Struct will be filled with information for 'thisProgramIndex'. Returns number of used programIndexes. If 0 is returned, no MidiProgramNames supported.
    function getCurrentMidiProgram(channel: Integer; currentProgram: PMidiProgramName): Integer; virtual; {return -1;} // returns the programIndex of the current program. -1 means not supported. Struct will be filled with information for the current program.
    function getMidiProgramCategory(channel: Integer; category: PMidiProgramCategory): Integer; virtual; {return 0;} // Struct will be filled with information for 'thisCategoryIndex'. Returns number of used categoryIndexes. If 0 is returned, no MidiProgramCategories supported/ used.
    function hasMidiProgramsChanged(channel: Integer): Boolean; virtual; {return false;} // returns true if the MidiProgramNames, MidiKeyNames or MidiControllerNames had changed on this channel.
    function getMidiKeyName(channel: Integer; keyName: PMidiKeyName): Boolean; virtual; {return false;} // Struct will be filled with information for 'thisProgramIndex' and 'thisKeyNumber' if keyName is "" the standard name of the key will be displayed. If false is returned, no MidiKeyNames defined for 'thisProgramIndex'.

    // Plug -> Host
    function beginEdit(Index: Integer): Boolean; virtual;  // to be called before a setParameterAutomated with mouse move (one per Mouse Down)
    function endEdit(Index: Integer): Boolean; virtual;    // to be called after a setParameterAutomated (on Mouse Up)

    function openFileSelector(ptr: PVstFileSelect): Boolean; virtual;
    function closeFileSelector(ptr: PVstFileSelect): Boolean;
    function getChunkFile(nativePath: pointer): Boolean;

    // Host -> Plug
    function setTotalSampleToProcess(value: Integer): Integer; virtual;   // Called in offline (non RealTime) Process before process is called, indicates how many sample will be processed
    function getNextShellPlugin(const AName: pchar): Integer; virtual;           // This opcode is only called, if Plugin is of type kPlugCategShell. Should return the next plugin's uniqueID. name points to a char buffer of size 64, which is to be filled with the name of the plugin including the terminating zero.
    function startProcess: Integer; virtual;                              // Called one time before the start of process call
    function stopProcess: Integer; virtual;                               // Called after the stop of process call
    function setPanLaw(var vType: Integer; var val: single): Boolean; virtual;    // Set the Panning Law used by the Host
    function beginLoadBank(ptr: PVstPatchChunkInfo): Integer; virtual;    // Called before a Bank is loaded. returns -1 if the Bank cannot be loaded, returns 1 if it can be loaded else 0 (for compatibility)
    function beginLoadProgram(ptr: PVstPatchChunkInfo): Integer; virtual; // Called before a Program is loaded. (called before beginSetProgram) returns -1 if the Program cannot be loaded, returns 1 if it can be loaded else 0 (for compatibility)

    // Tools
    function allocateArrangement(var arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean; virtual;   // Allocate memory for a VstSpeakerArrangement containing the given number of channels
    function deallocateArrangement(var arrangement: PVstSpeakerArrangement): Boolean; virtual;                      // Delete/free memory for a speaker arrangement
    function copySpeaker(copyTo, copyFrom: PVstSpeakerProperties): Boolean; virtual;    // Feed the "to" speaker properties with the same values than "from"'s ones. It is assumed here that "to" exists yet, ie this function won't allocate memory for the speaker (this will prevent from having a difference between an Arrangement's number of channels and its actual speakers...)
    function matchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): Boolean; virtual;    // "to" is deleted, then created and initialized with the same values as "from" ones ("from" must exist).
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure EditorPostUpdate; virtual;
    procedure Process(inputs, outputs: PPSingle; sampleFrames: Integer); virtual;
    procedure ProcessReplacing(inputs, outputs: PPSingle; sampleFrames: Integer); virtual;
    procedure ProcessDoubleReplacing(inputs, outputs: PPDouble; sampleFrames: Integer); virtual;
    function getChunk(var data: pointer; isPreset: Boolean): Integer; virtual;   // returns byteSize
    function setChunk(data: pointer; byteSize: Integer; isPreset: Boolean): Integer; virtual;

    // host communication
    function getMasterVersion: Integer; virtual;
    function getCurrentUniqueID: Integer; virtual;
    procedure masterIdle; virtual;
    function isInputConnected(input: Integer): Boolean; virtual;
    function isOutputConnected(output: Integer): Boolean; virtual;

    procedure MIDI_Out(b1, b2, b3, b4: byte; offset: integer = 0);
    procedure MIDI_CC(ch, num, val: integer; offset: integer = 0);
    procedure MIDI_ChannelAftertouch(ch, val: integer; offset: integer = 0);
    procedure MIDI_NoteOff(ch, note, val: integer; offset: integer = 0);
    procedure MIDI_NoteOn(ch, note, val: integer; offset: integer = 0);
    procedure MIDI_PitchBend(ch, val: integer; offset: integer = 0);
    procedure MIDI_PitchBend2(ch, x1, x2: integer; offset: integer = 0);
    procedure MIDI_PolyAftertouch(ch, note, val: integer; offset: integer = 0);
    procedure MIDI_ProgramChange(ch, val: integer; offset: integer = 0);

    // Plug -> Host
    function getNumAutomatableParameters: Integer; virtual;
    function getParameterQuantization: Integer; virtual; // returns the integer value for +1.0 representation, or 1 if full single float precision is maintained in automation. parameter Index in <value> (-1: all, any)

    // Plug -> Host
    function ioChanged: Boolean; virtual;   // tell host numInputs and/or numOutputs and/or numParameters has changed
    function needIdle: Boolean; virtual;    // plug needs idle calls (outside its editor window)
    function sizeWindow(width, height: Integer): Boolean; virtual;
    function updateSampleRate: Double; virtual;  // gets and returns sample rate from host (may issue setSampleRate() )
    function updateBlockSize: Integer; virtual;  // same for block size
    function getInputLatency: Integer; virtual;
    function getOutputLatency: Integer; virtual;
    function getPreviousPlug(input: Integer): PVSTEffect; virtual;  // input can be -1 in which case the first found is returned
    function getNextPlug(output: Integer): PVSTEffect; virtual;     // output can be -1 in which case the first found is returned
    function updateDisplay: Boolean; virtual; // something has changed, update 'multi-fx' display returns true if supported

    // properties
    property EditorForm: TForm read fEditorForm;
    property EditorNeedUpdate: Boolean read fEditorNeedUpdate write fEditorNeedUpdate;

    property AudioMaster: TAudioMasterCallbackFunc read faudioMaster write SetAudioMaster;
    property Flags: TEffFlags read GetPluginFlags write SetPluginFlags;
    property OnParameterChange: TParameterChangeEvent read fOnParameterChangeEvent write fOnParameterChangeEvent;
    property Effect: PVSTEffect read GetEffect;

    property SampleRate: Single read fSampleRate write SetSampleRate;
    property BlockSize: Integer read fBlockSize write SetBlockSize default 1024;
    property BlockModeSize: Integer read fBlockModeSize write SetBlockForcedSize default 1024;
    property BlockModeOverlap: Integer read fBlockModeOverlap write SetBlockOverlapSize default 0;
    property numInputs: Integer read fEffect.numInputs write SetNumInputs default 2;
    property numOutputs: Integer read fEffect.numOutputs write SetNumOutputs default 2;
    property numParams: Integer read fEffect.numParams write SetNumParams stored false;
    property numPrograms: Integer read fEffect.numPrograms write SetNumPrograms stored false;
    property CurrentProgram: Integer read fCurProgram write SetProgram;
    property CurrentProgramName: string read GetCurrentProgramName write SetCurrentProgramName;
    property InitialDelay: Integer read fEffect.initialDelay write SetInitialDelay default 0;
    property ProcessingMode: TProcessingMode read fProcessingMode write SetProcessingMode default pmNormal;
    property RealQualities: Integer read fEffect.realQualities write fEffect.realQualities default 0;
    property OffQualities: Integer read fEffect.offQualities write fEffect.offQualities default 0;
    property IORatio: Integer read fEffect.ioRatio write fEffect.ioRatio default 1;
    property About: string read FAbout write ReadOnlyString stored False;
    {$IFDEF DELPHI6_UP} property CPU: TCPU read fCPU; {$ENDIF}
    property Version: string read FVersion write FVersion;
    property UniqueID: string read GetUniqueID write setUniqueID;
    property Parameter[index: Integer]: Single read getParameter write setParameterAutomated;
    property Chunk: TMemoryStream read fChunkData write fChunkData;
    property Programs: TCustomVstPrograms read FVstPrograms write SetVstPrograms;
    property ParameterProperties: TCustomVstParameterProperties read FParameterProperties write SetParameterProperties;
    property OldCreateOrder;
    property numCategories: Integer read fNumCategories write fNumCategories;
    property EffectName: string read fEffectName write SetEffectName;
    property ProductName: string read fProductName write fProductName;
    property VendorName: string read fVendorName write fVendorName;
    property VersionMajor: Integer read fVersionMajor write SetVersionMajor;
    property VersionMinor: Integer read fVersionMinor write SetVersionMinor;
    property VersionRelease: Integer read fVersionRelease write SetVersionRelease;
    property PlugCategory: TVstPluginCategory read fPlugCategory write fPlugCategory;
    property KeysRequired: Boolean read fkeysRequired write SetKeysRequired;
    property Tempo: Single read fTempo;
    property ShellPlugins: TCustomVstShellPlugins read FVstShellPlugins write SetVstShellPlugins;
    property TailSize: integer read fTailSize write fTailSize;
    property CanDos: TVstCanDos read fCanDos write fCanDos;
    property OnCreate;
    property OnDestroy;
    property OnOpen: TNotifyEvent read fOnOpen write fOnOpen;
    property OnClose: TNotifyEvent read fOnClose write fOnClose;
    property OnResume: TNotifyEvent read fOnResume write fOnResume;
    property OnSuspend: TNotifyEvent read fOnSuspend write fOnSuspend;
    property OnEditOpen: TGetEditorEvent read fOnEditOpen write fOnEditOpen;
    property OnEditClose: TNotifyEvent read fOnEditClose write fOnEditClose;
    property OnEditIdle: TNotifyEvent read fOnEditIdle write fOnEditIdle;
    property OnEditTop: TNotifyEvent read fOnEditTop write fOnEditTop;
    property OnEditSleep: TNotifyEvent read fOnEditSleep write fOnEditSleep;
    property OnParameterSizeFailed: TNotifyEvent read fOnParameterSizeFailed write fOnParameterSizeFailed;
    property OnBlockSizeChange: TBlockSizeChangeEvent read fBlockSizeChangeEvent write fBlockSizeChangeEvent;
    property OnSampleRateChange: TSampleRateChangeEvent read fSampleRateChangeEvent write fSampleRateChangeEvent;
    property OnGetVU: TGetVUEvent read fOnGetVUEvent write fOnGetVUEvent;
    property OnProcess: TProcessAudioEvent read fOnProcess write SetOnProcess;
    property OnProcessReplacing: TProcessAudioEvent read fOnProcessReplacing write SetOnProcessReplacing;
    property OnProcessDoubleReplacing: TProcessDoubleEvent read fOnProcessDoubles write SetOnProcessDoubleReplacing;
    property OnInitialize: TNotifyEvent read fOnInitialize write fOnInitialize;
    property OnBeforeProgramChange: TNotifyEvent read fOnBeforeProgramChange write fOnBeforeProgramChange;
    property OnAfterProgramChange: TNotifyEvent read fOnAfterProgramChange write fOnAfterProgramChange;
    property OnDispatcher: TOnDispatcherEvent read fOnDispatcher write fOnDispatcher;
    property OnGetChunkParameter: TGetChunkParameterEvent read fOnGetChunkParamEvent write fOnGetChunkParamEvent;
    property OnSoftBypass: TSoftBypassEvent read fOnSoftBypass write fOnSoftBypass;
    property OnProcessMidi: TProcessMidiEvent read fProcessMidi write fProcessMidi;
    property OnInConnected: TInOutConnectedEvent read fOnInConnected write fOnInConnected;
    property OnOutConnected: TInOutConnectedEvent read fOnOutConnected write fOnOutConnected;
    property OnStartProcess: TNotifyEvent read fOnStartProcess write fOnStartProcess;
    property OnStopProcess: TNotifyEvent read fOnStopProcess write fOnStopProcess;
    property OnEditorKeyUp: TVSTKeyEvent read fOnKeyUp write fOnKeyUp;
    property OnEditorKeyDown: TVSTKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnEditorKnobMode: TSetKnobModeEvent read fOnSetKnobMode write fOnSetKnobMode;
    property OnOfflineNotify: TOfflineNotifyEvent read fOnOfflineNotify write fOnOfflineNotify;
    property OnOfflinePrepare: TOfflinePrepareEvent read fOnOfflinePrepare write fOnOfflinePrepare;
    property OnOfflineRun: TOfflineRunEvent read fOnOfflineRun write fOnOfflineRun;
    property OnProcessVarIO: TProcessVarIOEvent read fOnProcessVarIO write fOnProcessVarIO;
    property OnSetPanLaw: TOnSetPanLawEvent read fOnSetPanLaw write fOnSetPanLaw;
    property OnBeginSetProgram: TNotifyEvent read fOnBeginSetProgram write fOnBeginSetProgram;
    property OnEndSetProgram: TNotifyEvent read fOnEndSetProgram write fOnEndSetProgram;
    property OnVendorSpecific: TOnVendorSpecificEvent read fOnVendorSpecific write fOnVendorSpecific;
    property OnCanDo: TOnCanDoEvent read fOnCanDo write fOnCanDo;
    property OnCheckKey: TOnCheckKey read fOnCheckKey write fOnCheckKey;
    property OnInputProperties: TOnGetChannelPropertiesEvent read fOnGetInputProperties write fOnGetInputProperties;
    property OnOutputProperties: TOnGetChannelPropertiesEvent read fOnGetOutputProperties write fOnGetOutputProperties;
    property OnBeginLoadBank: TOnBeginLoadBankEvent read fOnBeginLoadBank write fOnBeginLoadBank;
    property OnBeginLoadProgram: TOnBeginLoadProgramEvent read fOnBeginLoadProgram write fOnBeginLoadProgram;
    property HostProduct: string read GetHostProduct stored false;
    property HostVendor: string read GetHostVendor stored false;
    property HostVersion: Integer read getHostVendorVersion stored false;
  end;

  TVSTModule = class(TCustomVSTModule)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Flags;
    property About;
   {$IFDEF DELPHI6_UP} property CPU; {$ENDIF}
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
//    property numCategories;
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
    property OldCreateOrder;
    property OnCreate;
    property OnDestroy;
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

function opcode2String(opcode: integer): string;
function dispatchEffectClass(effect: PVSTEffect; opcode, index, value: Integer; ptr: pointer; opt: Single): Integer; cdecl;
function getParameterClass(effect: PVSTEffect; index: Integer): Single; cdecl;
procedure setParameterClass(effect: PVSTEffect; index: Integer; value: Single); cdecl;
procedure processClass(effect: PVSTEffect; inputs, outputs: PPSingle; sampleframes: Integer); cdecl;
procedure processClassReplacing(effect: PVSTEffect; inputs, outputs: PPSingle; sampleframes: Integer); cdecl;
procedure processClassDoubleReplacing(effect: PVSTEffect; inputs, outputs: PPDouble; sampleframes: Integer); cdecl;

function KeyCodeToInteger(VKC:TVstKeyCode):Integer;

{$IFDEF FPC}
function InitResourceComponent(Instance: TComponent; RootAncestor: TClass):Boolean;
{$ENDIF}

implementation

uses Controls, Math;

resourcestring
  SResNotFound = 'Resource %s not found';

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

{$IFNDEF FPC}
  IntelLowestSEPSupportSignature = $633;
  K7DuronA0Signature = $630;
  C3Samuel2EffModel = 7;
  C3EzraEffModel = 8;
  PMBaniasEffModel = 9;
  PMDothanEffModel = $D;
  P3LowestEffModel = 7;
{$ENDIF}

const maxMidiEvents = 256;

function opcode2String(opcode: integer): string;
begin
 case opcode of
  effOpen                           : result:='effOpen';
  effClose                          : result:='effClose';
  effSetProgram                     : result:='effSetProgram';
  effGetProgram                     : result:='effGetProgram';
  effSetProgramName                 : result:='effSetProgramName';
  effGetProgramName                 : result:='effGetProgramName';
  effGetParamLabel                  : result:='effGetParamLabel';
  effGetParamDisplay                : result:='effGetParamDisplay';
  effGetParamName                   : result:='effGetParamName';
  effGetVu                          : result:='effGetVu';
  effSetSampleRate                  : result:='effSetSampleRate';
  effSetBlockSize                   : result:='effSetBlockSize';
  effMainsChanged                   : result:='effMainsChanged';
  effEditGetRect                    : result:='effEditGetRect';
  effEditOpen                       : result:='effEditOpen';
  effEditClose                      : result:='effEditClose';
  effEditDraw                       : result:='effEditDraw';
  effEditMouse                      : result:='effEditMouse';
  effEditKey                        : result:='effEditKey';
  effEditIdle                       : result:='effEditIdle';
  effEditTop                        : result:='effEditTop';
  effEditSleep                      : result:='effEditSleep';
  effIdentify                       : result:='effIdentify';
  effGetChunk                       : result:='effGetChunk';
  effSetChunk                       : result:='effSetChunk';
  effProcessEvents                  : result:='effProcessEvents';
  effCanBeAutomated                 : result:='effCanBeAutomated';
  effString2Parameter               : result:='effString2Parameter';
  effGetNumProgramCategories        : result:='effGetNumProgramCategories';
  effGetProgramNameIndexed          : result:='effGetProgramNameIndexed';
  effCopyProgram                    : result:='effCopyProgram';
  effConnectInput                   : result:='effConnectInput';
  effConnectOutput                  : result:='effConnectOutput';
  effGetInputProperties             : result:='effGetInputProperties';
  effGetOutputProperties            : result:='effGetOutputProperties';
  effGetPlugCategory                : result:='effGetPlugCategory';
  effGetCurrentPosition             : result:='effGetCurrentPosition';
  effGetDestinationBuffer           : result:='effGetDestinationBuffer';
  effOfflineNotify                  : result:='effOfflineNotify';
  effOfflinePrepare                 : result:='effOfflinePrepare';
  effOfflineRun                     : result:='effOfflineRun';
  effProcessVarIo                   : result:='effProcessVarIo';
  effSetSpeakerArrangement          : result:='effSetSpeakerArrangement';
  effSetBlockSizeAndSampleRate      : result:='effSetBlockSizeAndSampleRate';
  effSetBypass                      : result:='effSetBypass';
  effGetEffectName                  : result:='effGetEffectName';
  effGetErrorText                   : result:='effGetErrorText';
  effGetVendorString                : result:='effGetVendorString';
  effGetProductString               : result:='effGetProductString';
  effGetVendorVersion               : result:='effGetVendorVersion';
  effVendorSpecific                 : result:='effVendorSpecific';
  effCanDo                          : result:='effCanDo';
  effGetTailSize                    : result:='effGetTailSize';
  effIdle                           : result:='effIdle';
  effGetIcon                        : result:='effGetIcon';
  effSetViewPosition                : result:='effSetViewPosition';
  effGetParameterProperties         : result:='effGetParameterProperties';
  effKeysRequired                   : result:='effKeysRequired';
  effGetVstVersion                  : result:='effGetVstVersion';
  effEditKeyDown                    : result:='effEditKeyDown';
  effEditKeyUp                      : result:='effEditKeyUp';
  effSetEditKnobMode                : result:='effSetEditKnobMode';
  effGetMidiProgramName             : result:='effGetMidiProgramName';
  effGetCurrentMidiProgram          : result:='effGetCurrentMidiProgram';
  effGetMidiProgramCategory         : result:='effGetMidiProgramCategory';
  effHasMidiProgramsChanged         : result:='effHasMidiProgramsChanged';
  effGetMidiKeyName                 : result:='effGetMidiKeyName';
  effBeginSetProgram                : result:='effBeginSetProgram';
  effEndSetProgram                  : result:='effEndSetProgram';
  effGetSpeakerArrangement          : result:='effGetSpeakerArrangement';
  effShellGetNextPlugin             : result:='effShellGetNextPlugin';
  effStartProcess                   : result:='effStartProcess';
  effStopProcess                    : result:='effStopProcess';
  effSetTotalSampleToProcess        : result:='effSetTotalSampleToProcess';
  effSetPanLaw                      : result:='effSetPanLaw';
  effBeginLoadBank                  : result:='effBeginLoadBank';
  effBeginLoadProgram               : result:='effBeginLoadProgram';
 end;
end;

{ TVSTModule }

{$IFNDEF FPC}
constructor TVSTModule.Create(AOwner: TComponent);
begin
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
end;

{$ELSE}
constructor TVSTModule.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  if (ClassType <> TDataModule) and
     not (csDesigning in ComponentState) then
    begin
    if not InitInheritedComponent(Self, TVSTModule) then
      raise EStreamError.CreateFmt(SErrNoSTreaming, [ClassName]);
    if OldCreateOrder then
      DoCreate;
    end;
end;

function InitResourceComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
begin
 Result:=InitLazResourceComponent(Instance,RootAncestor);
end;
{$ENDIF}

{ TCustomVST1Module }

constructor TCustomVSTModule.Create(AOwner: TComponent);
var i : Integer;
begin
 inherited CreateNew(AOwner);
 {$IFDEF Debug} fLog:=TStringList.Create; {$ENDIF}
 {$IFDEF Debug} fTmStmp:=Now; {$ENDIF}
 {$IFDEF Debug} fLog.Add('Create '+TimeToStr(fTmStmp)); {$ENDIF}
 {$IFDEF Debug} fLog.SaveToFile('Debug.log'); {$ENDIF}
 FVersion := '0.0';
 FAbout := 'VST Plugin Wizard by Christian Budde & Tobybear';
 Randomize;
 fEditorForm := nil;
 fCurProgram := -1;
 fEffect.vObject := Self;
 fEffect.magic := 'PtsV';
 fEffect.dispatcher := @dispatchEffectClass;
 fEffect.process := @processClass;
 fEffect.processReplacing := processClassReplacing;
 fEffect.processDoubleReplacing := processClassDoubleReplacing;
 fEffect.setParameter := @setParameterClass;
 fEffect.getParameter := @getParameterClass;
 fEffect.EffectFlags := [];
 fEffect.reservedForHost := nil;
 fEffect.resvd2 := 0;
 fEffect.user := nil;
 fEffect.uniqueID := FourCharToLong('N', 'o', 'E', 'f');
 fVersionMajor := 1;
 fVersionMinor := 0;
 fVersionRelease := 0;
 UpdateVersion;
 fEffect.ioRatio:= 1;
 fParameterProperties := TCustomVstParameterProperties.Create(Self);
 fVstPrograms := TCustomVstPrograms.Create(Self);
 fParameterUpdate := False;
 fEffect.numParams:=0;
 fEffect.numPrograms:=0;
 fEffect.numInputs:=2;
 fEffect.numOutputs:=2;
 fSampleRate := 44100;
 fBlockSize:=1024;
 fBlockModeSize:=1024;
 fBlockModeOverlap:=0;
 {$IFNDEF FPC}
 fCPU:=TCPU.Create;
 {$ENDIF}
 fProcessingMode:=pmNormal;
 fChunkData:=TMemoryStream.Create;
 fVstShellPlugins := TCustomVstShellPlugins.Create(Self);
 fCurrentVstShellPlugin:=0;
 fNumCategories:=1;
 fMidiEvent.numEvents := 0;
 for i := 0 to maxMidiEvents - 1 do
 begin
  GetMem(fMidiEvent.events[i], sizeof(TVstMidiEvent));
  FillChar(fMidiEvent.events[i]^, sizeof(TVstMidiEvent), 0);
  PVstMidiEvent(fMidiEvent.events[i])^.vType := 1;
  PVstMidiEvent(fMidiEvent.events[i])^.byteSize := 24;
 end;
end;

destructor TCustomVSTModule.Destroy;
var i : Integer;
begin
 try
  if assigned(fParameterProperties) then fParameterProperties.Free;
  if assigned(fVstPrograms) then fVstPrograms.Free;
  if Assigned(fEditorForm) then fEditorForm.Free;
  {$IFNDEF FPC}
  if Assigned(fCPU) then fCPU.Free;
  {$ENDIF}
  if Assigned(fChunkData) then fChunkData.Free;
  if Assigned(fVstShellPlugins) then fVstShellPlugins.Free;
  for i := 0 to maxMidiEvents - 1 do FreeMem(fMidiEvent.events[i]);
  {$IFDEF Debug} fLog.SaveToFile('Debug.log'); {$ENDIF}
 finally
  {$IFDEF Debug} fLog.Free; {$ENDIF}
  inherited;
 end;
end;

Procedure TCustomVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
var rUID : Integer;
    i,j  : Integer;
    sUID : string;
    hv   : boolean;
begin
 fAudioMaster:=AM;
 if fAudioMaster(nil, audioMasterVersion, 0, 0, nil, 0) = 0
  then raise exception.Create('AudioMaster Error');
 hv:=(HostProduct<>'WaveLab') {or (shortstring(temp)<>'energyXT')};
 if hv then hv:=(canHostDo('shellCategory')=1);

 if (PlugCategory=cgShell) and hv then
  begin
   rUID:=getCurrentUniqueId;
   if (rUID>0) then
    begin
     for i:=0 to ShellPlugins.Count-1 do
      if rUID=ShellPlugins[i].UID then Break;
     if i<ShellPlugins.Count then
      if (rUID=ShellPlugins[i].UID) then
       begin
        fEffect.uniqueID:=rUID;
        if ShellPlugins[i].fNumInputs>=0 then fEffect.numInputs:=ShellPlugins[i].fNumInputs;
        if ShellPlugins[i].fNumOutputs>=0 then fEffect.numOutputs:=ShellPlugins[i].fNumOutputs;
        if ShellPlugins[i].fNumPrograms>=0 then fEffect.numPrograms:=ShellPlugins[i].fNumPrograms;
        if ShellPlugins[i].fNumParams>=0 then fEffect.numParams:=ShellPlugins[i].fNumParams;
        fPlugCategory:=ShellPlugins[i].fPlugCategory;
        if Assigned(ShellPlugins[i].fOnInstanciate) then
         begin
          sUID := '';
          for j := 3 downto 0 do sUID := sUID + char(rUID shr (j * 8));
          ShellPlugins[i].fOnInstanciate(Self,sUID);
         end;
        ioChanged;
       end;
    end;
  end
 else
  if (PlugCategory=cgShell)
   then PlugCategory:=cgUnknown;
end;

function TCustomVSTModule.dispatcher(opcode, index, value: Integer; ptr: pointer; opt: Single): Integer;
var a,b   : Integer;
    keyCode : TVstKeyCode;
    s       : Single;
    Hndl    : THandle;
begin
 if assigned(fOnDispatcher) then fOnDispatcher(Self,opcode);
 result := 0;

 {$IFDEF Debug} fLog.Add(TimeToStr(Now-fTmStmp)+' Opcode: '+opcode2String(opcode)+' Value: '+IntToStr(value)); {$ENDIF}
 {$IFDEF Debug} fLog.SaveToFile('Debug.log'); {$ENDIF}

 case opcode of
  effOpen            : if assigned(fOnOpen) then fOnOpen(Self);
  effClose           : if assigned(fOnClose) then fOnClose(Self);
  effSetProgram      : if (value < fEffect.numPrograms) and (value >= 0) and (value <> fCurProgram)
                        then CurrentProgram:=value;
  effGetProgram      : result := fCurProgram;
  effSetProgramName  : if numPrograms>0
                        then Programs[fCurProgram].DisplayName:=string(PChar(ptr));
  effGetProgramName  : if numPrograms>0
                        then StrPCopy(ptr,Programs[fCurProgram].DisplayName)
                        else StrPCopy(ptr,'');
  effGetParamLabel   : getParameterLabel(index, ptr);
  effGetParamDisplay : getParameterDisplay(index, ptr);
  effGetParamName    : getParameterName(index, ptr);
  effSetSampleRate   : begin setSampleRate(opt); result:=1; end;
  effSetBlockSize    : setBlockSize(value);
  effMainsChanged    : if (value = 0) then Suspend else Resume;
  effGetVu           : if assigned(fOnGetVUEvent)
                        then
                         begin
                          s:=0;
                          fOnGetVUEvent(s);
                          result := round(s * 32767);
                         end
                        else result := 0;
  // editor
  effEditGetRect     : begin
                        PPERect(ptr)^:=@fEditorRect;
                        fEditorRect.top := 0;
                        fEditorRect.left := 0;

                        if assigned(fEditorForm) then
                         begin
                          fEditorRect.bottom := fEditorForm.ClientHeight;
                          fEditorRect.right := fEditorForm.ClientWidth;
                          result := 1;
                         end
                        else result := 0;
                       end;
  effEditOpen        : if (effFlagsHasEditor in fEffect.EffectFlags) then result := EditorOpen(ptr);
  effEditClose       : if (effFlagsHasEditor in fEffect.EffectFlags) then EditorClose;
  effEditIdle        : if (effFlagsHasEditor in fEffect.EffectFlags) then EditorIdle;
  effEditTop         : if assigned(fOnEditTop) then fOnEditTop(Self);
  effEditSleep       : if assigned(fOnEditSleep) then fOnEditSleep(Self);
   // new
  effIdentify        : result := FourCharToLong('N', 'v', 'E', 'f');
  effGetChunk        : result := getChunk(pointer(ptr^), (index <> 0));
  effSetChunk        : result := setChunk(ptr, value, (index <> 0));
  effProcessEvents             : result := ProcessEvents(ptr);
  effCanBeAutomated            : result := integer(canParameterBeAutomated(Index));
  effString2Parameter          : result := integer(string2parameter(Index, ptr));
  effGetNumProgramCategories   : result := fNumCategories;
  effGetProgramNameIndexed     : result := integer(getProgramNameIndexed(value, Index, ptr));
  effCopyProgram               : result := integer(copyProgram(Index));
  effConnectInput              : begin
                                  if assigned(fOnInConnected) then fOnInConnected(Self,Index,(value <> 0));
                                  result := 1;
                                 end;
  effConnectOutput             : begin
                                  if assigned(fOnOutConnected) then fOnOutConnected(Self,Index,(value <> 0));
                                  result := 1;
                                 end;
  effGetInputProperties        : result := integer(getInputProperties(Index, ptr));
  effGetOutputProperties       : result := integer(getOutputProperties(Index, ptr));
  effGetPlugCategory           : result := integer(fPlugCategory);
  effGetCurrentPosition        : result := reportCurrentPosition;
  effGetDestinationBuffer      : result := Integer(reportDestinationBuffer);
  effOfflineNotify             : if assigned(fOnOfflineNotify)
                                  then begin fOnOfflineNotify(Self, PVstAudioFile(ptr)^, value, (Index <> 0)); result := 1; end
                                  else result := 0;
  effOfflinePrepare            : if assigned(fOnOfflinePrepare)
                                  then begin fOnOfflinePrepare(Self, PVstOfflineTask(offline)^, value); result := 1; end
                                  else result := 0;
  effOfflineRun                : if assigned(fOnofflineRun)
                                  then begin fOnofflineRun(Self, PVstOfflineTask(offline)^, value); result := 1; end
                                  else result := 0;
  effSetSpeakerArrangement     : result := Integer(setSpeakerArrangement(pointer(value), ptr));
  effProcessVarIo              : if assigned(fOnProcessVarIO)
                                  then begin fOnProcessVarIO(Self, PVstVariableIo(ptr)^); result := 1; end
                                  else result := 0;
  effSetBlockSizeAndSampleRate : begin
                                  setBlockSizeAndSampleRate(value, opt);
                                  result := 1;
                                 end;
  effSetBypass                 : result := Integer(setBypass(value <> 0));
  effGetEffectName             : result := Integer(getEffectName(ptr));
  effGetErrorText              : result := Integer(getErrorText(ptr));
  effGetVendorString           : result := Integer(getVendorString(ptr));
  effGetProductString          : result := Integer(getProductString(ptr));
  effGetVendorVersion          : result := getVendorVersion;
  effVendorSpecific            : if assigned(fOnVendorSpecific) then result := fOnVendorSpecific(Self, Index, value, ptr, opt);
  effCanDo                     : result := canDo(ptr);
  effGetIcon                   : result := Integer(getIcon);
  effSetViewPosition           : result := Integer(setViewPosition(Index, value));
  effGetTailSize               : result := fTailSize;
  effIdle                      : result := fxIdle;
  effGetParameterProperties    : result := Integer(getParameterProperties(Index, ptr));
  effKeysRequired              : result := Integer(not fKeysRequired); // reversed to keep v1 compatibility
  effGetVstVersion             : result := 2400;
  effEditKeyDown               : if fKeysRequired then
                                  try
                                   keyCode.character := Index;
                                   keyCode.virt := value;
                                   keyCode.modifier := Round(opt);
                                   if Assigned(EditorForm) then
                                    begin
                                     a:=KeyCodeToInteger(keyCode);
                                     if assigned(EditorForm.ActiveControl)
                                      then Hndl:=EditorForm.ActiveControl.Handle
                                      else Hndl:=EditorForm.Handle;
{$IFNDEF FPC}
                                     if keyCode.virt=0 then b:=0 else b:=KF_EXTENDED;
                                     if (keyCode.modifier and MODIFIER_ALTERNATE)<>0
                                      then SendMessage(Hndl, WM_KEYDOWN, a,b)
                                      else SendMessage(Hndl, WM_SYSKEYDOWN, a,KF_ALTDOWN);
                                     SendMessage(Hndl,WM_CHAR, a, b);
{$ENDIF}
                                     if Assigned(fOnKeyDown) then fOnKeyDown(Self, keyCode);
                                     if Assigned(fOnCheckKey)
                                      then if fOnCheckKey(Self, Char(a))
                                            then result := 1
                                            else result := -1
                                      else result := -1;
                                    end;
                                  except
                                   result := -1;
                                  end else result := -1;
  effEditKeyUp                 : if fKeysRequired then
                                  try
                                   keyCode.character := Index;
                                   keyCode.virt := value;
                                   keyCode.modifier := Round(opt);
                                   if Assigned(EditorForm) then
                                    begin
                                     a:=KeyCodeToInteger(keyCode);
                                     if assigned(EditorForm.ActiveControl)
                                      then Hndl:=EditorForm.ActiveControl.Handle
                                      else Hndl:=EditorForm.Handle;
{$IFNDEF FPC}
                                     if keyCode.virt=0 then b:=0 else b:=KF_EXTENDED;
                                     if (keyCode.modifier and MODIFIER_ALTERNATE)<>0
                                      then SendMessage(Hndl, WM_KEYUP, a, b)
                                      else SendMessage(Hndl, WM_SYSKEYUP, a, KF_ALTDOWN);
{$ENDIF}
                                     if Assigned(fOnKeyUp) then fOnKeyUp(Self, keyCode);
                                     if Assigned(fOnCheckKey)
                                      then if fOnCheckKey(Self, Char(a))
                                            then result := 1
                                            else result := -1
                                      else result := -1;
                                    end;
                                  except
                                   result := -1;
                                  end else result := -1;
  effSetEditKnobMode          : if Assigned(fOnSetKnobMode)
                                 then begin fOnSetKnobMode(Self, value); result := 1; end
                                 else result := 0;
  effGetMidiProgramName       : result := getMidiProgramName(Index, PMidiProgramName(ptr));
  effGetCurrentMidiProgram    : result := getCurrentMidiProgram(Index, PMidiProgramName(ptr));
  effGetMidiProgramCategory   : result := getMidiProgramCategory(Index, PMidiProgramCategory(ptr));
  effHasMidiProgramsChanged   : result := Integer(hasMidiProgramsChanged(Index));
  effGetMidiKeyName           : result := Integer(getMidiKeyName(Index, PMidiKeyName(ptr)));
  effBeginSetProgram          : if Assigned(fOnBeginSetProgram)
                                 then begin fOnBeginSetProgram(Self); result := 1; end
                                 else result := 0;
  effEndSetProgram          : if Assigned(fOnEndSetProgram)
                                 then begin fOnEndSetProgram(Self); result := 1; end
                                 else result := 0;
  effGetSpeakerArrangement    : result := Integer(getSpeakerArrangement(PVstSpeakerArrangement(value), PVstSpeakerArrangement(ptr)));
  effSetTotalSampleToProcess  : result := setTotalSampleToProcess(value);
  effShellGetNextPlugin       : result := getNextShellPlugin(pchar(ptr));
  effStartProcess             : result := startProcess;
  effStopProcess              : result := stopProcess ();
  effSetPanLaw                : result := Integer(setPanLaw(value, opt));
  effBeginLoadBank            : result := beginLoadBank(PVstPatchChunkInfo(ptr));
  effBeginLoadProgram         : result := beginLoadProgram(PVstPatchChunkInfo(ptr));
  else result := 0;
 end;
end;

procedure TCustomVSTModule.SetNumParams(newNum : Integer);
begin
 if assigned(fParameterProperties)
  then fEffect.numParams:=fParameterProperties.Count
  else fEffect.numParams:=0
end;

procedure TCustomVSTModule.SetNumPrograms(newNum : Integer);
begin
 if assigned(fVstPrograms)
  then fEffect.numPrograms:=fVstPrograms.Count
  else fEffect.numPrograms:=0
end;

function TCustomVSTModule.GetUniqueID:string;
var i : Integer;
begin
 result := '';
 for i := 3 downto 0
  do result := result + char(fEffect.uniqueID shr (i * 8));
end;

procedure TCustomVSTModule.SetUniqueID(fID:string);
begin
 fEffect.uniqueID:=FourCharToLong(fID[1], fID[2], fID[3], fID[4])
end;

procedure TCustomVSTModule.Process(inputs, outputs: PPSingle; sampleFrames: Integer);
var Ins  : TArrayOfSingleDynArray absolute inputs;
    Outs : TArrayOfSingleDynArray absolute outputs;
    OutsTmp: TArrayOfSingleDynArray;
    i, j: Integer;
begin
// fTempo := TempoAt(0) * 0.0001; // get current bpm tempo from host
 SetLength(OutsTmp, fEffect.NumOutputs, sampleFrames);
 for j := 0 to fEffect.NumOutputs - 1 do FillChar(OutsTmp[j][0], sampleFrames*SizeOf(Single), 0);
 if assigned(fOnProcessEx) then fOnProcessEx(Ins,Outs,SampleFrames);
 for i := 0 to sampleFrames - 1 do
  for j := 0 to fEffect.NumOutputs - 1 do
   Outs[j, i] := Outs[j, i] + OutsTmp[j, i];
 if fMidiEvent.numEvents > 0 then
  begin
   sendVstEventsToHost(@fMidiEvent);
   fMidiEvent.numEvents := 0;
  end;
end;

procedure TCustomVSTModule.ProcessReplacing(inputs, outputs: PPSingle; sampleFrames: Integer);
var Ins  : TArrayOfSingleDynArray absolute inputs;
    Outs : TArrayOfSingleDynArray absolute outputs;
begin
// fTempo := TempoAt(0) * 0.0001; // get current bpm tempo from host
 if assigned(fOnProcessReplacingEx) then fOnProcessReplacingEx(Ins,Outs,SampleFrames);
 if fMidiEvent.numEvents > 0 then
  begin
   sendVstEventsToHost(@fMidiEvent);
   fMidiEvent.numEvents := 0;
  end;
end;

procedure TCustomVSTModule.ProcessDoubleReplacing(inputs, outputs: PPDouble; sampleFrames: Integer);
var Ins  : TArrayOfDoubleDynArray absolute inputs;
    Outs : TArrayOfDoubleDynArray absolute outputs;
begin
// fTempo := TempoAt(0) * 0.0001; // get current bpm tempo from host
 if assigned(fOnProcessDoublesEx) then fOnProcessDoublesEx(Ins,Outs,SampleFrames);
 if fMidiEvent.numEvents > 0 then
  begin
   sendVstEventsToHost(@fMidiEvent);
   fMidiEvent.numEvents := 0;
  end;
end;

procedure TCustomVSTModule.fOnProcessCopy(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i,j: Integer;
begin
 j:=numInputs;
 if numOutputs<numInputs then j:=numOutputs;
 for i:=0 to j-1 do Move(inputs[i,0],outputs[i,0],sampleframes*SizeOf(Single));
end;

procedure TCustomVSTModule.fOnProcessMute(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i:=0 to numOutputs do Fillchar(outputs[i,0],0,sampleframes*SizeOf(Single));
end;

procedure TCustomVSTModule.fOnProcessCopy(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
var i,j: Integer;
begin
 j:=numInputs;
 if numOutputs<numInputs then j:=numOutputs;
 for i:=0 to j-1 do Move(inputs[i,0],outputs[i,0],sampleframes*SizeOf(Double));
end;

procedure TCustomVSTModule.fOnProcessMute(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i:=0 to numOutputs do Fillchar(outputs[i,0],0,sampleframes*SizeOf(Double));
end;

procedure TCustomVSTModule.SetBlockForcedSize(v: Integer);
begin
 if v>0 then fBlockModeSize:=v;
 fBlockPosition:=fBlockModeOverlap;
 PrepareBlockProcessing;
end;

procedure TCustomVSTModule.SetBlockOverlapSize(v: Integer);
begin
 if v<fBlockModeSize
  then fBlockModeOverlap:=v;
 if (fProcessingMode=pmBlockSave) and (fEffect.InitialDelay<fBlockModeSize-fBlockModeOverlap)
  then SetInitialDelay(fInitialDelay);
end;

procedure TCustomVSTModule.fOnBlockSaveProcess(const Inputs, Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;

 repeat
  if fBlockPosition+(SampleFrames-CurrentPosition)<fBlockModeSize then
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));

    fBlockPosition:=fBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i:=0 to numInputs-1  do move(inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Single));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],outputs[i,CurrentPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Single));

    fOnProcess(fBlockInBuffer,fBlockOutBuffer,fBlockModeSize);

    for i:=0 to numInputs-1  do move(fBlockInBuffer[i,(fBlockModeSize-fBlockModeOverlap)],fBlockInBuffer[i,0],fBlockModeOverlap*Sizeof(Single));
//    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,ProcessSizeH],fBlockOutBuffer[i,0],ProcessSizeH*Sizeof(Single));

    CurrentPosition:=CurrentPosition+(fBlockModeSize-fBlockPosition);
    fBlockPosition:=fBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TCustomVSTModule.fOnBlockSaveProcess(const Inputs, Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;

 repeat
  if fBlockPosition+(SampleFrames-CurrentPosition)<fBlockModeSize then
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));

    fBlockPosition:=fBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i:=0 to numInputs-1  do move(inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Double));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],outputs[i,CurrentPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Double));

    fOnProcess(fBlockInBuffer,fBlockOutBuffer,fBlockModeSize);

    for i:=0 to numInputs-1  do move(fBlockInBuffer[i,(fBlockModeSize-fBlockModeOverlap)],fBlockInBuffer[i,0],fBlockModeOverlap*Sizeof(Double));
//    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,ProcessSizeH],fBlockOutBuffer[i,0],ProcessSizeH*Sizeof(Double));

    CurrentPosition:=CurrentPosition+(fBlockModeSize-fBlockPosition);
    fBlockPosition:=fBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TCustomVSTModule.fOnBlockSaveProcessReplacing(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;

 repeat
  if fBlockPosition+(SampleFrames-CurrentPosition)<fBlockModeSize then
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Single));

    fBlockPosition:=fBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i:=0 to numInputs-1  do move(inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Single));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],outputs[i,CurrentPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Single));

    fOnProcessReplacing(fBlockInBuffer,fBlockOutBuffer,fBlockModeSize);

    for i:=0 to numInputs-1  do move(fBlockInBuffer[i,(fBlockModeSize-fBlockModeOverlap)],fBlockInBuffer[i,0],fBlockModeOverlap*Sizeof(Single));
//    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockModeOverlap],fBlockOutBuffer[i,0],(fBlockModeSize-fBlockModeOverlap)*Sizeof(Single));

    CurrentPosition:=CurrentPosition+(fBlockModeSize-fBlockPosition);
    fBlockPosition:=fBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TCustomVSTModule.fOnBlockSaveProcessReplacing(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
var CurrentPosition : Integer;
    i               : Integer;
begin
 CurrentPosition:=0;

 repeat
  if fBlockPosition+(SampleFrames-CurrentPosition)<fBlockModeSize then
   begin
    for i:=0 to numInputs-1  do move(Inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],Outputs[i,CurrentPosition],(SampleFrames-CurrentPosition)*Sizeof(Double));

    fBlockPosition:=fBlockPosition+(SampleFrames-CurrentPosition);
    CurrentPosition:=SampleFrames;
   end
  else
   begin
    for i:=0 to numInputs-1  do move(inputs[i,CurrentPosition],fBlockInBuffer[i,fBlockPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Double));
    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockPosition],outputs[i,CurrentPosition],(fBlockModeSize-fBlockPosition)*Sizeof(Double));

    fOnProcessReplacing(fBlockInBuffer,fBlockOutBuffer,fBlockModeSize);

    for i:=0 to numInputs-1  do move(fBlockInBuffer[i,(fBlockModeSize-fBlockModeOverlap)],fBlockInBuffer[i,0],fBlockModeOverlap*Sizeof(Double));
//    for i:=0 to numOutputs-1 do move(fBlockOutBuffer[i,fBlockModeOverlap],fBlockOutBuffer[i,0],(fBlockModeSize-fBlockModeOverlap)*Sizeof(Double));

    CurrentPosition:=CurrentPosition+(fBlockModeSize-fBlockPosition);
    fBlockPosition:=fBlockModeOverlap;
   end;
 until CurrentPosition>=SampleFrames;
end;

procedure TCustomVSTModule.PrepareBlockProcessing;
var i : Integer;
begin
 if fProcessingMode=pmBlockSave then
  begin
   SetLength(fBlockInBuffer,numInputs);
   SetLength(fBlockOutBuffer,numOutputs);
   for i:=0 to numInputs-1 do SetLength(fBlockInBuffer[i],fBlockModeSize);
   for i:=0 to numOutputs-1 do SetLength(fBlockOutBuffer[i],fBlockModeSize);
   fBlockPosition:=fBlockModeOverlap;
   if (fProcessingMode=pmBlockSave) and (fEffect.InitialDelay<fBlockModeSize-fBlockModeOverlap)
    then SetInitialDelay(fInitialDelay);
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
 fOnProcess:=v;
 case fProcessingMode of
  pmNormal: fOnProcessEx:=fOnProcess;
  pmBlockSave:  begin
                 if Assigned(fOnProcessReplacing)
                  then fOnProcessEx:=fOnBlockSaveProcess
                  else fOnProcessEx:=fOnProcess;
//                 PrepareBlockProcessing;
                end;
  pmCopy: fOnProcessEx:=fOnProcessCopy;
  pmMute: fOnProcessEx:=fOnProcessMute;
 end;
end;

procedure TCustomVSTModule.SetOnProcessReplacing(v : TProcessAudioEvent);
begin
 fOnProcessReplacing:=v;
 case fProcessingMode of
  pmNormal: fOnProcessReplacingEx:=fOnProcessReplacing;
  pmBlockSave: begin
                if Assigned(fOnProcessReplacing)
                 then fOnProcessReplacingEx:=fOnBlockSaveProcessReplacing
                 else fOnProcessReplacingEx:=fOnProcessReplacing;
//                PrepareBlockProcessing;
               end;
  pmCopy:   fOnProcessReplacingEx:=fOnProcessCopy;
  pmMute:   fOnProcessReplacingEx:=fOnProcessMute;
 end;
end;

procedure TCustomVSTModule.SetOnProcessDoubleReplacing(v : TProcessDoubleEvent);
begin
 fOnProcessDoubles:=v;
 case fProcessingMode of
  pmNormal: fOnProcessDoublesEx:=fOnProcessDoubles;
  pmBlockSave: begin
                if Assigned(fOnProcessDoubles)
                 then fOnProcessDoublesEx:=fOnBlockSaveProcessReplacing
                 else fOnProcessDoublesEx:=fOnProcessDoubles;
//                PrepareBlockProcessing;
               end;
  pmCopy:   fOnProcessReplacingEx:=fOnProcessCopy;
  pmMute:   fOnProcessReplacingEx:=fOnProcessMute;
 end;
end;

procedure TCustomVSTModule.SetProcessingMode(v : TProcessingMode);
begin
 if v<>fProcessingmode then
  begin
   fProcessingmode:=v;
   case fProcessingMode of
    pmNormal: begin
               fOnProcessEx:=fOnProcess;
               fOnProcessReplacingEx:=fOnProcessReplacing;
              end;
    pmBlockSave: begin
                  if Assigned(fOnProcess)
                   then fOnProcessEx:=fOnBlockSaveProcess
                   else fOnProcessEx:=fOnProcess;
                  if Assigned(fOnProcessReplacing)
                   then fOnProcessReplacingEx:=fOnBlockSaveProcessReplacing
                   else fOnProcessReplacingEx:=fOnProcessReplacing;
                PrepareBlockProcessing;
              end;
    pmCopy:   begin
               fOnProcessEx:=fOnProcessCopy;
               fOnProcessReplacingEx:=fOnProcessCopy;
              end;
    pmMute:   begin
               fOnProcessEx:=fOnProcessMute;
               fOnProcessReplacingEx:=fOnProcessMute;
              end;
   end;
  end;
end;

procedure TCustomVSTModule.setProgram(aProgram: Integer);
var i: Integer;
begin
 if (aProgram >= 0) and (aProgram < fEffect.numPrograms) and (numPrograms>0) then
  begin
   if Assigned(fOnBeforeProgramChange) then fOnBeforeProgramChange(Self);
   fCurProgram := aProgram;
   if Assigned(fOnAfterProgramChange) then fOnAfterProgramChange(Self);
//   if (effFlagsProgramChunks in fEffect.EffectFlags) then
    try
     for i := 0 to Length(Programs[fCurProgram].fParameter)-1
      do setParameter(i, Programs[fCurProgram].fParameter[i]);
    except
    end;
   fEditorNeedUpdate := true;
  end;
 updateDisplay;
end;

procedure TCustomVSTModule.setCurrentProgramName(AName: string);
begin
 if (fCurProgram<numPrograms) and (numPrograms>0) then
  begin
   Programs[fCurProgram].DisplayName:=AName;
   fEditorNeedUpdate := true;
  end;
 updateDisplay;
end;

function TCustomVSTModule.getCurrentProgramName:string;
begin
 if (fCurProgram<numPrograms) and (numPrograms>0) and (fCurProgram>=0)
  then result:=Programs[fCurProgram].DisplayName
  else result:='';
end;

procedure TCustomVSTModule.getParameterLabel(index: Integer; text: pchar);
var str : string;
begin
 if (index >= fEffect.numParams) or (Index>=fParameterProperties.Count)
  then str:='undefined'
  else
   begin
    str:=fParameterProperties[index].Units;
    if Assigned(fParameterProperties[index].fOnCPL)
     then fParameterProperties[index].fOnCPL(Self,Index,str);
   end;
 StrPCopy(text, str)
end;

procedure TCustomVSTModule.getParameterDisplay(index: Integer; text: pchar);
var str : string;
begin
 if (index >= fEffect.numParams) or (Index>=fParameterProperties.Count)
  then str:='undefined'
  else
   begin
    if (effFlagsProgramChunks in fEffect.EffectFlags)
     then str:=FloatToStr(fOnGetChunkParamEvent(Self,Index))
     else
      if (numPrograms>0)
       then str:=FloatToStrF(Programs[fCurProgram].fParameter[Index],ffGeneral,4,4)
       else str:=FloatToStrF(fParameter[index],ffGeneral,4,4);
    if Assigned(fParameterProperties[index].fOnCPD)
     then fParameterProperties[index].fOnCPD(Self,Index,str);
   end;
 StrPCopy(text, str)
end;

procedure TCustomVSTModule.getParameterName(index: Integer; text: pchar);
begin
 if (index >= fEffect.numParams) or (Index>=fParameterProperties.Count)
  then StrPCopy(text, 'undefined')
  else StrPCopy(text,fParameterProperties[index].DisplayName);
end;

function TCustomVSTModule.getChunk(var data: pointer; isPreset: Boolean): Integer;
var i,j  : Integer;
    tmps : TMemoryStream;
begin
 Result := 0;
 if (numPrograms<=0) then Exit;
 if isPreset then
 begin
  Programs[fCurProgram].Chunk.Position:=0;
  if Assigned(Programs[fCurProgram].fOnStoreChunk)
   then Programs[fCurProgram].fOnStoreChunk(Programs[fCurProgram],fCurProgram,True);
  data := Programs[fCurProgram].Chunk.Memory;
  result := Programs[fCurProgram].Chunk.Size;
 end else
 begin
  tmps:=TMemoryStream.Create;
  for i:=0 to numPrograms-1 do
   begin
    Programs[i].Chunk.Position:=0;
    if Assigned(Programs[i].fOnStoreChunk)
     then Programs[i].fOnStoreChunk(Programs[fCurProgram],fCurProgram,False);
    j := Programs[i].Chunk.Size;
    tmps.Write(j, 4);
    tmps.Write(Programs[i].Chunk.Memory^, Programs[i].Chunk.Size);
   end;
  Data := tmps.Memory;
  Result := tmps.Size;
 end;
end;

function TCustomVSTModule.setChunk(data: pointer; byteSize: Integer; isPreset: Boolean): Integer;
var i: integer;
    pi: pinteger;
    pb: pbyte;
begin
 Result := 0;
 if (numPrograms<=0) then Exit;
 if isPreset then
  with Programs[fCurProgram] do
   begin
    Chunk.Clear;
    Chunk.Write(data^, byteSize);
    Chunk.Position:=0;
    result:=bytesize;
    if Assigned(fOnLoadChunk)
     then fOnLoadChunk(Programs[fCurProgram],fCurProgram,True);
   end
 else
  begin
   pb := data;
   for i := 0 to NumPrograms - 1 do
    begin
     Programs[i].Chunk.Clear;
     pi := pinteger(pb);
     inc(pb, 4);
     Programs[i].Chunk.Write(pb^, pi^);
     Programs[i].Chunk.Position:=0;
     inc(pb, pi^);
     if Assigned(Programs[i].fOnLoadChunk)
      then Programs[i].fOnLoadChunk(Programs[i],i,False);
    end;
   Result := bytesize;
   if Assigned(Programs[CurrentProgram].fOnLoadChunk)
    then Programs[CurrentProgram].fOnLoadChunk(Programs[CurrentProgram],CurrentProgram,False);
  end;
 fEditorNeedUpdate:=True;
end;

procedure TCustomVSTModule.setSampleRate(newvalue: Single);
begin
 if fSampleRate<>newvalue then
  begin
   fSampleRate := newvalue;
   if assigned(fSampleRateChangeEvent) then fSampleRateChangeEvent(Self,newvalue);
  end;
end;

procedure TCustomVSTModule.setBlockSize(newvalue: Integer);
begin
 if fBlockSize<>newvalue then
  begin
   fBlockSize := newvalue;
   if assigned(fBlockSizeChangeEvent) then fBlockSizeChangeEvent(Self,newvalue);
  end;
end;

procedure TCustomVSTModule.Suspend;
begin
 if assigned(fOnSuspend) then fOnSuspend(Self);
end;

procedure TCustomVSTModule.Resume;
begin
 if assigned(fOnResume) then fOnResume(Self);
 wantEvents(1);
end;

procedure TCustomVSTModule.setNumInputs(inputs: Integer);
begin
 fEffect.numInputs := inputs;
 PrepareBlockProcessing;
 ioChanged;
end;

procedure TCustomVSTModule.setNumOutputs(outputs: Integer);
begin
 fEffect.numOutputs := outputs;
 PrepareBlockProcessing;
 ioChanged;
end;

function TCustomVSTModule.getMasterVersion: Integer;
var vers: Integer;
begin
 vers := 1;
 if Assigned(fAudioMaster) then
  begin
   vers := fAudioMaster(@fEffect, audioMasterVersion, 0, 0, nil, 0);
   if (vers = 0)
    then vers := 1;
  end;
 Result := vers;
end;

function TCustomVSTModule.getCurrentUniqueId: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterCurrentId, 0, 0, nil, 0)
  else Result := 0;
end;

procedure TCustomVSTModule.masterIdle;
begin
 if Assigned(fAudioMaster)
  then fAudioMaster(@fEffect, audioMasterIdle, 0, 0, nil, 0);
end;

function TCustomVSTModule.isInputConnected(input: Integer): Boolean;
var ret: Integer;
begin
 ret := 0;
 if Assigned(fAudioMaster)
  then ret := fAudioMaster(@fEffect, audioMasterPinConnected, input, 0, nil, 0);
 Result := (ret = 0);
end;

function TCustomVSTModule.isOutputConnected(output: Integer): Boolean;
var ret: Integer;
begin
 ret := 0;
 if Assigned(fAudioMaster)
  then ret := fAudioMaster(@fEffect, audioMasterPinConnected, output, 1, nil, 0);
 Result := (ret = 0);
end;

// flags

procedure TCustomVSTModule.SetPluginFlags(newFlags : TEffFlags);
begin
 fEffect.EffectFlags:=newFlags;
end;

function TCustomVSTModule.GetPluginFlags: TEffFlags;
begin
 result:=fEffect.EffectFlags;
end;

procedure TCustomVSTModule.setInitialDelay(delay: Integer);
var hst : string;
begin
 if fInitialDelay<>delay then
  begin
   fInitialDelay:=delay;
   if (fProcessingMode=pmBlockSave) and (fInitialDelay<fBlockModeSize-fBlockModeOverlap)
    then fEffect.initialDelay := fBlockModeSize-fBlockModeOverlap
    else fEffect.initialDelay := fInitialDelay;
   hst:=HostProduct;
   if hst<>'energyXT'
    then ioChanged;
  end;
end;

function TCustomVSTModule.GetEffect: PVSTEffect;
begin
 Result := @fEffect;
end;

procedure TCustomVSTModule.SetVstPrograms(const Value: TCustomVstPrograms);
begin
 FVstPrograms.Assign(Value);
end;

procedure TCustomVSTModule.SetParameterProperties(const Value : TCustomVstParameterProperties);
begin
 FVstPrograms.Assign(Value);
end;

function f_limit(v: Single; l: Single = -1; u: Single = 1): single;
begin
 if v < l then result := l else
 if v > u then result := u else
 result := v;
end;

function TCustomVSTModule.Parameter2VSTParameter(const Value: Single; Index : Integer): Single;
begin
 if (Index>=numParams) or (Index>=fParameterProperties.Count) then begin Result := 0; Exit; end; 
 result := (value - fParameterProperties[index].min) / (fParameterProperties[index].max - fParameterProperties[index].min);
 case fParameterProperties[index].curve of
  ctLogarithmic: result := ln(fParameterProperties[index].curveFactor * result + 1) / ln(fParameterProperties[index].curveFactor + 1);
  ctExponential: result := exp(result * ln(fParameterProperties[index].curveFactor + 1)) - 1;
  ctFrequencyScale: if fParameterProperties[index].min<>0
                     then result := ln((fParameterProperties[index].max/fParameterProperties[index].min)*result+1)/ln((fParameterProperties[index].max/fParameterProperties[index].min))
                     else result := ln((fParameterProperties[index].max)*result+1)/ln((fParameterProperties[index].max));
  else
 end;
 result := f_limit(result, 0, 1);
end;

function TCustomVSTModule.VSTParameter2Parameter(const Value: Single; Index : Integer): Single;
begin
 Result := Value;
 case fParameterProperties[index].curve of
  ctLogarithmic: Result := (exp(Result * ln(fParameterProperties[index].curveFactor + 1)) - 1) / fParameterProperties[index].curveFactor;
  ctExponential: Result := ln(fParameterProperties[index].curveFactor * Result + 1) / ln(fParameterProperties[index].curveFactor + 1);
 else
 end;
 Result := fParameterProperties[index].Smooth(Result * (fParameterProperties[index].max - fParameterProperties[index].min) + fParameterProperties[index].min);
end;

procedure TCustomVSTModule.ReadOnlyString(s: string);
begin end;

procedure TCustomVSTModule.setParameterAutomated(index: Integer; value: Single);
begin
 if (Index>=numParams) or (Index>=fParameterProperties.Count) then Exit;
 setParameter(Index,Value);
 if fParameterProperties[index].CanBeAutomated and Assigned(fAudioMaster) and not fIsHostAutomation
  then fAudioMaster(@fEffect, audioMasterAutomate, index, 0, nil, Parameter2VSTParameter(value,index));
end;

procedure TCustomVSTModule.setParameter(const index: Integer; value: Single);
begin
 if fParameterUpdate then exit;
 {$IFDEF Debug} fLog.Add('Set Parameter: '+FloatToStr(value)); {$ENDIF}
 fParameterUpdate:=True;
 try
  if (index >= fEffect.numParams) or (Index < 0) or (Index>=fParameterProperties.Count)
   then raise exception.Create('Index out of bounds');
  if (effFlagsProgramChunks in fEffect.EffectFlags)
   then
    begin
     if Assigned(ParameterProperties[index].fOnSPC)
      then fParameterProperties[index].fOnSPC(Self,Index,Value);
     if Assigned(OnParameterChange)
      then OnParameterChange(Self, Index, Value);
    end
   else
    begin
     if (numPrograms>0) and (fCurProgram>=0)
      then
       begin
        Programs[fCurProgram].fParameter[index] := Value;
        if Assigned(ParameterProperties[index].fOnSPC)
         then fParameterProperties[index].fOnSPC(Self,Index,Programs[fCurProgram].fParameter[Index]);
        if Assigned(OnParameterChange)
         then OnParameterChange(Self, Index, Programs[fCurProgram].fParameter[Index]);
       end
      else
       begin
        fParameter[index] := Value;
        if Assigned(ParameterProperties[index].fOnSPC)
         then fParameterProperties[index].fOnSPC(Self,Index,fParameter[Index]);
        if Assigned(OnParameterChange)
         then OnParameterChange(Self, Index, fParameter[Index]);
       end
    end;
  fEditorNeedUpdate := true;
 finally
  fParameterUpdate := False;
 end;
end;

function TCustomVSTModule.getParameter(index: Integer): Single;
begin
 if (effFlagsProgramChunks in fEffect.EffectFlags)
  then Result:=fOnGetChunkParamEvent(Self,Index)
  else
   if numPrograms>0
    then Result:=Programs[fCurProgram].fParameter[Index]
    else Result:=fParameter[Index];
end;

//------------------------------------------------------------------------------
//  TVSTModuleEditor
//------------------------------------------------------------------------------

function TCustomVSTModule.EditorOpen(ptr: Pointer): Integer;
var GUI  : TForm;
    i,pr : Integer;
begin
{
 try
  if Application.Handle=0 then Application.Handle:=HWnd(ptr);
 except
 end;
}
 Result := 0;
 if Assigned(fOnEditOpen) then fOnEditOpen(Self, GUI);
 if assigned(GUI) then
  try
   fEditorForm:=GUI;
   {$IFNDEF FPC}
   fEditorForm.ParentWindow:=HWnd(ptr);
   {$ENDIF}
   fEditorForm.Visible:=True;
   fEditorForm.BorderStyle:=bsNone;
   fEditorForm.SetBounds(0, 0, fEditorForm.Width, fEditorForm.Height);
   fEditorForm.Invalidate;
   Result := 1; pr := min(numParams,fParameterProperties.Count);
   if assigned(fOnParameterChangeEvent) and (not (effFlagsProgramChunks in fEffect.EffectFlags)) then
    if numPrograms>0
     then for i:=0 to pr-1 do fOnParameterChangeEvent(Self,i,Programs[fCurProgram].fParameter[i])
     else for i:=0 to pr-1 do fOnParameterChangeEvent(Self,i,fParameter[i]);
  except
  end;
end;

procedure TCustomVSTModule.EditorClose;
begin
// Application.Handle:=0;
 if Assigned(fOnEditClose) then fOnEditClose(Self);
 if assigned(fEditorForm) then
  begin
//   fEditorForm.ParentWindow:=0;
   fEditorForm.Free;
   fEditorForm:=nil;
  end;
end;

procedure TCustomVSTModule.EditorIdle;
begin
 if fEditorNeedUpdate and Assigned(fEditorForm) then
  begin
   if Assigned(fOnEditIdle) then fOnEditIdle(Self);
   fEditorNeedUpdate := False;
//   fEditorForm.Invalidate;
  end;
end;

procedure TCustomVSTModule.EditorPostUpdate;
begin
 fEditorNeedUpdate := True;
end;

procedure TCustomVSTModule.ReadState(Reader: TReader);
var i: integer;
begin
 {$IFDEF Debug} fLog.Add('ReadState'); {$ENDIF}
 {$IFDEF Debug} fLog.SaveToFile('Debug.log'); {$ENDIF}
 inherited;
 {$IFDEF Debug} fLog.Add('Ui1'); {$ENDIF}
 {$IFDEF Debug} fLog.SaveToFile('Debug.log'); {$ENDIF}
 for i:=0 to numPrograms-1
  do if assigned(Programs[i].fOnInitialize) then Programs[i].fOnInitialize(Programs[i]);
 if numPrograms<0
  then fCurProgram:=-1
  else CurrentProgram:=0;
 if assigned(fOnInitialize) then fOnInitialize(Self);
 {$IFDEF Debug} fLog.Add('Ui2'); {$ENDIF}
 {$IFDEF Debug} fLog.SaveToFile('Debug.log'); {$ENDIF}
end;

// Functions

function dispatchEffectClass(effect: PVSTEffect; opcode, index, value: Integer; ptr: pointer; opt: Single): Integer; cdecl;
var VSTModule: TCustomVSTModule;
begin
 VSTModule := TCustomVSTModule(effect^.vObject);
 if (opcode = effClose) then
  try
   VSTModule.dispatcher(opcode, index, value, ptr, opt);
   VSTModule.Free;
   Result := 1;
  except
   Result := 0;
  end
 else Result := VSTModule.dispatcher(opcode, index, value, ptr, opt);
end;

function getParameterClass(effect: PVSTEffect; index: Integer): Single; cdecl;
var VSTModule: TCustomVSTModule;
begin
 VSTModule := TCustomVSTModule(effect^.vObject);
 if (Index<VSTModule.numParams) and (Index<VSTModule.fParameterProperties.Count)
  then Result := VSTModule.Parameter2VSTParameter(VSTModule.getParameter(index),Index)
  else Result := 0;
end;

procedure setParameterClass(effect: PVSTEffect; index: Integer; value: Single); cdecl;
begin
 with TCustomVSTModule(effect^.vObject) do
  begin
   {$IFDEF Debug} fLog.Add('Set Parameter Class: '+FloatToStr(value)); {$ENDIF}
   if fIsHostAutomation then exit;
   fIsHostAutomation:=True;
   if ((Index>=numParams) or (Index>=fParameterProperties.Count)) and Assigned(OnParameterSizeFailed)
    then OnParameterSizeFailed(TCustomVSTModule(effect^.vObject))
    else setParameter(index, VSTParameter2Parameter(value,index));
   fIsHostAutomation:=False;
  end;
end;

procedure processClass(effect: PVSTEffect; inputs, outputs: PPSingle; sampleframes: Integer); cdecl;
begin
 TCustomVSTModule(effect^.vObject).process(inputs, outputs, sampleFrames);
end;

procedure processClassReplacing(effect: PVSTEffect; inputs, outputs: PPSingle; sampleframes: Integer); cdecl;
begin
 TCustomVSTModule(effect^.vObject).processReplacing(inputs, outputs, sampleFrames);
end;

procedure processClassDoubleReplacing(effect: PVSTEffect; inputs, outputs: PPDouble; sampleframes: Integer); cdecl;
begin
 TCustomVSTModule(effect^.vObject).processDoubleReplacing(inputs, outputs, sampleFrames);
end;

// TVstParameterProperty

{$IFDEF FPC}
constructor TCustomVstParameterProperty.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstParameterProperty.Create(Collection: TCollection);
{$ENDIF}
var i: integer;
begin
 inherited;
 fMin:=0;
 fMax:=1;
 fCC:=-1;
 fCurve:=ctLinear;
 fCurveFactor:=1;
 fSmoothingFactor:=1;
 fCanBeAutomated:=True;
 fDisplayName := 'Parameter '+IntTostr(Collection.Count);
 fVSTModule := (Collection As TCustomVstParameterProperties).VSTModule;
 VSTModule.FEffect.numParams:=Collection.Count;

 if not (effFlagsProgramChunks in VSTModule.FEffect.EffectFlags) then
  if (VSTModule.FEffect.numPrograms>0)
   then for i:=0 to VSTModule.FEffect.numPrograms-1 do SetLength(VSTModule.Programs[i].fParameter,Collection.Count)
   else SetLength(VSTModule.fParameter,Collection.Count);
end;

destructor TCustomVstParameterProperty.Destroy;
var i: integer;
begin
 try
  if not (effFlagsProgramChunks in VSTModule.FEffect.EffectFlags) then
   if VSTModule.FEffect.numPrograms>0
    then for i:=0 to VSTModule.FEffect.numPrograms-1 do SetLength(VSTModule.Programs[i].fParameter,Collection.Count-1)
    else SetLength(VSTModule.fParameter,Collection.Count-1);
 except
 end;
 inherited;
end;

function TCustomVstParameterProperty.Smooth(i: Single): Single;
begin
 i1 := i1 + SmoothingFactor * (i - i1);
 i2 := i2 + SmoothingFactor * (i1 - i2);
 Result := i2;
end;

procedure TCustomVstParameterProperty.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstParameterProperty then with TCustomVstParameterProperty(Dest) do
 try
  Units:=Self.Units;
  DisplayName := Self.DisplayName;
 except
  inherited;
 end else
  inherited;
end;

{$IFNDEF FPC}
procedure TCustomVstParameterProperty.SetDisplayName(const AValue: string);
begin
 fDisplayName:=copy(AValue,1,math.min(30,Length(AValue)));
end;

function TCustomVstParameterProperty.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TCustomVstParameterProperty.SetUnits(AUnits: string);
begin
 FUnits := AUnits;
end;

{$ELSE}

procedure TCustomVstParameterProperty.SetDisplayName(const AValue: ansistring);
begin
 fDisplayName:=copy(AValue,1,math.min(30,Length(AValue)));
end;

function TCustomVstParameterProperty.GetDisplayName: ansistring;
begin
 Result := FDisplayName;
end;

procedure TCustomVstParameterProperty.SetUnits(AUnits: ansistring);
begin
 FUnits := AUnits;
end;

{$ENDIF}

{ TVstParameterProperties }

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
     Add(#9+#9+'<Param name="'+Items[i].fDisplayName+'"'+#9+
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
  then SetLength(fParameter,VSTModule.numParams)
  else fChunkData:=TMemoryStream.Create;
 if VSTModule.fCurProgram<0 then VSTModule.fCurProgram:=0;
end;

destructor TCustomVstProgram.Destroy;
begin
 try
  SetLength(fParameter,0);
  FreeAndNil(fChunkData);
 finally
  inherited;
 end;
end;

{$IFNDEF FPC}
function TCustomVstProgram.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TCustomVstProgram.SetDisplayName(const AValue: string);
begin
 fDisplayName:=copy(AValue,0,50);
end;
{$ELSE}
function TCustomVstProgram.GetDisplayName: ansistring;
begin
 Result := FDisplayName;
end;

procedure TCustomVstProgram.SetDisplayName(const AValue: ansistring);
begin
 fDisplayName:=copy(AValue,0,50);
end;
{$ENDIF}

procedure TCustomVstProgram.AssignTo(Dest: TPersistent);
var i: Integer;
begin
 if Dest is TCustomVstProgram then
  with TCustomVstProgram(Dest) do
   begin
    if Length(Self.fParameter)>0 then
     begin
      SetLength(TCustomVstProgram(Dest).fParameter,Length(Self.fParameter));
      for i:=0 to Length(Self.fParameter)-1 do Parameter[i]:=Self.Parameter[i];
     end;
    DisplayName := Self.DisplayName;
   end
  else inherited;
end;

procedure TCustomVstProgram.SetParameter(AIndex: Integer; s: Single);
begin
 if effFlagsProgramChunks in fVSTModule.Flags then exit;
 if (AIndex>=0) and (AIndex<VSTModule.numParams)
  then fParameter[AIndex]:=s
  else //raise exception.Create('Index out of bounds');
end;

function TCustomVstProgram.GetParameter(AIndex: Integer): Single;
begin
 if (AIndex>=0) and (AIndex<VSTModule.numParams)
  then result:=fParameter[AIndex] else
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

{$IFDEF DELPHI6_UP}
{$WARNINGS ON}
function IsCPUID_Available: Boolean; register;
asm
  PUSHFD                 {save EFLAGS to stack}
  POP     EAX            {store EFLAGS in EAX}
  MOV     EDX, EAX       {save in EDX for later testing}
  XOR     EAX, $200000;  {flip ID bit in EFLAGS}
  PUSH    EAX            {save new EFLAGS value on stack}
  POPFD                  {replace current EFLAGS value}
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
  MOV     _FSW, $5A5A    {store a non-zero value}
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

constructor TCPU.Create;
begin
 inherited;
// Name := 'CPU';
 GetCPUInfo;
end;

destructor TCPU.Destroy;
begin
 inherited;
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
    on E: Exception do
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

procedure TCustomVSTModule.MIDI_Out(b1, b2, b3, b4: byte; offset: integer);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := b1;
   midiData[1] := b2;
   midiData[2] := b3;
   midiData[3] := b4;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_CC(ch, num, val: integer; offset: integer = 0);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := $B0 + ch;
   midiData[1] := num;
   midiData[2] := val;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_ChannelAftertouch(ch, val: integer; offset: integer = 0);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := $D0 + ch;
   midiData[1] := val;
   midiData[2] := 0;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_NoteOff(ch, note, val: integer; offset: integer = 0);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := $80 + ch;
   midiData[1] := note;
   midiData[2] := val;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_NoteOn(ch, note, val: integer; offset: integer = 0);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := $90 + ch;
   midiData[1] := note;
   midiData[2] := val;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_PitchBend(ch, val: integer; offset: integer = 0);
var a, b: integer;
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   a := (val div 128) + 64;
   b := (val div 128);
   b := val - b * 128;
   midiData[0] := $E0 + ch;
   midiData[1] := b;
   midiData[2] := a;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_PitchBend2(ch, x1, x2: integer; offset: integer = 0);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := $E0 + ch;
   midiData[1] := x1;
   midiData[2] := x2;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_PolyAftertouch(ch, note, val: integer; offset: integer = 0);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := $A0 + ch;
   midiData[1] := note;
   midiData[2] := val;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.MIDI_ProgramChange(ch, val: integer; offset: integer = 0);
begin
 with PVstMidiEvent(fMidiEvent.events[fMidiEvent.numEvents])^ do
  begin
   midiData[0] := $D0 + ch;
   midiData[1] := val;
   midiData[2] := 0;
   deltaframes := offset;
   if fMidiEvent.numEvents < maxMidiEvents - 1 then inc(fMidiEvent.numEvents);
  end;
end;

procedure TCustomVSTModule.wantEvents(filter: Integer);
begin
 if Assigned(fAudioMaster) then fAudioMaster(@fEffect, audioMasterWantMidi, 0, filter, nil, 0);
end;

function TCustomVSTModule.getTimeInfo(filter: Integer): PVstTimeInfo;
begin
 if Assigned(fAudioMaster)
  then Result := PVstTimeInfo(fAudioMaster (@fEffect, audioMasterGetTime, 0, filter, nil, 0))
  else Result := nil;
end;

procedure TCustomVSTModule.setTimeInfo(filter: Integer; ti: PVstTimeInfo);
begin
 if Assigned(fAudioMaster)
  then fAudioMaster(@fEffect, audioMasterSetTime, 0, filter, ti, 0);
end;

function TCustomVSTModule.tempoAt(pos: Integer): Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterTempoAt, 0, pos, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.sendVstEventsToHost(events: PVstEvents): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterProcessEvents, 0, 0, events, 0) = 1
  else Result := FALSE;
end;

{ parameters }

function TCustomVSTModule.getNumAutomatableParameters: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetNumAutomatableParameters, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.getParameterQuantization: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetParameterQuantization, 0, 0, nil, 0)
  else Result := 0;
end;

{ configuration }

function TCustomVSTModule.ioChanged: Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterIOChanged, 0, 0, nil, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.needIdle: Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterNeedIdle, 0, 0, nil, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.sizeWindow(width, height: Integer): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterSizeWindow, width, height, nil, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.updateSampleRate: Double;
var res: Integer;
begin
 if Assigned(fAudioMaster) then
  begin
   res := fAudioMaster(@fEffect, audioMasterGetSampleRate, 0, 0, nil, 0);
   if (res > 0) then
    begin
     fSampleRate := res;
     if Assigned(OnSampleRateChange) then OnSampleRateChange(Self, sampleRate);
    end;
  end;
 Result := sampleRate;
end;

function TCustomVSTModule.updateBlockSize: Integer;
var res: Integer;
begin
 if Assigned(fAudioMaster) then
  begin
   res := fAudioMaster(@fEffect, audioMasterGetBlockSize, 0, 0, nil, 0);
   if (res > 0) then
    begin
     fBlockSize := res;
     if Assigned(OnBlockSizeChange) then OnBlockSizeChange(Self, fBlockSize);
    end;
  end;
 Result := blockSize;
end;

function TCustomVSTModule.getInputLatency: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetInputLatency, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.getOutputLatency: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetOutputLatency, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.getPreviousPlug(input: Integer): PVSTEffect;
begin
 if Assigned(fAudioMaster)
  then Result := PVSTEffect(fAudioMaster(@fEffect, audioMasterGetPreviousPlug, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.getNextPlug(output: Integer): PVSTEffect;
begin
 if Assigned(fAudioMaster)
  then Result := PVSTEffect(fAudioMaster(@fEffect, audioMasterGetNextPlug, 0, 0, nil, 0))
  else Result := nil;
end;

{ configuration }

function TCustomVSTModule.willProcessReplacing: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterWillReplaceOrAccumulate, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.getCurrentProcessLevel: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetCurrentProcessLevel, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.getAutomationState: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetAutomationState, 0, 0, nil, 0)
  else Result := 0;
end;

{ offline }

function TCustomVSTModule.offlineRead(offline: PVstOfflineTask; option: TVstOfflineOption; readSource: Boolean): Boolean;
var ireadsource: integer;
begin
 ireadsource := Integer(readSource);

 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterOfflineRead, ireadsource, option, offline, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.offlineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterOfflineWrite, 0, option, offline, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.offlineStart(ptr: PVstAudioFile; numAudioFiles: Integer; numNewAudioFiles: Integer): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterOfflineStart, numNewAudioFiles, numAudioFiles, ptr, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.offlineGetCurrentPass: Integer;
begin
 if Assigned(fAudioMaster) then
  begin
   if fAudioMaster(@fEffect, audioMasterOfflineGetCurrentPass, 0, 0, nil, 0) <> 0
    then Result := 1
    else Result := 0;
  end
 else Result := 0;
end;

function TCustomVSTModule.offlineGetCurrentMetaPass: Integer;
begin
 if Assigned(fAudioMaster) then
  begin
   if (fAudioMaster(@fEffect, audioMasterOfflineGetCurrentMetaPass, 0, 0, nil, 0) <> 0)
    then Result := 1
    else Result := 0;
  end
 else Result := 0;
end;

procedure TCustomVSTModule.setOutputSampleRate(sampleRate: Single);
begin
 if Assigned(fAudioMaster)
  then fAudioMaster(@fEffect, audioMasterSetOutputSampleRate, 0, 0, nil, sampleRate);
end;

function TCustomVSTModule.getSpeakerArrangement(var pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean;
begin
 pluginInput := nil;
 pluginOutput := nil;
 Result := FALSE;
end;

function TCustomVSTModule.getHostVendorString(text: pchar): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterGetVendorString, 0, 0, text, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.getHostProductString(text: pchar): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterGetProductString, 0, 0, text, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.getHostVendorVersion: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetVendorVersion, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.hostVendorSpecific(lArg1, lArg2: Integer; ptrArg: pointer; floatArg: Single): Integer;
begin
 Result := 0;
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterVendorSpecific, lArg1, lArg2, ptrArg, floatArg);
end;

function TCustomVSTModule.canHostDo(text: pchar): Integer;
begin
 Result := 0;
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterCanDo, 0, 0, text, 0);
end;

function TCustomVSTModule.getHostLanguage: Integer;
begin
 if Assigned(fAudioMaster)
  then Result := fAudioMaster(@fEffect, audioMasterGetLanguage, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVSTModule.openWindow(aWindow: PVstWindow): pointer;
begin
 if Assigned(fAudioMaster)
  then Result := pointer(fAudioMaster(@fEffect, audioMasterOpenWindow, 0, 0, aWindow, 0))
  else Result := nil;
end;

function TCustomVSTModule.closeWindow(aWindow: PVstWindow): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterCloseWindow, 0, 0, aWindow, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.getDirectory: pointer;
begin
 if Assigned(fAudioMaster)
  then Result := pointer(fAudioMaster(@fEffect, audioMasterGetDirectory, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.updateDisplay: Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterUpdateDisplay, 0, 0, nil, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.ProcessEvents(events: PVstEvents): Integer;
var i: integer;
begin
 for i := 0 to events^.numEvents - 1 do
  if (events^.events[i]^.vtype = kVstMidiType) then
   if assigned(fProcessMidi) then fProcessMidi(Self, PVstMidiEvent(events^.events[i])^);
 Result := 1;
end;

function TCustomVSTModule.canParameterBeAutomated(Index: Integer): Boolean;
begin
 if Index<ParameterProperties.Count
  then Result := ParameterProperties[index].CanBeAutomated
  else Result := TRUE
end;

function TCustomVSTModule.string2parameter(Index: Integer; text: pchar): Boolean;
var tmp : string;
begin
 Result := FALSE;
 if text<>nil then
  try
   tmp:=text;
   Parameter[Index]:=StrtoFloat(tmp);
   Result := True;
  except
  end;
end;

function TCustomVSTModule.getChannelParameter(channel, Index: Integer): Single;
begin
 Result := 0;
end;

function TCustomVSTModule.getProgramNameIndexed(category, Index: Integer; text: pchar): Boolean;
begin
 Result := false;
 if (Index < fEffect.numPrograms) and not (Index<0) then
  begin
   StrPCopy(text,Programs[Index].DisplayName);
   Result := true;
  end;
end;

function TCustomVSTModule.copyProgram(destination: Integer): Boolean;
begin
 Result := FALSE; //ToDo
end;

function TCustomVSTModule.getInputProperties(Index: Integer; properties: PVstPinProperties): Boolean;
var str1  : string[63];
    str2  : string[7];
    sat   : TVstSpeakerArrangementType;
    cpf   : TChannelPropertyFlags;
begin
 Result := false;
 if (Index < fEffect.numInputs) then
 begin
  str1:='Input #' + inttostr(Index + 1);
  str2:='In' + inttostr(Index + 1);
  sat:=satStereo;
  cpf:=[cpfIsActive,cpfIsStereo];
  if Assigned(fOnGetInputProperties) then fOnGetInputProperties(Self,str1,str2,sat,cpf);
  StrPCopy(properties^.vLabel, str1); // set name of input channel:
  StrPCopy(properties^.shortLabel, str2); // set name of input channel:
  if cpfIsActive in cpf then properties^.flags := kVstPinIsActive else properties^.flags :=0;
  if cpfIsStereo in cpf then properties^.flags := properties^.flags or kVstPinIsStereo;
  if cpfUseSpeaker in cpf then properties^.flags := properties^.flags or kVstPinUseSpeaker;

  Result := true;
 end;
end;

function TCustomVSTModule.getOutputProperties(Index: Integer; properties: PVstPinProperties): Boolean;
var str1  : string[63];
    str2  : string[7];
    sat   : TVSTSpeakerArrangementType;
    cpf   : TChannelPropertyFlags;
begin
 Result := false;
 if (Index < fEffect.numOutputs) then
 begin
  str1:='Output #' + inttostr(Index + 1);
  str2:='Out' + inttostr(Index + 1);
  sat:=satStereo;
  cpf:=[cpfIsActive,cpfIsStereo];
  if Assigned(fOnGetOutputProperties) then fOnGetOutputProperties(Self,str1,str2,sat,cpf);
  StrPCopy(properties^.vLabel, str1); // set name of input channel:
  StrPCopy(properties^.shortLabel, str2); // set name of input channel:
  if cpfIsActive in cpf then properties^.flags := kVstPinIsActive else properties^.flags :=0;
  if cpfIsStereo in cpf then properties^.flags := properties^.flags or kVstPinIsStereo;
  if cpfUseSpeaker in cpf then properties^.flags := properties^.flags or kVstPinUseSpeaker;
  Result := true;
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

function TCustomVSTModule.setSpeakerArrangement(pluginInput, pluginOutput: PVstSpeakerArrangement): Boolean;
begin
  Result := FALSE;
end;

procedure TCustomVSTModule.setBlockSizeAndSampleRate(aBlockSize: Integer; aSampleRate: Single);
begin
 if fSampleRate<>aSampleRate then
  begin
   fSampleRate := aSampleRate;
   if assigned(fSampleRateChangeEvent) then fSampleRateChangeEvent(Self,aSampleRate);
  end;
 if fBlockSize<>aBlockSize then
  begin
   fBlockSize := aBlockSize;
   if assigned(fBlockSizeChangeEvent) then fBlockSizeChangeEvent(Self,aBlockSize);
  end;
end;

function TCustomVSTModule.setBypass(onOff: Boolean): Boolean;
begin
 if Assigned(fOnSoftBypass) then
  begin
   fOnSoftBypass(Self, onOff);
   Result := True;
  end
 else Result := False;
end;

function TCustomVSTModule.getEffectName(AName: pchar): Boolean;
begin
 StrPCopy(AName, fEffectName);
 Result := true;
end;

function TCustomVSTModule.getErrorText(text: pchar): Boolean;
begin
  Result := FALSE;
end;

function TCustomVSTModule.getVendorString(text: pchar): Boolean;
begin
 StrPCopy(text, fVendorName);
 Result := true;
end;

function TCustomVSTModule.getProductString(text: pchar): Boolean;
begin
  if fProductName <> '' then
    StrPCopy(text, fProductName)
  else
    StrPCopy(text, fEffectName);
  Result := true;
end;

function TCustomVSTModule.getVendorVersion: Integer;
begin
 Result := feffect.Version;
end;

function TCustomVSTModule.canDo(text: pchar): Integer;
begin
 Result := 0;
 if StrComp(text, 'receiveVstEvents') = 0 then Result := 2*integer(receiveVstEvents in fCanDos)-1 else
 if StrComp(text, 'receiveVstMidiEvent') = 0 then Result := 2*integer(receiveVstMidiEvent in fCanDos)-1 else
 if StrComp(text, 'receiveVstTimeInfo') = 0 then Result := 2*integer(receiveVstTimeInfo in fCanDos)-1 else
 if StrComp(text, 'sendVstEvents') = 0 then Result := 2*integer(sendVstEvents in fCanDos)-1 else
 if StrComp(text, 'sendVstMidiEvent') = 0 then Result := 2*integer(sendVstMidiEvent in fCanDos)-1 else
 if StrComp(text, 'sendVstTimeInfo') = 0 then Result := 2*integer(sendVstTimeInfo in fCanDos)-1 else
 if StrComp(text, 'offline') = 0 then Result := 2*integer(offline in fCanDos)-1 else
 if StrComp(text, 'plugAsChannelInsert') = 0 then Result := 2*integer(plugAsChannelInsert in fCanDos)-1 else
 if StrComp(text, 'plugAsSend') = 0 then Result := 2*integer(plugAsSend in fCanDos)-1 else
 if StrComp(text, 'mixDryWet') = 0 then Result := 2*integer(mixDryWet in fCanDos)-1 else
 if StrComp(text, 'noRealTime') = 0 then Result := 2*integer(noRealTime in fCanDos)-1 else
 if StrComp(text, 'multipass') = 0 then Result := 2*integer(multipass in fCanDos)-1 else
 if StrComp(text, 'metapass') = 0 then Result := 2*integer(metapass in fCanDos)-1 else
 if StrComp(text, '1in1out') = 0 then Result := 2*integer(_1in1out in fCanDos)-1 else
 if StrComp(text, '1in2out') = 0 then Result := 2*integer(_1in2out in fCanDos)-1 else
 if StrComp(text, '2in1out') = 0 then Result := 2*integer(_2in1out in fCanDos)-1 else
 if StrComp(text, '2in2out') = 0 then Result := 2*integer(_2in2out in fCanDos)-1 else
 if StrComp(text, '2in4out') = 0 then Result := 2*integer(_2in4out in fCanDos)-1 else
 if StrComp(text, '4in2out') = 0 then Result := 2*integer(_4in2out in fCanDos)-1 else
 if StrComp(text, '4in4out') = 0 then Result := 2*integer(_4in4out in fCanDos)-1 else
 if StrComp(text, '4in8out') = 0 then Result := 2*integer(_4in8out in fCanDos)-1 else
 if StrComp(text, '8in4out') = 0 then Result := 2*integer(_8in4out in fCanDos)-1 else
 if StrComp(text, '8in8out') = 0 then Result := 2*integer(_8in8out in fCanDos)-1 else
 if StrComp(text, 'midiProgramNames') = 0 then Result := 2*integer(midiProgramNames in fCanDos)-1 else
 if StrComp(text, 'conformsToWindowRules') = 0 then Result := 2*integer(conformsToWindowRules in fCanDos)-1 else
 if StrComp(text, 'LiveWithoutToolbar') = 0 then Result := 2*integer(LiveWithoutToolbar in fCanDos)-1 else
 if StrComp(text, 'bypass') = 0 then Result := 2*integer(bypass in fCanDos)-1;
 if assigned(fOnCanDo) then fOnCanDo(Self,text);
end;

function TCustomVSTModule.getIcon: pointer;
begin
 Result := nil;
end;

function TCustomVSTModule.setViewPosition(x, y: Integer): Boolean;
begin
 Result := FALSE;
end;

function TCustomVSTModule.fxIdle: Integer;
begin
 Result := 0;
end;

function TCustomVSTModule.getParameterProperties(Index: Integer; p: PVstParameterProperties): Boolean;
var str: string;
begin
 Result := ParameterProperties[Index].ReportVST2Properties;
 if Result then
  begin
   StrCopy(p^.vLabel,@ParameterProperties[Index].DisplayName[1]);
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
   if ppfIsSwitch in ParameterProperties[Index].Flags then p^.flags:=kVstParameterIsSwitch else p^.flags:=0;
   if ppfUsesIntegerMinMax in ParameterProperties[Index].Flags then p^.flags:=kVstParameterUsesIntegerMinMax;
   if ppfUsesFloatStep in ParameterProperties[Index].Flags then p^.flags:=kVstParameterUsesFloatStep;
   if ppfUsesIntStep in ParameterProperties[Index].Flags then p^.flags:=kVstParameterUsesIntStep;
   if ppfSupportsDisplayIndex in ParameterProperties[Index].Flags then p^.flags:=kVstParameterSupportsDisplayIndex;
   if ppfSupportsDisplayCategory in ParameterProperties[Index].Flags then p^.flags:=kVstParameterSupportsDisplayCategory;
   if ppfCanRamp in ParameterProperties[Index].Flags then p^.flags:=p^.flags+kVstParameterCanRamp;
  end;
end;

function TCustomVSTModule.getMidiProgramName(channel: Integer; midiProgramName: PMidiProgramName): Integer;
//var MPN: TMidiProgramName;
begin
// MPN.thisProgramIndex:=CurrentProgram;
// StrCopy(@MPN.name,@CurrentProgram);
// MPN.midiProgram:=CurrentProgram;
 Result := 0;
end;

function TCustomVSTModule.getCurrentMidiProgram(channel: Integer; currentProgram: PMidiProgramName): Integer;
begin
 Result := -1;
end;

function TCustomVSTModule.getMidiProgramCategory(channel: Integer; category: PMidiProgramCategory): Integer;
begin
 Result := 0;
end;

function TCustomVSTModule.hasMidiProgramsChanged(channel: Integer): Boolean;
begin
 Result := FALSE;
end;

function TCustomVSTModule.getMidiKeyName(channel: Integer; keyName: PMidiKeyName): Boolean;
begin
 Result := FALSE;
end;

function TCustomVSTModule.beginEdit(Index: Integer): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterBeginEdit, Index, 0, nil, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.endEdit(Index: Integer): Boolean;
begin
 if Assigned(fAudioMaster)
  then Result := (fAudioMaster(@fEffect, audioMasterEndEdit, Index, 0, nil, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.openFileSelector(ptr: PVstFileSelect): Boolean;
begin
 if Assigned(fAudioMaster) and (ptr <> nil)
  then Result := (fAudioMaster(@fEffect, audioMasterOpenFileSelector, 0, 0, ptr, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.closeFileSelector(ptr: PVstFileSelect): Boolean;
begin
 if Assigned(fAudioMaster) and (ptr <> nil)
  then Result := (fAudioMaster(@fEffect, audioMasterCloseFileSelector, 0, 0, ptr, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.getChunkFile(nativePath: pointer): Boolean;
begin
 if Assigned(fAudioMaster) and (nativePath <> nil)
  then Result := (fAudioMaster(@fEffect, audioMasterGetChunkFile, 0, 0, nativePath, 0) <> 0)
  else Result := FALSE;
end;

function TCustomVSTModule.getInputSpeakerArrangement: PVstSpeakerArrangement;
begin
 if Assigned(fAudioMaster)
  then Result := PVstSpeakerArrangement(fAudioMaster(@fEffect, audioMasterGetInputSpeakerArrangement, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.getOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
 if Assigned(fAudioMaster)
  then Result := PVstSpeakerArrangement(fAudioMaster(@fEffect, audioMasterGetOutputSpeakerArrangement, 0, 0, nil, 0))
  else Result := nil;
end;

function TCustomVSTModule.setTotalSampleToProcess(value: Integer): Integer;
begin
 Result := value;
end;

function TCustomVSTModule.getNextShellPlugin(const AName: pchar): Integer;
begin
 if fCurrentVstShellPlugin<fVstShellPlugins.Count then
  begin
   StrPCopy(AName,fVstShellPlugins[fCurrentVstShellPlugin].DisplayName);
   Result:=fVstShellPlugins[fCurrentVstShellPlugin].UID;
   Inc(fCurrentVstShellPlugin);
  end
 else
  begin
   Result:=0;
   fCurrentVstShellPlugin:=0;
  end;
end;

function TCustomVSTModule.startProcess: Integer;
begin
 Result := 1;
 if assigned(fOnStartProcess)
  then fOnStartProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.stopProcess: Integer;
begin
 Result := 1;
 if assigned(fOnStopProcess)
  then fOnStopProcess(Self)
  else Result := 0;
end;

function TCustomVSTModule.setPanLaw(var vType: Integer; var val: single): Boolean;
begin
 Result := True;
 if assigned(fOnSetPanLaw)
  then fOnSetPanLaw(Self, vType, val)
  else Result := false;
end;

function TCustomVSTModule.beginLoadBank(ptr: PVstPatchChunkInfo): Integer;
begin
 if ptr^.pluginUniqueID<>FEffect.uniqueID
  then Result := -1
  else Result := 0;
 if assigned(fOnBeginLoadBank)
  then fOnBeginLoadBank(Self, ptr^)
end;

function TCustomVSTModule.beginLoadProgram(ptr: PVstPatchChunkInfo): Integer;
begin
 if ptr^.pluginUniqueID<>FEffect.uniqueID
  then Result := -1
  else Result := 0;
 if assigned(fOnBeginLoadProgram)
  then fOnBeginLoadProgram(Self, ptr^)
end;

function TCustomVSTModule.allocateArrangement(var arrangement: PVstSpeakerArrangement; nbChannels: Integer): Boolean;
var size : Integer;
begin
 if Assigned(arrangement) then
  begin
   FreeMem(arrangement);
   arrangement := nil;
  end;

 size := SizeOf(Integer) + SizeOf(Integer) + (nbChannels) * SizeOf(TVstSpeakerProperties);
 GetMem(arrangement, size);
 if not Assigned(arrangement) then
  begin
   Result := FALSE;
   Exit;
  end;

 FillChar(arrangement^, size, 0);
 arrangement^.numChannels := nbChannels;
 Result := TRUE;
end;

function TCustomVSTModule.deallocateArrangement(var arrangement: PVstSpeakerArrangement): Boolean;
begin
 if Assigned(arrangement) then
  begin
   FreeMem(arrangement);
   arrangement := nil;
  end;
 Result := TRUE;
end;

function TCustomVSTModule.copySpeaker(copyTo, copyFrom: PVstSpeakerProperties): Boolean;
begin
  // We assume here that "to" exists yet, ie this function won't
  // allocate memory for the speaker (this will prevent from having
  // a difference between an Arrangement's number of channels and
  // its actual speakers...)
 if (copyFrom = nil) or (copyTo = nil) then
  begin
   Result := FALSE;
   Exit;
  end;

 StrCopy(copyTo^.name, copyFrom^.name);
 copyTo^.vType := copyFrom^.vType;
 copyTo^.azimuth := copyFrom^.azimuth;
 copyTo^.elevation := copyFrom^.elevation;
 copyTo^.radius := copyFrom^.radius;
 copyTo^.reserved := copyFrom^.reserved;
{$IFNDEF FPC}
 CopyMemory(@(copyTo^.future), @(copyFrom^.future), 28);
{$ELSE}
 Move(copyTo^.future, copyFrom^.future, 28);
{$ENDIF}

 Result := TRUE;
end;

function TCustomVSTModule.matchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): Boolean;
var i: integer;
begin
 if matchFrom = nil then
  begin
    Result := FALSE;
    Exit;
  end;

 if not deallocateArrangement(matchTo) or not allocateArrangement(matchTo, matchFrom^.numChannels) then
  begin
    Result := FALSE;
    Exit;
  end;

 matchTo^.vType := matchFrom^.vType;
 for i := 0 to matchTo^.numChannels-1 do
  begin
   if not copySpeaker(@(matchTo^.speakers[i]), @(matchFrom^.speakers[i])) then
    begin
     Result := FALSE;
     Exit;
    end;
  end;

 Result := FALSE;
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
 FDisplayName := 'Init'; // inherited GetDisplayName;
 fNumInputs:=-1;
 fNumOutputs:=-1;
 fNumPrograms:=-1;
 fNumParams:=-1;
 fPlugCategory:=cgUnknown;
 fVSTModule := (Collection As TCustomVstShellPlugins).VSTModule;
end;

destructor TCustomVstShellPlugin.Destroy;
begin
 try
 except
 end;
 inherited;
end;

procedure TCustomVstShellPlugin.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstShellPlugin then
  with TCustomVstShellPlugin(Dest) do
   begin
    DisplayName := Self.DisplayName;
   end
  else inherited;
end;

function TCustomVstShellPlugin.GetUniqueID:string;
var i : Integer;
begin
 result := '';
 for i := 3 downto 0
  do result := result + char(UID shr (i * 8));
end;

procedure TCustomVstShellPlugin.SetUniqueID(fID:string);
begin
 UID:=FourCharToLong(fID[1], fID[2], fID[3], fID[4])
end;

{$IFNDEF FPC}
procedure TCustomVstShellPlugin.SetDisplayName(const AValue: string);
begin
// inherited;
 fDisplayName:=copy(AValue,0,50);
end;

function TCustomVstShellPlugin.GetDisplayName: string;
begin
 Result := FDisplayName;
end;
{$ELSE}
procedure TCustomVstShellPlugin.SetDisplayName(const AValue: ansistring);
begin
 fDisplayName:=copy(AValue,0,50);
end;

function TCustomVstShellPlugin.GetDisplayName: ansistring;
begin
 Result := FDisplayName;
end;
{$ENDIF}

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
 fVstShellPlugins.Assign(Value);
end;

procedure TCustomVSTModule.SetKeysRequired(const Value: Boolean);
begin
 fkeysRequired := Value;
 updateDisplay;
end;

procedure TCustomVSTModule.SetEffectName(const Value: string);
begin
 fEffectName := Value;
end;

function TCustomVSTModule.GetHostProduct: string;
var text : pchar;
begin
 if fHostProduct='' then
  begin
   getmem(text, 64);
   try
    if getHostProductString(text)
     then Result:=shortstring(text)
     else Result:='Unknown';
   finally
    FreeMem(text);
    fHostProduct:=Result;
   end
  end
 else Result:=fHostProduct;
end;

function TCustomVSTModule.GetHostVendor: string;
var text : pchar;
begin
 getmem(text, 64);
 try
  if getHostVendorString(text)
   then Result:=shortstring(text)
   else Result:='Unknown';
 finally
  FreeMem(text);
 end;
end;

procedure TCustomVSTModule.SetVersionMajor(Value: Integer);
begin
  fVersionMajor := Value;
  UpdateVersion;
end;

procedure TCustomVSTModule.SetVersionMinor(Value: Integer);
begin
  fVersionMinor := Value;
  UpdateVersion;
end;

procedure TCustomVSTModule.SetVersionRelease(Value: Integer);
begin
  fVersionRelease := Value;
  UpdateVersion;
end;

procedure TCustomVSTModule.UpdateVersion;
begin
  fEffect.version := (fVersionMajor shl 16) + (fVersionMinor shl 8) +
    fVersionRelease;
end;

initialization
//  Set8087CW(Default8087CW or $3F);
{$IFDEF FPC}
  RegisterInitComponentHandler(TCustomVSTModule,@InitResourceComponent);
  RegisterInitComponentHandler(TVSTModule,@InitResourceComponent);
{$ENDIF}

end.
