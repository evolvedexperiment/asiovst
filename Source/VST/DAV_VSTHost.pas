{
This is a component to load a VST plugin and make its
properties and methods accessible, ie. show the interface,
fill the buffers and call the processing routines.

All host and plugin loading code was written by:
    Tobias Fleischer (http://www.tobybear.de)
    Christan W. Budde (http://www.savioursofsoul.de/Christian)

Delphi VST SDK tranlsation by:
    Frederic Vanmol (http://www.axiworld.be)

VST is a trademark of:
    Steinberg Media GmbH (http://www.steinberg.net)
}

unit DAV_VSTHost;

interface

{$I ..\DAV_Compiler.inc}

{$IFNDEF FPC}{$DEFINE MemDLL}{$ENDIF}

uses
  {$IFDEF FPC} LCLIntf, LResources, Dynlibs, {$ELSE} Windows, Messages, {$ENDIF}
  {$IFDEF MSWINDOWS} Registry, {$ENDIF} Contnrs, SysUtils, Classes, Graphics,
  {$IFDEF VstHostGUI} Controls, Forms, StdCtrls, ComCtrls, Dialogs,
  {$IFDEF FlatSrcollBar}TFlatScrollbarUnit, {$ENDIF}{$ENDIF}
  DAV_Common, DAV_AudioData, DAV_VSTEffect, DAV_VSTOfflineTask {$IFDEF MemDLL},
  DAV_DLLLoader{$ENDIF};

type
  {$IFDEF DELPHI10_UP} {$region 'General Types'} {$ENDIF}
  TVendorSpecificEvent = function(opcode : TAudioMasterOpcode; index, value: LongInt; ptr: Pointer; opt: Single): Integer of object;
  TVstAutomateEvent = procedure(Sender: TObject; Index, IntValue: LongInt; ParamValue: Single) of object;
  TVstProcessEventsEvent = procedure(Sender: TObject; p: PVstEvents) of object;
  TVstAutomationNotifyEvent = procedure(Sender: TObject; ParameterIndex: Integer) of object;
  TVstSampleRateChangedEvent = procedure(Sender: TObject; SampleRate: Single) of object;
  TVstPinConnectedEvent = function(Sender: TObject; PinNr: Integer; isInput: Boolean): Boolean of object;
  TVstOfflineEvent = procedure(Sender: TObject; VstOfflineTaskPointer: PVstOfflineTaskRecord) of object;
  {$IFDEF VstHostGUI}
  TVstShowEditEvent = procedure(Sender: TObject; Control: TWinControl) of object;
  TGUIStyle = (gsDefault, gsOld, gsList);
  {$ENDIF}

  // Reaper Extension Callbacks
  TGetPlayPosition = function: Double;
  TGetPlayPosition2 = function: Double;
  TGetCursorPosition = function: Double;
  TGetPlayState = function: Integer;
  TSetEditCurPos = procedure(Time: Double; MoveView, SeekPlay: Boolean);
  TGetSetRepeat =  function(Parm: Integer): Integer;
  TGetProjectPath =  procedure(Buffer: PAnsiChar; BufferSize: Integer);
  TOnPlayButton = procedure;
  TOnPauseButton = procedure;
  TOnStopButton = procedure;
  TIsInRealTimeAudio = function: Integer;
  TAudioIsRunning = function: Integer;


  THostCanDo = (hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo,
                hcdReceiveVstEvents, hcdReceiveVstMidiEvent,
                hcdReceiveVstTimeInfo, hcdReportConnectionChanges,
                hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing,
                hcdOffline, hcdSupplyIdle, hcdSupportShell, // 'shell' handling via uniqueID as suggested by Waves
                hcdOpenFileSelector, hcdCloseFileSelector, hcdEditFile,
                hcdShellCategory, hcdStartStopProcess);
  THostCanDos = set of THostCanDo;

  TKnobMode = (knCircular, knCircularRelativ, knLinear);

  TReplaceOrAccumulate = (roa0NotSupported, roa1Replace, roa2Accumulate);
  TCurrentProcessLevel = (cpl0NotSupported, cpl1UserThread, cpl2AudioThread,
   cpl3Sequencer, cpl4OfflineProcessing);
  TAutomationState = (as0NotSupported, as1Off, as2Read, as3Write, as4ReadWrite);

  TCustomVstHost = class;
  {$IFDEF DELPHI10_UP} {$endregion 'General Types'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TVstPlugIn'} {$ENDIF}
  TCustomVstPlugIn = class(TCollectionItem)
  private
    FActive                        : Boolean;
    FAutomationState               : TAutomationState;
    FDisplayName                   : string;
    FEditOpen                      : Boolean;
    FLoaded                        : Boolean;
    FVstDllFileName                : TFileName;
    FVstDllHandle                  : THandle;
    {$IFDEF MemDLL}
    FInternalDllLoader             : TDLLLoader;
    {$ENDIF}
    FMainFunction                  : TMainProc;
    FVstEffect                     : PVstEffect;
    FNeedIdle                      : Boolean;
    {$IFDEF VstHostGUI}
    FGUIControlCreated             : Boolean;
    FGUIStyle                      : TGUIStyle;
    FOnCloseEdit                   : TNotifyEvent;
    FOnShowEdit                    : TVstShowEditEvent;
    {$ENDIF}
    FOnAfterLoad                   : TNotifyEvent;
    FOnAMAutomate                  : TVstAutomateEvent;
    FOnAMBeginEdit                 : TVstAutomationNotifyEvent;
    FOnAMEndEdit                   : TVstAutomationNotifyEvent;
    FOnAMIdle                      : TNotifyEvent;
    FOnAMIOChanged                 : TNotifyEvent;
    FOnAMNeedIdle                  : TNotifyEvent;
    FOnAMOfflineGetCurrentMetaPass : TNotifyEvent;
    FOnAMOfflineGetCurrentPass     : TNotifyEvent;
    FOnAMOfflineRead               : TVstOfflineEvent;
    FOnAMOfflineStart              : TNotifyEvent;
    FOnAMOfflineWrite              : TVstOfflineEvent;
    FOnAMPinConnected              : TVstPinConnectedEvent;
    FOnAMSetOutputsampleRate       : TVstSampleRateChangedEvent;
    FOnAMUpdateDisplay             : TNotifyEvent;
    FOnAMWantMidi                  : TNotifyEvent;
    FOnProcessEvents               : TVstProcessEventsEvent;
    FOnVendorSpecific              : TVendorSpecificEvent;
    FPlugCategory                  : TVstPluginCategory;
    FProcessLevel                  : TCurrentProcessLevel;
    FProgramNr                     : Integer;
    FReplaceOrAccumulate           : TReplaceOrAccumulate;
    FVstOfflineTasks               : TOwnedCollection;
    FVstVersion                    : Integer;
    FWantMidi                      : Boolean;
    FVSTCanDos                     : TVstCanDos;
    FVSTCanDosScannedComplete      : Boolean;
    function GetEffOptions: TEffFlags;
    function GetInitialDelay: Integer;
    function GetIORatio: Single;
    function GetNumInputs: Integer;
    function GetNumOutputs: Integer;
    function GetNumParams: Integer;
    function GetNumPrograms: Integer;
    function GetOffQualities: LongInt;
    function GetPreset(ProgramNo: Integer): TFXPreset;
    function GetRealQualities: LongInt;
    function GetUniqueID: string;
    function GetVersion: Integer;
    function VstDispatch(const opCode : TDispatcherOpcode; const Index: Integer = 0; const value: Integer = 0; const pntr: Pointer = nil; const opt: Single = 0): Integer; {overload;} //virtual;
    procedure InitializeVstEffect;
    procedure SetActive(const Value: Boolean);
    procedure SetBlockSize(const Value: Integer);
    procedure SetVstDllFileName(const Value: TFilename);
    {$IFDEF VstHostGUI}
    procedure SetGUIStyle(const Value: TGUIStyle);
    procedure FormCloseHandler(Sender: TObject; var Action: TCloseAction);
    procedure ListParamChange(Sender: TObject);
    procedure ParamChange(Sender: TObject);
    procedure EditActivateHandler(Sender: TObject);
    procedure EditDeactivateHandler(Sender: TObject);
    {$IFDEF FlatSrcollBar}
    procedure ScrollChange(Sender: TObject; ScrollPos: Integer);
    {$ELSE}
    procedure TrackChange(Sender: TObject);
    {$ENDIF}
    {$ENDIF}
    function GetVSTCanDos: TVstCanDos;
  protected
    {$IFDEF VstHostGUI}
    FGUIControl  : TWinControl;
    FGUIElements : TObjectList;
    procedure EditClose;
    {$ENDIF}
    procedure AssignTo(Dest: TPersistent); override;
    procedure IOchanged;
    procedure NeedIdle;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function BeginLoadBank(const PatchChunkInfo : PVstPatchChunkInfo): Integer;
    function BeginLoadProgram(const PatchChunkInfo : PVstPatchChunkInfo): Integer;
    function CanBeAutomated(const Index: Integer): Integer;
    function VstCanDo(const CanDoString: string): Integer;
    function ConnectInput(const InputNr: Integer; const State: Boolean): Integer;
    function ConnectOutput(const OutputNr: Integer; const State: Boolean): Integer;
    function CopyCurrentProgramTo(const Destination: Integer): Boolean;
    function GetChunk(const Data: Pointer; const IsPreset: Boolean = False): Integer;
    function GetCurrentMidiProgram(var MidiProgramName: TMidiProgramName): Integer;
    function GetCurrentPosition: Integer;
    function GetDestinationBuffer: Integer;
    function GetDisplayName: string; override;
    function GetEffectName: string;
    function GetErrorText: string;
    function GetFriendlyNameString(const StringLength: Integer): string;
    function GetIcon: Integer;
    function GetInputProperties(const InputNr: Integer): TVstPinProperties;
    function GetMidiKeyName(var MidiKeyName: TMidiKeyName): Integer;
    function GetMidiProgramCategory(var MidiProgramCategory: TMidiProgramCategory): Integer;
    function GetMidiProgramName(var MidiProgramName: TMidiProgramName): Integer;
    function GetNumProgramCategories: Integer;
    function GetOutputProperties(OutputNr: Integer): TVstPinProperties;
    function GetParamDisplay(index: Integer): string;
    function GetParameter(index: Integer): Single; virtual;
    function GetParameterProperties(const Parameter: Integer): TVstParameterPropertyRecord;
    function GetParamLabel(index: Integer): string;
    function GetParamName(index: Integer): string;
    function GetPlugCategory: TVstPluginCategory;
    function GetProductString: string;
    function GetProgram: Integer;
    function GetProgramName: string;
    function GetProgramNameIndexed(const Category, Index: Integer; var ProgramName: string): Integer;
    function GetRect: TRect;
    function GetSpeakerArrangement(const SpeakerIn, SpeakerOut: PVstSpeakerArrangement): Integer;
    function GetTailSize: Integer;
    function GetVendorString: string;
    function GetVendorVersion: Integer;
    function GetVstVersion: Integer;
    function GetVu: Single;
    function HasMidiProgramsChanged: Integer;
    function Identify: Integer;
    function Idle: Integer;
    function KeysRequired: Integer;

    function OfflineNotify(var VstAudioFile: TVstAudioFile; const NumAudioFiles: Integer; const Start: Boolean): Integer;
    function OfflinePrepare(var VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count: Integer): Integer;
    function OfflineRun(var VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count :Integer): Integer;
    function ProcessEvents(var VstEvents: TVstEvents): Integer;
    function ProcessVarIo(var VarIo: TVstVariableIo): Integer;
    function SetBlockSizeAndSampleRate(const BlockSize: Integer; const SampleRate: Single): Integer;
    function SetBypass(const Value: Boolean): Integer;
    function SetChunk(const Data: Pointer; const ByteSize: Integer; const IsPreset: Boolean = False): Integer;
    function SetSpeakerArrangement(pluginInput: PVstSpeakerArrangement; pluginOutput: PVstSpeakerArrangement): Boolean;
    function ShellGetNextPlugin(var PluginName: string): Integer;
    function String2Parameter(const Index: Integer; const ParameterName: string): Integer;
    function VendorSpecific(const Index, Value: Integer; const Pntr: Pointer; const Opt: Single): Integer;
    procedure BeginSetProgram;
    procedure Close;
    {$IFDEF VstHostGUI}
    procedure CloseEdit;
    procedure EditActivate;
    procedure EditDeactivate;
    function EditGetRect: ERect;
    function EditIdle: Integer;
    function EditKeyDown(const Key: Char; const VirtualKeycode: Integer; const Modifier: TVstModifierKeys): Boolean;
    function EditKeyUp(const Key: Char; const VirtualKeycode: Integer; const Modifier: TVstModifierKeys): Boolean;
    function EditOpen(Handle: THandle): Integer;
    {$ENDIF}
    procedure EndSetProgram;
    procedure LoadBank(FileName: TFileName); overload;
    procedure LoadBank(Stream: TStream); overload;
    procedure LoadPreset(FileName: TFileName); overload;
    procedure LoadPreset(Stream: TStream); overload;
    procedure LoadFromFile(const FileName: TFilename);
    {$IFDEF MemDLL}
    procedure LoadFromStream(const Stream: TStream);
    {$ENDIF}
    procedure LoadFromVSTEffect(const Value: PVSTEffect);

    procedure MainsChanged(const IsOn: Boolean);
    procedure Open;
    procedure Process(Inputs, Outputs: PPSingle; SampleFrames: Integer); virtual;
    procedure ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: Integer);
    procedure ProcessAudioDataCollection(Inputs, Outputs: TAudioDataCollection32); overload; virtual;
    procedure ProcessAudioDataCollection(Inputs, Outputs: TAudioDataCollection64); overload; virtual;
    procedure ProcessAudioDataCollectionInplace(AudioData: TAudioDataCollection32); overload; virtual;
    procedure ProcessAudioDataCollectionInplace(AudioData: TAudioDataCollection64); overload; virtual;
    procedure ProcessDoubleReplacing(Inputs, Outputs: ppDouble; SampleFrames: Integer); virtual;
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: Integer); virtual;
    procedure SaveBank(FileName: TFileName); overload;
    procedure SaveBank(Stream: TStream); overload;
    procedure SavePreset(FileName: TFileName); overload;
    procedure SavePreset(Stream: TStream); overload;
    procedure SetPanLaw(const PanLaw: TVstPanLawType; const Gain: Single);
    procedure SetParameter(index: Integer; parameter: Single); virtual;
    procedure SetProgram(const lValue: Integer);
    procedure SetProgramName(const newName: string);
    procedure SetSampleRate(const Value: Single);
    procedure SetTotalSampleToProcess;
    procedure SetViewPosition(const x, y: Integer);
    {$IFDEF VstHostGUI}
    procedure SetEditKnobMode(Mode : TKnobMode);
    procedure ShowEdit(Control: TWinControl); overload;
    procedure ShowDefaultEditOld(Control: TWinControl);
    procedure ShowDefaultEditList(Control: TWinControl);
    procedure ShowEdit; overload;
    {$ENDIF}
    procedure StartProcess;
    procedure StopProcess;
    procedure UnLoad;

    property Parameter[Index: Integer]: Single read GetParameter write SetParameter;
    property ParameterName[Index: Integer]: string read GetParamName;
    property ParameterDisplay[Index: Integer]: string read GetParamDisplay;
    property ParameterLabel[Index: Integer]: string read GetParamLabel;
    property VstOfflineTasks: TOwnedCollection read FVstOfflineTasks write FVstOfflineTasks;

    // properties based on TVSTEffect
    property InitialDelay: Integer read GetInitialDelay stored False;
    property numInputs: Integer read GetnumInputs stored False default -1 ;
    property numOutputs: Integer read GetnumOutputs stored False default -1 ;
    property numParams: Integer read GetnumParams stored False default -1;
    property numPrograms: Integer read GetnumPrograms stored False default -1 ;
    property EffectOptions: TEffFlags read GetEffOptions stored False;
    property RealQualities: LongInt read GetRealQualities stored False;
    property OffQualities: LongInt read GetOffQualities stored False;
    property IORatio: Single read GetIORatio stored False;
    property UniqueID: string read GetUniqueID stored False;
    property Version: Integer read GetVersion stored False default -1;

    property Active: Boolean read FActive write SetActive default False;
    property AutomationState: TAutomationState read FAutomationState Write FAutomationState default as0NotSupported;
    property CurrentProcessLevel: TCurrentProcessLevel read FProcessLevel write FProcessLevel default cpl0NotSupported;
    property DisplayName: string read GetDisplayName write FDisplayName;
    property DLLFileName: TFileName read FVstDllFileName write SetVstDllFileName;
    property EditVisible: Boolean read FEditOpen;
    property EffectName: string read GetEffectName;
    {$IFDEF VstHostGUI}
    property GUIControl: TWinControl read FGUIControl;
    property GUIStyle : TGUIStyle read fGUIStyle write SetGUIStyle default gsDefault;
    {$ENDIF}
    property Loaded: Boolean read FLoaded stored False;
    property PlugCategory: TVstPluginCategory read FPlugCategory stored False;
    property PluginVstVersion: Integer read FVstVersion stored False default -1;
    property ProductString: string read GetProductString stored False;
    property ProgramName: string read GetProgramName write SetProgramName;
    property ProgramNr: Integer read GetProgram write SetProgram default -1;
    property ReplaceOrAccumulate: TReplaceOrAccumulate read FReplaceOrAccumulate write FReplaceOrAccumulate default roa0NotSupported;
    property VendorString: string read GetVendorString stored False;
    property VendorVersion: Integer read GetVendorVersion stored False default -1;
    property VSTCanDos: TVstCanDos read GetVSTCanDos stored False;

    property OnAfterLoad: TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnAudioMasterAutomate: TVstAutomateEvent read FOnAMAutomate write FOnAMAutomate;
    property OnAudioMasterBeginEdit: TVstAutomationNotifyEvent read FOnAMBeginEdit write FOnAMBeginEdit;
    property OnAudioMasterEndEdit: TVstAutomationNotifyEvent read FOnAMEndEdit write FOnAMEndEdit;
    property OnAudioMasterIdle: TNotifyEvent read FOnAMIdle write FOnAMIdle;
    property OnAudioMasterIOChanged: TNotifyEvent read FOnAMIOChanged write FOnAMIOChanged;
    property OnAudioMasterNeedIdle: TNotifyEvent read FOnAMNeedIdle write FOnAMNeedIdle;
    property OnAudioMasterOfflineGetCurrentMetaPass: TNotifyEvent read FOnAMOfflineGetCurrentMetaPass write FOnAMOfflineGetCurrentMetaPass;
    property OnAudioMasterOfflineGetCurrentPass: TNotifyEvent read FOnAMOfflineGetCurrentPass write FOnAMOfflineGetCurrentPass;
    property OnAudioMasterOfflineRead: TVstOfflineEvent read FOnAMOfflineRead write FOnAMOfflineRead;
    property OnAudioMasterOfflineStart: TNotifyEvent read FOnAMOfflineStart write FOnAMOfflineStart;
    property OnAudioMasterOfflineWrite: TVstOfflineEvent read FOnAMOfflineWrite write FOnAMOfflineWrite;
    property OnAudioMasterPinConnected: TVstPinConnectedEvent read FOnAMPinConnected write FOnAMPinConnected;
    property OnAudioMasterSetOutputsampleRate: TVstSampleRateChangedEvent read FOnAMSetOutputsampleRate write FOnAMSetOutputsampleRate;
    property OnAudioMasterUpdateDisplay: TNotifyEvent read FOnAMUpdateDisplay write FOnAMUpdateDisplay;
    property OnAudioMasterWantMidi: TNotifyEvent read FOnAMWantMidi write FOnAMWantMidi;
    property OnProcessEvents: TVstProcessEventsEvent read FOnProcessEvents write FOnProcessEvents;
    property OnVendorSpecific: TVendorSpecificEvent read FOnVendorSpecific write FOnVendorSpecific;
    {$IFDEF VstHostGUI}
    property OnCloseEdit: TNotifyEvent read FOnCloseEdit write FOnCloseEdit;
    property OnShowEdit: TVstShowEditEvent read FOnShowEdit write FOnShowEdit;
    {$ENDIF}
  end;

  TVstPlugIn = class(TCustomVstPlugIn)
  published
    property Active;
    property AutomationState;
    property CurrentProcessLevel;
    property DisplayName;
    property DLLFileName;
    property EditVisible;
    property EffectOptions;
    property InitialDelay;
    property numInputs;
    property numOutputs;
    property numParams;
    property numPrograms;
    property PlugCategory;
    property PluginVstVersion;
    property ProductString;
    property ProgramName;
    property ProgramNr;
    property ReplaceOrAccumulate;
    property UniqueID;
    property VendorString;
    property VendorVersion;
    property Version;
    property VstOfflineTasks;
    property OnAfterLoad;
    property OnAudioMasterAutomate;
    property OnAudioMasterBeginEdit;
    property OnAudioMasterEndEdit;
    property OnAudioMasterIdle;
    property OnAudioMasterIOChanged;
    property OnAudioMasterNeedIdle;
    property OnAudioMasterOfflineGetCurrentMetaPass;
    property OnAudioMasterOfflineGetCurrentPass;
    property OnAudioMasterOfflineRead;
    property OnAudioMasterOfflineStart;
    property OnAudioMasterOfflineWrite;
    property OnAudioMasterPinConnected;
    property OnAudioMasterSetOutputsampleRate;
    property OnAudioMasterUpdateDisplay;
    property OnAudioMasterWantMidi;
    property OnProcessEvents;
    property OnVendorSpecific;
    {$IFDEF VstHostGUI}
    property GUIStyle;
    property OnCloseEdit;
    property OnShowEdit;
    {$ENDIF}
  end;

  TVstPlugIns = class(TOwnedCollection)
  private
    FOwner: TComponent;
    function GetVSTHost: TCustomVstHost;
    function GetItem(Index: Integer): TVstPlugIn;
    procedure SetItem(Index: Integer; const Value: TVstPlugIn);
  protected
    property Items[Index: Integer]: TVstPlugIn read GetItem write SetItem; default;
    property VstHost: TCustomVstHost read GetVSTHost; 
  public
    constructor Create(AOwner: TComponent);
    function Add: TVstPlugIn;
    function CloneAdd(Source: TVstPlugIn): TVstPlugIn;
    function Insert(Index: Integer): TVstPlugIn;
    procedure Delete(Index: Integer);
    property Count;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TVstPlugIn'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TVstTimeInformation'} {$ENDIF}
  TCustomVstTimeInformation = class(TPersistent)
  private
    FOnChange    : TNotifyEvent;
    function GetVTI(Index :Integer) : Integer;
    function GetVTIDouble(Index :Integer) : Double;
    function GetVTIflags :TVstTimeInfoFlags;
    procedure SetVTI(Index,Value :Integer);
    procedure SetVTIDouble(Index :Integer; Value: Double);
    procedure SetVTIflags(Flags:TVstTimeInfoFlags);
  protected
    FVstTimeInfo : TVstTimeInfo;
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    constructor Create;

    property BarStartPos: Double Index 5 read GetVTIDouble write SetVTIDouble;
    property CycleEndPos: Double Index 7 read GetVTIDouble write SetVTIDouble;
    property CycleStartPos: Double Index 6 read GetVTIDouble write SetVTIDouble;
    property Flags: TVstTimeInfoFlags read GetVTIflags Write SetVTIflags;
    property NanoSeconds: Double Index 2 read GetVTIDouble write SetVTIDouble;
    property PpqPos: Double Index 3 read GetVTIDouble write SetVTIDouble;
    property SamplePos: Double index 0 read GetVTIDouble write SetVTIDouble;
    property SampleRate: Double Index 1 read GetVTIDouble write SetVTIDouble;
    property SamplesToNextClock: LongInt index 4 read GetVTI write SetVTI default 0;
    property SmpteFrameRate: LongInt index 3 read GetVTI write SetVTI default 1;
    property SmpteOffset: LongInt index 2 read GetVTI write SetVTI default 0;
    property Tempo: Double Index 4 read GetVTIDouble write SetVTIDouble;
    property TimeSigDenominator: LongInt Index 1 read GetVTI write SetVTI default 4;
    property TimeSigNumerator: LongInt Index 0 read GetVTI write SetVTI default 4;
  end;

  TVstTimeInformation = class(TCustomVstTimeInformation)
  published
    property SamplePos;
    property SampleRate;
    property NanoSeconds;
    property PpqPos;
    property Tempo;
    property BarStartPos;
    property CycleStartPos;
    property CycleEndPos;
    property TimeSigNumerator;
    property TimeSigDenominator;
    property SmpteOffset;
    property SmpteFrameRate;
    property SamplesToNextClock;
    property Flags;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TVstTimeInformation'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TVstHost'} {$ENDIF}
  TCustomVstHost = class(TComponent)
  private
    FAutoIdle           : Boolean;
    FCheckStringLengths : Boolean;
    FInputLatency       : Integer;
    FLanguage           : TVstHostLanguage;
    FnumAutomatable     : Integer;
    FOnCreate           : TNotifyEvent;
    FOnDestroy          : TNotifyEvent;
    FOutputLatency      : Integer;
    FParamQuan          : Integer;
    FPlugInDir          : string;
    FProductString      : string;
    FVendorString       : string;
    FVendorVersion      : Integer;
    FVstPlugIns         : TVstPlugIns;
    FVTI                : TVstTimeInformation;
    function GetBlockSize : Integer;
    function GetHostCanDos: THostCanDos;
    function GetHostTempo: Single;
    function GetHostVersion: Integer;
    function GetItem(Index: Integer): TCustomVstPlugIn;
    function GetPluginCount: Integer;
    procedure SetBlockSize(bs: Integer);
    procedure SetHostCanDos(hcd: THostCanDos);
    procedure SetHostTempo(Tempo: Single);
    procedure SetHostVersion(hv: Integer);
    procedure SetVstPlugIns(const Value: TVstPlugIns);
    procedure VstTimeInfoChanged(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    property Items[Index: Integer]: TCustomVstPlugIn read GetItem; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateVstTimeInfo(const Samples: Word = 1);
    procedure ResetVstTimeInformation;

    property BlockSize: Integer read getBlockSize write setBlocksize default 2048;
    property CanDos: THostCanDos read getHostCanDos write setHostCanDos;
    property Count: Integer read GetPluginCount;
    property CheckStringLengths: Boolean read FCheckStringLengths write FCheckStringLengths default false;
    property Language: TVstHostLanguage read FLanguage write FLanguage default kVstLangEnglish;
    property LatencyInput: Integer read FInputLatency write FInputLatency default 0;
    property LatencyOutput: Integer read FOutputLatency write FOutputLatency default 0;
    property ManageIdleAutomaticly : Boolean read FautoIdle write FautoIdle;
    property NumAutomatableParameters : Integer read FnumAutomatable write FnumAutomatable default 0;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property ParameterQuantization : Integer read FParamQuan write FParamQuan default MAXINT;
    property PlugInDir: string read FPlugInDir write FPlugInDir;
    property ProductString: string read FProductString write FProductString;
    property Tempo: Single read getHostTempo write SetHostTempo;
    property VendorString: string read FVendorString write FVendorString;
    property VendorVersion: Integer read FVendorVersion write FVendorVersion;
    property VstPlugIns: TVstPlugIns read FVstPlugIns write SetVstPlugIns;
    property VstTimeInfo: TVstTimeInformation read FVTI write FVTI;
    property VstVersion: Integer read GetHostVersion write SetHostVersion;
  end;

  TVstHost = class(TCustomVstHost)
  published
    property BlockSize;
    property CanDos;
    property Language;
    property LatencyInput;
    property LatencyOutput;
    property ManageIdleAutomaticly;
    property NumAutomatableParameters;
    property OnCreate;
    property OnDestroy;
    property ParameterQuantization;
    property PlugInDir;
    property ProductString;
    property Tempo;
    property VendorString;
    property VendorVersion;
    property VstPlugIns;
    property VstTimeInfo;
    property VstVersion;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TVstHost'} {$ENDIF}

var
  AudioMaster : TAudioMasterCallbackFunc;

function string2Language(LanguageString: string): TVstHostLanguage;
function PlugCategory2String(Category: TVstPluginCategory):string;
function EffOptions2String(EffOpts: TEffFlags):string;

implementation

{$IFDEF DELPHI10_UP} {$region 'Resource Strings'} {$ENDIF}
resourcestring
  RStrUnknown                    = 'Unknown';
  RStrEffect                     = 'Effect';
  RStrSynth                      = 'Synth';
  RStrAnalysis                   = 'Analysis';
  RStrMastering                  = 'Mastering';
  RStrSpacializer                = 'Spacializer';
  RStrRoomFx                     = 'RoomFx';
  RStrSurroundFx                 = 'SurroundFx';
  RStrRestoration                = 'Restoration';
  RStrOfflineProcess             = 'OfflineProcess';
  RStrShell                      = 'Shell';
  RStrGenerator                  = 'Generator';
  RStrVSTPluginNotValid          = 'This is not a valid Vst Plugin!';
  RStrNoEntryPoint               = 'DLL entry point could not be detected';
  RStrLoadingFailed              = 'Loading failed!';
  RStrFileDoesNotExist           = 'File %s does not exists';
  RStrPlugInCouldNotBeLoaded     = 'PlugIn %d could not be loaded';
  RStrPlugInStreamError          = 'PlugIn could not be loaded from stream';
  RStrBankFileDoesNotExist       = 'Bank file does not exist';
  StrPresetFileDoesNotExist      = 'Preset file does not exist';
  RStrBankFileNotForThisPlugin   = 'Bank file not for this plugin!';
  RStrPresetFileNotForThisPlugin = 'Preset file not for this plugin!';
  RStrCloseEditorFirst           = 'Close editor first!';
  RStrValue                      = 'Value';
{$IFDEF DELPHI10_UP} {$endregion 'Resource Strings'} {$ENDIF}

var
  FBlockSize     : Integer = 2048;
  FHostVersion   : Integer = 2300;
  FHostCanDos    : THostCanDos;
  FHostTempo     : Single = 120;
  FSampleRate    : Single  = 44100;
  {$IFDEF SearchPluginAndHost}
  HostList       : TObjectList;
  {$ENDIF}
  {$IFDEF VstHostGUI}
  HostDialog     : TCommonDialog;
  HostWindows    : TObjectList;
  {$ENDIF}

const
  SCRound8087CW     : Word = $133F; // round FPU codeword, with exceptions disabled
  SCChop8087CW      : Word = $1F3F; // Trunc (chop) FPU codeword, with exceptions disabled
  SCRoundDown8087CW : Word = $173F; // exceptions disabled
  SCRoundUp8087CW   : Word = $1B3F; // exceptions disabled

///////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'General Functions'} {$ENDIF}

function WhatIfNoEntry: Integer;
begin
 raise Exception.Create(RStrNoEntryPoint);
 result := 1;
end;

function string2Language(LanguageString : string): TVSTHostLanguage;
begin
 if      LanguageString = 'English'  then Result := kVstLangEnglish
 else if LanguageString = 'French'   then Result := kVstLangGerman
 else if LanguageString = 'German'   then Result := kVstLangFrench
 else if LanguageString = 'Italian'  then Result := kVstLangItalian
 else if LanguageString = 'Japanese' then Result := kVstLangSpanish
 else if LanguageString = 'Spanish'  then Result := kVstLangJapanese
 else Result := kVstLangEnglish
end;

function PlugCategory2String(Category: TVstPluginCategory): string;
begin
  case Category of
    vpcUnknown        : Result := RStrUnknown;
    vpcEffect         : Result := RStrEffect;
    vpcSynth          : Result := RStrSynth;
    vpcAnalysis       : Result := RStrAnalysis;
    vpcMastering      : Result := RStrMastering;
    vpcSpacializer    : Result := RStrSpacializer;
    vpcRoomFx         : Result := RStrRoomFx;
    vpcSurroundFx     : Result := RStrSurroundFx;
    vpcRestoration    : Result := RStrRestoration;
    vpcOfflineProcess : Result := RStrOfflineProcess;
    vpcShell          : Result := RStrShell;
    vpcGenerator      : Result := RStrGenerator;
  end;
end;

function EffOptions2String(EffOpts: TEffFlags): string;
begin
 Result := '';
 if effFlagsHasEditor     in EffOpts then Result := Result + 'HasEditor, ';
 if effFlagsHasClip       in EffOpts then Result := Result + 'HasClip, ';
 if effFlagsHasVu         in EffOpts then Result := Result + 'HasVU, ';
 if effFlagsCanMono       in EffOpts then Result := Result + 'CanMono, ';
 if effFlagsCanReplacing  in EffOpts then Result := Result + 'CanReplacing, ';
 if effFlagsProgramChunks in EffOpts then Result := Result + 'ProgramChunks, ';
 if effFlagsIsSynth       in EffOpts then Result := Result + 'IsSynth, ';
 if effFlagsNoSoundInStop in EffOpts then Result := Result + 'NoSoundInStop, ';
 if effFlagsExtIsAsync    in EffOpts then Result := Result + 'ExtIsAsync, ';
 if effFlagsExtHasBuffer  in EffOpts then Result := Result + 'ExtHasBuffer, ';

 if Length(Result) > 2 then Result := Copy(Result, 0, Length(Result) - 2)
end;

procedure DontRaiseExceptionsAndSetFPUcodeword;
asm
 fnclex                  // Don't raise pending exceptions enabled by the new flags
 fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
end;

function AudioMasterCallback(const Effect: PVstEffect; const Opcode : TAudioMasterOpcode; const Index, Value: LongInt; const Ptr: Pointer; const Opt: Single): LongInt; cdecl;
var
  thePlug : TCustomVstPlugIn;
  theHost : TCustomVstHost;
  i       : Integer;
  {$IFDEF SearchPluginAndHost}
  PlugNr  : Integer;
  {$ENDIF}
begin
 result := 0;
 try
  if assigned(Effect) then
   begin
    thePlug := Effect.ReservedForHost;
    theHost := Effect.Resvd2;
   end
  else
   begin
    // check for REAPER extension
    if (Ptr <> nil) and (Cardinal(Opcode) = $DEADBEEF) and (Cardinal(Index) = $DEADBEEF) then
     begin
      if PAnsiChar(Ptr) = 'GetPlayPosition' then else
      if PAnsiChar(Ptr) = 'GetPlayPosition2' then else
      if PAnsiChar(Ptr) = 'GetCursorPosition' then else
      if PAnsiChar(Ptr) = 'GetPlayState' then else
      if PAnsiChar(Ptr) = 'SetEditCurPos' then else
      if PAnsiChar(Ptr) = 'GetSetRepeat' then else
      if PAnsiChar(Ptr) = 'GetProjectPath' then else
      if PAnsiChar(Ptr) = 'OnPlayButton' then else
      if PAnsiChar(Ptr) = 'OnPauseButton' then else
      if PAnsiChar(Ptr) = 'OnStopButton' then else
      if PAnsiChar(Ptr) = 'IsInRealTimeAudio' then else
      if PAnsiChar(Ptr) = 'Audio_IsRunning' then else;
      exit;
     end;

    thePlug := nil;
    theHost := nil;

    {$IFDEF SearchPluginAndHost}
    // find plugin in host list
    for i := 0 to HostList.Count - 1 do
     with TCustomVstHost(HostList[i]) do
      begin
       assert(assigned(VstPlugIns));
       for PlugNr := 0 to VstPlugIns.Count - 1 do
        if VstPlugIns[PlugNr].FVstEffect = Effect then
         begin
          thePlug := VstPlugIns[PlugNr];
          theHost := TCustomVstHost(HostList[i]);
          Break;
         end;
       if assigned(thePlug) then break;
      end;
   {$ENDIF}
   end;

  DontRaiseExceptionsAndSetFPUcodeword;

  case TAudiomasterOpcode(opcode) of
   audioMasterAutomate                    : begin
                                             if Assigned(thePlug) then
                                              if Assigned(thePlug.FOnAMAutomate)
                                               then thePlug.FOnAMAutomate(thePlug, Index, Value, opt);
                                             result := 0;
                                            end;
   audioMasterVersion                     : result := FHostVersion;
   audioMasterIdle                        : if Assigned(thePlug) then
                                             begin
                                              {$IFDEF VstHostGUI}
                                              thePlug.FNeedIdle := True;
                                              if Assigned(thePlug.FOnAMIdle)
                                               then thePlug.FOnAMIdle(thePlug);
                                              if assigned(theHost) and theHost.FautoIdle then
                                               begin
                                                if thePlug.EditVisible then thePlug.EditIdle;
                                                for i := 0 to theHost.VstPlugIns.Count - 1 do // Norm-Konform!
                                                 if theHost.VstPlugIns[i].EditVisible then theHost.VstPlugIns[i].EditIdle;
                                               end;
                                              {$ENDIF}
                                             end;
   audioMasterCurrentId                   : if thePlug <> nil
                                             then thePlug.Identify
                                             else result := 0; // returns the unique id of a plug that's currently loading
   audioMasterPinConnected                : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMPinConnected)
                                              then
                                               begin
                                                if thePlug.FOnAMPinConnected(thePlug, Index, value = 0)
                                                 then Result := 0
                                                 else Result := 1;
                                               end
                                              else result := 0
                                             else result := 0;
   audioMasterWantMidi                    : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMWantMidi)
                                              then thePlug.FOnAMWantMidi(thePlug);
   audioMasterGetTime                     : if assigned(theHost)
                                             then Result := LongInt(@theHost.VstTimeInfo.FVstTimeInfo)
                                             else Result := 0;
   audioMasterProcessEvents               : if Assigned(thePlug.FOnProcessEvents)
                                             then thePlug.FOnProcessEvents(thePlug, ptr);
   audioMasterSetTime                     : {$IFDEF Debug} raise Exception.Create('TODO: audioMasterSetTime, VstTimenfo* in <ptr>, filter in <value>, not supported') {$ENDIF Debug};
   audioMasterTempoAt                     : result := round(FHostTempo) * 10000;
   audioMasterGetNumAutomatableParameters : if assigned(theHost)
                                             then result := theHost.FnumAutomatable
                                             else result := 0;
   audioMasterGetParameterQuantization    : if assigned(theHost) then
                                             if Value = -1
                                              then result := theHost.FParamQuan
                                              else {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetParameterQuantization, returns the Integer value for +1.0 representation') {$ENDIF Debug}
                                             // or 1 if full Single float precision is maintained
                                             // in automation. parameter index in <value> (-1: all, any)
                                            else result := 0;
   audioMasterIOChanged                   : if Assigned(thePlug)
                                             then thePlug.IOChanged;
   audioMasterNeedIdle                    : if Assigned(thePlug) then
                                             begin
                                              thePlug.NeedIdle;
                                              if assigned(theHost) and theHost.FAutoIdle
                                               then thePlug.Idle;
                                             end;
   audioMasterSizeWindow                  : begin
                                             {$IFDEF VstHostGUI}
                                             if Assigned(thePlug) then
                                              if pos('DASH', uppercase(thePlug.VendorString)) > 0
                                               then result := 0
                                              else if pos('WUSIK', uppercase(thePlug.VendorString)) > 0
                                               then result := 0 else
                                              if assigned(thePlug.GUIControl) then
                                               begin
                                                thePlug.GUIControl.ClientWidth := index;
                                                thePlug.GUIControl.ClientHeight := value;
                                                Result := 1;
                                               end;
                                             {$ENDIF}
                                            end;
   audioMasterGetSampleRate               : result := round(FSampleRate);
   audioMasterGetBlockSize                : result := FBlockSize;
   audioMasterGetInputLatency             : if assigned(theHost)
                                             then result := theHost.FInputLatency
                                             else result := 0;
   audioMasterGetOutputLatency            : if assigned(theHost)
                                             then result := theHost.FOutputLatency
                                             else result := 0;
   audioMasterGetPreviousPlug             : begin
//                                              if PlugNr = 0 then Result := 0;
                                             {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetPreviousPlug, input pin in <value> (-1: first to come), returns cEffect*') {$ENDIF Debug};
                                            end;
   audioMasterGetNextPlug                 : {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetNextPlug, output pin in <value> (-1: first to come), returns cEffect*') {$ENDIF Debug};
   audioMasterWillReplaceOrAccumulate     : if thePlug <> nil then result := Integer(thePlug.FReplaceOrAccumulate) else result := 0;
   audioMasterGetCurrentProcessLevel      : if thePlug <> nil then result := Integer(thePlug.FProcessLevel) else result := 0;
   audioMasterGetAutomationState          : if thePlug <> nil then result := Integer(thePlug.FAutomationState) else result := 0;
   audioMasterOfflineStart                : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMOfflineStart)
                                              then thePlug.FOnAMOfflineStart(thePlug); // audioMasterOfflineStart
   audioMasterOfflineRead                 : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMOfflineRead)
                                              then thePlug.FOnAMOfflineRead(thePlug,ptr); // audioMasterOfflineRead, ptr points to offline structure, see below. return 0: error, 1 ok
   audioMasterOfflineWrite                : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMOfflineWrite)
                                              then thePlug.FOnAMOfflineWrite(thePlug,ptr); // audioMasterOfflineWrite, same as read
   audioMasterOfflineGetCurrentPass       : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMOfflineGetCurrentPass)
                                              then thePlug.FOnAMOfflineGetCurrentPass(thePlug); // audioMasterOfflineGetCurrentPass
   audioMasterOfflineGetCurrentMetaPass   : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMOfflineGetCurrentMetaPass)
                                              then thePlug.FOnAMOfflineGetCurrentMetaPass(thePlug); // audioMasterOfflineGetCurrentMetaPass
   audioMasterSetOutputsampleRate         : begin
                                             if Assigned(thePlug) then
                                              if Assigned(thePlug.FOnAMSetOutputsampleRate)
                                               then thePlug.FOnAMSetOutputsampleRate(thePlug,opt);
                                             FSampleRate := opt; // raise Exception.Create('audioMasterSetOutputsampleRate, for variable i/o, sample rate in <opt>');
                                            end;
   audioMasterGetOutputspeakerArrangement : {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetSpeakerArrangement, (long)input in <value>, output in <ptr>') {$ENDIF Debug};
   audioMasterGetVendorString             : begin
                                             if assigned(theHost)
                                              then StrCopy(PAnsiChar(ptr), PAnsiChar(theHost.VendorString))
                                              else StrCopy(PAnsiChar(ptr), 'Delphi ASIO & VST Project');
                                             result := 1;
                                            end;
   audioMasterGetProductString            : try
                                             result := 1;
                                             if assigned(theHost)
                                              then StrCopy(PAnsiChar(ptr), PAnsiChar(theHost.ProductString))
                                              else
                                               if assigned(ptr)
                                                then StrCopy(PAnsiChar(ptr), 'Delphi VST Host')
                                                else result := 0;
                                            except
                                             result := 0;
                                            end;
   audioMasterGetVendorVersion            : if assigned(theHost)
                                             then result := theHost.FVendorVersion
                                             else result := 0;
   audioMasterVendorSpecific              : if assigned(thePlug) then
                                             if assigned(thePlug.FOnVendorSpecific)
                                              then result := thePlug.FOnVendorSpecific(TAudiomasterOpcode(opcode), index, value, ptr, opt)
                                              else result := 0
                                             else result := 0;
   audioMasterSetIcon                     : {$IFDEF Debug} ShowMessage('TODO: audioMasterSetIcon, void* in <ptr>, format not defined yet, Could be a CBitmap .') {$ENDIF Debug};
   audioMasterCanDo                       : begin
                                             if not assigned(ptr) then result := 0 else 
                                             if      ShortString(PAnsiChar(ptr)) = 'sendVstEvents' then Result := Integer(hcdSendVstEvents in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'sendVstMidiEvent' then Result := Integer(hcdSendVstMidiEvent in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'sendVstTimeInfo' then Result := Integer(hcdSendVstTimeInfo in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'receiveVstEvents' then Result := Integer(hcdReceiveVstEvents in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'receiveVstMidiEvent' then Result := Integer(hcdReceiveVstMidiEvent in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'receiveVstTimeInfo' then Result := Integer(hcdReceiveVstTimeInfo in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'reportConnectionChanges' then Result := Integer(hcdReportConnectionChanges in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'acceptIOChanges' then Result := Integer(hcdAcceptIOChanges in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'sizeWindow' then Result := Integer(hcdSizeWindow in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'asyncProcessing' then Result := Integer(hcdAsyncProcessing in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'offline' then Result := Integer(hcdOffline in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'supplyIdle' then Result := Integer(hcdSupplyIdle in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'supportShell' then Result := Integer(hcdSupportShell in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'openFileSelector' then Result := Integer(hcdOpenFileSelector in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'closeFileSelector' then Result := Integer(hcdcloseFileSelector in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'editFile' then Result := Integer(hcdEditFile in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'shellCategory' then Result := Integer(hcdShellCategory in FHostCanDos)
                                             else if ShortString(PAnsiChar(ptr)) = 'startStopProcess' then Result := Integer(hcdStartStopProcess in FHostCanDos)
                                             else Result := 0;
                                            end;
   audioMasterGetLanguage                 : if assigned(theHost)
                                             then result := Integer(theHost.FLanguage)
                                             else result := Integer(kVstLangUnknown);
   audioMasterOpenWindow                  : if assigned(ptr) then
                                             begin
                                             {$IFDEF VstHostGUI}
                                              with (HostWindows.Items[HostWindows.Add(TForm.Create(theHost))] as TForm) do
                                               begin
                                                Caption := PVstWindow(ptr).title;
                                                Left    := PVstWindow(ptr).xPos;
                                                Top     := PVstWindow(ptr).xPos;
                                                Width   := PVstWindow(ptr).Width;
                                                Height  := PVstWindow(ptr).Height;
                                                case PVstWindow(ptr).Style of
                                                 0: BorderStyle := bsSizeToolWin;
                                                 1: BorderStyle := bsNone;
                                                end;
                                                Parent := PVstWindow(ptr).Parent;
                                               end;
                                              ShowMessage('Please contact me if this happens: Christian@Aixcoustic.com');
//                                               PVstWindow(ptr).winHandle := (HostWindows.Items[i] as TForm).Handle;
                                              {$ENDIF}
                                             end;
   audioMasterCloseWindow                 : begin
                                             {$IFDEF Debug}
                                             ShowMessage('TODO: audioMasterCloseWindow, ' +
                                             'close window, platform specific handle in <ptr>')
                                             {$ENDIF Debug};
                                            end;
   audioMasterGetDirectory                : if assigned(theHost)
                                             then result := LongInt(PAnsiChar(theHost.FPlugInDir));
   audioMasterUpdateDisplay               : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMUpdateDisplay)
                                              then thePlug.FOnAMUpdateDisplay(thePlug);
   audioMasterBeginEdit                   : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMBeginEdit)
                                              then thePlug.FOnAMBeginEdit(thePlug,index);
   audioMasterEndEdit                     : if Assigned(thePlug) then
                                             if Assigned(thePlug.FOnAMEndEdit)
                                              then thePlug.FOnAMEndEdit(thePlug,index);
   audioMasterOpenFileSelector            : begin
                                             {$IFDEF VstHostGUI}
                                             if (ptr <> nil) and not Assigned(HostDialog) then
                                              begin
                                               case PVstFileSelect(ptr).Command of
                                                kVstFileLoad:
                                                 begin
                                                  HostDialog := TOpenDialog.Create(theHost);
                                                  with TOpenDialog(HostDialog) do
                                                   begin
                                                    Title := PVstFileSelect(ptr).title;
                                                    InitialDir := PVstFileSelect(ptr).initialPath;
                                                    for i := 0 to PVstFileSelect(ptr).nbFileTypes - 1
                                                     do Filter := Filter + ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name) +
                                                                    ' (*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType +
                                                                    ')|*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType + '|';
                                                    if Execute then
                                                     begin
//                                                      PVstFileSelect(ptr).returnPath := PAnsiChar(TOpenDialog(HostDialog).FileName);
//                                                      StrCopy(PVstFileSelect(ptr).returnPath,PAnsiChar(TOpenDialog(HostDialog).FileName));
                                                      PVstFileSelect(ptr).sizeReturnPath := Length(FileName);
                                                     end;
                                                   end;
                                                 end;
                                                kVstFileSave:
                                                 begin
                                                  HostDialog := TSaveDialog.Create(theHost);
                                                  with TSaveDialog(HostDialog) do
                                                   begin
                                                    Title := PVstFileSelect(ptr).title;
                                                    InitialDir := PVstFileSelect(ptr).initialPath;
                                                    for i := 0 to PVstFileSelect(ptr).nbFileTypes - 1
                                                     do Filter := Filter +
                                                      ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name) +
                                                      ' (*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType +
                                                      ')|*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType + '|';
                                                    if Execute then
                                                    begin
                                                     PVstFileSelect(ptr).returnPath := PAnsiChar(FileName);
                                                     PVstFileSelect(ptr).sizeReturnPath := Length(FileName);
                                                    end;
                                                   end;
                                                 end;
                                                kVstMultipleFilesLoad:
                                                begin
                                                 HostDialog := TOpenDialog.Create(theHost);
                                                 with TOpenDialog(HostDialog) do
                                                  begin
                                                   Title := PVstFileSelect(ptr).title;
                                                   InitialDir := PVstFileSelect(ptr).initialPath;
                                                   for i := 0 to PVstFileSelect(ptr).nbFileTypes - 1 do
                                                    Filter := Filter +
                                                    ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name) +
                                                    ' (*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType +
                                                    ')|*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType + '|';
                                                    if TOpenDialog(HostDialog).Execute
                                                     then
                                                      begin
                                                       PVstFileSelect(ptr).returnPath := PAnsiChar(FileName);
                                                       PVstFileSelect(ptr).sizeReturnPath := Length(FileName);
                                                      end;
                                                  end;
                                                 end;
                                                kVstDirectorySelect: {$IFDEF Debug} ShowMessage('TODO: Not implemented!') {$ENDIF Debug};
                                               end;
                                               if PVstFileSelect(ptr).returnPath = nil
                                                then FreeAndNil(HostDialog);
                                               Result := Integer(True);
                                              end;
                                             {$ENDIF}
                                            end;
   audioMasterCloseFileSelector           : begin
                                             {$IFDEF VstHostGUI}
                                             if assigned(HostDialog) then
                                              case PVstFileSelect(ptr).Command of
                                               kVstFileLoad:
                                                if TOpenDialog(HostDialog).Title = PVstFileSelect(ptr).title
                                                 then FreeAndNil(HostDialog);
                                               kVstFileSave:
                                                if TSaveDialog(HostDialog).Title = PVstFileSelect(ptr).title
                                                 then FreeAndNil(HostDialog);
                                               kVstMultipleFilesLoad:
                                                if TOpenDialog(HostDialog).Title = PVstFileSelect(ptr).title
                                                 then FreeAndNil(HostDialog);
                                               kVstDirectorySelect:
                                                begin
                                                end;
                                               else
                                                {$IFDEF Debug} raise Exception.Create('TODO: close a fileselector operation with VstFileSelect* in <ptr>: Must be always called after an open !') {$ENDIF Debug};
                                              end;
                                             {$ENDIF}
                                            end;
    audioMasterEditFile                   : {$IFDEF Debug} raise Exception.Create('TODO: open an editor for audio (defined by XML text in ptr') {$ENDIF Debug};
    audioMasterGetChunkFile               : {$IFDEF Debug} raise Exception.Create('TODO: get the native path of currently loading bank or project') {$ENDIF Debug};
   else
    try
      raise Exception.Create('Check: ' + IntToStr(Integer(opcode)) + ' - ' +
                                         IntToStr(index) + ' - ' +
                                         IntToStr(value) + ' - ' +
                                         FloatToStr(opt));
    except
      result := 0;
    end;
  end;
 except
  result := 0;
  {$IFDEF Debug}
  raise;
  {$ENDIF}
 end;
end;
{$IFDEF DELPHI10_UP} {$endregion 'General Functions'} {$ENDIF}

///////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomVstTimeInformation implementation'} {$ENDIF}

constructor TCustomVstTimeInformation.Create;
begin
 with FVstTimeInfo do
  begin
   SampleRate         :=  44100;
   timeSigNumerator   := 4;
   timeSigDenominator := 4;
   smpteFrameRate     := 1;
   samplePos          := 0;
   ppqPos             := 0;
  end;
 Flags := [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid,
           vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid];
end;

procedure TCustomVstTimeInformation.Change;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomVstTimeInformation.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstTimeInformation then
  with TCustomVstTimeInformation(Dest) do
   try
    FVstTimeInfo := Self.FVstTimeInfo;
   finally
    Change;
   end
 else inherited AssignTo(Dest);
end;

function TCustomVstTimeInformation.GetVTIflags: TVstTimeInfoFlags;
begin
 result := FVstTimeInfo.Flags;
end;

function TCustomVstTimeInformation.GetVTIDouble(Index: Integer): Double;
begin
 Result := 0;
 with FVstTimeInfo do
  case Index of
   0: Result := samplePos;
   1: Result := sampleRate;
   2: Result := nanoSeconds;
   3: Result := ppqPos;
   4: Result := tempo;
   5: Result := barStartPos;
   6: Result := cycleStartPos;
   7: Result := cycleEndPos;
  end;
end;

function TCustomVstTimeInformation.GetVTI(Index: Integer) :Integer;
begin
 Result := 0;
 with FVstTimeInfo do
  case Index of
   0: Result := timeSigNumerator;
   1: Result := timeSigDenominator;
   2: Result := smpteOffset;
   3: Result := smpteFrameRate;
   4: Result := samplesToNextClock;
  end;
end;

procedure TCustomVstTimeInformation.SetVTI(Index, Value: Integer);
begin
 with FVstTimeInfo do
  case Index of
   0: if Value <> timeSigNumerator then begin timeSigNumerator := Value; Change; end;
   1: if Value <> timeSigDenominator then begin timeSigDenominator := Value; Change; end;
   2: if Value <> smpteOffset then begin smpteOffset := Value; Change; end;
   3: if Value <> smpteFrameRate then begin smpteFrameRate := Value; Change; end;
   4: if Value <> samplesToNextClock then begin samplesToNextClock := Value; Change; end;
  end;
end;

procedure TCustomVstTimeInformation.SetVTIDouble(Index: Integer; Value: Double);
begin
 with FVstTimeInfo do
  case Index of
   0: if Value <> samplePos then begin SamplePos := Value; Change; end;
   1: if Value <> sampleRate then begin sampleRate := Value; FsampleRate := Value; Change; end;
   2: if Value <> nanoSeconds then begin nanoSeconds := Value; Change; end;
   3: if Value <> ppqPos then begin ppqPos := Value; Change; end;
   4: if Value <> tempo then begin tempo := Value; Change; end;
   5: if Value <> barStartPos then begin barStartPos := Value; Change; end;
   6: if Value <> cycleStartPos then begin cycleStartPos := Value; Change; end;
   7: if Value <> cycleEndPos then begin cycleEndPos := Value; Change; end;
  end;
end;

procedure TCustomVstTimeInformation.SetVTIflags(Flags: TVstTimeInfoFlags);
begin
 FVstTimeInfo.Flags := Flags;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

///////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomVstHost implementation'} {$ENDIF}

{ TCustomVstHost }

procedure TCustomVstHost.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstHost then
  with TCustomVstHost(Dest) do
   begin
    FAutoIdle           := Self.FAutoIdle;
    FCheckStringLengths := Self.FCheckStringLengths;
    FInputLatency       := Self.FInputLatency;
    FLanguage           := Self.FLanguage;
    FnumAutomatable     := Self.FnumAutomatable;
    FOnCreate           := Self.FOnCreate;
    FOnDestroy          := Self.FOnDestroy;
    FOutputLatency      := Self.FOutputLatency;
    FParamQuan          := Self.FParamQuan;
    FPlugInDir          := Self.FPlugInDir;
    FProductString      := Self.FProductString;
    FVendorString       := Self.FVendorString;
    FVendorVersion      := Self.FVendorVersion;
    FVTI.Assign(Self.FVTI);
    FVstPlugIns.Assign(Self.FVstPlugIns);
   end
 else inherited;
end;

constructor TCustomVstHost.Create(AOwner: TComponent);
begin
 inherited;
// if AOwner <> nil then
 {$IFDEF SearchPluginAndHost}
 HostList.Add(Self);
 {$ENDIF}
 FSampleRate := 44100;
 FBlocksize  := 2048;
 FLanguage   := kVstLangEnglish;
 FHostCanDos := [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo,
                 hcdReceiveVstEvents, hcdReceiveVstMidiEvent,
                 hcdReceiveVstTimeInfo, hcdReportConnectionChanges,
                 hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing,
                 hcdOffline, hcdSupplyIdle, hcdStartStopProcess];

 FCheckStringLengths := False;
 {$IFDEF MSWINDOWS}
 with TRegistry.Create do
  try
   RootKey := HKEY_LOCAL_MACHINE;
   OpenKey('SOFTWARE\Vst',False);

   if ValueExists('VstPluginsPath')
    then FPlugInDir := ReadString('VstPluginsPath')
    {$IFDEF VstHostGUI} else FPlugInDir := ExtractFileDir(Application.ExeName){$ENDIF};
   CloseKey;
  finally
   Free;
  end;
 {$ENDIF}
 try
  FVstPlugIns := TVstPlugIns.Create(Self);
  FVTI := TVstTimeInformation.Create;
  FVTI.OnChanged := VstTimeInfoChanged;
 finally
  if Assigned(FOnCreate) then FOnCreate(Self);
 end;
end;

destructor TCustomVstHost.Destroy;
begin
 try
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  if Assigned(FVstPlugIns) then
   begin
    while FVstPlugIns.Count > 0 do FVstPlugIns[0].Free;
 //   FreeAndNil(FVstPlugIns); do not use this or it will bite!!!
    FreeAndNil(FVstPlugIns);
   end;
  if Assigned(FVTI) then FreeAndNil(FVTI);
  {$IFDEF SearchPluginAndHost}
  assert(assigned(HostList));
  HostList.Remove(Self);
  {$ENDIF}
 finally
  inherited;
 end;
end;

procedure TCustomVstHost.VstTimeInfoChanged(Sender: TObject);
begin
 //
end;

function TCustomVstHost.GetItem(Index: Integer): TCustomVstPlugIn;
begin
 assert(assigned(FVstPlugIns));
 Result := FVstPlugIns[Index];
end;

function TCustomVstHost.GetPluginCount: Integer;
begin
 assert(assigned(FVstPlugIns));
 result := FVstPlugIns.Count;
end;

procedure TCustomVstHost.SetVstPlugIns(const Value: TVstPlugIns);
begin
 assert(assigned(FVstPlugIns));
 FVstPlugIns.Assign(Value);
end;

function TCustomVstHost.getHostTempo: Single;
begin
 Result := FHostTempo;
end;

procedure TCustomVstHost.setHostTempo(tempo: Single);
begin
 FHostTempo := tempo;
 if Assigned(VstTimeInfo)
  then VstTimeInfo.tempo := Tempo;
end;

function TCustomVstHost.getHostCanDos: THostCanDos;
begin
 Result := FHostCanDos;
end;

procedure TCustomVstHost.setHostCanDos(hcd: THostCanDos);
begin
 FHostCanDos := hcd;
end;

function TCustomVstHost.GetHostVersion: Integer;
begin
 Result := FHostVersion;
end;

procedure TCustomVstHost.SetHostVersion(hv: Integer);
begin
 FHostVersion := hv;
end;

function TCustomVstHost.GetBlockSize: Integer;
begin
 Result := FBlockSize;
end;

procedure TCustomVstHost.SetBlockSize(bs: Integer);
var
  i : Integer;
begin
 FBlockSize := bs;
 assert(assigned(VstPlugIns));
 for i := 0 to VstPlugIns.Count - 1 do
  if assigned( VstPlugIns[i].FVstEffect) then
   VstPlugIns[i].SetBlockSize(FBlockSize);
end;

procedure TCustomVstHost.UpdateVstTimeInfo(const Samples: Word = 1);
var
  p: Double;
begin
 if Assigned(FVTI) then
  with FVTI.FVstTimeInfo do
   begin
    samplePos := samplePos + samples;
    p := sampleRate * 240 / tempo;
    if samplePos >= p then samplePos := samplePos - p;
    ppqPos := (samplePos / sampleRate) * (tempo / 60);
   end;
end;

procedure TCustomVstHost.ResetVstTimeInformation;
begin
 if Assigned(FVTI) then
  with FVTI do
   begin
    FVstTimeInfo.samplePos := 0;
    FVstTimeInfo.ppqPos := 0;
   end;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TVstPlugIns implementation'} {$ENDIF}

{ TVstPlugIns }

function TVstPlugIns.Add: TVstPlugIn;
begin
  Result := TVstPlugIn(inherited Add);
end;

function TVstPlugIns.CloneAdd(Source: TVstPlugIn): TVstPlugIn;
begin
 Result := TVstPlugIn(inherited Add);
 Source.AssignTo(Result);
end;

constructor TVstPlugIns.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TVstPlugIn);
 FOwner := AOwner;
end;

function TVstPlugIns.GetItem(Index: Integer): TVstPlugIn;
begin
 Result := TVstPlugIn(inherited GetItem(Index));
end;

function TVstPlugIns.GetVSTHost: TCustomVstHost;
begin
 result := TCustomVstHost(FOwner);
end;

function TVstPlugIns.Insert(Index: Integer): TVstPlugIn;
begin
 Result := TVstPlugIn(inherited Insert(Index));
end;

procedure TVstPlugIns.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TVstPlugIns.SetItem(Index: Integer; const Value: TVstPlugIn);
begin
 inherited SetItem(Index, Value);
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomVstPlugIn implementation'} {$ENDIF}

{ TCustomVstPlugIn }

constructor TCustomVstPlugIn.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName       := inherited GetDisplayName;
 FMainFunction      := nil;
 FVstEffect         := nil;
 FEditOpen          := False;
 FNeedIdle          := False;
 FWantMidi          := False;
 FVstVersion        := -1;
 FPlugCategory      := vpcUnknown;
 FVstDllFileName    := '';
 {$IFDEF VstHostGUI}
 FGUIStyle          := gsDefault;
 FGUIControlCreated := False;
 {$ENDIF}
 FVstOfflineTasks   := TOwnedCollection.Create(Self, TVstOfflineTaskCollectionItem);
 Randomize;
end;

destructor TCustomVstPlugIn.Destroy;
begin
 try
  if Active then Close;
  if assigned(FVstOfflineTasks)
   then FreeAndNil(FVstOfflineTasks);
  if FVstDllFileName <> '' then
   try
    {$IFDEF VstHostGUI}
    if EditVisible then CloseEdit;
    if assigned(GUIControl) and FGUIControlCreated
     then FreeAndNil(FGUIControl);
    {$ENDIF}
   finally 
    Unload;
   end;
  {$IFDEF MemDLL}
  if assigned(FInternalDllLoader) then
   try
    {$IFDEF VstHostGUI}
    if EditVisible then CloseEdit;
    if assigned(GUIControl) and FGUIControlCreated
     then FreeAndNil(FGUIControl);
    {$ENDIF}
   finally
    FreeAndNil(FInternalDllLoader)
   end;
  {$ENDIF}
 finally
  inherited;
 end;
end;

procedure TCustomVstPlugIn.AssignTo(Dest: TPersistent);
var
  p: Pointer;
  i: Integer;
begin
 if Dest is TCustomVstPlugIn then with TCustomVstPlugIn(Dest) do
  begin
   // assign events
   FOnAfterLoad                   := Self.FOnAfterLoad;
   FOnAMAutomate                  := Self.FOnAMAutomate;
   FOnAMBeginEdit                 := Self.FOnAMBeginEdit;
   FOnAMEndEdit                   := Self.FOnAMEndEdit;
   FOnAMIdle                      := Self.FOnAMIdle;
   FOnAMIOChanged                 := Self.FOnAMIOChanged;
   FOnAMNeedIdle                  := Self.FOnAMNeedIdle;
   FOnAMOfflineGetCurrentMetaPass := Self.FOnAMOfflineGetCurrentMetaPass;
   FOnAMOfflineGetCurrentPass     := Self.FOnAMOfflineGetCurrentPass;
   FOnAMOfflineRead               := Self.FOnAMOfflineRead;
   FOnAMOfflineStart              := Self.FOnAMOfflineStart;
   FOnAMOfflineWrite              := Self.FOnAMOfflineWrite;
   FOnAMPinConnected              := Self.FOnAMPinConnected;
   FOnAMSetOutputsampleRate       := Self.FOnAMSetOutputsampleRate;
   FOnAMUpdateDisplay             := Self.FOnAMUpdateDisplay;
   FOnAMWantMidi                  := Self.FOnAMWantMidi;
   FOnProcessEvents               := Self.FOnProcessEvents;
   FOnVendorSpecific              := Self.FOnVendorSpecific;
   {$IFDEF VstHostGUI}
   FOnCloseEdit                   := Self.FOnCloseEdit;
   FOnShowEdit                    := Self.FOnShowEdit;
   {$ENDIF}

   FDisplayName                   := Self.FDisplayName;
   {$IFDEF VstHostGUI}
   FGUIStyle                      := Self.FGUIStyle;
   {$ENDIF}

   FVstOfflineTasks.Assign(Self.FVstOfflineTasks);

   if Self.FLoaded then
    begin
     if FileExists(Self.FVstDllFileName)
      then LoadFromFile(Self.FVstDllFileName) else
     {$IFDEF MemDLL}
     if assigned(Self.FInternalDllLoader)
      then FInternalDllLoader.Assign(Self.FInternalDllLoader) else
     {$ENDIF}
     if assigned(Self.FVstEffect)
      then LoadFromVSTEffect(Self.FVstEffect);

     Active := Self.FActive;

     {$IFDEF VstHostGUI}
     if Self.FEditOpen then
      begin
       ShowEdit(Self.FGUIControl);
      end;
     {$ENDIF}

     // copy chunk
     try
      i := Self.GetChunk(@p);
      TCustomVstPlugIn(Dest).SetChunk(p, i)
     except
     end;
    end
   else
    begin
     if FLoaded then Unload;
     FLoaded            := False;
     FActive            := False;
     FEditOpen          := False;
     FMainFunction      := nil;
     {$IFDEF VstHostGUI}
     FGUIControlCreated := False;
     {$ENDIF}
     FAutomationState           := Self.FAutomationState;
     FNeedIdle                  := Self.FNeedIdle;
     FPlugCategory              := Self.FPlugCategory;
     FProcessLevel              := Self.FProcessLevel;
     FProgramNr                 := Self.FProgramNr;
     FReplaceOrAccumulate       := Self.FReplaceOrAccumulate;
     FVstVersion                := Self.FVstVersion;
     FWantMidi                  := Self.FWantMidi;
     FVSTCanDos                 := Self.FVSTCanDos;
     FVSTCanDosScannedComplete  := Self.FVSTCanDosScannedComplete;
    end;
  end
 else inherited;
end;

function TCustomVstPlugIn.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TCustomVstPlugIn.GetEffOptions: TEffFlags;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect.EffectFlags
  else Result := [];
end;

procedure TCustomVstPlugIn.SetActive(const Value: Boolean);
begin
 if FActive <> Value then
  if Value then Open else Close;
end;

procedure TCustomVstPlugIn.Open;
var
  tmp : string;
begin
 if not FLoaded then exit;
 if FActive then Close;

 FActive   := True;
 FEditOpen := False;
 FNeedIdle := False;
 FWantMidi := False;

 if (Integer(FVstEffect.uniqueID) = 0) then
  with TStringList.Create do
   try
    while ShellGetNextPlugin(tmp) <> 0 do Add(tmp);
   finally
    Free;
   end;

 try
  VstDispatch(effOpen);
  if VstCanDo('bypass') <> 0
   then FVSTCanDos := [vcdBypass]
   else FVSTCanDos := [];
  FVSTCanDosScannedComplete := False;

  SetPanLaw(plLinear, sqrt(2));
  SetBlockSize(FBlocksize);
  SetSampleRate(FSampleRate);
  if vcdBypass in FVSTCanDos
   then SetBypass(False);
  FVstVersion   := GetVstVersion;
  FPlugCategory := GetPlugCategory;
  MainsChanged(True);
 except
  FActive := False;
 end;
end;

procedure TCustomVstPlugIn.Close;
begin
 {$IFDEF VstHostGUI}
 while FEditOpen do CloseEdit;
 {$ENDIF}
 if FActive then VstDispatch(effClose);
 FActive       := False;
 FPlugCategory := vpcUnknown;
end;

function TCustomVstPlugIn.VstDispatch(const opCode : TDispatcherOpcode; const Index, Value: Integer; const Pntr: Pointer; const opt: Single): Integer;
begin
 try
  DontRaiseExceptionsAndSetFPUcodeword;
  if not assigned(FVstEffect) then
   result := 0
  else
   result := Integer(opCode);
   result := FVstEffect.Dispatcher(FVstEffect, TDispatcherOpcode(result), index, value, pntr, opt);
 except
  result := 0;
 end;
end;

procedure TCustomVstPlugIn.Process(Inputs, Outputs: PPSingle; SampleFrames: Integer);
begin
 if FVstEffect <> nil
  then FVstEffect.Process(FVstEffect, Inputs, Outputs, SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: Integer);
begin
 if FVstEffect <> nil
  then FVstEffect.ProcessReplacing(FVstEffect, Inputs, Outputs, SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessDoubleReplacing(Inputs, Outputs: ppDouble; SampleFrames: Integer);
begin
 if FVstEffect <> nil
  then FVstEffect.ProcessDoubleReplacing(FVstEffect, Inputs, Outputs, SampleFrames);
end;

procedure TCustomVstPlugIn.SetParameter(Index:Integer; Parameter: Single);
begin
 if FVstEffect <> nil
  then FVstEffect.setParameter(FVstEffect, Index, Parameter);
end;

function TCustomVstPlugIn.GetParameter(Index: Integer): Single;
begin
 if FVstEffect = nil
  then result := 0
  else result := FVstEffect.getParameter(FVstEffect, Index);
end;

procedure TCustomVstPlugIn.SetProgram(const lValue: Integer);
begin
 if FActive and (FProgramNr <> lValue) then
  begin
   FProgramNr := lValue;
   VstDispatch(effSetProgram, 0, FProgramNr);
  end;
end;

function TCustomVstPlugIn.GetProgram: Integer;
begin
 if FActive
  then result := VstDispatch(effGetProgram)
  else result := -1;
 FProgramNr := result;
end;

procedure TCustomVstPlugIn.SetProgramName(const NewName: string);
begin
 if FActive
  then VstDispatch(effSetProgramName, 0, 0, PAnsiChar(NewName));
end;

function TCustomVstPlugIn.GetProgramName: string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetProgramName, 0, 0, Temp) = 0 // check is not part of the official specification
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if (TVSTPlugins(Collection).VSTHost.CheckStringLengths) and (Length(result) > 24)
    then raise Exception.Create('String too short');
 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetParamLabel(index: Integer): string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 result := '';

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetParamLabel, index, 0, Temp) = 0 // check is not part of the specification
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths and (Length(result) > 8)
    then raise Exception.Create('String too short');
 finally

  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetParamDisplay(index: Integer): string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 result := '';

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetParamDisplay, index, 0, Temp) = 0 // check is not part of the specification
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths and (Length(result) > 8)
    then raise Exception.Create('String too short');
 finally

  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetParamName(index: Integer): string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 result := '';

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetParamName, index, 0, Temp) = 0 // check is not part of the official specification
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths and (Length(result) > 8)
    then raise Exception.Create('String too short');
 finally

  // dispose memory
  Dispose(Temp);
 end;
end;

procedure TCustomVstPlugIn.SetSampleRate(const Value: Single);
begin
 VstDispatch(effSetSampleRate, 0, 0, nil, Value);
end;

procedure TCustomVstPlugIn.SetBlockSize(const Value: Integer);
begin
 VstDispatch(effSetBlockSize, 0, Value);
end;

procedure TCustomVstPlugIn.MainsChanged(const IsOn: Boolean);
begin
 VstDispatch(effMainsChanged, 0, Integer(IsOn));
end;

procedure TCustomVstPlugIn.NeedIdle;
begin
 FNeedIdle := True;
 if Assigned(FOnAMNeedIdle)
  then FOnAMNeedIdle(Self);
end;

function TCustomVstPlugIn.GetVu: Single;
const
  Divisor : Double = 1 / 32767;
begin
 if FActive
  then result := VstDispatch(effGetVu) * Divisor
  else result := -1;
end;

function TCustomVstPlugIn.GetRealQualities: LongInt;
begin
 if assigned(FVstEffect)
  then result := FVstEffect^.RealQualities
  else result := 0;
end;

function TCustomVstPlugIn.GetRect: TRect;
var
  theRect: ERect;
begin
 {$IFDEF VstHostGUI}
 theRect := EditGetRect;
 {$ELSE}
 theRect := Rect(0, 0, 0, 0);
 {$ENDIF}
 result := Classes.Rect(theRect.left, theRect.Top, theRect.right, theRect.Bottom);
end;

{$IFDEF VstHostGUI}
function TCustomVstPlugIn.EditGetRect: ERect;
var
  temp: PPERect;
begin
 GetMem(temp, SizeOf(PPERect));
 try
  if fActive then VstDispatch(effEditGetRect, 0, 0, temp);
  if Assigned(temp) then
   if Assigned(temp^)
    then result := temp^^;
 finally
  Dispose(temp);
 end;
end;

function TCustomVstPlugIn.EditOpen(Handle: THandle): Integer;
var
  i : Integer;
begin
 i := 0;
 try
//  raise Exception.Create(IntToStr(Integer(effEditOpen)));
  i := VstDispatch(effEditOpen, 0, 0, Pointer(Handle));
 finally
  if i > 0 then FEditOpen := True
  else FEditOpen := False;
  result := i;
 end;
end;

procedure TCustomVstPlugIn.EditActivateHandler(Sender: TObject);
begin
 EditActivate;
end;

procedure TCustomVstPlugIn.EditDeactivateHandler(Sender: TObject);
begin
 EditDeActivate;
end;

procedure TCustomVstPlugIn.ShowEdit;
begin
 if not assigned(GUIControl) then
  begin
   FGUIControl := TForm.Create(nil);
   with TForm(FGUIControl) do
    begin
     Caption := VendorString + ' - ' + ProductString;
     BorderStyle := bsToolWindow;
     Position := poDesktopCenter;
     OnClose := FormCloseHandler;
     OnActivate := EditActivateHandler;
     OnDeActivate := EditDeactivateHandler;
     if Caption = ' - ' then Caption := GetEffectName;
    end;
   FGUIControlCreated := True;
   ShowEdit(GUIControl);
  end;
end;

procedure TCustomVstPlugIn.ShowEdit(Control: TWinControl);
begin
 if Control = nil
  then raise Exception.Create('Control must exist!');
 if (effFlagsHasEditor in FVstEffect.EffectFlags) and (fGUIStyle = gsDefault) then
  begin
   if not FEditOpen then
    begin
     EditOpen(Control.Handle);
     FGUIControl := Control;
     EditIdle;
    end;
//  else raise Exception.Create('Editor is already open!');
  end
 else // Vst has no GUI
  case fGUIStyle of
   gsOld: ShowDefaultEditOld(Control);
   gsDefault, gsList: ShowDefaultEditList(Control);
  end;
 if assigned(FOnShowEdit) then FOnShowEdit(Self, GUIControl);
end;

procedure TCustomVstPlugIn.ShowDefaultEditOld(Control: TWinControl);
var
  param : string;
  i     : Integer;
begin
 FGUIControl := Control;
 FEditOpen := True;
 if not assigned(FGUIElements)
  then FGUIElements := TObjectList.Create;

 with TLabel(FGUIElements[FGUIElements.Add(TLabel.Create(Control))]) do
  begin
   Name := 'LbL'; Parent := Control; Caption := '';
   Alignment := taCenter; Left := 10; Top := 64;
  end;
 {$IFDEF FlatSrcollBar}
 with TFlatScrollBar(FGUIElements[FGUIElements.Add(TFlatScrollBar.Create(Control))]) do
  begin
   Name := 'ParamBar'; ClientWidth := 560;
   Anchors := [akLeft, akTop, akRight];
   BevelInner := bvNone; BevelOuter := bvNone;
   Color := clGray; Parent := Control; Align := alClient;
   Left := 0; Top := 0; Width := 560;
   VertScrollBar.Smooth := True; VertScrollBar.Tracking := True;
   HorzScrollBar.Smooth := True; HorzScrollBar.Tracking := True;
  end;
 {$ELSE}
 with TTrackBar(FGUIElements[FGUIElements.Add(TTrackBar.Create(Control))]) do
  begin
   Name := 'ParamBar'; Parent := Control;
   Anchors := [akLeft, akTop, akRight];
   Left := 5; Top := 33; Orientation := trHorizontal;
   Width := Control.Width - 4 * Left; Height := 32;
   Frequency := 1; Position := 0; {$IFNDEF FPC} SelEnd := 0; SelStart := 0; {$ENDIF}
   TabOrder := 3; Min := 0; Max := 100; OnChange := TrackChange;
   TickMarks := tmBottomRight; TickStyle := tsNone;//Auto;
  end;
 {$ENDIF}
 with TComboBox(FGUIElements[FGUIElements.Add(TComboBox.Create(Control))]) do
  begin
   Name := 'ParamBox'; Parent := Control; param := '';
   for i := 0 to numParams - 1
    do begin param := GetParamName(i); Items.Add(param); end;
   Anchors := [akLeft, akTop, akRight]; Left := 4; Top := 5;
   Width := Control.Width - 4 * Left; Height := 21; ItemHeight := 13;
   TabOrder := 2; OnChange := ParamChange; Text := ''; itemindex := 0;
   Font.Color := clWindowText;
   OnChange(nil);
  end;
end;

procedure TCustomVstPlugIn.ShowDefaultEditList(Control: TWinControl);
var
  i, j          : Integer;
  MaxParamWidth : Integer;
  theRect       : ERect;
begin
 theRect := Rect(0, 0, Control.Width, 4 + numParams * 16);
 FGUIControl := Control;
 if not assigned(FGUIElements)
  then FGUIElements := TObjectList.Create;
 GUIControl.Visible := False;
 GUIControl.ClientWidth := theRect.right - theRect.left;
 GUIControl.ClientHeight := theRect.Bottom - theRect.Top;

 // scan maximum parameter name length
 MaxParamWidth := 0;
 with TLabel.Create(Control) do
  try
   Parent := Control;
   Alignment := taCenter;
   for i := 0 to numParams - 1 do
    if Canvas.TextWidth(GetParamName(i) + ':_') > MaxParamWidth
     then MaxParamWidth := Canvas.TextWidth(GetParamName(i) + ':_');
  finally
   Free;
  end;

 for i := 0 to numParams - 1 do
  begin
   with TLabel(FGUIElements[FGUIElements.Add(TLabel.Create(Control))]) do
    begin
     Parent := Control; Caption := GetParamName(i) + ':'; Tag := i;
     Height := 16; Alignment := taCenter; Left := 2; Top := 2 + i * Height;
    end;
   with TLabel(FGUIElements[FGUIElements.Add(TLabel.Create(Control))]) do
    begin
     Name := 'LbV' + IntToStr(I); Tag := i;
     Parent  := Control; Alignment := taCenter; AutoSize := False;
     Height  := 16; Left := Control.Width - Left - 72;
     Alignment := taCenter; Width := 65; Top := 2 + i * Height;
    end;
   j := FGUIElements.Add(TScrollBar.Create(Control));
   with TScrollBar(FGUIElements[j]) do
    begin
     Parent := Control; Anchors := [akLeft, akTop, akRight];
     Kind := sbHorizontal; LargeChange := 10;
     Height := 16; Top := 2 + i * Height; Tag := i;
     Left := MaxParamWidth + 2; Width := Control.Width - Left - 72;
     Min := 0; Max := 1000; TabOrder := 3 + i;
     Position := Round(1000 * Parameter[i]);
     OnChange := ListParamChange;
     ListParamChange(FGUIElements[j]);
    end;
  end;
 GUIControl.Visible := True;
 GUIControl.Invalidate;
 FEditOpen := True;
end;

procedure TCustomVstPlugIn.ListParamChange(Sender: TObject);
var
  lb  : TLabel;
  str : string;
  i   : Integer;
begin
 assert(Sender is TScrollBar);
 with TScrollBar(Sender) do
  try
   Parameter[Tag] := Position * 0.001;
   lb := TLabel(GUIControl.FindComponent('LbV' + IntToStr(Tag)));
   if Assigned(lb) then
    begin
     if GetParamLabel(Tag) <> ''
      then str := GetParamDisplay(Tag) + ' ' + GetParamLabel(Tag)
      else str := GetParamDisplay(Tag);
     if Length(str) < 9
      then lb.Caption := str
      else
       begin
        str := GetParamDisplay(Tag);
        if Pos('.', str) > 0 then
         begin
          i := Length(str) - 1;
          while str[i] = '0' do
           begin
            Delete(str, i, 1);
            dec(i);
           end;
         end;
        if GetParamLabel(Tag) <> ''
         then lb.Caption := str + ' ' + GetParamLabel(Tag)
         else lb.Caption := str;
        if Length(lb.Caption) > 9 then lb.Caption := str
       end;
    end;
  except
  end;
end;

procedure TCustomVstPlugIn.ParamChange(Sender: TObject);
var
  nr : Integer;
  wc : TComponent;
begin
 wc := GUIControl.FindComponent('ParamBar');

 if wc <> nil then
  with wc as {$IFDEF FlatSrcollBar}TFlatScrollBar{$ELSE}TTrackBar{$ENDIF} do
   try
    nr := (GUIControl.FindComponent('ParamBox') as TComboBox).ItemIndex;
    if (nr >= 0) and (nr < numParams)
     then Position := round(Parameter[nr] * 100);
   except
   end;
end;

{$IFDEF FlatSrcollBar}
procedure TCustomVstPlugIn.ScrollChange(Sender: TObject; ScrollPos: Integer);
var
  nr: Integer;
begin
 with (GUIControl.FindComponent('ParamBar') as TFlatScrollBar) do
  begin
   nr := (GUIControl.FindComponent('ParamBox') as TComboBox).ItemIndex;
   Parameter[nr] := Position * 0.01;
   (GUIControl.FindComponent('LbL') as TLabel).Caption  :=
     RStrValue + ': ' + GetParamDisplay(nr) + GetParamLabel(nr);
  end;
end;
{$ELSE}
procedure TCustomVstPlugIn.TrackChange(Sender: TObject);
var
  nr: Integer;
begin
 with (GUIControl.FindComponent('ParamBar') as TTrackBar) do
  begin
   nr := (GUIControl.FindComponent('ParamBox') as TComboBox).ItemIndex;
   Parameter[nr] := Position * 0.01;
   (GUIControl.FindComponent('LbL') as TLabel).Caption  :=
     RStrValue + ': ' + GetParamDisplay(nr) + GetParamLabel(nr);
  end;
end;
{$ENDIF}

procedure TCustomVstPlugIn.EditClose;
begin
 if FEditOpen then
  if not Boolean(VstDispatch(effEditClose))
   then; // ToDo
 FEditOpen := False;
end;

procedure TCustomVstPlugIn.CloseEdit;
begin
 if not Assigned(FVstEffect) then Exit;
 try
  if assigned(FOnCloseEdit) then FOnCloseEdit(Self);
  if (effFlagsHasEditor in FVstEffect.EffectFlags) and (FGUIStyle = gsDefault)
   then EditClose else
   if Assigned(FGUIElements)
    then FreeAndNil(FGUIElements);
  if assigned(GUIControl) and FGUIControlCreated
   then FreeAndNil(FGUIControl); //and (not FExternalForm) then
 finally
  FEditOpen := False;
 end;
end;

procedure TCustomVstPlugIn.FormCloseHandler(Sender: TObject; var Action: TCloseAction);
begin
 CloseEdit;
 if assigned(GUIControl)
  then FreeAndNil(FGUIControl);
end;

function TCustomVstPlugIn.EditIdle: Integer;
begin
 if FEditOpen
  then result := VstDispatch(effEditIdle)
  else result := 0;
end;

procedure TCustomVstPlugIn.EditActivate;
begin
 if FEditOpen then VstDispatch(effEditTop);
end;

procedure TCustomVstPlugIn.EditDeactivate;
begin
 if FEditOpen then VstDispatch(effEditSleep);
end;
{$ENDIF}

function TCustomVstPlugIn.Identify: Integer;
begin
 result := VstDispatch(effIdentify);
end;

function TCustomVstPlugIn.GetChunk(const Data: Pointer; const IsPreset: Boolean = False): Integer;
begin
 result := VstDispatch(effGetChunk, Integer(isPreset), 0, Data);
end;

function TCustomVstPlugIn.SetChunk(const Data: Pointer; const ByteSize: Integer; const IsPreset: Boolean = False): Integer;
begin
 result := VstDispatch(effSetChunk, Integer(isPreset), ByteSize, Data);
end;

function TCustomVstPlugIn.ProcessEvents(var VstEvents: TVstEvents): Integer;
begin
 result := VstDispatch(effProcessEvents, 0, 0, @VstEvents);
end;

function TCustomVstPlugIn.CanBeAutomated(const Index: Integer): Integer;
begin
 result := VstDispatch(effCanBeAutomated, Index);
end;

function TCustomVstPlugIn.String2Parameter(const Index: Integer; const ParameterName: string): Integer;
begin
 result := VstDispatch(effString2Parameter, Index, 0, PAnsiChar(ParameterName));
end;

function TCustomVstPlugIn.GetNumProgramCategories: Integer;
begin
 if FActive
  then result := VstDispatch(effGetNumProgramCategories)
  else result := -1;
end;

function TCustomVstPlugIn.GetnumPrograms: Integer;
begin
 if Assigned(FVstEffect)
  then result := FVstEffect^.numPrograms
  else result := 0;
end;

function TCustomVstPlugIn.GetProgramNameIndexed(const Category, Index: Integer; var ProgramName: string): Integer;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);

  if FActive
   then
    begin
     result := VstDispatch(effGetProgramNameIndexed, Index, Category, Temp);
     if result > 0
      then ProgramName := StrPas(Temp);
    end
   else result := -1;

 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.CopyCurrentProgramTo(const Destination: Integer): Boolean;
begin
 if FActive
  then result := Boolean(VstDispatch(effCopyProgram, Destination))
  else result := False;
end;

function TCustomVstPlugIn.ConnectInput(const InputNr: Integer; const State: Boolean): Integer;
begin
 if FActive
  then result := VstDispatch(effConnectInput, InputNr, Integer(State))
  else result := -1;
end;

function TCustomVstPlugIn.ConnectOutput(const OutputNr: Integer; const State: Boolean): Integer;
begin
 if FActive
  then result := VstDispatch(effConnectOutput, OutputNr, Integer(State))
  else result := -1;
end;

function TCustomVstPlugIn.GetInputProperties(const InputNr: Integer): TVstPinProperties;
begin
 FillChar(result, SizeOf(TVstPinProperties), 0);
 if FActive
  then VstDispatch(effGetInputProperties, InputNr, 0, @result);
end;

function TCustomVstPlugIn.GetIORatio: Single;
begin
 if assigned(FVstEffect)
  then result := FVstEffect^.IORatio
  else result := 1;
end;

function TCustomVstPlugIn.GetOffQualities: LongInt;
begin
 if assigned(FVstEffect)
  then result := FVstEffect^.OffQualities
  else result := 0;
end;

function TCustomVstPlugIn.GetOutputProperties(OutputNr: Integer): TVstPinProperties;
begin
 FillChar(result, SizeOf(TVstPinProperties), 0);
 if FActive
  then VstDispatch(effGetOutputProperties, OutputNr, 0, @result);
end;

function TCustomVstPlugIn.GetPlugCategory:TVstPluginCategory;
begin
 if FActive
  then result := TVstPluginCategory(VstDispatch(effGetPlugCategory))
  else result := vpcUnknown;
end;

function TCustomVstPlugIn.GetCurrentPosition:Integer;
begin
 if FActive
  then result := VstDispatch(effGetCurrentPosition)
  else result := -1;
end;

function TCustomVstPlugIn.GetDestinationBuffer: Integer;
begin
 if FActive
  then result := VstDispatch(effGetDestinationBuffer)
  else result := -1;
end;

function TCustomVstPlugIn.OfflineNotify(var VstAudioFile: TVstAudioFile; const NumAudioFiles: Integer; const Start: Boolean): Integer;
begin
 result := VstDispatch(effOfflineNotify, Integer(Start), NumAudioFiles, @VstAudioFile);
end;

function TCustomVstPlugIn.OfflinePrepare(var VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count: Integer): Integer;
begin
 result := VstDispatch(effOfflinePrepare, 0, count, @VstOfflineTaskRecord);
end;

function TCustomVstPlugIn.OfflineRun(var VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count: Integer): Integer;
begin
 result := VstDispatch(effOfflineRun, 0, count, @VstOfflineTaskRecord);
end;

function TCustomVstPlugIn.ProcessVarIo(var VarIo: TVstVariableIo): Integer;
begin
 result := VstDispatch(effProcessVarIo, 0, 0, @VarIo);
end;

function TCustomVstPlugIn.SetSpeakerArrangement(pluginInput: PVstSpeakerArrangement; pluginOutput: PVstSpeakerArrangement): Boolean;
begin
 result := Boolean(VstDispatch(effSetSpeakerArrangement, 0, Integer(pluginInput), pluginOutput));
end;

function TCustomVstPlugIn.SetBlockSizeAndSampleRate(const BlockSize: Integer; const SampleRate: Single): Integer;
begin
 result := VstDispatch(effSetBlockSizeAndSampleRate, 0, BlockSize, nil, SampleRate);
end;

function TCustomVstPlugIn.SetBypass(const Value: Boolean): Integer;
begin
 result := VstDispatch(effSetBypass, 0, Integer(Value));
end;

function TCustomVstPlugIn.GetEffectName: string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetEffectName, 0, 0, Temp) <> 0 // not sure about this
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(result) <= 32);
 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetErrorText: string;
var
  Temp : PAnsiChar;
const
  Lngth = 512;
begin
 result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetErrorText, 0, 0, Temp) <> 0 // not sure here!
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(result) <= 256);
 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetFriendlyNameString(const StringLength: Integer): string;
var
  Variations : array [0..6] of string;
  i, j, v    : Integer;
begin
 Variations[0] := GetEffectName;
 Variations[1] := GetVendorString  + ' - ' + GetProductString;
 Variations[2] := GetVendorString  + ' - ' + GetProductString + ' - ' + GetEffectName;
 Variations[3] := GetProductString + ' - ' + GetEffectName;
 Variations[4] := GetVendorString  + ' - ' + GetEffectName;
 Variations[5] := GetProductString + ' - ' + GetEffectName;
 Variations[6] := GetProductString;
 v := 0;
 j := Length(Variations[0]);
 for i := 1 to 6 do
  if (Length(Variations[i]) > j) and
     (Length(Variations[i]) < StringLength) then
   begin
    v := i;
    j := Length(Variations[i]);
   end;
 result := Variations[v];
 if result = ''
  then result := DisplayName;
end;

function TCustomVstPlugIn.GetVendorString: string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetVendorString, 0, 0, Temp) <> 0 // not sure here!
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(result) <= 64);
 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetProductString: string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetProductString, 0, 0, Temp) <> 0 // not sure here!
    then result := StrPas(Temp);

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(result) <= 64);
 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetVendorVersion: Integer;
begin
 if FActive
  then result := VstDispatch(effGetVendorVersion)
  else result := -1;
end;

function TCustomVstPlugIn.GetVersion: Integer;
begin
 if assigned(FVstEffect)
  then result := FVstEffect.Version
  else result := 0;
end;

function TCustomVstPlugIn.GetVSTCanDos: TVstCanDos;
begin
 if FVSTCanDosScannedComplete
  then result := FVstCanDos
  else
   begin
    if VstCanDo('receiveVstEvents')      = 0 then FVstCanDos := FVstCanDos - [vcdReceiveVstEvents] else FVstCanDos := FVstCanDos + [vcdReceiveVstEvents];
    if VstCanDo('receiveVstMidiEvent')   = 0 then FVstCanDos := FVstCanDos - [vcdReceiveVstMidiEvent] else FVstCanDos := FVstCanDos + [vcdReceiveVstMidiEvent];
    if VstCanDo('receiveVstTimeInfo')    = 0 then FVstCanDos := FVstCanDos - [vcdReceiveVstTimeInfo] else FVstCanDos := FVstCanDos + [vcdReceiveVstTimeInfo];
    if VstCanDo('sendVstEvents')         = 0 then FVstCanDos := FVstCanDos - [vcdSendVstEvents] else FVstCanDos := FVstCanDos + [vcdSendVstEvents];
    if VstCanDo('sendVstMidiEvent')      = 0 then FVstCanDos := FVstCanDos - [vcdSendVstMidiEvent] else FVstCanDos := FVstCanDos + [vcdSendVstMidiEvent];
    if VstCanDo('sendVstTimeInfo')       = 0 then FVstCanDos := FVstCanDos - [vcdSendVstTimeInfo] else FVstCanDos := FVstCanDos + [vcdSendVstTimeInfo];
    if VstCanDo('offline')               = 0 then FVstCanDos := FVstCanDos - [vcdOffline] else FVstCanDos := FVstCanDos + [vcdOffline];
    if VstCanDo('plugAsChannelInsert')   = 0 then FVstCanDos := FVstCanDos - [vcdPlugAsChannelInsert] else FVstCanDos := FVstCanDos + [vcdPlugAsChannelInsert];
    if VstCanDo('plugAsSend')            = 0 then FVstCanDos := FVstCanDos - [vcdPlugAsSend] else FVstCanDos := FVstCanDos + [vcdPlugAsSend];
    if VstCanDo('mixDryWet')             = 0 then FVstCanDos := FVstCanDos - [vcdMixDryWet] else FVstCanDos := FVstCanDos + [vcdMixDryWet];
    if VstCanDo('noRealTime')            = 0 then FVstCanDos := FVstCanDos - [vcdNoRealTime] else FVstCanDos := FVstCanDos + [vcdNoRealTime];
    if VstCanDo('multipass')             = 0 then FVstCanDos := FVstCanDos - [vcdMultipass] else FVstCanDos := FVstCanDos + [vcdMultipass];
    if VstCanDo('metapass')              = 0 then FVstCanDos := FVstCanDos - [vcdMetapass] else FVstCanDos := FVstCanDos + [vcdMetapass];
    if VstCanDo('1in1out')               = 0 then FVstCanDos := FVstCanDos - [vcd1in1out] else FVstCanDos := FVstCanDos + [vcd1in1out];
    if VstCanDo('1in2out')               = 0 then FVstCanDos := FVstCanDos - [vcd1in2out] else FVstCanDos := FVstCanDos + [vcd1in2out];
    if VstCanDo('2in1out')               = 0 then FVstCanDos := FVstCanDos - [vcd2in1out] else FVstCanDos := FVstCanDos + [vcd2in1out];
    if VstCanDo('2in2out')               = 0 then FVstCanDos := FVstCanDos - [vcd2in2out] else FVstCanDos := FVstCanDos + [vcd2in2out];
    if VstCanDo('2in4out')               = 0 then FVstCanDos := FVstCanDos - [vcd2in4out] else FVstCanDos := FVstCanDos + [vcd2in4out];
    if VstCanDo('4in2out')               = 0 then FVstCanDos := FVstCanDos - [vcd4in2out] else FVstCanDos := FVstCanDos + [vcd4in2out];
    if VstCanDo('4in4out')               = 0 then FVstCanDos := FVstCanDos - [vcd4in4out] else FVstCanDos := FVstCanDos + [vcd4in4out];
    if VstCanDo('4in8out')               = 0 then FVstCanDos := FVstCanDos - [vcd4in8out] else FVstCanDos := FVstCanDos + [vcd4in8out];
    if VstCanDo('8in4out')               = 0 then FVstCanDos := FVstCanDos - [vcd8in4out] else FVstCanDos := FVstCanDos + [vcd8in4out];
    if VstCanDo('8in8out')               = 0 then FVstCanDos := FVstCanDos - [vcd8in8out] else FVstCanDos := FVstCanDos + [vcd8in8out];
    if VstCanDo('midiProgramNames')      = 0 then FVstCanDos := FVstCanDos - [vcdMidiProgramNames] else FVstCanDos := FVstCanDos + [vcdMidiProgramNames];
    if VstCanDo('conformsToWindowRules') = 0 then FVstCanDos := FVstCanDos - [vcdConformsToWindowRules] else FVstCanDos := FVstCanDos + [vcdConformsToWindowRules];
    if VstCanDo('LiveWithoutToolbar')    = 0 then FVstCanDos := FVstCanDos - [vcdLiveWithoutToolbar] else FVstCanDos := FVstCanDos + [vcdLiveWithoutToolbar];
    FVSTCanDosScannedComplete := True;
   end;
end;

function TCustomVstPlugIn.VendorSpecific(const Index, Value: Integer; const Pntr: Pointer; const Opt: Single): Integer;
begin
 result := VstDispatch(effVendorSpecific, index, value, pntr, opt);
end;

function TCustomVstPlugIn.VstCanDo(const CanDoString: string): Integer;
begin
 result := VstDispatch(effCanDo, 0, 0, PAnsiChar(CanDoString));
end;

function TCustomVstPlugIn.GetTailSize: Integer;
begin
 if FActive then result := VstDispatch(effGetTailSize) else result := -1;
end;

function TCustomVstPlugIn.GetUniqueID: string;
begin
 if assigned(FVstEffect) then
  with FVstEffect^
   do result := uniqueID[3] + uniqueID[2] + uniqueID[1] + uniqueID[0]
  else result := '';
end;

function TCustomVstPlugIn.Idle: Integer;
begin
 if FNeedIdle
  then result := VstDispatch(effIdle)
  else result := 0;
end;

procedure TCustomVstPlugIn.IOchanged;
begin
 if Assigned(FOnAMIOChanged)
  then FOnAMIOChanged(Self);
end;

function TCustomVstPlugIn.GetIcon: Integer;
begin
 if FActive
  then result := VstDispatch(effGetIcon)
  else result := -1;
end;

procedure TCustomVstPlugIn.SetViewPosition(const x, y: Integer);
begin
 VstDispatch(effSetViewPosition, x, y);
end;

function TCustomVstPlugIn.GetParameterProperties(const Parameter: Integer): TVstParameterPropertyRecord;
begin
 if FActive
  then VstDispatch(effGetParameterProperties, Parameter, 0, @result);
end;

function TCustomVstPlugIn.KeysRequired: Integer;
begin
 if FActive
  then result := VstDispatch(effKeysRequired)
  else result := -1;
end;

function TCustomVstPlugIn.GetVstVersion: Integer;
begin
 if FActive
  then result := VstDispatch(effGetVstVersion)
  else result := -1;
end;

{$IFDEF VstHostGUI}
function TCustomVstPlugIn.EditKeyDown(const Key: Char; const VirtualKeycode: Integer; const Modifier: TVstModifierKeys): Boolean;
var
  IntMod : Integer;
begin
 // character in <index>, virtual in <value>, modifiers in <opt>, return True if used, else False
 Result := False;
 if FActive then
  begin
   IntMod := PByte(@Modifier)^;
   Result := (VstDispatch(effEditKeyDown, Integer(Key), VirtualKeycode, nil, PSingle(@IntMod)^) = 1);
  end;
end;

function TCustomVstPlugIn.EditKeyUp(const Key: Char; const VirtualKeycode: Integer; const Modifier: TVstModifierKeys): Boolean;
var
  IntMod : Integer;
begin
 // character in <index>, virtual in <value>, modifiers in <opt>, return True if used, else False
 Result := False;
 if FActive then
  begin
   IntMod := PByte(@Modifier)^;
   Result := (VstDispatch(effEditKeyUp, Integer(Key), VirtualKeycode, nil, PSingle(@IntMod)^) = 1);
  end;
end;

procedure TCustomVstPlugIn.SetEditKnobMode(Mode : TKnobMode);
begin
 if FActive then VstDispatch(effSetEditKnobMode, 0, Integer(Mode));
end;
{$ENDIF}

// midi plugins channel dependent programs
function TCustomVstPlugIn.GetMidiProgramName(var MidiProgramName: TMidiProgramName): Integer;
begin
 // struct will be filled with information for 'thisProgramIndex'.
 // returns number of used programIndexes.
 // if 0 is returned, no MidiProgramNames supported.

 if FActive
  then Result := VstDispatch(effGetMidiProgramName, 0, 0, @MidiProgramName, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.GetNumInputs: Integer;
begin
 if Assigned(FVstEffect)
  then result := FVstEffect^.numInputs
  else result := 0;
end;

function TCustomVstPlugIn.GetNumOutputs: Integer;
begin
 if Assigned(FVstEffect)
  then result := FVstEffect^.numOutputs
  else result := 0;
end;

function TCustomVstPlugIn.GetNumParams: Integer;
begin
 if Assigned(FVstEffect)
  then result := FVstEffect^.numParams
  else result := 0;
end;

function TCustomVstPlugIn.GetCurrentMidiProgram(var MidiProgramName: TMidiProgramName): Integer;
begin
 // returns the programIndex of the current program.
 // passed <ptr> points to MidiProgramName struct.
 // struct will be filled with information for the current program.
 if FActive
  then Result := VstDispatch(effGetCurrentMidiProgram, 0, 0, @MidiProgramName, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.GetMidiProgramCategory(var MidiProgramCategory: TMidiProgramCategory): Integer;
begin
 // passed <ptr> points to MidiProgramCategory struct.
 // struct will be filled with information for 'thisCategoryIndex'.
 // returns number of used CategoryIndexes.
 // if 0 is returned, no MidiProgramCategories supported.
 if FActive
  then Result := VstDispatch(effGetMidiProgramCategory, 0, 0, @MidiProgramCategory, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.HasMidiProgramsChanged: Integer;
begin
 // returns 1 if the MidiProgramNames or MidiKeyNames
 // had changed on this channel, 0 otherwise. <ptr> ignored.

 if FActive
  then Result := VstDispatch(effHasMidiProgramsChanged, 0, 0, nil, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.GetMidiKeyName(var MidiKeyName: TMidiKeyName): Integer;
begin
 // struct will be filled with information for 'thisProgramIndex' and
 // 'thisKeyNumber'. If keyName is "" the standard name of the key
 // will be displayed. If 0 is returned, no MidiKeyNames are
 // defined for 'thisProgramIndex'.
 if FActive
  then Result := VstDispatch(effGetMidiKeyName, 0, 0, @MidiKeyName, 0)
  else Result := 0;
end;

procedure TCustomVstPlugIn.BeginSetProgram;
begin
 // called before a new program is loaded
 if FActive then VstDispatch(effBeginSetProgram);
end;

procedure TCustomVstPlugIn.EndSetProgram;
begin
 // called when the program is loaded
 if FActive
  then VstDispatch(effEndSetProgram);
end;

function TCustomVstPlugIn.GetSpeakerArrangement(const SpeakerIn, SpeakerOut: PVstSpeakerArrangement): Integer;
begin
// VstSpeakerArrangement** pluginInput in <value>
// VstSpeakerArrangement** pluginOutput in <ptr>
 Result := 0;
 if FActive
  then VstDispatch(effGetSpeakerArrangement, 0, Integer(@SpeakerOut), @SpeakerOut);
end;

function TCustomVstPlugIn.ShellGetNextPlugin(var PluginName: string): Integer;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 PluginName := '';
 result := 0;

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   begin
    result := VstDispatch(effShellGetNextPlugin, 0, 0, Temp);
    if result <> 0 then PluginName := StrPas(temp);
   end;

  // check whether the result string accords to the specs
  if assigned(Collection) and assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(PluginName) <= 64);
 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

procedure TCustomVstPlugIn.StartProcess;
begin
 // Called before the start of process call
 if FActive then VstDispatch(effStartProcess);
end;

procedure TCustomVstPlugIn.StopProcess;
begin
 // Called after the stop of process call
 if FActive then VstDispatch(effStopProcess);
end;

procedure TCustomVstPlugIn.SetTotalSampleToProcess;
begin
 // Called in offline (non RealTime) Process before process
 // is called, indicates how many sample will be processed
 if FActive then VstDispatch(effSetTotalSampleToProcess);
end;

procedure TCustomVstPlugIn.SetPanLaw(const PanLaw: TVstPanLawType; const Gain: Single);
var
  i : Integer;
begin
 // PanLaw : Type (Linear, Equal Power,.. see enum PanLaw Type) in <value>,
 // Gain in <opt>: for Linear : [1.0 => 0dB PanLaw], [~0.58 => -4.5dB], [0.5 => -6.02dB]
 i := Integer(PanLaw);
 if FActive then VstDispatch(effSetPanLaw, 0, i, nil, Gain);
end;

function TCustomVstPlugIn.BeginLoadBank(const PatchChunkInfo : PVstPatchChunkInfo): Integer;
begin
 // Called before a Bank is loaded, <ptr> points to VstPatchChunkInfo structure
 // return -1 if the Bank can not be loaded, return 1 if it can be loaded else 0 (for compatibility)
 if FActive
  then Result := VstDispatch(effBeginLoadBank, 0, 0, PatchChunkInfo)
  else Result := 0;
end;

function TCustomVstPlugIn.BeginLoadProgram(const PatchChunkInfo : PVstPatchChunkInfo): Integer;
begin
 // Called before a Program is loaded, <ptr> points to VstPatchChunkInfo structure
 // return -1 if the Program can not be loaded, return 1 if it can be loaded else 0 (for compatibility)

 if FActive
  then Result := VstDispatch(effBeginLoadProgram, 0, 0, PatchChunkInfo)
  else Result := 0;
end;

procedure TCustomVstPlugIn.InitializeVstEffect;
begin
 // run VST plugin main function to create the VstEffect pointer
 if assigned(FMainFunction)
  then FVstEffect := FMainFunction(@audioMaster);

 // check if a VstEffect pointer has been created created!
 if FVstEffect = nil
  then raise Exception.Create(RStrLoadingFailed);

 // check if the VstEffect pointer is valid
 if FVstEffect.Magic <> 'PtsV'
  then raise Exception.Create(RStrVSTPluginNotValid);

 // set host related variables
 FVstEffect.ReservedForHost := Self;
 {$IFDEF DELPHI6_UP}
 if assigned(Collection)
  then FVstEffect.resvd2    := Collection.Owner
  else FVstEffect.resvd2    := nil;
 {$ENDIF}

 DontRaiseExceptionsAndSetFPUcodeword;
 if Assigned(FOnAfterLoad) then FOnAfterLoad(Self);
end;

procedure TCustomVstPlugIn.SetVstDllFileName(const Value: TFilename);
begin
 if FVstDllFileName <> Value then
  if FileExists(Value)
   then LoadFromFile(Value) else
  {$IFDEF MemDLL}
  if not assigned(FInternalDLLLoader)
   then Unload;
  {$ENDIF}
end;

procedure TCustomVstPlugIn.LoadFromVSTEffect(const Value: PVSTEffect);
begin
 if FLoaded
  then Unload;
 try
   if Value^.Magic <> 'PtsV'
    then raise Exception.Create(RStrVSTPluginNotValid);
  FVstEffect := Value;  
  DontRaiseExceptionsAndSetFPUcodeword;
  FLoaded := True;
 except
  Unload;
  raise;
 end;
end;

procedure TCustomVstPlugIn.LoadFromFile(const FileName: TFilename);
{$IFNDEF FPC}
var
  Buf : array[0..255] of Char;
  LE  : Integer;
  str : string;
{$ENDIF}
begin
 if not FileExists(FileName)
  then raise Exception.CreateFmt(RStrFileDoesNotExist, [FileName]);

 if FLoaded
  then Unload;

 FVstDllFileName := FileName;

 try
  DontRaiseExceptionsAndSetFPUcodeword;
  FVstDllHandle := SafeLoadLibrary(PAnsiChar(DLLFileName), 7);
 // FVstDllHandle := LoadLibraryEx(PAnsiChar(DLLFileName), 0, DONT_RESOLVE_DLL_REFERENCES);

  if FVstDllHandle = 0 then
   begin
    {$IFNDEF FPC}
    LE := GetLastError;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LE, 0, @Buf[0], SizeOf(Buf), nil);
    if Buf = '' then
     begin
      str := IntToStr(LE) + StrPas(Buf);
      raise Exception.Create(str);
     end else raise Exception.Create(StrPas(Buf));
    {$ENDIF}
   end
  else
   begin
    if Lowercase(ExtractFileExt(DLLFileName)) = '.vst3'
     then @FMainFunction := GetProcAddress(FVstDllHandle, 'GetPluginFactory')
     else @FMainFunction := GetProcAddress(FVstDllHandle, 'main');
    if not Assigned(FMainFunction) then @FMainFunction := GetProcAddress(FVstDllHandle, 'VSTPluginMain');
    if not Assigned(FMainFunction) then
     begin
      @FMainFunction := @WhatIfNoEntry;
      raise Exception.Create(RStrNoEntryPoint);
     end;
   end;

  InitializeVstEffect;
  FLoaded := True;
 except
  Unload;
  raise;
 end;
end;

{$IFDEF MemDLL}
procedure TCustomVstPlugIn.LoadFromStream(const Stream: TStream);
begin
 if FLoaded
  then Unload;

 if not assigned(FInternalDLLLoader)
  then FInternalDLLLoader := TDLLLoader.Create;
 try
  FInternalDLLLoader.Load(Stream);
  FMainFunction := FInternalDllLoader.FindExport('VSTPluginMain');
  if not Assigned(FMainFunction)
   then FMainFunction := FInternalDllLoader.FindExport('main');

  InitializeVstEffect;
  FLoaded := True;
 except
  Unload;
  raise;
 end;
end;
{$ENDIF}

procedure TCustomVstPlugIn.UnLoad;
begin
 Active := False;
 if FVstDllHandle > 0 then
  try
   FreeLibrary(FVstDllHandle);
  finally
   FVstDllHandle := 0;
   FVstDllFileName := '';
  end;
 {$IFDEF MemDLL}
 if assigned(FInternalDllLoader) then
  begin
   FInternalDllLoader.Unload;
   FreeAndNil(FInternalDllLoader);
  end;
 {$ENDIF}

 FMainFunction := nil;
 FVstEffect := nil;
 FLoaded := False;
end;

procedure TCustomVstPlugIn.LoadBank(FileName: TFileName);
var
  chnk: TFileStream;
begin
 if not FileExists(FileName)
  then raise Exception.Create(RStrBankFileDoesNotExist);
 chnk := TFileStream.Create(FileName, fmOpenRead);
 try
  LoadBank(chnk);
 finally
  chnk.Free;
 end; 
end;

procedure TCustomVstPlugIn.LoadPreset(FileName: TFileName);
var
  chnk: TFileStream;
begin
 if not FileExists(FileName)
  then raise Exception.Create(StrPresetFileDoesNotExist);
 chnk := TFileStream.Create(FileName, fmOpenRead);
 try
  LoadPreset(chnk);
 finally
  chnk.Free;
 end; 
end;

function TCustomVstPlugIn.GetPreset(ProgramNo: Integer): TFXPreset;
var
  str : string;
  i   : Integer;
begin
 SetProgram(ProgramNo);
 with result do
  begin
   ChunkMagic := 'KncC';
   FXMagic    := 'kCxF';
   Version    := 1;
   FXID       := FVstEffect^.UniqueID;
   FXVersion  := FVstEffect^.version;
   NumParams  := Self.numParams;
   str        := GetProgramName + #0;
   StrLCopy(prgName, PAnsiChar(str), 26);

   GetMem(Params, numParams * SizeOf(Single));
   for i := 0 to numParams - 1 do
    begin
     PDAVSingleFixedArray(params)^[i] := Parameter[i];
     SwapLong(PDAVSingleFixedArray(params)^[i]);
    end;
   // set bytesize excl. ChunkMagic, ByteSize & Params (not part of the spec)
   result.ByteSize := SizeOf(result) - SizeOf(LongInt) * 3 + numParams * SizeOf(Single);

   // swap
   SwapLong(ByteSize);
   SwapLong(ChunkMagic);
   SwapLong(FXMagic);
   SwapLong(Version);
   SwapLong(FXID);
   SwapLong(FXVersion);
   SwapLong(NumParams);
  end;
end;

procedure TCustomVstPlugIn.SaveBank(FileName: TFileName);
var
  chnk: TFileStream;
begin
 chnk := TFileStream.Create(FileName, fmCreate);
 try
  SaveBank(chnk);
 finally
  chnk.Free;
 end;
end;

procedure TCustomVstPlugIn.SavePreset(FileName: TFileName);
var
  chnk: TFileStream;
begin
 chnk := TFileStream.Create(FileName, fmCreate);
 try
  SavePreset(chnk);
 finally
  chnk.Free;
 end; 
end;

procedure TCustomVstPlugIn.SavePreset(Stream: TStream);
var
  FXChunkSet : TFXChunkSet;
  str        : string;
  IntChkSize : Integer;
  ChunkData  : Pointer;
  FXPreset   : TFXPreset;
begin
 Stream.Seek(0, 0);
 if not assigned(FVstEffect) then exit;
 if effFlagsProgramChunks in EffectOptions then
  with FXChunkSet do
   begin
    ChunkMagic  := 'KncC';
    FXMagic     := 'hCPF';
    Version     := 1;
    FXID        := FVstEffect^.UniqueID;
    FXVersion   := FVstEffect^.version;
    NumPrograms := FVstEffect^.numPrograms;

    str := GetProgramName + #0;
    StrLCopy(prgName, PAnsiChar(str), 26);

    IntChkSize := GetChunk(@ChunkData, True);
    chunkSize := IntChkSize;
    ByteSize := SizeOf(FXChunkSet) - SizeOf(LongInt) * 2 + chunkSize - 8;

    // swap-o-matic
    SwapLong(chunkMagic);
    SwapLong(fxMagic);
    SwapLong(version);
    SwapLong(fxID);
    SwapLong(fxVersion);
    SwapLong(numPrograms);
    SwapLong(ByteSize);
    SwapLong(chunkSize);

    // write data to stream 
    Stream.WriteBuffer(FXChunkSet, SizeOf(FXChunkSet) - SizeOf(Pointer));
    Stream.WriteBuffer(ChunkData^, IntChkSize);
   end
  else
   begin
    FXPreset := GetPreset(GetProgram);
    try
     Stream.WriteBuffer(FXPreset, SizeOf(FXPreset) - SizeOf(Single));
     Stream.WriteBuffer(FXPreset.Params^, SizeOf(Single) * numParams);
    finally
     Dispose(FXPreset.params);
    end;
   end;
end;

procedure TCustomVstPlugIn.LoadBank(Stream: TStream);
var
  i, j           : Integer;
  FXSet          : TFXSet;
  FXChunkBank    : TFXChunkBank;
  FXPreset       : TFXPreset;
  Param          : Single;
  ChunkData      : Pointer;
  ChunkDataSize  : Integer;
  PatchChunkInfo : TVstPatchChunkInfo;
  b              : Byte;
  UseChunk       : Boolean;
begin
 if not assigned(FVstEffect) then exit;
 Stream.Seek(9, 0);
 Stream.Read(b, 1);
 UseChunk := (b <> $78);
 Stream.Seek(0, 0);

 if UseChunk then
  begin
   assert(effFlagsProgramChunks in EffectOptions);
   Stream.Read(FXChunkBank, SizeOf(TFXChunkBank) - SizeOf(Pointer));
   SwapLong(FXChunkBank.chunkMagic);
   SwapLong(FXChunkBank.fxMagic);
   assert(FXChunkBank.chunkMagic = 'KncC');
   assert(FXChunkBank.fxMagic = 'kCxF');

   // swap unique ID
   SwapLong(FXChunkBank.fxId);
   if FXChunkBank.fxId <> FVstEffect^.UniqueID
    then raise Exception.Create(RStrBankFileNotForThisPlugin);

   // allocate chunk data memory
   ChunkDataSize := Stream.Size - Stream.Position;
   GetMem(ChunkData, ChunkDataSize);
   try
    if Stream.Read(ChunkData^, ChunkDataSize) <> ChunkDataSize
     then raise Exception.Create('chunk error, actual stream smaller than chunk');
    SetChunk(ChunkData, ChunkDataSize, False);
   finally
    Dispose(ChunkData);
   end;
  end
 else
  begin
   Stream.Read(FXSet, SizeOf(TFXSet) - SizeOf(Pointer));
   SwapLong(FXSet.chunkMagic);
   SwapLong(FXSet.fxMagic);
   assert(FXSet.chunkMagic = 'KncC');
   assert(FXSet.fxMagic = 'kCxF');

   // swap
   SwapLong(FXSet.fxId);
   if FXChunkBank.fxId <> FVstEffect^.UniqueID
    then raise Exception.Create(RStrBankFileNotForThisPlugin);

   with PatchChunkInfo do
    begin
     version        := 1;
     pluginUniqueID := FVstEffect.uniqueID;
     pluginVersion  := FVstEffect.version;
     numElements    := FVstEffect.numPrograms; // Number of Programs (Bank)
     BeginLoadBank(@PatchChunkInfo);
    end;

   SwapLong(FXSet.numPrograms);
   for i := 0 to FXSet.numPrograms - 1 do
    begin
     Stream.Read(FXPreset, SizeOf(TFXPreset) - SizeOf(Pointer));
     SetProgram(i);
     SetProgramName(FXPreset.prgName);
     SwapLong(FXPreset.numParams);
     for j := 0 to FXPreset.numParams - 1 do
      begin
       Stream.Read(Param, SizeOf(Single));
       SwapLong(Param);
       SetParameter(j, Param);
      end;
    end;
  end;
end;

procedure TCustomVstPlugIn.LoadPreset(Stream: TStream);
var
  FXPreset       : TFXPreset;
  FXChunkset     : TFXChunkset;
  Param          : Single;
  ParamNo        : Integer;
  ChunkData      : Pointer;
  ChunkDataSize  : Integer;
  PatchChunkInfo : TVstPatchChunkInfo;
  b              : Char;
  UseChunk       : Boolean;
begin
 if not assigned(FVstEffect) then exit;

 // read nineth byte to check, whether chunk are used here
 Stream.Seek(9, 0);
 Stream.Read(b, 1);
 UseChunk := (b <> #$78);
 Stream.Seek(0, 0);

// if eoProgramChunks in EffectOptions then
 if UseChunk then
  begin
   assert(effFlagsProgramChunks in EffectOptions);
   Stream.Read(FXChunkset, SizeOf(TFXChunkset) - SizeOf(Pointer));

   // check unique ID
   SwapLong(FXChunkset.fxId);
   if FVstEffect^.UniqueID <> FXChunkset.fxId
    then raise Exception.Create(RStrPresetFileNotForThisPlugin);

   // set program name
   SetProgramName(FXChunkset.prgName);

   // allocate chunk data memory
   ChunkDataSize := Stream.Size - Stream.Position;
   GetMem(ChunkData, ChunkDataSize);
   try
    if Stream.Read(ChunkData^, ChunkDataSize) <> ChunkDataSize
     then raise Exception.Create('chunk error, actual stream smaller than chunk');
    SetChunk(ChunkData, ChunkDataSize, True);
   finally
    Dispose(ChunkData);
   end;
  end
 else
  begin
   Stream.Read(FXPreset, SizeOf(TFXPreset) - SizeOf(Pointer));

   // check unique ID
   SwapLong(FXPreset.fxId);
   if FVstEffect^.UniqueID <> FXPreset.fxId
    then raise Exception.Create(RStrPresetFileNotForThisPlugin);

   with PatchChunkInfo do
    begin
     version := 1;
     pluginUniqueID := FVstEffect.uniqueID;
     pluginVersion  := FVstEffect.version;
     numElements    := FVstEffect.numParams; // Number of Programs (Bank)
    end; 
   BeginLoadProgram(@PatchChunkInfo);

   SetProgramName(FXPreset.prgName);
   SwapLong(FXPreset.numParams);
   for ParamNo := 0 to FXPreset.numParams - 1 do
    begin
     Stream.Read(Param, SizeOf(Single));
     SwapLong(Param);
     SetParameter(ParamNo, Param);
    end;
  end;
end;

procedure TCustomVstPlugIn.SaveBank(Stream: TStream);
var
  FXSet         : TFXSet;
  FXChunkBank   : TFXChunkBank;
  PrgNo         : Integer;
  ChunkDataSize : Integer;
  ChunkData     : Pointer;
  FXPreset      : TFXPreset;
begin
 if not assigned(FVstEffect) then exit;
 Stream.Seek(0, 0);
 if effFlagsProgramChunks in EffectOptions then
  with FXChunkBank do
   begin
    chunkMagic    := 'KncC';
    fxMagic       := 'hCBF';
    version       := 1;
    fxID          := FVstEffect^.UniqueID;
    fxVersion     := FVstEffect^.version;
    numPrograms   := FVstEffect^.numPrograms;

    ChunkDataSize := GetChunk(@ChunkData, False);
    ChunkSize     := ChunkDataSize;
    ByteSize      := SizeOf(FXChunkBank) - SizeOf(LongInt) * 3 + chunkSize + 8;

    // swap-o-matic
    SwapLong(chunkMagic);
    SwapLong(fxMagic);
    SwapLong(version);
    SwapLong(fxID);
    SwapLong(fxVersion);
    SwapLong(numPrograms);
    SwapLong(ByteSize);
    SwapLong(chunkSize);

    // write data to stream
    Stream.WriteBuffer(FXChunkBank, SizeOf(FXChunkBank) - SizeOf(Pointer));
    Stream.WriteBuffer(ChunkData^, ChunkDataSize);
   end
 else
  with FXSet do
   begin
    chunkMagic    := 'KncC';
    fxMagic       := 'hCBF';
    version       := 1;
    fxID          := FVstEffect^.UniqueID;
    fxVersion     := FVstEffect^.version;
    numPrograms   := FVstEffect^.numPrograms;
    ByteSize      := SizeOf(FXSet) - SizeOf(LongInt) + (SizeOf(TFXPreset) + (numParams - 1) * SizeOf(Single)) * numPrograms;

    // swap-o-matic
    SwapLong(chunkMagic);
    SwapLong(fxMagic);
    SwapLong(version);
    SwapLong(fxID);
    SwapLong(fxVersion);
    SwapLong(numPrograms);
    SwapLong(ByteSize);

    Stream.WriteBuffer(FXSet, SizeOf(FXSet) - SizeOf(Single));
    for PrgNo := 0 to numPrograms - 1 do
     begin
      FXPreset := GetPreset(PrgNo);
      try
       Stream.WriteBuffer(FXPreset, SizeOf(FXPreset) - SizeOf(Single));
       Stream.WriteBuffer(FXPreset.Params^, SizeOf(Single) * numParams);
      finally
       Dispose(FXPreset.params);
      end;
     end;
   end;
end;


procedure TCustomVstPlugIn.ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: Integer);
begin
 if FVstEffect <> nil then
  with FVstEffect^ do
   if effFlagsCanReplacing in EffectFlags
    then ProcessReplacing(FVstEffect, Inputs, Outputs, SampleFrames)
    else Process(FVstEffect, Inputs, Outputs, SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessAudioDataCollection(Inputs,
  Outputs: TAudioDataCollection32);
var
  Channel : Integer;
  InList,
  OutList : array of PDAVSingleFixedArray;
begin
 assert(Inputs.SampleFrames = Outputs.SampleFrames);
 assert(Inputs.ChannelCount >= numInputs);
 assert(Outputs.ChannelCount >= numOutputs);

 SetLength(InList, Inputs.ChannelCount);
 for Channel := 0 to Inputs.ChannelCount - 1
  do InList[Channel] := Inputs[Channel].ChannelDataPointer;

 SetLength(OutList, Outputs.ChannelCount);
 for Channel := 0 to Outputs.ChannelCount - 1
  do OutList[Channel] := Outputs[Channel].ChannelDataPointer;
 ProcessAudio(PPSingle(InList), PPSingle(OutList), Inputs.SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessAudioDataCollection(Inputs,
  Outputs: TAudioDataCollection64);
var
  Channel : Integer;
  InList,
  OutList : array of PDAVDoubleFixedArray;
begin
 assert(Inputs.SampleFrames = Outputs.SampleFrames);
 assert(Inputs.ChannelCount >= numInputs);
 assert(Outputs.ChannelCount >= numOutputs);

 SetLength(InList, Inputs.ChannelCount);
 for Channel := 0 to Inputs.ChannelCount - 1
  do InList[Channel] := Inputs[Channel].ChannelDataPointer;

 SetLength(OutList, Outputs.ChannelCount);
 for Channel := 0 to Outputs.ChannelCount - 1
  do OutList[Channel] := Outputs[Channel].ChannelDataPointer;
 ProcessDoubleReplacing(PPDouble(InList), PPDouble(OutList), Inputs.SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessAudioDataCollectionInplace(
  AudioData: TAudioDataCollection32);
var
  Channel  : Integer;
  DataList : array of PDAVSingleFixedArray;
begin
 assert(AudioData.ChannelCount >= numInputs);
 assert(AudioData.ChannelCount >= numOutputs);

 SetLength(DataList, AudioData.ChannelCount);
 for Channel := 0 to AudioData.ChannelCount - 1
  do DataList[Channel] := AudioData[Channel].ChannelDataPointer;

 ProcessAudio(PPSingle(DataList), PPSingle(DataList), AudioData.SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessAudioDataCollectionInplace(
  AudioData: TAudioDataCollection64);
var
  Channel  : Integer;
  DataList : array of PDAVDoubleFixedArray;
begin
 assert(AudioData.ChannelCount >= numInputs);
 assert(AudioData.ChannelCount >= numOutputs);

 SetLength(DataList, AudioData.ChannelCount);
 for Channel := 0 to AudioData.ChannelCount - 1
  do DataList[Channel] := AudioData[Channel].ChannelDataPointer;

 ProcessDoubleReplacing(PPDouble(DataList), PPDouble(DataList), AudioData.SampleFrames);
end;

function TCustomVstPlugIn.GetInitialDelay: Integer;
begin
 if assigned(FVstEffect)
  then result := FVstEffect.initialDelay
  else result := 0;
end;

{$IFDEF VstHostGUI}
procedure TCustomVstPlugIn.SetGUIStyle(const Value: TGUIStyle);
begin
 if FEditOpen
  then raise Exception.Create(RStrCloseEditorFirst)
  else fGUIStyle := Value;
end;
{$ENDIF}
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////

initialization
  audioMaster := AudioMasterCallback;
  {$IFDEF SearchPluginAndHost}
  HostList    := TObjectList.Create(False);
  {$ENDIF}
  {$IFDEF VstHostGUI}
  HostWindows := TObjectList.Create;
  {$ENDIF}

finalization
  {$IFDEF VstHostGUI}
  FreeAndNil(HostWindows);
  {$ENDIF}
  {$IFDEF SearchPluginAndHost}
  FreeAndNil(HostList);
  {$ENDIF}

end.
