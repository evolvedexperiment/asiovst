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

{$I ASIOVST.INC}
{-$DEFINE SB}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, Dynlibs, {$ELSE} Windows, Messages, {$ENDIF}
  {$IFDEF MSWINDOWS} Registry, {$ENDIF} SysUtils, Classes, Graphics, Controls,
  Forms, StdCtrls, ComCtrls, Dialogs, DAV_Common, DAV_AudioData, DAV_VSTEffect, 
  DAV_DLLLoader {$IFDEF SB}, TFlatScrollbarUnit{$ENDIF};

type
  TVendorSpecificEvent = function(opcode : TAudioMasterOpcode; index, value: LongInt; ptr: Pointer; opt: Single): Integer of object;
  TVstShowEditEvent = procedure(Sender: TObject; Form: TForm) of object;
  TVstAutomateEvent = procedure(Sender: TObject; ParamIndex, ParamValue: LongInt) of object;
  TVstProcessEventsEvent = procedure(Sender: TObject; p: PVstEvents) of object;
  TVstAutomationNotifyEvent = procedure(Sender: TObject; ParameterIndex: Integer) of object;
  TVstSampleRateChangedEvent = procedure(Sender: TObject; SampleRate: Single) of object;
  TVstPinConnectedEvent = function(Sender: TObject; PinNr: Integer; isInput: Boolean):Boolean of object;
  TVstOfflineEvent = procedure(Sender: TObject; VstOfflineTaskPointer: PVstOfflineTask) of object;
  TGUIStyle = (gsDefault, gsOld, gsList);

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

  TCustomVstPlugIn = class(TCollectionItem)
  private
    FDLLHandle                     : THandle;
    FDisplayName                   : string;
    FMainFunction                  : TMainProc;
    FEditOpen                      : Boolean;
    FWantMidi                      : Boolean;
    FNeedIdle                      : Boolean;
    FActive                        : Boolean;
    FnumInputs                     : Integer;
    FnumOutputs                    : Integer;
    FnumPrograms                   : Integer;
    FnumParams                     : Integer;
    Fversion                       : Integer;
    FProgramNr                     : Integer;
    FUniqueID                      : string;
    FVstVersion                    : Integer;
    FPlugCategory                  : TVstPluginCategory;
    FDLLFileName                   : TFileName;
    FVstOfflineTask                : TVstOfflineTask;
    FReplaceOrAccumulate           : TReplaceOrAccumulate;
    FProcessLevel                  : TCurrentProcessLevel;
    FAutomationState               : TAutomationState;
    FOnAfterLoad                   : TNotifyEvent;
    FOnShowEdit                    : TVstShowEditEvent;
    FOnCloseEdit                   : TNotifyEvent;
    FOnProcessEvents               : TVstProcessEventsEvent;
    FOnAMAutomate                  : TVstAutomateEvent;
    FOnAMIdle                      : TNotifyEvent;
    FOnAMNeedIdle                  : TNotifyEvent;
    FOnAMWantMidi                  : TNotifyEvent;
    FOnAMIOChanged                 : TNotifyEvent;
    FOnAMOfflineStart              : TNotifyEvent;
    FOnAMOfflineRead               : TVstOfflineEvent;
    FOnAMOfflineWrite              : TVstOfflineEvent;
    FOnAMOfflineGetCurrentPass     : TNotifyEvent;
    FOnAMOfflineGetCurrentMetaPass : TNotifyEvent;
    FOnAMSetOutputsampleRate       : TVstSampleRateChangedEvent;
    FOnAMUpdateDisplay             : TNotifyEvent;
    FOnAMBeginEdit                 : TVstAutomationNotifyEvent;
    FOnAMEndEdit                   : TVstAutomationNotifyEvent;
    FOnAMPinConnected              : TVstPinConnectedEvent;
    FOnVendorSpecific              : TVendorSpecificEvent;
    FGUIFormCreated                : Boolean;
    FGUIStyle                      : TGUIStyle;
    FInternalDLLLoader             : TDLLLoader;
    function GetEffOptions: TEffFlags;
    function GetEntryPoints(theDll: TFileName): Integer;
    function GetInitialDelay: Integer;
    function GetnumInputs: Integer;
    function GetnumOutputs: Integer;
    function GetnumParams: Integer;
    function GetnumPrograms: Integer;
    function GetPreset(i: Integer): TFXPreset;
    function VstDispatch(opCode : TDispatcherOpcode; Index: Integer = 0; value: Integer = 0; pntr: Pointer = nil; opt: Double = 0): Integer; {overload;} //virtual;
    procedure Activate(b: Boolean);
    procedure EditActivateHandler(Sender: TObject);
    procedure EditDeactivateHandler(Sender: TObject);
    procedure FormCloseHandler(Sender: TObject; var Action: TCloseAction);
    procedure ParamChange(Sender: TObject);
    procedure SetBlockSize(value: Integer);
    procedure SetDLLFileName(VstFilename: TFilename);
    {$IFDEF SB}
    procedure ScrollChange(Sender: TObject; ScrollPos: Integer);
    {$ELSE}
    procedure TrackChange(Sender: TObject);
    procedure SetGUIStyle(const Value: TGUIStyle);
    procedure ListParamChange(Sender: TObject);
    {$ENDIF}
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure EditClose;
  public
    PVstEffect          : PVSTEffect;
    GUIForm             : TForm;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function BeginLoadBank(PatchChunkInfo : PVstPatchChunkInfo): Integer;
    function CanBeAutomated(index: Integer): Integer;
    function CanDo(pntr: PChar): Integer;
    function ConnectInput(InputNr: Integer; State: Boolean): Integer;
    function ConnectOutput(OutputNr: Integer; State: Boolean): Integer;
    function CopyCurrentProgramTo(Destination: Integer): Boolean;
    function EditGetRect: ERect;
    function EditIdle: Integer;
    function EditKeyDown(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
    function EditKeyUp(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
    function EditOpen(Handle: THandle): Integer;
    function GetChunk(pntr: Pointer; isPreset: Boolean = False): Integer;
    function GetCurrentMidiProgram(MidiProgramNamePointer : PMidiProgramName): Integer;
    function GetCurrentPosition: Integer;
    function GetDestinationBuffer: Integer;
    function GetDisplayName: string; override;
    function GetEffectName: string;
    function GetErrorText: string;
    function GetFriendlyNameString(const StringLength: Integer): string;
    function GetIcon: Integer;
    function GetInputProperties(InputNr: Integer): TVstPinProperties;
    function GetMidiKeyName(MidiKeyNamePointer: PMidiKeyName): Integer;
    function GetMidiProgramCategory(MidiProgramCategoryPointer : PMidiProgramCategory): Integer;
    function GetMidiProgramName(MidiProgramNamePointer : PMidiProgramName): Integer;
    function GetNumProgramCategories: Integer;
    function GetOutputProperties(OutputNr: Integer): TVstPinProperties;
    function GetParamDisplay(index: Integer): string;
    function GetParameter(index: Integer): Single; virtual;
    function GetParameterProperties(Parameter: Integer): TVstParameterPropertyRecord;
    function GetParamLabel(index: Integer): string;
    function GetParamName(index: Integer): string;
    function GetPlugCategory: TVstPluginCategory;
    function GetProductString: string;
    function GetProgram: Integer;
    function GetProgramName: string;
    function GetProgramNameIndexed(Category, index: Integer; ProgramName: PChar): Integer;
    function GetRect: TRect;
    function GetSpeakerArrangement(SpeakerIn, SpeakerOut:PVstSpeakerArrangement): Integer;
    function GetTailSize: Integer;
    function GetVendorString: string;
    function GetVendorVersion: Integer;
    function GetVstVersion: Integer;
    function GetVu: Single;
    function HasMidiProgramsChanged: Integer;
    function Identify: Integer;
    function Idle: Integer;
    function KeysRequired: Integer;
    function LoadFromFile(PluginDll: TFilename): Boolean;
    function LoadFromStream(Stream: TStream): Boolean;
    function OfflineNotify(pntr: PVstAudioFile; numAudioFiles: Integer; start: Boolean): Integer;
    function OfflinePrepare(pntr: PVstOfflineTask; count: Integer): Integer;
    function OfflineRun(pntr: PVstOfflineTask; count :Integer): Integer;
    function ProcessEvents(pntr: PVstEvents): Integer;
    function ProcessVarIo(varIo: PVstVariableIo): Integer;
    function SetBlockSizeAndSampleRate(blockSize: Integer; sampleRate: Single): Integer;
    function SetBypass(onOff: Boolean): Integer;
    function SetChunk(data: Pointer; ByteSize: Integer; isPreset: Boolean = False): Integer;
    function SetSpeakerArrangement(pluginInput: PVstSpeakerArrangement; pluginOutput: PVstSpeakerArrangement): Boolean;
    function ShellGetNextPlugin(var PluginName:String): Integer;
    function String2Parameter(ParameterName: string): Integer;
    function VendorSpecific(index, value:Integer; pntr: Pointer; opt: Single): Integer;
    procedure BeginLoadProgram(PatchChunkInfo : PVstPatchChunkInfo);
    procedure BeginSetProgram;
    procedure Close;
    procedure CloseEdit;
    procedure EditActivate;
    procedure EditDeactivate;
    procedure EndSetProgram;
    procedure LoadBank(FileName: TFileName); overload;
    procedure LoadBank(Stream: TStream); overload;
    procedure LoadPreset(FileName: TFileName); overload;
    procedure LoadPreset(Stream: TStream); overload;
    procedure LoadVSTDLL;
    procedure MainsChanged(bOn: Boolean);
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
    procedure SetEditKnobMode(Mode : TKnobMode);
    procedure SetPanLaw(PanLaw: TVstPanLawType; Gain: Single);
    procedure SetParameter(index: Integer; parameter: Single); virtual;
    procedure SetProgram(lValue: Integer);
    procedure SetProgramName(newName: string);
    procedure SetSampleRate(fSR: Double);
    procedure SetTotalSampleToProcess;
    procedure SetViewPosition(x, y: Integer);
    procedure ShowEdit(Form: TForm); overload;
    procedure ShowEdit; overload;
    procedure StartProcess;
    procedure StopProcess;
    procedure UnLoad;

    property Parameters[Index: Integer]:Single read GetParameter write SetParameter;
    property VstOfflineTask : TVstOfflineTask read FVstOfflineTask;
  published
    property Active: Boolean read FActive write Activate default False;
    property AutomationState: TAutomationState read FAutomationState Write FAutomationState default as0NotSupported;
    property CurrentProcessLevel: TCurrentProcessLevel read FProcessLevel write FProcessLevel default cpl0NotSupported;
    property DisplayName: string read GetDisplayName write FDisplayName;
    property DLLFileName: TFileName read FDLLFileName write SetDLLFileName;
    property EditVisible: Boolean read FEditOpen;
    property EffectOptions: TEffFlags read GetEffOptions stored False;
    property GUIStyle : TGUIStyle read fGUIStyle write SetGUIStyle default gsDefault;
    property InitialDelay: Integer read GetInitialDelay stored False;
    property numInputs: Integer read GetnumInputs stored False default -1 ;
    property numOutputs: Integer read GetnumOutputs stored False default -1 ;
    property numParams: Integer read GetnumParams stored False default -1;
    property numPrograms: Integer read GetnumPrograms stored False default -1 ;
    property PlugCategory: TVstPluginCategory read FPlugCategory stored False;
    property PluginVstVersion: Integer read FVstVersion stored False default -1;
    property ProductString: string read GetProductString stored False;
    property ProgramName: string read GetProgramName write SetProgramName;
    property ProgramNr: Integer read GetProgram write SetProgram default -1;
    property ReplaceOrAccumulate: TReplaceOrAccumulate read FReplaceOrAccumulate write FReplaceOrAccumulate default roa0NotSupported;
    property UniqueID: string read FUniqueID stored False;
    property VendorString: string read GetVendorString stored False;
    property VendorVersion: Integer read GetVendorVersion stored False default -1;
    property Version: Integer read Fversion stored False default -1;
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
    property OnCloseEdit: TNotifyEvent read FOnCloseEdit write FOnCloseEdit;
    property OnProcessEvents: TVstProcessEventsEvent read FOnProcessEvents write FOnProcessEvents;
    property OnShowEdit: TVstShowEditEvent read FOnShowEdit write FOnShowEdit;
    property OnVendorSpecific: TVendorSpecificEvent read FOnVendorSpecific write FOnVendorSpecific;
  end;

  TVstPlugIn = class(TCustomVstPlugIn)
  private
    property Active;
    property AutomationState;
    property CurrentProcessLevel;
    property DisplayName;
    property DLLFileName;
    property EditVisible;
    property EffectOptions;
    property GUIStyle;
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
    property OnCloseEdit;
    property OnProcessEvents;
    property OnShowEdit;
    property OnVendorSpecific;
  end;

  TVstPlugIns = class(TOwnedCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TCustomVstPlugIn;
    procedure SetItem(Index: Integer; const Value: TCustomVstPlugIn);
  protected
    property Items[Index: Integer]: TCustomVstPlugIn read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TCustomVstPlugIn;
    function CloneAdd(Source: TCustomVstPlugIn): TCustomVstPlugIn;
    function Insert(Index: Integer): TCustomVstPlugIn;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TCustomVstTimeInformation = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    function GetVTI(Index :Integer) : Integer;
    function GetVTIDouble(Index :Integer) : Double;
    function GetVTIflags :TVstTimeInfoFlags;
    procedure SetVTI(Index,Value :Integer);
    procedure SetVTIDouble(Index :Integer; Value: Double);
    procedure SetVTIflags(Flags:TVstTimeInfoFlags);
  protected
    fVstTimeInfo        : TVstTimeInfo;
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

  TCustomVstHost = class(TComponent)
  private
    FInputLatency   : Integer;
    FOutputLatency  : Integer;
    FVstPlugIns     : TVstPlugIns;
    FLanguage       : TVstHostLanguage;
    FnumAutomatable : Integer;
    FParamQuan      : Integer;
    FVendorString   : string;
    FVendorVersion  : Integer;
    FProductString  : string;
    FPlugInDir      : string;
    FVTI            : TVstTimeInformation;
    FAutoIdle       : Boolean;
    FOnCreate       : TNotifyEvent;
    FOnDestroy      : TNotifyEvent;
    function getBlockSize : Integer;
    function getHostCanDos: THostCanDos;
    function getHostTempo: Single;
    function getHostVersion: Integer;
    function GetItem(Index: Integer): TCustomVstPlugIn;
    function GetPluginCount: Integer;
    procedure SetBlockSize(bs: Integer);
    procedure SetHostCanDos(hcd: THostCanDos);
    procedure SetHostTempo(Tempo: Single);
    procedure SetHostVersion(hv: Integer);
    procedure SetVstPlugIns(const Value: TVstPlugIns);
    procedure VstTimeInfoChanged(Sender: TObject);
  protected
    property Items[Index: Integer]: TCustomVstPlugIn read GetItem; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateVstTimeInfo(samples: word = 1);
    procedure ResetVstTimeInformation;

    property BlockSize: Integer read getBlockSize write setBlocksize default 2048;
    property CanDos: THostCanDos read getHostCanDos write setHostCanDos;
    property Count: Integer read GetPluginCount;
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
    property VstVersion: Integer read getHostVersion write setHostVersion;
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

var audioMaster : TAudioMasterCallbackFunc;

procedure Register;
function string2Language(LanguageString: string): TVstHostLanguage;
function PlugCategory2String(Category: TVstPluginCategory):string;
function EffOptions2String(EffOpts: TEffFlags):string;

implementation

uses
  contnrs;

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
  RStrLoadingFailed              = 'Loading failed!';
  RStrFileDoesNotExist           = 'File %d does not exists';
  RStrPlugInCouldNotBeLoaded     = 'PlugIn %d could not be loaded';
  RStrPlugInStreamError          = 'PlugIn could not be loaded from stream';
  RStrBankFileDoesNotExist       = 'Bank file does not exist';
  StrPresetFileDoesNotExist      = 'Preset file does not exist';
  RStrBankFileNotForThisPlugin   = 'Bank file not for this plugin!';
  RStrPresetFileNotForThisPlugin = 'Preset file not for this plugin!';
  RStrCloseEditorFirst           = 'Close editor first!';
  RStrPluginNotValid             = 'This is not a valid Vst Plugin!';

var
  FBlockSize     : Integer = 2048;
  FHostVersion   : Integer = 2300;
  FHostCanDos    : THostCanDos;
  FHostTempo     : Single = 120;
  FSampleRate    : Single  = 44100;
  theHost        : TCustomVstHost;
  HostDialog     : TCommonDialog;
  HostWindows    : TObjectList;

const
  SCRound8087CW     : Word = $133F; // round FPU codeword, with exceptions disabled
  SCChop8087CW      : Word = $1F3F; // Trunc (chop) FPU codeword, with exceptions disabled
  SCRoundDown8087CW : Word = $173F; // exceptions disabled
  SCRoundUp8087CW   : Word = $1B3F; // exceptions disabled

///////////////////////////////////////////////////////////////////////////////

function WhatIfNoEntry: Integer;
begin
 raise Exception.Create(RStrPluginNotValid);
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

function AudioMasterCallback(effect: PVSTEffect; opcode : TAudioMasterOpcode; index,value: LongInt; ptr: Pointer; opt: Single): LongInt; cdecl;
var
  thePlug   : TCustomVstPlugIn;
  PlugNr, i : Integer;
begin
 try
   thePlug := nil;
   for PlugNr := 0 to theHost.VstPlugIns.Count - 1 do
    if theHost.VstPlugIns[PlugNr].PVstEffect = Effect then
     begin
      thePlug := theHost.VstPlugIns[PlugNr];
      Break;
     end;
   asm
    fnclex                  // Don't raise pending exceptions enabled by the new flags
    fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
   end;

   result := 0;
   case TAudiomasterOpcode(opcode) of
    audioMasterAutomate                    : begin
                                              if Assigned(thePlug) then
                                               if Assigned(thePlug.FOnAMAutomate)
                                                then thePlug.FOnAMAutomate(thePlug, Index, Value);
                                              result := 0;
                                             end;
    audioMasterVersion                     : result := FHostVersion;
    audioMasterIdle                        : if Assigned(thePlug) then
                                              begin
                                               thePlug.FNeedIdle := True;
                                               if Assigned(thePlug.FOnAMIdle)
                                                then thePlug.FOnAMIdle(thePlug);
                                               if theHost.FautoIdle then
                                                begin
                                                 if thePlug.EditVisible then thePlug.EditIdle;
                                                 for i := 0 to theHost.VstPlugIns.Count - 1 do // Norm-Konform!
                                                  if theHost.VstPlugIns[i].EditVisible then theHost.VstPlugIns[i].EditIdle;
                                                end;
                                              end;
    audioMasterCurrentId                   : if thePlug <> nil
                                              then thePlug.Identify
                                              else result := 0; // returns the unique id of a plug that's currently loading
    audioMasterPinConnected                : if Assigned(thePlug) then
                                              if Assigned(thePlug.FOnAMPinConnected)
                                               then
                                                begin
                                                 if thePlug.FOnAMPinConnected(thePlug,Index,value=0)
                                                  then Result := 0
                                                  else Result := 1;
                                                end
                                               else result := 0
                                              else result := 0;
    audioMasterWantMidi                    : if Assigned(thePlug) then
                                              if Assigned(thePlug.FOnAMWantMidi)
                                               then thePlug.FOnAMWantMidi(thePlug);
    audioMasterGetTime                     : Result := LongInt(@theHost.VstTimeInfo.FVstTimeInfo);
    audioMasterProcessEvents               : if Assigned(thePlug.FOnProcessEvents)
                                              then thePlug.FOnProcessEvents(thePlug, ptr);
    audioMasterSetTime                     : {$IFDEF Debug} raise Exception.Create('TODO: audioMasterSetTime, VstTimenfo* in <ptr>, filter in <value>, not supported') {$ENDIF Debug};
    audioMasterTempoAt                     : result := round(FHostTempo) * 10000;
    audioMasterGetNumAutomatableParameters : result := theHost.FnumAutomatable;
    audioMasterGetParameterQuantization    : if Value=-1
                                              then result := theHost.FParamQuan
                                              else {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetParameterQuantization, returns the Integer value for +1.0 representation') {$ENDIF Debug};
                                              // or 1 if full Single float precision is maintained
                                              // in automation. parameter index in <value> (-1: all, any)
    audioMasterIOChanged                   : if Assigned(thePlug) then
                                              begin
                                               thePlug.FnumInputs := effect.numInputs;
                                               thePlug.FnumOutputs := effect.numOutputs;
                                               thePlug.FnumPrograms := effect.numPrograms;
                                               thePlug.FnumParams := effect.numParams;
                                               if Assigned(thePlug.FOnAMIOChanged)
                                                then thePlug.FOnAMIOChanged(thePlug);
                                              end;
    audioMasterNeedIdle                    : if Assigned(thePlug) then
                                              begin
                                               thePlug.FNeedIdle := True;
                                               if Assigned(thePlug.FOnAMNeedIdle)
                                                then thePlug.FOnAMNeedIdle(thePlug);
                                               if theHost.FAutoIdle then thePlug.Idle;
                                              end;
    audioMasterSizeWindow                  : begin
                                              if Assigned(thePlug) then
                                               if assigned(thePlug.GUIForm) then
                                               begin
                                                if pos('DASH', uppercase(thePlug.VendorString)) > 0 then
                                                 result := 0
                                                else if pos('WUSIK', uppercase(thePlug.VendorString)) > 0 then
                                                 result := 0
                                                else
                                                begin
                                                 thePlug.GUIForm.ClientWidth := index;
                                                 thePlug.GUIForm.ClientHeight := value;
                                                 Result := 1;
                                                end;
                                               end;
                                             end;
    audioMasterGetSampleRate               : result := round(FSampleRate);
    audioMasterGetBlockSize                : result := FBlockSize;
    audioMasterGetInputLatency             : if theHost <> nil then result := theHost.FInputLatency else result := 0;
    audioMasterGetOutputLatency            : if theHost <> nil then result := theHost.FOutputLatency else result := 0;
    audioMasterGetPreviousPlug             : begin
                                              if PlugNr = 0 then Result := 0;
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
    audioMasterGetVendorString             : StrCopy(PChar(ptr), PChar(theHost.VendorString));
    audioMasterGetProductString            : StrCopy(PChar(ptr), PChar(theHost.ProductString));
    audioMasterGetVendorVersion            : if theHost <> nil
                                              then result := theHost.FVendorVersion
                                              else result := 0;
    audioMasterVendorSpecific              : if assigned(thePlug) then
                                              if assigned(thePlug.FOnVendorSpecific)
                                               then result := thePlug.FOnVendorSpecific(TAudiomasterOpcode(opcode), index, value, ptr, opt)
                                               else result := 0
                                              else result := 0;
    audioMasterSetIcon                     : {$IFDEF Debug} showmessage('TODO: audioMasterSetIcon, void* in <ptr>, format not defined yet, Could be a CBitmap .') {$ENDIF Debug};
    audioMasterCanDo                       : begin
                                              if      ShortString(PChar(ptr)) = 'sendVstEvents' then Result := Integer(hcdSendVstEvents in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'sendVstMidiEvent' then Result := Integer(hcdSendVstMidiEvent in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'sendVstTimeInfo' then Result := Integer(hcdSendVstTimeInfo in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'receiveVstEvents' then Result := Integer(hcdReceiveVstEvents in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'receiveVstMidiEvent' then Result := Integer(hcdReceiveVstMidiEvent in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'receiveVstTimeInfo' then Result := Integer(hcdReceiveVstTimeInfo in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'reportConnectionChanges' then Result := Integer(hcdReportConnectionChanges in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'acceptIOChanges' then Result := Integer(hcdAcceptIOChanges in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'sizeWindow' then Result := Integer(hcdSizeWindow in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'asyncProcessing' then Result := Integer(hcdAsyncProcessing in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'offline' then Result := Integer(hcdOffline in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'supplyIdle' then Result := Integer(hcdSupplyIdle in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'supportShell' then Result := Integer(hcdSupportShell in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'openFileSelector' then Result := Integer(hcdOpenFileSelector in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'closeFileSelector' then Result := Integer(hcdcloseFileSelector in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'editFile' then Result := Integer(hcdEditFile in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'shellCategory' then Result := Integer(hcdShellCategory in FHostCanDos)
                                              else if ShortString(PChar(ptr)) = 'startStopProcess' then Result := Integer(hcdStartStopProcess in FHostCanDos)
                                              else Result := 0;
                                             end;
    audioMasterGetLanguage                 : result := Integer(theHost.FLanguage);
    audioMasterOpenWindow                  : if ptr <> nil then
                                              begin
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
                                              end;
    audioMasterCloseWindow                 : begin
                                              {$IFDEF Debug}
                                              ShowMessage('TODO: audioMasterCloseWindow, ' +
                                              'close window, platform specific handle in <ptr>')
                                              {$ENDIF Debug};
                                             end;
    audioMasterGetDirectory                : result := LongInt(PChar(theHost.FPlugInDir));
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
//                                                      PVstFileSelect(ptr).returnPath := PChar(TOpenDialog(HostDialog).FileName);
//                                                      StrCopy(PVstFileSelect(ptr).returnPath,PChar(TOpenDialog(HostDialog).FileName));
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
                                                      PVstFileSelect(ptr).returnPath := PChar(FileName);
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
                                                        PVstFileSelect(ptr).returnPath := PChar(FileName);
                                                        PVstFileSelect(ptr).sizeReturnPath := Length(FileName);
                                                       end;
                                                   end;
                                                  end;
                                                 kVstDirectorySelect: {$IFDEF Debug} ShowMessage('TODO: Not implemented!') {$ENDIF Debug};
                                                end;
                                                if PVstFileSelect(ptr).returnPath = nil then
                                                 begin
                                                  HostDialog.Free;
                                                  HostDialog := nil;
                                                 end;
                                                Result := Integer(True);
                                               end;
                                             end;
    audioMasterCloseFileSelector           : begin
                                              if assigned(HostDialog) then
                                               case PVstFileSelect(ptr).Command of
                                                kVstFileLoad:
                                                 begin
                                                  if TOpenDialog(HostDialog).Title = PVstFileSelect(ptr).title then
                                                  begin
                                                   HostDialog.Free;
                                                   HostDialog := nil;
                                                  end;
                                                 end;
                                                kVstFileSave:
                                                 begin
                                                  if TSaveDialog(HostDialog).Title = PVstFileSelect(ptr).title then
                                                  begin
                                                   HostDialog.Free;
                                                   HostDialog := nil;
                                                  end;
                                                 end;
                                                kVstMultipleFilesLoad:
                                                 begin
                                                  if TOpenDialog(HostDialog).Title = PVstFileSelect(ptr).title then
                                                  begin
                                                   HostDialog.Free;
                                                   HostDialog := nil;
                                                  end;
                                                 end;
                                                kVstDirectorySelect:
                                                 begin
                                                 end;
                                                else
                                                 {$IFDEF Debug} raise Exception.Create('TODO: close a fileselector operation with VstFileSelect* in <ptr>: Must be always called after an open !') {$ENDIF Debug};
                                               end;
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

///////////////////////////////////////////////////////////////////////////////

{$REGION 'TCustomVstTimeInformation implementation'}

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
    fVstTimeInfo := Self.FVstTimeInfo;
   finally
    Change;
   end
 else inherited AssignTo(Dest);
end;

function TCustomVstTimeInformation.GetVTIflags :TVstTimeInfoFlags;
begin
 result := fVstTimeInfo.Flags;
end;

function TCustomVstTimeInformation.GetVTIDouble(Index :Integer): Double;
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

function TCustomVstTimeInformation.GetVTI(Index :Integer) :Integer;
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

procedure TCustomVstTimeInformation.SetVTI(Index,Value :Integer);
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

procedure TCustomVstTimeInformation.SetVTIDouble(Index :Integer; Value: Double);
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

procedure TCustomVstTimeInformation.SetVTIflags(Flags:TVstTimeInfoFlags);
begin
 fVstTimeInfo.Flags := Flags;
end;

{$ENDREGION}

///////////////////////////////////////////////////////////////////////////////

{$REGION 'TCustomVstHost implementation'}

{ TCustomVstHost }

constructor TCustomVstHost.Create(AOwner: TComponent);
begin
 inherited;
// if AOwner <> nil then
 theHost     := Self;
 FSampleRate := 44100;
 FBlocksize  := 2048;
 FLanguage   := kVstLangEnglish;
 FHostCanDos := [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo,
                 hcdReceiveVstEvents, hcdReceiveVstMidiEvent,
                 hcdReceiveVstTimeInfo, hcdReportConnectionChanges,
                 hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing,
                 hcdOffline, hcdSupplyIdle, hcdStartStopProcess];

 {$IFDEF MSWINDOWS}
 with TRegistry.Create do
  try
   RootKey := HKEY_LOCAL_MACHINE;
   OpenKey('SOFTWARE\Vst',False);
   if ValueExists('VstPluginsPath')
    then FPlugInDir := ReadString('VstPluginsPath')
    else FPlugInDir := ExtractFileDir(Application.ExeName);
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
 if Assigned(FOnDestroy) then FOnDestroy(Self);
 if Assigned(FVstPlugIns) then
  begin
   while FVstPlugIns.Count > 0 do FVstPlugIns[0].Free;
   FVstPlugIns.Free;
  end;
 if Assigned(FVTI) then FreeAndNil(FVTI);
 inherited;
end;

procedure TCustomVstHost.VstTimeInfoChanged(Sender: TObject);
begin
 //
end;

function TCustomVstPlugIn.GetEffOptions: TEffFlags;
begin
 if Assigned(PVstEffect)
  then Result := PVstEffect.EffectFlags
  else Result := [];
end;

function TCustomVstHost.GetItem(Index: Integer): TCustomVstPlugIn;
begin
 Result := FVstPlugIns[Index];
end;

function TCustomVstHost.GetPluginCount: Integer;
begin
 result := FVstPlugIns.Count;
end;

procedure TCustomVstHost.SetVstPlugIns(const Value: TVstPlugIns);
begin
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

function TCustomVstHost.getHostVersion: Integer;
begin
 Result := FHostVersion;
end;

procedure TCustomVstHost.setHostVersion(hv: Integer);
begin
 FHostVersion := hv;
end;

function TCustomVstHost.getBlockSize: Integer;
begin
 Result := FBlockSize;
end;

procedure TCustomVstHost.setBlockSize(bs: Integer);
var
  i : Integer;
begin
 FBlockSize := bs;
 for i := 0 to VstPlugIns.Count - 1 do
  if assigned( VstPlugIns[i].PVstEffect) then
   VstPlugIns[i].SetBlockSize(FBlockSize);
end;

procedure TCustomVstHost.UpdateVstTimeInfo(samples: word = 1);
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

{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////

{$REGION 'TVstPlugIns implementation'}

{ TVstPlugIns }

function TVstPlugIns.Add: TCustomVstPlugIn;
begin
  Result := TCustomVstPlugIn(inherited Add);
end;

function TVstPlugIns.CloneAdd(Source: TCustomVstPlugIn): TCustomVstPlugIn;
begin
 Result := TCustomVstPlugIn(inherited Add);
 Source.AssignTo(Result);
end;

constructor TVstPlugIns.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TCustomVstPlugIn);
 FOwner := AOwner;
end;

function TVstPlugIns.GetItem(Index: Integer): TCustomVstPlugIn;
begin
 Result := TCustomVstPlugIn(inherited GetItem(Index));
end;

function TVstPlugIns.Insert(Index: Integer): TCustomVstPlugIn;
begin
 Result := TCustomVstPlugIn(inherited Insert(Index));
end;

procedure TVstPlugIns.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TVstPlugIns.SetItem(Index: Integer; const Value: TCustomVstPlugIn);
begin
  inherited SetItem(Index, Value);
end;
{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////

{$REGION 'TCustomVstPlugIn implementation'}

{ TCustomVstPlugIn }

procedure TCustomVstPlugIn.AssignTo(Dest: TPersistent);
var
  p: Pointer;
  i: Integer;
begin
 if Dest is TCustomVstPlugIn then with TCustomVstPlugIn(Dest) do
 begin
  DisplayName := Self.DisplayName;
  ReplaceOrAccumulate := Self.ReplaceOrAccumulate;
  CurrentProcessLevel := Self.CurrentProcessLevel;
  AutomationState := Self.AutomationState;
  OnAudioMasterAutomate := Self.OnAudioMasterAutomate;
  OnAudioMasterIdle := Self.OnAudioMasterIdle;
  OnAudioMasterNeedIdle := Self.OnAudioMasterNeedIdle;
  OnAudioMasterIOChanged := Self.OnAudioMasterIOChanged;
  OnAudioMasterWantMidi := Self.OnAudioMasterWantMidi;
  OnAudioMasterOfflineStart := Self.OnAudioMasterOfflineStart;
  OnAudioMasterOfflineRead := Self.OnAudioMasterOfflineRead;
  OnAudioMasterOfflineWrite := Self.OnAudioMasterOfflineWrite;
  OnAudioMasterOfflineGetCurrentPass := Self.OnAudioMasterOfflineGetCurrentPass;
  OnAudioMasterOfflineGetCurrentMetaPass := Self.OnAudioMasterOfflineGetCurrentMetaPass;
  OnAudioMasterSetOutputsampleRate := Self.OnAudioMasterSetOutputsampleRate;
  OnAudioMasterUpdateDisplay := Self.OnAudioMasterUpdateDisplay;
  OnAudioMasterBeginEdit := Self.OnAudioMasterBeginEdit;
  OnAudioMasterEndEdit := Self.OnAudioMasterEndEdit;
  OnAudioMasterPinConnected := Self.OnAudioMasterPinConnected;
  OnShowEdit := Self.OnShowEdit;
  OnCloseEdit := Self.OnCloseEdit;
  OnAfterLoad := Self.OnAfterLoad;
  OnProcessEvents := Self.OnProcessEvents;
  DLLFileName := Self.DLLFileName;
  Active := Self.Active;
  FProgramNr := Self.ProgramNr;
  i := Self.GetChunk(@p);
  TCustomVstPlugIn(Dest).SetChunk(p,i)
 end else
  inherited;
end;

function TCustomVstPlugIn.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

constructor TCustomVstPlugIn.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName    := inherited GetDisplayName;
 FMainFunction   := nil;
 PVstEffect      := nil;
 FEditOpen       := False;
 FNeedIdle       := False;
 FWantMidi       := False;
 FVstVersion     := -1;
 FPlugCategory   := vpcUnknown;
 FDLLFileName    := '';
 FGUIStyle       := gsDefault;
 fGUIFormCreated := False;
end;

destructor TCustomVstPlugIn.Destroy;
begin
 try
  if FDLLFilename <> '' then
   begin
    if EditVisible then CloseEdit;
    Unload;
    if (GUIForm <> nil) and fGUIFormCreated
     then FreeAndNil(GUIForm);
   end;
  if assigned(FInternalDLLLoader)
   then FreeAndNil(FInternalDLLLoader)
 finally
  inherited;
 end;
end;

procedure TCustomVstPlugIn.Activate(b: Boolean);
begin
  if FActive <> b then
  if b then Open else Close;
end;

procedure TCustomVstPlugIn.LoadVSTDLL;
var
  loadOK : Boolean;
begin
 loadOK := True;
 if not Assigned(PVstEffect) and FileExists(FDLLFileName)
  then loadOK := LoadFromFile(FDLLFileName)
  else FDLLFileName := '';
 asm
  fnclex                  // Don't raise pending exceptions enabled by the new flags
  fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
 end;
 if PVstEffect = nil then
  try
   if not loadOK
    then raise Exception.Create(RStrVSTPluginNotValid)
    else raise Exception.Create(RStrLoadingFailed);
  except
   raise;
  end;
end;

procedure TCustomVstPlugIn.Open;
var
  i      : Integer;
  tmp    : string;
  sl     : TStringList;
begin
 LoadVSTDLL;

 if LongInt(PVstEffect.Magic)<>FourCharToLong('V','s','t','P')
  then raise Exception.Create('There is no magic in it... failed!');
 if PVstEffect.uniqueID = 0 then
  begin
   sl :=  TStringList.Create;
   while ShellGetNextPlugin(tmp) <> 0 do
   sl.Add(tmp);
   sl.Free;
  end;

 Randomize;
 FEditOpen := False;
 FNeedIdle := False;
 FWantMidi := False;

 VstDispatch(effOpen);
 CanDo('bypass');
 //setPanLaw(0,0.707107)
 SetSampleRate(FSampleRate);
 SetBlockSize(FBlocksize);
 SetBypass(False);
 FActive := True;
 FUniqueID := ''; for i := 3 downto 0 do FUniqueID := FUniqueID + char(PVstEffect.uniqueID shr (i * 8));

 Fversion      := PVstEffect.version;
 FVstVersion   := GetVstVersion;
 FPlugCategory := GetPlugCategory;
 FnumInputs    := PVstEffect.numInputs;
 FnumOutputs   := PVstEffect.numOutputs;
 FnumPrograms  := PVstEffect.numPrograms;
 FnumParams    := PVstEffect.numParams;
 MainsChanged(True);
end;

procedure TCustomVstPlugIn.Close;
begin
 while FEditOpen do CloseEdit;
 if FActive then
  begin
   VstDispatch(effClose);
   PVstEffect := nil;
  end;
 FActive       := False;
 FVersion      := 0;
 FPlugCategory := vpcUnknown;
 FnumInputs    := 0;
 FnumOutputs   := 0;
 FnumPrograms  := 0;
 FnumParams    := 0;
end;

function TCustomVstPlugIn.VstDispatch(opCode : TDispatcherOpcode; Index, Value: Integer; Pntr: Pointer; opt: Double): Integer;
begin
 try
  asm
   fnclex                  // Don't raise pending exceptions enabled by the new flags
   fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
  end;
  if not assigned(PVstEffect) then
   result := 0
  else
   result := Integer(opCode);
   result := PVstEffect.Dispatcher(PVstEffect, TDispatcherOpcode(result), index, value, pntr, opt);
 except
  result := 0;
 end;
end;

procedure TCustomVstPlugIn.Process(Inputs, Outputs: PPSingle; SampleFrames:Integer);
begin
 if PVstEffect <> nil
  then PVstEffect.Process(PVstEffect, Inputs, Outputs, SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames:Integer);
begin
 if PVstEffect <> nil
  then PVstEffect.ProcessReplacing(PVstEffect, Inputs, Outputs, SampleFrames);
end;

procedure TCustomVstPlugIn.ProcessDoubleReplacing(Inputs, Outputs: ppDouble; SampleFrames: Integer);
begin
 if PVstEffect <> nil
  then PVstEffect.ProcessDoubleReplacing(PVstEffect, Inputs, Outputs, SampleFrames);
end;

procedure TCustomVstPlugIn.SetParameter(index:Integer; parameter:Single);
begin
 if PVstEffect <> nil
  then PVstEffect.setParameter(PVstEffect, Index, Parameter);
end;

function TCustomVstPlugIn.GetParameter(Index:Integer):Single;
begin
 if PVstEffect = nil
  then result := 0
  else result := PVstEffect.getParameter(PVstEffect, Index);
end;

procedure TCustomVstPlugIn.SetProgram(lValue:Integer);
begin
 if FActive then
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

procedure TCustomVstPlugIn.SetProgramName(NewName:string);
begin
 if FActive then
  VstDispatch(effSetProgramName, 0, 0, PChar(NewName));
end;

function TCustomVstPlugIn.GetProgramName:string;
var temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 255);
   if VstDispatch(effGetProgramName, 0, 0, temp)= 0 then
    result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TCustomVstPlugIn.GetParamLabel(index:Integer): string;
var temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 255);
   if VstDispatch(effGetParamLabel, index, 0, temp)= 0 then
    result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TCustomVstPlugIn.GetParamDisplay(index:Integer): string;
var
  temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 255);
   if VstDispatch(effGetParamDisplay, index, 0, temp) = 0
    then result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TCustomVstPlugIn.GetParamName(index:Integer): string;
var
  temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 255);
   if VstDispatch(effGetParamName, index, 0, temp) = 0
    then result := ShortString(temp);
   FreeMem(temp);
  end;
end;

procedure TCustomVstPlugIn.SetSampleRate(fSR: Double);
begin
 VstDispatch(effSetSampleRate, 0, 0, nil, fSR);
end;

procedure TCustomVstPlugIn.SetBlockSize(value: Integer);
begin
 VstDispatch(effSetBlockSize, 0, value);
end;

procedure TCustomVstPlugIn.MainsChanged(bOn: Boolean);
begin
 VstDispatch(effMainsChanged, 0, Integer(bOn));
end;

function TCustomVstPlugIn.GetVu: Single;
const
  Divisor : Double = 1 / 32767;
begin
 if FActive
  then result := VstDispatch(effGetVu) * Divisor else result := -1;
end;

function TCustomVstPlugIn.GetRect: TRect;
var
  theRect: ERect;
begin
 theRect := EditGetRect;
 result := Classes.Rect(theRect.left, theRect.Top, theRect.right, theRect.Bottom);
end;

function TCustomVstPlugIn.EditGetRect: ERect;
var
  temp: PPERect;
begin
 GetMem(temp, SizeOf(PPERect));
 if fActive then VstDispatch(effEditGetRect, 0, 0, temp);
 if Assigned(temp) then
  if Assigned(temp^)
   then result := temp^^;
 FreeMem(temp);
end;

function TCustomVstPlugIn.EditOpen(Handle: THandle): Integer;
var i: Integer;
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
var theRect: ERect;
begin
 if GUIForm = nil then
  begin
   GUIForm := TForm.Create(nil);
   with GUIForm do
    begin
     Caption := VendorString + ' - ' + ProductString;
     BorderStyle := bsToolWindow;
     Position := poDesktopCenter;
     OnClose := FormCloseHandler;
     OnActivate := EditActivateHandler;
     OnDeActivate := EditDeactivateHandler;
     if Caption=' - ' then Caption := GetEffectName;
    end;
   fGUIFormCreated := True;
   ShowEdit(GUIForm);
   if (effFlagsHasEditor in PVstEffect.EffectFlags)
    then theRect := EditGetRect
    else theRect := Rect(0, 200, 0, 80);
   GUIForm.ClientWidth := theRect.right - theRect.left;
   GUIForm.ClientHeight := theRect.Bottom - theRect.Top;
  end;
 GUIForm.Visible := True;
end;

procedure TCustomVstPlugIn.ShowEdit(Form: TForm);
var
  param   : string;
  i,wxw   : Integer;
  theRect : ERect;
begin
 if (effFlagsHasEditor in PVstEffect.EffectFlags) and (fGUIStyle = gsDefault) then
  begin
   if not FEditOpen then
   begin
    EditOpen(Form.Handle);
    EditIdle;
   end;
//  else raise Exception.Create('Editor is already open!');
  end
 else // Vst has no GUI
  case fGUIStyle of
   gsOld:
    begin
     GUIForm := Form;
     FEditOpen := True;
     with TLabel.Create(Form) do
      begin
       Name := 'LbL'; Parent := Form; Caption := '';
       Alignment := taCenter; Left := 10; Top := 64;
      end;
     {$IFDEF SB}
     with TFlatScrollBar.Create(Form) do
      begin
       Name := 'SBox'; ClientWidth := 560;
       BevelInner := bvNone; BevelOuter := bvNone;
       Color := clGray; Parent := Form; Align := alClient;
       Left := 0; Top := 0; Width := 560;
       VertScrollBar.Smooth := True; VertScrollBar.Tracking := True;
       HorzScrollBar.Smooth := True; HorzScrollBar.Tracking := True;
      end;
     {$ELSE}
     with TTrackBar.Create(Form) do
      begin
       Name := 'ParamBar'; Parent := Form;
       Anchors := [akLeft, akTop, akRight];
       Left := 5; Top := 33; Orientation := trHorizontal;
       Width := Form.Width - 4 * Left; Height := 32;
       Frequency := 1; Position := 0; {$IFNDEF FPC} SelEnd := 0; SelStart := 0; {$ENDIF}
       TabOrder := 3; Min := 0; Max := 100; OnChange := TrackChange;
       TickMarks := tmBottomRight; TickStyle := tsNone;//Auto;
      end;
     {$ENDIF}
     with TComboBox.Create(Form) do
      begin
       Name := 'ParamBox'; Parent := Form; param := '';
       for i := 0 to FnumParams - 1
        do begin param := GetParamName(i); Items.Add(param); end;
       Anchors := [akLeft, akTop, akRight]; Left := 4; Top := 5;
       Width := Form.Width - 4 * Left; Height := 21; ItemHeight := 13;
       TabOrder := 2; OnChange := ParamChange; Text := ''; itemindex := 0;
       Font.Color := clWindowText;
       OnChange(nil);
      end;
    end;
   gsDefault, gsList:
    begin
     theRect := Rect(0,0,Form.Width,4 + numParams * 16);
     GUIForm := Form; wxw := 0;
     GUIForm.Visible := False;
     GUIForm.ClientWidth := theRect.right - theRect.left;
     GUIForm.ClientHeight := theRect.Bottom - theRect.Top;
     with TLabel.Create(Form) do
      try
       Parent := Form; Alignment := taCenter;
       for i := 0 to FnumParams - 1 do
        if Canvas.TextWidth(GetParamName(i)+':_')>wxw
         then wxw := Canvas.TextWidth(GetParamName(i)+':_');
      finally
       Free;
      end;

     for i := 0 to numParams - 1 do
      begin
       with TLabel.Create(Form) do
        begin
         Name := 'LbL' + IntToStr(i); Parent := Form; Caption := GetParamName(i)+':';
         Height := 16; Alignment := taCenter; Left := 2; Top := 2+i*Height;
        end;
       with TLabel.Create(Form) do
        begin
         Name := 'LbV' + IntToStr(i); Parent := Form; Alignment := taCenter;
         Height := 16; Left := Form.Width-Left-72; AutoSize := False;
         Alignment := taCenter; Width := 65; Top := 2+i*Height;
        end;
       with TScrollBar.Create(Form) do
        begin
         Name := 'ParamBar' + IntToStr(i); Parent := Form;
         Anchors := [akLeft, akTop, akRight];
         Kind := sbHorizontal; LargeChange := 10;
         Height := 16; Top := 2+i*Height; Tag := i;
         Left := wxw+2; Width := Form.Width-Left-72;
         Min := 0; Max := 1000; TabOrder := 3+i;
         Position := Round(1000 * Parameters[i]);
         OnChange := ListParamChange;
         ListParamChange(GUIForm.FindComponent('ParamBar'+IntToStr(i)));
        end;
      end;
     GUIForm.Visible := True;
     GUIForm.Invalidate;
     FEditOpen := True;
    end;
  end;
 if assigned(FOnShowEdit) then FOnShowEdit(Self, GUIForm);
end;

procedure TCustomVstPlugIn.ListParamChange(Sender: TObject);
var
  lb  : TLabel;
  str : string;
  i   : Integer;
begin
 with (Sender as TScrollBar) do
  try
   Parameters[Tag] := Position * 0.001;
   lb := TLabel(GUIForm.FindComponent('LbV'+IntToStr(Tag)));
   if Assigned(lb) then
    begin
     if GetParamLabel(Tag)<>''
      then str := GetParamDisplay(Tag) + ' ' + GetParamLabel(Tag)
      else str := GetParamDisplay(Tag);
     if Length(str) < 9
      then lb.Caption := str
      else
       begin
        str := GetParamDisplay(Tag);
        if Pos('.', str)>0 then
         begin
          i := Length(str) - 1;
          while str[i] = '0' do
           begin
            Delete(str, i, 1);
            dec(i);
           end;
         end;
        if GetParamLabel(Tag)<>''
         then lb.Caption := str + ' ' + GetParamLabel(Tag)
         else lb.Caption := str;
        if Length(lb.Caption) > 9 then lb.Caption := str
       end;
    end;
  except
  end;
end;

procedure TCustomVstPlugIn.ParamChange(Sender: TObject);
var nr: Integer;
begin
 if GUIForm.FindComponent('ParamBar') <> nil then
 {$IFDEF SB}
  with (GUIForm.FindComponent('ParamBar') as TFlatScrollBar) do
  {$ELSE}
  with (GUIForm.FindComponent('ParamBar') as TTrackBar) do
  {$ENDIF}
   try
    nr := (GUIForm.FindComponent('ParamBox') as TComboBox).ItemIndex;
    if (nr >= 0) and (nr < numParams) then
     Position := round(GetParameter(nr) * 100);
   except
   end;
end;

{$IFDEF SB}
procedure TCustomVstPlugIn.ScrollChange(Sender: TObject; ScrollPos: Integer);
var nr: Integer;
begin
 nr := (Sender as TFlatScrollBar).tag;
 SetParameter(nr, (Sender as TFlatScrollBar).Position * 0.01);
 (GUIForm.FindComponent('LbL' + inttostr(nr)) as TLabel).Caption  := 
  'Value: ' + GetParamDisplay(nr) + GetParamLabel(nr);
end;
{$ELSE}
procedure TCustomVstPlugIn.TrackChange(Sender: TObject);
var nr: Integer;
begin
 with (GUIForm.FindComponent('ParamBar') as TTrackBar) do
  begin
   nr := (GUIForm.FindComponent('ParamBox') as TComboBox).ItemIndex;
   SetParameter(nr, Position * 0.01);
   (GUIForm.FindComponent('LbL') as TLabel).Caption  :=
    'Value: ' + GetParamDisplay(nr) + GetParamLabel(nr);
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
var
  i: Integer;
begin
 if not Assigned(PVstEffect) then Exit;
 if assigned(FOnCloseEdit) then FOnCloseEdit(Self);
 if (effFlagsHasEditor in PVstEffect.EffectFlags) and (FGUIStyle = gsDefault)
  then EditClose else
  if Assigned(GUIForm) then
   case fGUIStyle of
    gsOld:
     begin
      GUIForm.FindComponent('ParamBox').Free;
      GUIForm.FindComponent('ParamBar').Free;
      GUIForm.FindComponent('LbL').Free;
     end;
    gsDefault, gsList:
     begin
      i := 0;
      repeat
       if GUIForm.FindComponent('ParamBar' + IntToStr(i)) = nil then Break;
       GUIForm.FindComponent('ParamBar' + IntToStr(i)).Free;
       if GUIForm.FindComponent('LbL' + IntToStr(i)) = nil then Break;
       GUIForm.FindComponent('LbL' + IntToStr(i)).Free;
       if GUIForm.FindComponent('LbV' + IntToStr(i)) = nil then Break;
       GUIForm.FindComponent('LbV' + IntToStr(i)).Free;
       inc(i);
      until False;
     end;
   end;
 if (GUIForm <> nil) and fGUIFormCreated
  then FreeAndNil(GUIForm); //and (not FExternalForm) then
 FEditOpen := False;
end;

procedure TCustomVstPlugIn.FormCloseHandler(Sender: TObject; var Action: TCloseAction);
begin
 CloseEdit;
 if GUIForm <> nil
  then FreeAndNil(GUIForm);
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

function TCustomVstPlugIn.Identify: Integer;
begin
 result := VstDispatch(effIdentify);
end;

function TCustomVstPlugIn.GetChunk(pntr: Pointer; isPreset: Boolean = False): Integer;
begin
 result := VstDispatch(effGetChunk, Integer(isPreset), 0, pntr);
end;

function TCustomVstPlugIn.SetChunk(data: Pointer; ByteSize: Integer; isPreset: Boolean = False): Integer;
begin
 result := VstDispatch(effSetChunk, Integer(isPreset), ByteSize, data);
end;

function TCustomVstPlugIn.ProcessEvents(pntr: PVstEvents): Integer;
begin
 result := VstDispatch(effProcessEvents, 0, 0, pntr);
end;

function TCustomVstPlugIn.CanBeAutomated(index: Integer): Integer;
begin
 result := VstDispatch(effCanBeAutomated, index);
end;

function TCustomVstPlugIn.String2Parameter(ParameterName: string): Integer;
var
  temp: Integer;
begin
 temp := 0;
 VstDispatch(effString2Parameter, temp, 0, PChar(ParameterName));
 result := temp;
end;

function TCustomVstPlugIn.GetNumProgramCategories: Integer;
begin
 if FActive
  then result := VstDispatch(effGetNumProgramCategories)
  else result := -1;
end;

function TCustomVstPlugIn.GetnumPrograms: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numPrograms
  else result := FnumPrograms;
end;

function TCustomVstPlugIn.GetProgramNameIndexed(Category, Index: Integer; ProgramName: PChar): Integer;
begin
 if FActive
  then result := VstDispatch(effGetProgramNameIndexed, Index, Category, ProgramName)
  else result := -1;
end;

function TCustomVstPlugIn.CopyCurrentProgramTo(Destination: Integer): Boolean;
begin
 if FActive
  then result := Boolean(VstDispatch(effCopyProgram, Destination))
  else result := False;
end;

function TCustomVstPlugIn.ConnectInput(InputNr: Integer; State: Boolean): Integer;
begin
 if FActive
  then result := VstDispatch(effConnectInput, InputNr, Integer(State))
  else result := -1;
end;

function TCustomVstPlugIn.ConnectOutput(OutputNr: Integer; State: Boolean): Integer;
begin
 if FActive
  then result := VstDispatch(effConnectOutput, OutputNr, Integer(State))
  else result := -1;
end;

function TCustomVstPlugIn.GetInputProperties(InputNr: Integer): TVstPinProperties;
var
  temp: PVstPinProperties;
begin
 new(temp);
 if FActive
  then VstDispatch(effGetInputProperties, InputNr, 0, temp);
 result := temp^;
 Dispose(temp);
end;

function TCustomVstPlugIn.GetOutputProperties(OutputNr: Integer): TVstPinProperties;
var
  temp: PVstPinProperties;
begin
 new(temp);
 if FActive
  then VstDispatch(effGetOutputProperties, OutputNr, 0, temp);
 result := temp^;
 Dispose(temp);
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

function TCustomVstPlugIn.OfflineNotify(pntr: PVstAudioFile; numAudioFiles: Integer; start: Boolean): Integer;
begin
 result := VstDispatch(effOfflineNotify, Integer(start), numAudioFiles, pntr);
end;

function TCustomVstPlugIn.OfflinePrepare(pntr: PVstOfflineTask; count: Integer): Integer;
begin
 result := VstDispatch(effOfflinePrepare, 0, count, pntr);
end;

function TCustomVstPlugIn.OfflineRun(pntr: PVstOfflineTask; count: Integer): Integer;
begin
 result := VstDispatch(effOfflineRun, 0, count, pntr);
end;

function TCustomVstPlugIn.ProcessVarIo(varIo: PVstVariableIo): Integer;
begin
 result := VstDispatch(effProcessVarIo, 0, 0, varIo);
end;

function TCustomVstPlugIn.SetSpeakerArrangement(pluginInput: PVstSpeakerArrangement; pluginOutput: PVstSpeakerArrangement): Boolean;
begin
 result := Boolean(VstDispatch(effSetSpeakerArrangement, 0, Integer(pluginInput), pluginOutput));
end;

function TCustomVstPlugIn.SetBlockSizeAndSampleRate(blockSize :Integer; sampleRate:Single): Integer;
begin
 result := VstDispatch(effSetBlockSizeAndSampleRate, 0, blockSize, nil, sampleRate);
end;

function TCustomVstPlugIn.SetBypass(onOff: Boolean): Integer;
begin
 result := VstDispatch(effSetBypass, 0, Integer(onOff));
end;

function TCustomVstPlugIn.GetEffectName: string;
var
  temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 255);
   if VstDispatch(effGetEffectName, 0, 0, temp) <> 0
    then result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TCustomVstPlugIn.GetErrorText: string;
var
  temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 258);
   if VstDispatch(effGetErrorText, 0, 0, temp) = 0
    then result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TCustomVstPlugIn.GetFriendlyNameString(const StringLength: Integer): string;
var
  Variations : array [0..6] of string;
  i, j, v    : Integer;
begin
 Variations[0] := GetEffectName;
 Variations[1] := GetVendorString + ' - ' + GetProductString;
 Variations[2] := GetVendorString + ' - ' + GetProductString + ' - ' + GetEffectName;
 Variations[3] := GetProductString + ' - ' + GetEffectName;
 Variations[4] := GetVendorString + ' - ' + GetEffectName;
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
  temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 255);
   if VstDispatch(effGetVendorString, 0, 0, temp) <> 0
    then result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TCustomVstPlugIn.GetProductString: string;
var
  temp: PChar;
begin
 result := '';
 if FActive then
  begin
   GetMem(temp, 255);
   if VstDispatch(effGetProductString, 0, 0, temp) <> 0
    then result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TCustomVstPlugIn.GetVendorVersion: Integer;
begin
 if FActive
  then result := VstDispatch(effGetVendorVersion)
  else result := -1;
end;

function TCustomVstPlugIn.VendorSpecific(index, value: Integer; pntr: Pointer; opt :Single):Integer;
begin
 result := VstDispatch(effVendorSpecific, index, value, pntr, opt);
end;

function TCustomVstPlugIn.CanDo(pntr: PChar): Integer;
begin
 result := VstDispatch(effCanDo, 0, 0, pntr);
end;

function TCustomVstPlugIn.GetTailSize: Integer;
begin
 if FActive then result := VstDispatch(effGetTailSize) else result := -1;
end;

function TCustomVstPlugIn.Idle: Integer;
begin
 if FNeedIdle
  then result := VstDispatch(effIdle)
  else result := 0;
end;

function TCustomVstPlugIn.GetIcon: Integer;
begin
 if FActive
  then result := VstDispatch(effGetIcon)
  else result := -1;
end;

procedure TCustomVstPlugIn.SetViewPosition(x, y: Integer);
begin
 VstDispatch(effSetViewPosition, x, y);
end;

function TCustomVstPlugIn.GetParameterProperties(Parameter: Integer): TVstParameterPropertyRecord;
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
  then result := VstDispatch(effGetVstVersion) else result := -1;
end;

function TCustomVstPlugIn.EditKeyDown(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
begin
 // character in <index>, virtual in <value>, modifiers in <opt>, return True if used, else False
 Result := False;
 if FActive then
  Result := (VstDispatch(effEditKeyDown, Integer(Key), VirtualKeycode, nil, Modifier) = 1);
end;

function TCustomVstPlugIn.EditKeyUp(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
begin
 // character in <index>, virtual in <value>, modifiers in <opt>, return True if used, else False
 Result := False;
 if FActive
  then Result := (VstDispatch(effEditKeyUp, Integer(Key), VirtualKeycode, nil, Modifier) = 1);
end;

procedure TCustomVstPlugIn.SetEditKnobMode(Mode : TKnobMode);
begin
 if FActive then VstDispatch(effSetEditKnobMode,0,Integer(Mode));
end;

// midi plugins channel dependent programs
function TCustomVstPlugIn.GetMidiProgramName(MidiProgramNamePointer : PMidiProgramName): Integer;
begin
 // struct will be filled with information for 'thisProgramIndex'.
 // returns number of used programIndexes.
 // if 0 is returned, no MidiProgramNames supported.

 if FActive
  then Result := VstDispatch(effGetMidiProgramName, 0, 0, MidiProgramNamePointer, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.GetnumInputs: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numInputs
  else result := FnumInputs;
end;

function TCustomVstPlugIn.GetnumOutputs: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numOutputs
  else result := FnumOutputs;
end;

function TCustomVstPlugIn.GetnumParams: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numParams
  else result := FnumParams;
end;

function TCustomVstPlugIn.GetCurrentMidiProgram(MidiProgramNamePointer : PMidiProgramName): Integer;
begin
 // returns the programIndex of the current program.
 // passed <ptr> points to MidiProgramName struct.
 // struct will be filled with information for the current program.
 if FActive
  then Result := VstDispatch(effGetCurrentMidiProgram, 0, 0, MidiProgramNamePointer, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.GetMidiProgramCategory(MidiProgramCategoryPointer : PMidiProgramCategory): Integer;
begin
 // passed <ptr> points to MidiProgramCategory struct.
 // struct will be filled with information for 'thisCategoryIndex'.
 // returns number of used CategoryIndexes.
 // if 0 is returned, no MidiProgramCategories supported.
 if FActive
  then Result := VstDispatch(effGetMidiProgramCategory, 0, 0, MidiProgramCategoryPointer, 0)
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

function TCustomVstPlugIn.GetMidiKeyName(MidiKeyNamePointer: PMidiKeyName): Integer;
begin
 // struct will be filled with information for 'thisProgramIndex' and
 // 'thisKeyNumber'. If keyName is "" the standard name of the key
 // will be displayed. If 0 is returned, no MidiKeyNames are
 // defined for 'thisProgramIndex'.
 if FActive
  then Result := VstDispatch(effGetMidiKeyName, 0, 0, MidiKeyNamePointer, 0)
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

function TCustomVstPlugIn.GetSpeakerArrangement(SpeakerIn, SpeakerOut:PVstSpeakerArrangement): Integer;
begin
// VstSpeakerArrangement** pluginInput in <value>
// VstSpeakerArrangement** pluginOutput in <ptr>
 Result := 0;
 if FActive
  then VstDispatch(effGetSpeakerArrangement, 0, Integer(@SpeakerOut), @SpeakerOut);
end;

function TCustomVstPlugIn.ShellGetNextPlugin(var PluginName:String): Integer;
var
  temp: PChar;
begin
 Result := 0; 
 if FActive then
 begin
  GetMem(temp, 255);
  Result := VstDispatch(effShellGetNextPlugin, 0, 0, temp); // returns the next plugin's uniqueID.
  if Result <> 0 then PluginName := ShortString(temp);
  FreeMem(temp);
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

procedure TCustomVstPlugIn.SetPanLaw(PanLaw: TVstPanLawType; Gain: Single);
begin
 // PanLaw : Type (Linear, Equal Power,.. see enum PanLaw Type) in <value>,
 // Gain in <opt>: for Linear : [1.0 => 0dB PanLaw], [~0.58 => -4.5dB], [0.5 => -6.02dB]

 if FActive then VstDispatch(effSetPanLaw, 0, Integer(PanLaw), nil, Gain);
end;

function TCustomVstPlugIn.BeginLoadBank(PatchChunkInfo : PVstPatchChunkInfo): Integer;
begin
 // Called before a Bank is loaded, <ptr> points to VstPatchChunkInfo structure
 // return -1 if the Bank can not be loaded, return 1 if it can be loaded else 0 (for compatibility)
 if FActive then VstDispatch(effBeginLoadBank);
 Result := 0;
end;

procedure TCustomVstPlugIn.BeginLoadProgram(PatchChunkInfo : PVstPatchChunkInfo);
begin
 // Called before a Program is loaded, <ptr> points to VstPatchChunkInfo structure

 if FActive then VstDispatch(effBeginLoadProgram, 0, 0, PatchChunkInfo);
end;

function TCustomVstPlugIn.GetEntryPoints(theDll: TFileName): Integer;
{$IFNDEF FPC}
var
  Buf : Array[0..511] of char;
  LE  : Integer;
  str : string;
{$ENDIF}
begin
 result := 0;
 FDLLHandle := SafeLoadLibrary(PChar(theDll),7);
// FDLLHandle := LoadLibraryEx(PChar(theDll), 0, DONT_RESOLVE_DLL_REFERENCES);

 case FDLLHandle = 0 of
   True:
   try
    {$IFNDEF FPC}
    LE := GetLastError;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NIL, LE, 0, @Buf[0], 256, NIL);
    if Buf='' then
     begin
      str := IntToStr(LE);
      StrCopy(@Buf[0], @str[1]);
      raise Exception.Create(str);
     end else raise Exception.Create(buf);
    {$ENDIF}
   finally
    result := 1;
   end
   else
    begin
     if Lowercase(ExtractFileExt(theDll)) = '.vst3'
      then @FMainFunction := GetProcAddress(FDLLHandle, 'GetPluginFactory')
      else @FMainFunction := GetProcAddress(FDLLHandle, 'main');
     if not Assigned(FMainFunction) then @FMainFunction := GetProcAddress(FDLLHandle, 'VSTPluginMain');
     if not Assigned(FMainFunction) then
      begin
       @FMainFunction := @whatifnoentry;
       result := 1;
      end;
    end;
 end;
end;

procedure TCustomVstPlugIn.SetDLLFileName(VstFilename:TFilename);
begin
 FDLLFileName := VstFilename;
end;

function TCustomVstPlugIn.LoadFromFile(PluginDll: TFilename): Boolean;
begin
 result := False;
 try
   Unload;  // make sure nothing else is loaded
   FMainFunction := nil;
   if not FileExists(PluginDll)
    then raise Exception.CreateFmt(RStrFileDoesNotExist, [PluginDll]);
   if GetEntryPoints(PluginDll) <> 0 then
    begin
     Unload;
     exit;
    end;
   if assigned(FMainFunction)
    then PVstEffect := FMainFunction(@audioMaster)
    else PVstEffect := nil;
  if PVstEffect <> nil then
  begin
   if Assigned(FOnAfterLoad) then FOnAfterLoad(Self);
   result := True;
  end else raise Exception.CreateFmt(RStrPlugInCouldNotBeLoaded, [PluginDll]);
 except
  result := False;
  Unload;
  exit;
 end;
 try
  fActive := True;
  FDLLFileName := PluginDLL;
 except
 end;
end;

function TCustomVstPlugIn.LoadFromStream(Stream: TStream): Boolean;
begin
 if not assigned(FInternalDLLLoader)
  then FInternalDLLLoader := TDLLLoader.Create;
 try
  FMainFunction := nil;
  FInternalDLLLoader.Load(Stream);
  FMainFunction := FInternalDLLLoader.FindExport('main');
  if assigned(FMainFunction)
   then PVstEffect := FMainFunction(@audioMaster)
   else PVstEffect := nil;
  if PVstEffect <> nil then
   begin
    if Assigned(FOnAfterLoad) then FOnAfterLoad(Self);
    result := True;
   end else raise Exception.Create(RStrPlugInStreamError);
 except
  result := False;
  FreeAndNil(FInternalDLLLoader);
  raise;
 end;
end;

procedure TCustomVstPlugIn.UnLoad;
begin
 try
  CloseEdit;
  Close;
 except
 end;
 FnumInputs := -1;
 FnumOutputs := -1;
 FnumPrograms := -1;
 FVersion := -1;
 FUniqueID := '';
 if FDLLHandle > 0 then
 begin
  try
   FreeLibrary(FDLLHandle);
  except
  end;
  FDLLHandle := 0;
 end;
 PVstEffect := nil;
end;

function SwapLong(var l: LongInt): LongInt;
var
  t : Integer;
type
  X = array [0..1] of word;
begin
 T := Swap(X(L)[1]);
 X(L)[1] := Swap(X(L)[0]);
 X(L)[0] := T;
 result := t;
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

function TCustomVstPlugIn.GetPreset(i: Integer): TFXPreset;
var
  s  : string;
  pc : pLongInt;
  x  : Integer;
  si : Single;
begin
 SetProgram(i);
 with result do
  begin
   chunkMagic := FourCharToLong('C','c','n','K');
   fxMagic    := FourCharToLong('F','x','C','k');
   version    := 1;
   fxID       := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
   fxVersion  := PVstEffect^.version;
   numParams  := numParams;
   SwapLong(chunkMagic);
   SwapLong(fxMagic);
   SwapLong(version);
   SwapLong(fxID);
   SwapLong(fxVersion);
   SwapLong(numParams);
   s := GetProgramName + #0;
   StrLCopy(prgName, PChar(s), 26);
   GetMem(params, numParams * SizeOf(Single));
   pc := pLongInt(params);
   for i := 0 to numParams - 1 do
   begin
    si := GetParameter(i);
    x := pLongInt(@si)^;
    SwapLong(x);
    pc^ := x;
    inc(pc);
   end;
   result.ByteSize := SizeOf(result) - SizeOf(LongInt) * 2 + (numParams - 1) * SizeOf(Single);
   SwapLong(ByteSize);
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
  s          : string;
  x          : Integer;
  PBuffer    : Pointer;
  FXPreset   : TFXPreset;
begin
 Stream.Seek(0, 0);
 if not assigned(PVstEffect) then exit;
 if effFlagsProgramChunks in EffectOptions then
  with FXChunkSet do
   begin
    chunkMagic := FourCharToLong('C','c','n','K');
    fxMagic := FourCharToLong('F','P','C','h');
    version := 1;
    fxID := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
    fxVersion := PVstEffect^.version;
    numPrograms := numPrograms;
    SwapLong(chunkMagic);
    SwapLong(fxMagic);
    SwapLong(version);
    SwapLong(fxID);
    SwapLong(fxVersion);
    SwapLong(numPrograms);
    s := GetProgramName + #0;
    StrLCopy(prgName, PChar(s), 26);
    x := GetChunk(@PBuffer, True);
    chunkSize := x;
    ByteSize := SizeOf(FXChunkSet) - SizeOf(LongInt) * 2 + chunkSize - 8;
    SwapLong(ByteSize);
    SwapLong(chunkSize);
    Stream.WriteBuffer(FXChunkSet, SizeOf(FXChunkSet) - SizeOf(Pointer));
    Stream.WriteBuffer(PBuffer^, x);
   end
  else
   begin
    FXPreset := GetPreset(GetProgram);
    Stream.WriteBuffer(FXPreset, SizeOf(FXPreset) - SizeOf(Single));
    Stream.WriteBuffer(FXPreset.params^, SizeOf(Single) * numParams);
    FreeMem(FXPreset.params);
   end;
end;

procedure TCustomVstPlugIn.LoadBank(Stream: TStream);
var
  i              : Integer;
  FXSet          : TFXSet;
  FXChunkBank    : TFXChunkBank;
  pp             : TFXPreset;
  s              : Single;
  j, x           : Integer;
  ptr            : Pointer;
  pb2            : Pointer;
  PatchChunkInfo : TVstPatchChunkInfo;
  b              : Byte;
  UseChunk       : Boolean;
begin
 if not assigned(PVstEffect) then exit;
 Stream.Seek(9, 0);
 Stream.Read(b, 1);
 UseChunk := (b <> $78);
 Stream.Seek(0, 0);

// if eoProgramChunks in EffectOptions then
 if UseChunk then
  begin
   ptr := @FXChunkBank;
   Stream.Read(ptr^, SizeOf(TFXChunkBank) - SizeOf(Pointer));

   x := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
   SwapLong(x);
   if FXChunkBank.fxId <> x then raise Exception.Create(RStrBankFileNotForThisPlugin);

   x := Stream.Size - Stream.Position;
   GetMem(pb2, x + 1);
   j := Stream.Read(pb2^, x);
   SetChunk(pb2, j, False);
   FreeMem(pb2);
  end
 else
  begin
   ptr := @FXSet;
   Stream.Read(ptr^, SizeOf(TFXSet) - SizeOf(Pointer));
   x := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
   SwapLong(x);
   if FXSet.fxId <> x then raise Exception.Create(RStrBankFileNotForThisPlugin);

   PatchChunkInfo.version := 1;
   PatchChunkInfo.pluginUniqueID := PVstEffect.uniqueID;
   PatchChunkInfo.pluginVersion := PVstEffect.version;
   PatchChunkInfo.numElements := PVstEffect.numPrograms; // Number of Programs (Bank)
   BeginLoadBank(@PatchChunkInfo);

   SwapLong(FXSet.numPrograms);
   for j := 0 to FXSet.numPrograms - 1 do
    begin
     ptr := @pp;
     Stream.Read(ptr^, SizeOf(TFXPreset) - SizeOf(Pointer));
     SetProgram(j);
     SetProgramName(pp.prgName);
     SwapLong(pp.numParams);
     ptr := @x;
     for i := 0 to pp.numParams - 1 do
      begin
       Stream.Read(ptr^, SizeOf(Single));
       SwapLong(x);
       s := pSingle(ptr)^;
       SetParameter(i, s);
      end;
    end;
  end;
end;

procedure TCustomVstPlugIn.LoadPreset(Stream: TStream);
var
  i              : Integer;
  FXPreset       : TFXPreset;
  FXChunkset     : TFXChunkset;
  s              : Single;
  j, x           : Integer;
  ptr            : Pointer;
  pb2            : Pointer;
  PatchChunkInfo : TVstPatchChunkInfo;
  b              : Byte;
  UseChunk       : Boolean;
begin
 if not assigned(PVstEffect) then exit;
 Stream.Seek(9, 0);
 Stream.Read(b, 1);
 UseChunk := (b <> $78);
 Stream.Seek(0, 0);

// if eoProgramChunks in EffectOptions then
 if UseChunk then
  begin
   ptr := @FXChunkset;
   Stream.Read(ptr^, SizeOf(TFXChunkset) - SizeOf(Pointer));
   x := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
   SwapLong(x);
   if FXChunkset.fxId <> x
    then raise Exception.Create(RStrPresetFileNotForThisPlugin);
   SetProgramName(FXChunkset.prgName);

   x := Stream.Size - Stream.Position;
   GetMem(pb2, x + 1);
   j := Stream.Read(pb2^, x);
   SetChunk(pb2, j, True);
   FreeMem(pb2);
  end
 else
  begin
   ptr := @FXPreset;
   Stream.Read(ptr^, SizeOf(TFXPreset) - SizeOf(Pointer));
   x := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
   SwapLong(x);
   if FXPreset.fxId <> x
    then raise Exception.Create(RStrPresetFileNotForThisPlugin);
   PatchChunkInfo.version := 1;
   PatchChunkInfo.pluginUniqueID := PVstEffect.uniqueID;
   PatchChunkInfo.pluginVersion := PVstEffect.version;
   PatchChunkInfo.numElements := PVstEffect.numParams; // Number of Programs (Bank)
   BeginLoadProgram(@PatchChunkInfo);

   SetProgramName(FXPreset.prgName);
   SwapLong(FXPreset.numParams);
   ptr := @x;
   for i := 0 to FXPreset.numParams - 1 do
    begin
     Stream.Read(ptr^, SizeOf(Single));
     SwapLong(x);
     s := pSingle(ptr)^;
     SetParameter(i, s);
    end;
  end;
end;

procedure TCustomVstPlugIn.SaveBank(Stream: TStream);
var
  FXSet       : TFXSet;
  FXChunkBank : TFXChunkBank;
  j, x        : Integer;
  PBuffer     : Pointer;
  FXPreset    : TFXPreset;
begin
 if not assigned(PVstEffect) then exit;
 Stream.Seek(0, 0);
 if effFlagsProgramChunks in EffectOptions then
  with FXChunkBank do
   begin
    chunkMagic := FourCharToLong('C','c','n','K');
    fxMagic := FourCharToLong('F','B','C','h');
    version := 1;
    fxID := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
    fxVersion := PVstEffect^.version;
    numPrograms := numPrograms;
    SwapLong(chunkMagic);
    SwapLong(fxMagic);
    SwapLong(version);
    SwapLong(fxID);
    SwapLong(fxVersion);
    SwapLong(numPrograms);
    x := GetChunk(@PBuffer, False);
    chunkSize := x;
    ByteSize := SizeOf(FXChunkBank) - SizeOf(LongInt) * 3 + chunkSize + 8;
    SwapLong(ByteSize);
    SwapLong(chunkSize);
    Stream.WriteBuffer(FXChunkBank, SizeOf(FXChunkBank) - SizeOf(Pointer));
    Stream.WriteBuffer(PBuffer^, x);
   end
 else
  with FXSet do
   begin
    chunkMagic := FourCharToLong('C','c','n','K');
    fxMagic := FourCharToLong('F','x','B','k');
    version := 1;
    fxID := FourCharToLong(UniqueID[1], UniqueID[2], UniqueID[3], UniqueID[4]);
    fxVersion := PVstEffect^.version;
    numPrograms := numPrograms;
    SwapLong(chunkMagic);
    SwapLong(fxMagic);
    SwapLong(version);
    SwapLong(fxID);
    SwapLong(fxVersion);
    SwapLong(numPrograms);

    ByteSize := SizeOf(FXSet) - SizeOf(LongInt) +
     (SizeOf(TFXPreset) + (numParams - 1) * SizeOf(Single)) * numPrograms;
    SwapLong(ByteSize);
    Stream.WriteBuffer(FXSet, SizeOf(FXSet) - SizeOf(Single));
    for j := 0 to numPrograms - 1 do
     begin
      FXPreset := GetPreset(j);
      Stream.WriteBuffer(FXPreset, SizeOf(FXPreset) - SizeOf(Single));
      Stream.WriteBuffer(FXPreset.params^, SizeOf(Single) * numParams);
      FreeMem(FXPreset.params);
     end;
   end;
end;


procedure TCustomVstPlugIn.ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: Integer);
begin
 if PVstEffect <> nil then
  with PVstEffect^ do
   if effFlagsCanReplacing in EffectFlags
    then processreplacing(PVstEffect, Inputs, Outputs, SampleFrames)
    else process(PVstEffect, Inputs, Outputs, SampleFrames);
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
 if assigned(PVstEffect)
  then result := PVstEffect.initialDelay
  else result := 0;
end;

procedure TCustomVstPlugIn.SetGUIStyle(const Value: TGUIStyle);
begin
 if FEditOpen
  then raise Exception.Create(RStrCloseEditorFirst)
  else fGUIStyle := Value;
end;
{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TVstHost]);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 {$IFDEF FPC}
 {$I TVstHost.lrs}
 {$ENDIF}
 audioMaster := AudioMasterCallback;
 HostWindows := TObjectList.Create;

finalization
 HostWindows.Free;

end.
