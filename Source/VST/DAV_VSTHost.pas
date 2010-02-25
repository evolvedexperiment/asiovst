unit DAV_VSTHost;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde, based on a code snipped by Frederic Vanmol            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
//  Contributor(s):                                                           //
//    Maik Menz (various refacturings)                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  This is a component to load a VST plugin and make its properties and      //
//  methods accessible, ie. show the interface, fill the buffers and call     //
//  the processing routines.                                                  //
//                                                                            //
//  All host and plugin loading code was written by:                          //
//    Tobias Fleischer (http://www.tobybear.de)                               //
//    Christan W. Budde (http://www.savioursofsoul.de/Christian)              //
//                                                                            //
//  Delphi VST SDK tranlsation by:                                            //
//    Frederic Vanmol (http://www.axiworld.be)                                //
//                                                                            //
//  VST is a trademark of:                                                    //
//    Steinberg Media GmbH (http://www.steinberg.net)                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$IFNDEF FPC}{$DEFINE MemDLL}{$ENDIF}

uses
  {$IFDEF FPC} LCLIntf, LResources, Dynlibs, {$ELSE} Windows, Messages, {$ENDIF}
  {$IFDEF MSWINDOWS} Registry, {$ENDIF} Contnrs, SysUtils, Classes, Graphics,
  {$IFDEF VstHostGUI} Controls, Forms, StdCtrls, ComCtrls, Dialogs,{$ENDIF}
  DAV_Types, DAV_VSTEffect, DAV_VSTOfflineTask {$IFDEF MemDLL},
  DAV_DLLLoader{$ENDIF};

const
  CDefaultBlockSize = 2048;
  CDefaultSampleRate = 44100;

type
  {$IFDEF DELPHI10_UP} {$region 'General Types'} {$ENDIF}
  TVendorSpecificEvent = function(Index, Value: LongInt; Ptr: Pointer; Opt: Single): Integer of object;
  TVstAutomateEvent = procedure(Sender: TObject; Index: Integer; ParameterValue: Single) of object;
  TVstProcessEventsEvent = procedure(Sender: TObject; VstEvents: PVstEvents) of object;
  TVstAutomationNotifyEvent = procedure(Sender: TObject; ParameterIndex: Integer) of object;
  TVstSampleRateChangedEvent = procedure(Sender: TObject; SampleRate: Single) of object;
  TVstPinConnectedEvent = function(Sender: TObject; PinNr: Integer; isInput: Boolean): Boolean of object;
  TVstOfflineEvent = procedure(Sender: TObject; VstOfflineTaskPointer: PVstOfflineTaskRecord) of object;
  TVSTGetTempoAtSamplePositionEvent = function(Sender: TObject; SamplePosition: Integer): Double of object;
  {$IFDEF VstHostGUI}
  TVstShowEditEvent = procedure(Sender: TObject; Control: TWinControl) of object;
  TGUIStyle = (gsDefault, gsParameterList, gsParameterSelector,
    gsCustom);
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
    function GetPreset(const ProgramNo: Integer): TFXPreset;
    function GetRealQualities: LongInt;
    function GetUniqueID: string;
    function GetVersion: Integer;
    function GetVSTCanDos: TVstCanDos;
    procedure InitializeVstEffect;
    procedure SetActive(const Value: Boolean);
    procedure SetVstDllFileName(const Value: TFilename);
    {$IFDEF VstHostGUI}
    function CalculateGuiRectList: ERect;
    function CalculateGuiRectSelector: ERect;
    procedure FormCloseHandler(Sender: TObject; var Action: TCloseAction);
    procedure ControlChangeList(Sender: TObject);
    procedure ControlChangeSelector(Sender: TObject);
    procedure GuiParameterChangeList(Sender: TObject);
    procedure GuiParameterChangeSelector(Sender: TObject);
    procedure EditActivateHandler(Sender: TObject);
    procedure EditDeactivateHandler(Sender: TObject);
    procedure SetGUIStyle(const Value: TGUIStyle);
    procedure UpdateParameterOnGui(ParameterIndex: Integer);
    {$ENDIF}
  protected
    {$IFDEF VstHostGUI}
    FGUIControl  : TWinControl;
    FGUIElements : TObjectList;
    procedure EditClose;
    procedure ShowHostGuiSelector(Control: TWinControl);
    procedure ShowHostGuiList(Control: TWinControl);
    procedure ShowHostGuiCustom(Control: TWinControl);
    {$ENDIF}
    procedure AssignTo(Dest: TPersistent); override;

    procedure AudioMasterAutomate(Index: Integer; Value: Single); virtual;
    procedure AudioMasterIdle; virtual;
    function AudioMasterCurrentId: Integer;
    function AudioMasterPinConnected(Index: Integer; PinConnected: Boolean): Boolean; virtual;
    procedure AudioMasterWantMidi; virtual;
    procedure AudioMasterProcessEvents(VstEvents: PVstEvents); virtual;
    procedure AudioMasterIOChanged; virtual;
    procedure AudioMasterNeedIdle; virtual;
    function AudioMasterSizeWindow(Width, Height: Integer): Boolean; virtual;
    function AudioMasterVendorSpecific(Index, Value: Integer; ptr: Pointer; opt: Single): Integer; virtual;
    procedure AudioMasterUpdateDisplay; virtual;
    procedure AudioMasterBeginEdit(ParameterIndex: Integer); virtual;
    procedure AudioMasterEndEdit(ParameterIndex: Integer); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function BeginLoadBank(const PatchChunkInfo : PVstPatchChunkInfo): Integer;
    function BeginLoadProgram(const PatchChunkInfo : PVstPatchChunkInfo): Integer;
    function CanBeAutomated(const Index: Integer): Integer;
    function ConnectInput(const InputNr: Integer; const State: Boolean): Integer;
    function ConnectOutput(const OutputNr: Integer; const State: Boolean): Integer;
    function CopyCurrentProgramTo(const Destination: Integer): Boolean;
    function GetChunk(const Data: Pointer; const IsPreset: Boolean = False): Integer;
    function GetCurrentMidiProgram(var MidiProgramName: TMidiProgramName; const Channel: Integer = 0): Integer;
    function GetCurrentPosition: Integer;
    function GetDestinationBuffer: Integer;
    function GetDisplayName: string; override;
    function GetEffectName: string;
    function GetErrorText: string;
    function GetFriendlyNameString(const StringLength: Integer): string;
    function GetIcon: Integer;
    function GetInputProperties(const InputNr: Integer): TVstPinProperties;
    function GetMidiKeyName(var MidiKeyName: TMidiKeyName; const Channel: Integer = 0): Integer;
    function GetMidiProgramCategory(var MidiProgramCategory: TMidiProgramCategory; const Channel: Integer = 0): Integer;
    function GetMidiProgramName(var MidiProgramName: TMidiProgramName): Integer;
    function GetNumProgramCategories: Integer;
    function GetOutputProperties(const OutputNr: Integer): TVstPinProperties;
    function GetParamDisplay(index: Integer): string;
    function GetParameter(index: Integer): Single; virtual;
    function GetParameterProperties(const Index: Integer;
      var ParameterProperties: TVstParameterPropertyRecord): Boolean;
    function GetParamLabel(Index: Integer): string;
    function GetParamName(Index: Integer): string;
    function GetPlugCategory: TVstPluginCategory;
    function GetProductString: string;
    function GetCurrentProgram: Integer;
    function GetProgramName: string;
    function GetProgramNameIndexed(const Category, Index: Integer; var ProgramName: string): Integer;
    function GetRect: TRect;
    function GetSpeakerArrangement(const SpeakerIn, SpeakerOut: PVstSpeakerArrangement): Integer;
    function GetTailSize: Integer;
    function GetVendorString: string;
    function GetVendorVersion: Integer;
    function GetVstVersion: Integer;
    function GetVu: Single;
    function HasMidiProgramsChanged(const Channel: Integer = 0): Integer;
    function Identify: Integer;
    function Idle: Integer;
    function KeysRequired: Integer;

    // Offline Functions
    function OfflineNotify(const VstAudioFile: TVstAudioFile; const NumAudioFiles: Integer; const Start: Boolean): Integer;
    function OfflinePrepare(const VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count: Integer): Integer;
    function OfflineRun(const VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count :Integer): Integer;

    function ProcessEvents(const VstEvents: TVstEvents): Integer;
    function ProcessVarIo(const VarIo: TVstVariableIo): Integer;
    function SetBlockSizeAndSampleRate(const BlockSize: Integer; const SampleRate: Single): Integer;
    function SetBypass(const Value: Boolean): Integer;
    function SetChunk(const Data: Pointer; const ByteSize: Integer; const IsPreset: Boolean = False): Integer;
    function SetInputSpeakerArrangement(const PluginInput: TVstSpeakerArrangement): Boolean;
    function SetOutputSpeakerArrangement(const PluginOutput: TVstSpeakerArrangement): Boolean;
    function SetSpeakerArrangement(const PluginInput, PluginOutput: TVstSpeakerArrangement): Boolean;
    function ShellGetNextPlugin(var PluginName: string): Integer;
    function String2Parameter(const Index: Integer; ValueString: string): Boolean;
    function VendorSpecific(const Index, Value: Integer; const Pntr: Pointer; const Opt: Single): Integer;
    function VstCanDo(const CanDoString: string): Integer;
    function VstDispatch(const opCode : TDispatcherOpcode; const Index: Integer = 0; const value: Integer = 0; const pntr: Pointer = nil; const opt: Single = 0): Integer; {overload;} //virtual;

    // load plugin
    function CheckValidPlugin(const FileName: TFilename): Boolean;
    procedure LoadFromFile(const FileName: TFilename);
    {$IFDEF MemDLL}
    procedure LoadFromStream(const Stream: TStream);
    {$ENDIF}
    procedure LoadFromVSTEffect(const Value: PVSTEffect);
    procedure UnLoad;

    // open / close
    procedure Open;
    procedure Close;

    procedure BeginSetProgram;
    procedure EndSetProgram;
    procedure LoadBank(FileName: TFileName); overload;
    procedure LoadBank(Stream: TStream); overload;
    procedure LoadPreset(FileName: TFileName); overload;
    procedure LoadPreset(Stream: TStream); overload;
    procedure MainsChanged(const IsOn: Boolean);
    procedure Process(Inputs, Outputs: PPSingle; SampleFrames: Integer); virtual;
    procedure ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: Integer);
    procedure ProcessDoubleReplacing(Inputs, Outputs: ppDouble; SampleFrames: Integer); virtual;
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: Integer); virtual;
    procedure SaveBank(FileName: TFileName); overload;
    procedure SaveBank(Stream: TStream); overload;
    procedure SavePreset(FileName: TFileName); overload;
    procedure SavePreset(Stream: TStream); overload;
    procedure SetBlockSize(const Value: Integer);
    procedure SetPanLaw(const PanLaw: TVstPanLawType; const Gain: Single);
    procedure SetParameter(index: Integer; parameter: Single); virtual;
    procedure SetCurrentProgram(const lValue: Integer);
    procedure SetProgramName(const newName: string);
    procedure SetSampleRate(const Value: Single);
    procedure SetTotalSampleToProcess;
    procedure SetViewPosition(const x, y: Integer);
    procedure StartProcess;
    procedure StopProcess;

    {$IFDEF VstHostGUI}
    procedure CloseEdit;
    procedure EditActivate;
    procedure EditDeactivate;
    function EditGetRect: ERect;
    function EditIdle: Integer;
    function EditKeyDown(const Key: Char; const VirtualKeycode: Integer; const Modifier: TVstModifierKeys): Boolean;
    function EditKeyUp(const Key: Char; const VirtualKeycode: Integer; const Modifier: TVstModifierKeys): Boolean;
    function EditOpen(const Handle: THandle): Integer;
    procedure RenderEditorToBitmap(Bitmap: TBitmap);
    procedure SetEditKnobMode(Mode : TKnobMode);
    procedure ShowEdit(Control: TWinControl); overload;
    procedure ShowEdit; overload;
    {$ENDIF}

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
    property GUIStyle : TGUIStyle read FGUIStyle write SetGUIStyle default gsDefault;
    {$ENDIF}
    property Loaded: Boolean read FLoaded stored False;
    property PlugCategory: TVstPluginCategory read FPlugCategory stored False;
    property PluginVstVersion: Integer read FVstVersion stored False default -1;
    property ProductString: string read GetProductString stored False;
    property ProgramName: string read GetProgramName write SetProgramName;
    property CurrentProgram: Integer read GetCurrentProgram write SetCurrentProgram default -1;
    property ReplaceOrAccumulate: TReplaceOrAccumulate read FReplaceOrAccumulate write FReplaceOrAccumulate default roa0NotSupported;
    property VendorString: string read GetVendorString stored False;
    property VendorVersion: Integer read GetVendorVersion stored False default -1;
    property VSTCanDos: TVstCanDos read GetVSTCanDos stored False;
    property VstEffectPointer: PVstEffect read FVstEffect;

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
    property CurrentProgram;
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
    FNumAutomatable     : Integer;
    FOutputLatency      : Integer;
    FParamQuan          : Integer;
    FPlugInDir          : string;
    FProductString      : string;
    FVendorString       : string;
    FVendorVersion      : Integer;
    FHostVersion        : Integer;
    FHostCanDos         : THostCanDos;
    FVTI                : TVstTimeInformation;
    FBlockSize          : Integer;
    FOnCreate           : TNotifyEvent;
    FOnDestroy          : TNotifyEvent;
    FOnGetTempoAtSamplePosition: TVSTGetTempoAtSamplePositionEvent;
    function GetHostTempo: Single;
    function GetItem(Index: Integer): TCustomVstPlugIn;
    function GetPluginCount: Integer;
    procedure SetBlockSize(const Value: Integer);
    procedure SetHostCanDos(const Value: THostCanDos);
    procedure SetHostTempo(const Value: Single);
    procedure SetHostVersion(const Value: Integer);
    procedure SetVstPlugIns(const Value: TVstPlugIns);
    procedure VstTimeInfoChanged(Sender: TObject);
  protected
    FVstPlugIns : TVstPlugIns;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BlockSizeChanged; virtual;
    procedure CreateVstPluginList; virtual;

    function AudioMasterGetTime: PVstTimeInfo; virtual;
    procedure AudioMasterSetTime(VstTimeInfo: PVstTimeInfo; Filter: TVstTimeInfoFlags); virtual;
    function AudioMasterTempoAt(const SamplePosition: Integer): Integer; virtual;
    function AudioMasterGetParameterQuantization(Index: Integer): Integer; virtual;
    function AudioMasterGetNumAutomatableParameters: Integer; virtual;
    function AudioMasterGetSampleRate: Single; virtual;
    function AudioMasterGetPreviousPlug(InputPin: Integer): PVstEffect;
    function AudioMasterGetNextPlug(OutputPin: Integer): PVstEffect;
    function AudioMasterCanDo(CanDo: PAnsiChar): Boolean;

    property Items[Index: Integer]: TCustomVstPlugIn read GetItem; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateVstTimeInfo(const Samples: Word = 1);
    procedure ResetVstTimeInformation;

    property BlockSize: Integer read FBlockSize write SetBlocksize default CDefaultBlockSize;
    property CanDos: THostCanDos read FHostCanDos write SetHostCanDos;
    property Count: Integer read GetPluginCount;
    property CheckStringLengths: Boolean read FCheckStringLengths write FCheckStringLengths default false;
    property Language: TVstHostLanguage read FLanguage write FLanguage default hlEnglish;
    property LatencyInput: Integer read FInputLatency write FInputLatency default 0;
    property LatencyOutput: Integer read FOutputLatency write FOutputLatency default 0;
    property ManageIdleAutomaticly : Boolean read FAutoIdle write FAutoIdle;
    property NumAutomatableParameters : Integer read FNumAutomatable write FNumAutomatable default 0;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnGetTempoAtSamplePosition: TVSTGetTempoAtSamplePositionEvent read FOnGetTempoAtSamplePosition write FOnGetTempoAtSamplePosition;
    property ParameterQuantization : Integer read FParamQuan write FParamQuan default MAXINT;
    property PlugInDir: string read FPlugInDir write FPlugInDir;
    property ProductString: string read FProductString write FProductString;
    property Tempo: Single read GetHostTempo write SetHostTempo;
    property VendorString: string read FVendorString write FVendorString;
    property VendorVersion: Integer read FVendorVersion write FVendorVersion;
    property VstPlugIns: TVstPlugIns read FVstPlugIns write SetVstPlugIns;
    property VstTimeInfo: TVstTimeInformation read FVTI write FVTI;
    property VstVersion: Integer read FHostVersion write SetHostVersion;
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

function String2Language(LanguageString: string): TVstHostLanguage;
function PlugCategory2String(Category: TVstPluginCategory):string;
function EffOptions2String(EffOpts: TEffFlags):string;
function CheckValidVstPlugin(const FileName: TFilename): Boolean;

implementation

uses
  DAV_Common;

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
  RCStrInvalidSMTPEFrameRate     = 'Invalid SMTPE FrameRate';
{$IFDEF DELPHI10_UP} {$endregion 'Resource Strings'} {$ENDIF}

var
  GTempPlugin  : TCustomVstPlugIn;
  {$IFDEF SearchPluginAndHost}
  GHostList    : TObjectList;
  {$ENDIF}
  {$IFDEF VstHostGUI}
  GHostDialog  : TCommonDialog;
  GHostWindows : TObjectList;
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
 Result := 1;
end;

function String2Language(LanguageString : string): TVSTHostLanguage;
begin
 if      LanguageString = 'English'  then Result := hlEnglish
 else if LanguageString = 'French'   then Result := hlGerman
 else if LanguageString = 'German'   then Result := hlFrench
 else if LanguageString = 'Italian'  then Result := hlItalian
 else if LanguageString = 'Japanese' then Result := hlSpanish
 else if LanguageString = 'Spanish'  then Result := hlJapanese
 else Result := hlEnglish
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

function TimeInfoFlags2Integer(VstTimeInfoFlags: TVstTimeInfoFlags): Integer;
begin
 if vtiTransportChanged in VstTimeInfoFlags then Result := 1 else Result := 0;
 if vtiTransportPlaying in VstTimeInfoFlags then Result := Result + 1 shl 1;
 if vtiTransportCycleActive in VstTimeInfoFlags then Result := Result + 1 shl 2;
 if vtiTransportRecording in VstTimeInfoFlags then Result := Result + 1 shl 3;

 if vtiAutomationWriting in VstTimeInfoFlags then Result := Result + 1 shl 6;
 if vtiAutomationReading in VstTimeInfoFlags then Result := Result + 1 shl 7;

 if vtiNanosValid in VstTimeInfoFlags then Result := Result + 1 shl 8;
 if vtiPpqPosValid in VstTimeInfoFlags then Result := Result + 1 shl 9;
 if vtiTempoValid in VstTimeInfoFlags then Result := Result + 1 shl 10;
 if vtiBarsValid in VstTimeInfoFlags then Result := Result + 1 shl 11;
 if vtiCyclePosValid in VstTimeInfoFlags then Result := Result + 1 shl 12;
 if vtiTimeSigValid in VstTimeInfoFlags then Result := Result + 1 shl 13;
 if vtiSmpteValid in VstTimeInfoFlags then Result := Result + 1 shl 14;
 if vtiClockValid in VstTimeInfoFlags then Result := Result + 1 shl 15;
end;

function Integer2TimeInfoFlags(Value: Integer): TVstTimeInfoFlags;
begin
 if (Value and 1) <> 0
  then Result := [vtiTransportChanged]
  else Result := [];

 if (Value and (1 shl  1)) <> 0 then Result := Result + [vtiTransportPlaying];
 if (Value and (1 shl  2)) <> 0 then Result := Result + [vtiTransportCycleActive];
 if (Value and (1 shl  3)) <> 0 then Result := Result + [vtiTransportRecording];

 if (Value and (1 shl  6)) <> 0 then Result := Result + [vtiAutomationWriting];
 if (Value and (1 shl  7)) <> 0 then Result := Result + [vtiAutomationReading];

 if (Value and (1 shl  8)) <> 0 then Result := Result + [vtiNanosValid];
 if (Value and (1 shl  9)) <> 0 then Result := Result + [vtiPpqPosValid];
 if (Value and (1 shl 10)) <> 0 then Result := Result + [vtiTempoValid];
 if (Value and (1 shl 11)) <> 0 then Result := Result + [vtiBarsValid];
 if (Value and (1 shl 12)) <> 0 then Result := Result + [vtiCyclePosValid];
 if (Value and (1 shl 13)) <> 0 then Result := Result + [vtiTimeSigValid];
 if (Value and (1 shl 14)) <> 0 then Result := Result + [vtiSmpteValid];
 if (Value and (1 shl 15)) <> 0 then Result := Result + [vtiClockValid];
end;

procedure DontRaiseExceptionsAndSetFPUcodeword;
{$IFDEF PUREPASCAL}
begin

end;
{$ELSE}
asm
 fnclex                  // Don't raise pending exceptions enabled by the new flags
 fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
end;
{$ENDIF}

function CheckValidVstPlugin(const FileName: TFilename): Boolean;
var
  VstDllHandle : THandle;
begin
 Result := False;
 if not FileExists(FileName) then exit; 
 DontRaiseExceptionsAndSetFPUcodeword;
 VstDllHandle := SafeLoadLibrary(PAnsiChar(FileName), 7);
 if VstDllHandle <> 0 then
  try
   Result := GetProcAddress(VstDllHandle, 'main') <> nil;
   if Result = False
    then Result := GetProcAddress(VstDllHandle, 'VSTPluginMain') <> nil;
  finally
   FreeLibrary(VstDllHandle);
  end;
end;

function AudioMasterCallback(const Effect: PVstEffect; const Opcode : TAudioMasterOpcode; const Index, Value: LongInt; const Ptr: Pointer; const Opt: Single): LongInt; cdecl;
var
  Plugin    : TCustomVstPlugIn;
  Host      : TCustomVstHost;
  i         : Integer;
  {$IFDEF SearchPluginAndHost}
  PlugIndex : Integer;
  {$ENDIF}
begin
 Result := 0;
 {$IFDEF VstHostExceptionHandling}
 try
 {$ENDIF}
  if Assigned(Effect) then
   begin
    Plugin := Effect.ReservedForHost;
    {$IFDEF DELPHI6_UP}
    Host := Effect.Resvd2;
    {$ELSE}
    Host := TCustomVstHost(Plugin.Collection.Owner);
    {$ENDIF}
    if (Host = nil) then
     begin
      Plugin := GTempPlugin;
      Host := TCustomVstHost(Plugin.Collection.Owner);
     end;
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
      Exit;
     end;

    Plugin := nil;
    Host := nil;

    {$IFDEF SearchPluginAndHost}
    // find plugin in host list
    for i := 0 to GHostList.Count - 1 do
     with TCustomVstHost(GHostList[i]) do
      begin
       Assert(Assigned(VstPlugIns));
       for PlugIndex := 0 to VstPlugIns.Count - 1 do
        if VstPlugIns[PlugIndex].FVstEffect = Effect then
         begin
          Plugin := VstPlugIns[PlugIndex];
          Host := TCustomVstHost(GHostList[i]);
          Break;
         end;
       if Assigned(Plugin) then break;
      end;
   {$ENDIF}
   end;

  DontRaiseExceptionsAndSetFPUcodeword;

  case TAudiomasterOpcode(opcode) of
   audioMasterAutomate                    : begin
                                             if Assigned(Plugin)
                                              then Plugin.AudioMasterAutomate(Index, Opt);
                                             Result := 0;
                                            end;
   audioMasterVersion                     : if Assigned(Host)
                                             then Result := Host.VSTVersion
                                             else Result := 2300;
   audioMasterIdle                        : if Assigned(Plugin)
                                             then Plugin.AudioMasterIdle;
   audioMasterCurrentId                   : if Assigned(Plugin)
                                             then Result := Plugin.AudioMasterCurrentId
                                             else Result := 0;
   audioMasterPinConnected                : if Assigned(Plugin)
                                             then Result := Integer(Plugin.AudioMasterPinConnected(Index, Value = 0))
                                             else Result := 0;
   audioMasterWantMidi                    : if Assigned(Plugin)
                                             then Plugin.AudioMasterWantMidi;
   audioMasterGetTime                     : if Assigned(Host)
                                             then Result := LongInt(AudioMasterGetTime)
                                             else Result := 0;
   audioMasterProcessEvents               : if Assigned(Plugin)
                                             then Plugin.AudioMasterProcessEvents(ptr);
   audioMasterSetTime                     : if Assigned(Host)
                                             then Host.AudioMasterSetTime(ptr, Integer2TimeInfoFlags(Value));
   audioMasterTempoAt                     : if Assigned(Host)
                                             then Result := Host.AudioMasterTempoAt(Value)
                                             else Result := 1200000;
   audioMasterGetNumAutomatableParameters : if Assigned(Host)
                                             then Result := Host.AudioMasterGetNumAutomatableParameters
                                             else Result := 0;
   audioMasterGetParameterQuantization    : if Assigned(Host)
                                             then Result := Host.AudioMasterGetParameterQuantization(Value)
                                             else Result := 0;
   audioMasterIOChanged                   : if Assigned(Plugin)
                                             then Plugin.AudioMasterIOChanged;
   audioMasterNeedIdle                    : if Assigned(Plugin)
                                             then Plugin.AudioMasterNeedIdle;
   audioMasterSizeWindow                  : if Assigned(Plugin)
                                              then Result := Integer(Plugin.AudioMasterSizeWindow(Index, Value));
   audioMasterGetSampleRate               : if Assigned(Host)
                                             then Result := Round(Host.AudioMasterGetSampleRate)
                                             else Result := CDefaultSampleRate;
   audioMasterGetBlockSize                : if Assigned(Host)
                                             then Result := Host.BlockSize
                                             else Result := CDefaultBlockSize;
   audioMasterGetInputLatency             : if Assigned(Host)
                                             then Result := Host.FInputLatency
                                             else Result := 0;
   audioMasterGetOutputLatency            : if Assigned(Host)
                                             then Result := Host.FOutputLatency
                                             else Result := 0;
   audioMasterGetPreviousPlug             : if Assigned(Host)
                                             then Result := Integer(Host.AudioMasterGetPreviousPlug(Value))
                                             else Result := 0;
   audioMasterGetNextPlug                 : if Assigned(Host)
                                             then Result := Integer(Host.AudioMasterGetNextPlug(Value))
                                             else Result := 0;
   audioMasterWillReplaceOrAccumulate     : if Assigned(Plugin)
                                             then Result := Integer(Plugin.FReplaceOrAccumulate)
                                             else Result := 0;
   audioMasterGetCurrentProcessLevel      : if Assigned(Plugin)
                                             then Result := Integer(Plugin.FProcessLevel)
                                             else Result := 0;
   audioMasterGetAutomationState          : if Assigned(Plugin)
                                             then Result := Integer(Plugin.FAutomationState)
                                             else Result := 0;
   audioMasterOfflineStart                : if Assigned(Plugin) then
                                             if Assigned(Plugin.FOnAMOfflineStart)
                                              then Plugin.FOnAMOfflineStart(Plugin); // audioMasterOfflineStart
   audioMasterOfflineRead                 : if Assigned(Plugin) then
                                             if Assigned(Plugin.FOnAMOfflineRead)
                                              then Plugin.FOnAMOfflineRead(Plugin,ptr); // audioMasterOfflineRead, ptr points to offline structure, see below. return 0: error, 1 ok
   audioMasterOfflineWrite                : if Assigned(Plugin) then
                                             if Assigned(Plugin.FOnAMOfflineWrite)
                                              then Plugin.FOnAMOfflineWrite(Plugin,ptr); // audioMasterOfflineWrite, same as read
   audioMasterOfflineGetCurrentPass       : if Assigned(Plugin) then
                                             if Assigned(Plugin.FOnAMOfflineGetCurrentPass)
                                              then Plugin.FOnAMOfflineGetCurrentPass(Plugin); // audioMasterOfflineGetCurrentPass
   audioMasterOfflineGetCurrentMetaPass   : if Assigned(Plugin) then
                                             if Assigned(Plugin.FOnAMOfflineGetCurrentMetaPass)
                                              then Plugin.FOnAMOfflineGetCurrentMetaPass(Plugin); // audioMasterOfflineGetCurrentMetaPass
   audioMasterSetOutputSampleRate         : begin
                                             if Assigned(Plugin) then
                                              if Assigned(Plugin.FOnAMSetOutputSampleRate)
                                               then Plugin.FOnAMSetOutputSampleRate(Plugin,opt);
                                             if Assigned(Host)
                                              then Host.VstTimeInfo.SampleRate := Opt;
                                             // raise Exception.Create('audioMasterSetOutputSampleRate, for variable i/o, sample rate in <opt>');
                                            end;
   audioMasterGetOutputspeakerArrangement : {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetSpeakerArrangement, (long)input in <value>, output in <ptr>') {$ENDIF Debug};
   audioMasterGetVendorString             : begin
                                             if Assigned(Host)
                                              then StrCopy(PAnsiChar(ptr), PAnsiChar(Host.VendorString))
                                              else StrCopy(PAnsiChar(ptr), 'Delphi ASIO & VST Project');
                                             Result := 1;
                                            end;
   audioMasterGetProductString            : try
                                             Result := 1;
                                             if Assigned(Host)
                                              then StrCopy(PAnsiChar(ptr), PAnsiChar(Host.ProductString))
                                              else
                                               if Assigned(ptr)
                                                then StrCopy(PAnsiChar(ptr), 'Delphi VST Host')
                                                else Result := 0;
                                            except
                                             Result := 0;
                                            end;
   audioMasterGetVendorVersion            : if Assigned(Host)
                                             then Result := Host.FVendorVersion
                                             else Result := 0;
   audioMasterVendorSpecific              : if Assigned(Plugin)
                                             then Plugin.AudioMasterVendorSpecific(index, value, ptr, opt)
                                             else Result := 0;
   audioMasterSetIcon                     : {$IFDEF Debug} ShowMessage('TODO: audioMasterSetIcon, void* in <ptr>, format not defined yet, Could be a CBitmap .') {$ENDIF Debug};
   audioMasterCanDo                       : if Assigned(Host)
                                             then Result := Integer(Host.AudioMasterCanDo(PAnsiChar(ptr)))
                                             else Result := 0;
   audioMasterGetLanguage                 : if Assigned(Host)
                                             then Result := Integer(Host.FLanguage)
                                             else Result := Integer(hlUnknown);
   audioMasterOpenWindow                  : if Assigned(ptr) then
                                             begin
                                             {$IFDEF VstHostGUI}
                                              with (GHostWindows.Items[GHostWindows.Add(TForm.Create(Host))] as TForm) do
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
//                                               PVstWindow(ptr).winHandle := (GHostWindows.Items[i] as TForm).Handle;
                                              {$ENDIF}
                                             end;
   audioMasterCloseWindow                 : begin
                                             {$IFDEF Debug}
                                             ShowMessage('TODO: audioMasterCloseWindow, ' +
                                             'close window, platform specific handle in <ptr>')
                                             {$ENDIF Debug};
                                            end;
   audioMasterGetDirectory                : if Assigned(Host)
                                             then Result := LongInt(PAnsiChar(Host.FPlugInDir));
   audioMasterUpdateDisplay               : if Assigned(Plugin)
                                             then Plugin.AudioMasterUpdateDisplay;
   audioMasterBeginEdit                   : if Assigned(Plugin)
                                             then Plugin.AudioMasterBeginEdit(Index);
   audioMasterEndEdit                     : if Assigned(Plugin)
                                             then Plugin.AudioMasterEndEdit(Index);
   audioMasterOpenFileSelector            : begin
                                             {$IFDEF VstHostGUI}
                                             if (ptr <> nil) and not Assigned(GHostDialog) then
                                              begin
                                               case PVstFileSelect(ptr).Command of
                                                kVstFileLoad:
                                                 begin
                                                  GHostDialog := TOpenDialog.Create(Host);
                                                  with TOpenDialog(GHostDialog) do
                                                   begin
                                                    Title := PVstFileSelect(ptr).title;
                                                    InitialDir := PVstFileSelect(ptr).initialPath;
                                                    for i := 0 to PVstFileSelect(ptr).nbFileTypes - 1
                                                     do Filter := Filter + ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name) +
                                                                    ' (*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType +
                                                                    ')|*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType + '|';
                                                    if Execute then
                                                     begin
//                                                      PVstFileSelect(ptr).returnPath := PAnsiChar(TOpenDialog(GHostDialog).FileName);
//                                                      StrCopy(PVstFileSelect(ptr).returnPath,PAnsiChar(TOpenDialog(GHostDialog).FileName));
                                                      PVstFileSelect(ptr).sizeReturnPath := Length(FileName);
                                                     end;
                                                   end;
                                                 end;
                                                kVstFileSave:
                                                 begin
                                                  GHostDialog := TSaveDialog.Create(Host);
                                                  with TSaveDialog(GHostDialog) do
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
                                                 GHostDialog := TOpenDialog.Create(Host);
                                                 with TOpenDialog(GHostDialog) do
                                                  begin
                                                   Title := PVstFileSelect(ptr).title;
                                                   InitialDir := PVstFileSelect(ptr).initialPath;
                                                   for i := 0 to PVstFileSelect(ptr).nbFileTypes - 1 do
                                                    Filter := Filter +
                                                    ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name) +
                                                    ' (*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType +
                                                    ')|*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType + '|';
                                                    if TOpenDialog(GHostDialog).Execute
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
                                                then FreeAndNil(GHostDialog);
                                               Result := Integer(True);
                                              end;
                                             {$ENDIF}
                                            end;
   audioMasterCloseFileSelector           : begin
                                             {$IFDEF VstHostGUI}
                                             if Assigned(GHostDialog) then
                                              case PVstFileSelect(ptr).Command of
                                               kVstFileLoad:
                                                if TOpenDialog(GHostDialog).Title = PVstFileSelect(ptr).title
                                                 then FreeAndNil(GHostDialog);
                                               kVstFileSave:
                                                if TSaveDialog(GHostDialog).Title = PVstFileSelect(ptr).title
                                                 then FreeAndNil(GHostDialog);
                                               kVstMultipleFilesLoad:
                                                if TOpenDialog(GHostDialog).Title = PVstFileSelect(ptr).title
                                                 then FreeAndNil(GHostDialog);
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
      Result := 0;
    end;
  end;
 {$IFDEF VstHostExceptionHandling}
 except
  Result := 0;
  {$IFDEF Debug}
  raise;
  {$ENDIF}
 end;
 {$ENDIF}
end;
{$IFDEF DELPHI10_UP} {$endregion 'General Functions'} {$ENDIF}

///////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomVstTimeInformation implementation'} {$ENDIF}

constructor TCustomVstTimeInformation.Create;
begin
 with FVstTimeInfo do
  begin
   SampleRate         := CDefaultSampleRate;
   TimeSigNumerator   := 4;
   TimeSigDenominator := 4;
   SmpteFrameRate     := 1;
   SamplePos          := 0;
   PpqPos             := 0;
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
 Result := FVstTimeInfo.Flags;
end;

function TCustomVstTimeInformation.GetVTIDouble(Index: Integer): Double;
begin
 Result := 0;
 with FVstTimeInfo do
  case Index of
   0: Result := SamplePos;
   1: Result := SampleRate;
   2: Result := NanoSeconds;
   3: Result := PpqPos;
   4: Result := Tempo;
   5: Result := BarStartPos;
   6: Result := CycleStartPos;
   7: Result := CycleEndPos;
  end;
end;

function TCustomVstTimeInformation.GetVTI(Index: Integer) :Integer;
begin
 Result := 0;
 with FVstTimeInfo do
  case Index of
   0: Result := TimeSigNumerator;
   1: Result := TimeSigDenominator;
   2: Result := SmpteOffset;
   3: Result := SmpteFrameRate;
   4: Result := SamplesToNextClock;
  end;
end;

procedure TCustomVstTimeInformation.SetVTI(Index, Value: Integer);
begin
 with FVstTimeInfo do
  case Index of
   0: if Value <> TimeSigNumerator then begin TimeSigNumerator := Value; Change; end;
   1: if Value <> TimeSigDenominator then begin TimeSigDenominator := Value; Change; end;
   2: if Value <> SmpteOffset then begin SmpteOffset := Value; Change; end;
   3: if Value <> SmpteFrameRate then begin SmpteFrameRate := Value; Change; end;
   4: if Value <> SamplesToNextClock then begin SamplesToNextClock := Value; Change; end;
  end;
end;

procedure TCustomVstTimeInformation.SetVTIDouble(Index: Integer; Value: Double);
begin
 with FVstTimeInfo do
  case Index of
   0: if Value <> SamplePos then begin SamplePos := Value; Change; end;
   1: if Value <> SampleRate then begin SampleRate := Value; Change; end;
   2: if Value <> NanoSeconds then begin NanoSeconds := Value; Change; end;
   3: if Value <> PpqPos then begin PpqPos := Value; Change; end;
   4: if Value <> Tempo then begin Tempo := Value; Change; end;
   5: if Value <> BarStartPos then begin BarStartPos := Value; Change; end;
   6: if Value <> CycleStartPos then begin CycleStartPos := Value; Change; end;
   7: if Value <> CycleEndPos then begin CycleEndPos := Value; Change; end;
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
    FNumAutomatable     := Self.FNumAutomatable;
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
 GHostList.Add(Self);
 {$ENDIF}
 FBlockSize   := CDefaultBlockSize;
 FHostVersion := 2300;
 FLanguage    := hlEnglish;
 FHostCanDos  := [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo,
   hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo,
   hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow,
   hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess];

 FCheckStringLengths := False;

 // get current VST plugin path
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
  CreateVstPluginList;
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
  assert(Assigned(GHostList));
  GHostList.Remove(Self);
  {$ENDIF}
 finally
  inherited;
 end;
end;

procedure TCustomVstHost.CreateVstPluginList;
begin
 FVstPlugIns := TVstPlugIns.Create(Self);
end;

procedure TCustomVstHost.VstTimeInfoChanged(Sender: TObject);
begin
 //
end;

function TCustomVstHost.GetItem(Index: Integer): TCustomVstPlugIn;
begin
 Assert(Assigned(FVstPlugIns));
 Result := FVstPlugIns[Index];
end;

function TCustomVstHost.GetPluginCount: Integer;
begin
 assert(Assigned(FVstPlugIns));
 Result := FVstPlugIns.Count;
end;

procedure TCustomVstHost.SetVstPlugIns(const Value: TVstPlugIns);
begin
 assert(Assigned(FVstPlugIns));
 FVstPlugIns.Assign(Value);
end;

function TCustomVstHost.GetHostTempo: Single;
begin
 Result := FVTI.Tempo;
end;

procedure TCustomVstHost.SetHostTempo(const Value: Single);
begin
 if FVTI.Tempo <> Value then
  begin
   FVTI.Tempo := Value;
  end;
end;

procedure TCustomVstHost.setHostCanDos(const Value: THostCanDos);
begin
 if FHostCanDos <> Value
  then FHostCanDos := Value;
end;

procedure TCustomVstHost.SetHostVersion(const Value: Integer);
begin
 if FHostVersion <> Value
  then FHostVersion := Value;
end;

procedure TCustomVstHost.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
end;

function TCustomVstHost.AudioMasterGetNumAutomatableParameters: Integer;
begin
 Result := FNumAutomatable;
end;

function TCustomVstHost.AudioMasterGetParameterQuantization(
  Index: Integer): Integer;
begin
 // returns the integer value for +1.0 representation or 1 if full single
 // float precision is maintained in automation. Index = -1 for all parameters

 if Index = -1
  then Result := FParamQuan
  else {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetParameterQuantization, returns the Integer value for +1.0 representation') {$ELSE} Result := 0 {$ENDIF Debug}
end;

function TCustomVstHost.AudioMasterGetPreviousPlug(
  InputPin: Integer): PVstEffect;
begin
// if PlugIndex = 0 then Result := 0;
 {$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetPreviousPlug, input pin in <value> (-1: first to come), returns cEffect*') {$ELSE} Result := nil {$ENDIF Debug};
end;

function TCustomVstHost.AudioMasterCanDo(CanDo: PAnsiChar): Boolean;
begin
 Result := False;

 if Assigned(CanDo) then
  if      ShortString(CanDo) = 'sendVstEvents' then Result := hcdSendVstEvents in CanDos
  else if ShortString(CanDo) = 'sendVstMidiEvent' then Result := hcdSendVstMidiEvent in CanDos
  else if ShortString(CanDo) = 'sendVstTimeInfo' then Result := hcdSendVstTimeInfo in CanDos
  else if ShortString(CanDo) = 'receiveVstEvents' then Result := hcdReceiveVstEvents in CanDos
  else if ShortString(CanDo) = 'receiveVstMidiEvent' then Result := hcdReceiveVstMidiEvent in CanDos
  else if ShortString(CanDo) = 'receiveVstTimeInfo' then Result := hcdReceiveVstTimeInfo in CanDos
  else if ShortString(CanDo) = 'reportConnectionChanges' then Result := hcdReportConnectionChanges in CanDos
  else if ShortString(CanDo) = 'acceptIOChanges' then Result := hcdAcceptIOChanges in CanDos
  else if ShortString(CanDo) = 'sizeWindow' then Result := hcdSizeWindow in CanDos
  else if ShortString(CanDo) = 'asyncProcessing' then Result := hcdAsyncProcessing in CanDos
  else if ShortString(CanDo) = 'offline' then Result := hcdOffline in CanDos
  else if ShortString(CanDo) = 'supplyIdle' then Result := hcdSupplyIdle in CanDos
  else if ShortString(CanDo) = 'supportShell' then Result := hcdSupportShell in CanDos
  else if ShortString(CanDo) = 'openFileSelector' then Result := hcdOpenFileSelector in CanDos
  else if ShortString(CanDo) = 'closeFileSelector' then Result := hcdcloseFileSelector in CanDos
  else if ShortString(CanDo) = 'editFile' then Result := hcdEditFile in CanDos
  else if ShortString(CanDo) = 'shellCategory' then Result := hcdShellCategory in CanDos
  else if ShortString(CanDo) = 'startStopProcess' then Result := hcdStartStopProcess in CanDos;
end;

function TCustomVstHost.AudioMasterGetNextPlug(OutputPin: Integer): PVstEffect;
begin
{$IFDEF Debug} raise Exception.Create('TODO: audioMasterGetNextPlug, output pin in <value> (-1: first to come), returns cEffect*') {$ELSE} Result := nil {$ENDIF Debug};
end;

function TCustomVstHost.AudioMasterGetSampleRate: Single;
begin
 Result := VstTimeInfo.SampleRate;
end;

function TCustomVstHost.AudioMasterGetTime: PVstTimeInfo;
begin
 Result := @VstTimeInfo.FVstTimeInfo;
end;

procedure TCustomVstHost.AudioMasterSetTime(VstTimeInfo: PVstTimeInfo;
  Filter: TVstTimeInfoFlags);
begin
 // the flags below are not yet supported! (to do: read the SDK)
 if vtiTransportChanged in Filter then ;
 if vtiTransportPlaying in Filter then ;
 if vtiTransportCycleActive in Filter then ;
 if vtiTransportRecording in Filter then ;
 if vtiAutomationWriting in Filter then ;
 if vtiAutomationReading in Filter then ;

 if vtiNanosValid in Filter
  then Self.VstTimeInfo.NanoSeconds := VstTimeInfo.NanoSeconds;

 if vtiPpqPosValid in Filter
  then Self.VstTimeInfo.PpqPos := VstTimeInfo.PpqPos;

 if vtiTempoValid in Filter
  then Self.VstTimeInfo.Tempo := VstTimeInfo.Tempo;

 if vtiBarsValid in Filter
  then Self.VstTimeInfo.BarStartPos := VstTimeInfo.BarStartPos;

 if vtiCyclePosValid in Filter then
  begin
   Self.VstTimeInfo.CycleStartPos := VstTimeInfo.CycleStartPos;
   Self.VstTimeInfo.CycleEndPos := VstTimeInfo.CycleEndPos;
  end;

 if vtiTimeSigValid in Filter then
  begin
   Self.VstTimeInfo.TimeSigNumerator := VstTimeInfo.TimeSigNumerator;
   Self.VstTimeInfo.TimeSigDenominator := VstTimeInfo.TimeSigDenominator;
  end;

 if vtiSmpteValid in Filter then
  begin
   Self.VstTimeInfo.SmpteOffset := VstTimeInfo.SmpteOffset;
   Self.VstTimeInfo.SmpteFrameRate := VstTimeInfo.SmpteFrameRate;
  end;

 if vtiClockValid in Filter
  then Self.VstTimeInfo.SamplesToNextClock := VstTimeInfo.SamplesToNextClock;
end;

function TCustomVstHost.AudioMasterTempoAt(const SamplePosition: Integer): Integer;
begin
 if Assigned(OnGetTempoAtSamplePosition)
  then Result := Round(OnGetTempoAtSamplePosition(Self, SamplePosition) * 10000)
  else Result := Round(Tempo * 10000);
end;

procedure TCustomVstHost.BlockSizeChanged;
var
  PlugIndex : Integer;
begin
 Assert(Assigned(VstPlugIns));
 for PlugIndex := 0 to VstPlugIns.Count - 1 do
  if Assigned(VstPlugIns[PlugIndex].FVstEffect)
   then VstPlugIns[PlugIndex].SetBlockSize(FBlockSize);
end;

procedure TCustomVstHost.UpdateVstTimeInfo(const Samples: Word = 1);
var
  Seconds           : Double;
  SingleSampleClock : Double;
  OffsetInSecond    : Double;
  SmpteDiv          : Double;
begin
 if Assigned(FVTI) then
  with FVTI.FVstTimeInfo do
   begin
    // SamplePos and PpqPos
    SamplePos := SamplePos + Samples;
    Seconds := SamplePos / SampleRate;
    PpqPos := Seconds * (Tempo / 60);
    Flags := Flags + [vtiPpqPosValid];

    // BarStartPos
    BarStartPos := TimeSigDenominator* (round(PpqPos) div TimeSigDenominator);
    Flags := Flags + [vtiBarsValid];

    //SamplesToNextClock
    SingleSampleClock := (60 * SampleRate) / (Tempo*24.0);
    SamplesToNextClock := round(SingleSampleClock
         * (round(SamplePos) div round(SingleSampleclock) + 1));
    Flags := Flags + [vtiClockValid];

(*
    // NanoSeconds
    try
     NanoSeconds := TimeGetTime * 1000000;
     Flags := Flags + [vtiNanosValid];
    except
     Flags := Flags - [vtiNanosValid];
    end;
*)

    // SmpteOffset
    OffsetInSecond := Seconds - round(Seconds - CHalf32);
    case SmpteFrameRate of
        0 : SmpteDiv := 24;
        1 : SmpteDiv := 25;
        2 : SmpteDiv := 29.97;
        3 : SmpteDiv := 30;
        4 : SmpteDiv := 29.97;
        5 : SmpteDiv := 30;
     6..9 : SmpteDiv := 0;
       10 : SmpteDiv := 23.976;
       11 : SmpteDiv := 24.976;
       12 : SmpteDiv := 59.94;
       13 : SmpteDiv := 60;
      else  raise Exception.Create(RCStrInvalidSMTPEFrameRate);
    end;

    SmpteOffset := Round(OffsetInSecond * SmpteDiv * 80);
    Flags := Flags + [vtiSmpteValid];

    // Transport Changed
    Flags := Flags + [vtiTransportChanged];
   end;
end;

procedure TCustomVstHost.ResetVstTimeInformation;
begin
 if Assigned(FVTI) then
  with FVTI do
   begin
    FVstTimeInfo.SamplePos := 0;
    FVstTimeInfo.PpqPos := 0;
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
 Result := TCustomVstHost(FOwner);
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
  if Assigned(FVstOfflineTasks)
   then FreeAndNil(FVstOfflineTasks);
  if FVstDllFileName <> '' then
   try
    {$IFDEF VstHostGUI}
    if EditVisible then CloseEdit;
    if Assigned(GUIControl) and FGUIControlCreated
     then FreeAndNil(FGUIControl);
    {$ENDIF}
   finally
    Unload;
   end;
  {$IFDEF MemDLL}
  if Assigned(FInternalDllLoader) then
   try
    {$IFDEF VstHostGUI}
    if EditVisible then CloseEdit;
    if Assigned(GUIControl) and FGUIControlCreated
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
     if Assigned(Self.FInternalDllLoader)
      then FInternalDllLoader.Assign(Self.FInternalDllLoader) else
     {$ENDIF}
     if Assigned(Self.FVstEffect)
      then LoadFromVSTEffect(Self.FVstEffect);

     Active := Self.FActive;

     {$IFDEF VstHostGUI}
     if Self.FEditOpen then
      begin
       ShowEdit(Self.FGUIControl);
      end;
     {$ENDIF}

     // copy chunk
     {$IFDEF VstHostExceptionHandling}
     try
      i := Self.GetChunk(@p);
      TCustomVstPlugIn(Dest).SetChunk(p, i)
     except
     end;
     {$ELSE}
     i := Self.GetChunk(@p);
     TCustomVstPlugIn(Dest).SetChunk(p, i)
     {$ENDIF}
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

 {$IFDEF VstHostExceptionHandling}
 try
 {$ENDIF}
  {$IFDEF VstHostCubase4}
  VstDispatch(effSetProcessPrecision);
  VstDispatch(effSetBlockSize, 0, 1024);
  VstDispatch(effSetSampleRate, 0, 0, nil, FSampleRate);
  {$ENDIF}

  VstDispatch(effOpen);

  {$IFDEF VstHostCubase4}
  VstDispatch(effMainsChanged, 0, 1);
  {$ENDIF}

  if VstCanDo('bypass') <> 0
   then FVSTCanDos := [vcdBypass]
   else FVSTCanDos := [];
  FVSTCanDosScannedComplete := False;

  {$IFDEF VstHostCubase4}
  VstDispatch(effMainsChanged, 0, 0);

  if VstCanDo('receiveVstMidiEvent') <> 0
   then FVSTCanDos := FVSTCanDos + [vcdReceiveVstMidiEvent]

  if VstCanDo('sendVstMidiEvent') <> 0
   then FVSTCanDos := FVSTCanDos + [vcdSendVstMidiEvent]

  if VstCanDo('midiProgramNames') <> 0
   then FVSTCanDos := FVSTCanDos + [vcdMidiProgramNames]

  VstDispatch(effSetEditKnobMode, 0, 2);
  {$ENDIF}

  SetPanLaw(plLinear, Sqrt(2));

  // update blocksize
  if Collection.Owner is TCustomVstHost
   then SetBlockSize(TCustomVstHost(Collection.Owner).BlockSize)
   else SetBlockSize(CDefaultBlockSize);

  // update samplerate 
  if Collection.Owner is TCustomVstHost
   then SetSampleRate(TCustomVstHost(Collection.Owner).VstTimeInfo.SampleRate)
   else SetSampleRate(CDefaultSampleRate);

  // eventually disable bypass 
  if vcdBypass in FVSTCanDos
   then SetBypass(False);

  FVstVersion   := GetVstVersion;
  FPlugCategory := GetPlugCategory;
  MainsChanged(True);
 {$IFDEF VstHostExceptionHandling}
 except
  FActive := False;
 end;
 {$ENDIF}
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
 {$IFDEF VstHostExceptionHandling}
 try
  DontRaiseExceptionsAndSetFPUcodeword;
  if not Assigned(FVstEffect)
   then Result := 0
   else Result := FVstEffect.Dispatcher(FVstEffect, opCode, Index, Value, Pntr, opt);
 except
  Result := 0;
 end;
 {$ELSE}
 DontRaiseExceptionsAndSetFPUcodeword;
 Result := FVstEffect.Dispatcher(FVstEffect, opCode, index, value, pntr, opt);
 {$ENDIF}
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
  then FVstEffect.SetParameter(FVstEffect, Index, Parameter);
end;

function TCustomVstPlugIn.GetParameter(Index: Integer): Single;
begin
 if FVstEffect = nil
  then Result := 0
  else Result := FVstEffect.GetParameter(FVstEffect, Index);
end;

procedure TCustomVstPlugIn.SetCurrentProgram(const lValue: Integer);
begin
 if FActive and (FProgramNr <> lValue) then
  begin
   FProgramNr := lValue;
   VstDispatch(effSetProgram, 0, FProgramNr);
  end;
end;

function TCustomVstPlugIn.GetCurrentProgram: Integer;
begin
 if FActive
  then Result := VstDispatch(effGetProgram)
  else Result := -1;
 FProgramNr := Result;
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
 Result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetProgramName, 0, 0, Temp) = 0 // check is not part of the official specification
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if (TVSTPlugins(Collection).VSTHost.CheckStringLengths) and (Length(Result) > 24)
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
 Result := '';

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetParamLabel, index, 0, Temp) = 0 // check is not part of the specification
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths and (Length(Result) > 8)
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
 Result := '';

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetParamDisplay, index, 0, Temp) = 0 // check is not part of the specification
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths and (Length(Result) > 8)
    then raise Exception.Create('String too short');
 finally

  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetParamName(Index: Integer): string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 Result := '';

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetParamName, Index, 0, Temp) = 0 // check is not part of the official specification
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths and (Length(Result) > 8)
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

procedure TCustomVstPlugIn.AudioMasterNeedIdle;
begin
 FNeedIdle := True;
 if Assigned(FOnAMNeedIdle)
  then FOnAMNeedIdle(Self);

 if (Collection.Owner <> nil) and TCustomVSTHost(Collection.Owner).FAutoIdle
  then Idle;
end;

function TCustomVstPlugIn.GetVu: Single;
const
  Divisor : Double = 1 / 32767;
begin
 if FActive
  then Result := VstDispatch(effGetVu) * Divisor
  else Result := -1;
end;

function TCustomVstPlugIn.GetRealQualities: LongInt;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect^.RealQualities
  else Result := 0;
end;

function TCustomVstPlugIn.GetRect: TRect;
var
  theRect: ERect;
begin
 {$IFDEF VstHostGUI}
 case FGUIStyle of
  gsDefault : if (effFlagsHasEditor in FVstEffect.EffectFlags)
               then theRect := EditGetRect
               else theRect := CalculateGuiRectList;
  gsParameterList : theRect := CalculateGuiRectList;
  gsParameterSelector : theRect := CalculateGuiRectSelector;
  gsCustom : if Assigned(FGUIControl)
              then theRect := Rect(0, 0, FGUIControl.Width, FGUIControl.Height)
              else theRect := Rect(0, 0, 0, 0);
 end;
 {$ELSE}
 theRect := Rect(0, 0, 0, 0);
 {$ENDIF}
 Result := Classes.Rect(theRect.left, theRect.Top, theRect.right, theRect.Bottom);
end;

{$IFDEF VstHostGUI}
function TCustomVstPlugIn.CalculateGuiRectList: ERect;
begin
 Result := Rect(0, 0, 320, 4 + numParams * 16);
end;

function TCustomVstPlugIn.CalculateGuiRectSelector: ERect;
begin
 Result := Rect(0, 0, 256, 80);
end;

function TCustomVstPlugIn.EditGetRect: ERect;
var
  temp: PPERect;
begin
 GetMem(temp, SizeOf(PPERect));
 try
  if FActive then VstDispatch(effEditGetRect, 0, 0, temp);
  if Assigned(temp) then
   if Assigned(temp^)
    then Result := temp^^;
 finally
  Dispose(temp);
 end;
end;

function TCustomVstPlugIn.EditOpen(const Handle: THandle): Integer;
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
  Result := i;
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
var
  rct : TRect;
begin
 if not Assigned(GUIControl) then
  begin
   FGUIControl := TForm.Create(nil);
   with TForm(FGUIControl) do
    begin
     Caption := VendorString + ' - ' + ProductString;
     BorderStyle := bsToolWindow;
     Position := poDesktopCenter;
     Visible := True;
     OnClose := FormCloseHandler;
     OnActivate := EditActivateHandler;
     OnDeActivate := EditDeactivateHandler;
     if Caption = ' - ' then Caption := GetEffectName;
    end;
   FGUIControlCreated := True;
   ShowEdit(GUIControl);

   Rct := GetRect;
   with TForm(FGUIControl) do
    begin
     ClientWidth := rct.Right - rct.Left;
     ClientHeight := rct.Bottom - rct.Top;
    end; 
  end;
end;

procedure TCustomVstPlugIn.ShowEdit(Control: TWinControl);
begin
 if not Active
  then raise Exception.Create('Plugin not active!');

 if Control = nil
  then raise Exception.Create('Control must exist!');
 if (effFlagsHasEditor in FVstEffect.EffectFlags) and (FGUIStyle = gsDefault) then
  begin
   if not FEditOpen then
    begin
     EditOpen(Control.Handle);
     FGUIControl := Control;
     EditIdle;
    end;
//  else raise Exception.Create('Editor is already open!');
  end
 else // VST plugin has no GUI itself
  case FGUIStyle of
   gsDefault, gsParameterList: ShowHostGuiList(Control);
   gsParameterSelector: ShowHostGuiSelector(Control);
   gsCustom: ShowHostGuiCustom(Control);
  end;
 if Assigned(FOnShowEdit) then FOnShowEdit(Self, GUIControl);
end;

procedure TCustomVstPlugIn.ShowHostGuiSelector(Control: TWinControl);
var
  Param : string;
  i     : Integer;
begin
 FGUIControl := Control;
 FEditOpen := True;
 if not Assigned(FGUIElements)
  then FGUIElements := TObjectList.Create;

 with TLabel(FGUIElements[FGUIElements.Add(TLabel.Create(Control))]) do
  begin
   Name := 'LbL'; Parent := Control; Caption := '';
   Alignment := taCenter; Left := 10; Top := 64;
  end;
 with TTrackBar(FGUIElements[FGUIElements.Add(TTrackBar.Create(Control))]) do
  begin
   Name := 'ParamBar'; Parent := Control;
   Anchors := [akLeft, akTop, akRight];
   Left := 5; Top := 33; Orientation := trHorizontal;
   Width := Control.Width - 4 * Left; Height := 32;
   Frequency := 1; Position := 0; {$IFNDEF FPC} SelEnd := 0; SelStart := 0; {$ENDIF}
   TabOrder := 3; Min := 0; Max := 1000; OnChange := ControlChangeSelector;
   TickMarks := tmBottomRight; TickStyle := tsNone;//Auto;
  end;
 with TComboBox(FGUIElements[FGUIElements.Add(TComboBox.Create(Control))]) do
  begin
   Name := 'ParamBox'; Parent := Control; param := '';
   for i := 0 to numParams - 1
    do begin param := GetParamName(i); Items.Add(param); end;
   Anchors := [akLeft, akTop, akRight]; Left := 4; Top := 5;
   Width := Control.Width - 4 * Left; Height := 21; ItemHeight := 13;
   TabOrder := 2; OnChange := GuiParameterChangeSelector; Text := ''; ItemIndex := 0;
   Font.Color := clWindowText;
   OnChange(nil);
  end;
end;

procedure TCustomVstPlugIn.ShowHostGuiList(Control: TWinControl);
var
  i, j          : Integer;
  MaxParamWidth : Integer;
  theRect       : ERect;
begin
 theRect := Rect(0, 0, Control.Width, 4 + numParams * 16);
 FGUIControl := Control;
 if not Assigned(FGUIElements)
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
     Name := 'LbV' + IntToStr(I); Tag := i; Anchors := [akTop, akRight];
     Parent  := Control; Alignment := taCenter; AutoSize := False;
     Height  := 16; Left := Control.Width - Left - 72;
     Alignment := taCenter; Width := 65; Top := 2 + i * Height;
    end;
   j := FGUIElements.Add(TScrollBar.Create(Control));
   with TScrollBar(FGUIElements[j]) do
    begin
     Parent := Control; Anchors := [akLeft, akTop, akRight];
     Kind := sbHorizontal; LargeChange := 10; Tag := i;
     Height := 16; Top := 2 + i * Height; Name := 'SB' + IntToStr(I);
     Left := MaxParamWidth + 2; Width := Control.Width - Left - 72;
     Min := 0; Max := 1000; TabOrder := 3 + i;
     Position := Round(1000 * Parameter[i]);
     OnChange := ControlChangeList;
     ControlChangeList(FGUIElements[j]);
    end;
  end;
 GUIControl.Visible := True;
 GUIControl.Invalidate;
 FEditOpen := True;
end;

procedure TCustomVstPlugIn.ShowHostGuiCustom(Control: TWinControl);
begin
 FGUIControl := Control;
 GUIControl.Visible := True;
 GUIControl.Invalidate;
 FEditOpen := True;
end;

procedure TCustomVstPlugIn.ControlChangeList(Sender: TObject);
var
  Lbl    : TLabel;
  Str    : string;
  ChrPos : Integer;
begin
 // ensure sender is TScrollBar
 Assert(Sender is TScrollBar);

 with TScrollBar(Sender) do
  try
   // convert integer position to float
   Parameter[Tag] := Position * 0.001;

   // locate value label
   Lbl := TLabel(GUIControl.FindComponent('LbV' + IntToStr(Tag)));
   if Assigned(Lbl) then
    begin
     // eventually add parameter label
     if GetParamLabel(Tag) <> ''
      then Str := GetParamDisplay(Tag) + ' ' + GetParamLabel(Tag)
      else Str := GetParamDisplay(Tag);

     if Length(Str) < 9
      then Lbl.Caption := Str
      else
       begin
        Str := GetParamDisplay(Tag);
        if Pos('.', Str) > 0 then
         begin
          ChrPos := Length(Str) - 1;
          while Str[ChrPos] = '0' do
           begin
            Delete(Str, ChrPos, 1);
            Dec(ChrPos);
           end;
         end;
        if GetParamLabel(Tag) <> ''
         then Lbl.Caption := Str + ' ' + GetParamLabel(Tag)
         else Lbl.Caption := Str;
        if Length(Lbl.Caption) > 9 then Lbl.Caption := Str
       end;
    end;
  except
  end;
end;

procedure TCustomVstPlugIn.GuiParameterChangeList(Sender: TObject);
var
  Lbl    : TLabel;
  Str    : string;
  ChrPos : Integer;
begin
 // ensure sender is TScrollBar
 Assert(Sender is TScrollBar);

 with TScrollBar(Sender) do
  try
   // convert integer position to float
   Position := Round(Parameter[Tag] * 1000);

   // locate value label
   Lbl := TLabel(GUIControl.FindComponent('LbV' + IntToStr(Tag)));
   if Assigned(Lbl) then
    begin
     // eventually add parameter label
     if GetParamLabel(Tag) <> ''
      then Str := GetParamDisplay(Tag) + ' ' + GetParamLabel(Tag)
      else Str := GetParamDisplay(Tag);

     if Length(Str) < 9
      then Lbl.Caption := Str
      else
       begin
        Str := GetParamDisplay(Tag);
        if Pos('.', Str) > 0 then
         begin
          ChrPos := Length(Str) - 1;
          while Str[ChrPos] = '0' do
           begin
            Delete(Str, ChrPos, 1);
            Dec(ChrPos);
           end;
         end;
        if GetParamLabel(Tag) <> ''
         then Lbl.Caption := Str + ' ' + GetParamLabel(Tag)
         else Lbl.Caption := Str;
        if Length(Lbl.Caption) > 9 then Lbl.Caption := Str
       end;
    end;
  except
  end;
end;

procedure TCustomVstPlugIn.RenderEditorToBitmap(Bitmap: TBitmap);
var
  r : TRect;
begin
 // make sure the editor is visible
 if not Assigned(FGUIControl)
  then raise Exception.Create('Editor not instanciated yet');

 {$IFNDEF FPC}
 with TCanvas.Create do
  try
   Handle := GetWindowDC(GUIControl.Handle);
   try
    r := GetRect;
    Bitmap.Width  := r.Right - r.Left;
    Bitmap.Height := r.Bottom - r.Top;
    BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Handle,
      r.Left, r.Top, SRCCOPY)
 //   Bitmap.Canvas.CopyRect(r, c, r);
   finally
    ReleaseDC(0, Handle);
    Handle := 0;
   end;
  finally
   Free;
  end;
 {$ENDIF}
end;

procedure TCustomVstPlugIn.GuiParameterChangeSelector(Sender: TObject);
var
  GuiControlIndex : Integer;
  WindowControl   : TComponent;
begin
 WindowControl := GUIControl.FindComponent('ParamBar');

 if WindowControl is TTrackBar then
  with TTrackBar(WindowControl) do
   try
    GuiControlIndex := (GUIControl.FindComponent('ParamBox') as TComboBox).ItemIndex;
    if (GuiControlIndex >= 0) and (GuiControlIndex < numParams)
     then Position := Round(Parameter[GuiControlIndex] * 1000);

    // update parameter on GUI
    UpdateParameterOnGui(GuiControlIndex);
   except
   end;
end;

procedure TCustomVstPlugIn.ControlChangeSelector(Sender: TObject);
var
  GuiControlIndex : Integer;
  WindowControl   : TComponent;
begin
 WindowControl := GUIControl.FindComponent('ParamBar');

 if WindowControl is TTrackBar then
  with TTrackBar(WindowControl) do
   try
    GuiControlIndex := (GUIControl.FindComponent('ParamBox') as TComboBox).ItemIndex;

    Parameter[GuiControlIndex] := Position * 0.001;

    // update parameter on GUI
    UpdateParameterOnGui(GuiControlIndex);
   except
   end;
end;

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
  if Assigned(FOnCloseEdit) then FOnCloseEdit(Self);
  if (effFlagsHasEditor in FVstEffect.EffectFlags) and (FGUIStyle = gsDefault)
   then EditClose else
   if Assigned(FGUIElements)
    then FreeAndNil(FGUIElements);
  if Assigned(GUIControl) and FGUIControlCreated
   then FreeAndNil(FGUIControl); //and (not FExternalForm) then
 finally
  FEditOpen := False;
 end;
end;

procedure TCustomVstPlugIn.FormCloseHandler(Sender: TObject; var Action: TCloseAction);
begin
 CloseEdit;
 if Assigned(GUIControl)
  then FreeAndNil(FGUIControl);
end;

function TCustomVstPlugIn.EditIdle: Integer;
begin
 if FEditOpen
  then Result := VstDispatch(effEditIdle)
  else Result := 0;
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
 Result := VstDispatch(effIdentify);
end;

function TCustomVstPlugIn.GetChunk(const Data: Pointer; const IsPreset: Boolean = False): Integer;
begin
 Result := VstDispatch(effGetChunk, Integer(isPreset), 0, Data);
end;

function TCustomVstPlugIn.SetChunk(const Data: Pointer; const ByteSize: Integer; const IsPreset: Boolean = False): Integer;
begin
 Result := VstDispatch(effSetChunk, Integer(isPreset), ByteSize, Data);
end;

function TCustomVstPlugIn.SetInputSpeakerArrangement(const PluginInput: TVstSpeakerArrangement): Boolean;
begin
 Result := Boolean(VstDispatch(effSetSpeakerArrangement, 0, Integer(@PluginInput), nil));
end;

function TCustomVstPlugIn.SetOutputSpeakerArrangement(const PluginOutput: TVstSpeakerArrangement): Boolean;
begin
 Result := Boolean(VstDispatch(effSetSpeakerArrangement, 0, 0, @PluginOutput));
end;

function TCustomVstPlugIn.ProcessEvents(const VstEvents: TVstEvents): Integer;
begin
 Result := VstDispatch(effProcessEvents, 0, 0, @VstEvents);
end;

function TCustomVstPlugIn.CanBeAutomated(const Index: Integer): Integer;
begin
 Result := VstDispatch(effCanBeAutomated, Index);
end;

function TCustomVstPlugIn.CheckValidPlugin(const FileName: TFilename): Boolean;
begin
 Result := CheckValidVstPlugin(FileName);
end;

function TCustomVstPlugIn.String2Parameter(const Index: Integer; ValueString: string): Boolean;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 Result := False;
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   begin
    if Length(ValueString) > Lngth
     then SetLength(ValueString, Lngth);

    StrPCopy(Temp, ValueString);
    if VstDispatch(effString2Parameter, Index, 0, Temp) <> 0 then
     begin
      ValueString := StrPas(Temp);
      Result := True;
     end;
   end;
 finally
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetNumProgramCategories: Integer;
begin
 if FActive
  then Result := VstDispatch(effGetNumProgramCategories)
  else Result := -1;
end;

function TCustomVstPlugIn.GetnumPrograms: Integer;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect^.numPrograms
  else Result := 0;
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
     Result := VstDispatch(effGetProgramNameIndexed, Index, Category, Temp);
     if Result > 0
      then ProgramName := StrPas(Temp);
    end
   else Result := -1;

 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.CopyCurrentProgramTo(const Destination: Integer): Boolean;
begin
 if FActive
  then Result := Boolean(VstDispatch(effCopyProgram, Destination))
  else Result := False;
end;

function TCustomVstPlugIn.ConnectInput(const InputNr: Integer; const State: Boolean): Integer;
begin
 if FActive
  then Result := VstDispatch(effConnectInput, InputNr, Integer(State))
  else Result := -1;
end;

function TCustomVstPlugIn.ConnectOutput(const OutputNr: Integer; const State: Boolean): Integer;
begin
 if FActive
  then Result := VstDispatch(effConnectOutput, OutputNr, Integer(State))
  else Result := -1;
end;

function TCustomVstPlugIn.GetInputProperties(const InputNr: Integer): TVstPinProperties;
begin
 FillChar(Result, SizeOf(TVstPinProperties), 0);
 if FActive
  then VstDispatch(effGetInputProperties, InputNr, 0, @Result);
end;

function TCustomVstPlugIn.GetIORatio: Single;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect^.IORatio
  else Result := 1;
end;

function TCustomVstPlugIn.GetOffQualities: LongInt;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect^.OffQualities
  else Result := 0;
end;

function TCustomVstPlugIn.GetOutputProperties(const OutputNr: Integer): TVstPinProperties;
begin
 FillChar(Result, SizeOf(TVstPinProperties), 0);
 if FActive
  then VstDispatch(effGetOutputProperties, OutputNr, 0, @Result);
end;

function TCustomVstPlugIn.GetPlugCategory: TVstPluginCategory;
begin
 if FActive
  then Result := TVstPluginCategory(VstDispatch(effGetPlugCategory))
  else Result := vpcUnknown;
end;

function TCustomVstPlugIn.GetCurrentPosition: Integer;
begin
 if FActive
  then Result := VstDispatch(effGetCurrentPosition)
  else Result := -1;
end;

function TCustomVstPlugIn.GetDestinationBuffer: Integer;
begin
 if FActive
  then Result := VstDispatch(effGetDestinationBuffer)
  else Result := -1;
end;

function TCustomVstPlugIn.OfflineNotify(const VstAudioFile: TVstAudioFile; const NumAudioFiles: Integer; const Start: Boolean): Integer;
begin
 Result := VstDispatch(effOfflineNotify, Integer(Start), NumAudioFiles, @VstAudioFile);
end;

function TCustomVstPlugIn.OfflinePrepare(const VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count: Integer): Integer;
begin
 Result := VstDispatch(effOfflinePrepare, 0, Count, @VstOfflineTaskRecord);
end;

function TCustomVstPlugIn.OfflineRun(const VstOfflineTaskRecord: TVstOfflineTaskRecord; const Count: Integer): Integer;
begin
 Result := VstDispatch(effOfflineRun, 0, Count, @VstOfflineTaskRecord);
end;

function TCustomVstPlugIn.ProcessVarIo(const VarIo: TVstVariableIo): Integer;
begin
 Result := VstDispatch(effProcessVarIo, 0, 0, @VarIo);
end;

function TCustomVstPlugIn.SetSpeakerArrangement(const PluginInput, PluginOutput: TVstSpeakerArrangement): Boolean;
begin
 Result := Boolean(VstDispatch(effSetSpeakerArrangement, 0, Integer(@PluginInput), @PluginOutput));
end;

function TCustomVstPlugIn.SetBlockSizeAndSampleRate(const BlockSize: Integer; const SampleRate: Single): Integer;
begin
 Result := VstDispatch(effSetBlockSizeAndSampleRate, 0, BlockSize, nil, SampleRate);
end;

function TCustomVstPlugIn.SetBypass(const Value: Boolean): Integer;
begin
 Result := VstDispatch(effSetBypass, 0, Integer(Value));
end;

function TCustomVstPlugIn.GetEffectName: string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 Result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetEffectName, 0, 0, Temp) <> 0 // not sure about this
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(Result) <= 32);
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
 Result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetErrorText, 0, 0, Temp) <> 0 // not sure here!
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(Result) <= 256);
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
 Result := Variations[v];
 if Result = ''
  then Result := DisplayName;
end;

function TCustomVstPlugIn.GetVendorString: string;
var
  Temp : PAnsiChar;
const
  Lngth = 256;
begin
 Result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetVendorString, 0, 0, Temp) <> 0 // not sure here!
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(Result) <= 64);
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
 Result := '';
 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   if VstDispatch(effGetProductString, 0, 0, Temp) <> 0 // not sure here!
    then Result := StrPas(Temp);

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
   if TVSTPlugins(Collection).VSTHost.CheckStringLengths
    then assert(Length(Result) <= 64);
 finally
  // dispose memory
  Dispose(Temp);
 end;
end;

function TCustomVstPlugIn.GetVendorVersion: Integer;
begin
 if FActive
  then Result := VstDispatch(effGetVendorVersion)
  else Result := -1;
end;

function TCustomVstPlugIn.GetVersion: Integer;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect.Version
  else Result := 0;
end;

function TCustomVstPlugIn.GetVSTCanDos: TVstCanDos;
begin
 if FVSTCanDosScannedComplete
  then Result := FVstCanDos
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
 Result := VstDispatch(effVendorSpecific, index, value, pntr, opt);
end;

function TCustomVstPlugIn.VstCanDo(const CanDoString: string): Integer;
begin
 Result := VstDispatch(effCanDo, 0, 0, PAnsiChar(CanDoString));
end;

function TCustomVstPlugIn.GetTailSize: Integer;
begin
 if FActive then Result := VstDispatch(effGetTailSize) else Result := -1;
end;

function TCustomVstPlugIn.GetUniqueID: string;
begin
 if Assigned(FVstEffect) then
  with FVstEffect^
   do Result := uniqueID[3] + uniqueID[2] + uniqueID[1] + uniqueID[0]
  else Result := '';
end;

function TCustomVstPlugIn.Idle: Integer;
begin
 if FNeedIdle
  then Result := VstDispatch(effIdle)
  else Result := 0;
end;

procedure TCustomVstPlugIn.AudioMasterIOchanged;
begin
 if Assigned(FOnAMIOChanged)
  then FOnAMIOChanged(Self);
end;

function TCustomVstPlugIn.GetIcon: Integer;
begin
 if FActive
  then Result := VstDispatch(effGetIcon)
  else Result := -1;
end;

procedure TCustomVstPlugIn.SetViewPosition(const x, y: Integer);
begin
 VstDispatch(effSetViewPosition, x, y);
end;

function TCustomVstPlugIn.GetParameterProperties(const Index: Integer;
  var ParameterProperties: TVstParameterPropertyRecord): Boolean;
begin
 if FActive
  then Result := VstDispatch(effGetParameterProperties, Index, 0, @ParameterProperties) <> 0
  else Result := False;
end;

function TCustomVstPlugIn.KeysRequired: Integer;
begin
 if FActive
  then Result := VstDispatch(effKeysRequired)
  else Result := -1;
end;

function TCustomVstPlugIn.GetVstVersion: Integer;
begin
 if FActive
  then Result := VstDispatch(effGetVstVersion)
  else Result := -1;
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
  then Result := FVstEffect^.numInputs
  else Result := 0;
end;

function TCustomVstPlugIn.GetNumOutputs: Integer;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect^.numOutputs
  else Result := 0;
end;

function TCustomVstPlugIn.GetNumParams: Integer;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect^.numParams
  else Result := 0;
end;

function TCustomVstPlugIn.GetCurrentMidiProgram(var MidiProgramName: TMidiProgramName; const Channel: Integer = 0): Integer;
begin
 // returns the programIndex of the current program.
 // passed <ptr> points to MidiProgramName struct.
 // struct will be filled with information for the current program.
 if FActive
  then Result := VstDispatch(effGetCurrentMidiProgram, Channel, 0, @MidiProgramName, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.GetMidiProgramCategory(var MidiProgramCategory: TMidiProgramCategory; const Channel: Integer = 0): Integer;
begin
 // passed <ptr> points to MidiProgramCategory struct.
 // struct will be filled with information for 'thisCategoryIndex'.
 // returns number of used CategoryIndexes.
 // if 0 is returned, no MidiProgramCategories supported.
 if FActive
  then Result := VstDispatch(effGetMidiProgramCategory, Channel, 0, @MidiProgramCategory, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.HasMidiProgramsChanged(const Channel: Integer = 0): Integer;
begin
 // returns 1 if the MidiProgramNames or MidiKeyNames
 // had changed on this channel, 0 otherwise. <ptr> ignored.

 if FActive
  then Result := VstDispatch(effHasMidiProgramsChanged, Channel, 0, nil, 0)
  else Result := 0;
end;

function TCustomVstPlugIn.GetMidiKeyName(var MidiKeyName: TMidiKeyName; const Channel: Integer = 0): Integer;
begin
 // struct will be filled with information for 'thisProgramIndex' and
 // 'thisKeyNumber'. If keyName is "" the standard name of the key
 // will be displayed. If 0 is returned, no MidiKeyNames are
 // defined for 'thisProgramIndex'.
 if FActive
  then Result := VstDispatch(effGetMidiKeyName, Channel, 0, @MidiKeyName, 0)
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
 Result := 0;

 // allocate and zero memory (256 byte, which is more than necessary, but
 // just to be sure and in case of host ignoring the specs)
 GetMem(Temp, Lngth);
 try
  FillChar(Temp^, Lngth, 0);
  if FActive then
   begin
    Result := VstDispatch(effShellGetNextPlugin, 0, 0, Temp);
    if Result <> 0 then PluginName := StrPas(temp);
   end;

  // check whether the Result string accords to the specs
  if Assigned(Collection) and Assigned(TVSTPlugins(Collection).VSTHost) then
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

procedure TCustomVstPlugIn.AudioMasterAutomate(Index: Integer; Value: Single);
begin
 if Assigned(FOnAMAutomate)
  then FOnAMAutomate(Self, Index, Value);

 {$IFDEF VstHostGUI}
 // eventually update GUI
 if Assigned(FGUIElements)
  then UpdateParameterOnGui(Index);
 {$ENDIF}
end;

procedure TCustomVstPlugIn.AudioMasterBeginEdit(ParameterIndex: Integer);
begin
 if Assigned(FOnAMBeginEdit)
  then FOnAMBeginEdit(Self, ParameterIndex);
end;

function TCustomVstPlugIn.AudioMasterPinConnected(Index: Integer;
  PinConnected: Boolean): Boolean;
begin
 if Assigned(FOnAMPinConnected)
  then Result := not FOnAMPinConnected(Self, Index, PinConnected)
  else Result := False;
end;

procedure TCustomVstPlugIn.AudioMasterProcessEvents(VstEvents: PVstEvents);
begin
 if Assigned(FOnProcessEvents)
  then FOnProcessEvents(Self, VstEvents);
end;

function TCustomVstPlugIn.AudioMasterSizeWindow(Width, Height: Integer): Boolean;
begin
 Result := False;
 {$IFDEF VstHostGUI}
 if Pos('DASH', UpperCase(VendorString)) > 0
  then Result := False else
 if Pos('WUSIK', UpperCase(VendorString)) > 0
  then Result := False else
 if Assigned(GUIControl) then
  begin
   GUIControl.ClientWidth := Width;
   GUIControl.ClientHeight := Height;
   Result := True;
  end;
 {$ENDIF}
end;

procedure TCustomVstPlugIn.AudioMasterUpdateDisplay;
begin
 if Assigned(FOnAMUpdateDisplay)
  then FOnAMUpdateDisplay(Self);

 {$IFDEF VstHostGUI}
 // eventually update all GUI elements
 if Assigned(FGUIElements)
  then UpdateParameterOnGui(-1);
 {$ENDIF}
end;

function TCustomVstPlugIn.AudioMasterVendorSpecific(Index, Value: Integer;
  ptr: Pointer; opt: Single): Integer;
begin
 if Assigned(FOnVendorSpecific)
  then Result := FOnVendorSpecific(index, value, ptr, opt)
  else Result := 0;
end;

procedure TCustomVstPlugIn.AudioMasterWantMidi;
begin
 if Assigned(FOnAMWantMidi)
  then FOnAMWantMidi(Self);
end;

function TCustomVstPlugIn.AudioMasterCurrentId: Integer;
begin
 Result := Identify;
end;

procedure TCustomVstPlugIn.AudioMasterEndEdit(ParameterIndex: Integer);
begin
 if Assigned(FOnAMEndEdit)
  then FOnAMEndEdit(Self, ParameterIndex);
end;

procedure TCustomVstPlugIn.AudioMasterIdle;
var
  PluginIndex : Integer;
begin
 {$IFDEF VstHostGUI}
 FNeedIdle := True;

 if Assigned(FOnAMIdle)
  then FOnAMIdle(Self);


 if Collection.Owner <> nil then
  with TCustomVstHost(Collection.Owner) do
   if FAutoIdle then
    begin
     if EditVisible then EditIdle;

     for PluginIndex := 0 to VstPlugIns.Count - 1 do
      if VstPlugIns[PluginIndex].EditVisible
       then VstPlugIns[PluginIndex].EditIdle;
    end;
 {$ENDIF}
end;

{$IFDEF VstHostGUI}
procedure TCustomVstPlugIn.UpdateParameterOnGui(ParameterIndex: Integer);
var
  ParamIndex : Integer;
  SB         : TScrollBar;
begin
 case FGUIStyle of
  gsDefault, gsParameterList :
   if ParameterIndex >= 0 then
    begin
     SB := TScrollBar(GUIControl.FindComponent('SB' + IntToStr(ParameterIndex)));
     if Assigned(SB)
      then GuiParameterChangeList(SB);
    end else
   for ParamIndex := 0 to numParams - 1
    do UpdateParameterOnGui(ParamIndex);
  gsParameterSelector :
   if ParameterIndex >= 0 then
    begin
     (GUIControl.FindComponent('LbL') as TLabel).Caption  :=
       RStrValue + ': ' + GetParamDisplay(ParameterIndex) + ' ' + GetParamLabel(ParameterIndex);
    end;
 end;
end;
{$ENDIF}

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
 // set temporary host pointer
 GTempPlugin := Self;

 // run VST plugin main function to create the VstEffect pointer
 if Assigned(FMainFunction)
  then FVstEffect := FMainFunction(@audioMaster);

 // clear temporary host pointer
 GTempPlugin := nil;

 // check if a VstEffect pointer has been created created!
 if FVstEffect = nil
  then raise Exception.Create(RStrLoadingFailed);

 // check if the VstEffect pointer is valid
 if FVstEffect.Magic <> 'PtsV' // reversed (due to reversed byte order)
  then raise Exception.Create(RStrVSTPluginNotValid);

 // set host related variables
 FVstEffect.ReservedForHost := Self;
 {$IFDEF DELPHI6_UP}
 if Assigned(Collection)
  then FVstEffect.Resvd2    := Collection.Owner
  else FVstEffect.Resvd2    := nil;
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
  if not Assigned(FInternalDLLLoader)
   then Unload;
  {$ENDIF}
end;

procedure TCustomVstPlugIn.LoadFromVSTEffect(const Value: PVSTEffect);
begin
 if FLoaded
  then Unload;
 try
   if Value^.Magic <> 'PtsV' // reversed (due to reversed byte order)
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

 if not Assigned(FInternalDLLLoader)
  then FInternalDLLLoader := TDLLLoader.Create;
 try
  FInternalDLLLoader.Load(Stream);
  FMainFunction := FInternalDllLoader.FindExport('VSTPluginMain');
  if not Assigned(FMainFunction)
   then FMainFunction := FInternalDllLoader.FindExport('main');

  if not Assigned(FMainFunction)
   then FMainFunction := FInternalDllLoader.FindExportPerIndex(0);

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
 if Assigned(FInternalDllLoader) then
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

function TCustomVstPlugIn.GetPreset(const ProgramNo: Integer): TFXPreset;
var
  str : string;
  i   : Integer;
begin
 SetCurrentProgram(ProgramNo);
 with Result do
  begin
   ChunkMagic := 'CcnK';
   FXMagic    := 'FxCk';
   Version    := 1;
   FXID       := FVstEffect^.UniqueID;
   FXVersion  := FVstEffect^.version;
   NumParams  := Self.numParams;
   str        := GetProgramName + #0;
   FillChar(prgName, 26, #0);
   StrLCopy(prgName, PAnsiChar(str), 26);

   GetMem(Params, numParams * SizeOf(Single));
   for i := 0 to numParams - 1 do
    begin
     PDAVSingleFixedArray(params)^[i] := Parameter[i];
     SwapLong(PDAVSingleFixedArray(params)^[i]);
    end;
   // set bytesize excl. ChunkMagic, ByteSize & Params (not part of the spec)
   Result.ByteSize := SizeOf(Result) - SizeOf(LongInt) * 3 + numParams * SizeOf(Single);

   // swap
   SwapLong(ByteSize);
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
 if not Assigned(FVstEffect) then exit;
 if effFlagsProgramChunks in EffectOptions then
  with FXChunkSet do
   begin
    ChunkMagic  := 'CcnK';
    FXMagic     := 'FPCh';
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
    FXPreset := GetPreset(GetCurrentProgram);
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
  ChunkName      : TChunkName;
  PatchChunkInfo : TVstPatchChunkInfo;
  UseChunk       : Boolean;
begin
 if not Assigned(FVstEffect) then exit;
 Stream.Seek(0, soBeginning);

 // read chunk magic
 Stream.Read(ChunkName, 4);
 assert(ChunkName = 'CcnK');

 // check stream size
 Stream.Read(ChunkDataSize, 4);
 SwapLong(ChunkDataSize);
 assert(ChunkDataSize <= Stream.Size);

 // read fx magic
 Stream.Read(ChunkName, 4);

 UseChunk := (ChunkName[1] <> 'x');
 Stream.Seek(0, 0);

 if UseChunk then
  begin
   assert(effFlagsProgramChunks in EffectOptions);
   Stream.Read(FXChunkBank, SizeOf(TFXChunkBank) - SizeOf(Pointer));
   assert(FXChunkBank.chunkMagic = 'CcnK');
   assert(FXChunkBank.fxMagic = 'FBCh');

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
   assert(FXSet.chunkMagic = 'CcnK');
   assert(FXSet.fxMagic = 'FxBk');

   // swap
   SwapLong(FXSet.fxId);
   if FXSet.fxId <> FVstEffect^.UniqueID
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
     SetCurrentProgram(i);
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
 if not Assigned(FVstEffect) then exit;

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
 if not Assigned(FVstEffect) then exit;
 Stream.Seek(0, 0);
 if effFlagsProgramChunks in EffectOptions then
  with FXChunkBank do
   begin
    chunkMagic    := 'CcnK';
    fxMagic       := 'FBCh';
    version       := 1;
    fxID          := FVstEffect^.UniqueID;
    fxVersion     := FVstEffect^.version;
    numPrograms   := FVstEffect^.numPrograms;

    ChunkDataSize := GetChunk(@ChunkData, False);
    ChunkSize     := ChunkDataSize;
    ByteSize      := SizeOf(FXChunkBank) - SizeOf(LongInt) * 3 + chunkSize + 8;

    // swap-o-matic
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
    chunkMagic    := 'CcnK';
    fxMagic       := 'FxBk';
    version       := 1;
    fxID          := FVstEffect^.UniqueID;
    fxVersion     := FVstEffect^.version;
    numPrograms   := FVstEffect^.numPrograms;
    ByteSize      := SizeOf(FXSet) - SizeOf(LongInt) + (SizeOf(TFXPreset) + (numParams - 1) * SizeOf(Single)) * numPrograms - 8;

    // swap-o-matic
    SwapLong(version);
    SwapLong(fxID);
    SwapLong(fxVersion);
    SwapLong(numPrograms);
    SwapLong(ByteSize);

    Stream.WriteBuffer(FXSet, SizeOf(FXSet) - SizeOf(Single));
    for PrgNo := 0 to Self.numPrograms - 1 do
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

function TCustomVstPlugIn.GetInitialDelay: Integer;
begin
 if Assigned(FVstEffect)
  then Result := FVstEffect.initialDelay
  else Result := 0;
end;

{$IFDEF VstHostGUI}
procedure TCustomVstPlugIn.SetGUIStyle(const Value: TGUIStyle);
begin
 if FEditOpen
  then raise Exception.Create(RStrCloseEditorFirst)
  else FGUIStyle := Value;
end;
{$ENDIF}
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////

initialization
  audioMaster := AudioMasterCallback;
  {$IFDEF SearchPluginAndHost}
  GHostList    := TObjectList.Create(False);
  {$ENDIF}
  {$IFDEF VstHostGUI}
  GHostWindows := TObjectList.Create;
  {$ENDIF}

finalization
  {$IFDEF VstHostGUI}
  FreeAndNil(GHostWindows);
  {$ENDIF}
  {$IFDEF SearchPluginAndHost}
  FreeAndNil(GHostList);
  {$ENDIF}

end.