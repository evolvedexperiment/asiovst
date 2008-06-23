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

unit DVSTHost;

{$I ASIOVST.INC}
{-$DEFINE SB}
{$IFNDEF FPC}
{$R DVstHost.res}
{$ENDIF}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, Dynlibs, {$ELSE} Windows, Messages, {$ENDIF}
  {$IFDEF MSWINDOWS} Registry, {$ENDIF} SysUtils, Classes, Graphics, Controls,
  Forms, DAVDCommon, DVSTEffect, Dialogs, StdCtrls, ComCtrls
  {$IFDEF SB}, TFlatScrollbarUnit{$ENDIF};

type
  TVendorSpecificEvent = function(opcode : TAudioMasterOpcode; index, value: longint; ptr: pointer; opt: Single): integer of object;
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
  TKnobMode = (knCircular, knCircularRelativ, knLinear);

  TReplaceOrAccumulate = (roa0NotSupported, roa1Replace, roa2Accumulate);
  TCurrentProcessLevel = (cpl0NotSupported, cpl1UserThread, cpl2AudioThread,
   cpl3Sequencer, cpl4OfflineProcessing);
  TAutomationState = (as0NotSupported, as1Off, as2Read, as3Write, as4ReadWrite);
  THostCanDos = set of THostCanDo;

  TVstPlugIn = class(TCollectionItem)
  private
    FDLLHandle          : THandle;
    FDisplayName        : string;
    FMainFunction       : TMainProc;
    FEditOpen           : boolean;
    FWantMidi           : boolean;
    FNeedIdle           : boolean;
    FActive             : boolean;
    FnumInputs          : Integer;
    FnumOutputs         : Integer;
    FnumPrograms        : Integer;
    FnumParams          : Integer;
    Fversion            : Integer;
    FProgramNr          : Integer;
    FuID                : string;
    FVstVersion         : Integer;
    FPlugCategory       : TVstPluginCategory;
    FDLLFileName        : TFileName;
    FVstOfflineTask     : TVstOfflineTask;
    FReplaceOrAccumulate: TReplaceOrAccumulate;
    FProcessLevel       : TCurrentProcessLevel;
    FAutomationState    : TAutomationState;
    FOnAfterLoad        : TNotifyEvent;
    FOnShowEdit         : TVstShowEditEvent;
    FOnCloseEdit        : TNotifyEvent;
    FOnProcessEvents    : TVstProcessEventsEvent;
    FOnAMAutomate       : TVstAutomateEvent;
    FOnAMIdle           : TNotifyEvent;
    FOnAMNeedIdle       : TNotifyEvent;
    FOnAMWantMidi       : TNotifyEvent;
    FOnAMIOChanged      : TNotifyEvent;
    FOnAMOfflineStart   : TNotifyEvent;
    FOnAMOfflineRead    : TVstOfflineEvent;
    FOnAMOfflineWrite   : TVstOfflineEvent;
    FOnAMOfflineGetCurrentPass       : TNotifyEvent;
    FOnAMOfflineGetCurrentMetaPass   : TNotifyEvent;
    FOnAMSetOutputSampleRate         : TVstSampleRateChangedEvent;
    FOnAMUpdateDisplay  : TNotifyEvent;
    FOnAMBeginEdit      : TVstAutomationNotifyEvent;
    FOnAMEndEdit        : TVstAutomationNotifyEvent;
    FOnAMPinConnected   : TVstPinConnectedEvent;
    FOnVendorSpecific   : TVendorSpecificEvent;
    FGUIFormCreated     : Boolean;
    FGUIStyle           : TGUIStyle;
    function VstDispatch(opCode : TDispatcherOpcode; Index: Integer = 0; value: Integer = 0; pntr: pointer = nil; opt: double = 0): Integer; {overload;} //virtual;
    procedure Activate(b: Boolean);
    procedure onFormClose(Sender: TObject; var Action: TCloseAction);
    procedure ParamChange(Sender: TObject);
    procedure onEditActivate(Sender: TObject);
    procedure onEditDeactivate(Sender: TObject);
    procedure SetBlockSize(value: Integer);
    procedure SetDLLFileName(VstFilename: TFilename);
    function GetEntryPoints(theDll: TFileName): integer;
    function GetPreset(i: integer): TFXPreset;
    function GetInitialDelay: Integer;
    function GetEffOptions: TEffFlags;
    function GetnumInputs: Integer;
    function GetnumOutputs: Integer;
    function GetnumParams: Integer;
    function GetnumPrograms: Integer;
    {$IFDEF SB}
    procedure ScrollChange(Sender: TObject; ScrollPos: Integer);
    {$ELSE}
    procedure TrackChange(Sender: TObject);
    procedure SetGUIStyle(const Value: TGUIStyle);
    procedure ListParamChange(Sender: TObject);
    {$ENDIF}
  public
    PVstEffect          : PVSTEffect;
    GUIForm             : TForm;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetDisplayName: string; override;
    function GetFriendlyNameString(const StringLength: Integer): string;
    function GetVendorString: string;
    function GetProductString: string;
    function GetVendorVersion: Integer;
    function GetProgram: Integer;
    procedure SetProgram(lValue: Integer);
    function GetProgramName: string;
    procedure SetProgramName(newName: string);
    procedure SetSampleRate(fSR: double);
    procedure Open;
    procedure Close;
    function Load(PluginDll: TFilename): Boolean;
    procedure UnLoad;
    procedure ShowEdit(Form: TForm); overload;
    procedure ShowEdit; overload;
    function EditGetRect: ERect;
    function EditOpen(Handle: THandle): Integer;
    procedure EditClose;
    procedure CloseEdit;
    procedure MainsChanged(bOn: boolean);
    procedure SetParameter(index: Integer; parameter: Single); virtual;
    function GetParameter(index: Integer): Single; virtual;
    function GetParamLabel(index: Integer): string;
    function GetParamDisplay(index: Integer): string;
    function GetParamName(index: Integer): string;
    function GetVu: Single;
    function Identify: Integer;
    procedure Process(inputs, outputs: ppSingle; Sampleframes: Integer); virtual;
    procedure ProcessReplacing(inputs, outputs: ppSingle; Sampleframes: Integer); virtual;
    procedure ProcessDoubleReplacing(inputs, outputs: ppDouble; Sampleframes: Integer); virtual;
    procedure ProcessAudio(inputs, outputs: ppSingle; Sampleframes: Integer);
    function GetChunk(pntr: pointer; isPreset: boolean = false): Integer;
    function SetChunk(data: pointer; byteSize: Integer; isPreset: boolean = false): Integer;
    function ProcessEvents(pntr: PVstEvents): Integer;
    function CanBeAutomated(index: Integer): Integer;
    function String2Parameter(ParameterName: string): Integer;
    function GetNumProgramCategories: Integer;
    function GetProgramNameIndexed(category, index: Integer; ProgramName: PChar): Integer;
    function CopyCurrentProgramTo(Destination: Integer): Boolean;
    function ConnectInput(InputNr: Integer; state: boolean): Integer;
    function ConnectOutput(OutputNr: Integer; state: boolean): Integer;
    function GetInputProperties(InputNr: Integer): TVstPinProperties;
    function GetOutputProperties(OutputNr: Integer): TVstPinProperties;
    function GetPlugCategory: TVstPluginCategory;
    function GetCurrentPosition: Integer;
    function GetDestinationBuffer: Integer;
    function OfflineNotify(pntr: PVstAudioFile; numAudioFiles: Integer; start: boolean): Integer;
    function OfflinePrepare(pntr: PVstOfflineTask; count: Integer): Integer;
    function OfflineRun(pntr: PVstOfflineTask; count :Integer): Integer;
    function ProcessVarIo(varIo: PVstVariableIo): Integer;
    function SetSpeakerArrangement(pluginInput: PVstSpeakerArrangement; pluginOutput: PVstSpeakerArrangement): Boolean;
    function SetBlockSizeAndSampleRate(blockSize: Integer; sampleRate: Single): Integer;
    function SetBypass(onOff: Boolean): Integer;
    function GetEffectName: string;
    function GetErrorText: string;
    function GetTailSize: Integer;
    function GetIcon: Integer;
    function GetVstVersion: Integer;
    function GetSpeakerArrangement(SpeakerIn, SpeakerOut:PVstSpeakerArrangement): Integer;
    function ShellGetNextPlugin(var PluginName:String): Integer;
    function EditKeyDown(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
    function EditKeyUp(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
    procedure SetEditKnobMode(Mode : TKnobMode);
    procedure StartProcess;
    procedure StopProcess;
    procedure SetTotalSampleToProcess;
    procedure BeginSetProgram;
    procedure EndSetProgram;
    procedure SetPanLaw(PanLaw: TVstPanLawType; Gain: Single);
    function GetMidiProgramName(MidiProgramNamePointer : PMidiProgramName): Integer;
    function GetCurrentMidiProgram(MidiProgramNamePointer : PMidiProgramName): Integer;
    function GetMidiProgramCategory(MidiProgramCategoryPointer : PMidiProgramCategory): Integer;
    function HasMidiProgramsChanged: Integer;
    function GetMidiKeyName(MidiKeyNamePointer: PMidiKeyName): Integer;
    function BeginLoadBank(PatchChunkInfo : PVstPatchChunkInfo): integer;
    procedure BeginLoadProgram(PatchChunkInfo : PVstPatchChunkInfo);
    function GetRect: TRect;
    function GetParameterProperties(Parameter: Integer): TVstParameterProperties;
    function VendorSpecific(index, value:Integer; pntr: pointer; opt: single): Integer;
    function CanDo(pntr: pchar): Integer;
    function Idle: Integer;
    function EditIdle: Integer;
    procedure EditActivate;
    procedure EditDeactivate;
    procedure SetViewPosition(x, y: Integer);
    function KeysRequired: Integer;
    procedure SavePreset(FileName: TFileName); overload;
    procedure SavePreset(Stream: TStream); overload;
    procedure LoadPreset(FileName: TFileName); overload;
    procedure LoadPreset(Stream: TStream); overload;
    procedure SaveBank(FileName: TFileName); overload;
    procedure SaveBank(Stream: TStream); overload;
    procedure LoadBank(FileName: TFileName); overload;
    procedure LoadBank(Stream: TStream); overload;
    property Parameters[Index: Integer]:Single read GetParameter write SetParameter;
    property VstOfflineTask : TVstOfflineTask read FVstOfflineTask;
  published
    property Active: boolean read FActive write Activate default false;
    property DisplayName: string read GetDisplayName write FDisplayName;
    property numInputs: Integer read GetnumInputs stored False default -1 ;
    property numOutputs: Integer read GetnumOutputs stored False default -1 ;
    property numPrograms: Integer read GetnumPrograms stored False default -1 ;
    property numParams: Integer read GetnumParams stored False default -1;
    property Version: Integer read Fversion stored False default -1;
    property InitialDelay: Integer read GetInitialDelay stored False;
    property ReplaceOrAccumulate: TReplaceOrAccumulate read FReplaceOrAccumulate write FReplaceOrAccumulate default roa0NotSupported;
    property CurrentProcessLevel: TCurrentProcessLevel read FProcessLevel write FProcessLevel default cpl0NotSupported;
    property AutomationState: TAutomationState read FAutomationState Write FAutomationState default as0NotSupported;
    property uID: string read FuID stored False;
    property PluginVstVersion: Integer read FVstVersion stored False default -1;
    property EditVisible: Boolean read FEditOpen;
    property EffectOptions: TEffFlags read GetEffOptions stored False;
    property PlugCategory: TVstPluginCategory read FPlugCategory stored False;
    property ProgramNr: Integer read GetProgram write SetProgram default -1;
    property ProgramName: string read GetProgramName write SetProgramName;
    property VendorString: string read GetVendorString stored False;
    property VendorVersion: Integer read GetVendorVersion stored False default -1;
    property ProductString: string read GetProductString stored False;
    property OnAudioMasterAutomate: TVstAutomateEvent read FOnAMAutomate write FOnAMAutomate;
    property OnAudioMasterIdle: TNotifyEvent read FOnAMIdle write FOnAMIdle;
    property OnAudioMasterNeedIdle: TNotifyEvent read FOnAMNeedIdle write FOnAMNeedIdle;
    property OnAudioMasterIOChanged: TNotifyEvent read FOnAMIOChanged write FOnAMIOChanged;
    property OnAudioMasterWantMidi: TNotifyEvent read FOnAMWantMidi write FOnAMWantMidi;
    property OnAudioMasterOfflineStart: TNotifyEvent read FOnAMOfflineStart write FOnAMOfflineStart;
    property OnAudioMasterOfflineRead: TVstOfflineEvent read FOnAMOfflineRead write FOnAMOfflineRead;
    property OnAudioMasterOfflineWrite: TVstOfflineEvent read FOnAMOfflineWrite write FOnAMOfflineWrite;
    property OnAudioMasterOfflineGetCurrentPass: TNotifyEvent read FOnAMOfflineGetCurrentPass write FOnAMOfflineGetCurrentPass;
    property OnAudioMasterOfflineGetCurrentMetaPass: TNotifyEvent read FOnAMOfflineGetCurrentMetaPass write FOnAMOfflineGetCurrentMetaPass;
    property OnAudioMasterSetOutputSampleRate: TVstSampleRateChangedEvent read FOnAMSetOutputSampleRate write FOnAMSetOutputSampleRate;
    property OnAudioMasterUpdateDisplay: TNotifyEvent read FOnAMUpdateDisplay write FOnAMUpdateDisplay;
    property OnAudioMasterBeginEdit: TVstAutomationNotifyEvent read FOnAMBeginEdit write FOnAMBeginEdit;
    property OnAudioMasterEndEdit: TVstAutomationNotifyEvent read FOnAMEndEdit write FOnAMEndEdit;
    property OnAudioMasterPinConnected: TVstPinConnectedEvent read FOnAMPinConnected write FOnAMPinConnected;
    property OnVendorSpecific: TVendorSpecificEvent read FOnVendorSpecific write FOnVendorSpecific;
    property OnShowEdit: TVstShowEditEvent read FOnShowEdit write FOnShowEdit;
    property OnCloseEdit: TNotifyEvent read FOnCloseEdit write FOnCloseEdit;
    property OnAfterLoad: TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnProcessEvents: TVstProcessEventsEvent read FOnProcessEvents write FOnProcessEvents;
    property DLLFileName: TFileName read FDLLFileName write SetDLLFileName;
    property GUIStyle : TGUIStyle read fGUIStyle write SetGUIStyle default gsDefault;
  end;

  TVstPlugIns = class(TOwnedCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TVstPlugIn;
    procedure SetItem(Index: Integer; const Value: TVstPlugIn);
  protected
    property Items[Index: Integer]: TVstPlugIn read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TVstPlugIn;
    function CloneAdd(Source: TVstPlugIn): TVstPlugIn;
    function Insert(Index: Integer): TVstPlugIn;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TVstTimeInformation = class(TPersistent)
  private
    FOnChange              : TNotifyEvent;
    function GetVTI(Index :Integer) : Integer;
    function GetVTIdouble(Index :Integer) : Double;
    function GetVTIflags :TVstTimeInfoFlags;
    procedure SetVTI(Index,Value :Integer);
    procedure SetVTIdouble(Index :Integer; Value: Double);
    procedure SetVTIflags(Flags:TVstTimeInfoFlags);
  protected
    fVstTimeInfo        : TVstTimeInfo;
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    constructor Create;
  published
    property SamplePos: Double index 0 read GetVTIdouble write SetVTIdouble;
    property SampleRate: Double Index 1 read GetVTIdouble write SetVTIdouble;
    property NanoSeconds: Double Index 2 read GetVTIdouble write SetVTIdouble;
    property PpqPos: Double Index 3 read GetVTIdouble write SetVTIdouble;
    property Tempo: Double Index 4 read GetVTIdouble write SetVTIdouble;
    property BarStartPos: Double Index 5 read GetVTIdouble write SetVTIdouble;
    property CycleStartPos: Double Index 6 read GetVTIdouble write SetVTIdouble;
    property CycleEndPos: Double Index 7 read GetVTIdouble write SetVTIdouble;
    property TimeSigNumerator: Longint Index 0 read GetVTI write SetVTI default 4;
    property TimeSigDenominator: longint Index 1 read GetVTI write SetVTI default 4;
    property SmpteOffset: longint index 2 read GetVTI write SetVTI default 0;
    property SmpteFrameRate: longint index 3 read GetVTI write SetVTI default 1;
    property SamplesToNextClock: longint index 4 read GetVTI write SetVTI default 0;
    property Flags: TVstTimeInfoFlags read GetVTIflags Write SetVTIflags;
  end;

  TVstHost = class(TComponent)
  private
    FInputLatency: Integer;
    FOutputLatency: Integer;
    FVstPlugIns: TVstPlugIns;
    FLanguage: TVstHostLanguage;
    FnumAutomatable: Integer;
    FParamQuan: Integer;
    FVendorString: string;
    FVendorVersion: Integer;
    FProductString: string;
    FPlugInDir: string;
    FVTI: TVstTimeInformation;
    FAutoIdle: Boolean;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    procedure VstTimeInfoChanged(Sender: TObject);
    function GetItem(Index: Integer): TVstPlugIn;
    procedure SetVstPlugIns(const Value: TVstPlugIns);
    function getHostVersion: Integer;
    procedure setHostVersion(hv: Integer);
    function getHostCanDos: THostCanDos;
    procedure setHostCanDos(hcd: THostCanDos);
    function getHostTempo: Single;
    procedure setHostTempo(Tempo: Single);
    function getBlockSize : Integer;
    procedure setBlockSize(bs: Integer);
  protected
    property Items[Index: Integer]: TVstPlugIn read GetItem; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateVstTimeInfo(samples: word = 1);
    procedure ResetVstTimeInformation;
  published
    property LatencyInput: Integer read FInputLatency write FInputLatency default 0;
    property LatencyOutput: Integer read FOutputLatency write FOutputLatency default 0;
    property VstPlugIns: TVstPlugIns read FVstPlugIns write SetVstPlugIns;
    property Language: TVstHostLanguage read FLanguage write FLanguage default kVstLangEnglish;
    property NumAutomatableParameters : Integer read FnumAutomatable write FnumAutomatable default 0;
    property ParameterQuantization : Integer read FParamQuan write FParamQuan default MAXINT;
    property BlockSize: Integer read getBlockSize write setBlocksize default 2048;
    property CanDos: THostCanDos read getHostCanDos write setHostCanDos;
    property ManageIdleAutomaticly : Boolean read FautoIdle write FautoIdle;
    property Tempo: Single read getHostTempo write SetHostTempo;
    property VstVersion: Integer read getHostVersion write setHostVersion;
    property VendorString: string read FVendorString write FVendorString;
    property VendorVersion: Integer read FVendorVersion write FVendorVersion;
    property ProductString: string read FProductString write FProductString;
    property PlugInDir: string read FPlugInDir write FPlugInDir;
    property VstTimeInfo : TVstTimeInformation read FVTI write FVTI;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

var audioMaster : TAudioMasterCallbackFunc;

procedure Register;
function string2Language(LanguageString : string): TVstHostLanguage;
function PlugCategory2String(Category:TVstPluginCategory):string;
function EffOptions2String(EffOpts: TEffFlags):string;

implementation

uses contnrs;

var FBlockSize     : Integer = 2048;
    FHostVersion   : Integer = 2300;
    FHostCanDos    : THostCanDos;
    FHostTempo     : Single = 120;
    FSampleRate    : Single  = 44100;
    theHost        : TVstHost;
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
 raise Exception.Create('This is not a valid Vst Plugin!');
 result := 1;
end;

function AudioMasterCallback(effect: PVSTEffect; opcode : TAudioMasterOpcode; index,value: longint; ptr: pointer; opt: Single): longint; cdecl;
var thePlug  : TVstPlugin;
    PlugNr,i : Integer;
begin
 try
   thePlug := nil;
   for PlugNr := 0 to theHost.VstPlugIns.Count - 1 do
    if theHost.VstPlugIns[PlugNr].PVstEffect = effect then
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
                                               thePlug.FNeedIdle:=True;
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
                                                  then Result:=0
                                                  else Result:=1;
                                                end
                                               else result := 0
                                              else result := 0;
    audioMasterWantMidi                    : if Assigned(thePlug) then
                                              if Assigned(thePlug.FOnAMWantMidi)
                                               then thePlug.FOnAMWantMidi(thePlug);
    audioMasterGetTime                     : Result := Longint(@theHost.VstTimeInfo.FVstTimeInfo);
    audioMasterProcessEvents               : begin
                                               if Assigned(thePlug.FOnProcessEvents)
                                                then thePlug.FOnProcessEvents(thePlug, ptr);
                                             end;
    audioMasterSetTime                     : {$IFDEF Debug} Exception.Create('TODO: audioMasterSetTime, VstTimenfo* in <ptr>, filter in <value>, not supported') {$ENDIF Debug};
    audioMasterTempoAt                     : result := round(FHostTempo) * 10000;
    audioMasterGetNumAutomatableParameters : result := theHost.FnumAutomatable;
    audioMasterGetParameterQuantization    : if Value=-1
                                              then result := theHost.FParamQuan
                                              else {$IFDEF Debug} Exception.Create('TODO: audioMasterGetParameterQuantization, returns the integer value for +1.0 representation') {$ENDIF Debug};
                                              // or 1 if full single float precision is maintained
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
                                               thePlug.FNeedIdle := true;
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
                                              if PlugNr=0 then Result:=0;
                                              {$IFDEF Debug} Exception.Create('TODO: audioMasterGetPreviousPlug, input pin in <value> (-1: first to come), returns cEffect*') {$ENDIF Debug};
                                             end;
    audioMasterGetNextPlug                 : {$IFDEF Debug}  Exception.Create('TODO: audioMasterGetNextPlug, output pin in <value> (-1: first to come), returns cEffect*') {$ENDIF Debug};
    audioMasterWillReplaceOrAccumulate     : if thePlug<>nil then result := Integer(thePlug.FReplaceOrAccumulate) else result := 0;
    audioMasterGetCurrentProcessLevel      : if thePlug<>nil then result := Integer(thePlug.FProcessLevel) else result := 0;
    audioMasterGetAutomationState          : if thePlug<>nil then result := Integer(thePlug.FAutomationState) else result := 0;
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
    audioMasterSetOutputSampleRate         : begin
                                              if Assigned(thePlug) then
                                               if Assigned(thePlug.FOnAMSetOutputSampleRate)
                                                then thePlug.FOnAMSetOutputSampleRate(thePlug,opt);
                                              FSampleRate := opt; // Exception.Create('audioMasterSetOutputSampleRate, for variable i/o, sample rate in <opt>');
                                             end;
    audioMasterGetOutputSpeakerArrangement : {$IFDEF Debug} Exception.Create('TODO: audioMasterGetSpeakerArrangement, (long)input in <value>, output in <ptr>') {$ENDIF Debug};
    audioMasterGetVendorString             : StrCopy(pchar(ptr),PChar(theHost.VendorString));
    audioMasterGetProductString            : StrCopy(pchar(ptr),PChar(theHost.ProductString));
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
                                              if shortstring(PChar(ptr))='sendVstEvents' then Result := Integer(hcdSendVstEvents in FHostCanDos)
                                              else if shortstring(PChar(ptr))='sendVstMidiEvent' then Result := Integer(hcdSendVstMidiEvent in FHostCanDos)
                                              else if shortstring(PChar(ptr))='sendVstTimeInfo' then Result := Integer(hcdSendVstTimeInfo in FHostCanDos)
                                              else if shortstring(PChar(ptr))='receiveVstEvents' then Result := Integer(hcdReceiveVstEvents in FHostCanDos)
                                              else if shortstring(PChar(ptr))='receiveVstMidiEvent' then Result := Integer(hcdReceiveVstMidiEvent in FHostCanDos)
                                              else if shortstring(PChar(ptr))='receiveVstTimeInfo' then Result := Integer(hcdReceiveVstTimeInfo in FHostCanDos)
                                              else if shortstring(PChar(ptr))='reportConnectionChanges' then Result := Integer(hcdReportConnectionChanges in FHostCanDos)
                                              else if shortstring(PChar(ptr))='acceptIOChanges' then Result := Integer(hcdAcceptIOChanges in FHostCanDos)
                                              else if shortstring(PChar(ptr))='sizeWindow' then Result := Integer(hcdSizeWindow in FHostCanDos)
                                              else if shortstring(PChar(ptr))='asyncProcessing' then Result := Integer(hcdAsyncProcessing in FHostCanDos)
                                              else if shortstring(PChar(ptr))='offline' then Result := Integer(hcdOffline in FHostCanDos)
                                              else if shortstring(PChar(ptr))='supplyIdle' then Result := Integer(hcdSupplyIdle in FHostCanDos)
                                              else if shortstring(PChar(ptr))='supportShell' then Result := Integer(hcdSupportShell in FHostCanDos)
                                              else if shortstring(PChar(ptr))='openFileSelector' then Result := Integer(hcdOpenFileSelector in FHostCanDos)
                                              else if shortstring(PChar(ptr))='closeFileSelector' then Result := Integer(hcdcloseFileSelector in FHostCanDos)
                                              else if shortstring(PChar(ptr))='editFile' then Result := Integer(hcdEditFile in FHostCanDos)
                                              else if shortstring(PChar(ptr))='shellCategory' then Result := Integer(hcdShellCategory in FHostCanDos)
                                              else if shortstring(PChar(ptr))='startStopProcess' then Result := Integer(hcdStartStopProcess in FHostCanDos)
                                              else Result := 0;
                                             end;
    audioMasterGetLanguage                 : result := Integer(theHost.FLanguage);
    audioMasterOpenWindow                  : if ptr<>nil then
                                              begin
                                               i:=HostWindows.Add(TForm.Create(theHost));
                                               (HostWindows.Items[i] As TForm).Caption:=PVstWindow(ptr).title;
                                               (HostWindows.Items[i] As TForm).Left:=PVstWindow(ptr).xPos;
                                               (HostWindows.Items[i] As TForm).Top:=PVstWindow(ptr).xPos;
                                               (HostWindows.Items[i] As TForm).Width:=PVstWindow(ptr).Width;
                                               (HostWindows.Items[i] As TForm).Height:=PVstWindow(ptr).Height;
                                               case PVstWindow(ptr).Style of
                                                0: (HostWindows.Items[i] As TForm).BorderStyle:=bsSizeToolWin;
                                                1: (HostWindows.Items[i] As TForm).BorderStyle:=bsNone;
                                               end;
                                               (HostWindows.Items[i] As TForm).Parent:=PVstWindow(ptr).Parent;
                                               ShowMessage('Please contact me if this happens: Christian@savioursofsoul.de');
//                                               PVstWindow(ptr).winHandle:=(HostWindows.Items[i] As TForm).Handle;
                                              end;
    audioMasterCloseWindow                 : begin
                                              {$IFDEF Debug}
                                              showmessage('TODO: audioMasterCloseWindow, ' +
                                              'close window, platform specific handle in <ptr>')
                                              {$ENDIF Debug};
                                             end;
    audioMasterGetDirectory                : result := longint(PChar(theHost.FPlugInDir));
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
                                                   TOpenDialog(HostDialog).Name := 'HostDialog';
                                                   TOpenDialog(HostDialog).Title := PVstFileSelect(ptr).title;
                                                   TOpenDialog(HostDialog).InitialDir := PVstFileSelect(ptr).initialPath;
                                                   for i:=0 to PVstFileSelect(ptr).nbFileTypes - 1
                                                    do TOpenDialog(HostDialog).Filter := TOpenDialog(HostDialog).Filter+ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name)+' (*.'+PVstFileType(PVstFileSelect(ptr).fileTypes).dosType+')|*.'+PVstFileType(PVstFileSelect(ptr).fileTypes).dosType+'|';
                                                   if TOpenDialog(HostDialog).Execute
                                                    then
                                                     begin
//                                                      PVstFileSelect(ptr).returnPath:=PChar(TOpenDialog(HostDialog).FileName);
//                                                      StrCopy(PVstFileSelect(ptr).returnPath,PChar(TOpenDialog(HostDialog).FileName));
                                                      PVstFileSelect(ptr).sizeReturnPath := Length(TOpenDialog(HostDialog).FileName);
                                                     end;
                                                  end;
                                                 kVstFileSave:
                                                  begin
                                                   HostDialog := TSaveDialog.Create(theHost);
                                                   TSaveDialog(HostDialog).Name := 'HostDialog';
                                                   TSaveDialog(HostDialog).Title := PVstFileSelect(ptr).title;
                                                   TSaveDialog(HostDialog).InitialDir := PVstFileSelect(ptr).initialPath;
                                                   for i := 0 to PVstFileSelect(ptr).nbFileTypes - 1 do
                                                    TSaveDialog(HostDialog).Filter := TSaveDialog(HostDialog).Filter +
                                                     ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name) +
                                                     ' (*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType +
                                                     ')|*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType + '|';
                                                   if TSaveDialog(HostDialog).Execute then
                                                   begin
                                                    PVstFileSelect(ptr).returnPath := PChar(TSaveDialog(HostDialog).FileName);
                                                    PVstFileSelect(ptr).sizeReturnPath := Length(TSaveDialog(HostDialog).FileName);
                                                   end;
                                                  end;
                                                 kVstMultipleFilesLoad:
                                                 begin
                                                  HostDialog := TOpenDialog.Create(theHost);
                                                  TOpenDialog(HostDialog).Name := 'HostDialog';
                                                  TOpenDialog(HostDialog).Title := PVstFileSelect(ptr).title;
                                                  TOpenDialog(HostDialog).InitialDir := PVstFileSelect(ptr).initialPath;
                                                  for i := 0 to PVstFileSelect(ptr).nbFileTypes - 1 do
                                                   TOpenDialog(HostDialog).Filter := TOpenDialog(HostDialog).Filter +
                                                   ShortString(PVstFileType(PVstFileSelect(ptr).fileTypes).name) +
                                                   ' (*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType +
                                                   ')|*.' + PVstFileType(PVstFileSelect(ptr).fileTypes).dosType + '|';
                                                   if TOpenDialog(HostDialog).Execute
                                                    then
                                                     begin
                                                      PVstFileSelect(ptr).returnPath := PChar(TOpenDialog(HostDialog).FileName);
                                                      PVstFileSelect(ptr).sizeReturnPath := Length(TOpenDialog(HostDialog).FileName);
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
                                                  HostDialog:=nil;
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
     audioMasterEditFile                   : {$IFDEF Debug} Exception.Create('TODO: open an editor for audio (defined by XML text in ptr') {$ENDIF Debug};
     audioMasterGetChunkFile               : {$IFDEF Debug} Exception.Create('TODO: get the native path of currently loading bank or project') {$ENDIF Debug};
    else
     try
       raise Exception.Create('Check: '+inttostr(Integer(opcode))+' - '+inttostr(index)+' - '+inttostr(value)+' - '+floattostr(opt));
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

constructor TVstTimeInformation.Create;
begin
 with FVstTimeInfo do
  begin
   SampleRate :=  44100;
   timeSigNumerator := 4;
   timeSigDenominator := 4;
   smpteFrameRate := 1;
   samplePos := 0;
   ppqPos := 0;
  end;
 Flags := [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid,
           vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid];
end;

procedure TVstTimeInformation.Change;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TVstTimeInformation.AssignTo(Dest: TPersistent);
begin
 if Dest is TVstTimeInformation then
   with TVstTimeInformation(Dest) do
   try
     fVstTimeInfo := Self.FVstTimeInfo;
   finally  
     Change;
   end
 else inherited AssignTo(Dest);
end;

function TVstTimeInformation.GetVTIflags :TVstTimeInfoFlags;
begin
 result := fVstTimeInfo.Flags;
end;

function TVstTimeInformation.GetVTIdouble(Index :Integer): Double;
begin
 Result := 0;
 case Index of
  0: Result := FVstTimeInfo.samplePos;
  1: Result := FVstTimeInfo.sampleRate;
  2: Result := FVstTimeInfo.nanoSeconds;
  3: Result := FVstTimeInfo.ppqPos;
  4: Result := FVstTimeInfo.tempo;
  5: Result := FVstTimeInfo.barStartPos;
  6: Result := FVstTimeInfo.cycleStartPos;
  7: Result := FVstTimeInfo.cycleEndPos;
 end;
end;

function TVstTimeInformation.GetVTI(Index :Integer) :Integer;
begin
 Result := 0;
 case Index of
  0: Result := FVstTimeInfo.timeSigNumerator;
  1: Result := FVstTimeInfo.timeSigDenominator;
  2: Result := FVstTimeInfo.smpteOffset;
  3: Result := FVstTimeInfo.smpteFrameRate;
  4: Result := FVstTimeInfo.samplesToNextClock;
 end;
end;

procedure TVstTimeInformation.SetVTI(Index,Value :Integer);
begin
 case Index of
  0: if Value <> FVstTimeInfo.timeSigNumerator then begin FVstTimeInfo.timeSigNumerator := Value; Change; end;
  1: if Value <> FVstTimeInfo.timeSigDenominator then begin FVstTimeInfo.timeSigDenominator := Value; Change; end;
  2: if Value <> FVstTimeInfo.smpteOffset then begin FVstTimeInfo.smpteOffset := Value; Change; end;
  3: if Value <> FVstTimeInfo.smpteFrameRate then begin FVstTimeInfo.smpteFrameRate := Value; Change; end;
  4: if Value <> FVstTimeInfo.samplesToNextClock then begin FVstTimeInfo.samplesToNextClock := Value; Change; end;
 end;
end;

procedure TVstTimeInformation.SetVTIdouble(Index :Integer; Value: Double);
begin
 case Index of
  0: if Value <> FVstTimeInfo.samplePos then begin FVstTimeInfo.SamplePos := Value; Change; end;
  1: if Value <> FVstTimeInfo.sampleRate then begin FVstTimeInfo.sampleRate := Value; FsampleRate := Value; Change; end;
  2: if Value <> FVstTimeInfo.nanoSeconds then begin FVstTimeInfo.nanoSeconds := Value; Change; end;
  3: if Value <> FVstTimeInfo.ppqPos then begin FVstTimeInfo.ppqPos := Value; Change; end;
  4: if Value <> FVstTimeInfo.tempo then begin FVstTimeInfo.tempo := Value; Change; end;
  5: if Value <> FVstTimeInfo.barStartPos then begin FVstTimeInfo.barStartPos := Value; Change; end;
  6: if Value <> FVstTimeInfo.cycleStartPos then begin FVstTimeInfo.cycleStartPos := Value; Change; end;
  7: if Value <> FVstTimeInfo.cycleEndPos then begin FVstTimeInfo.cycleEndPos := Value; Change; end;
 end;
end;

procedure TVstTimeInformation.SetVTIflags(Flags:TVstTimeInfoFlags);
begin
 fVstTimeInfo.Flags := Flags;
end;

{ TVstHost }

constructor TVstHost.Create(AOwner: TComponent);
begin
 inherited;
// if AOwner <> nil then
 theHost := Self;
 FSampleRate := 44100;
 FBlocksize := 2048;
 FLanguage := kVstLangEnglish;
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

destructor TVstHost.Destroy;
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

procedure TVstHost.VstTimeInfoChanged(Sender: TObject);
begin
 //
end;

function TVstPlugIn.GetEffOptions: TEffFlags;
begin
 if Assigned(PVstEffect)
  then Result:=PVstEffect.EffectFlags
  else Result:=[];
end;

function TVstHost.GetItem(Index: Integer): TVstPlugIn;
begin
 Result := FVstPlugIns[Index];
end;

procedure TVstHost.SetVstPlugIns(const Value: TVstPlugIns);
begin
 FVstPlugIns.Assign(Value);
end;

function TVstHost.getHostTempo : Single;
begin
 Result := FHostTempo;
end;

procedure TVstHost.setHostTempo(tempo:Single);
begin
 FHostTempo := tempo;
 if Assigned(VstTimeInfo)
  then VstTimeInfo.tempo := Tempo;
end;

function TVstHost.getHostCanDos : THostCanDos;
begin
 Result := FHostCanDos;
end;

procedure TVstHost.setHostCanDos(hcd:THostCanDos);
begin
 FHostCanDos := hcd;
end;

function TVstHost.getHostVersion : Integer;
begin
 Result := FHostVersion;
end;

procedure TVstHost.setHostVersion(hv:Integer);
begin
 FHostVersion := hv;
end;

function TVstHost.getBlockSize : Integer;
begin
 Result := FBlockSize;
end;

procedure TVstHost.setBlockSize(bs:Integer);
var i : Integer;
begin
 FBlockSize := bs;
 for i := 0 to VstPlugIns.Count - 1 do
  if assigned( VstPlugIns[i].PVstEffect) then
   VstPlugIns[i].SetBlockSize(FBlockSize);
end;

procedure TVstHost.UpdateVstTimeInfo(samples: word = 1);
var p: double;
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

procedure TVstHost.ResetVstTimeInformation;
begin
 if Assigned(FVTI) then
  with FVTI do
   begin
    FVstTimeInfo.samplePos := 0;
    FVstTimeInfo.ppqPos := 0;
   end;
end;

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

////////////////////////////////////////////////////////////////////////////////

{ TVstPlugIn }

procedure TVstPlugin.AssignTo(Dest: TPersistent);
var p: pointer;
    i: Integer;
begin
 if Dest is TVstPlugin then with TVstPlugin(Dest) do
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
  OnAudioMasterSetOutputSampleRate := Self.OnAudioMasterSetOutputSampleRate;
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
  FProgramNr:=Self.ProgramNr;
  i:=Self.GetChunk(@p);
  TVstPlugin(Dest).SetChunk(p,i)
 end else
  inherited;
end;

function TVstPlugin.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

constructor TVstPlugIn.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName    := inherited GetDisplayName;
 FMainFunction   := nil;
 PVstEffect      := nil;
 FEditOpen       := false;
 FNeedIdle       := false;
 FWantMidi       := false;
 FVstVersion     := -1;
 FPlugCategory   := vpcUnknown;
 FDLLFileName    := '';
 FGUIStyle       := gsDefault;
 fGUIFormCreated := False;
end;

destructor TVstPlugin.Destroy;
begin
 if FDLLFilename <> '' then
 begin
  if EditVisible then CloseEdit;
  Unload;
  if (GUIForm <> nil) and fGUIFormCreated then
  try
   GUIForm.Free;
  finally
   GUIForm := nil;
  end;
 end;
 inherited;
end;

procedure TVstPlugin.Activate(b: Boolean);
begin
  if FActive<>b then
  if b then Open else Close;
end;

procedure TVstPlugin.Open;
var i      : Integer;
    loadOK : Boolean;
    tmp    : string;
    sl     : TStringList;
begin
 loadOK:=True;
 if not Assigned(PVstEffect)
  then loadOK := Load(FDLLFileName)
  else FDLLFileName := '';
 asm
  fnclex                  // Don't raise pending exceptions enabled by the new flags
  fldcw   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled
 end;
 Randomize;
 FEditOpen := false;
 FNeedIdle := false;
 FWantMidi := false;
 if PVstEffect = nil then
  try
   if not loadOK then
    raise Exception.Create('This is not a valid Vst Plugin!')
   else
    raise Exception.Create('Loading failed!');
  except
   raise;
  end;
 if Longint(PVstEffect.Magic)<>FourCharToLong('V','s','t','P')
  then raise Exception.Create('There is no magic in it... failed!');
 if PVstEffect.uniqueID = 0 then
  begin
   sl:= TStringList.Create;
   while ShellGetNextPlugin(tmp)<>0 do
   sl.Add(tmp);
   sl.Free;
  end;

 VstDispatch(effOpen);
 CanDo('bypass');
 //setPanLaw(0,0.707107)
 SetSampleRate(FSampleRate);
 SetBlockSize(FBlocksize);
 SetBypass(false);
 FActive := true;
 FuID := ''; for i := 3 downto 0 do FuID := FuID + char(PVstEffect.uniqueID shr (i * 8));

 Fversion := PVstEffect.version;
 FVstVersion := GetVstVersion;
 FPlugCategory := GetPlugCategory;
 FnumInputs := PVstEffect.numInputs;
 FnumOutputs := PVstEffect.numOutputs;
 FnumPrograms := PVstEffect.numPrograms;
 FnumParams := PVstEffect.numParams;
 MainsChanged(True);
end;

procedure TVstPlugin.Close;
begin
 while FEditOpen do CloseEdit;
 if FActive then
  begin
   VstDispatch(effClose);
   PVstEffect:=nil;
  end;
 FActive := false;
 FVersion := 0;
 FPlugCategory := vpcUnknown;
 FnumInputs := 0;
 FnumOutputs := 0;
 FnumPrograms := 0;
 FnumParams := 0;
end;

function TVstPlugin.VstDispatch(opCode : TDispatcherOpcode; Index, Value: Integer; Pntr: pointer; opt: double): Integer;
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

procedure TVstPlugin.Process(Inputs, Outputs: ppSingle; Sampleframes:Integer);
begin
 if PVstEffect <> nil
  then PVstEffect.Process(PVstEffect, Inputs, Outputs, Sampleframes);
end;

procedure TVstPlugin.ProcessReplacing(Inputs, Outputs: ppSingle; Sampleframes:Integer);
begin
 if PVstEffect <> nil
  then PVstEffect.ProcessReplacing(PVstEffect, Inputs, Outputs, Sampleframes);
end;

procedure TVstPlugIn.ProcessDoubleReplacing(Inputs, Outputs: ppDouble; Sampleframes: Integer);
begin
 if PVstEffect <> nil
  then PVstEffect.ProcessDoubleReplacing(PVstEffect, Inputs, Outputs, Sampleframes);
end;

procedure TVstPlugin.SetParameter(index:Integer; parameter:Single);
begin
 if PVstEffect <> nil
  then PVstEffect.setParameter(PVstEffect, Index, Parameter);
end;

function TVstPlugin.GetParameter(Index:Integer):Single;
begin
 if PVstEffect = nil
  then result := 0
  else result := PVstEffect.getParameter(PVstEffect, Index);
end;

procedure TVstPlugin.SetProgram(lValue:Integer);
begin
 if FActive then
  begin
   FProgramNr := lValue;
   VstDispatch(effSetProgram, 0, FProgramNr);
  end;
end;

function TVstPlugin.GetProgram: Integer;
begin
 if FActive then result := VstDispatch(effGetProgram) else result := -1;
 FProgramNr := result;
end;

procedure TVstPlugin.SetProgramName(NewName:string);
begin
 if FActive then
  VstDispatch(effSetProgramName, 0, 0, PChar(NewName));
end;

function TVstPlugin.GetProgramName:string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 255);
   if VstDispatch(effGetProgramName, 0, 0, temp)= 0 then
    result := shortstring(temp);
   FreeMem(temp);
  end;
end;

function TVstPlugin.GetParamLabel(index:Integer): string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 255);
   if VstDispatch(effGetParamLabel, index, 0, temp)= 0 then
    result := shortstring(temp);
   FreeMem(temp);
  end;
end;

function TVstPlugin.GetParamDisplay(index:Integer): string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 255);
   if VstDispatch(effGetParamDisplay, index, 0, temp)= 0
    then result := shortstring(temp);
   FreeMem(temp);
  end;
end;

function TVstPlugin.GetParamName(index:Integer): string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 255);
   if VstDispatch(effGetParamName, index, 0, temp) = 0 then
    result := shortstring(temp);
   FreeMem(temp);
  end;
end;

procedure TVstPlugin.SetSampleRate(fSR: double);
begin
 VstDispatch(effSetSampleRate, 0, 0, nil, fSR);
end;

procedure TVstPlugin.SetBlockSize(value: Integer);
begin
 VstDispatch(effSetBlockSize, 0, value);
end;

procedure TVstPlugin.MainsChanged(bOn: boolean);
begin
 VstDispatch(effMainsChanged, 0, integer(bOn));
end;

function TVstPlugin.GetVu: Single;
const Divisor : Double = 1 / 32767;
begin
 if FActive then result := VstDispatch(effGetVu) * Divisor else result := -1;
end;

function TVstPlugin.GetRect: TRect;
var theRect: ERect;
begin
 theRect := EditGetRect;
 result.Left := theRect.left;
 result.right := theRect.right;
 result.Top := theRect.Top;
 result.Bottom := theRect.Bottom;
end;

function TVstPlugin.EditGetRect: ERect;
var temp: PPERect;
begin
 getmem(temp, SizeOf(PPERect));
 if fActive then VstDispatch(effEditGetRect, 0, 0, temp);
 if Assigned(temp) then
  if Assigned(temp^)
   then result := temp^^;
 FreeMem(temp);
end;

function TVstPlugin.EditOpen(Handle: THandle): Integer;
var i: integer;
begin
 i:=0;
 try
//  raise Exception.Create(IntToStr(Integer(effEditOpen)));
  i := VstDispatch(effEditOpen, 0, 0, Pointer(Handle));
 finally
  if i > 0 then FEditOpen := true
  else FEditOpen := false;
  result := i;
 end;
end;

procedure TVstPlugin.onEditActivate(Sender: TObject);
begin
 EditActivate;
end;

procedure TVstPlugin.onEditDeActivate(Sender: TObject);
begin
 EditDeActivate;
end;

procedure TVstPlugin.ShowEdit;
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
     OnClose := onFormClose;
     OnActivate := onEditActivate;
     OnDeActivate := onEditDeActivate;
     if Caption=' - ' then Caption := GetEffectName;
    end;
   fGUIFormCreated:=True;
   ShowEdit(GUIForm);
   if (effFlagsHasEditor in PVstEffect.EffectFlags)
    then theRect := EditGetRect
    else theRect:=Rect(0, 200, 0, 80);
   GUIForm.ClientWidth := theRect.right - theRect.left;
   GUIForm.ClientHeight := theRect.Bottom - theRect.Top;
  end;
 GUIForm.Visible := True;
end;

procedure TVstPlugin.ShowEdit(Form: TForm);
var param: string;
    i,wxw: Integer;
    theRect: ERect;
begin
 if (effFlagsHasEditor in PVstEffect.EffectFlags) and (fGUIStyle = gsDefault) then
  begin
   if not FEditOpen then
   begin
    EditOpen(Form.Handle);
    EditIdle;
   end;
//  else raise exception.Create('Editor is already open!');
  end
 else // Vst has no GUI
  case fGUIStyle of
   gsOld:
    begin
     GUIForm := Form;
     FEditOpen:=True;
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
       VertScrollBar.Smooth := true; VertScrollBar.Tracking := true;
       HorzScrollBar.Smooth := true; HorzScrollBar.Tracking := true;
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
       Font.Color:=clWindowText;
       OnChange(nil);
      end;
    end;
   gsDefault, gsList:
    begin
     theRect:=Rect(0,0,Form.Width,4 + numParams * 16);
     GUIForm:=Form; wxw:=0;
     GUIForm.Visible:=False;
     GUIForm.ClientWidth := theRect.right - theRect.left;
     GUIForm.ClientHeight := theRect.Bottom - theRect.Top;
     with TLabel.Create(Form) do
      try
       Parent := Form; Alignment := taCenter;
       for i:=0 to FnumParams - 1 do
        if Canvas.TextWidth(GetParamName(i)+':_')>wxw
         then wxw:=Canvas.TextWidth(GetParamName(i)+':_');
      finally
       Free;
      end;

     for i:=0 to numParams - 1 do
      begin
       with TLabel.Create(Form) do
        begin
         Name := 'LbL'+IntToStr(i); Parent := Form; Caption := GetParamName(i)+':';
         Height := 16; Alignment := taCenter; Left := 2; Top := 2+i*Height;
        end;
       with TLabel.Create(Form) do
        begin
         Name := 'LbV'+IntToStr(i); Parent := Form; Alignment := taCenter;
         Height := 16; Left := Form.Width-Left-72; AutoSize:=False;
         Alignment:=taCenter; Width:=65; Top := 2+i*Height;
        end;
       with TScrollBar.Create(Form) do
        begin
         Name := 'ParamBar'+IntToStr(i); Parent := Form;
         Anchors := [akLeft, akTop, akRight];
         Kind := sbHorizontal; LargeChange:=10;
         Height := 16; Top := 2+i*Height; Tag:=i;
         Left := wxw+2; Width := Form.Width-Left-72;
         Min := 0; Max := 1000; TabOrder := 3+i;
         Position := Round(1000 * Parameters[i]);
         OnChange := ListParamChange;
         ListParamChange(GUIForm.FindComponent('ParamBar'+IntToStr(i)));
        end;
      end;
     GUIForm.Visible:=True;
     GUIForm.Invalidate;
     FEditOpen:=True;
    end;
  end;
 if assigned(FOnShowEdit) then FOnShowEdit(Self, GUIForm);
end;

procedure TVstPlugin.ListParamChange(Sender: TObject);
var lb  : TLabel;
    str : string;
    i   : Integer;
begin
 with (Sender As TScrollBar) do
  try
   Parameters[Tag] := Position * 0.001;
   lb:=TLabel(GUIForm.FindComponent('LbV'+IntToStr(Tag)));
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

procedure TVstPlugin.ParamChange(Sender: TObject);
var nr: Integer;
begin
 if GUIForm.FindComponent('ParamBar') <> nil then
 {$IFDEF SB}
  with (GUIForm.FindComponent('ParamBar') As TFlatScrollBar) do
  {$ELSE}
  with (GUIForm.FindComponent('ParamBar') As TTrackBar) do
  {$ENDIF}
   try
    nr := (GUIForm.FindComponent('ParamBox') As TComboBox).ItemIndex;
    if (nr >= 0) and (nr < numParams) then
     Position := round(GetParameter(nr) * 100);
   except
   end;
end;

{$IFDEF SB}
procedure TVstPlugIn.ScrollChange(Sender: TObject; ScrollPos: Integer);
var nr: integer;
begin
 nr := (Sender as TFlatScrollBar).tag;
 SetParameter(nr, (Sender as TFlatScrollBar).Position * 0.01);
 (GUIForm.FindComponent('LbL' + inttostr(nr)) As TLabel).Caption :=
  'Value: ' + GetParamDisplay(nr) + GetParamLabel(nr);
end;
{$ELSE}
procedure TVstPlugin.TrackChange(Sender: TObject);
var nr: integer;
begin
 with (GUIForm.FindComponent('ParamBar') As TTrackBar) do
  begin
   nr := (GUIForm.FindComponent('ParamBox') As TComboBox).ItemIndex;
   SetParameter(nr, Position * 0.01);
   (GUIForm.FindComponent('LbL') As TLabel).Caption :=
    'Value: ' + GetParamDisplay(nr) + GetParamLabel(nr);
  end;
end;
{$ENDIF}

procedure TVstPlugin.EditClose;
begin
 if FEditOpen then
  if not Boolean(VstDispatch(effEditClose))
   then // ToDo
 FEditOpen := False;
end;

procedure TVstPlugin.CloseEdit;
var i: Integer;
begin
 if not Assigned(PVstEffect) then Exit;
 if assigned(FOnCloseEdit) then FOnCloseEdit(Self);
 if (effFlagsHasEditor in PVstEffect.EffectFlags) and (FGUIStyle=gsDefault)
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
      i:=0;
      repeat
       if GUIForm.FindComponent('ParamBar'+IntToStr(i))=nil then Break;
       GUIForm.FindComponent('ParamBar'+IntToStr(i)).Free;
       if GUIForm.FindComponent('LbL'+IntToStr(i))=nil then Break;
       GUIForm.FindComponent('LbL'+IntToStr(i)).Free;
       if GUIForm.FindComponent('LbV'+IntToStr(i))=nil then Break;
       GUIForm.FindComponent('LbV'+IntToStr(i)).Free;
       inc(i);
      until false;
     end;
   end;
 if (GUIForm <> nil) and fGUIFormCreated then //and (not FExternalForm) then
  try
   GUIForm.Free;
  finally
   GUIForm := nil;
  end;
 FEditOpen := false;
end;

procedure TVstPlugin.onFormClose(Sender: TObject; var Action: TCloseAction);
begin
 CloseEdit;
 if GUIForm <> nil then
 begin
  GUIForm.Free;
  GUIForm := nil;
 end;
end;

function TVstPlugin.EditIdle: integer;
begin
 if FEditOpen then result := VstDispatch(effEditIdle) else result := 0;
end;

procedure TVstPlugin.EditActivate;
begin
 if FEditOpen then VstDispatch(effEditTop);
end;

procedure TVstPlugin.EditDeactivate;
begin
 if FEditOpen then VstDispatch(effEditSleep);
end;

function TVstPlugin.Identify: Integer;
begin
 result := VstDispatch(effIdentify);
end;

function TVstPlugin.GetChunk(pntr: pointer; isPreset: boolean = false): Integer;
begin
 result := VstDispatch(effGetChunk, integer(isPreset), 0, pntr);
end;

function TVstPlugin.SetChunk(data: pointer; byteSize: Integer; isPreset: boolean = false): Integer;
begin
 result := VstDispatch(effSetChunk, integer(isPreset), byteSize, data);
end;

function TVstPlugin.ProcessEvents(pntr: PVstEvents):Integer;
begin
 result := VstDispatch(effProcessEvents, 0, 0, pntr);
end;

function TVstPlugin.CanBeAutomated(index: Integer):Integer;
begin
 result := VstDispatch(effCanBeAutomated, index);
end;

function TVstPlugin.String2Parameter(ParameterName: string):Integer;
var temp: Integer;
begin
 temp := 0;
 VstDispatch(effString2Parameter, temp, 0, pchar(ParameterName));
 result := temp;
end;

function TVstPlugin.GetNumProgramCategories: Integer;
begin
 if FActive then
  result := VstDispatch(effGetNumProgramCategories)
 else result := -1;
end;

function TVstPlugIn.GetnumPrograms: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numPrograms
  else result := FnumPrograms;
end;

function TVstPlugin.GetProgramNameIndexed(category, index: Integer; ProgramName: PChar): Integer;
begin
 if FActive then
  result := VstDispatch(effGetProgramNameIndexed, index, category, ProgramName)
 else result := -1;
end;

function TVstPlugin.CopyCurrentProgramTo(Destination: Integer): Boolean;
begin
 if FActive then
  result := boolean(VstDispatch(effCopyProgram, Destination))
 else result := false;
end;

function TVstPlugin.ConnectInput(InputNr: Integer; state: boolean): Integer;
begin
 if FActive then
  result := VstDispatch(effConnectInput, InputNr, integer(state))
 else result := -1;
end;

function TVstPlugin.ConnectOutput(OutputNr: Integer; state: boolean): Integer;
begin
 if FActive then
  result := VstDispatch(effConnectOutput, OutputNr, integer(state))
 else result := -1;
end;

function TVstPlugin.GetInputProperties(InputNr: Integer): TVstPinProperties;
var temp: PVstPinProperties;
begin
 new(temp);
 if FActive then VstDispatch(effGetInputProperties, InputNr, 0, temp);
 result := temp^;
 Dispose(temp);
end;

function TVstPlugin.GetOutputProperties(OutputNr: Integer): TVstPinProperties;
var temp: PVstPinProperties;
begin
 new(temp);
 if FActive then VstDispatch(effGetOutputProperties, OutputNr, 0, temp);
 result := temp^;
 Dispose(temp);
end;

function TVstPlugin.GetPlugCategory:TVstPluginCategory;
begin
 if FActive
  then result := TVstPluginCategory(VstDispatch(effGetPlugCategory))
  else result := vpcUnknown;
end;

function TVstPlugin.GetCurrentPosition:Integer;
begin
 if FActive then result := VstDispatch(effGetCurrentPosition) else result := -1;
end;

function TVstPlugin.GetDestinationBuffer: Integer;
begin
 if FActive then result := VstDispatch(effGetDestinationBuffer) else result := -1;
end;

function TVstPlugin.OfflineNotify(pntr: PVstAudioFile; numAudioFiles: Integer; start: boolean):Integer;
begin
 result := VstDispatch(effOfflineNotify, integer(start), numAudioFiles, pntr);
end;

function TVstPlugin.OfflinePrepare(pntr: PVstOfflineTask; count: Integer):Integer;
begin
 result := VstDispatch(effOfflinePrepare, 0, count, pntr);
end;

function TVstPlugin.OfflineRun(pntr: PVstOfflineTask; count: Integer):Integer;
begin
 result := VstDispatch(effOfflineRun, 0, count, pntr);
end;

function TVstPlugin.ProcessVarIo(varIo: PVstVariableIo):Integer;
begin
 result := VstDispatch(effProcessVarIo, 0, 0, varIo);
end;

function TVstPlugin.SetSpeakerArrangement(pluginInput: PVstSpeakerArrangement; pluginOutput:PVstSpeakerArrangement):Boolean;
begin
 result := boolean(VstDispatch(effSetSpeakerArrangement, 0, Integer(pluginInput), pluginOutput));
end;

function TVstPlugin.SetBlockSizeAndSampleRate(blockSize :Integer; sampleRate:Single):Integer;
begin
 result := VstDispatch(effSetBlockSizeAndSampleRate, 0, blockSize, nil, sampleRate);
end;

function TVstPlugin.SetBypass(onOff :boolean):Integer;
begin
 result := VstDispatch(effSetBypass, 0, Integer(onOff));
end;

function TVstPlugin.GetEffectName:string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 255);
   if VstDispatch(effGetEffectName, 0, 0, temp) <> 0 then
    result := shortstring(temp);
   FreeMem(temp);
  end;
end;

function TVstPlugin.GetErrorText:string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 258);
   if VstDispatch(effGetErrorText, 0, 0, temp) = 0 then
    result := shortstring(temp);
   FreeMem(temp);
  end;
end;

function TVstPlugIn.GetFriendlyNameString(const StringLength: Integer): string;
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

function TVstPlugin.GetVendorString: string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 255);
   if VstDispatch(effGetVendorString, 0, 0, temp) <> 0 then
    result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TVstPlugin.GetProductString:string;
var temp: pchar;
begin
 result := '';
 if FActive then
  begin
   getmem(temp, 255);
   if VstDispatch(effGetProductString, 0, 0, temp) <> 0 then
    result := ShortString(temp);
   FreeMem(temp);
  end;
end;

function TVstPlugin.GetVendorVersion:Integer;
begin
 if FActive then
  result := VstDispatch(effGetVendorVersion)
 else
  result := -1;
end;

function TVstPlugin.VendorSpecific(index, value:Integer; pntr: Pointer; opt :single):Integer;
begin
 result := VstDispatch(effVendorSpecific, index, value, pntr, opt);
end;

function TVstPlugin.CanDo(pntr: pchar):Integer;
begin
 result := VstDispatch(effCanDo, 0, 0, pntr);
end;

function TVstPlugin.GetTailSize:Integer;
begin
 if FActive then result := VstDispatch(effGetTailSize) else result := -1;
end;

function TVstPlugin.Idle:Integer;
begin
 if FNeedIdle then
  result := VstDispatch(effIdle)
 else result := 0;
end;

function TVstPlugin.GetIcon:Integer;
begin
 if FActive then
  result := VstDispatch(effGetIcon)
 else result := -1;
end;

procedure TVstPlugin.SetViewPosition(x, y: Integer);
begin
 VstDispatch(effSetViewPosition, x, y);
end;

function TVstPlugin.GetParameterProperties(Parameter: Integer):TVstParameterProperties;
var temp: PVstParameterProperties;
begin
 GetMem(temp, SizeOf(TVstParameterProperties));
 if FActive then VstDispatch(effGetParameterProperties, Parameter, 0, temp);
 result := temp^;
 FreeMem(temp);
end;

function TVstPlugin.KeysRequired: Integer;
begin
 if FActive then result := VstDispatch(effKeysRequired)
  else result := -1;
end;

function TVstPlugin.GetVstVersion: Integer;
begin
 if FActive then
  result := VstDispatch(effGetVstVersion) else result := -1;
end;

function TVstPlugin.EditKeyDown(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
begin
 Result := false;
 if FActive then
  Result := (VstDispatch(effEditKeyDown, Integer(Key), VirtualKeycode, nil, Modifier) = 1);
  // character in <index>, virtual in <value>, modifiers in <opt>, return true if used, else false
end;

function TVstPlugin.EditKeyUp(Key : Char; VirtualKeycode : Integer; Modifier :Double): Boolean;
begin
 Result := false;
 if FActive then
  Result := (VstDispatch(effEditKeyUp, Integer(Key), VirtualKeycode, nil, Modifier) = 1);
 // character in <index>, virtual in <value>, modifiers in <opt>, return true if used, else false
end;

procedure TVstPlugin.SetEditKnobMode(Mode : TKnobMode);
begin
 if FActive then VstDispatch(effSetEditKnobMode,0,Integer(Mode));
end;

// midi plugins channel dependent programs
function TVstPlugin.GetMidiProgramName(MidiProgramNamePointer : PMidiProgramName): Integer;
begin
 if FActive then
  Result := VstDispatch(effGetMidiProgramName, 0, 0, MidiProgramNamePointer, 0)
 else
  Result := 0;
 // struct will be filled with information for 'thisProgramIndex'.
 // returns number of used programIndexes.
 // if 0 is returned, no MidiProgramNames supported.
end;

function TVstPlugIn.GetnumInputs: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numInputs
  else result := FnumInputs;
end;

function TVstPlugIn.GetnumOutputs: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numOutputs
  else result := FnumOutputs;
end;

function TVstPlugIn.GetnumParams: Integer;
begin
 if Assigned(PVstEffect)
  then result := PVstEffect^.numParams
  else result := FnumParams;
end;

function TVstPlugin.GetCurrentMidiProgram(MidiProgramNamePointer : PMidiProgramName): Integer;
begin
 if FActive then
  Result := VstDispatch(effGetCurrentMidiProgram, 0, 0, MidiProgramNamePointer, 0)
 else
  Result := 0;
 // returns the programIndex of the current program.
 // passed <ptr> points to MidiProgramName struct.
 // struct will be filled with information for the current program.
end;

function TVstPlugin.GetMidiProgramCategory(MidiProgramCategoryPointer : PMidiProgramCategory): Integer;
begin
 if FActive then
  Result := VstDispatch(effGetMidiProgramCategory, 0, 0, MidiProgramCategoryPointer, 0)
 else
  Result := 0;
 // passed <ptr> points to MidiProgramCategory struct.
 // struct will be filled with information for 'thisCategoryIndex'.
 // returns number of used categoryIndexes.
 // if 0 is returned, no MidiProgramCategories supported.
end;

function TVstPlugin.HasMidiProgramsChanged: Integer;
begin
 if FActive then
  Result := VstDispatch(effHasMidiProgramsChanged, 0, 0, nil, 0)
 else
  Result := 0;
 // returns 1 if the MidiProgramNames or MidiKeyNames
 // had changed on this channel, 0 otherwise. <ptr> ignored.
end;

function TVstPlugin.GetMidiKeyName(MidiKeyNamePointer: PMidiKeyName): Integer;
begin
 if FActive then
  Result := VstDispatch(effGetMidiKeyName, 0, 0, MidiKeyNamePointer, 0)
 else
  Result := 0;
// struct will be filled with information for 'thisProgramIndex' and
// 'thisKeyNumber'. If keyName is "" the standard name of the key
// will be displayed. If 0 is returned, no MidiKeyNames are
// defined for 'thisProgramIndex'.
end;

procedure TVstPlugin.BeginSetProgram;
begin // called before a new program is loaded
 if FActive then VstDispatch(effBeginSetProgram);
end;

procedure TVstPlugin.EndSetProgram;
begin // called when the program is loaded
 if FActive then VstDispatch(effEndSetProgram);
end;

function TVstPlugin.GetSpeakerArrangement(SpeakerIn, SpeakerOut:PVstSpeakerArrangement): Integer;
begin
 Result := 0;
 if FActive then
  VstDispatch(effGetSpeakerArrangement, 0, Integer(@SpeakerOut), @SpeakerOut);
// VstSpeakerArrangement** pluginInput in <value>
// VstSpeakerArrangement** pluginOutput in <ptr>
end;

function TVstPlugin.ShellGetNextPlugin(var PluginName:String): Integer;
var temp: pchar;
begin
 Result := 0; 
 if FActive then
 begin
  getmem(temp, 255);
  Result := VstDispatch(effShellGetNextPlugin, 0, 0, temp); // returns the next plugin's uniqueID.
  if Result <> 0 then PluginName:=ShortString(temp);
  FreeMem(temp);
 end;
end;

procedure TVstPlugin.StartProcess;
begin
 if FActive then VstDispatch(effStartProcess); // Called before the start of process call
end;

procedure TVstPlugin.StopProcess;
begin
 if FActive then VstDispatch(effStopProcess);
 // Called after the stop of process call
end;

procedure TVstPlugin.SetTotalSampleToProcess;
begin
 if FActive then VstDispatch(effSetTotalSampleToProcess);
 // Called in offline (non RealTime) Process before process
 // is called, indicates how many sample will be processed
end;

procedure TVstPlugin.SetPanLaw(PanLaw: TVstPanLawType; Gain: Single);
begin
 if FActive then VstDispatch(effSetPanLaw, 0, Integer(PanLaw), nil, Gain); // PanLaw : Type (Linear, Equal Power,.. see enum PanLaw Type) in <value>,                                                                // Gain in <opt>: for Linear : [1.0 => 0dB PanLaw], [~0.58 => -4.5dB], [0.5 => -6.02dB]
end;

function TVstPlugin.BeginLoadBank(PatchChunkInfo : PVstPatchChunkInfo): integer;
begin
 if FActive then VstDispatch(effBeginLoadBank);
 Result := 0;
 // Called before a Bank is loaded, <ptr> points to VstPatchChunkInfo structure
 // return -1 if the Bank can not be loaded, return 1 if it can be loaded else 0 (for compatibility)
end;

procedure TVstPlugin.BeginLoadProgram(PatchChunkInfo : PVstPatchChunkInfo);
begin
 if FActive then VstDispatch(effBeginLoadProgram, 0, 0, PatchChunkInfo);
 // Called before a Program is loaded, <ptr> points to VstPatchChunkInfo structure
end;

function TVstPlugin.GetEntryPoints(theDll: TFileName): integer;
    {$IFNDEF FPC}
var Buf : Array[0..511] of char;
    LE  : Integer;
    str : string;
    {$ENDIF}
begin
 result := 0;
 FDLLHandle := SafeLoadLibrary(pChar(theDll),7);
// FDLLHandle := LoadLibraryEx(pChar(theDll), 0, DONT_RESOLVE_DLL_REFERENCES);

 case FDLLHandle = 0 of
   TRUE:
   try
    {$IFNDEF FPC}
    LE:=GetLastError;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NIL, LE, 0, @Buf[0], 256, NIL);
    if Buf='' then
     begin
      str:=IntToStr(LE);
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

procedure TVstPlugin.SetDLLFileName(VstFilename:TFilename);
begin
 FDLLFileName := VstFilename;
end;

function TVstPlugin.Load(PluginDll: TFilename):Boolean;
begin
 result := false;
 try
   Unload;  // make sure nothing else is loaded
   FMainFunction := nil;
   if not FileExists(PluginDll) then raise exception.Create('File ' + PluginDll + ' does not exists');
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
   result := true;
  end else raise exception.Create('PlugIn ' + PluginDll + ' could not be loaded');
 except
  result := false;
  Unload;
  exit;
 end;
 try
  fActive := true;
  FDLLFileName := PluginDLL;
 except
 end;
end;

procedure TVstPlugin.UnLoad;
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
 FuID := '';
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

function SwapLong(var l: longint): longint;
var t:longint;
type X = array [0..1] of word;
begin
 T := Swap(X(L)[1]);
 X(L)[1] := Swap(X(L)[0]);
 X(L)[0] := T;
 result := t;
end;

procedure TVstPlugIn.LoadBank(FileName: TFileName);
var chnk: TFileStream;
begin
 if not FileExists(FileName) then raise Exception.Create('bank file does not exist');
 chnk := TFileStream.Create(FileName, fmOpenRead);
 try
  LoadBank(chnk);
 finally
  chnk.Free;
 end; 
end;

procedure TVstPlugIn.LoadPreset(FileName: TFileName);
var chnk: TFileStream;
begin
 if not FileExists(FileName) then raise Exception.Create('preset file does not exist');
 chnk := TFileStream.Create(FileName, fmOpenRead);
 try
  LoadPreset(chnk);
 finally
  chnk.Free;
 end; 
end;

function TVstPlugIn.GetPreset(i: integer): TFXPreset;
var s: string;
    pc: plongint;
    x: integer;
    si: single;
begin
 SetProgram(i);
 result.chunkMagic := FourCharToLong('C','c','n','K');
 result.fxMagic := FourCharToLong('F','x','C','k');
 result.version := 1;
 result.fxID := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
 result.fxVersion := PVstEffect^.version;
 result.numParams := numParams;
 SwapLong(result.chunkMagic);
 SwapLong(result.fxMagic);
 SwapLong(result.version);
 SwapLong(result.fxID);
 SwapLong(result.fxVersion);
 SwapLong(result.numParams);
 s := GetProgramName + #0;
 StrLCopy(result.prgName, PChar(s), 26);
 GetMem(result.params, numParams * sizeof(single));
 pc := plongint(result.params);
 for i := 0 to numParams - 1 do
 begin
  si := GetParameter(i);
  x := plongint(@si)^;
  SwapLong(x);
  pc^ := x;
  inc(pc);
 end;
 result.byteSize := sizeof(result) - sizeof(longint) * 2 + (numParams - 1) * sizeof(single);
 SwapLong(result.byteSize);
end;

procedure TVstPlugIn.SaveBank(FileName: TFileName);
var chnk: TFileStream;
begin
 chnk := TFileStream.Create(FileName, fmCreate);
 try
  SaveBank(chnk);
 finally
  chnk.Free;
 end;
end;

procedure TVstPlugIn.SavePreset(FileName: TFileName);
var chnk: TFileStream;
begin
 chnk := TFileStream.Create(FileName, fmCreate);
 try
  SavePreset(chnk);
 finally
  chnk.Free;
 end; 
end;

procedure TVstPlugIn.SavePreset(Stream: TStream);
var p2: TFXChunkSet;
    s: string;
    x: integer;
    PBuffer: Pointer;
    pp: TFXPreset;
begin
 Stream.Seek(0, 0);
 if not assigned(PVstEffect) then exit;
 if effFlagsProgramChunks in EffectOptions then
  begin
   p2.chunkMagic := FourCharToLong('C','c','n','K');
   p2.fxMagic := FourCharToLong('F','P','C','h');
   p2.version := 1;
   p2.fxID := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
   p2.fxVersion := PVstEffect^.version;
   p2.numPrograms := numPrograms;
   SwapLong(p2.chunkMagic);
   SwapLong(p2.fxMagic);
   SwapLong(p2.version);
   SwapLong(p2.fxID);
   SwapLong(p2.fxVersion);
   SwapLong(p2.numPrograms);
   s := GetProgramName + #0;
   StrLCopy(p2.prgName, PChar(s), 26);
   x := GetChunk(@PBuffer, true);
   p2.chunkSize := x;
   p2.byteSize := sizeof(p2) - sizeof(longint) * 2 + p2.chunkSize - 8;
   SwapLong(p2.byteSize);
   SwapLong(p2.chunkSize);
   Stream.WriteBuffer(p2, sizeof(p2) - sizeof(pointer));
   Stream.WriteBuffer(PBuffer^, x);
  end
 else
  begin
   pp := GetPreset(GetProgram);
   Stream.WriteBuffer(pp, sizeof(pp) - sizeof(single));
   Stream.WriteBuffer(pp.params^, sizeof(single) * numParams);
   FreeMem(pp.params);
  end;
end;

procedure TVstPlugIn.LoadBank(Stream: TStream);
var i: Integer;
    p: TFXSet;
    p2: TFXChunkBank;
    pp: TFXPreset;
    s: single;
    j, x: integer;
    ptr: pointer;
    pb2: pointer;
    pci: TVstPatchChunkInfo;
    b: byte;
    usechunk: boolean;
begin
 if not assigned(PVstEffect) then exit;
 Stream.Seek(9, 0);
 Stream.Read(b, 1);
 usechunk := (b <> $78);
 Stream.Seek(0, 0);

// if eoProgramChunks in EffectOptions then
 if usechunk then
 begin
  ptr := @p2;
  Stream.Read(ptr^, sizeof(TFXChunkBank) - sizeof(pointer));

  x := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
  SwapLong(x);
  if p2.fxId <> x then raise Exception.Create('bank file not for this plugin!');

  x := Stream.Size - Stream.Position;
  GetMem(pb2, x + 1);
  j := Stream.Read(pb2^, x);
  SetChunk(pb2, j, false);
  FreeMem(pb2);
 end else
 begin
  ptr := @p;
  Stream.Read(ptr^, sizeof(TFXSet) - sizeof(pointer));
  x := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
  SwapLong(x);
  if p.fxId <> x then raise Exception.Create('bank file not for this plugin!');

  pci.version:=1;
  pci.pluginUniqueID := PVstEffect.uniqueID;
  pci.pluginVersion := PVstEffect.version;
  pci.numElements := PVstEffect.numPrograms; // Number of Programs (Bank)
  BeginLoadBank(@pci);

  SwapLong(p.numPrograms);
  for j := 0 to p.numPrograms - 1 do
  begin
   ptr := @pp;
   Stream.Read(ptr^, sizeof(TFXPreset) - sizeof(pointer));
   SetProgram(j);
   SetProgramName(pp.prgName);
   SwapLong(pp.numParams);
   ptr := @x;
   for i := 0 to pp.numParams - 1 do
   begin
    Stream.Read(ptr^, sizeof(single));
    SwapLong(x);
    s := psingle(ptr)^;
    SetParameter(i, s);
   end;
  end;
 end;
end;

procedure TVstPlugIn.LoadPreset(Stream: TStream);
var i: Integer;
    p: TFXPreset;
    p2: TFXChunkset;
    s: single;
    j, x: integer;
    ptr: pointer;
    pb2: pointer;
    pci: TVstPatchChunkInfo;
    b: byte;
    usechunk: boolean;
begin
 if not assigned(PVstEffect) then exit;
 Stream.Seek(9, 0);
 Stream.Read(b, 1);
 usechunk := (b <> $78);
 Stream.Seek(0, 0);

// if eoProgramChunks in EffectOptions then
 if usechunk then
 begin
  ptr := @p2;
  Stream.Read(ptr^, sizeof(TFXChunkset) - sizeof(pointer));
  x := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
  SwapLong(x);
  if p2.fxId <> x then raise Exception.Create('preset file not for this plugin!');
  SetProgramName(p2.prgName);

  x := Stream.Size - Stream.Position;
  GetMem(pb2, x + 1);
  j := Stream.Read(pb2^, x);
  SetChunk(pb2, j, true);
  FreeMem(pb2);
 end else
 begin
  ptr := @p;
  Stream.Read(ptr^, sizeof(TFXPreset) - sizeof(pointer));
  x := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
  SwapLong(x);
  if p.fxId <> x then raise Exception.Create('preset file not for this plugin!');
  pci.version := 1;
  pci.pluginUniqueID := PVstEffect.uniqueID;
  pci.pluginVersion := PVstEffect.version;
  pci.numElements := PVstEffect.numParams; // Number of Programs (Bank)
  BeginLoadProgram(@pci);

  SetProgramName(p.prgName);
  SwapLong(p.numParams);
  ptr := @x;
  for i := 0 to p.numParams - 1 do
  begin
   Stream.Read(ptr^, sizeof(single));
   SwapLong(x);
   s := psingle(ptr)^;
   SetParameter(i, s);
  end;
 end;
end;

procedure TVstPlugIn.SaveBank(Stream: TStream);
var p: TFXSet;
    p2: TFXChunkBank;
    j, x: integer;
    PBuffer: Pointer;
    pp: TFXPreset;
begin
 if not assigned(PVstEffect) then exit;
 Stream.Seek(0, 0);
 if effFlagsProgramChunks in EffectOptions then
 begin
  p2.chunkMagic := FourCharToLong('C','c','n','K');
  p2.fxMagic := FourCharToLong('F','B','C','h');
  p2.version := 1;
  p2.fxID := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
  p2.fxVersion := PVstEffect^.version;
  p2.numPrograms := numPrograms;
  SwapLong(p2.chunkMagic);
  SwapLong(p2.fxMagic);
  SwapLong(p2.version);
  SwapLong(p2.fxID);
  SwapLong(p2.fxVersion);
  SwapLong(p2.numPrograms);
  x := GetChunk(@PBuffer, false);
  p2.chunkSize := x;
  p2.byteSize := sizeof(p2) - sizeof(longint) * 3 + p2.chunkSize + 8;
  SwapLong(p2.byteSize);
  SwapLong(p2.chunkSize);
  Stream.WriteBuffer(p2, sizeof(p2) - sizeof(pointer));
  Stream.WriteBuffer(PBuffer^, x);
 end else
 begin
  p.chunkMagic := FourCharToLong('C','c','n','K');
  p.fxMagic := FourCharToLong('F','x','B','k');
  p.version := 1;
  p.fxID := FourCharToLong(uid[1], uid[2], uid[3], uid[4]);
  p.fxVersion := PVstEffect^.version;
  p.numPrograms := numPrograms;

  SwapLong(p.chunkMagic);
  SwapLong(p.fxMagic);
  SwapLong(p.version);
  SwapLong(p.fxID);
  SwapLong(p.fxVersion);
  SwapLong(p.numPrograms);
  p.byteSize := sizeof(p) - sizeof(longint) +
   (sizeof(TFXPreset) + (numParams - 1) * sizeof(single)) * numPrograms;
  SwapLong(p.byteSize);
  Stream.WriteBuffer(p, sizeof(p) - sizeof(single));
  for j := 0 to numPrograms - 1 do
  begin
   pp := GetPreset(j);
   Stream.WriteBuffer(pp, sizeof(pp) - sizeof(single));
   Stream.WriteBuffer(pp.params^, sizeof(single) * numParams);
   FreeMem(pp.params);
  end;
 end;
end;

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TVstHost]);
end;

function string2Language(LanguageString : string): TVSTHostLanguage;
begin
 if      LanguageString='English'  then Result:=kVstLangEnglish
 else if LanguageString='French'   then Result:=kVstLangGerman
 else if LanguageString='German'   then Result:=kVstLangFrench
 else if LanguageString='Italian'  then Result:=kVstLangItalian
 else if LanguageString='Japanese' then Result:=kVstLangSpanish
 else if LanguageString='Spanish'  then Result:=kVstLangJapanese
 else Result:=kVstLangEnglish
end;

function PlugCategory2String(Category:TVstPluginCategory):string;
begin
  case Category of
    vpcUnknown        : Result:='Unknown';
    vpcEffect         : Result:='Effect';
    vpcSynth          : Result:='Synth';
    vpcAnalysis       : Result:='Analysis';
    vpcMastering      : Result:='Mastering';
    vpcSpacializer    : Result:='Spacializer';
    vpcRoomFx         : Result:='RoomFx';
    vpcSurroundFx     : Result:='SurroundFx';
    vpcRestoration    : Result:='Restoration';
    vpcOfflineProcess : Result:='OfflineProcess';
    vpcShell          : Result:='Shell';
    vpcGenerator      : Result:='Generator';
  end;
end;

function EffOptions2String(EffOpts: TEffFlags):string;
begin
 Result:='';
 if effFlagsHasEditor     in EffOpts then Result:=Result+'HasEditor, ';
 if effFlagsHasClip       in EffOpts then Result:=Result+'HasClip, ';
 if effFlagsHasVu         in EffOpts then Result:=Result+'HasVU, ';
 if effFlagsCanMono       in EffOpts then Result:=Result+'CanMono, ';
 if effFlagsCanReplacing  in EffOpts then Result:=Result+'CanReplacing, ';
 if effFlagsProgramChunks in EffOpts then Result:=Result+'ProgramChunks, ';
 if effFlagsIsSynth       in EffOpts then Result:=Result+'IsSynth, ';
 if effFlagsNoSoundInStop in EffOpts then Result:=Result+'NoSoundInStop, ';
 if effFlagsExtIsAsync    in EffOpts then Result:=Result+'ExtIsAsync, ';
 if effFlagsExtHasBuffer  in EffOpts then Result:=Result+'ExtHasBuffer, ';

 if Length(Result)>2 then Result:=Copy(Result,0,Length(Result)-2)
end;

procedure TVstPlugIn.ProcessAudio(inputs, outputs: ppsingle; sampleframes: Integer);
begin
 if PVstEffect <> nil then
 begin
  if effFlagsCanReplacing in PVstEffect.EffectFlags
   then PVstEffect.processreplacing(PVstEffect, inputs, outputs, sampleframes)
   else PVstEffect.process(PVstEffect, inputs, outputs, sampleframes);
 end;
end;

function TVstPlugIn.GetInitialDelay: Integer;
begin
 if assigned(PVstEffect)
  then result:=PVstEffect.initialDelay
  else result:=0;
end;

procedure TVstPlugIn.SetGUIStyle(const Value: TGUIStyle);
begin
 if FEditOpen
  then raise Exception.Create('Close Editor first!')
  else fGUIStyle := Value;
end;

initialization
 {$IFDEF FPC}
 {$i TVSTHost.lrs}
 {$ENDIF}
 audioMaster := AudioMasterCallback;
 HostWindows := TObjectList.Create;
 
finalization
 HostWindows.Free;

end.
