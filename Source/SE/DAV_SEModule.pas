unit DAV_SEModule;

interface

uses
  Windows, Classes, SysUtils, DAV_Common, DAV_SECommon;

type
  TUgFlag = (
    ugfVoiceMonIgnore = 2,      // DON'T WANT VOICE HELD OPEN BECAUSE A SCOPE IS CONNECTED
    ugfPolyphonicAgregator = 6, // A ug that always combines voices

    // read only
    ugfSuspend = 7,
    ugfOpen = 8,

    // normally when a voice has faded out, SE shuts all that voice's modules off
    // in very special cases you can prevent SE from shutting off your module
    ugfNeverSuspend = 9,
    ugfClone = 11,
    ugfSendTimeInfoToHost = 17,
    ugfDoNeitherUseNorRemove = 31 // necessary so that TGuiFlags is an integer
  );
  TUgFlags = set of TUgFlag;

  TGuiFlag = (
    gfControlView = 7,            // visible in PanelEdit mode
    gfStructureView = 8,          // visible in structure mode
    gfDoNeitherUseNorRemove = 31  // necessary so that TGuiFlags is an integer
  );
  TGuiFlags = set of TGuiFlag;

  TSEIOFlag =
    (iofPolyphonicActive,    // midi channel selection etc should should ignore patch changes
     iofIgnorePatchChange,   // auto-rename on new connection
     iofRename,              // plugs which are automaticly duplicated (like a container's 'spare' plug)
     iofAutoDuplicate,
     iofFilename,
     iofSetableOutput,       // ALLOW USER TO SET THE VALUE OF THIS OUTPUT eg on 'constant value' ug
     iofCustomisable,        // plugs which can be duplicated/deleted by CUG
     iofAdder,               // plugs which handle multiple inputs, must belong to an Adder ug
     iofHidePin,             // plugs which are private or obsolete, but are enabled on load if connected somewhere
     iofLinearInput,         // set this if this input can handle more that one polyphonic voice
     iofUICommunication,
     iofAutoEnum,
     iofHideWhenLocked,
     iofParameterScreenOnly, // DON'T EXPOSE AS PLUG (TO SAVE SPACE)
     iofDoNotCheckEnum,
     iofUIDualFlag,          // don't use iofUIDualFlag by itself, use iofUICommunication
     iofPatchStore,          // Patch store is similar to dual but for DSP output plugs that appear as input plugs on UI (Output paramters) could consolodate?
     iofParamPrivate,        // Private parameter (not exposed to user of VST plugin)
     iofMinimized,           // minimised (not exposed on structure view (only properties window)
     iofDisableIfPos = iofHidePin,
     iofPrivate = iofHidePin);
  TSEIOFlags = set of TSEIOFlag;
  //  iofCommunicationDual = iofUIDualFlag or iofUICommunication; // obsolete, use iofPatchStore instead

  // state type
  TSEStateType = (stStop, stOneOff, stStatic = 1, stRun); // stOneOff obsolete. Use stStatic or stRun!

  // Order is important for comparisons
  TUGEventType = (uetStatChange, uetSuspend, uetMIDI, uetRunFunction,
    uetIOFunc, uetUINotify, uetProgChange, uetNoteOn, uetNoteOff,
    uetNoteMute, uetPitchBend, uetAftertouch, uetStartPortamento,
    uetWSTableChange, uetDelayedGate, uetParamAutomation,
    uetNoteOffHeld, uetHeldNotesOff, uetNull, uetGeneric1,
    uetSetOutputPin, uetRunFunction2);

  PSEFloatSample = ^TSEFloatSample;
  TSEFloatSample = Single;

  PSEFloatSampleFixedArray = ^TSEFloatSampleFixedArray;
  TSEFloatSampleFixedArray = Array [0..0] of TSEFloatSample;

  ///////////////////////////
  // Plugin Module Opcodes //
  ///////////////////////////

  TSEPluginModuleOpcodes =
    (seffOpen  = 0,       // initialise
     seffClose = 1,       // exit, release all memory and other resources!

     seffSetSampleRate,   // in Opt (float)
     seffSetBlockSize,    // in Value
     seffGetUniqueId,
     seffGetEffectName,
     seffGetPinProperties,
     seffAddEvent,
     seffResume,
     seffGetModuleProperties,
     seffIsEventListEmpty,
     seffGetSdkVersion,
     seffGuiNotify,
     seffQueryDebugInfo); // allows to host to determin compiler settings etc


  //////////////////
  // Host Opcodes //
  //////////////////

  TSEHostOpcodes = (
    SEAudioMasterSetPinStatus = 0,
    SEAudioMasterIsPinConnected,     //
    SEAudioMasterGetPinInputText,    // gets pointer to plugs input string (DT_TEXT only)
    SEAudioMasterGetSampleClock,     // get current sampleclock at block start
    SEAudioMasterSendMIDI,           // send short MIDI msg
    SEAudioMasterGetInputPinCount,   // total AUDIO ins
    SEAudioMasterGetOutputPinCount,  // total AUDIO outs
    SEAudioMasterGetPinVarAddress,
    SEAudioMasterGetBlockStartClock,
    SEAudioMasterGetTime,
    SEAudioMasterSleepMode,
    SEAudioMasterGetRegisteredName,  // limited to 50 characters or less
    (* EXAMPLE CALLING CODE
      name : array [0..49] of Char;
      CallHost(SEAudioMasterGetRegisteredName, 0, 0, @name[0]);
    *)
    SEAudioMasterGetFirstClone,
    SEAudioMasterGetNextClone,
    (* EXAMPLE CALLING CODE

      procedure IterateThroughAllClones;
      var
        CloneStruct : PSE2ModStructBase;
        Clone       : PModule;
      begin
        // get first one
        CallHost(SEAudioMasterGetFirstClone, 0, 0, CloneStruct);

        while (clone_struct <> 0)
         begin
          // convert host's clone pointer to a 'Module' object
          Clone := PModule(CloneStruct.Object);

          // Access each clone here

          // step to Next clone
          Clone.CallHost(SEAudioMasterGetNextClone, 0, 0, CloneStruct);
         end;
      end;
    *)
    SEAudioMasterGetTotalPinCount,   // Total pins of all types
    SEAudioMasterCallVstHost,        // Call VST Host direct (see se_call_vst_host_params struct)
    SEAudioMasterResolveFilename,    // get full path from a short filename, (int pin_idx, float max_characters, Char *destination)
    SEAudioMasterSendStringToGui,    // Reserved for Experimental use (by Jef)
    SEAudioMasterGetModuleHandle,    // Reserved for Experimental use (by Jef)
    SEAudioMasterAddEvent,           // pass SeEvent *, host will copy data from struct. Safe to discard after call.
    SEAudioMasterCreateSharedLookup,
    SEAudioMasterSetPinOutputText,   // sets plug's output string (DT_TEXT only)
    SEAudioMasterSetProcessFunction, // sets the current SubProcess function
    SEAudioMasterResolveFilename2,   // get full path from a short filename - UNICODE
    (* EXAMPLE CALLING CODE
      uses windows;  //for WideCharToMultiByte

      // get the full path of an imbedded file when you only know it's short name
      const
        MAX_FILENAME_LENGTH : Integer = 300;

      // Both source and destination are UNICODE (two-byte) character strings
      unsigned short *source = L"test.txt";
      unsigned short dest[MAX_FILENAME_LENGTH];

      CallHost(SEAudioMasterResolveFilename2, Integer(source), MAX_FILENAME_LENGTH, &dest);

      // to convert to ascii (optional)
      Char ascii_filename[MAX_FILENAME_LENGTH];
      WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_filename, MAX_FILENAME_LENGTH, NULL, NULL);
    *)
    SEAudioMasterGetSeVersion,       // returns SE Version number times 100,000 ( e.g. 120000 is V 1.2 )
    (* EXAMPLE CALLING CODE
      int v = CallHost(SEAudioMasterGetSeVersion, 0, 0, 0);
    *)
    SEAudioMasterIsInteger = $7FFFFFFF
  );


  //////////////////////////
  // forward declarations //
  //////////////////////////

  PSE1ModStructBase  = ^TSE1ModStructBase;
  PSE2ModStructBase  = ^TSE2ModStructBase;
  PSEEvent           = ^TSEEvent;
  TSEModuleBase      = class;
  TSEPin             = class;

  ////////////////////
  // function types //
  ////////////////////

  TSE1Dispatcher = function(Effect: PSE1ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
  TSE2Dispatcher = function(Effect: PSE2ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
  TSE1Process = procedure(Effect: PSE1ModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer); cdecl;
  TSE2Process = procedure(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
  TSE2Event = procedure(ModuleBase: TSEModuleBase; Event: PSEEvent); cdecl;

  TSESetParameter = procedure(Effect: PSE1ModStructBase; Index: Integer; Parameter: Single);
  TSEGetParameter = function(Effect: PSE1ModStructBase; Index: Integer): Single;

  TSE2EventEvent = function(Event: PSEEvent): Pointer of object;
  TSE2DispatcherEvent = function(Opcode: Integer; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer of object;
  TSE2ProcessEvent = procedure(const BufferOffset, SampleFrames: Integer) of object;
  TSEPlugStateChangeEvent = procedure(Sender: TObject; Pin: TSEPin) of object;
  TSEPinStatusUpdateEvent = procedure(Sender: TObject; AStatus: TSEStateType) of object;
  TSEMidiDataEvent = procedure(Sender: TObject; AClock, AMidiMsg: Cardinal; PinID: Integer) of object;
  TSEInputStateChangedEvent = procedure(Sender: TObject; PlugIndex: Integer; NewState: TSEStateType) of object;
  TSEGuiNotifyEvent = procedure(Sender: TObject; AUserMsgID, ASize: Integer; AData: Pointer) of object;
  TSEVoiceResetEvent = procedure(Sender: TObject; Future: Integer);

  TSE1AudioMasterCallback = function (Effect: PSE1ModStructBase; Opcode: TSEHostOpcodes; Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;
  TSE2AudioMasterCallback = function (Effect: PSE2ModStructBase; Opcode: TSEHostOpcodes; Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;

  ////////////////
  // structures //
  ////////////////

  {$A4} // align all records to 4 byte

  TSE1ModStructBase = record
    Magic            : Integer;              // the 'magic number' that identifies a SynthEdit module (spells SEPL)
    Flags            : Integer;              // see constants
    ReservedA        : Pointer;              // reserved for host use, must be 0
    ReservedB        : Integer;              // reserved, must be 0
    SEModule         : TSEModuleBase;        // for class access, MUST be 0 else!
    User             : Pointer;              // user access
    Version          : Integer;              //
    ProcessReplacing : TSE1Process;          // see TSE1Process
    Future           : array[0..59] of Char; // pls zero
  end;

  TSE2ModStructBase = record
    Magic            : Integer;              // the 'magic number' that identifies a SynthEdit module (spells SEPL)
    Version          : Integer;              //
    HostPtr          : Pointer;              // reserved for host use, must be 0
    SEModule         : TSEModuleBase;        // for class access, MUST be 0 else!
    User             : Pointer;              // User access
    Dispatcher       : TSE2Dispatcher;
    SubProcessPtr    : TSE2Process;
    EventHandlerPtr  : TSE2Event;
    Future           : array[0..15] of Char; // pls zero
  end;

  // fill in this structure when using Opcode SEAudioMasterCallVstHost
  TSECallVstHostParams = record
    Opcode : Integer;
    Index  : Integer;
    Value  : Integer;
    Ptr    : Pointer;
    Opt    : Single;
  end;

  TSEEvent = record // a generic timestamped event
    TimeStamp : Cardinal;
    EventType : TUGEventType;
    IntParamA : Integer;
    IntParamB : Integer;
    PtrParam  : Pointer;
    Next      : PSEEvent; // Next in list (not used)
  end;

  PSEModuleProperties = ^TSEModuleProperties;
  TSEModuleProperties = record
    Name       : PChar;
    ID         : PChar;
    About      : PChar;
    Flags      : TUgFlags;
    GuiFlags   : TGuiFlags;
    SdkVersion : Integer;
  end;

  PSEPinProperties = ^TSEPinProperties;
  TSEPinProperties = record
    VariableAddress  : Pointer;
    Direction        : TSEDirection;
    Datatype         : TSEPlugDataType;
    Name             : PChar;
    DefaultValue     : PChar;
    DatatypeExtra    : PChar;
    Flags            : TSEIOFlags;
    Spare            : Integer;
  end;

  TAutoduplicatePlugData = record
    case Integer of
     0 : (FloatPtr   : PSingle);
     1 : (EnumValue  : Smallint);
     2 : (BoolValue  : Boolean);
     3 : (FloatValue : Single);
     4 : (TextPtr    : PPchar);
     5 : (IntValue   : Integer);
    end;

  TSEPin = class
  private
    FModule               : TSEModuleBase;
    FPinIndex             : Integer;
    FStatus               : TSEStateType;
    FDataType             : TSEPlugDataType;
    FVariablePtr          : Pointer;
    FAutoDuplicatePlugVar : TAutoduplicatePlugData; // Holds pointer to buffer (auto duplicate plugs only)
    FOnStatusupdate       : TSEPinStatusUpdateEvent;
    function GetIsConnected: Boolean;
    function GetValue: Double;
  protected
    procedure StatusUpdate(AStatus: TSEStateType);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Init(Module: TSEModuleBase; PinIndex: Integer; DataType: TSEPlugDataType; VariablePtr: Pointer);
    procedure TransmitStatusChange(SampleClock: Cardinal; NewState: TSEStateType);
    procedure TransmitMIDI(SampleClock, Msg: Cardinal);

    property IsConnected: Boolean read GetIsConnected;
    property Module: TSEModuleBase read FModule;
    property Value: Double read GetValue;
    property VariableAddress: Pointer read FVariablePtr;
  published
    property DataType: TSEPlugDataType read FDataType;
    property PinID: Integer read FPinIndex;
    property Status: TSEStateType read FStatus;

    // for audio plugs only
    property ValueNonAudio: TAutoduplicatePlugData read FAutoDuplicatePlugVar;

    property OnStatusupdate: TSEPinStatusUpdateEvent read FOnStatusupdate write FOnStatusupdate;
  end;

  TSEPins = array of TSEPin;

  TUgFunc = procedure of object;

  TSEModuleBase = class(TObject)
  private
    FOnOpen                  : TNotifyEvent;
    FOnClose                 : TNotifyEvent;
    FOnResume                : TNotifyEvent;
    FOnSampleRateChangeEvent : TNotifyEvent;
    FOnBlockSizeChangeEvent  : TNotifyEvent;
    FOnPlugStateChangeEvent  : TSEPlugStateChangeEvent;
    FOnProcessEvent          : TSE2ProcessEvent;
    FOnEventEvent            : TSE2EventEvent;
    FOnMidiData              : TSEMidiDataEvent;
    FOnInputStatusChange     : TSEInputStateChangedEvent;
    FOnGuiNotify             : TSEGuiNotifyEvent;
    FOnVoiceReset            : TSEVoiceResetEvent;

    function GetEffect: PSE2ModStructBase;
    function GetSampleClock: Cardinal;
    function GetPin(Index: Integer): TSEPin;
    function GetPinPropertiesClean(const Index: Integer; Properties: PSEPinProperties): Boolean;
    function GetTotalPinCount: Integer;
    procedure SetProcess(const Value: TSE2ProcessEvent);
    procedure SetSampleRate(const Value: Single);
    procedure SetBlockSize(const Value: Integer);
    procedure ProcessIdle(const BufferPos, SampleFrames: Integer);
  protected
    FSEAudioMaster : TSE2AudioMasterCallback;
    FEffect        : TSE2ModStructBase;
    FSampleRate    : Single;
    FBlockSize     : Integer;
    FPins          : TSEPins;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); virtual;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; virtual;
    function GetName(name: PChar): Boolean; virtual;     // name max 32 Char
    function GetUniqueId(name: PChar): Boolean; virtual; // id max 32 Char

    procedure Open; virtual;
    procedure Close; virtual;
    procedure Resume; virtual;
    procedure VoiceReset(Future: Integer); virtual;
    procedure SampleRateChanged; virtual;
    procedure BlockSizeChanged; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); virtual;
    procedure InputStatusChange(PlugIndex: Integer; NewState: TSEStateType); virtual;
    procedure MidiData(AClock, AMidiMsg: Cardinal; PinID: Integer); virtual;
    procedure GuiNotify(AUserMsgID: Integer; ASize: Integer; AData: Pointer); virtual;

    { divert to virtual function }
    procedure HandleEvent(Event: PSEEvent); virtual;
    function Dispatcher(Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; virtual;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); virtual;
    destructor Destroy; override;

    function ResolveFileName(const Pin: Integer): TFileName; overload;
    function ResolveFileName(const FileName: TFileName): TFileName; overload;

    procedure AddEvent(Event: TSEEvent);
    procedure RunDelayed(SampleClock: Cardinal; Func: TUgFunc);

    { inquiry }
    function CallHost(Opcode: TSEHostOpcodes; Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;

    property Effect: PSE2ModStructBase read GetEffect;
    property Pin[Index: Integer]: TSEPin read GetPin;
    property SampleClock: Cardinal read GetSampleClock;
    property TotalPinCount: Integer read GetTotalPinCount;
  published
    property SampleRate: Single read FSampleRate;
    property BlockSize: Integer read FBlockSize;

    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnProcess: TSE2ProcessEvent read FOnProcessEvent write SetProcess;
    property OnEvent: TSE2EventEvent read FOnEventEvent write FOnEventEvent;
    property OnMidiData: TSEMidiDataEvent read FOnMidiData write FOnMidiData;
    property OnSampleRateChange: TNotifyEvent read FOnSampleRateChangeEvent write FOnSampleRateChangeEvent;
    property OnBlockSizeChange: TNotifyEvent read FOnBlockSizeChangeEvent write FOnBlockSizeChangeEvent;
    property OnPlugStateChange: TSEPlugStateChangeEvent read FOnPlugStateChangeEvent write FOnPlugStateChangeEvent;
    property OnInputStateChanged: TSEInputStateChangedEvent read FOnInputStatusChange write FOnInputStatusChange;
  end;

//////////////////////
// static functions //
//////////////////////

function SE1Dispatcher(Effect: PSE1ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
function SE2Dispatcher(Effect: PSE2ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
procedure SE1Process(Effect: PSE1ModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer); cdecl;
procedure SE2Process(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
procedure SE2Event(ModuleBase: TSEModuleBase; Event: PSEEvent); cdecl;

// handy function to fix denormals.
procedure KillDenormal(var Sample: Single);
function IOFlagToString(IOFlag: TSEIOFlag): string;
function IOFlagsToString(IOFlags: TSEIOFlags): string;
function PropertyFlagsToString(Flags: TUgFlags): string;
function PropertyGUIFlagsToString(Flags: TGuiFlags): string;

implementation

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

constructor TSEPin.Create;
begin
 // nothing in here yet!
end;

destructor TSEPin.Destroy;
begin
 inherited;
end;

procedure TSEPin.Init(Module: TSEModuleBase; PinIndex: Integer; DataType: TSEPlugDataType; VariablePtr: Pointer);
begin
 FModule       := Module;
 FPinIndex     := PinIndex;
 FDataType     := DataType;
 FVariablePtr  := VariablePtr;
 FStatus       := stStop;

 // auto-duplicate pins hold their own pointer to the audio buffer
 if (FVariablePtr = nil) then
  begin
   case FDataType of
    dtFSample:
     begin
      FAutoDuplicatePlugVar.FloatPtr := PSingle(FModule.CallHost(SEAudioMasterGetPinVarAddress, FPinIndex));
      FVariablePtr := @FAutoDuplicatePlugVar.FloatPtr;
     end;
    dtEnum:
     begin
      FAutoDuplicatePlugVar.EnumValue := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.EnumValue;
     end;
    dtBoolean:
     begin
      FAutoDuplicatePlugVar.BoolValue := False;
      FVariablePtr := @FAutoDuplicatePlugVar.BoolValue;
     end;
    dtSingle:
     begin
      FAutoDuplicatePlugVar.FloatValue := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.FloatValue;
     end;
    dtInteger:
     begin
      FAutoDuplicatePlugVar.IntValue := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.IntValue;
     end;
    dtText:
     begin
      (* don't work
      // FAutoDuplicatePlugVar.TextPtr := 0;
      FAutoDuplicatePlugVar.TextPtr := PPChar(FModule.CallHost(SEAudioMasterGetPinVarAddress, FPinIndex));
      FVariablePtr := @FAutoDuplicatePlugVar.TextPtr;
      *)
     end;
  end;
 end else FAutoDuplicatePlugVar.FloatPtr := nil;
end;

function TSEPin.GetIsConnected: Boolean;
begin
 result := FModule.CallHost(SEAudioMasterIsPinConnected, FPinIndex, 0, nil) <> 0;
end;

procedure TSEPin.StatusUpdate(AStatus: TSEStateType);
begin
 FStatus := AStatus;
 Module.PlugStateChange(Self);
 if AStatus = stOneOff // one-offs need re-set once processed
  then FStatus := stStop;
end;

function TSEPin.GetValue: Double;
var
  BlockPos    : Cardinal;
  SampleClock : Cardinal;
  Buffer      : PDAVSingleFixedArray;
begin
 assert(FDataType = dtFSample);
 assert(assigned(FModule));
 BlockPos := FModule.CallHost(SEAudioMasterGetBlockStartClock, FPinIndex, 0, nil);
 SampleClock := FModule.SampleClock;
 Buffer := PDAVSingleFixedArray(FVariablePtr);
 result := Buffer[SampleClock - BlockPos];
end;

procedure TSEPin.TransmitStatusChange(SampleClock: Cardinal; NewState: TSEStateType);
begin
 assert(assigned(FModule));
 FModule.CallHost(SEAudioMasterSetPinStatus, FPinIndex, Integer(NewState), Pointer(SampleClock));
end;

procedure TSEPin.TransmitMIDI(SampleClock: Cardinal; Msg: Cardinal);
begin
 // MIDI data must allways be timestamped at or after the current 'clock'.
 assert(assigned(FModule));
 assert(SampleClock >= FModule.SampleClock);
 FModule.CallHost(SEAudioMasterSendMIDI, FPinIndex, Msg, Pointer(SampleClock));
end;

{ TSEModuleBase }

constructor TSEModuleBase.Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 FPins := nil;
 FSEAudioMaster := AudioMaster;

 FillChar(FEffect, SizeOf(TSE2ModStructBase), 0);

 FOnProcessEvent := ProcessIdle;
 with FEffect do
  begin
   Magic           := SepMagic;
   Version         := 1;
   HostPtr         := Reserved;
   SEModule        := Self;
   Dispatcher      := SE2Dispatcher;
   EventHandlerPtr := SE2Event;
   SubProcessPtr   := SE2Process;
  end;

 FSampleRate := 44100;
 FBlockSize := 1024;
end;

destructor TSEModuleBase.Destroy;
var
  i : Integer;
begin
 for i := 0 to Length(FPins) - 1
  do FreeAndNil(FPins[i]);
 SetLength(FPins, 0); 
 inherited;
end;


procedure TSEModuleBase.Open;
var
  i, ActualPlugCount   : Integer;
  PlugDescriptionIndex : Integer;
  Properties           : TSEPinProperties;
begin
 // set sampleclock
 //  SetSampleClock(CallHost(SEAudioMasterGetSampleClock, 0, 0, 0));

 // get actual number of pins used (may be more or less if auto-duplicating plugs used)
 ActualPlugCount := TotalPinCount;

 if ActualPlugCount > 0 then
  begin
   SetLength(FPins, ActualPlugCount);
   for i := 0 to ActualPlugCount - 1
    do FPins[i] := TSEPin.Create;
  end;

 // Set up standard plugs
 PlugDescriptionIndex := 0;
 i := 0;

 while (GetPinPropertiesClean(PlugDescriptionIndex, @Properties)) and (i < ActualPlugCount) do
  begin
   if (not (iofUICommunication in Properties.Flags)) or
      (          iofUIDualFlag in Properties.Flags) then // skip GUI plugs
    begin
     FPins[i].Init(Self, i, TSEPlugDataType(Properties.DataType), Properties.VariableAddress);
     inc(i);
    end;
   inc(PlugDescriptionIndex);
  end;

 // now set up any additional 'autoduplicate' plugs
 // Assumed they are the last plug described in GetPinProperties

 // Get the properites of last pin
 GetPinPropertiesClean(PlugDescriptionIndex - 1, @Properties);

 if (not (iofUICommunication in Properties.Flags)) or
    (          iofUIDualFlag in Properties.Flags) then // skip GUI plugs
   if (iofAutoDuplicate in Properties.Flags) then
    while (i < ActualPlugCount) do
     begin
      FPins[i].Init(Self, i, TSEPlugDataType(Properties.DataType), nil);
      inc(i);
     end;
 if assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TSEModuleBase.Close;
begin
 if assigned(FOnClose) then FOnClose(Self);
end;


function TSEModuleBase.Dispatcher(opCode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
var
  Event : TSEEvent;
begin
 result := 0;
 case opCode of
  seffOpen  : Open;
  seffClose : begin
               Close;
               Free;
               result := 1;
              end;
  seffSetSampleRate: setSampleRate(Opt);
  seffSetBlockSize: setBlockSize(Value);
  seffGetPinProperties:
   begin
    result := Integer(GetPinProperties(Index, PSEPinProperties(Ptr)));
(*
    // check for illegal flag combinations
    // 'Dual' Input plugs must be private (GuiModule can set the Value, but User must not be able to)
    assert((not iofUICommunicationDual in SEPinProperties(Ptr).Flags) or
               (iofPrivate in PSEPinProperties(Ptr).Flags) or
               (PSEPinProperties(Ptr).direction = drOut);
*)

    // 'Patch Store' Input plugs must be private, or GuiModule
    assert((not (iofPatchStore in PSEPinProperties(Ptr).Flags)) or
                (iofHidePin in PSEPinProperties(Ptr).Flags) or
                (PSEPinProperties(Ptr).Direction = drOut) or
                (iofUICommunication in PSEPinProperties(Ptr).Flags));
   end;
  seffGetModuleProperties:
   begin
    // obsolete
    // getModuleProperties ( (SEModuleProperties * )Ptr) ? 1 : 0;
    result := 0;
   end;
  seffGetEffectName:
   begin
//    StrCopy(Ptr, PChar('obsolete'));

    result := 0;
   end;
  seffGetUniqueId:
   begin
//    StrCopy(Ptr, PChar('obsolete'));
    result := 0;
   end;
  seffAddEvent:
   begin
    assert(False); // not used in SDK2
    Event := PSEEvent(Ptr)^; // can't directly use object allocated by host, make a copy
//    AddEvent(Event);
   end;
  seffResume:
   begin
    // Index = 0 - Module initiated sleep
    // Index = 1 - Voice initiated sleep. Indicates new note.
    Resume;
    if (Index > 0) then VoiceReset(Index);
   end;
  seffIsEventListEmpty:
   begin
    assert(false);  // not used in SDK2
//      return events == 0 ? 1 : 0;
   end;
  seffGetSdkVersion: result := CSeSdkVersion;
  seffGuiNotify: GuiNotify(Value, Index, Ptr);
  seffQueryDebugInfo:
   begin
(*
    static int info[4];
    info[0] = 1; // Version number
    info[1] = sizeof(process_function_ptr2);
    info[2] = (int) &m_process_function_pointer.pointer;
    info[3] = 0;
    return (long) info;
*)
   end;
  end;
end;

function TSEModuleBase.GetPin(Index: Integer): TSEPin;
begin
 if (Length(FPins) > Index) and (Index >= 0)
  then result := FPins[Index]
  else result := nil;
end;

function TSEModuleBase.CallHost(Opcode: TSEHostOpcodes; Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;
begin
 if assigned(FSEAudioMaster)
  then result := FSEAudioMaster(@FEffect, Opcode, Index, Value, Ptr, Opt)
  else result := 0;
end;

function TSEModuleBase.GetSampleClock: Cardinal;
begin
  result := CallHost(SEAudioMasterGetSampleClock);
end;

function TSEModuleBase.GetTotalPinCount: Integer;
begin
 result := CallHost(SEAudioMasterGetTotalPinCount);
end;

procedure TSEModuleBase.GuiNotify(AUserMsgID, ASize: Integer; AData: Pointer);
begin
 if assigned(FOnGuiNotify)
  then FOnGuiNotify(Self, AUserMsgID, ASize, AData);
end;

procedure TSEModuleBase.InputStatusChange(PlugIndex: Integer;
  NewState: TSEStateType);
begin
 if assigned(FOnInputStatusChange)
  then FOnInputStatusChange(Self, PlugIndex, NewState);
end;

procedure TSEModuleBase.MidiData(AClock, AMidiMsg: Cardinal; PinID: Integer);
begin
 if assigned(FOnMidiData)
  then FOnMidiData(Self, AClock, AMidiMsg, PinID);
end;

procedure TSEModuleBase.PlugStateChange(const CurrentPin: TSEPin);
begin
 if assigned(FOnPlugStateChangeEvent)
  then FOnPlugStateChangeEvent(Self, CurrentPin);
end;

procedure TSEModuleBase.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
end;

procedure TSEModuleBase.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TSEModuleBase.BlockSizeChanged;
begin
 if assigned(FOnBlockSizeChangeEvent)
  then FOnBlockSizeChangeEvent(Self);
end;

procedure TSEModuleBase.SampleRateChanged;
begin
 if assigned(FOnSampleRateChangeEvent)
  then FOnSampleRateChangeEvent(Self);
end;

procedure TSEModuleBase.SetProcess(const Value: TSE2ProcessEvent);
begin
 if @FOnProcessEvent <> @Value then
  begin
   if assigned(Value)
    then FOnProcessEvent := Value
    else FOnProcessEvent := ProcessIdle;
  end;
end;

procedure TSEModuleBase.VoiceReset(Future: Integer);
begin
 if assigned(FOnVoiceReset)
  then FOnVoiceReset(Self, Future);
end;

// gets a pins properties, after clearing the structure (prevents garbage getting in)
function TSEModuleBase.GetEffect: PSE2ModStructBase;
begin
 result := @FEffect;
end;

class procedure TSEModuleBase.GetModuleProperties(Properties: PSEModuleProperties);
var
  ModuleFileName : array[0..MAX_PATH] of Char;
begin
 // describe the plugin, this is the name the end-user will see.
 with Properties^ do
  begin
   Name := 'Delphi ASIO & VST Packages';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   try
    GetModuleFileName(HInstance, ModuleFileName, SizeOf(ModuleFileName));
//    FileName := ;
    ID := PChar(ExtractFileName(ModuleFileName));
   except
    ID := 'Delphi ASIO & VST Packages';
   end;

   // Info, may include Author, Web page whatever
   About := 'Delphi ASIO & VST Packages';
  end;
end;

function TSEModuleBase.GetName(name: PChar): Boolean;
begin
 result := False;
end;

function TSEModuleBase.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 {return m_sample_clock;}
 result := False;
end;

function TSEModuleBase.GetPinPropertiesClean(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 FillChar(Properties^, SizeOf(TSEPinProperties), 0); // clear structure
 result := GetPinProperties(Index, Properties);
end;

function TSEModuleBase.GetUniqueId(name: PChar): Boolean;
begin
 result := False;
end;

procedure TSEModuleBase.HandleEvent(Event: PSeEvent);
begin
 assert(Event.TimeStamp = SampleClock);
 case Event.EventType of
  uetStatChange: GetPin(Integer(Event.PtrParam)).StatusUpdate(TSEStateType(Event.IntParamA));
(* not used anymore
    case UET_RUN_FUNCTION: // buggy ( dll can't allocate mem and attach to event, causes crash when SE trys to free it)
      begin
        // PtrParam points to a ug_func pointer (allocated)
        function_pointer *fp = (function_pointer* ) Event.PtrParam;
        fp.Run;

        // TODO!!!!would be better to perform deletion in event detructor
        // will prevent mem leaks on events that are deleted without being used (due to power-off situation)
        delete fp;
        Event.PtrParam = NULL;
      end;
      break;
*)
  uetRunFunction2:
   begin
(* TODO!!!
//    ug_func Func = 0; // important to initialise (code below is a hack)
//    *(PInteger(@Func) = *(PInteger(@(Event.PtrParam));
//    (this.*(Func));
        my_delegate<ug_func> temp_delegate(Event.PtrParam);
//        temp_delegate.pointer.native = 0;
//        temp_delegate.pointer.raw_pointer = Event.PtrParam;
        (this.*(temp_delegate.pointer.native));
*)
   end;
(*  case UET_UI _NOTIFY2:
      GuiNotify( Event.IntParamA, (void * ) Event.IntParamB );
      free( (void * ) Event.IntParamB ); // free memory block
      break;*)
  uetProgChange: ; // do nothing
  uetMIDI : MidiData(Event.TimeStamp, Event.IntParamA, Event.IntParamB);
  else; // assert(false); // un-handled event
 end;
 if assigned(OnEvent)
  then OnEvent(Event)
end;

procedure TSEModuleBase.RunDelayed(SampleClock: Cardinal; Func: TUgFunc);
begin
//  my_delegate<ug_func> temp_delegate(Func);

//  temp_delegate.pointer.native = Func;
(*
  // NO, can't allocate events here (in dll)
//  function_pointer *fp = new function_pointer( this, Func );
  void *function_address;
  *( (int* ) &(function_address)) = *( (int* ) &Func); // copy first 32 bits of function pointer
*)

// AddEvent(SampleClock, Integer(uetRunFunction2), 0, 0, temp_delegate.RawPointer);
end;

// insert event sorted.  Pass pointer to tempory event structure(event data will be copied by SE)
procedure TSEModuleBase.AddEvent(Event: TSEEvent);
begin
 assert(Event.TimeStamp >= SampleClock);
 CallHost(SEAudioMasterAddEvent, 0, 0, @Event);

//  delete p_event;

(*
  unsigned long TimeStamp = p_event.TimeStamp;

  SeEvent *e = events;
  SeEvent *prev = 0;
  while(true)
  begin
    if( e == 0 || e.TimeStamp > TimeStamp ) // events with same time must be added in same order received (for stat changes to work properly)
    begin
      p_event.Next = e;
      if( prev )
      begin
        prev.Next = p_event;
      end;
      else
      begin
        events = p_event;
      end;
      return;
    end;
    prev = e;
    e = e.Next;
  end;*)
end;

function TSEModuleBase.ResolveFileName(const FileName: TFileName): TFileName;
begin
 SetLength(result, 256);
 CallHost(SEAudioMasterResolveFilename2, Integer(PChar(FileName)),
   Length(result), PChar(result));
end;

function TSEModuleBase.ResolveFileName(const Pin: Integer): TFileName;
var
  str: array[0..1023] of char;
begin
 CallHost(SEAudioMasterResolveFilename, Pin, Length(str), @str[0]);
 result := StrPas(str);
end;

procedure TSEModuleBase.Resume; // from either sleep or suspend
(*
var
  e : PSeEvent;
*)
begin
 // not used, see original SDK
(*
 SetSampleClock(CallHost(SEAudioMasterGetSampleClock, 0, 0, 0 ));

 // update the time on any pending events
 // this applies to resume from suspend only
 e := events;
 while (e <> nil) do
  begin
   if e.TimeStamp < SampleClock
    then e.TimeStamp := SampleClock;
   e = e.Next;
  end;
*)
 if assigned(FOnResume) then FOnResume(Self);
end;

procedure TSEModuleBase.ProcessIdle(const BufferPos, SampleFrames: Integer);
begin
 // do nothing here!
end;


//////////////////////
// static functions //
//////////////////////

function SE1Dispatcher(Effect: PSE1ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
 result := Effect.SEModule.Dispatcher(opCode, Index, Value, Ptr, Opt);
end;

function SE2Dispatcher(Effect: PSE2ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
 result := Effect.SEModule.Dispatcher(opCode, Index, Value, Ptr, Opt);
end;

procedure SE1Process(Effect: PSE1ModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer); cdecl;
begin
 // not yet supported
end;

procedure SE2Process(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
begin
 with ModuleBase do
  if assigned(OnProcess)
   then OnProcess(BufferOffset, SampleFrames);
end;

procedure SE2Event(ModuleBase: TSEModuleBase; Event: PSEEvent); cdecl;
begin
 with ModuleBase do HandleEvent(Event);
end;

procedure KillDenormal(var Sample: Single);
const
  CAntiDenormal: Single = 1E-18;
begin
 Sample := Sample + CAntiDenormal;
 Sample := Sample - CAntiDenormal;
end;

function IOFlagToString(IOFlag: TSEIOFlag): string;
begin
 case IOFlag of
  iofPolyphonicActive    : result := 'Polyphonic Active';
  iofIgnorePatchChange   : result := 'Ignore Patch Change';
  iofRename              : result := 'Rename';
  iofAutoDuplicate       : result := 'Auto Duplicate';
  iofFilename            : result := 'Filename';
  iofSetableOutput       : result := 'Setable Output';
  iofCustomisable        : result := 'Customisable';
  iofAdder               : result := 'Adder';
  iofHidePin             : result := 'Hide Pin';
  iofLinearInput         : result := 'Linear Input';
  iofUICommunication     : result := 'UI Communication';
  iofAutoEnum            : result := 'Auto Enum';
  iofHideWhenLocked      : result := 'Hide When Locked';
  iofParameterScreenOnly : result := 'Parameter Screen Only';
  iofDoNotCheckEnum      : result := 'Do Not Check Enum';
  iofUIDualFlag          : result := 'UI Dual Flag';
  iofPatchStore          : result := 'Patch Store';
  iofParamPrivate        : result := 'Parameter Private';
  iofMinimized           : result := 'Minimized';
  else                     result := '';
 end;
end;

function IOFlagsToString(IOFlags: TSEIOFlags): string;
begin
 result := '';
 if iofPolyphonicActive in IOFlags then result := 'Polyphonic Active';
 if iofIgnorePatchChange in IOFlags then result := result + 'Ignore Patch Change,';
 if iofRename in IOFlags then result := result + 'Rename,';
 if iofAutoDuplicate in IOFlags then result := result + 'Auto Duplicate,';
 if iofFilename in IOFlags then result := result + 'Filename,';
 if iofSetableOutput in IOFlags then result := result + 'Setable Output,';
 if iofCustomisable in IOFlags then result := result + 'Customisable,';
 if iofAdder in IOFlags then result := result + 'Adder,';
 if iofHidePin in IOFlags then result := result + 'Hide Pin,';
 if iofLinearInput in IOFlags then result := result + 'Linear Input,';
 if iofUICommunication in IOFlags then result := result + 'UI Communication,';
 if iofAutoEnum in IOFlags then result := result + 'Auto Enum,';
 if iofHideWhenLocked in IOFlags then result := result + 'Hide When Locked,';
 if iofParameterScreenOnly in IOFlags then result := result + 'Parameter Screen Only,';
 if iofDoNotCheckEnum in IOFlags then result := result + 'Do Not Check Enum,';
 if iofUIDualFlag in IOFlags then result := result + 'UI Dual Flag,';
 if iofPatchStore in IOFlags then result := result + 'Patch Store,';
 if iofParamPrivate in IOFlags then result := result + 'Parameter Private,';
 if iofMinimized in IOFlags then result := result + 'Minimized,';
 if Length(result) > 0 then SetLength(result, Length(result) - 1); 
end;

function PropertyFlagsToString(Flags: TUgFlags): string;
begin
 result := '';
 if ugfVoiceMonIgnore in Flags then result := result + 'Voice Monitor Ignore, ';
 if ugfPolyphonicAgregator in Flags then result := result + 'Polyphonic Agregator, ';
 if ugfSuspend in Flags then result := result + 'Suspended, ';
 if ugfOpen in Flags then result := result + 'Open, ';
 if ugfNeverSuspend in Flags then result := result + 'Never Suspend, ';
 if ugfClone in Flags then result := result + 'Clone, ';
 if ugfSendTimeinfoToHost in Flags then result := result + 'Send TimeInfo to Host';
 if result = '' then result := '-'
end;

function PropertyGUIFlagsToString(Flags: TGuiFlags): string;
begin
 result := '';
 if gfControlView in Flags then result := result + 'Control View, ';
 if gfStructureView in Flags then result := result + 'Structure View';
 if result = '' then result := '-'
end;

end.
