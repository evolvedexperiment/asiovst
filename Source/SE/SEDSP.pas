unit SEDSP;

interface

uses
  Classes, DAV_Common, SECommon;

const
  // module Flags, indicate special abilities
  UGF_VOICE_MON_IGNORE = $0002;     // DON'T WANT VOICE HELD OPEN BECAUSE A SCOPE IS CONNECTED
  UGF_POLYPHONIC_AGREGATOR = $0040; // A ug that always combines voices

  // read only
  UGF_SUSPENDED = $0080;
  UGF_OPEN = $0100;

  // normally when a voice has faded out, SE shuts all that voice's modules off
  // in very special cases you can prevent SE from shutting off your module
  UGF_NEVER_SUSPEND = $0200;

  UGF_CLONE = $0800;
  UGF_SEND_TIMEINFO_TO_HOST = $20000;

  // visible on control panel (applys to GuiFlags member)
  CF_CONTROL_VIEW = 128;
  CF_STRUCTURE_VIEW = 256;
////////////////////////////////////////////////////////////////////////////////

type
  TSEIOFlag =
    (iofPolyphonicActive,    // midi channel selection etc should should ignore patch changes
     iofIgnorePatchChange,   // auto-rename on new connection
     iofRename,              // plugs which are automaticly duplicated (like a container's 'spare' plug)
     iofAutoDuplicate,
     iofFilename,            // ALLOW User TO SET THE Value OF THIS OUTPUt eg on 'constant value' ug
     iofSetableOutput,       // plugs which can be duplicated/deleted by CUG
     iofCustomisable,        // plugs which handle multiple inputs, must belong to an Adder ug
     iofAdder,               // plugs which are private or obsolete, but are enabled on load if connected somewhere
     iofHidePin,             // = iofDisableIfPos = iofPrivate;
     iofLinearInput,         // set this if this input can handle more that one polyphonic voice
     iofUICommunication,
     iofAutoEnum,
     iofHideWhenLocked,
     iofParameterScreenOnly, // DON'T EXPOSE AS PLUG (TO SAVE SPACE)
     iofDoNotCheckEnum,
     iofUIDualFlag,          // don't use iofUIDualFlag by itself, use iofUICommunication
     iofPatchStore,          // Patch store is similar to dual but for DSP output plugs that appear as input plugs on UI (Output paramters) could consolodate?
     iofParamPrivate,        // Private parameter (not exposed to User of VST plugin)
     iofMinimized);          // minimised (not exposed on structure view (only properties window)
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
    SEAudioMasterGetSeVersion        // returns SE Version number times 100,000 ( e.g. 120000 is V 1.2 )
    (* EXAMPLE CALLING CODE
      int v = CallHost(SEAudioMasterGetSeVersion, 0, 0, 0);
    *)
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
  TSE2Event = function(ModuleBase: TSEModuleBase; Event: PSEEvent): Pointer; cdecl;

  TSESetParameter = procedure(Effect: PSE1ModStructBase; Index: Integer; Parameter: Single);
  TSEGetParameter = function(Effect: PSE1ModStructBase; Index: Integer): Single;

  TSE2EventEvent = function(Event: PSEEvent): Pointer of object;
  TSE2DispatcherEvent = function(Opcode: Integer; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer of object;
  TSE2ProcessEvent = procedure(BufferOffset: Integer; SampleFrames: Integer) of object;
  TSEPlugStateChangeEvent = procedure (Sender: TObject; Pin: TSEPin) of object; 

  TSE1AudioMasterCallback = function (Effect: PSE1ModStructBase; Opcode, Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;
  TSE2AudioMasterCallback = function (Effect: PSE2ModStructBase; Opcode, Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;

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
  public
    TimeStamp : Cardinal;
    EventType : TUGEventType;
    IntParamA : Integer;
    IntParamB : Integer;
    PtrParam  : Pointer;
    Next      : PSEEvent; // Next in list (not used)
    constructor Create(ATimeStamp: Cardinal; AEventType: TUGEventType; AIntParamA, AIntParamB: Integer; APtrParam: Pointer);
  end;

  PSEModuleProperties = ^TSEModuleProperties;
  TSEModuleProperties = record
    Name       : PChar;
    ID         : PChar;
    About      : PChar;
    Flags      : Integer;
    GuiFlags   : Integer;
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
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Init(Module: TSEModuleBase; PinIndex: Integer; DataType: TSEPlugDataType; VariablePtr: Pointer);
    function IsConnected: Boolean;

    procedure OnStatusUpdate(AStatus: TSEStateType);
    function GetStatus: TSEStateType;
    function GetValue: Double;

    // for audio plugs only
    function getValueNonAudio: TAutoduplicatePlugData;

    function GetDataType: TSEPlugDataType;
    procedure TransmitStatusChange(SampleClock: Cardinal; NewState: TSEStateType);
    procedure TransmitMIDI(SampleClock, Msg: Cardinal);

    function GetModule: TSEModuleBase;
    function GetPinID: Integer;
    function GetVariableAddress: Pointer;
  end;

  TSEPins = array of TSEPin;

  TUgFunc = procedure of object;

  TSEModuleBase = class(TObject)
  private
    FOnSampleRateChangeEvent : TNotifyEvent;
    FOnBlockSizeChangeEvent  : TNotifyEvent;
    FOnPlugStateChangeEvent  : TSEPlugStateChangeEvent;
    FOnProcessEvent          : TSE2ProcessEvent;
    FOnEventEvent            : TSE2EventEvent;
    procedure SetProcess(const Value: TSE2ProcessEvent);
  protected
    FSEAudioMaster : TSE2AudioMasterCallback;
    FEffect        : TSE2ModStructBase;
    FSampleRate    : Single;
    FBlockSize     : Integer;
    FPins          : TSEPins;

    procedure PlugStateChange(Pin: TSEPin); virtual;
    procedure InputStatusChange(PlugIndex: Integer; NewState: TSEStateType); virtual;
    procedure MidiData(AClock, AMidiMsg: Cardinal; PinID: ShortInt); virtual;
    procedure GuiNotify(AUserMsgID: Integer; ASize: Integer; AData: Pointer); virtual;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); virtual;
    destructor Destroy; override;

    procedure AddEvent(ATimeStamp: Cardinal; AEventType: TUGEventType; AIntParamA: Integer = 0; AIntParamB: Integer = 0; APtrParam : Pointer = nil);
    procedure RunDelayed(SampleClock: Cardinal; Func: TUgFunc);

    function GetEffect: PSE2ModStructBase;

    { called from audio master }
    procedure ProcessIdle(StartPos, SampleFrames: Integer);
//    procedure ProcessReplacing(BufferOffset, SampleFrames: Integer); not used anymore

    { divert to virtual function }
    procedure HandleEvent(Event: PSEEvent); virtual;
    function Dispatcher(Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    procedure SetSampleRate(const Value: Single); virtual;
    procedure SetBlockSize(const Value: Integer); virtual;

    { inquiry }
    function CallHost(opcode: TSEHostOpcodes; Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;

    function GetPin(Index: Integer): TSEPin;
    function SampleClock: Cardinal;
(* not used anymore
//    procedure SetSampleClock(AClock: Cardinal); {m_sample_clock = AClock;}
//    function getModuleProperties(Properties: PSEModuleProperties): Boolean; virtual; { return false;}
*)
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; virtual;
    function GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean; virtual;
    function GetPinPropertiesClean(Index: Integer; Properties: PSEPinProperties): Boolean;

    function GetName(name: PChar): Boolean; virtual;     // name max 32 Char
    function GetUniqueId(name: PChar): Boolean; virtual; // id max 32 Char

    procedure Resume; virtual;
    procedure VoiceReset(Future: Integer); virtual;
  published
    property SampleRate: Single read FSampleRate;
    property BlockSize: Integer read FBlockSize;
    property OnProcess: TSE2ProcessEvent read FOnProcessEvent write SetProcess;
    property OnProcessEvent: TSE2EventEvent read FOnEventEvent write FOnEventEvent;
    property OnSampleRateChange: TNotifyEvent read FOnSampleRateChangeEvent write FOnSampleRateChangeEvent;
    property OnBlockSizeChangeEvent: TNotifyEvent read FOnBlockSizeChangeEvent write FOnBlockSizeChangeEvent;
    property OnPlugStateChange: TSEPlugStateChangeEvent read FOnPlugStateChangeEvent write FOnPlugStateChangeEvent;
  end;

//////////////////////
// static functions //
//////////////////////

function SE1Dispatcher(Effect: PSE1ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
function SE2Dispatcher(Effect: PSE2ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
procedure SE1Process(Effect: PSE1ModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer); cdecl;
procedure SE2Process(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
function SE2Event(ModuleBase: TSEModuleBase; Event: PSEEvent): Pointer; cdecl;

// handy function to fix denormals.
procedure KillDenormal(var Sample: Single);

implementation

{ TSEEvent }

constructor TSEEvent.Create(ATimeStamp: Cardinal; AEventType: TUGEventType;
  AIntParamA, AIntParamB: Integer; APtrParam: Pointer);
begin
 TimeStamp := ATimeStamp;
 EventType := AEventType;
 IntParamA := AIntParamA;
 IntParamB := AIntParamB;
 PtrParam  := APtrParam;
 Next      := nil;
end;

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

function TSEPin.IsConnected: Boolean;
begin
 result := FModule.CallHost(SEAudioMasterIsPinConnected, FPinIndex, 0, nil) <> 0;
end;

procedure TSEPin.OnStatusUpdate(AStatus: TSEStateType);
begin
 FStatus := AStatus;
 getModule.PlugStateChange(Self);
 if AStatus = stOneOff // one-offs need re-set once processed
  then FStatus := stStop;
end;

function TSEPin.GetDataType: TSEPlugDataType;
begin
 result := FDataType;
end;

function TSEPin.GetModule: TSEModuleBase;
begin
 result := FModule;
end;

function TSEPin.GetPinID: Integer;
begin
 result := FPinIndex;
end;

function TSEPin.GetStatus: TSEStateType;
begin
 result := FStatus;
end;

function TSEPin.GetValue: Double;
var
  BlockPos    : Cardinal;
  SampleClock : Cardinal;
  Buffer      : PDAVSingleFixedArray;
begin
 assert(FDataType = dtFSample);
 BlockPos := FModule.CallHost(SEAudioMasterGetBlockStartClock, FPinIndex, 0, nil);
 SampleClock := FModule.SampleClock;
 Buffer := PDAVSingleFixedArray(FVariablePtr);
 result := Buffer[SampleClock - BlockPos];
end;

function TSEPin.getValueNonAudio: TAutoduplicatePlugData;
begin
 result := FAutoDuplicatePlugVar;
end;

function TSEPin.GetVariableAddress: Pointer;
begin
 result := FVariablePtr;
end;

procedure TSEPin.TransmitStatusChange(SampleClock: Cardinal; NewState: TSEStateType);
begin
 FModule.CallHost(SEAudioMasterSetPinStatus, FPinIndex, Integer(NewState), Pointer(SampleClock));
end;

procedure TSEPin.TransmitMIDI(SampleClock: Cardinal; Msg: Cardinal);
begin
  // MIDI data must allways be timestamped at or after the current 'clock'.
  assert(SampleClock >= FModule.SampleClock);
  FModule.CallHost(SEAudioMasterSendMIDI, FPinIndex, Msg, Pointer(SampleClock));
end;

{ TSEModuleBase }

constructor TSEModuleBase.Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 FPins := nil;
 FSEAudioMaster := AudioMaster;

 FillChar(FEffect, SizeOf(TSE2ModStructBase), 0);
 FEffect.Magic := SepMagic;

 FOnProcessEvent := ProcessIdle;
 with FEffect do
  begin
   SubProcessPtr   := SE2Process;
   EventHandlerPtr := SE2Event;
   Dispatcher      := SE2Dispatcher;
   HostPtr         := Reserved;
   SEModule        := Self;
   Version         := 1;
  end;

 FSampleRate := 44100;
 FBlockSize := 1024;
end;

destructor TSEModuleBase.Destroy;
var
  i : Integer;
begin
 for i := 0 to Length(FPins) - 1
  do FPins[i].Free;
 SetLength(FPins, 0); 
 inherited;
end;



procedure TSEModuleBase.Close;
begin
 // do nothing!
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
 ActualPlugCount := CallHost(SEAudioMasterGetTotalPinCount);

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
 GetPinPropertiesClean(PlugDescriptionIndex-1, @properties);

 if (not (iofUICommunication in Properties.Flags)) or
    (          iofUIDualFlag in Properties.Flags) then // skip GUI plugs
   if (iofAutoDuplicate in Properties.Flags) then
    while (i < ActualPlugCount) do
     begin
      FPins[i].Init(Self, i, TSEPlugDataType(Properties.DataType), nil);
      inc(i);
     end;
end;


function TSEModuleBase.Dispatcher(opCode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
begin
 result := 0;
 case opCode of
  seffOpen  : Open;
  seffClose : begin
               Close;
               // delete this;
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
//      assert( ((PSEPinProperties(Ptr).Flags & IO_UI_COMMUNICATION_DUAL) == 0 || ((PSEPinProperties(Ptr).Flags & IO_PRIVATE) != 0 || (PSEPinProperties(Ptr).direction == DR_OUT );
*)

      // 'Patch Store' Input plugs must be private, or GuiModule
//      assert( ((PSEPinProperties(Ptr).Flags & IO_PATCH_STORE) == 0 || (((SEPinProperties*)Ptr).Flags & IO_PRIVATE) != 0 || ((SEPinProperties*)Ptr).direction == DR_OUT || ( ( (SEPinProperties*)Ptr).Flags & IO_UI_COMMUNICATION) != 0 );
   end;
(* obsolete
    case seffGetModuleProperties:
      result := getModuleProperties ( (SEModuleProperties * )Ptr) ? 1 : 0;
      break;
//    case seffInputStatusChange:
//      InputStatusChange(Index, (state_type) Value);
//      result := 0;
//      break;
    case seffGetEffectName:
      result := GetName ((Char * )Ptr) ? 1 : 0;
      break;
    case seffGetUniqueId:
      result := GetUniqueId ((Char * )Ptr) ? 1 : 0;
      break;
*)
  seffAddEvent:
   begin
    assert(False); // not used in SDK2
(*
    SeEvent *e = (SeEvent * )Ptr;

    SeEvent *ne = new SeEvent(e.TimeStamp, e.EventType, e.IntParamA, e.IntParamB, e.PtrParam );
    // can't directly use object allocated by host, make a copy
    AddEvent( ne );
*)
   end;
  seffResume:
   begin
    // Index = 0 - Module initiated sleep
    // Index = 1 - Voice initiated sleep. Indicates new note.
    Resume;
    if( Index > 0 )
     then VoiceReset(Index);
   end;
  seffIsEventListEmpty:
   begin
      assert(false);  // not used in SDK2
//      return events == 0 ? 1 : 0;
   end;
(* retired
    case seffGetSdkVersion:
      return SDK_VERSION;
      break;
*)
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
  then result := FSEAudioMaster(@FEffect, Integer(Opcode), Index, Value, Ptr, Opt)
  else result := 0;
end;

function TSEModuleBase.SampleClock: Cardinal;
begin
  result := CallHost(SEAudioMasterGetSampleClock);
end;

procedure TSEModuleBase.GuiNotify(AUserMsgID, ASize: Integer;
  AData: Pointer);
begin
(*
 if assigned(FOnGuiNotify)
  then FOnGuiNotify(Self, AUserMsgID, ASize, AData);
*)
end;

procedure TSEModuleBase.InputStatusChange(PlugIndex: Integer;
  NewState: TSEStateType);
begin
(*
 if assigned(FOnInputStatusChange)
  then FOnInputStatusChange(Self, PlugIndex, NewState);
*)
end;

procedure TSEModuleBase.MidiData(AClock, AMidiMsg: Cardinal; PinID: ShortInt);
begin
(*
 if assigned(FOnMidiData)
  then FOnMidiData(Self, AClock, AMidiMsg, PinID);
*)
end;

procedure TSEModuleBase.PlugStateChange(Pin: TSEPin);
begin
 if assigned(FOnPlugStateChangeEvent)
  then FOnPlugStateChangeEvent(Self, Pin);
end;

procedure TSEModuleBase.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   if assigned(FOnBlockSizeChangeEvent)
    then FOnBlockSizeChangeEvent(Self);
  end;
end;

procedure TSEModuleBase.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   if assigned(FOnSampleRateChangeEvent)
    then FOnSampleRateChangeEvent(Self);
  end;
end;

procedure TSEModuleBase.SetProcess(const Value: TSE2ProcessEvent);
begin
 if @FOnProcessEvent <> @Value then
  begin
   FOnProcessEvent := Value;
(*
//   cEffect.sub_process_ptr := (long)m_process_function_pointer.RawPointer;
//   CallHost(SEAudioMasterSetProcessFunction, 0, 0, 0);
*)
  end;
end;

procedure TSEModuleBase.VoiceReset(Future: Integer);
begin
 // do nothing!
end;

// gets a pins properties, after clearing the structure (prevents garbage getting in)
function TSEModuleBase.GetEffect: PSE2ModStructBase;
begin
 result := @FEffect;
end;

class function TSEModuleBase.GetModuleProperties(Properties: PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Delphi ASIO & VST Packages';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.

 // generates a random ID (will surely fail!!!)
 GetMem(Properties.ID, 4);
 Properties.ID[0] := Char(32 + random(64));
 Properties.ID[1] := Char(32 + random(64));
 Properties.ID[2] := Char(32 + random(64));
 Properties.ID[3] := Char(32 + random(64));

 // Info, may include Author, Web page whatever
 Properties.About := 'Delphi ASIO & VST Packages';
 result := True;
end;

function TSEModuleBase.GetName(name: PChar): Boolean;
begin
 result := False;
end;

function TSEModuleBase.GetPinProperties(Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 {return m_sample_clock;}
 result := False;
end;

function TSEModuleBase.GetPinPropertiesClean(Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 FillChar(Properties^, SizeOf(TSEPinProperties), 0); // clear structure
 result := GetPinProperties(Index, Properties);
end;

function TSEModuleBase.GetUniqueId(name: PChar): Boolean;
begin
 result := False;
end;

(* not used anymore!!!
procedure TSEModuleBase.ProcessReplacing(BufferOffset, SampleFrames: Integer);
var
  CurrentSampleClock : Cardinal;
  EndTime, DeltaTime : Cardinal;
  e, NextEvent       : PSeEvent;
begin
  assert(SampleFrames > 0);

  CurrentSampleClock := SampleClock;
  EndTime := CurrentSampleClock + SampleFrames;

  while false do
   begin
{
    if (events = 0) // fast Version, when no events on list.
     begin
      assert(SampleFrames > 0 );
      (Self.*(ProcessFunction))(BufferOffset, SampleFrames);
      SetSampleClock(EndTime);
      exit;
     end;

    DeltaTime := SampleFrames;

    NextEvent := events;

    assert(NextEvent.TimeStamp >= CurrentSampleClock );

    if( NextEvent.TimeStamp < EndTime ) // will happen in this block
     begin
      DeltaTime = NextEvent.TimeStamp - CurrentSampleClock;
      // no, sub_process needs to know if event pending
      // events.RemoveHead;
     end;

    if( DeltaTime > 0 )
     begin
      //assert(DeltaTime > 0 ); // it's unsigned silly
      (this.*(ProcessFunction))( BufferOffset, DeltaTime );
      SampleFrames -= DeltaTime;
      CurrentSampleClock += DeltaTime;
      SetSampleClock( CurrentSampleClock );

      if (SampleFrames == 0)
       then exit;// done
      BufferOffset := BufferOffset + DeltaTime;
     end;

    e = events;
    events = events.Next;
    HandleEvent(e);
    delete e;
}

   exit; // emergency exit, pls remove if the above part has been completely translated
  end;
end;
*)

procedure TSEModuleBase.HandleEvent(Event: PSeEvent);
begin
 assert(Event.TimeStamp = SampleClock);
 case Event.EventType of
//  uetStatChange: GetPin(Integer(Event.PtrParam)).OnStatusUpdate((state_type) e.IntParamA );
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
  uetRunFunction:
   begin
(*
//    ug_func Func = 0; // important to initialise (code below is a hack)
//    *( (int* ) &Func) = *( (int* ) &(Event.PtrParam));
//    (this.*(Func));
        my_delegate<ug_func> temp_delegate(Event.PtrParam);
//        temp_delegate.pointer.native = 0;
//        temp_delegate.pointer.raw_pointer = Event.PtrParam;
        (this.*(temp_delegate.pointer.native));
*)
   end;
(*  case UET_UI _NOTIFY2:
      OnGui Notify( Event.IntParamA, (void * ) Event.IntParamB );
      free( (void * ) Event.IntParamB ); // free memory block
      break;*)
  uetProgChange: ; // do nothing
  uetMIDI : MidiData(Event.TimeStamp, Event.IntParamA, Event.IntParamB);
  else; // assert(false); // un-handled event
 end;
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
procedure TSEModuleBase.AddEvent(ATimeStamp: Cardinal; AEventType: TUGEventType; AIntParamA, AIntParamB: Integer; APtrParam: Pointer);
var
  temp : TSeEvent;
begin
  assert(ATimeStamp >= SampleClock);

  temp.Create(ATimeStamp, AEventType, AIntParamA, AIntParamB, APtrParam);
  CallHost(SEAudioMasterAddEvent, 0, 0, @temp);

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
end;

procedure TSEModuleBase.ProcessIdle(StartPos, SampleFrames: Integer);
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

function SE2Event(ModuleBase: TSEModuleBase; Event: PSEEvent): Pointer; cdecl;
begin
 with ModuleBase do
  if assigned(OnProcessEvent)
   then result := OnProcessEvent(Event)
   else result := nil;
end;

procedure KillDenormal(var Sample: Single);
const
  CAntiDenormal: Single = 1E-18;
begin
 Sample := Sample + CAntiDenormal;
 Sample := Sample - CAntiDenormal;
end;

end.
