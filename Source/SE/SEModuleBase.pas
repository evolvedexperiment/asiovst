unit SEModuleBase;

interface

uses
  Classes, DAV_Common, SEDataTypes, SEModStruct, SEModStructBase;

type
  PSEModStructBase  = ^TSEModStructBase;

(*
  // carefull of clash with SE's use of this
  // typedef void (SEModule_base::* process_func_ptr)(long start_pos, long sampleframes); // Pointer to sound processing member function

  // Needs to be defined by the audio effect and is
  // called to create the audio effect object instance.
  // createEffectInstance(SEAudioMaster: SEAudioMasterCallback): PSEModule_base;



// handy macro to assign a function pointer
#define SET_PROCESS_FUNC(func)  setSubProcess( static_cast <process_function_ptr2> (&func) )

// macro to run a Module function at a later time, specified as a sampleframe
#define RUN_AT(sample_clock, func) RunDelayed( sample_clock, static_cast <ug_func> (func) )
*)

(*
template <typename member_pointer_type> class my_delegate
{
public:
 my_delegate(member_pointer_type p_native = 0) {pointer.native = p_native;};
 my_delegate(void *p_raw){ pointer.native = 0; pointer.raw_data[0] = p_raw;};
 void *RawPointer(void) {return pointer.raw_data[0];};

 union {
  member_pointer_type native;
  void * raw_data[2];
 } pointer; //different compilers use different layouts. may be more than 32 bits
};

*)

  function dispatchEffectClass(E: PSEModStructBase; opCode: TSEPluginModuleOpcodes; index, value: Integer; ptr: Pointer; opt: Single): Integer;

//  procedure processClassReplacing(E: PSEModStructBase; Inputs, Outputs: PDAVArrayOfDoubleFixedArray; SampleFrames: Integer);

type
  PSEModuleBase     = ^TSEModuleBase;
  TSEModuleBase     = class;
//  TSEPin            = class;

  TUgFunc = procedure of object;
  TFunctionPointer = class
  private
    FUg   : PSEModuleBase;
    FFunc : TUGFunc;
  public
    constructor Create(Ug: PSEModuleBase; Func: TUgFunc);
    procedure Run;
  end;

  TSEModuleBase = class(TObject)
//    m_process_function_pointer: my_delegate<process_function_ptr2>;
  protected
    FSEAudioMaster : TSEAudioMasterCallback2;
    FEffect        : TSEModStructBase2;
    FSampleRate    : Single;
    FBlockSize     : Integer;
//    Fpins          : PSEPin;
{
friend long dispatchEffectClass(SEMod_struct_base *e, long opCode, long index, long value, void *ptr, float opt);
friend void processClassReplacing(SEMod_struct_base *e, float **inputs, float **outputs, long sampleFrames);
}
  public
    constructor Create(audioMaster: TSEAudioMasterCallback2; p_resvd1: Pointer);
    destructor Destroy; override;

    procedure AddEvent(p_time_stamp: Cardinal; p_event_type: TUGEventType; p_int_parm1: Integer = 0; p_int_parm2: Integer = 0; p_ptr_parm1 : Pointer = nil);
    procedure RunDelayed(sample_clock: Cardinal; func: TUgFunc);

//    procedure OnPlugStateChange(pin: PSEPin); virtual; abstract;
    function getAeffect: PSEModStructBase2;

    { called from audio master }
    procedure ProcessIdle(start_pos, sampleframes: Integer); cdecl;
    procedure Private_HandleEvent(Event: PSEEvent); cdecl; // divert to virtual function

    { divert to virtual function }
    procedure HandleEvent(Event: PSEEvent); virtual;
    function Dispatcher(Opcode: TSEPluginModuleOpcodes; index, value: Integer; ptr: Pointer; opt: Single): Integer; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    procedure OnInputStatusChange(PlugIndex: Integer; NewState: TStateType); virtual;
    procedure SetSampleRate(opt: Single); virtual;
    procedure setBlockSize(bs: Integer); virtual;

    { inquiry }
    function getSampleRate: Single; virtual;
    function getBlockSize: Integer; virtual;
    function CallHost(opcode: TSEHostOpcodes; index: Integer = 0; value: Integer = 0; ptr: Pointer = 0; opt: Single = 0): Integer;

//    function getPin(index: longint): PSEPin;
    function SampleClock: Cardinal; {return m_sample_clock;}
(*
//    procedure SetSampleClock(p_clock: Cardinal); {m_sample_clock = p_clock;}
//    function getModuleProperties(Properties: PSEModuleProperties): Boolean; virtual; { return false;}
*)
    function getPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean; virtual;
    function getPinProperties_clean(index: Integer; Properties: PSEPinProperties): Boolean;

    function getName(name: PChar): Boolean; virtual;     // name max 32 char
    function getUniqueId(name: PChar): Boolean; virtual; // id max 32 char
//    function getSubProcess: TProcessFunctionPtr2;

    procedure Resume; virtual;
    procedure VoiceReset(future: Integer); virtual;
    procedure OnMidiData(p_clock, midi_msg: Cardinal; pin_id: ShortInt); virtual;
    procedure OnGuiNotify(p_user_msg_id: Integer; p_size: Integer; p_data: Pointer); virtual;
  end;

implementation

uses
  SEPin;

{ TFunctionPointer }

constructor TFunctionPointer.Create(Ug: PSEModuleBase; Func: TUgFunc);
begin
 FUg   := ug;
 FFunc := func;
end;

procedure TFunctionPointer.Run;
begin
// FUg.Func;
end;

function dispatchEffectClass(E: PSEModStructBase; opCode: TSEPluginModuleOpcodes; index, value: Integer; ptr: Pointer; opt: Single): Integer;
begin
  result := PSEModuleBase(e.vobject).dispatcher(opCode, index, value, ptr, opt);
end;

constructor TSEModuleBase.Create(audioMaster: TSEAudioMasterCallback2; p_resvd1: Pointer);
begin
// Fpins := nil
 FSEAudioMaster := audioMaster;

(*
  memset(&cEffect, 0, sizeof(cEffect));
  cEffect.magic = SepMagic;

  m_process_function_pointer.pointer.native = &TSEModuleBase.process_idle;
  cEffect.sub_process_ptr = (long) m_process_function_pointer.RawPointer();

  event_function_ptr temp2 = (event_function_ptr) &TSEModuleBase.private_HandleEvent;
//  cEffect.event_handler_ptr = *reinterpret_cast<long*>( &temp2 );
  
  //test
  int * t = reinterpret_cast<int*>( &temp2 );
  /* old, don't work on newer gcc
#if defined(MINGW) || defined(__GNUC__)
  cEffect.event_handler_ptr = t[1];
#else
  cEffect.event_handler_ptr = t[0];
#endif
*/
  // new. extract pointer-to-member address
  if( t[0] != 0 && t[0] != 0xffff0000) // GNU c++ (old) had ffff0000 in first dword
  begin
    cEffect.event_handler_ptr = t[0]; // MS and newer gcc (V 3.3.1 was first I noticed)
  end;
  else
  begin
    cEffect.event_handler_ptr = t[1]; // older gcc
  end;

  cEffect.dispatcher = &dispatchEffectClass;
  cEffect.resvd1 = p_resvd1;
  cEffect.object = this;
  cEffect.version = 1;
*)
  FSampleRate := 44100;
  FBlockSize := 1024;
end;

destructor TSEModuleBase.Destroy;
begin
//  Dispose(FPins);
end;

function TSEModuleBase.Dispatcher(opCode: TSEPluginModuleOpcodes; index, value: Integer; ptr: Pointer; opt: Single): Integer;
begin
 result := 0;
 case opCode of
  seffOpen  : Open;
  seffClose : begin
               Close;
               // delete this;
               result := 1;
              end;
  seffSetSampleRate: setSampleRate(opt);
  seffSetBlockSize: setBlockSize(value);
  seffGetPinProperties:
   begin
    result := Integer(getPinProperties(index, PSEPinProperties(ptr)));


(*
      // check for illegal flag combinations
      // 'Dual' Input plugs must be private (GuiModule can set the value, but User must not be able to)
//      assert( ((PSEPinProperties(ptr).flags & IO_UI_COMMUNICATION_DUAL) == 0 || ((PSEPinProperties(ptr).flags & IO_PRIVATE) != 0 || (PSEPinProperties(ptr).direction == DR_OUT );
*)

      // 'Patch Store' Input plugs must be private, or GuiModule
//      assert( ((PSEPinProperties(ptr).flags & IO_PATCH_STORE) == 0 || (((SEPinProperties*)ptr).flags & IO_PRIVATE) != 0 || ((SEPinProperties*)ptr).direction == DR_OUT || ( ( (SEPinProperties*)ptr).flags & IO_UI_COMMUNICATION) != 0 );
   end;
(* obsolete
    case seffGetModuleProperties:
      result := getModuleProperties ( (SEModuleProperties * )ptr) ? 1 : 0;
      break;
//    case seffInputStatusChange:
//      OnInputStatusChange(index, (state_type) value);
//      result := 0;
//      break;
    case seffGetEffectName:
      result := getName ((char * )ptr) ? 1 : 0;
      break;
    case seffGetUniqueId:
      result := getUniqueId ((char * )ptr) ? 1 : 0;
      break;
*)
  seffAddEvent:
   begin
    assert(False); // not used in SDK2
(*
    SeEvent *e = (SeEvent * )ptr;

    SeEvent *ne = new SeEvent(e.time_stamp, e.event_type, e.int_parm1, e.int_parm2, e.ptr_parm1 );
    // can't directly use object allocated by host, make a copy
    AddEvent( ne );
*)
   end;
  seffResume:
   begin
    // index = 0 - Module initiated sleep
    // index = 1 - Voice initiated sleep. Indicates new note.
    Resume;
    if( index > 0 )
     then VoiceReset(index);
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
  seffGuiNotify: OnGuiNotify(value, index, ptr);
  seffQueryDebugInfo:
   begin
(*
    static int info[4];
    info[0] = 1; // version number
    info[1] = sizeof(process_function_ptr2);
    info[2] = (int) &m_process_function_pointer.pointer;
    info[3] = 0;
    return (long) info;
*)
   end;
  end;
end;

(*
function TSEModuleBase.GetPin(index: Integer): PSEPin;
begin
 assert(Fpins <> 0);
 result := FPins + Index;
end;
*)

function TSEModuleBase.CallHost(Opcode: TSEHostOpcodes; index, value: Integer; ptr: Pointer; opt: Single): Integer;
begin
 if assigned(FSEAudioMaster)
  then result := FSEAudioMaster(@FEffect, Integer(opcode), index, value, ptr, opt)
  else result := 0;
end;

procedure TSEModuleBase.Close;
begin
 // do nothing!
end;

function TSEModuleBase.SampleClock: Cardinal;
begin
  result := CallHost(SEAudioMasterGetSampleClock);
end;

procedure TSEModuleBase.OnGuiNotify(p_user_msg_id, p_size: Integer;
  p_data: Pointer);
begin
 // do nothing!
end;

procedure TSEModuleBase.OnInputStatusChange(PlugIndex: Integer;
  NewState: TStateType);
begin
 // do nothing!
end;

procedure TSEModuleBase.OnMidiData(p_clock, midi_msg: Cardinal;
  pin_id: ShortInt);
begin
 // do nothing here!
end;

procedure TSEModuleBase.setBlockSize(bs: Integer);
begin
 FBlockSize := bs;
end;

procedure TSEModuleBase.SetSampleRate(opt: Single);
begin
 FSampleRate := opt;
end;

procedure TSEModuleBase.VoiceReset(future: Integer);
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
// //  SetSampleClock(CallHost(SEAudioMasterGetSampleClock, 0, 0, 0));

  // get actual number of pins used (may be more or less if auto-duplicating plugs used)
  ActualPlugCount := CallHost(SEAudioMasterGetTotalPinCount);

(*
  if ActualPlugCount > 0
   then m_pins = new SEPin[ActualPlugCount];
*)

  // Set up standard plugs
  PlugDescriptionIndex := 0;
  i := 0;

  while (getPinProperties_clean(PlugDescriptionIndex, @Properties)) and (i < ActualPlugCount) do
   begin
    if ((properties.flags and CIO_UI_COMMUNICATION) = 0) or
        (properties.flags and CIO_UI_DUAL_FLAG <> 0) then // skip GUI plugs
     begin
//      m_pins[i].Init(this, i, properties.datatype, properties.variable_address);
      inc(i);
     end;
    inc(PlugDescriptionIndex);
   end;

  // now set up any additional 'autoduplicate' plugs
  // Assumed they are the last plug described in getPinProperties

  // Get the properites of last pin
  getPinProperties_clean(PlugDescriptionIndex-1, @properties);

  if ((properties.flags and CIO_UI_COMMUNICATION = 0) or
      (properties.flags and CIO_UI_DUAL_FLAG <> 0)) then // skip GUI plugs
   begin
    if (properties.flags and CIO_AUTODUPLICATE <> 0) then
     begin
      while (i < ActualPlugCount) do
       begin
//        m_pins[i].Init(this, i, properties.datatype, 0);
        inc(i);
       end;
     end;
   end;
end;

// gets a pins properties, after clearing the structure (prevents garbage getting in)
function TSEModuleBase.getAeffect: PSEModStructBase2;
begin

end;

function TSEModuleBase.getBlockSize: Integer;
begin
 result := FBlockSize;
end;

function TSEModuleBase.getName(name: PChar): Boolean;
begin
 result := False;
end;

function TSEModuleBase.getPinProperties(index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := False;
end;

function TSEModuleBase.getPinProperties_clean (index: Integer; properties: PSEPinProperties): Boolean;
begin
 FillChar(Properties, SizeOf(TSEPinProperties), 0); // clear structure
 result := getPinProperties(index, Properties);
end;

function TSEModuleBase.getSampleRate: Single;
begin
 result := FsampleRate;
end;

function TSEModuleBase.getUniqueId(name: PChar): Boolean;
begin
 result := False;
end;

(*
procedure TSEModuleBase.processReplacing(BufferOffset, SampleFrames: Integer); cdecl;
var
  CurrentSampleClock : Cardinal;
  EndTime, DeltaTime : Cardinal;
  e, NextEvent       : PSeEvent;
begin
  assert(SampleFrames > 0);

  CurrentSampleClock := SampleClock;
  EndTime := CurrentSampleClock + SampleFrames;

  for(;;)
   begin
    if( events == 0 ) // fast version, when no events on list.
     begin
      assert(SampleFrames > 0 );
      (this.*(ProcessFunction))( BufferOffset, SampleFrames );
      SetSampleClock(EndTime);
      return;
     end;

    DeltaTime := SampleFrames;

    NextEvent := events;

    assert(NextEvent.time_stamp >= CurrentSampleClock );

    if( NextEvent.time_stamp < EndTime ) // will happen in this block
     begin
      DeltaTime = NextEvent.time_stamp - CurrentSampleClock;
      // no, sub_process needs to know if event pending
      // events.RemoveHead();
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
    events = events.next;
    HandleEvent(e);
    delete e;
  end;
end;
*)

procedure TSEModuleBase.HandleEvent(Event: PSeEvent);
begin
 assert(Event.time_stamp = SampleClock);
 case Event.event_type of
//  uetStatChange: getPin(Integer(Event.ptr_parm1)).OnStatusUpdate((state_type) e.int_parm1 );
(* not used anymore
    case UET_RUN_FUNCTION: // buggy ( dll can't allocate mem and attach to event, causes crash when SE trys to free it)
      begin
        // ptr_parm1 points to a ug_func pointer (allocated)
        function_pointer *fp = (function_pointer* ) Event.ptr_parm1;
        fp.Run();

        // TODO!!!!would be better to perform deletion in event detructor
        // will prevent mem leaks on events that are deleted without being used (due to power-off situation)
        delete fp;
        Event.ptr_parm1 = NULL;
      end;
      break;
*)
  uetRunFunction:
   begin
(*
//    ug_func func = 0; // important to initialise (code below is a hack)
//    *( (int* ) &func) = *( (int* ) &(Event.ptr_parm1));
//    (this.*(func))();
        my_delegate<ug_func> temp_delegate(Event.ptr_parm1);
//        temp_delegate.pointer.native = 0;
//        temp_delegate.pointer.raw_pointer = Event.ptr_parm1;
        (this.*(temp_delegate.pointer.native))();
*)
   end;
(*  case UET_UI _NOTIFY2:
      OnGui Notify( Event.int_parm1, (void * ) Event.int_parm2 );
      free( (void * ) Event.int_parm2 ); // free memory block
      break;*)
  uetProgChange: ; // do nothing
  uetMIDI : OnMidiData(Event.time_stamp, Event.int_parm1, Event.int_parm2);
  else; // assert(false); // un-handled event
 end;
end;

procedure TSEModuleBase.RunDelayed(sample_clock: Cardinal; func: TUgFunc);
begin
//  my_delegate<ug_func> temp_delegate(func);

//  temp_delegate.pointer.native = func;
(*
  // NO, can't allocate events here (in dll)
//  function_pointer *fp = new function_pointer( this, func );
  void *function_address;
  *( (int* ) &(function_address)) = *( (int* ) &func); // copy first 32 bits of function pointer
*)

// AddEvent(sample_clock, Integer(uetRunFunction2), 0, 0, temp_delegate.RawPointer());
end;

// insert event sorted.  Pass pointer to tempory event structure(event data will be copied by SE)
procedure TSEModuleBase.AddEvent(p_time_stamp: Cardinal; p_event_type: TUGEventType; p_int_parm1, p_int_parm2: Integer; p_ptr_parm1: Pointer);
var
  temp : TSeEvent;
begin
  assert(p_time_stamp >= SampleClock);

  temp.Create(p_time_stamp, p_event_type, p_int_parm1, p_int_parm2, p_ptr_parm1);
  CallHost(SEAudioMasterAddEvent, 0, 0, @temp);

//  delete p_event;

(*
  unsigned long time_stamp = p_event.time_stamp;

  SeEvent *e = events;
  SeEvent *prev = 0;
  while(true)
  begin
    if( e == 0 || e.time_stamp > time_stamp ) // events with same time must be added in same order received (for stat changes to work properly)
    begin
      p_event.next = e;
      if( prev )
      begin
        prev.next = p_event;
      end;
      else
      begin
        events = p_event;
      end;
      return;
    end;
    prev = e;
    e = e.next;
  end;*)
end;

procedure TSEModuleBase.Resume; // from either sleep or suspend
(*
var
  e : PSeEvent;
*)
begin
(*
 SetSampleClock(CallHost(SEAudioMasterGetSampleClock, 0, 0, 0 ));

 // update the time on any pending events
 // this applies to resume from suspend only
 e := events;
 while (e <> nil) do
  begin
   if e.time_stamp < SampleClock
    then e.time_stamp := SampleClock;
   e = e.next;
  end;
*)
end;

(*
procedure TSEModuleBase.setSubProcess(p_function: TProcessFunctionPtr2)
begin
 if (m_process_function_pointer.pointer.native <> p_function) then
  begin
   m_process_function_pointer.pointer.native := p_function;

   cEffect.sub_process_ptr := (long)m_process_function_pointer.RawPointer;

   CallHost(SEAudioMasterSetProcessFunction, 0, 0, 0);
  end;
end;

function TSEModuleBase.getSubProcess: TProcessFunctionPtr2;
begin
  result := m_process_function_pointer.pointer.native;
end;
*)

procedure TSEModuleBase.private_HandleEvent(Event: PSeEvent); cdecl;
begin
  HandleEvent(Event);
end;

procedure TSEModuleBase.ProcessIdle(start_pos, sampleframes: Integer);
begin
 // do nothing here!
end;

// divert to virtual function

(*
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function SET_PROCESS_FUNC(func : longint) : longint;
begin
 SET_PROCESS_FUNC := setSubProcess((static_cast<process_function_ptr2)>(@(func)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function RUN_AT(sample_clock,func : longint) : longint;
begin
 RUN_AT := RunDelayed(sample_clock,(static_cast<ug_func)>func);
end;

function func(_para1: p_func): class;
begin
end;

function getAeffect:^SEModStructBase2;
begin
  return and cEffect;
end;

function getPin(index:longint):^SEPin;
begin
  { You must implement this function }
end;
*)

end.
