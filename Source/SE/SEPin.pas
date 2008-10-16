unit SEPin;

interface

uses
  SEDatatypes, SEModuleBase;

type
  TAutoduplicatePlugData = record
    case longint of
     0 : ( float_ptr   : PDouble );
     1 : ( enum_value  : Smallint );
     2 : ( bool_value  : Boolean );
     3 : ( float_value : Double );
     4 : ( text_ptr    : PPchar );
     5 : ( int_value   : Longint );
    end;

  PSEPin = ^TSEPin;  
  TSEPin = class
  private
    FModule               : PSEModuleBase;
    FPinIndex             : Integer;
    FStatus               : TStateType;
    FDataType             : TPlugDataType;
    FVariablePtr          : Pointer;
    FAutoDuplicatePlugVar : TAutoduplicatePlugData;
    { Holds pointer to buffer (auto duplicate plugs only) }
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Init(Module: PSEModuleBase; PinIndex: Integer; DataType: TPlugDataType; VariablePtr: Pointer);
    function IsConnected: Boolean;

    procedure OnStatusUpdate(Status: TStateType);
    function GetStatus: TStateType;
    function GetValue: Double;

    { for audio plugs only }
    {  AutoduplicatePlugData getValueNonAudio(void)return FAutoDuplicatePlugVar;; }

//    EPlugDataType getDataType(void){return FDataType;};
    procedure TransmitStatusChange(SampleClock: Cardinal; NewState: TStateType);
    procedure TransmitMIDI(SampleClock, Msg: Cardinal);

//    SEModule_base *getModule(void){return FModule;};
//    int getPinID(){return FPinIndex;};
//    void *GetVariableAddress(){return FVariablePtr;};
  end;

implementation

uses
  DAV_Common, SEModStructBase;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

constructor TSEPin.Create;
begin
 // nothing in here yet!
end;

destructor TSEPin.Destroy;
begin
 // nothing in here yet!
 inherited;
end;

procedure TSEPin.Init(Module: PSEModuleBase; PinIndex: Integer; DataType: TPlugDataType; VariablePtr: Pointer);
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
(*
      FAutoDuplicatePlugVar.float_ptr := PSingle(FModule.CallHost(SEAudioMasterGetPinVarAddress, FPinIndex));
      FVariablePtr := @FAutoDuplicatePlugVar.float_ptr;
*)
     end;
    dtEnum:
     begin
      FAutoDuplicatePlugVar.enum_value := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.enum_value;
     end;
    dtBool:
     begin
      FAutoDuplicatePlugVar.bool_value := False;
      FVariablePtr := @FAutoDuplicatePlugVar.bool_value;
     end;
    dtFloat:
     begin
      FAutoDuplicatePlugVar.float_value := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.float_value;
     end;
    dtInt:
     begin
      FAutoDuplicatePlugVar.int_value := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.int_value;
     end;
   (* don't work
    dtText:
     begin
      //      FAutoDuplicatePlugVar.text_ptr = 0;
      FAutoDuplicatePlugVar.text_ptr = (char * * ) FModule->CallHost(SEAudioMasterGetPinVarAddress, FPinIndex);
      FVariablePtr = &FAutoDuplicatePlugVar.text_ptr;
     end;
   *)
  end;
 end else FAutoDuplicatePlugVar.float_ptr := 0;
end;

function TSEPin.IsConnected: Boolean;
begin
 result := FModule.CallHost(SEAudioMasterIsPinConnected, FPinIndex, 0, 0) <> 0;
end;

procedure TSEPin.OnStatusUpdate(Status: TStateType);
begin
(*
  FStatus = p_status;
  /* done in host now
  switch (getDataType())
  {
    case DT_ENUM:
    {
    *((short * )FVariablePtr) = (short) p_new_value;
    }
    break;
    case DT_BOOL:
    {
    *((bool * )FVariablePtr) = p_new_value != 0;
    }
    break;
    case DT_FLOAT:
    {
    *((float * )FVariablePtr) =  *((float* )&p_new_value);
    }
    break;
    case DT_TEXT: // ask host to update the text value
    {
    getModule()->CallHost(SEAudioMasterGetPinInputText, FPinIndex, p_new_value, 0);
    }
    break;
    }
    */
  getModule()->OnPlugStateChange(this);

  if (p_status == ST_ONE_OFF) // one-offs need re-set once processed
  {
    FStatus = ST_STOP;
  }
*)
end;

function TSEPin.GetStatus: TStateType;
begin
 result := FStatus;
end;

function TSEPin.getValue: Double;
var
  BlockPos    : Cardinal;
  SampleClock : Cardinal;
  Buffer      : PDAVSingleFixedArray;
begin
 assert(FDataType = dtFSample);
 BlockPos := FModule.CallHost(SEAudioMasterGetBlockStartClock, FPinIndex, 0, 0);
 SampleClock := FModule.SampleClock;
 Buffer := PDAVSingleFixedArray(FVariablePtr);
 result := Buffer[SampleClock - BlockPos];
end;

procedure TSEPin.TransmitStatusChange(SampleClock: Cardinal; NewState: TStateType);
begin
//  FModule->CallHost(SEAudioMasterSetPinStatus, FPinIndex, NewState, (void * ) SampleClock);
end;

procedure TSEPin.TransmitMIDI(SampleClock: Cardinal; Msg: Cardinal);
begin
{
  // MIDI data must allways be timestamped at or after the current 'clock'.
  assert(SampleClock >= FModule->SampleClock());

  FModule->CallHost(SEAudioMasterSendMIDI, FPinIndex, Msg, (void * ) SampleClock);
}
end;

end.
