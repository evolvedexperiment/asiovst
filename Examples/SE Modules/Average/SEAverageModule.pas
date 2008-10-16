unit SEAverageModule;

interface

uses
  DAV_Common, SEModuleBase;

type
  // define some constants to make referencing in/outs clearer
  TSEAveragePins = (pinInput, pinOutput);

  TSEAverageModule = class(TSEModuleBase)
  private
    FOutput            : PDAVSingleFixedArray;
    FStaticCount       : Integer;
    FDynamicPlugsCount : Integer;
    FDynamicPlugs      : array of PDAVSingleFixedArray;
  public
    constructor Create(SEAudioMaster: SEAudioMasterCallback2; p_resvd1: Pointer);
    destructor Destroy;

    class function getModuleProperties(Properties : PSEPinProperties): Boolean;
    function GetPinProperties(Index: Integer; Properties : PSEPinProperties): Boolean; virtual;
    procedure SubProcess(BufferOffset, SampleFrames: Integer); cdecl;
    procedure SubProcessStatic(BufferOffset, SampleFrames: Integer); cdecl;
    procedure OnPlugStateChange(Pin: PSEPin);
    procedure Open; virtual;
  end;

implementation

uses
  SEModStruct, SEPin;

constructor TSEAverageModule.Create(SEAudioMaster: SEAudioMasterCallback2; p_resvd1: Pointer);
begin
 inherited Create(SEAudioMaster, p_resvd1);
 FDynamicPlugs := nil;
end;

destructor TSEAverageModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 Dispose(FDynamicPlugs);
end;

procedure TSEAverageModule.Open;
var
  i : Integer;
begin
 // choose which function is used to process audio
 SET_PROCESS_FUNC(TSEAverageModule.SubProcess);

 // call the base class
 inherited Open;

 // to work out how many 'dynamic' plugs the module has..
 // Ask host how many input plugs this module actually has,
 // then subtract the number of 'regular' plugs
 DynamicPlugsCount := CallHost(SEAudioMasterGetTotalPinCount) - Integer(pinInput);

 if DynamicPlugsCount > 0 then
  begin
   // allocate an array to hold pointers to all the input buffers
   SetLength(FDynamicPlugs, DynamicPlugsCount);

   // ask the host for a pointer to each input buffer
   // store them in the array
   for i := 0 to DynamicPlugsCount - 1
    do FDynamicPlugs[i] := getPin(i + PN_INPUT1).GetVariableAddress; //(float*)CallHost(SEAudioMasterGetPinVarAddress, i + PN_INPUT1);
  end;
end;

// The most important part, processing the audio
procedure TSEAverageModule.SubProcess(BufferOffset, SampleFrames: Integer);
var
  Total  : Single;
  Scaler : Single;
  Sample : Integer;
  PlugNo : Integer
begin
 // To calculate an average, add up all the inputs, then divide by the total
 // number of inputs dividing by a number is the same as multiplying by
 // 1/number
 // I've pre-calculated this value to make the calculateion a little faster.

 if (DynamicPlugsCount > 0)
  then Scaler = 1 / DynamicPlugsCount;
  else Scaler = 1;// if there are zero inputs, just set scaler to one.

 for Sample := 0 to SampleFrames - 1 do
  begin
   // step though each input, calculating the average
   Total := 0;
   for PlugNo := 0 to DynamicPlugsCount - 1
    do Total := Total + FDynamicPlugs[PlugNo, Sample + BufferOffset];

   output1[Sample + BufferOffset] := Total * Scaler;
  end; 
end;

procedure TSEAverageModule.SubProcessStatic(BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

// describe your module
function TSEAverageModule.getModuleProperties(Properties: PSEModuleProperties): Boolean;
begin
  // describe the plugin, this is the name the end-user will see.
  properties.name := 'Averager';              // !!TODO!!

  // return a unique string 32 characters max
  // if posible include manufacturer and plugin identity
  // this is used internally by SE to identify the plug.
  // No two plugs may have the same id.
  properties.id := 'Synthedit Averager';      // !!TODO!!

  // Info, may include Author, Web page whatever
  properties.about := 'Christian-W. Budde';   // !!TODO!!
  result := True;
end;

// describe the pins (plugs and parameters)
function TSEAverageModule.GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case index of                   // !!TODO!! list your in / out plugs
   pinInput: with Properties^ do
              begin
               name             := 'Output';
               variable_address := FOutput;
               direction        := DR_OUT;
               datatype         := DT_FSAMPLE;
              end;
  pinOutput: with Properties^ do // this plug automatically duplicates itself
              begin              // it must be the last plug in the list
               name             := 'Input';
               direction        := DR_IN;
               datatype         := DT_FSAMPLE;
               default_value    := '0';
               flags            := IO_AUTODUPLICATE or IO_CUSTOMISABLE;
              end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSEAverageModule.OnPlugStateChange(Pin: PSEPin)
var
  InState  : TStateType;
  OutState : TStateType;
begin
 // query the 'state of the input plugs...
 //     ST_RUN     = Normal Streaming Audio        (e.g. from an oscillator)
 //     ST_STATIC  = Fixed, unchanging input value (e.g. a slider at rest)

 // we need to pass on the state of this module's output signal
 // it depends on the inputs.  Choose the 'highest'..
 OutState := ST_STATIC;

 for i := 0 to DynamicPlugsCount - 1 do
  begin
   InState := getPin(i).getStatus;
   if InState > OutState
    then OutState = InState;
  end;

 // 'transmit' this modules new FOutput status to next module 'downstream'
 getPin(1).TransmitStatusChange(SampleClock, OutState);

 // setup 'sleep mode' or not
 if (OutState < ST_RUN)
  begin
   FStaticCount := getBlockSize;
   SET_PROCESS_FUNC(SubProcessStatic);
  end
 else SET_PROCESS_FUNC(SubProcess);
end;

end.
