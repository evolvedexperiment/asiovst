unit SEGainModule;

interface

uses
  DAV_Common, SECommon, SEDSP;

type
  // define some constants to make referencing in/outs clearer
  TSEGainPins = (pinInput1, pinInput2, pinOutput);

  TSEGainModule = class(TSEModuleBase)
  private
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FInput2Buffer : PDAVSingleFixedArray;
    FOutputBuffer : PDAVSingleFixedArray;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    procedure Open; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    function GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(BufferOffset: Integer; SampleFrames: Integer);
    procedure PlugStateChange(Pin: TSEPin); override;
  end; 

implementation

constructor TSEGainModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
end;

destructor TSEGainModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 inherited;
end;

procedure TSEGainModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEGainModule.SubProcess(BufferOffset: Integer; SampleFrames: Integer);
var
  In1    : PDAVSingleFixedArray;
  In2    : PDAVSingleFixedArray;
  Out1   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 In1  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 In2  := PDAVSingleFixedArray(@FInput2Buffer[BufferOffset]);
 Out1 := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Out1^[Sample] := In1^[Sample] * In2^[Sample];
  end;
end;

// describe your module
class function TSEGainModule.getModuleProperties(Properties : PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Gain Example';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'Synthedit Gain Example';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

// describe the pins (plugs)
function TSEGainModule.GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEGainPins(index) of
  // typical input plug (inputs are listed first)
  pinInput1: with Properties^ do
              begin
               Name            := 'Input';
               VariableAddress := @FInput1Buffer;
               Direction       := drIn;
               Datatype        := dtFSample;
               DefaultValue    := '0';
              end;
  pinInput2: with Properties^ do
              begin
               Name            := 'Input';
               VariableAddress := @FInput2Buffer;
               Direction       := drIn;
               Datatype        := dtFSample;
               DefaultValue    := '5';
              end;

  // typical output plug
  pinOutput: with Properties^ do
              begin
               Name            := 'Output';
               VariableAddress := @FOutputBuffer;
               Direction       := drOut;
               Datatype        := dtFSample;
              end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEGainModule.PlugStateChange(Pin: TSEPin);
var
  InState  : array [0..1] of TSEStateType;
  OutState : TSEStateType;
begin
 // query the 'state of the input plugs...
 //   stRun    = Normal Streaming Audio        (e.g. from an oscillator)
 //   stStatic = Fixed, unchanging input value (e.g. a slider at rest)
 InState[0] := getPin(Integer(pinInput1)).getStatus;
 InState[1] := getPin(Integer(pinInput2)).getStatus;

 // we need to pass on the state of this module's output signal
 // it depends on the inputs...
 OutState := InState[0];
 if InState[1] > OutState
  then OutState := InState[1];

 // if either input zero, tell downstream modules audio has stopped
 if (InState[0] < stRun) and (getPin(Integer(pinInput1)).getValue = 0)
  then OutState := stStatic;

 if (InState[1] < stRun) and (getPin(Integer(pinInput2)).getValue = 0)
  then OutState := stStatic;

 // 'transmit' new output status to next module 'downstream'
 getPin(Integer(pinOutput)).TransmitStatusChange(SampleClock, OutState);

 inherited; 
end;

end.
