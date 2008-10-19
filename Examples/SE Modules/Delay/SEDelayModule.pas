unit SEDelayModule;

interface

uses
  DAV_Common, SECommon, SEDSP;

type
  // define some constants to make referencing in/outs clearer
  TSEDelayPins = (pinInput1, pinInput2, pinOutput, pinDelayTime);

  TSEDelayModule = class(TSEModuleBase)
  private
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FInput2Buffer : PDAVSingleFixedArray;
    FOutputBuffer : PDAVSingleFixedArray;
    FBuffer       : PSEFloatSampleFixedArray;
    FCount        : Integer;
    FBufferSize   : Integer;
    FDelay_ms     : Single;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    procedure Open; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    function GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(BufferOffset: Integer; SampleFrames: Integer);
    procedure PlugStateChange(Pin: TSEPin); override;
    procedure CreateBuffer;
  end;

implementation

constructor TSEDelayModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FBuffer := nil;
end;

destructor TSEDelayModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 Dispose(FBuffer);
 inherited;
end;

procedure TSEDelayModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 CreateBuffer;

 // let 'downstream' modules know audio data is coming
 getPin(Integer(pinOutput)).TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEDelayModule.CreateBuffer;
begin
  FBufferSize := round(SampleRate * FDelay_ms * 0.001);
  if (FBufferSize < 1) then FBufferSize := 1;

  if (FBufferSize > SampleRate * 10)  // limit to 10 s sample
   then FBufferSize := round(SampleRate * 10);

  ReallocMem(FBuffer, FBufferSize * SizeOf(TSEFLoatSample));

  // clear buffer
  FillChar(FBuffer^, FBufferSize * SizeOf(TSEFLoatSample), 0); // clear buffer

  // ensure we arn't accessing data outside buffer
  FCount := 0;
end;

// The most important part, processing the audio
procedure TSEDelayModule.SubProcess(BufferOffset: Integer; SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Fbk    : PDAVSingleFixedArray;
  Out1   : PDAVSingleFixedArray;
  Temp   : Single;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Fbk  := PDAVSingleFixedArray(@FInput2Buffer[BufferOffset]);
 Out1 := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Out1^[Sample] := FBuffer[FCount];
   Temp := Inp[Sample] + Out1^[Sample] * Fbk[Sample];
   KillDenormal(Temp);
   FBuffer[FCount] := Temp;
   FCount := FCount mod FBufferSize;
  end;
end;

// describe your module
class function TSEDelayModule.getModuleProperties(Properties : PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Delay Example';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'Synthedit Delay Example';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

// describe the pins (plugs)
function TSEDelayModule.GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEDelayPins(index) of
  // typical input plug (inputs are listed first)
  pinInput1: with Properties^ do
              begin
               Name            := 'Input';
               VariableAddress := @FInput1Buffer;
               Direction       := drIn;
               Datatype        := dtFSAMPLE;
               DefaultValue    := '0';
              end;
  pinInput2: with Properties^ do
              begin
               Name            := 'Feedback';
               VariableAddress := @FInput2Buffer;
               Direction       := drIn;
               Datatype        := dtFSAMPLE;
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
  pinDelayTime: with Properties^ do
                 begin
                  Name            := 'Max Delay ms';
                  VariableAddress := @FDelay_ms;
                  Direction       := drParameter;
                  DataType        := dtSingle;
                  DefaultValue    := '1000';
                 end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEDelayModule.PlugStateChange(Pin: TSEPin);
begin
 // has user altered delay time parameter?
 if (pin.getPinID = Integer(pinDelayTime))
  then CreateBuffer; // re-create the audio buffer
 inherited; 
end;

end.
