unit SEButterworthLPModule;

interface

uses
  DAV_Common, DAV_DSPButterworthFilter, SECommon, SEDSP;

type
  // define some constants to make referencing in/outs clearer
  TSEButterworthLPPins = (pinInput, pinOutput, pinFrequency, pinOrder);

  TSEButterworthLPModule = class(TSEModuleBase)
  private
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFrequency    : Single;
    FOrder        : Integer;
    FFilter       : TButterworthLP;
    procedure FilterChanged;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;

    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    function GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(BufferOffset: Integer; SampleFrames: Integer);
    procedure PlugStateChange(Pin: TSEPin); override;
  end;

implementation

uses
  SysUtils;

constructor TSEButterworthLPModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);

 FFrequency := 1000;
 FOrder := 4;
 FFilter := TButterworthLP.Create;
 FFilter.SetFilterValues(1000, 0);
end;

destructor TSEButterworthLPModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSEButterworthLPModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 getPin(Integer(pinOutput)).TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEButterworthLPModule.Close;
begin
 // nothing here todo yet
 inherited;
end;

procedure TSEButterworthLPModule.FilterChanged;
begin
 FFilter.Frequency := FFrequency;
 FFilter.Order     := FOrder;
end;

// The most important part, processing the audio
procedure TSEButterworthLPModule.SubProcess(BufferOffset: Integer; SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Output^[Sample] := FFilter.ProcessSample(Input[Sample] + cDenorm64);
  end;
end;

// describe your module
class function TSEButterworthLPModule.getModuleProperties(Properties : PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Butterworth Lowpass';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'DAV Butterworth Lowpass';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

// describe the pins (plugs)
function TSEButterworthLPModule.GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEButterworthLPPins(index) of
  // typical input plug (inputs are listed first)
  pinInput: with Properties^ do
             begin
              Name            := 'Input';
              VariableAddress := @FInput1Buffer;
              Direction       := drIn;
              Datatype        := dtFSAMPLE;
              DefaultValue    := '0';
             end;

  // typical output plug
  pinOutput: with Properties^ do
              begin
               Name            := 'Output';
               VariableAddress := @FOutputBuffer;
               Direction       := drOut;
               Datatype        := dtFSample;
              end;
  pinFrequency: with Properties^ do
                 begin
                  Name            := 'Frequency';
                  VariableAddress := @FFrequency;
                  Direction       := drParameter;
                  DataType        := dtSingle;
                  DefaultValue    := '1000';
                 end;
  pinOrder: with Properties^ do
             begin
              Name            := 'Order';
              VariableAddress := @FOrder;
              Direction       := drParameter;
              DataType        := dtInteger;
              DefaultValue    := '4';
             end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEButterworthLPModule.PlugStateChange(Pin: TSEPin);
begin
 // has user altered ButterworthLP time parameter?
 if (pin.getPinID = Integer(pinFrequency)) or
    (pin.getPinID = Integer(pinOrder))
  then FilterChanged; // re-create the audio buffer
 inherited; 
end;

end.
