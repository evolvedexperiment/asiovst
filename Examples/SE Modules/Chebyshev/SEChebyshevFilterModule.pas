unit SEChebyshevFilterModule;

interface

uses
  DAV_Common, DAV_DSPChebyshevFilter, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEChebyshevFilterPins = (pinInput, pinOutput, pinFrequency, pinOrder,
    pinRipple);

  TSEChebyshevFilterModule = class(TSEModuleBase)
  private
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FFrequency    : Single;
    FOrder        : Integer;
    FRipple       : Single;
    FFilter       : TChebyshev1Filter;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSEChebyshevFilterLPModule = class(TSEChebyshevFilterModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

  TSEChebyshevFilterHPModule = class(TSEChebyshevFilterModule)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
  end;

implementation

uses
  SysUtils;

constructor TSEChebyshevFilterModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);

 FFrequency := 1000;
 FOrder := 4;
end;

destructor TSEChebyshevFilterModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilter);
 inherited;
end;

procedure TSEChebyshevFilterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

// The most important part, processing the audio
procedure TSEChebyshevFilterModule.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
end;

procedure TSEChebyshevFilterModule.SubProcess(const BufferOffset, SampleFrames: Integer);
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
class procedure TSEChebyshevFilterModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEChebyshevFilterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEChebyshevFilterPins(index) of
  // typical input plug (inputs are listed first)
  pinInput: with Properties^ do
             begin
              Name            := 'Input';
              VariableAddress := @FInput1Buffer;
              Direction       := drIn;
              Datatype        := dtFSample;
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
              DataType        := dtEnum;
              DefaultValue    := '4';
              DatatypeExtra   := 'range -0,32';
             end;
  pinRipple: with Properties^ do
              begin
               Name            := 'Ripple';
               VariableAddress := @FRipple;
               Direction       := drParameter;
               DataType        := dtSingle;
               DefaultValue    := '1';
              end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSEChebyshevFilterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered a filter parameter?
 case TSEChebyshevFilterPins(CurrentPin.PinID) of
  pinFrequency : FFilter.Frequency := FFrequency;
      pinOrder : FFilter.Order     := FOrder;
     pinRipple : FFilter.Ripple    := FRipple;
 end;
 inherited;
end;

{ TSEChebyshevFilterModule }

constructor TSEChebyshevFilterLPModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1LP.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class procedure TSEChebyshevFilterLPModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Chebyshev Lowpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Chebyshev Lowpass';
  end;
end;

{ TSEChebyshevFilterHPModule }

constructor TSEChebyshevFilterHPModule.Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FFilter := TChebyshev1HP.Create;
 FFilter.SetFilterValues(FFrequency, 0, 0.1);
 FFilter.Order := FOrder;
end;

class procedure TSEChebyshevFilterHPModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Chebyshev Highpass';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Chebyshev Highpass';
  end;
end;

end.
