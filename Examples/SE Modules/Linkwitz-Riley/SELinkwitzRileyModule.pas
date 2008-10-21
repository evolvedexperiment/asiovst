unit SELinkwitzRileyModule;

interface

uses
  DAV_Common, DAV_DSPButterworthFilter, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSELinkwitzRileyPins = (pinInput, pinOutputLow, pinOutputHigh, pinFrequency,
    pinOrder);

  TSELinkwitzRileyModule = class(TSEModuleBase)
  private
  protected
    FInputBuffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutLoBuffer : PDAVSingleFixedArray;
    FOutHiBuffer : PDAVSingleFixedArray;
    FFrequency   : Single;
    FOrder       : Integer;
    FHighSign    : Single; 
    FFilterLP    : array [0..1] of TButterworthLP;
    FFilterHP    : array [0..1] of TButterworthHP;
    procedure SampleRateChanged; override;
    procedure Open; override;
    procedure Close; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class function GetModuleProperties(Properties : PSEModuleProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

constructor TSELinkwitzRileyModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);

 FFilterLP[0] := TButterworthLP.Create;
 FFilterLP[1] := TButterworthLP.Create;
 FFilterHP[0] := TButterworthHP.Create;
 FFilterHP[1] := TButterworthHP.Create;

 FFrequency := 1000;
 FOrder     := 2;
 FHighSign  := 1;
end;

destructor TSELinkwitzRileyModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 FreeAndNil(FFilterLP[0]);
 FreeAndNil(FFilterLP[1]);
 FreeAndNil(FFilterHP[0]);
 FreeAndNil(FFilterHP[1]);
 inherited;
end;

procedure TSELinkwitzRileyModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[Integer(pinOutputLow)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputHigh)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSELinkwitzRileyModule.Close;
begin
 // nothing here todo yet
 inherited;
end;

// The most important part, processing the audio
procedure TSELinkwitzRileyModule.SampleRateChanged;
begin
 inherited;
 FFilterLP[0].SampleRate := SampleRate;
 FFilterLP[1].SampleRate := SampleRate;
 FFilterHP[0].SampleRate := SampleRate;
 FFilterHP[1].SampleRate := SampleRate;
end;

procedure TSELinkwitzRileyModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  OutLo  : PDAVSingleFixedArray;
  OutHi  : PDAVSingleFixedArray;
  Sample : Integer;
  Temp   : Double;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Input := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutLo := PDAVSingleFixedArray(@FOutLoBuffer[BufferOffset]);
 OutHi := PDAVSingleFixedArray(@FOutHiBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Temp           := Input[Sample] + cDenorm64;
   OutLo^[Sample] := FFilterLP[0].ProcessSample(
                     FFilterLP[1].ProcessSample(Temp));
   OutHi^[Sample] := FFilterHP[0].ProcessSample(
                     FFilterHP[1].ProcessSample(Temp)) * FHighSign;
  end;
end;

// describe your module
class function TSELinkwitzRileyModule.getModuleProperties(Properties : PSEModuleProperties): Boolean;
begin
 // describe the plugin, this is the name the end-user will see.
 Properties.Name := 'Linkwitz-Riley Splitter';

 // return a unique string 32 characters max
 // if posible include manufacturer and plugin identity
 // this is used internally by SE to identify the plug.
 // No two plugs may have the same id.
 Properties.ID := 'DAV Linkwitz-Riley Splitter';

 // Info, may include Author, Web page whatever
 Properties.About := 'by Christian-W. Budde';
 result := True;
end;

// describe the pins (plugs)
function TSELinkwitzRileyModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSELinkwitzRileyPins(index) of
  // typical input plug (inputs are listed first)
  pinInput: with Properties^ do
             begin
              Name            := 'Input';
              VariableAddress := @FInputBuffer;
              Direction       := drIn;
              Datatype        := dtFSAMPLE;
              DefaultValue    := '0';
              Flags           := [iofLinearInput];
             end;

  // typical output plug
  pinOutputLow: with Properties^ do
                 begin
                  Name            := 'Output Low';
                  VariableAddress := @FOutLoBuffer;
                  Direction       := drOut;
                  Datatype        := dtFSample;
                 end;
  pinOutputHigh: with Properties^ do
                  begin
                   Name            := 'Output High';
                   VariableAddress := @FOutHiBuffer;
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
              DatatypeExtra   := 'range 0,64';
             end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// this routine is called whenever an input changes status.
// e.g when the user changes a module's parameters,
// or when audio stops/starts streaming into a pin
procedure TSELinkwitzRileyModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 // has user altered LinkwitzRiley time parameter?
 case TSELinkwitzRileyPins(CurrentPin.PinID) of
  pinFrequency:
   begin
    FFilterLP[0].Frequency := FFrequency;
    FFilterHP[0].Frequency := FFrequency;
    FFilterLP[1].Frequency := FFrequency;
    FFilterHP[1].Frequency := FFrequency;
   end;
  pinOrder :
   begin
    FFilterLP[0].Order := FOrder;
    FFilterHP[0].Order := FOrder;
    FFilterLP[1].Order := FOrder;
    FFilterHP[1].Order := FOrder;
    if FOrder mod 2 = 1
     then FHighSign := -1
     else FHighSign :=  1
   end;
 end;
 inherited;
end;

end.
