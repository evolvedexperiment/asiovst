unit SEEnvelopeModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspPolyphaseHilbert;

type
  // define some constants to make referencing in/outs clearer
  TSEEnvelopePins = (pinInput, pinOutput, pinCoefficients, pinTransition);

  TSEEnvelopeModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FCoefficients : Integer;
    FTransition   : Single;
  protected
    FHilbert      : TPhaseHalfPi32;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

  TSEHilbertModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : array [0..1] of PDAVSingleFixedArray;
    FCoefficients : Integer;
    FTransition   : Single;
  protected
    FHilbert      : TPhaseHalfPi32;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

{ TSEEnvelopeModule }

constructor TSEEnvelopeModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FHilbert := TPhaseHalfPi32.Create;
end;

destructor TSEEnvelopeModule.Destroy;
begin
 FreeAndNil(FHilbert);
 inherited;
end;

procedure TSEEnvelopeModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEEnvelopeModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do // sampleFrames = how many samples to process (can vary). repeat (loop) that many times
  begin
   // do the actual processing (multiplying the two input samples together)
   Outp^[Sample] := FHilbert.ProcessEnvelopeSample(Inp^[Sample]);
  end;
end;

// describe your module
class procedure TSEEnvelopeModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Polyphase Envelope';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Polyphase Envelope';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEEnvelopeModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEEnvelopePins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinCoefficients:
   with Properties^ do
    begin
     Name            := 'Coefficients';
     VariableAddress := @FCoefficients;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range -0,64';
     DefaultValue    := '8';
    end;
  pinTransition:
   with Properties^ do
    begin
     Name            := 'Transition';
     VariableAddress := @FTransition;
     Direction       := drIn;
     Datatype        := dtSingle;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEEnvelopeModule.PlugStateChange(const CurrentPin: TSEPin);
var
  InState  : TSEStateType;
  OutState : TSEStateType;
begin
 InState  := Pin[Integer(pinInput)].Status;
 OutState := InState;
 if (InState < stRun) and (Pin[Integer(pinInput)].Value = 0)
  then OutState := stStatic;

 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, OutState);
 inherited;

 case TSEEnvelopePins(CurrentPin.PinID) of
  pinCoefficients: FHilbert.NumberOfCoefficients := FCoefficients;
    pinTransition: FHilbert.Transition := f_Limit(FTransition, 0.01, 0.499); 
 end;
end;


{ TSEHilbertModule }

constructor TSEHilbertModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FHilbert := TPhaseHalfPi32.Create;
end;

destructor TSEHilbertModule.Destroy;
begin
 FreeAndNil(FHilbert);
 inherited;
end;

procedure TSEHilbertModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEHilbertModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp, OutA, OutB : PDAVSingleFixedArray;
  Sample          : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutA := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 OutB := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do FHilbert.ProcessHilbertSample(Inp^[Sample], OutA^[Sample], OutB^[Sample]);
end;

// describe your module
class procedure TSEHilbertModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Hilbert Split';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Polyphase Hilbert Split';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEHilbertModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEEnvelopePins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinCoefficients:
   with Properties^ do
    begin
     Name            := 'Coefficients';
     VariableAddress := @FCoefficients;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range -0,64';
     DefaultValue    := '8';
    end;
  pinTransition:
   with Properties^ do
    begin
     Name            := 'Transition';
     VariableAddress := @FTransition;
     Direction       := drIn;
     Datatype        := dtSingle;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEHilbertModule.PlugStateChange(const CurrentPin: TSEPin);
var
  InState  : TSEStateType;
  OutState : TSEStateType;
begin
 InState  := Pin[Integer(pinInput)].Status;
 OutState := InState;
 if (InState < stRun) and (Pin[Integer(pinInput)].Value = 0)
  then OutState := stStatic;

 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, OutState);
 inherited;

 case TSEEnvelopePins(CurrentPin.PinID) of
  pinCoefficients: FHilbert.NumberOfCoefficients := FCoefficients;
    pinTransition: FHilbert.Transition := f_Limit(FTransition, 0.01, 0.499); 
 end;
end;

end.
