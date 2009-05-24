unit SEVocoderModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspVocoder;

type
  // define some constants to make referencing in/outs clearer
  TSEVocoderPins = (pinInput, pinCarrier, pinOutput, pinBandwidth, pinOrder,
    pinAttack, pinRelease);

  TCustomSEVocoderModule = class(TSEModuleBase)
  private
    FInputBuffer   : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer  : PDAVSingleFixedArray;
    FCarrierBuffer : PDAVSingleFixedArray;
    FStaticCount   : Integer;
    FBandwidth     : Single;
    FAnalysisOrder : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FVocoder       : TVocoder;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual;
  end;

  TSEVocoderStaticModule = class(TCustomSEVocoderModule)
  private
    FAttack        : Single;
    FRelease       : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEVocoderControllableModule = class(TSEVocoderStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEVocoderAutomatableModule = class(TCustomSEVocoderModule)
  protected
    FAttackBuffer  : PDAVSingleFixedArray;
    FReleaseBuffer : PDAVSingleFixedArray;
    procedure SubProcessAutomated(const BufferOffset, SampleFrames: Integer);
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEVocoderModule }

constructor TCustomSEVocoderModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FVocoder := TVocoder.Create;
 FVocoder.SynthLevel := 0;
 FVocoder.InputLevel := 0;
 FVocoder.VocoderLevel := 1;
end;

destructor TCustomSEVocoderModule.Destroy;
begin
 FreeAndNil(FVocoder);
 inherited;
end;

procedure TCustomSEVocoderModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEVocoderModule.SampleRateChanged;
begin
 inherited;
 FVocoder.SampleRate := SampleRate;
end;

procedure TCustomSEVocoderModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp     : PDAVSingleFixedArray;
  Carrier : PDAVSingleFixedArray;
  Outp    : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp     := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp    := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Carrier := PDAVSingleFixedArray(@FCarrierBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FVocoder.Process(Inp^[Sample], Carrier^[Sample]);
end;

procedure TCustomSEVocoderModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEVocoderModule.ChooseProcess;
begin
 if (Pin[Integer(pinInput)].Status = stRun) and
    (Pin[Integer(pinCarrier)].Status = stRun)
  then
   begin
    OnProcess := SubProcess;
    Pin[2].TransmitStatusChange(SampleClock, stRun);
   end
  else
   begin
    FStaticCount := BlockSize + round(0.001 * FVocoder.Release * FVocoder.SampleRate);
    OnProcess := SubProcessStatic;
    Pin[2].TransmitStatusChange(SampleClock, stStop);
   end;
end;

// describe your module
class procedure TCustomSEVocoderModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEVocoderModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEVocoderPins(index) of
  pinInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinCarrier:
   with Properties^ do
    begin
     Name            := 'Carrier';
     VariableAddress := @FCarrierBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
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
  pinBandwidth:
   with Properties^ do
    begin
     Name            := 'Bandwidth';
     VariableAddress := @FBandwidth;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     result          := True;
    end;
  pinOrder:
   with Properties^ do
    begin
     Name            := 'Analysis Order';
     VariableAddress := @FAnalysisOrder;
     Direction       := drIn;
     Datatype        := dtEnum;
     DefaultValue    := '12';
     DatatypeExtra   := 'range 1,32';
     result          := True;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEVocoderModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVocoderPins(CurrentPin.PinID) of
  pinInput,
  pinCarrier   : ChooseProcess;
  pinBandwidth : FVocoder.SynthesisBandwidth := FBandwidth * sqrt(0.5);
  pinOrder     : FVocoder.AnalysisOrder := FAnalysisOrder;
 end;
end;


{ TSEVocoderStaticModule }

// describe your module
class procedure TSEVocoderStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Vocoder (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Vocoder (static)';
  end;
end;

// describe the pins (plugs)
function TSEVocoderStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEVocoderPins(index) of
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [ms]';
     VariableAddress := @FAttack;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.1';
     result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [ms]';
     VariableAddress := @FRelease;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEVocoderStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEVocoderPins(CurrentPin.PinID) of
     pinAttack : FVocoder.Attack := FAttack;
    pinRelease : FVocoder.Release := FRelease;
 end;
end;


{ TSEVocoderControllableModule }

class procedure TSEVocoderControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Vocoder';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Vocoder';
  end;
end;

function TSEVocoderControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if TSEVocoderPins(index) in [pinBandwidth..pinRelease]
  then with Properties^ do Direction := drIn;
end;

{ TSEVocoderAutomatableModule }

class procedure TSEVocoderAutomatableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Vocoder (automatable)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Vocoder (automatable)';
  end;
end;

function TSEVocoderAutomatableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEVocoderPins(index) of
  pinAttack:
   with Properties^ do
    begin
     Name            := 'Attack [ms]';
     VariableAddress := @FAttackBuffer;
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '0.1';
     result          := True;
    end;
  pinRelease:
   with Properties^ do
    begin
     Name            := 'Release [ms]';
     VariableAddress := @FReleaseBuffer;
     Direction       := drIn;
     Datatype        := dtFSample;
     DefaultValue    := '1';
     result          := True;
    end;
 end;
end;

procedure TSEVocoderAutomatableModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 case TSEVocoderPins(CurrentPin.PinID) of
  pinAttack, pinRelease:
    if (Pin[Integer(pinAttack)].Status <> stRun) and
       (Pin[Integer(pinRelease)].Status <> stRun)
     then OnProcess := SubProcess
     else OnProcess := SubProcessAutomated;
 end;
 inherited;
end;

procedure TSEVocoderAutomatableModule.SubProcessAutomated(const BufferOffset,
  SampleFrames: Integer);
var
  Inp     : PDAVSingleFixedArray;
  Carrier : PDAVSingleFixedArray;
  Outp    : PDAVSingleFixedArray;
  Att     : PDAVSingleFixedArray;
  Rel     : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp     := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp    := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Carrier := PDAVSingleFixedArray(@FCarrierBuffer[BufferOffset]);
 Att     := PDAVSingleFixedArray(@FAttackBuffer[BufferOffset]);
 Rel     := PDAVSingleFixedArray(@FReleaseBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  with FVocoder do
   begin
    Attack  := Att^[Sample];
    Release := Rel^[Sample];
    Outp^[Sample] := Process(Inp^[Sample], Carrier^[Sample]);
   end;
end;

end.
