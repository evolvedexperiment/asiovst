unit SETunerModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspTuner;

type
  // define some constants to make referencing in/outs clearer
  TSETunerPins = (pinInput, pinMinimum, pinMaximum, pinSmooth, pinFrequency);

  TCustomSETunerModule = class(TSEModuleBase)
  private
    FInputBuffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FFrequency   : PDAVSingleFixedArray;
    FStaticCount : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FTuner : TTuner;
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

  TSETunerStaticModule = class(TCustomSETunerModule)
  private
    FMinimum      : Single;
    FMaximum      : Single;
    FSmoothFactor : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSETunerControllableModule = class(TSETunerStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSETunerModule }

constructor TCustomSETunerModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FTuner := TTuner.Create;
end;

destructor TCustomSETunerModule.Destroy;
begin
 FreeAndNil(FTuner);
 inherited;
end;

procedure TCustomSETunerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSETunerModule.SampleRateChanged;
begin
 inherited;
 if SampleRate > 0
  then FTuner.SampleRate := SampleRate;
end;

procedure TCustomSETunerModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
var
  Inp, Freq : PDAVSingleFixedArray;
  Sample    : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Freq := PDAVSingleFixedArray(@FFrequency[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   FTuner.Process(Inp^[Sample]);
   Freq^[Sample] := 0.1 * FTuner.CurrentFrequency;
  end;
end;

procedure TCustomSETunerModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSETunerModule.ChooseProcess;
begin
 if (Pin[Integer(pinInput)].Status = stRun)
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSETunerModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSETunerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSETunerPins(index) of
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
  pinFrequency:
   with Properties^ do
    begin
     Name            := 'Frequency';
     VariableAddress := @FFrequency;
     Direction       := drOut;
     Datatype        := dtFSample;
     DefaultValue    := '0';
     result          := True;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSETunerModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSETunerPins(CurrentPin.PinID) of
  pinInput : begin
              Pin[Integer(pinFrequency)].TransmitStatusChange(SampleClock, Pin[0].Status);
              ChooseProcess;
             end;
 end;
end;


{ TSETunerStaticModule }

// describe your module
class procedure TSETunerStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Tuner (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Tuner (static)';
  end;
end;

// describe the pins (plugs)
function TSETunerStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSETunerPins(index) of
  pinMinimum:
   with Properties^ do
    begin
     Name            := 'Minimum [Hz]';
     VariableAddress := @FMinimum;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '100';
     result          := True;
    end;
  pinMaximum:
   with Properties^ do
    begin
     Name            := 'Maximum [Hz]';
     VariableAddress := @FMaximum;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '4000';
     result          := True;
    end;
  pinSmooth:
   with Properties^ do
    begin
     Name            := 'Smooth [0..1]';
     VariableAddress := @FSmoothFactor;
     Direction       := drParameter;
     Datatype        := dtSingle;
     DefaultValue    := '0.99';
     result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSETunerStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSETunerPins(CurrentPin.PinID) of
  pinMinimum : FTuner.MinimumFrequency := FMinimum;
  pinMaximum : FTuner.MaximumFrequency := FMaximum;
  pinSmooth  : FTuner.SmoothFactor := FSmoothFactor;
 end;
end;


{ TSETunerControllableModule }

class procedure TSETunerControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Tuner';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Tuner';
  end;
end;

function TSETunerControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if TSETunerPins(index) in [pinMinimum..pinSmooth]
  then with Properties^ do Direction := drIn;
end;

end.
