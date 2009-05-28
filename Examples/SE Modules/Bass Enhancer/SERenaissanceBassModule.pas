unit SERenaissanceBassModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspFrequencyDivider,
  DAV_DspPsychoacousticBassEnhancer;

type
  // define some constants to make referencing in/outs clearer
  TSERenaissancePins = (pinInput, pinOutput, pinFrequency, pinAddOriginalBass,
    pinIntensity, pinGain);

  TSERenaissanceBassModule = class(TSEModuleBase)
  private
    FInputBuffer        : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer       : PDAVSingleFixedArray;
    FStaticCount        : Integer;

    FFrequency          : Single;
    FAddOriginalBass    : Boolean;
    FIntensity          : PDAVSingleFixedArray;
    FGain               : PDAVSingleFixedArray;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FBassEnhancer : TRenaissanceBass;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
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

{ TSERenaissanceBassModule }

constructor TSERenaissanceBassModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FBassEnhancer := TRenaissanceBass.Create
end;

destructor TSERenaissanceBassModule.Destroy;
begin
 FreeAndNil(FBassEnhancer);
 inherited;
end;

procedure TSERenaissanceBassModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSERenaissanceBassModule.SampleRateChanged;
begin
 inherited;
 FBassEnhancer.SampleRate := SampleRate;
end;

procedure TSERenaissanceBassModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp     : PDAVSingleFixedArray;
  Outp    : PDAVSingleFixedArray;
  Intense : PDAVSingleFixedArray;
  Gain    : PDAVSingleFixedArray;
  Sample  : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp     := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp    := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 Intense := PDAVSingleFixedArray(@FIntensity[BufferOffset]);
 Gain    := PDAVSingleFixedArray(@FGain[BufferOffset]);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outp^[Sample] := FBassEnhancer.Process(Inp^[Sample]);
   if (Sample div 2) = 0 then
    begin
     FBassEnhancer.Intensity := Intense^[Sample];
     FBassEnhancer.Gain := Gain^[Sample];
    end;
  end;
end;

procedure TSERenaissanceBassModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSERenaissanceBassModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSERenaissanceBassModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Renaissance Clone';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Renaissance Clone';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSERenaissanceBassModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSERenaissancePins(index) of
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
  pinOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinFrequency:
   with Properties^ do
    begin
     Name            := 'Frequency';
     VariableAddress := @FFrequency;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '80';
    end;
  pinAddOriginalBass:
   with Properties^ do
    begin
     Name            := 'Add Original Bass';
     VariableAddress := @FAddOriginalBass;
     Direction       := drIn;
     Datatype        := dtBoolean;
     DefaultValue    := '1';
    end;
  pinIntensity:
   with Properties^ do
    begin
     Name            := 'Intensity';
     VariableAddress := @FIntensity;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  pinGain:
   with Properties^ do
    begin
     Name            := 'Gain';
     VariableAddress := @FGain;
     Direction       := drIn;
     Datatype        := dtFSample;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSERenaissanceBassModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSERenaissancePins(CurrentPin.PinID) of
            pinInput : begin
                        ChooseProcess;
                        Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                       end;
        pinFrequency : FBassEnhancer.Frequency := FFrequency;
  pinAddOriginalBass : FBassEnhancer.AddOriginalBass := FAddOriginalBass;
 end;
end;

end.
