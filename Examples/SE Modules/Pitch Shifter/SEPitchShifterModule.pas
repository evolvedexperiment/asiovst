unit SEPitchshifterModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_StkPitchShift;

type
  // define some constants to make referencing in/outs clearer
  TSEStkPitchshifterPins = (pinStkInput, pinStkOutput, pinStkSemitones,
    pinStkMix);

  TSEStkPitchshifterModule = class(TSEModuleBase)
  private
    FInputBuffer   : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer  : PDAVSingleFixedArray;
    FStaticCount   : Integer;
    FSemiTones     : Single;
    FEffectMix     : Single;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FStkPitchshifter     : TStkPitchshifter;
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
  SysUtils, Math;

{ TSEStkPitchshifterModule }

constructor TSEStkPitchshifterModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FStkPitchshifter := TStkPitchshifter.Create(SampleRate);
end;

destructor TSEStkPitchshifterModule.Destroy;
begin
 FreeAndNil(FStkPitchshifter);
 inherited;
end;

procedure TSEStkPitchshifterModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEStkPitchshifterModule.SampleRateChanged;
begin
 inherited;
 FStkPitchshifter.SampleRate := SampleRate;
end;

procedure TSEStkPitchshifterModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FStkPitchshifter.Tick(Inp^[Sample]);
end;

procedure TSEStkPitchshifterModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSEStkPitchshifterModule.ChooseProcess;
begin
 if Pin[0].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSEStkPitchshifterModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Pitchshifter (STK based)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Pitchshifter (STK based)';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEStkPitchshifterModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEStkPitchshifterPins(index) of
  pinStkInput:
   with Properties^ do
    begin
     Name            := 'Input';
     VariableAddress := @FInputBuffer;
     Direction       := drIn;
     Flags           := [iofLinearInput];
     Datatype        := dtFSample;
     DefaultValue    := '0';
    end;
  pinStkOutput:
   with Properties^ do
    begin
     Name            := 'Output';
     VariableAddress := @FOutputBuffer;
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinStkSemitones:
   with Properties^ do
    begin
     Name            := 'Semitones';
     VariableAddress := @FSemitones;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0';
    end;
  pinStkMix:
   with Properties^ do
    begin
     Name            := 'Mix [%]';
     VariableAddress := @FEffectMix;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '100';
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEStkPitchshifterModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case TSEStkPitchshifterPins(CurrentPin.PinID) of
      pinStkInput: begin
                    ChooseProcess;
                    Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                   end;
  pinStkSemitones: FStkPitchshifter.Shift := Power(2, FSemiTones / 12);
        pinStkMix: FStkPitchshifter.EffectMix := 0.01 * FEffectMix;
 end;
end;

end.
