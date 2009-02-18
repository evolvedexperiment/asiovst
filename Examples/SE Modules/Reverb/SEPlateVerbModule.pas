unit SEPlateVerbModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspPlateReverb;

type
  // define some constants to make referencing in/outs clearer
  TSEPlateVerbPins = (pinInput, pinOutputLeft, pinOutputRight, pinPreDelay,
    pinDecay, pinDamping, pinInputDiffusion, pinDecayDiffusion, pinModulation,
    pinEffectMix);

  TCustomSEPlateReverbModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : array [0..1] of PDAVSingleFixedArray;
    FStaticCount  : Integer;
    FPeak       : Single;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FPlateReverb : TPlateReverb;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SampleRateChanged; override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); virtual; abstract;
  end;

  TSEPlateReverbStaticModule = class(TCustomSEPlateReverbModule)
  private
    FPreDelay   : Single;
    FDecay      : Single;
    FDamping    : Single;
    FModulation : Single;
    FInputDiff  : Single;
    FDecayDiff  : Single;
    FEffectMix  : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEPlateReverbControllableModule = class(TSEPlateReverbStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils, DAV_StkReverb;

{ TCustomSEPlateReverbModule }

constructor TCustomSEPlateReverbModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FPlateReverb := TPlateReverb.Create
end;

destructor TCustomSEPlateReverbModule.Destroy;
begin
 FreeAndNil(FPlateReverb);
 inherited;
end;

procedure TCustomSEPlateReverbModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEPlateReverbModule.SampleRateChanged;
begin
 inherited;
 FPlateReverb.SampleRate := SampleRate;
end;

procedure TCustomSEPlateReverbModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if (FStaticCount <= 0) and (FPeak < 3.16E-5)
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEPlateReverbModule.ChooseProcess;
begin
 if Pin[Integer(pinInput)].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize + round(SampleRate);
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TCustomSEPlateReverbModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEPlateReverbModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEPlateVerbPins(index) of
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
  pinOutputLeft:
   with Properties^ do
    begin
     Name            := 'Output Left';
     VariableAddress := @FOutputBuffer[0];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  pinOutputRight:
   with Properties^ do
    begin
     Name            := 'Output Right';
     VariableAddress := @FOutputBuffer[1];
     Direction       := drOut;
     Datatype        := dtFSample;
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEPlateReverbModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEPlateVerbPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEPlateReverbStaticModule }

// describe your module
class procedure TSEPlateReverbStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV Plate Reverb (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Plate Reverb (static)';
  end;
end;

procedure TSEPlateReverbStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  OutL   : PDAVSingleFixedArray;
  OutR   : PDAVSingleFixedArray;
  Sample : Integer;
  Mono   : Single;
  Mix    : array [0..1] of Single;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 OutL := PDAVSingleFixedArray(@FOutputBuffer[0, BufferOffset]);
 OutR := PDAVSingleFixedArray(@FOutputBuffer[1, BufferOffset]);
 Mix[1] := 0.01 * FEffectMix;
 Mix[0] := 1 - Mix[1];

 for Sample := 0 to SampleFrames - 1 do
  begin
   Mono := abs(FPlateReverb.ProcessSample(Inp^[Sample]));
   OutL^[Sample] := Mix[0] * Inp^[Sample] + Mix[1] * FPlateReverb.OutputLeft;
   OutR^[Sample] := Mix[0] * Inp^[Sample] + Mix[1] * FPlateReverb.OutputRight;
   if Mono > FPeak then FPeak := Mono;
   FPeak := FPeak * 0.9999;
  end;
end;

// describe the pins (plugs)
function TSEPlateReverbStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if not result then
  case TSEPlateVerbPins(index) of
   pinPreDelay:
    with Properties^ do
     begin
      Name            := 'Pre-Delay';
      VariableAddress := @FPreDelay;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      result          := True;
     end;
   pinDecay:
    with Properties^ do
     begin
      Name            := 'Decay [%]';
      VariableAddress := @FDecay;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '1';
      result          := True;
     end;
   pinDamping:
    with Properties^ do
     begin
      Name            := 'Damping [kHz]';
      VariableAddress := @FDamping;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '10';
      result          := True;
     end;
   pinInputDiffusion:
    with Properties^ do
     begin
      Name            := 'Input Diffusion [%]';
      VariableAddress := @FInputDiff;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '70';
      result          := True;
     end;
   pinDecayDiffusion:
    with Properties^ do
     begin
      Name            := 'Decay Diffusion [%]';
      VariableAddress := @FDecayDiff;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '30';
      result          := True;
     end;
   pinModulation:
    with Properties^ do
     begin
      Name            := 'Modulation [%]';
      VariableAddress := @FModulation;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '30';
      result          := True;
     end;
   pinEffectMix:
    with Properties^ do
     begin
      Name            := 'Effect Mix [%]';
      VariableAddress := @FEffectMix;
      Direction       := drParameter;
      Datatype        := dtSingle;
      DefaultValue    := '50';
      result          := True;
     end;
  end;
end;

// An input plug has changed value
procedure TSEPlateReverbStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEPlateVerbPins(CurrentPin.PinID) of
        pinPreDelay : FPlateReverb.PreDelay := 4E-4 * FPreDelay;
           pinDecay : FPlateReverb.Decay := 1E-2 * FDecay;
         pinDamping : FPlateReverb.DampingFrequency := 1E3 * FDamping;
  pinInputDiffusion : FPlateReverb.InputDiffusion := 1E-2 * FInputDiff;
  pinDecayDiffusion : FPlateReverb.DecayDiffusion := 1E-2 * FDecayDiff;
      pinModulation : FPlateReverb.Modulation := 1E-2 * FModulation;
 end;
end;


{ TSEPlateReverbControllableModule }

class procedure TSEPlateReverbControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV Plate Reverb';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Plate Reverb';
  end;
end;

function TSEPlateReverbControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if TSEPlateVerbPins(index) in [pinPreDelay..pinEffectMix]
  then with Properties^ do Direction := drIn;
end;

end.
