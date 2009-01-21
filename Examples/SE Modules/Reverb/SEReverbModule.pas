unit SEReverbModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_StkNReverb, DAV_StkJCReverb;

type
  // define some constants to make referencing in/outs clearer
  TSEStkReverbPins = (pinInput, pinOutput, pinT60, pinEffectMix);

  TCustomSEStkNReverbModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FNReverb      : TStkNReverb;
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

  TSEStkNReverbStaticModule = class(TCustomSEStkNReverbModule)
  private
    FT60       : Single;
    FEffectMix : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStkNReverbControllableModule = class(TSEStkNReverbStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TCustomSEStkJCReverbModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStaticCount  : Integer;
    procedure ChooseProcess;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    FJCReverb     : TStkJCReverb;
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

  TSEStkJCReverbStaticModule = class(TCustomSEStkJCReverbModule)
  private
    FT60       : Single;
    FEffectMix : Single;
  protected
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer); override;
  end;

  TSEStkJCReverbControllableModule = class(TSEStkJCReverbStaticModule)
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

implementation

uses
  SysUtils;

{ TCustomSEStkNReverbModule }

constructor TCustomSEStkNReverbModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FNReverb := TStkNReverb.Create
end;

destructor TCustomSEStkNReverbModule.Destroy;
begin
 FreeAndNil(FNReverb);
 inherited;
end;

procedure TCustomSEStkNReverbModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEStkNReverbModule.SampleRateChanged;
begin
 inherited;
 FNReverb.SampleRate := SampleRate;
end;

procedure TCustomSEStkNReverbModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEStkNReverbModule.ChooseProcess;
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
class procedure TCustomSEStkNReverbModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEStkNReverbModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEStkReverbPins(index) of
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
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEStkNReverbModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEStkNReverbStaticModule }

// describe your module
class procedure TSEStkNReverbStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" Reverb Network (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK Reverb Network (static)';
  end;
end;

procedure TSEStkNReverbStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FNReverb.Tick(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEStkNReverbStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEStkReverbPins(index) of
  pinT60:
   with Properties^ do
    begin
     Name            := 'T60 [s]';
     VariableAddress := @FT60;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     result          := True;
    end;
  pinEffectMix:
   with Properties^ do
    begin
     Name            := 'EffectMix [%]';
     VariableAddress := @FEffectMix;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEStkNReverbStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
        pinT60: FNReverb.T60       := FT60;
  pinEffectMix: FNReverb.EffectMix := 0.01 * FEffectMix;
 end;
end;


{ TSEStkNReverbControllableModule }

class procedure TSEStkNReverbControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" Reverb Network';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK Reverb Network';
  end;
end;

function TSEStkNReverbControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if TSEStkReverbPins(index) in [pinT60..pinEffectMix]
  then with Properties^ do Direction := drIn;
end;


{ TCustomSEStkJCReverbModule }

constructor TCustomSEStkJCReverbModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FJCReverb := TStkJCReverb.Create
end;

destructor TCustomSEStkJCReverbModule.Destroy;
begin
 FreeAndNil(FJCReverb);
 inherited;
end;

procedure TCustomSEStkJCReverbModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TCustomSEStkJCReverbModule.SampleRateChanged;
begin
 inherited;
 FJCReverb.SampleRate := SampleRate;
end;

procedure TCustomSEStkJCReverbModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TCustomSEStkJCReverbModule.ChooseProcess;
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
class procedure TCustomSEStkJCReverbModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TCustomSEStkJCReverbModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEStkReverbPins(index) of
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
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TCustomSEStkJCReverbModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
       pinInput: begin
                  ChooseProcess;
                  Pin[1].TransmitStatusChange(SampleClock, Pin[0].Status);
                 end;
 end;
end;


{ TSEStkJCReverbStaticModule }

// describe your module
class procedure TSEStkJCReverbStaticModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" JC Reverb (static)';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK JC Reverb (static)';
  end;
end;

procedure TSEStkJCReverbStaticModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FJCReverb.Tick(Inp^[Sample]);
end;

// describe the pins (plugs)
function TSEStkJCReverbStaticModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 case TSEStkReverbPins(index) of
  pinT60:
   with Properties^ do
    begin
     Name            := 'T60 [s]';
     VariableAddress := @FT60;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
     result          := True;
    end;
  pinEffectMix:
   with Properties^ do
    begin
     Name            := 'EffectMix [%]';
     VariableAddress := @FEffectMix;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '30';
     result          := True;
    end;
 end;
end;

// An input plug has changed value
procedure TSEStkJCReverbStaticModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 case TSEStkReverbPins(CurrentPin.PinID) of
        pinT60: FJCReverb.T60       := FT60;
  pinEffectMix: FJCReverb.EffectMix := 0.01 * FEffectMix;
 end;
end;


{ TSEStkJCReverbControllableModule }

class procedure TSEStkJCReverbControllableModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited GetModuleProperties(Properties);
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'DAV modified "STK" JC Reverb';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV STK JC Reverb';
  end;
end;

function TSEStkJCReverbControllableModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 result := inherited GetPinProperties(Index, Properties);
 if TSEStkReverbPins(index) in [pinT60..pinEffectMix]
  then with Properties^ do Direction := drIn;
end;

end.
