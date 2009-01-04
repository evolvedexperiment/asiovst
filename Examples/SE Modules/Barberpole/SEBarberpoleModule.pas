unit SEBarberpoleModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspBarberpole;

type
  // define some constants to make referencing in/outs clearer
  TSEBarberpolePins = (pinInput, pinOutput, pinStages, pinDepth, pinSpeed, pinMix);

  TSEBarberpoleModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStages       : Integer;
    FDepth        : Single;
    FSpeed        : Single;
    FMix          : Single;
  protected
    FBarberpole       : TDspBarberpole32;
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

{ TSEBarberpoleModule }

constructor TSEBarberpoleModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FBarberpole := TDspBarberpole32.Create
end;

destructor TSEBarberpoleModule.Destroy;
begin
 FreeAndNil(FBarberpole);
 inherited;
end;

procedure TSEBarberpoleModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEBarberpoleModule.SampleRateChanged;
begin
 inherited;
 FBarberpole.SampleRate := SampleRate;
end;

procedure TSEBarberpoleModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FBarberpole.Process(Inp^[Sample]);
end;

// describe your module
class procedure TSEBarberpoleModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Barberpole';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Barberpole';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEBarberpoleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEBarberpolePins(index) of
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
  pinStages:
   with Properties^ do
    begin
     Name            := 'Stages';
     VariableAddress := @FStages;
     Direction       := drIn;
     Datatype        := dtEnum;
     DatatypeExtra   := 'range 1,16';
     DefaultValue    := '2';
    end;
  pinDepth:
   with Properties^ do
    begin
     Name            := 'Depth';
     VariableAddress := @FDepth;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinSpeed:
   with Properties^ do
    begin
     Name            := 'Speed';
     VariableAddress := @FSpeed;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.2';
    end;
  pinMix:
   with Properties^ do
    begin
     Name            := 'Mix';
     VariableAddress := @FMix;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '0.5';
    end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

// An input plug has changed value
procedure TSEBarberpoleModule.PlugStateChange(const CurrentPin: TSEPin);
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

 case TSEBarberpolePins(CurrentPin.PinID) of
      pinStages: FBarberpole.Stages := FStages;
       pinDepth: FBarberpole.Depth := FDepth;
       pinSpeed: FBarberpole.Speed := FSpeed;
         pinMix: FBarberpole.Mix := FMix;
 end;
end;

end.
