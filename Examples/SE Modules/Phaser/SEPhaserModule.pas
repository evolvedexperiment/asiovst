unit SEPhaserModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspPhaser;

type
  // define some constants to make referencing in/outs clearer
  TSEPhaserPins = (pinInput, pinOutput, pinDepth, pinFeedback, pinMinimum,
    pinMaximum, pinRate, pinStages);

  TSEPhaserModule = class(TSEModuleBase)
  private
    FInputBuffer  : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FStages       : Integer;
    FDepth        : Single;
    FRate         : Single;
    FMinimum      : Single;
    FMaximum      : Single;
    FFeedback     : Single;
  protected
    FPhaser       : TPhaser;
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

{ TSEPhaserModule }

constructor TSEPhaserModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 FPhaser := TPhaser.Create
end;

destructor TSEPhaserModule.Destroy;
begin
 FreeAndNil(FPhaser);
 inherited;
end;

procedure TSEPhaserModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

// The most important part, processing the audio
procedure TSEPhaserModule.SampleRateChanged;
begin
 inherited;
 FPhaser.SampleRate := SampleRate;
end;

procedure TSEPhaserModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Inp    : PDAVSingleFixedArray;
  Outp   : PDAVSingleFixedArray;
  Sample : Integer;
begin
 // assign some pointers to your in/output buffers. usually blocks (array) of 96 samples
 Inp  := PDAVSingleFixedArray(@FInputBuffer[BufferOffset]);
 Outp := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);

 for Sample := 0 to SampleFrames - 1
  do Outp^[Sample] := FPhaser.Process(Inp^[Sample]);
end;

// describe your module
class procedure TSEPhaserModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Phaser';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Phaser';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEPhaserModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEPhaserPins(index) of
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
  pinDepth:
   with Properties^ do
    begin
     Name            := 'Depth [%]';
     VariableAddress := @FDepth;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '10';
    end;
  pinFeedback:
   with Properties^ do
    begin
     Name            := 'Feedback [%]';
     VariableAddress := @FFeedback;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '10';
    end;
  pinMinimum:
   with Properties^ do
    begin
     Name            := 'Minimum [Hz]';
     VariableAddress := @FMinimum;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '300';
    end;
  pinMaximum:
   with Properties^ do
    begin
     Name            := 'Maximum [Hz]';
     VariableAddress := @FMaximum;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1000';
    end;
  pinRate:
   with Properties^ do
    begin
     Name            := 'Rate [Hz]';
     VariableAddress := @FRate;
     Direction       := drIn;
     Datatype        := dtSingle;
     DefaultValue    := '1';
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
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSEPhaserModule.PlugStateChange(const CurrentPin: TSEPin);
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

 case TSEPhaserPins(CurrentPin.PinID) of
       pinDepth: FPhaser.Depth    := 0.01 * FDepth;
    pinFeedback: FPhaser.Feedback := 0.01 * FFeedback;
     pinMinimum: FPhaser.Minimum  := FMinimum;
     pinMaximum: FPhaser.Maximum  := FMaximum;
        pinRate: FPhaser.Rate     := FRate;
      pinStages: FPhaser.Stages   := FStages;
 end;
end;

end.
