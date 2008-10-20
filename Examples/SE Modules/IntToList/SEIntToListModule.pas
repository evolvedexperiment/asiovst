unit SEIntToListModule;

interface

uses
  DAV_Common, DAV_SECommon, DAV_SEModule;

type
  TSEIntToListModule = class(TSEModuleBase)
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    procedure Open; override;
    function GetPinProperties(Index: Integer; Properties : PSEPinProperties): Boolean; override;
    procedure SubProcessSleep(BufferOffset: Integer; SampleFrames: Integer);
  end;

implementation

constructor TSEIntToListModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
end;

procedure TSEIntToListModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcessSleep;
end;

procedure TSEIntToListModule.SubProcessSleep(BufferOffset: Integer; SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

// describe the pins (plugs)
function TSEIntToListModule.GetPinProperties(Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       Direction       := drIn;
       Datatype        := dtInteger;
       Flags           := [iofUICommunication];
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       Direction       := drOut;
       Datatype        := dtEnum;
       Flags           := [iofUICommunication];
      end;
  2: with Properties^ do
      begin
       Name          := 'Mode';
       DatatypeExtra := 'Index,Value';
       Direction     := drIn;
       Datatype      := dtEnum;
       Flags         := [iofUICommunication];
      end
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

end.
