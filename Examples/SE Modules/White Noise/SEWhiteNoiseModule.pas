unit SEWhiteNoiseModule;

interface

uses
  DAV_Common, DAV_Complex, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEWhiteNoisePins = (pinOutput);

  TSEWhiteNoiseModule = class(TSEModuleBase)
  private
    FOutputBuffer : PDAVSingleFixedArray;
  protected
    procedure Open; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

procedure TSEWhiteNoiseModule.Open;
begin
 inherited Open;
 // choose which function is used to process audio
 OnProcess := SubProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

// The most important part, processing the audio
procedure TSEWhiteNoiseModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FOutputBuffer[BufferOffset + Sample] := 2 * random - 1;
  end;
end;

// describe your module
class procedure TSEWhiteNoiseModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'White Noise Generator';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV White Noise Generator';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEWhiteNoiseModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEWhiteNoisePins(index) of
  // typical output plug
  pinOutput: with Properties^ do
              begin
               Name            := 'Output';
               VariableAddress := @FOutputBuffer;
               Direction       := drOut;
               Datatype        := dtFSample;
//               Flags           := [iofAutoDuplicate, iofRename, iofCustomisable];
              end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
