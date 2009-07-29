unit SESimpleAudioFileOscillatorModule;

interface

uses
  SysUtils, SyncObjs, DAV_Common, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_AudioData,
  DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  // define some constants to make referencing in/outs clearer
  TSEAudioFileOscillatorPins = (pinFileName, pinReset, pinOutput);

  TSESimpleAudioFileOscillatorModule = class(TSEModuleBase)
  private
    FOutputBuffer    : PDAVSingleFixedArray;
    FAudioData       : TAudioDataCollection32;
    FFileName        : TFileName;
    FPosition        : Integer;
    FReset           : Boolean;
    FCriticalSection : TCriticalSection;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

constructor TSESimpleAudioFileOscillatorModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FAudioData := TAudioDataCollection32.Create(nil);
 FCriticalSection := TCriticalSection.Create;
end;

destructor TSESimpleAudioFileOscillatorModule.Destroy;
begin
 FreeAndNil(FCriticalSection);
 FreeAndNil(FAudioData);
 inherited;
end;

procedure TSESimpleAudioFileOscillatorModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESimpleAudioFileOscillatorModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 inherited;
 case TSEAudioFileOscillatorPins(CurrentPin.PinID) of
  pinFileName : begin
                 FCriticalSection.Enter;
                 try
                  if FileExists(FFileName) then
                   try
                    FAudioData.LoadFromFile(FFileName);
                    FPosition := 0;
                   except
                   end;
                 finally
                  FCriticalSection.Leave
                 end;
                end;
     pinReset : if FReset then
                 begin
                  FPosition := 0;
                  FReset := False;
                  Pin[Integer(pinReset)].TransmitStatusChange(SampleClock, stOneOff);
                 end;
 end;
end;

// The most important part, processing the audio
procedure TSESimpleAudioFileOscillatorModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     FOutputBuffer[BufferOffset + Sample] := FAudioData[0].ChannelDataPointer^[FPosition];
     inc(FPosition);
     if FPosition >= FAudioData[0].SampleCount
      then FPosition := 0;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

// describe your module
class procedure TSESimpleAudioFileOscillatorModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Simple Audio File Oscillator';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Simple Audio File Oscillator';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSESimpleAudioFileOscillatorModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 result := True;
 case TSEAudioFileOscillatorPins(index) of
  // typical output plug
  pinFileName : with Properties^ do
                 begin
                  Name            := 'FileName';
                  VariableAddress := @FFileName;
                  Direction       := drIn;
                  Datatype        := dtText;
                  Flags           := [iofFilename];
                 end;
     pinReset : with Properties^ do
                 begin
                  Name            := 'Reset';
                  VariableAddress := @FReset;
                  Direction       := drIn;
                  Datatype        := dtBoolean;
                 end;
    pinOutput : with Properties^ do
                 begin
                  Name            := 'Output';
                  VariableAddress := @FOutputBuffer;
                  Direction       := drOut;
                  Datatype        := dtFSample;
                 end;
  else result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
