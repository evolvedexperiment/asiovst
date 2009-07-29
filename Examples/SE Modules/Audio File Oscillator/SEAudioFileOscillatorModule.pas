unit SEAudioFileOscillatorModule;

interface

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Common, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_AudioData,
  DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  // define some constants to make referencing in/outs clearer
  TSEAudioFileOscillatorPins = (pinFileName, pinReset, pinPlaybackSpeed,
    pinOutput);

  TSEAudioFileOscillatorModule = class(TSEModuleBase)
  private
    FOutputBuffer    : PDAVSingleFixedArray;
    FPlaybackSpeed   : PDAVSingleFixedArray;
    FAudioData       : TAudioDataCollection32;
    FFileName        : TFileName;
    FPosition        : Single;
    FReset           : Boolean;
    FCriticalSection : TCriticalSection;
    {$IFDEF UseEmbedding}
    FContainedData   : TStringList;
    procedure LoadFromResource(ID: Integer);
    {$ENDIF}
  protected
    procedure Open; override;
    procedure ChooseProcess; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);
  end;

implementation

{$IFDEF UseEmbedding}
function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;
{$ENDIF}

constructor TSEAudioFileOscillatorModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FAudioData := TAudioDataCollection32.Create(nil);
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF UseEmbedding}
 FContainedData := TStringList.Create;
 EnumResourceNames(HInstance, 'Wavetable', @EnumNamesFunc, LongWord(FContainedData));

 if FContainedData.Count > 0
  then Integer(FFileName) := 0
  else FFileName := '';
 {$ENDIF}
end;

destructor TSEAudioFileOscillatorModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 FreeAndNil(FAudioData);
 inherited;
end;

procedure TSEAudioFileOscillatorModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEAudioFileOscillatorModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 inherited;
 case TSEAudioFileOscillatorPins(CurrentPin.PinID) of
  pinFileName : begin
                 FCriticalSection.Enter;
                 try
 {$IFDEF UseEmbedding}
                  if FContainedData.Count <= 0 then
 {$ENDIF}
                   if FileExists(FFileName) then
                    try
                     FAudioData.LoadFromFile(FFileName);
                     FPosition := 0;
                    except
                    end else
 {$IFDEF UseEmbedding}
                   else LoadFromResource(Integer(FFileName));
 {$ENDIF}

                  ChooseProcess;
                 finally
                  FCriticalSection.Leave
                 end;
                end;
     pinReset : if FReset then
                 begin
                  FPosition := 0;
                  FReset := False;
                 end;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEAudioFileOscillatorModule.LoadFromResource(ID: Integer);
var
  RS  : TResourceStream;
begin
 if (ID >= 0) and (ID < FContainedData.Count) then
  begin
   RS := TResourceStream.Create(HInstance, FContainedData[ID], 'Wavetable');
   try
    FAudioData.LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;
  end;
end;
{$ENDIF}

procedure TSEAudioFileOscillatorModule.ChooseProcess;
begin
 if  {$IFDEF UseEmbedding} (FContainedData.Count = 0) and {$ENDIF} (not FileExists(FFileName))
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

// The most important part, processing the audio
procedure TSEAudioFileOscillatorModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     FOutputBuffer[BufferOffset + Sample] := FAudioData[0].ChannelDataPointer^[round(FPosition)];
     FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];
     while round(FPosition) >= FAudioData[0].SampleCount
      do FPosition := FPosition - FAudioData[0].SampleCount;
     while round(FPosition) < 0
      do FPosition := FPosition + FAudioData[0].SampleCount;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSEAudioFileOscillatorModule.SubProcessBypass(const BufferOffset,
  SampleFrames: Integer);
begin
 FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSEAudioFileOscillatorModule.getModuleProperties(Properties : PSEModuleProperties);
{$IFDEF UseEmbedding}
var
  ContainedData : TStringList;
  i             : Integer;
  str           : string;
{$ENDIF}
begin
 {$IFDEF UseEmbedding}
 ContainedData := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'Wavetable', @EnumNamesFunc, LongWord(ContainedData));
 {$ENDIF}
  with Properties^ do
   begin
    {$IFDEF UseEmbedding}
    if ContainedData.Count > 0 then
     begin
      Name := 'Embedded Audio File Oscillator';
      str  := 'DAV EAFO';
      for i := 0 to ContainedData.Count - 1
       do str := str + ContainedData[i];
      ID   := PAnsiChar(str);
     end
    else
    {$ENDIF}
     begin
      Name := 'Audio File Oscillator';
      ID := 'DAV Audio File Oscillator';
     end;

    // Info, may include Author, Web page whatever
    About := 'by Christian-W. Budde';
    SDKVersion := CSeSdkVersion;
   end;
 {$IFDEF UseEmbedding}
 finally
  FreeAndNil(ContainedData);
 end;
 {$ENDIF}
end;

// describe the pins (plugs)
function TSEAudioFileOscillatorModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
var
  str : string;
begin
 result := True;
 case TSEAudioFileOscillatorPins(index) of
       pinFileName : with Properties^ do
                      {$IFDEF UseEmbedding}
                      if FContainedData.Count > 0 then
                       begin
                        Name            := 'Wavetable ID';
                        VariableAddress := @FFileName;
                        Direction       := drIn;
                        DataType        := dtEnum;
                        DefaultValue    := '0';
                        str             := 'range 0,' + IntToStr(FContainedData.Count - 1);
                        DatatypeExtra   := PChar(str);
                       end
                      else
                      {$ENDIF}
                       begin
                        Name            := 'FileName';
                        VariableAddress := @FFileName;
                        Flags           := [iofFilename];
                        Direction       := drIn;
                        DataType        := dtText;
                       end;
          pinReset : with Properties^ do
                      begin
                       Name            := 'Reset';
                       VariableAddress := @FReset;
                       Direction       := drIn;
                       Datatype        := dtBoolean;
                      end;
  pinPlaybackSpeed : with Properties^ do
                      begin
                       Name            := 'Playback Speed';
                       VariableAddress := @FPlaybackSpeed;
                       Direction       := drIn;
                       Datatype        := dtFSample;
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
