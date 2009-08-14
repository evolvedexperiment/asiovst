unit SESimpleMp3PlayerModule;

interface

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspBufferedMP3Player;

type
  // define some constants to make referencing in/outs clearer
  TSEMp3PlayerPins = (pinFileName, pinReset, pinOutputLeft, pinOutputRight);

  TSESimpleMp3PlayerModule = class(TSEModuleBase)
  private
    FOutLeftBuffer   : PDAVSingleFixedArray;
    FOutRightBuffer  : PDAVSingleFixedArray;
    FFileName        : TFileName;
    FPosition        : Integer;
    FReset           : Boolean;
    FCriticalSection : TCriticalSection;
    {$IFDEF UseEmbedding}
    FBufferedPlayer  : TBufferedMP3StreamPlayer;
    FResourceStream  : TResourceStream;
    FContainedData   : TStringList;
    procedure LoadFromResource(ID: Integer);
    {$ELSE}
    FBufferedPlayer  : TBufferedMP3FilePlayer;
    {$ENDIF}
  protected
    procedure Open; override;
    procedure Close; override;
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

constructor TSESimpleMp3PlayerModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF UseEmbedding}
 FBufferedPlayer := TBufferedMP3StreamPlayer.Create;
 FContainedData := TStringList.Create;
 EnumResourceNames(HInstance, 'MP3', @EnumNamesFunc, LongWord(FContainedData));

 if FContainedData.Count > 0
  then Integer(FFileName) := 0
  else FFileName := '';
 {$ELSE}
 FBufferedPlayer := TBufferedMP3FilePlayer.Create;
 {$ENDIF}
end;

destructor TSESimpleMp3PlayerModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 if assigned(FBufferedPlayer)
  then FreeAndNil(FBufferedPlayer);
 inherited;
end;

procedure TSESimpleMp3PlayerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSESimpleMp3PlayerModule.Close;
begin
 OnProcess := SubProcessBypass;
 inherited;
end;

procedure TSESimpleMp3PlayerModule.ChooseProcess;
begin
 if {$IFDEF UseEmbedding}(FContainedData.Count = 0) {$ELSE} (not FileExists(FFileName)) {$ENDIF}
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

procedure TSESimpleMp3PlayerModule.PlugStateChange(
  const CurrentPin: TSEPin);
begin
 inherited;
 case TSEMp3PlayerPins(CurrentPin.PinID) of
  pinFileName : begin
                 FCriticalSection.Enter;
                 try
                  {$IFDEF UseEmbedding}
                  if FContainedData.Count > 0
                   then LoadFromResource(Integer(FFileName))
                   else
                  {$ENDIF}
                    if FileExists(FFileName) then
                     try
                      FBufferedPlayer.Filename := FFileName;
                      FPosition := 0;
                     except
                     end;

                  ChooseProcess;
                 finally
                  FCriticalSection.Leave;
                 end;
                end;
     pinReset : if FReset then
                 begin
                  FPosition := 0;
                  FReset := False;
                  if assigned(FBufferedPlayer)
                   then FBufferedPlayer.Reset;
                  Pin[Integer(pinReset)].TransmitStatusChange(SampleClock, stOneOff);
                 end;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSESimpleMp3PlayerModule.LoadFromResource(ID: Integer);
begin
 if (ID >= 0) and (ID < FContainedData.Count) then
  begin
   if assigned(FResourceStream) then FreeAndNil(FResourceStream)
   FResourceStream := TResourceStream.Create(HInstance, FContainedData[ID], 'MP3');
   FBufferedPlayer.Stream := FResourceStream;
  end;
end;
{$ENDIF}

// The most important part, processing the audio
procedure TSESimpleMp3PlayerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 FCriticalSection.Enter;
 try
  FBufferedPlayer.GetSamples(@FOutLeftBuffer^[BufferOffset], @FOutRightBuffer^[BufferOffset], SampleFrames)
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSESimpleMp3PlayerModule.SubProcessBypass(
  const BufferOffset, SampleFrames: Integer);
begin
 FillChar(FOutLeftBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 FillChar(FOutRightBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSESimpleMp3PlayerModule.GetModuleProperties(Properties : PSEModuleProperties);
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
  EnumResourceNames(HInstance, 'MP3', @EnumNamesFunc, LongWord(ContainedData));
  {$ENDIF}
  with Properties^ do
   begin
    {$IFDEF UseEmbedding}
    if ContainedData.Count > 0 then
     begin
      Name := 'Embedded Simple MP3 Player';
      str  := 'DAV ESMP3';
      for i := 0 to ContainedData.Count - 1
       do str := str + ContainedData[i];
      ID := PAnsiChar(str);
     end
    else
    {$ENDIF}
     begin
      Name := 'Simple MP3 Player';
      ID := 'DAV Simple MP3 Player';
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
function TSESimpleMp3PlayerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
{$IFDEF UseEmbedding}
var
  str : string;
{$ENDIF}
begin
 Result := True;
 case TSEMp3PlayerPins(index) of
       pinFileName : with Properties^ do
                      {$IFDEF UseEmbedding}
                      if FContainedData.Count > 0 then
                       begin
                        Name            := 'MP3 ID';
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
     pinOutputLeft : with Properties^ do
                      begin
                       Name            := 'Left';
                       VariableAddress := @FOutLeftBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
    pinOutputRight : with Properties^ do
                      begin
                       Name            := 'Right';
                       VariableAddress := @FOutRightBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.