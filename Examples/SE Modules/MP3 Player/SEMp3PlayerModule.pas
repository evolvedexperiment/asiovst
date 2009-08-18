unit SEMp3PlayerModule;

interface

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Common, DAV_SECommon, DAV_SEModule, DAV_DspBufferedMP3Player;

type
  // define some constants to make referencing in/outs clearer
  TSEMp3PlayerPins = (pinFileName, pinBufferSize, pinSemitones, pinInterpolation,
    pinReset, pinOutputLeft, pinOutputRight, pinTitle, pinArtist, pinAlbum,
    pinYear, pinComment);

  TSEMp3PlayerModule = class(TSEModuleBase)
  private
    FOutLeftBuffer   : PDAVSingleFixedArray;
    FOutRightBuffer  : PDAVSingleFixedArray;
    FFileName        : PChar;
    FPosition        : Integer;
    FReset           : Boolean;
    FBufferSize      : Integer;
    FSemitones       : Single;
    FInterpolation   : TBufferInterpolation;
    FTitle           : PChar;
    FArtist          : PChar;
    FAlbum           : PChar;
    FComment         : PChar;
    FYear            : PChar;
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

constructor TSEMp3PlayerModule.Create(
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

destructor TSEMp3PlayerModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 if assigned(FBufferedPlayer)
  then FreeAndNil(FBufferedPlayer);
 inherited;
end;

procedure TSEMp3PlayerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSEMp3PlayerModule.Close;
begin
 OnProcess := SubProcessBypass;
 inherited;
end;

procedure TSEMp3PlayerModule.ChooseProcess;
begin
 if {$IFDEF UseEmbedding}(FContainedData.Count = 0) {$ELSE} (not FileExists(FFileName)) {$ENDIF}
  then OnProcess := SubProcessBypass
  else OnProcess := SubProcess
end;

procedure TSEMp3PlayerModule.PlugStateChange(
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

                       if assigned(FBufferedPlayer.MpegAudio) then
                        with FBufferedPlayer.MpegAudio do
                         begin
                          if assigned(FTitle)   then StrPCopy(FTitle, Id3Title);
                          if assigned(FArtist)  then StrPCopy(FArtist, Id3Artist);
                          if assigned(FAlbum)   then StrPCopy(FAlbum, Id3Album);
                          if assigned(FYear)    then StrPCopy(FYear, Id3Year);
                          if assigned(FComment) then StrPCopy(FComment, Id3Comment);
                         end;

                       ChooseProcess;
                      finally
                       FCriticalSection.Leave;
                      end;
                     end;
     pinBufferSize : begin
                      if FBufferSize < 1024 then FBufferSize := 1024 else
                      if FBufferSize > 65536 then FBufferSize := 65536;
                      FBufferedPlayer.BufferSize := FBufferSize;
                      FBufferedPlayer.BlockSize := FBufferSize div 4;
                     end;
      pinSemitones : begin
                      FBufferedPlayer.Pitch := FSemitones;
                     end;
  pinInterpolation : begin
                      FBufferedPlayer.Interpolation := FInterpolation;
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
procedure TSEMp3PlayerModule.LoadFromResource(ID: Integer);
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
procedure TSEMp3PlayerModule.SubProcess(const BufferOffset, SampleFrames: Integer);
begin
 FCriticalSection.Enter;
 try
  FBufferedPlayer.GetSamples(@FOutLeftBuffer^[BufferOffset], @FOutRightBuffer^[BufferOffset], SampleFrames)
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSEMp3PlayerModule.SubProcessBypass(
  const BufferOffset, SampleFrames: Integer);
begin
 FillChar(FOutLeftBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 FillChar(FOutRightBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSEMp3PlayerModule.GetModuleProperties(Properties : PSEModuleProperties);
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
      Name := 'Embedded MP3 Player';
      str  := 'DAV ESMP3';
      for i := 0 to ContainedData.Count - 1
       do str := str + ContainedData[i];
      ID := PAnsiChar(str);
     end
    else
    {$ENDIF}
     begin
      Name := 'MP3 Player';
      ID := 'DAV MP3 Player';
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
function TSEMp3PlayerModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
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
     pinBufferSize : with Properties^ do
                      begin
                       Name            := 'BufferSize';
                       VariableAddress := @FBufferSize;
                       Direction       := drIn;
                       Datatype        := dtInteger;
                      end;
      pinSemitones : with Properties^ do
                      begin
                       Name            := 'Semitones';
                       VariableAddress := @FSemitones;
                       Direction       := drIn;
                       Datatype        := dtSingle;
                      end;
  pinInterpolation : with Properties^ do
                      begin
                       Name            := 'Interpolation';
                       VariableAddress := @FInterpolation;
                       Direction       := drIn;
                       Datatype        := dtEnum;
                       DatatypeExtra   := 'none, linear, hermite, bspline';
                       DefaultValue    := 'none';
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
          pinTitle : with Properties^ do
                      begin
                       Name            := 'Title';
                       VariableAddress := @FTitle;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
          pinArtist : with Properties^ do
                      begin
                       Name            := 'Artist';
                       VariableAddress := @FArtist;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
          pinAlbum : with Properties^ do
                      begin
                       Name            := 'Album';
                       VariableAddress := @FAlbum;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
           pinYear : with Properties^ do
                      begin
                       Name            := 'Year';
                       VariableAddress := @FYear;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
        pinComment : with Properties^ do
                      begin
                       Name            := 'Comment';
                       VariableAddress := @FComment;
                       Direction       := drOut;
                       Datatype        := dtText;
                      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
