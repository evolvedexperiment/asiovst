unit SEAudioFileOscillatorModule;

interface

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Common, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_AudioData,
  DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  // define some constants to make referencing in/outs clearer
  TSEAudioFileOscillatorPins = (pinFileName, pinInterpolation, pinReset,
    pinPlaybackSpeed, pinOutput, pinAudiodata);

  TInterpolationMethod = (imNone, imLinear, imHermite, imBSpline4);

  TSEAudioFileOscillatorModule = class(TSEModuleBase)
  private
    FOutputBuffer       : PDAVSingleFixedArray;
    FPlaybackSpeed      : PDAVSingleFixedArray;
    FAudioData          : TAudioDataCollection32;
    FFileName           : TFileName;
    FPosition           : Single;
    FReset              : Boolean;
    FInterpolation      : TInterpolationMethod;
    FCriticalSection    : TCriticalSection;
    {$IFDEF UseEmbedding}
    FContainedData      : TStringList;
    FExtraOutputBuffers : array of PDAVSingleFixedArray;
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
    procedure SubProcessBypass(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessNoInterpolation(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessLinearInterpolation(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessHermiteInterpolation(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessBSpline4Interpolation(const BufferOffset, SampleFrames: Integer);
    {$IFDEF UseEmbedding}
    procedure SubProcessNoInterpolationMulti(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessLinearInterpolationMulti(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessHermiteInterpolationMulti(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessBSpline4InterpolationMulti(const BufferOffset, SampleFrames: Integer);
    {$ENDIF}
  end;

implementation

uses
  DAV_DspInterpolation;

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
 EnumResourceNames(HInstance, 'WAVETABLE', @EnumNamesFunc, LongWord(FContainedData));

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
 FInterpolation := imNone;
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
  pinInterpolation : ChooseProcess;
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
   RS := TResourceStream.Create(HInstance, FContainedData[ID], 'WAVETABLE');
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
  else
    {$IFDEF UseEmbedding}
    if (FContainedData.Count = 1) and (FAudioData.ChannelCount > 1) then
     case FInterpolation of
          imNone : OnProcess := SubProcessNoInterpolationMulti;
        imLinear : OnProcess := SubProcessLinearInterpolationMulti;
       imHermite : OnProcess := SubProcessHermiteInterpolationMulti;
      imBSpline4 : OnProcess := SubProcessBSpline4InterpolationMulti;
     end
    else
    {$ENDIF}
     case FInterpolation of
          imNone : OnProcess := SubProcessNoInterpolation;
        imLinear : OnProcess := SubProcessLinearInterpolation;
       imHermite : OnProcess := SubProcessHermiteInterpolation;
      imBSpline4 : OnProcess := SubProcessBSpline4Interpolation;
     end;
end;

// The most important part, processing the audio
procedure TSEAudioFileOscillatorModule.SubProcessNoInterpolation(const BufferOffset,
  SampleFrames: Integer);
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

{$IFDEF UseEmbedding}
procedure TSEAudioFileOscillatorModule.SubProcessNoInterpolationMulti(const BufferOffset,
  SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     FOutputBuffer[BufferOffset + Sample] := FAudioData[0].ChannelDataPointer^[round(FPosition)];
     for Channel := 0 to Length(FExtraOutputBuffers) - 1
      do FExtraOutputBuffers[Channel, BufferOffset + Sample] :=
           FAudioData[Channel + 1].ChannelDataPointer^[round(FPosition)];

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
{$ENDIF}

procedure TSEAudioFileOscillatorModule.SubProcessLinearInterpolation(const BufferOffset,
  SampleFrames: Integer);
var
  Sample : Integer;
  Offset : Integer;
  Ratio  : Single;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     Offset := FastTrunc(FPosition);
     if Offset + 1 >= FAudioData[0].SampleCount then
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         Ratio * FAudioData[0].ChannelDataPointer^[0] +
         (1 - Ratio) * FAudioData[0].ChannelDataPointer^[Offset]
      end
     else
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         Ratio * FAudioData[0].ChannelDataPointer^[Offset + 1] +
         (1 - Ratio) * FAudioData[0].ChannelDataPointer^[Offset]
      end;

     // wrap around 
     FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];
     Offset := FastTrunc(FPosition) + 1;
     while Offset >= FAudioData[0].SampleCount do
      begin
       FPosition := FPosition - FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
     while Offset < 0 do
      begin
       FPosition := FPosition + FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEAudioFileOscillatorModule.SubProcessLinearInterpolationMulti(
  const BufferOffset, SampleFrames: Integer);
var
  Sample  : Integer;
  Offset  : Integer;
  Channel : Integer;
  Ratio   : Single;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     Offset := FastTrunc(FPosition);
     if Offset + 1 >= FAudioData[0].SampleCount then
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         Ratio * FAudioData[0].ChannelDataPointer^[0] +
         (1 - Ratio) * FAudioData[0].ChannelDataPointer^[Offset];

       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] :=
             Ratio * FAudioData[Channel + 1].ChannelDataPointer^[0] +
             (1 - Ratio) * FAudioData[Channel + 1].ChannelDataPointer^[Offset];
      end
     else
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         Ratio * FAudioData[0].ChannelDataPointer^[Offset + 1] +
         (1 - Ratio) * FAudioData[0].ChannelDataPointer^[Offset];

       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] :=
             Ratio * FAudioData[Channel + 1].ChannelDataPointer^[Offset + 1] +
             (1 - Ratio) * FAudioData[Channel + 1].ChannelDataPointer^[Offset];
      end;

     // wrap around 
     FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];
     Offset := FastTrunc(FPosition) + 1;
     while Offset >= FAudioData[0].SampleCount do
      begin
       FPosition := FPosition - FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
     while Offset < 0 do
      begin
       FPosition := FPosition + FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;
{$ENDIF}

procedure TSEAudioFileOscillatorModule.SubProcessHermiteInterpolation(const BufferOffset,
  SampleFrames: Integer);
var
  Sample : Integer;
  Offset : Integer;
  Ratio  : Single;
  Data   : TDAV4SingleArray;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     Offset := FastTrunc(FPosition);
     if Offset + 3 >= FAudioData[0].SampleCount then
      begin
       Ratio := FPosition - Offset;
       Data[0] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[1] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[2] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[3] := FAudioData[0].ChannelDataPointer^[Offset];

       FOutputBuffer[BufferOffset + Sample] := Hermite32_asm(Ratio, @Data[0]);
      end
     else
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         Hermite32_asm(Ratio, @FAudioData[0].ChannelDataPointer^[Offset]);
      end;

     // wrap around
     FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];
     Offset := FastTrunc(FPosition) + 1;
     while Offset >= FAudioData[0].SampleCount do
      begin
       FPosition := FPosition - FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
     while Offset < 0 do
      begin
       FPosition := FPosition + FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEAudioFileOscillatorModule.SubProcessHermiteInterpolationMulti(
  const BufferOffset, SampleFrames: Integer);
var
  Sample  : Integer;
  Offset  : Integer;
  Channel : Integer;
  Ratio   : Single;
  Data    : TDAV4SingleArray;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     Offset := FastTrunc(FPosition);
     if Offset + 1 >= FAudioData[0].SampleCount then
      begin
       Ratio := FPosition - Offset;
       Data[0] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[1] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[2] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[3] := FAudioData[0].ChannelDataPointer^[Offset];

       FOutputBuffer[BufferOffset + Sample] := Hermite32_asm(Ratio, @Data[0]);
      end
     else
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         Hermite32_asm(Ratio, @FAudioData[0].ChannelDataPointer^[Offset]);

       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] :=
           Hermite32_asm(Ratio, @FAudioData[Channel + 1].ChannelDataPointer^[Offset]);
      end;

     // wrap around 
     FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];
     Offset := FastTrunc(FPosition) + 1;
     while Offset >= FAudioData[0].SampleCount do
      begin
       FPosition := FPosition - FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
     while Offset < 0 do
      begin
       FPosition := FPosition + FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;
{$ENDIF}

procedure TSEAudioFileOscillatorModule.SubProcessBSpline4Interpolation(
  const BufferOffset, SampleFrames: Integer);
var
  Sample : Integer;
  Offset : Integer;
  Ratio  : Single;
  Data   : TDAV4SingleArray;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     Offset := FastTrunc(FPosition);
     if Offset + 3 >= FAudioData[0].SampleCount then
      begin
       Ratio := FPosition - Offset;
       Data[0] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[1] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[2] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[3] := FAudioData[0].ChannelDataPointer^[Offset];

       FOutputBuffer[BufferOffset + Sample] := BSplineInterpolation4Point3rdOrder(Ratio, Data);
      end
     else
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         BSplineInterpolation4Point3rdOrder(Ratio, @FAudioData[0].ChannelDataPointer^[Offset]);
      end;

     // wrap around
     FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];
     Offset := FastTrunc(FPosition) + 1;
     while Offset >= FAudioData[0].SampleCount do
      begin
       FPosition := FPosition - FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
     while Offset < 0 do
      begin
       FPosition := FPosition + FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEAudioFileOscillatorModule.SubProcessBSpline4InterpolationMulti(
  const BufferOffset, SampleFrames: Integer);
var
  Sample  : Integer;
  Offset  : Integer;
  Channel : Integer;
  Ratio   : Single;
  Data    : TDAV4SingleArray;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     Offset := FastTrunc(FPosition);
     if Offset + 1 >= FAudioData[0].SampleCount then
      begin
       Ratio := FPosition - Offset;
       Data[0] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[1] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[2] := FAudioData[0].ChannelDataPointer^[Offset];

       inc(Offset);
       if Offset >= FAudioData[0].SampleCount then Offset := 0;
       Data[3] := FAudioData[0].ChannelDataPointer^[Offset];

       FOutputBuffer[BufferOffset + Sample] := BSplineInterpolation4Point3rdOrder(Ratio, Data);
      end
     else
      begin
       Ratio := FPosition - Offset;
       FOutputBuffer[BufferOffset + Sample] :=
         BSplineInterpolation4Point3rdOrder(Ratio, @FAudioData[0].ChannelDataPointer^[Offset]);

       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] :=
           BSplineInterpolation4Point3rdOrder(Ratio, @FAudioData[Channel + 1].ChannelDataPointer^[Offset]);
      end;

     // wrap around
     FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];
     Offset := FastTrunc(FPosition) + 1;
     while Offset >= FAudioData[0].SampleCount do
      begin
       FPosition := FPosition - FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
     while Offset < 0 do
      begin
       FPosition := FPosition + FAudioData[0].SampleCount;
       Offset := FastTrunc(FPosition) + 1;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;
{$ENDIF}

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
  EnumResourceNames(HInstance, 'WAVETABLE', @EnumNamesFunc, LongWord(ContainedData));
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
      ID := PAnsiChar(str);
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
  pinInterpolation : with Properties^ do
                      begin
                       Name            := 'Interpolation';
                       VariableAddress := @FInterpolation;
                       Direction       := drIn;
                       Datatype        := dtEnum;
                       DatatypeExtra   := 'none, linear, hermite, bspline4';
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
      pinAudiodata : with Properties^ do
                      begin
                       Name            := 'Audiodata';
                       VariableAddress := @FOutputBuffer;
                       Direction       := drOut;
                       Datatype        := dtExperimental;
                      end;
  else
   begin
    result := False;
    {$IFDEF UseEmbedding}
    if (FContainedData.Count = 1) then
     begin
      LoadFromResource(0);
      SetLength(FExtraOutputBuffers, FAudioData.ChannelCount - 1);
      if Index - Integer(pinOutput) < FAudioData.ChannelCount then
       with Properties^ do
        begin
         Name            := 'Output';
         VariableAddress := @FExtraOutputBuffers[Index - Integer(pinOutput) - 1];
         Direction       := drOut;
         Datatype        := dtFSample;
         result          := True;
        end;
     end;
    {$ENDIF}
   end;
 end;
end;

end.
