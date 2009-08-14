unit SEMp3PlayerModule;

interface

uses
  {$IFDEF UseEmbedding}Windows, Classes, {$ENDIF} SysUtils, SyncObjs,
  DAV_Common, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_MpegAudio;

type
  // define some constants to make referencing in/outs clearer
  TSEMp3PlayerPins = (pinFileName, pinInterpolation, pinTrigger,
    pinMode, pinPlaybackSpeed, pinOutputLeft, pinOutputRight);

  TInterpolationMethod = (imNone, imLinear, imHermite, imBSpline4);
  TLoopMode = (lmLooped, lmOneShot);

  TSEMp3PlayerModule = class(TSEModuleBase)
  private
    FOutputLeftBuffer   : PDAVSingleFixedArray;
    FOutputRightBuffer  : PDAVSingleFixedArray;
    FPlaybackSpeed      : PDAVSingleFixedArray;
    FTrigger            : PDAVSingleFixedArray;
    FFileName           : TFileName;
    FPosition           : Single;
    FMode               : TLoopMode;
    FIsPlaying          : Boolean;
    FInterpolation      : TInterpolationMethod;
    FCriticalSection    : TCriticalSection;
    FLastProcessMethod  : TSE2ProcessEvent;
    FMpegAudio          : TMpegAudio;
    {$IFDEF UseEmbedding}
    FContainedData      : TStringList;
    FExtraOutputBuffers : array of PDAVSingleFixedArray;
    procedure LoadFromResource(ID: Integer);
    {$ENDIF}
    procedure CheckTrigger(Trigger: Boolean);
  protected
    procedure Open; override;
    procedure ChooseProcess; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
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

constructor TSEMp3PlayerModule.Create(
  AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF UseEmbedding}
 FContainedData := TStringList.Create;
 EnumResourceNames(HInstance, 'MP3', @EnumNamesFunc, LongWord(FContainedData));

 if FContainedData.Count > 0
  then Integer(FFileName) := 0
  else FFileName := '';
 {$ENDIF}
end;

destructor TSEMp3PlayerModule.Destroy;
begin
 {$IFDEF UseEmbedding}
 FreeAndNil(FContainedData);
 {$ENDIF}
 FreeAndNil(FCriticalSection);
 FreeAndNil(FMpegAudio);
 inherited;
end;

procedure TSEMp3PlayerModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 FInterpolation := imNone;
 ChooseProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutputLeft)].TransmitStatusChange(SampleClock, stRun);
 Pin[Integer(pinOutputRight)].TransmitStatusChange(SampleClock, stRun);
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
                       if FContainedData.Count <= 0 then
                       {$ENDIF}
                        if FileExists(FFileName) then
                         try
                          FMpegAudio := TMpegAudio.Create(FFileName);
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
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEMp3PlayerModule.LoadFromResource(ID: Integer);
var
  RS  : TResourceStream;
begin
 if (ID >= 0) and (ID < FContainedData.Count) then
  begin
   RS := TResourceStream.Create(HInstance, FContainedData[ID], 'MP3');
   try
    FAudioData.LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;
  end;
end;
{$ENDIF}

procedure TSEMp3PlayerModule.ChooseProcess;
begin
 if  {$IFDEF UseEmbedding} (FContainedData.Count = 0) and {$ENDIF} (not FileExists(FFileName))
  then OnProcess := SubProcessBypass
  else
//   if Pin[Integer(pinInput)].Status = stRun then
    begin
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

     FLastProcessMethod := OnProcess;
(*
    end
  else
   begin
    FStaticCount := BlockSize + FConvolver.IRSize;
    OnProcess := SubProcessStatic;
*)
   end;
end;

procedure TSEMp3PlayerModule.SubProcessStatic(const BufferOffset,
  SampleFrames: Integer);
begin
 FLastProcessMethod(BufferOffset, SampleFrames);
(*
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
*)
end;

procedure TSEMp3PlayerModule.CheckTrigger(Trigger: Boolean);
begin
 // check if trigger state changed
 if Trigger <> FIsPlaying then
  begin
   if Trigger then
    begin
     FIsPlaying := True;
     FPosition := 0;
    end
   else
    if FMode <> lmOneShot
     then FIsPlaying := False;
  end;
end;

// The most important part, processing the audio
procedure TSEMp3PlayerModule.SubProcessNoInterpolation(const BufferOffset,
  SampleFrames: Integer);
var
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
      begin
       FOutputBuffer[BufferOffset + Sample] := FAudioData[0].ChannelDataPointer^[round(FPosition)];

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
          while round(FPosition) >= FAudioData[0].SampleCount
           do FPosition := FPosition - FAudioData[0].SampleCount;
          while round(FPosition) < 0
           do FPosition := FPosition + FAudioData[0].SampleCount;
         end;
      end
     else FOutputBuffer[BufferOffset + Sample] := 0;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEMp3PlayerModule.SubProcessNoInterpolationMulti(const BufferOffset,
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
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
      begin
       FOutputBuffer[BufferOffset + Sample] := FAudioData[0].ChannelDataPointer^[round(FPosition)];
       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] :=
             FAudioData[Channel + 1].ChannelDataPointer^[round(FPosition)];

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
          while round(FPosition) >= FAudioData[0].SampleCount
           do FPosition := FPosition - FAudioData[0].SampleCount;
          while round(FPosition) < 0
           do FPosition := FPosition + FAudioData[0].SampleCount;
         end;
      end
     else
      begin
       FOutputBuffer[BufferOffset + Sample] := 0;
       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] := 0;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$ENDIF}

procedure TSEMp3PlayerModule.SubProcessLinearInterpolation(const BufferOffset,
  SampleFrames: Integer);
var
  Sample  : Integer;
  Offset  : Integer;
  Ratio   : Single;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
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

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
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
         end;
      end
     else FOutputBuffer[BufferOffset + Sample] := 0;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEMp3PlayerModule.SubProcessLinearInterpolationMulti(
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
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
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

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
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
         end;
      end
     else
      begin
       FOutputBuffer[BufferOffset + Sample] := 0;

       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] := 0;
      end;

    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;
{$ENDIF}

procedure TSEMp3PlayerModule.SubProcessHermiteInterpolation(const BufferOffset,
  SampleFrames: Integer);
var
  Sample  : Integer;
  Offset  : Integer;
  Ratio   : Single;
  Data    : TDAV4SingleArray;
begin
 FCriticalSection.Enter;
 try
  if FAudioData.SampleFrames > 0 then
   for Sample := 0 to SampleFrames - 1 do
    begin
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
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

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
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
         end;
      end
     else FOutputBuffer[BufferOffset + Sample] := 0;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEMp3PlayerModule.SubProcessHermiteInterpolationMulti(
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
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
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

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
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
         end;
      end
     else
      begin
       FOutputBuffer[BufferOffset + Sample] := 0;

       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] := 0;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;
{$ENDIF}

procedure TSEMp3PlayerModule.SubProcessBSpline4Interpolation(
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
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
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

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
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
         end;
      end
     else FOutputBuffer[BufferOffset + Sample] := 0;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;

{$IFDEF UseEmbedding}
procedure TSEMp3PlayerModule.SubProcessBSpline4InterpolationMulti(
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
     CheckTrigger(FTrigger[BufferOffset + Sample] > 0.5);

     if FIsPlaying then
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

       // advance position
       FPosition := FPosition + FPlaybackSpeed[BufferOffset + Sample];

       // handle one shot
       if FMode = lmOneShot
        then FIsPlaying := (FPosition >= 0) and (FPosition < FAudioData[0].SampleCount)
        else
         begin
          // wrap around
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
         end;
      end
     else
      begin
       FOutputBuffer[BufferOffset + Sample] := 0;

       for Channel := 0 to Length(FExtraOutputBuffers) - 1
        do FExtraOutputBuffers[Channel, BufferOffset + Sample] := 0;
      end;
    end
   else FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
 finally
  FCriticalSection.Leave;
 end;
end;
{$ENDIF}

procedure TSEMp3PlayerModule.SubProcessBypass(const BufferOffset,
  SampleFrames: Integer);
begin
 FillChar(FOutputBuffer[BufferOffset], SampleFrames * SizeOf(Single), 0);
end;

// describe your module
class procedure TSEMp3PlayerModule.getModuleProperties(Properties : PSEModuleProperties);
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
      // build name
      Name := 'Embedded MP3 Player';
      if (ContainedData.Count = 1) and (Length(ContainedData[0]) <= 11) then
       begin
        str := 'Audio File Osc. (' + Trim(ContainedData[0]) + ')';
        if Length(str) > 31 then SetLength(str, 31);
        GetMem(Name, Length(str) + 1);
        StrPCopy(Name, str);
       end;

      // build ID
      ID := 'DAV MP3 Player';
      str := 'DAV EMP3';
      for i := 0 to ContainedData.Count - 1
       do str := str + ' ' + Trim(ContainedData[i]);
      if Length(str) > 31 then SetLength(str, 31);
      GetMem(ID, Length(str) + 1);
      StrPCopy(ID, str);
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
var
  str : string;
begin
 result := True;
 case TSEMp3PlayerPins(index) of
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
        pinTrigger : with Properties^ do
                      begin
                       Name            := 'Trigger';
                       VariableAddress := @FTrigger;
                       Direction       := drIn;
                       Datatype        := dtFSample;
                      end;
          pinMode : with Properties^ do
                      begin
                       Name            := 'Mode';
                       VariableAddress := @FMode;
                       Direction       := drIn;
                       Datatype        := dtEnum;
                       DatatypeExtra   := 'loop, one shot';
                      end;
  pinPlaybackSpeed : with Properties^ do
                      begin
                       Name            := 'Playback Speed';
                       VariableAddress := @FPlaybackSpeed;
                       Direction       := drIn;
                       Datatype        := dtFSample;
                      end;
     pinOutputLeft : with Properties^ do
                      begin
                       Name            := 'Left';
                       VariableAddress := @FOutputLeftBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
                      end;
    pinOutputRight : with Properties^ do
                      begin
                       Name            := 'Right';
                       VariableAddress := @FOutputRightBuffer;
                       Direction       := drOut;
                       Datatype        := dtFSample;
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
