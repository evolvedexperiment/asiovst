unit DAV_DspBufferedAudioFilePlayer;

interface

{$I DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_DspCommon, DAV_DspCircularBuffer,
  DAV_AudioFile, DAV_ChannelDataCoder;

type
  TBufferThread = class(TThread)
  private
    FAudioFile        : TCustomAudioFile;
    FBufferSize       : Integer;
    FBuffer           : TCircularStereoBuffer32;
    FSampleRate       : Single;
    FStreamBuffer     : array [0..1] of PDAVSingleFixedArray;
    FStreamBufSize    : Integer;
    FTimeOut          : Integer;
    FAllowSuspend     : Boolean;
    FCurrentPosition  : Integer;
    FSubBlockPosition : Integer;
    function GetBufferFill: Single;
    procedure SetBufferSize(const Value: Integer);
    procedure SetBlockSize(Value: Integer);
    procedure DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
  protected
    procedure Execute; override;
    procedure BlockSizeChanged; virtual;
    procedure BufferSizeChanged; virtual;
    procedure CalculateTimeOut; virtual;
    procedure SampleRateChanged; virtual;
    procedure AudioFileChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);

    property AllowSuspend: Boolean read FAllowSuspend write FAllowSuspend;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property BlockSize: Integer read FStreamBufSize write SetBlockSize;
    property SampleRate: Single read FSampleRate;
    property BufferFill: Single read GetBufferFill;

    property AudioFile: TCustomAudioFile read FAudioFile;
  end;

  TBufferInterpolation = (biNone, biLinear, biHermite, biBSpline6Point5thOrder);

  TCustomBufferedAudioPlayer = class(TDspObject)
  private
    FSampleRate          : Single;
    FRatio               : Single;
    FAllowSuspend        : Boolean;
    FFractalPos          : Single;
    FInterpolation       : TBufferInterpolation;
    FPitch               : Single;
    FPitchFactor         : Single;
    FInterpolationBuffer : array [0..1] of PDAV8SingleArray;
    function GetBlockSize: Integer;
    function GetBufferSize: Integer;
    function GetBufferFill: Single;
    function GetAudioFile: TCustomAudioFile;
    procedure SetBlockSize(const Value: Integer);
    procedure SetBufferSize(const Value: Integer);
    procedure SetSampleRate(const Value: Single);
    procedure SetAllowSuspend(const Value: Boolean);
    procedure SetInterpolation(const Value: TBufferInterpolation);
    procedure SetPitch(const Value: Single);
  protected
    FBufferThread : TBufferThread;
    procedure CalculatePitchFactor; virtual;
    procedure CalculateSampleRateRatio; virtual;
    procedure SampleRateChanged; virtual;
    procedure InterpolationChanged; virtual;
    procedure PitchChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure GetSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    property AllowSuspend: Boolean read FAllowSuspend write SetAllowSuspend;
    property AudioFile: TCustomAudioFile read GetAudioFile;
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
    property BufferFill: Single read GetBufferFill;
    property BufferSize: Integer read GetBufferSize write SetBufferSize;
    property Interpolation: TBufferInterpolation read FInterpolation write SetInterpolation;
    property Pitch: Single read FPitch write SetPitch;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TBufferedAudioFilePlayer = class(TCustomBufferedAudioPlayer)
  private
    FFileName : TFileName;
    procedure SetFileName(const Value: TFileName);
  protected
    procedure FileNameChanged; virtual;
  public
    constructor Create; override;
  published
    property Filename: TFileName read FFileName write SetFileName;
    property BufferSize;
    property BlockSize;
    property SampleRate;
    property BufferFill;
    property Interpolation;
    property Pitch;
  end;

  TBufferedAudioStreamPlayer = class(TCustomBufferedAudioPlayer)
  private
    FStream : TStream;
    procedure SetStream(const Value: TStream);
  protected
    procedure StreamChanged; virtual;
  public
    constructor Create; override;
  published
    property Stream: TStream read FStream write SetStream;
    property BufferSize;
    property BlockSize;
    property SampleRate;
    property BufferFill;
    property Interpolation;
    property Pitch;
  end;

implementation

uses
  Math, DAV_DspInterpolation;

{ TBufferThread }

constructor TBufferThread.Create;
begin
 inherited Create(True);
 FBufferSize := 16384;
 FSampleRate := 44100;
 FStreamBufSize := 4096;
 FAllowSuspend := False;
 CalculateTimeOut;

 GetMem(FStreamBuffer[0], FStreamBufSize * SizeOf(Single));
 GetMem(FStreamBuffer[1], FStreamBufSize * SizeOf(Single));

 FBuffer := TCircularStereoBuffer32.Create(FBufferSize);
end;

destructor TBufferThread.Destroy;
begin
 Dispose(FStreamBuffer[0]);
 Dispose(FStreamBuffer[1]);
 FreeAndNil(FBuffer);
 FreeAndNil(FAudioFile);
 inherited;
end;

procedure TBufferThread.Execute;
var
  IdleLoops: Integer;
begin
 IdleLoops := 20;
 while not Terminated do
  begin
   while (FBuffer.BufferSize - FBuffer.SamplesInBuffer) > FStreamBufSize do
    begin
     IdleLoops := 20;

     if assigned(FAudioFile) then
      begin
       FAudioFile.OnDecode := DecodeHandler;
       FSubBlockPosition := 0;
       if FCurrentPosition + FStreamBufSize < FAudioFile.SampleFrames
        then FAudioFile.Decode(FCurrentPosition * FAudioFile.ChannelCount, FStreamBufSize)
        else
         begin
          FAudioFile.Decode(FCurrentPosition * FAudioFile.ChannelCount, FAudioFile.SampleFrames - FCurrentPosition);
          FAudioFile.Decode(0, FStreamBufSize - (FAudioFile.SampleFrames - FCurrentPosition));
          FCurrentPosition := 0;
         end;
       Inc(FCurrentPosition, FStreamBufSize);
      end
     else
      begin
       FillChar(FStreamBuffer[0]^, FStreamBufSize * SizeOf(Single), 0);
       FillChar(FStreamBuffer[1]^, FStreamBufSize * SizeOf(Single), 0);
      end;
     FBuffer.WriteBuffer(FStreamBuffer[0], FStreamBuffer[1], FStreamBufSize);
    end;

   Dec(IdleLoops);
   if FAllowSuspend and (IdleLoops <= 0)
    then Suspend
    else Sleep(FTimeOut);
  end;
end;

function TBufferThread.GetBufferFill: Single;
begin
 result := 100 * (FBuffer.SamplesInBuffer / FBuffer.BufferSize); 
end;

procedure TBufferThread.GetSamples(Left, Right: PDAVSingleFixedArray;
  SampleFrames: Integer);
var
  SampleInBuffer: Integer;  
begin
 SampleInBuffer := FBuffer.SamplesInBuffer;
 if SampleFrames < SampleInBuffer
  then FBuffer.ReadBuffer(Left, Right, SampleFrames)
  else
   begin
    if SampleInBuffer > 0
     then FBuffer.ReadBuffer(Left, Right, SampleInBuffer);
    FillChar( Left^, (SampleFrames - SampleInBuffer) * SizeOf(Single), 0);
    FillChar(Right^, (SampleFrames - SampleInBuffer) * SizeOf(Single), 0);
   end;
end;

procedure TBufferThread.LoadFromFile(FileName: TFileName);
var
  AudioFileClass : TAudioFileClass;
begin
 AudioFileClass := FileNameToFormat(FileName);
 if Assigned(AudioFileClass) then
  begin
   if assigned(FAudioFile)
    then FreeAndNil(FAudioFile);

   FAudioFile := AudioFileClass.Create(FileName);
   FCurrentPosition := 0;
  end;

 AudioFileChanged;
end;

procedure TBufferThread.LoadFromStream(Stream: TStream);
var
  AudioFileClass : TAudioFileClass;
begin
 AudioFileClass := StreamToFormat(Stream);
 if Assigned(AudioFileClass) then
  begin
   if assigned(FAudioFile)
    then FreeAndNil(FAudioFile);

   FAudioFile := AudioFileClass.Create(Stream);
   FCurrentPosition := 0;
  end;

 AudioFileChanged;
end;

procedure TBufferThread.DecodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  begin
   if ChannelCount = 0 then Exit else
   if ChannelCount = 1 then
    begin
     Move(ChannelPointer[0]^[0], FStreamBuffer[0]^[FSubBlockPosition], SampleFrames * SizeOf(Single));
     Move(ChannelPointer[0]^[0], FStreamBuffer[1]^[FSubBlockPosition], SampleFrames * SizeOf(Single));
    end
   else
    begin
     Move(ChannelPointer[0]^[0], FStreamBuffer[0]^[FSubBlockPosition], SampleFrames * SizeOf(Single));
     Move(ChannelPointer[1]^[0], FStreamBuffer[1]^[FSubBlockPosition], SampleFrames * SizeOf(Single));
    end;
   FSubBlockPosition := FSubBlockPosition + SampleFrames;
  end;
end;

procedure TBufferThread.AudioFileChanged;
begin
 if assigned(FAudioFile) then
  begin
   FSampleRate := FAudioFile.SampleRate;
   SampleRateChanged;
  end;
end;

procedure TBufferThread.Reset;
begin
 FCurrentPosition := 0;
end;

procedure TBufferThread.BufferSizeChanged;
begin
 FBuffer.BufferSize := FBufferSize;
end;

procedure TBufferThread.BlockSizeChanged;
begin
 ReallocMem(FStreamBuffer[0], FStreamBufSize * SizeOf(Single));
 ReallocMem(FStreamBuffer[1], FStreamBufSize * SizeOf(Single));
end;

procedure TBufferThread.CalculateTimeOut;
begin
 FTimeOut := round(1000 * FStreamBufSize / FSampleRate);
end;

procedure TBufferThread.SampleRateChanged;
begin
 CalculateTimeOut;
end;

procedure TBufferThread.SetBlockSize(Value: Integer);
begin
 if Value > FBufferSize div 2
  then Value := FBufferSize div 2;
 
 if FStreamBufSize <> Value then
  begin
   FStreamBufSize := Value;
   FAudioFile.BlockSize := FAudioFile.ChannelCount * FStreamBufSize;
   BlockSizeChanged;
  end;
end;

procedure TBufferThread.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   if BlockSize > FBufferSize div 2
    then BlockSize := FBufferSize div 2;
   
   BufferSizeChanged;
  end;
end;

{ TCustomBufferedAudioPlayer }

constructor TCustomBufferedAudioPlayer.Create;
begin
 inherited;
 FSampleRate := 44100;
 FAllowSuspend := False; 
 FBufferThread := TBufferThread.Create;
 FBufferThread.Priority := tpNormal;
 FBufferThread.AllowSuspend := FAllowSuspend;
end;

destructor TCustomBufferedAudioPlayer.Destroy;
begin
 with FBufferThread do
  begin
   if Suspended
    then Resume;
   Terminate;
   WaitFor;
  end;
 FreeAndNil(FBufferThread);
 inherited;
end;

function TCustomBufferedAudioPlayer.GetBlockSize: Integer;
begin
 result := FBufferThread.BlockSize;
end;

function TCustomBufferedAudioPlayer.GetBufferFill: Single;
begin
 result := FBufferThread.BufferFill;
end;

function TCustomBufferedAudioPlayer.GetBufferSize: Integer;
begin
 result := FBufferThread.BufferSize;
end;

function TCustomBufferedAudioPlayer.GetAudioFile: TCustomAudioFile;
begin
 result := FBufferThread.AudioFile;
end;

procedure TCustomBufferedAudioPlayer.SetAllowSuspend(const Value: Boolean);
begin
 if FAllowSuspend <> Value then
  begin
   FAllowSuspend := Value;
   FBufferThread.AllowSuspend := True;
  end;
end;

procedure TCustomBufferedAudioPlayer.SetBlockSize(const Value: Integer);
begin
 FBufferThread.BlockSize := Value;
end;

procedure TCustomBufferedAudioPlayer.SetBufferSize(const Value: Integer);
begin
 FBufferThread.BufferSize := Value;
end;

procedure TCustomBufferedAudioPlayer.SetInterpolation(
  const Value: TBufferInterpolation);
begin
 if FInterpolation <> Value then
  begin
   FInterpolation := Value;
   InterpolationChanged;
  end;
end;

procedure TCustomBufferedAudioPlayer.SetPitch(const Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

procedure TCustomBufferedAudioPlayer.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomBufferedAudioPlayer.SampleRateChanged;
begin
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedAudioPlayer.InterpolationChanged;
begin
 case FInterpolation of
  biNone:
   begin
    ReallocMem(FInterpolationBuffer[0], 1 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 1 * SizeOf(Single));
   end;
  biLinear:
   begin
    ReallocMem(FInterpolationBuffer[0], 2 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 2 * SizeOf(Single));
   end;
  biHermite:
   begin
    ReallocMem(FInterpolationBuffer[0], 4 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 4 * SizeOf(Single));
   end;
  biBSpline6Point5thOrder:
   begin
    ReallocMem(FInterpolationBuffer[0], 6 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 6 * SizeOf(Single));
   end;
 end;
end;

procedure TCustomBufferedAudioPlayer.CalculatePitchFactor;
begin
 FPitchFactor := Power(2, FPitch / 12);
end;

procedure TCustomBufferedAudioPlayer.PitchChanged;
begin
 CalculatePitchFactor;
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedAudioPlayer.CalculateSampleRateRatio;
begin
 FRatio := FPitchFactor * FBufferThread.SampleRate / FSampleRate;
end;

procedure TCustomBufferedAudioPlayer.GetSamples(Left, Right: PDAVSingleFixedArray;
  SampleFrames: Integer);
var
  Sample : Integer;  
begin
 // eventually reactivate thread
 if FAllowSuspend and FBufferThread.Suspended then FBufferThread.Resume;
 if FRatio = 1
  then FBufferThread.GetSamples(Left, Right, SampleFrames)
  else
   case FInterpolation of
    biNone:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := FInterpolationBuffer[0]^[0];
       Right^[Sample] := FInterpolationBuffer[1]^[0];
      end;
    biLinear:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         FInterpolationBuffer[0]^[1] := FInterpolationBuffer[0]^[0];
         FInterpolationBuffer[1]^[1] := FInterpolationBuffer[1]^[0];
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := LinearInterpolation(1 - FFractalPos, PDAV2SingleArray(FInterpolationBuffer[0]));
       Right^[Sample] := LinearInterpolation(1 - FFractalPos, PDAV2SingleArray(FInterpolationBuffer[1]));
      end;
    biHermite:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         Move(FInterpolationBuffer[0]^[0], FInterpolationBuffer[0]^[1], 3 * SizeOf(Single));
         Move(FInterpolationBuffer[1]^[0], FInterpolationBuffer[1]^[1], 3 * SizeOf(Single));
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := Hermite32_asm(1 - FFractalPos, PDAV4SingleArray(FInterpolationBuffer[0]));
       Right^[Sample] := Hermite32_asm(1 - FFractalPos, PDAV4SingleArray(FInterpolationBuffer[1]));
      end;
    biBSpline6Point5thOrder:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         Move(FInterpolationBuffer[0]^[0], FInterpolationBuffer[0]^[1], 5 * SizeOf(Single));
         Move(FInterpolationBuffer[1]^[0], FInterpolationBuffer[1]^[1], 5 * SizeOf(Single));
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := BSplineInterpolation6Point5thOrder(1 - FFractalPos, PDAV6SingleArray(FInterpolationBuffer[0])^);
       Right^[Sample] := BSplineInterpolation6Point5thOrder(1 - FFractalPos, PDAV6SingleArray(FInterpolationBuffer[1])^);
      end;
   end;
end;

procedure TCustomBufferedAudioPlayer.Reset;
begin
 FBufferThread.Reset;
end;

{ TBufferedAudioFilePlayer }

procedure TBufferedAudioFilePlayer.SetFileName(const Value: TFileName);
begin
 if FFileName <> Value then
  begin
   FFileName := Value;
   FileNameChanged;
  end;
end;

constructor TBufferedAudioFilePlayer.Create;
begin
 inherited;
 FFileName := '';
end;

procedure TBufferedAudioFilePlayer.FileNameChanged;
begin
 with FBufferThread do
  begin
   LoadFromFile(FFileName);
   CalculateSampleRateRatio;
   Resume;
  end;
end;

{ TBufferedAudioStreamPlayer }

constructor TBufferedAudioStreamPlayer.Create;
begin
 inherited;
 FStream := nil;
end;

procedure TBufferedAudioStreamPlayer.SetStream(const Value: TStream);
begin
 if FStream <> Value then
  begin
   FStream := Value;
   StreamChanged;
  end;
end;

procedure TBufferedAudioStreamPlayer.StreamChanged;
begin
 with FBufferThread do
  begin
   LoadFromStream(FStream);
   CalculateSampleRateRatio;
   Resume;
  end;
end;

end.