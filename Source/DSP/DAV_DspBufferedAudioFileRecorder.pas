unit DAV_DspBufferedAudioFileRecorder;

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
    procedure EncodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
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

    procedure PutSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    procedure OpenFile(FileName: TFileName);
    procedure OpenStream(Stream: TStream);

    property AllowSuspend: Boolean read FAllowSuspend write FAllowSuspend;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property BlockSize: Integer read FStreamBufSize write SetBlockSize;
    property SampleRate: Single read FSampleRate;
    property BufferFill: Single read GetBufferFill;

    property AudioFile: TCustomAudioFile read FAudioFile;
  end;

  TBufferInterpolation = (biNone, biLinear, biHermite, biBSpline6Point5thOrder);

  TCustomBufferedAudioRecorder = class(TDspObject)
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

    procedure PutSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
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

  TBufferedAudioFileRecorder = class(TCustomBufferedAudioRecorder)
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

  TBufferedAudioStreamRecorder = class(TCustomBufferedAudioRecorder)
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
   while (not Terminated) and
     (FBuffer.SamplesInBuffer > FStreamBufSize) do
    begin
     IdleLoops := 20;

     FBuffer.ReadBuffer(FStreamBuffer[0], FStreamBuffer[1], FStreamBufSize);
     if assigned(FAudioFile) then
      begin
       FAudioFile.OnEncode := EncodeHandler;
       FSubBlockPosition := 0;
       FAudioFile.Encode(FCurrentPosition * FAudioFile.ChannelCount, FStreamBufSize);
       Inc(FCurrentPosition, FStreamBufSize);
      end;
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

procedure TBufferThread.PutSamples(Left, Right: PDAVSingleFixedArray;
  SampleFrames: Integer);
var
  SampleInBuffer: Integer;  
begin
 SampleInBuffer := FBuffer.SamplesInBuffer;
 if (FBuffer.BufferSize - SampleInBuffer) > SampleFrames
  then FBuffer.WriteBuffer(Left, Right, SampleFrames)
  else
   begin
    if SampleInBuffer > 0
     then FBuffer.WriteBuffer(Left, Right, (FBuffer.BufferSize - SampleInBuffer));
    // eventually enlarge buffer here! 
   end;
end;

procedure TBufferThread.OpenFile(FileName: TFileName);
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

procedure TBufferThread.OpenStream(Stream: TStream);
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

procedure TBufferThread.EncodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  begin
   if ChannelCount = 0 then Exit else
   if ChannelCount = 1
    then Move(FStreamBuffer[0]^[FSubBlockPosition], ChannelPointer[0]^[0], SampleFrames * SizeOf(Single))
    else
     begin
      Move(FStreamBuffer[0]^[FSubBlockPosition], ChannelPointer[0]^[0], SampleFrames * SizeOf(Single));
      Move(FStreamBuffer[1]^[FSubBlockPosition], ChannelPointer[1]^[0], SampleFrames * SizeOf(Single));
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

{ TCustomBufferedAudioRecorder }

constructor TCustomBufferedAudioRecorder.Create;
begin
 inherited;
 FSampleRate := 44100;
 FPitchFactor := 1;
 FRatio := 1;
 FAllowSuspend := False; 
 FBufferThread := TBufferThread.Create;
 FBufferThread.Priority := tpNormal;
 FBufferThread.AllowSuspend := FAllowSuspend;
end;

destructor TCustomBufferedAudioRecorder.Destroy;
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

function TCustomBufferedAudioRecorder.GetBlockSize: Integer;
begin
 result := FBufferThread.BlockSize;
end;

function TCustomBufferedAudioRecorder.GetBufferFill: Single;
begin
 result := FBufferThread.BufferFill;
end;

function TCustomBufferedAudioRecorder.GetBufferSize: Integer;
begin
 result := FBufferThread.BufferSize;
end;

function TCustomBufferedAudioRecorder.GetAudioFile: TCustomAudioFile;
begin
 result := FBufferThread.AudioFile;
end;

procedure TCustomBufferedAudioRecorder.SetAllowSuspend(const Value: Boolean);
begin
 if FAllowSuspend <> Value then
  begin
   FAllowSuspend := Value;
   FBufferThread.AllowSuspend := True;
  end;
end;

procedure TCustomBufferedAudioRecorder.SetBlockSize(const Value: Integer);
begin
 FBufferThread.BlockSize := Value;
end;

procedure TCustomBufferedAudioRecorder.SetBufferSize(const Value: Integer);
begin
 FBufferThread.BufferSize := Value;
end;

procedure TCustomBufferedAudioRecorder.SetInterpolation(
  const Value: TBufferInterpolation);
begin
 if FInterpolation <> Value then
  begin
   FInterpolation := Value;
   InterpolationChanged;
  end;
end;

procedure TCustomBufferedAudioRecorder.SetPitch(const Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

procedure TCustomBufferedAudioRecorder.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomBufferedAudioRecorder.SampleRateChanged;
begin
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedAudioRecorder.InterpolationChanged;
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

procedure TCustomBufferedAudioRecorder.CalculatePitchFactor;
begin
 FPitchFactor := Power(2, FPitch / 12);
end;

procedure TCustomBufferedAudioRecorder.PitchChanged;
begin
 CalculatePitchFactor;
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedAudioRecorder.CalculateSampleRateRatio;
begin
 FRatio := FPitchFactor * FBufferThread.SampleRate / FSampleRate;
end;

procedure TCustomBufferedAudioRecorder.PutSamples(Left, Right: PDAVSingleFixedArray;
  SampleFrames: Integer);
var
  Sample : Integer;  
begin
 // eventually reactivate thread
 if FAllowSuspend and FBufferThread.Suspended then FBufferThread.Resume;
 if FRatio = 1
  then FBufferThread.PutSamples(Left, Right, SampleFrames)
  else raise Exception.Create('not yet implemented');
(*
   case FInterpolation of
    biNone:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         FBufferThread.PutSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
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
         FBufferThread.PutSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
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
         FBufferThread.PutSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
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
         FBufferThread.PutSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := BSplineInterpolation6Point5thOrder(1 - FFractalPos, PDAV6SingleArray(FInterpolationBuffer[0])^);
       Right^[Sample] := BSplineInterpolation6Point5thOrder(1 - FFractalPos, PDAV6SingleArray(FInterpolationBuffer[1])^);
      end;
   end;
*)
end;

procedure TCustomBufferedAudioRecorder.Reset;
begin
 FBufferThread.Reset;
end;

{ TBufferedAudioFileRecorder }

procedure TBufferedAudioFileRecorder.SetFileName(const Value: TFileName);
begin
 if FFileName <> Value then
  begin
   FFileName := Value;
   FileNameChanged;
  end;
end;

constructor TBufferedAudioFileRecorder.Create;
begin
 inherited;
 FFileName := '';
end;

procedure TBufferedAudioFileRecorder.FileNameChanged;
begin
 with FBufferThread do
  begin
   OpenFile(FFileName);
   CalculateSampleRateRatio;
   Resume;
  end;
end;

{ TBufferedAudioStreamRecorder }

constructor TBufferedAudioStreamRecorder.Create;
begin
 inherited;
 FStream := nil;
end;

procedure TBufferedAudioStreamRecorder.SetStream(const Value: TStream);
begin
 if FStream <> Value then
  begin
   FStream := Value;
   StreamChanged;
  end;
end;

procedure TBufferedAudioStreamRecorder.StreamChanged;
begin
 with FBufferThread do
  begin
   OpenStream(FStream);
   CalculateSampleRateRatio;
   Resume;
  end;
end;

end.
