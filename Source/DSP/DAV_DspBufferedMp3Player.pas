unit DAV_DspBufferedMp3Player;

interface

{$I DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_DspCommon, DAV_DspCircularBuffer,
  DAV_MpegAudio;

type
  TBufferThread = class(TThread)
  private
    FMP3        : TMPEGAudio;
    FBufferSize : Integer;
    FBuffer     : TCircularStereoBuffer32;
    FSampleRate : Single;
    FMP3Buffer  : array [0..1] of PDAVSingleFixedArray;
    FMP3BufSize : Integer;
    FTimeOut    : Integer;
    procedure SetBufferSize(const Value: Integer);
    procedure SetBlockSize(Value: Integer);
    function GetBufferFill: Single;
  protected
    procedure Execute; override;
    procedure BlockSizeChanged; virtual;
    procedure BufferSizeChanged; virtual;
    procedure CalculateTimeOut; virtual;
    procedure SampleRateChanged; virtual;
    procedure MP3Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);

    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property BlockSize: Integer read FMP3BufSize write SetBlockSize;
    property SampleRate: Single read FSampleRate;
    property BufferFill: Single read GetBufferFill;
  end;

  TCustomBufferedMP3Player = class(TDspObject)
  private
    FSampleRate : Single;
    FRatio      : Single;
    function GetBlockSize: Integer;
    function GetBufferSize: Integer;
    procedure SetBlockSize(const Value: Integer);
    procedure SetBufferSize(const Value: Integer);
    procedure SetSampleRate(const Value: Single);
    procedure CalculateSampleRateRatio;
    function GetBufferFill: Single;
  protected
    FBufferThread : TBufferThread;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure GetSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    property BufferSize: Integer read GetBufferSize write SetBufferSize;
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property BufferFill: Single read GetBufferFill;
  end;

  TBufferedMP3FilePlayer = class(TCustomBufferedMP3Player)
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
  end;

  TBufferedMP3StreamPlayer = class(TCustomBufferedMP3Player)
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
  end;

implementation

{ TBufferThread }

constructor TBufferThread.Create;
begin
 inherited Create(True);
 FBufferSize := 16384;
 FSampleRate := 44100;
 FMP3BufSize := 4096;
 CalculateTimeOut;

 GetMem(FMP3Buffer[0], FMP3BufSize * SizeOf(Single));
 GetMem(FMP3Buffer[1], FMP3BufSize * SizeOf(Single));

 FBuffer := TCircularStereoBuffer32.Create(FBufferSize);
end;

destructor TBufferThread.Destroy;
begin
 Dispose(FMP3Buffer[0]);
 Dispose(FMP3Buffer[1]);
 FreeAndNil(FBuffer);
 FreeAndNil(FMP3);
 inherited;
end;

procedure TBufferThread.Execute;
var
  IdleLoops: Integer;
begin
 IdleLoops := 10;
 while not Terminated do
  begin
   while (FBuffer.BufferSize - FBuffer.SamplesInBuffer) > FMP3BufSize do
    begin
     IdleLoops := 10;
     if assigned(FMP3) 
      then FMP3.ReadBuffer(FMP3Buffer[0], FMP3Buffer[1], FMP3BufSize)
      else
       begin
        FillChar(FMP3Buffer[0], FMP3BufSize * SizeOf(Single), 0);
        FillChar(FMP3Buffer[1], FMP3BufSize * SizeOf(Single), 0);
       end;
     FBuffer.WriteBuffer(FMP3Buffer[0], FMP3Buffer[1], FMP3BufSize);
    end;
   Dec(IdleLoops);
   if IdleLoops <= 0
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
begin
 if assigned(FMP3) then FreeAndNil(FMP3);
 if FileExists(FileName) then
  begin
   FMP3 := TMPEGAudio.Create(FileName);
   MP3Changed;
  end;
end;

procedure TBufferThread.LoadFromStream(Stream: TStream);
begin
 if assigned(FMP3) then FreeAndNil(FMP3);
 if Stream <> nil then
  begin
   FMP3 := TMPEGAudio.Create(Stream);
   MP3Changed;
  end;
end;

procedure TBufferThread.MP3Changed;
begin
 if assigned(FMP3) then
  begin
   FSampleRate := FMP3.Frequency;
   SampleRateChanged;
  end;
end;

procedure TBufferThread.Reset;
begin
 if assigned(FMP3) then FMP3.Reset;
end;

procedure TBufferThread.BufferSizeChanged;
begin
 FBuffer.BufferSize := FBufferSize;
end;

procedure TBufferThread.BlockSizeChanged;
begin
 ReallocMem(FMP3Buffer[0], FMP3BufSize * SizeOf(Single));
 ReallocMem(FMP3Buffer[1], FMP3BufSize * SizeOf(Single));
end;

procedure TBufferThread.CalculateTimeOut;
begin
 FTimeOut := round(1000 * FMP3BufSize / FSampleRate);
end;

procedure TBufferThread.SampleRateChanged;
begin
 CalculateTimeOut;
end;

procedure TBufferThread.SetBlockSize(Value: Integer);
begin
 if Value > FBufferSize div 2
  then Value := FBufferSize div 2;
 
 if FMP3BufSize <> Value then
  begin
   FMP3BufSize := Value;
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

{ TCustomBufferedMP3Player }

constructor TCustomBufferedMP3Player.Create;
begin
 inherited;
 FSampleRate := 44100; 
 FBufferThread := TBufferThread.Create;
 FBufferThread.Priority := tpHigher;
end;

destructor TCustomBufferedMP3Player.Destroy;
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

function TCustomBufferedMP3Player.GetBlockSize: Integer;
begin
 result := FBufferThread.BlockSize;
end;

function TCustomBufferedMP3Player.GetBufferFill: Single;
begin
 result := FBufferThread.BufferFill;
end;

function TCustomBufferedMP3Player.GetBufferSize: Integer;
begin
 result := FBufferThread.BufferSize;
end;

procedure TCustomBufferedMP3Player.SetBlockSize(const Value: Integer);
begin
 FBufferThread.BlockSize := Value;
end;

procedure TCustomBufferedMP3Player.SetBufferSize(const Value: Integer);
begin
 FBufferThread.BufferSize := Value;
end;

procedure TCustomBufferedMP3Player.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomBufferedMP3Player.SampleRateChanged;
begin
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedMP3Player.CalculateSampleRateRatio;
begin
 FRatio := FBufferThread.SampleRate / FSampleRate;
end;

procedure TCustomBufferedMP3Player.GetSamples(Left, Right: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 // eventually reactivate thread
 if FBufferThread.Suspended then FBufferThread.Resume;
 FBufferThread.GetSamples(Left, Right, SampleFrames);
end;

procedure TCustomBufferedMP3Player.Reset;
begin
 FBufferThread.Reset;
end;

{ TBufferedMP3FilePlayer }

procedure TBufferedMP3FilePlayer.SetFileName(const Value: TFileName);
begin
 if FFileName <> Value then
  begin
   FFileName := Value;
   FileNameChanged;
  end;
end;

constructor TBufferedMP3FilePlayer.Create;
begin
 inherited;
 FFileName := '';
end;

procedure TBufferedMP3FilePlayer.FileNameChanged;
begin
 with FBufferThread do
  begin
   LoadFromFile(FFileName);
   CalculateSampleRateRatio;
   Resume;
  end;
end;

{ TBufferedMP3StreamPlayer }

constructor TBufferedMP3StreamPlayer.Create;
begin
 inherited;
 FStream := nil;
end;

procedure TBufferedMP3StreamPlayer.SetStream(const Value: TStream);
begin
 if FStream <> Value then
  begin
   FStream := Value;
   StreamChanged;
  end;
end;

procedure TBufferedMP3StreamPlayer.StreamChanged;
begin
 with FBufferThread do
  begin
   LoadFromStream(FStream);
   CalculateSampleRateRatio;
   Resume;
  end;
end;

end.
