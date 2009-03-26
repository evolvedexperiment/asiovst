unit DAV_DspDelayLines;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TCustomDelayLine = class(TDspObject)
  private
    procedure SetBufferSize(const Value: Integer);
  protected
    FBufferPos  : Integer;
    FBufferSize : Integer;
    procedure BufferSizeChanged; virtual; abstract;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create(const BufferSize: Integer = 0); virtual;
    procedure Reset; virtual;
  end;

  TDelayLineSamples32 = class(TCustomDelayLine)
  private
    function GetSample(Index: Integer): Single;
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Single): Single;
    property Sample[Index: Integer]: Single read GetSample;
  published
    property BufferSize;
  end;

  TDelayLineSamples64 = class(TCustomDelayLine)
  protected
    FBuffer : PDAVDoubleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Double): Double;
  published
    property BufferSize;
  end;

  TCustomDelayLineTime = class(TCustomDelayLine)
  private
    procedure SetSampleRate(const Value: Single);
    procedure SetTime(const Value: Single);
  protected
    FSampleRate : Single;
    FTime       : Single;
    procedure SampleRateChanged; virtual; abstract;
    procedure TimeChanged; virtual; abstract;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    property Samplerate: Single read FSampleRate write SetSampleRate;
    property Time: Single read FTime write SetTime;
  end;

  TDelayLineTime32 = class(TCustomDelayLineTime)
  private
    procedure CalculateBufferSize;
  protected
    FBuffer         : PDAVSingleFixedArray;
    FRealBufferSize : Integer;
    FFractional     : Single;
    FIntBuffer      : TDAV4SingleArray;
    procedure BufferSizeChanged; override;
    procedure SampleRateChanged; override;
    procedure TimeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Single): Single;
  published
    property Samplerate;
    property Time;
  end;

implementation

uses
  SysUtils, DAV_DspInterpolation;

{ TCustomDelayLine }

constructor TCustomDelayLine.Create(const BufferSize: Integer = 0);
begin
 inherited Create;
 FBufferSize := BufferSize;
 if BufferSize > 0
  then BufferSizeChanged;
 FBufferPos  := 0;
end;

procedure TCustomDelayLine.Reset;
begin
 FBufferPos := 0;
end;

procedure TCustomDelayLine.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;


{ TDelayLineSamples32 }

constructor TDelayLineSamples32.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(Buffersize);
end;

destructor TDelayLineSamples32.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function TDelayLineSamples32.GetSample(Index: Integer): Single;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FBufferSize)
  then raise Exception.CreateFmt('Index out of bounds(%d)', [Index]);

 Pos := FBufferPos - Index;
 if Pos < 0
  then Inc(Pos, FBufferSize);
 result := FBuffer^[Pos];
end;

function TDelayLineSamples32.ProcessSample(const Input: Single): Single;
begin
 result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TDelayLineSamples32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;

procedure TDelayLineSamples32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;


{ TDelayLineSamples64 }

constructor TDelayLineSamples64.Create(const BufferSize: Integer = 0);
begin
 inherited Create(BufferSize);
 FBuffer := nil;
end;

destructor TDelayLineSamples64.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineSamples64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

function TDelayLineSamples64.ProcessSample(const Input: Double): Double;
begin
 result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TDelayLineSamples64.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;


{ TCustomDelayLineTime }

constructor TCustomDelayLineTime.Create;
begin
 inherited;
 FTime := 1;
 FSampleRate := 44100;
end;

procedure TCustomDelayLineTime.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDelayLineTime.SetTime(const Value: Single);
begin
 if FTime <> Value then
  begin
   FTime := Value;
   TimeChanged;
  end;
end;


{ TDelayLineTime32 }

constructor TDelayLineTime32.Create;
begin
 inherited;
 FBuffer := nil;
 FIntBuffer[3] := 0;
end;

destructor TDelayLineTime32.Destroy;
begin
// assert(FBuffer[BufferSize - 1] = 0);
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineTime32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

function TDelayLineTime32.ProcessSample(const Input: Single): Single;
begin
 FBuffer^[FBufferPos] := Input;

 inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[1], FIntBuffer[0], 2 * SizeOf(Single));
 FIntBuffer[2] := FBuffer^[FBufferPos];
 result := Hermite32_asm(FFractional, @FIntBuffer);
end;

procedure TDelayLineTime32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;

procedure TDelayLineTime32.CalculateBufferSize;
begin
 BufferSize      := round(FTime * FSampleRate + 0.5) + 1;
 FFractional     := BufferSize - 1 - (FTime * FSampleRate);
 FBuffer[BufferSize - 1] := 0;
 assert(FFractional >= 0);
 assert(FFractional <= 1);
end;

procedure TDelayLineTime32.SampleRateChanged;
begin
 inherited;
 CalculateBufferSize;
end;

procedure TDelayLineTime32.TimeChanged;
begin
 inherited;
 CalculateBufferSize;
end;

end.
