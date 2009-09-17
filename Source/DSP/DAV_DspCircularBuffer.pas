unit DAV_DspCircularBuffer;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TCustomCircularBuffer = class(TDspObject)
  private
    procedure SetBufferSize(const Value: Integer);
    procedure ResetBufferPositions;
    function GetSamplesInBuffer: Integer;
  protected
    FReadBufferPos  : Integer;
    FWriteBufferPos : Integer;
    FBufferSize     : Integer;
    function GetFreeSampleCount: Integer; virtual;
    procedure BufferSizeChanged; virtual; abstract;

    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create(const BufferSize: Integer = 0); virtual;
    procedure Reset; virtual;

    property SamplesInBuffer: Integer read GetSamplesInBuffer;
    property FreeSampleCount: Integer read GetFreeSampleCount;
  end;

  TCustomCircularBuffer32 = class(TCustomCircularBuffer)
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Data: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Data: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
  end;

  TCustomCircularBuffer64 = class(TCustomCircularBuffer)
  protected
    FBuffer : PDAVDoubleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Data: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
  end;

  TCustomCircularStereoBuffer32 = class(TCustomCircularBuffer)
  protected
    FBuffer : array [0..1] of PDAVSingleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Left, Right: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Left, Right: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
  end;

  TCustomCircularStereoBuffer64 = class(TCustomCircularBuffer)
  protected
    FBuffer : array [0..1] of PDAVDoubleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Left, Right: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Left, Right: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
  end;

  TCustomCircularMultiBuffer = class(TCustomCircularBuffer)
  private
    FChannelCount          : Integer;
    FOnChannelCountChanged : TNotifyEvent;
    procedure SetChannelCount(const Value: Integer);
  protected
    procedure ChannelCountChanged; virtual;
  public
    constructor Create(const BufferSize: Integer = 0); override;

    property ChannelCount: Integer read FChannelCount write SetChannelCount default 1;
    property OnChannelCountChanged: TNotifyEvent read FOnChannelCountChanged write FOnChannelCountChanged;
  end;

  TCustomCircularMultiBuffer32 = class(TCustomCircularMultiBuffer)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVSingleFixedArray;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
  end;

  TCustomCircularMultiBuffer64 = class(TCustomCircularMultiBuffer)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVDoubleFixedArray;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer): Integer;
  end;


  TCustomCircularReserveMultiBuffer = class(TCustomCircularMultiBuffer)
  private
    FLatency: Cardinal;
    procedure SetLatency(const Value: Cardinal);
  protected
    function GetFreeSampleCount: Integer; override;
    procedure LatencyChanged; virtual;
  public
    constructor Create(const BufferSize: Integer = 0); override;

    property Latency: Cardinal read FLatency write SetLatency;
  end;

  TCustomCircularReserveMultiBuffer32 = class(TCustomCircularMultiBuffer32)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVSingleFixedArray;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
    function ReadReserveBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
    function WriteReserveBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
  end;

  TCustomCircularReserveMultiBuffer64 = class(TCustomCircularMultiBuffer64)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVDoubleFixedArray;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer): Integer;
    function WriteBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer): Integer;
    function ReadReserveBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
    function WriteReserveBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer): Integer;
  end;


  TCircularBuffer32 = class(TCustomCircularBuffer32)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularBuffer64 = class(TCustomCircularBuffer64)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularStereoBuffer32 = class(TCustomCircularStereoBuffer32)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularStereoBuffer64 = class(TCustomCircularStereoBuffer64)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularMultiBuffer32 = class(TCustomCircularMultiBuffer32)
  published
    property BufferSize;
    property ChannelCount;
    property SamplesInBuffer;
  end;

  TCircularMultiBuffer64 = class(TCustomCircularMultiBuffer64)
  published
    property BufferSize;
    property ChannelCount;
    property SamplesInBuffer;
  end;

implementation

uses
  SysUtils;

resourcestring
  RcStrLatencyTooHigh = 'Latency must be <= Buffersize!';

{ TCustomCircularBuffer }

constructor TCustomCircularBuffer.Create(const BufferSize: Integer = 0);
begin
 inherited Create;
 FBufferSize := BufferSize;
 if BufferSize > 0
  then BufferSizeChanged;
 ResetBufferPositions;
end;

function TCustomCircularBuffer.GetFreeSampleCount: Integer;
begin
 if FReadBufferPos >= FWriteBufferPos
  then Result := FReadBufferPos - FWriteBufferPos
  else Result := FReadBufferPos + (FBufferSize - FWriteBufferPos);
end;

function TCustomCircularBuffer.GetSamplesInBuffer: Integer;
begin
 if FWriteBufferPos >= FReadBufferPos
  then Result := FWriteBufferPos - FReadBufferPos
  else Result := FWriteBufferPos + (FBufferSize - FReadBufferPos);
end;

procedure TCustomCircularBuffer.Reset;
begin
 ResetBufferPositions;
end;

procedure TCustomCircularBuffer.ResetBufferPositions;
begin
 FReadBufferPos := 0;
 FWriteBufferPos := 0;
end;

procedure TCustomCircularBuffer.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;


{ TCustomCircularBuffer32 }

constructor TCustomCircularBuffer32.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(Buffersize);
end;

destructor TCustomCircularBuffer32.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TCustomCircularBuffer32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularBuffer32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;

function TCustomCircularBuffer32.ReadBuffer(const Data: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer^[FReadBufferPos], Data^[0], PartialSamples * SizeOf(Single));
   Move(FBuffer^[0], Data^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer^[FReadBufferPos], Data^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularBuffer32.WriteBuffer(
  const Data: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move(Data^[0], FBuffer^[FWriteBufferPos], PartialSamples * SizeOf(Single));
   Move(Data^[PartialSamples], FBuffer^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move(Data^[0], FBuffer^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;


{ TCustomCircularBuffer64 }

constructor TCustomCircularBuffer64.Create(const BufferSize: Integer = 0);
begin
 inherited Create(BufferSize);
 FBuffer := nil;
end;

destructor TCustomCircularBuffer64.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TCustomCircularBuffer64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

procedure TCustomCircularBuffer64.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;

function TCustomCircularBuffer64.ReadBuffer(
  const Data: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer^[FReadBufferPos], Data^[0], PartialSamples * SizeOf(Double));
   Move(FBuffer^[0], Data^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FReadBufferPos := PartialSamples;

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer^[FReadBufferPos], Data^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularBuffer64.WriteBuffer(
  const Data: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move(Data^[0], FBuffer^[FWriteBufferPos], PartialSamples * SizeOf(Double));
   Move(Data^[PartialSamples], FBuffer^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FWriteBufferPos := PartialSamples;

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move(Data^[0], FBuffer^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;


{ TCustomCircularStereoBuffer32 }

constructor TCustomCircularStereoBuffer32.Create(const BufferSize: Integer);
begin
 FBuffer[0] := nil;
 FBuffer[1] := nil;
 inherited Create(Buffersize);
end;

destructor TCustomCircularStereoBuffer32.Destroy;
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
 inherited;
end;

procedure TCustomCircularStereoBuffer32.BufferSizeChanged;
begin
 ReallocMem(FBuffer[0], FBufferSize * SizeOf(Single));
 ReallocMem(FBuffer[1], FBufferSize * SizeOf(Single));
 FillChar(FBuffer[0]^, FBufferSize * SizeOf(Single), 0);
 FillChar(FBuffer[1]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularStereoBuffer32.Reset;
begin
 inherited;
 FillChar(FBuffer[0]^, FBufferSize * SizeOf(Single), 0);
 FillChar(FBuffer[1]^, FBufferSize * SizeOf(Single), 0);
end;

function TCustomCircularStereoBuffer32.ReadBuffer(const Left,
  Right: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer[0]^[FReadBufferPos],  Left^[0], PartialSamples * SizeOf(Single));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], PartialSamples * SizeOf(Single));
   Move(FBuffer[0]^[0],  Left^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   Move(FBuffer[1]^[0], Right^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer[0]^[FReadBufferPos], Left^[0], Result * SizeOf(Single));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularStereoBuffer32.WriteBuffer(const Left,
  Right: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
   Move( Left^[PartialSamples], FBuffer[0]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   Move(Right^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Single));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;


{ TCustomCircularStereoBuffer64 }

constructor TCustomCircularStereoBuffer64.Create(const BufferSize: Integer);
begin
 inherited Create(BufferSize);
 FBuffer[0] := nil;
 FBuffer[1] := nil;
end;

destructor TCustomCircularStereoBuffer64.Destroy;
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
 inherited;
end;

procedure TCustomCircularStereoBuffer64.BufferSizeChanged;
begin
 ReallocMem(FBuffer[0], FBufferSize * SizeOf(Double));
 ReallocMem(FBuffer[1], FBufferSize * SizeOf(Double));
end;

procedure TCustomCircularStereoBuffer64.Reset;
begin
 inherited;
 FillChar(FBuffer[0]^, FBufferSize * SizeOf(Double), 0);
 FillChar(FBuffer[1]^, FBufferSize * SizeOf(Double), 0);
end;

function TCustomCircularStereoBuffer64.ReadBuffer(const Left,
  Right: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer[0]^[FReadBufferPos],  Left^[0], PartialSamples * SizeOf(Double));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], PartialSamples * SizeOf(Double));
   Move(FBuffer[0]^[0],  Left^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   Move(FBuffer[1]^[0], Right^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FReadBufferPos := PartialSamples;

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer[0]^[FReadBufferPos],  Left^[0], Result * SizeOf(Double));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularStereoBuffer64.WriteBuffer(const Left,
  Right: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
   Move( Left^[PartialSamples], FBuffer[0]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   Move(Right^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FWriteBufferPos := PartialSamples;

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Double));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;


{ TCustomCircularMultiBuffer }

constructor TCustomCircularMultiBuffer.Create(const BufferSize: Integer);
begin
 inherited Create(Buffersize);
 FChannelCount := 1;
 ChannelCountChanged;
end;

procedure TCustomCircularMultiBuffer.ChannelCountChanged;
begin
 if assigned(FOnChannelCountChanged)
  then FOnChannelCountChanged(Self);
end;

procedure TCustomCircularMultiBuffer.SetChannelCount(const Value: Integer);
begin
 if FChannelCount <> Value then
  begin
   FChannelCount := Value;
   ChannelCountChanged;
  end;
end;


{ TCustomCircularMultiBuffer32 }

constructor TCustomCircularMultiBuffer32.Create(const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularMultiBuffer32.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularMultiBuffer32.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularMultiBuffer32.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularMultiBuffer32.AllocateChannelData(Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Single));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularMultiBuffer32.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
 inherited;
end;

function TCustomCircularMultiBuffer32.ReadBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Single));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularMultiBuffer32.WriteBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
     Move(Data[Channel]^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;


{ TCustomCircularMultiBuffer64 }

constructor TCustomCircularMultiBuffer64.Create(const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularMultiBuffer64.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularMultiBuffer64.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularMultiBuffer64.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularMultiBuffer64.AllocateChannelData(Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Double));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
end;

procedure TCustomCircularMultiBuffer64.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
 inherited;
end;

function TCustomCircularMultiBuffer64.ReadBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Double));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularMultiBuffer64.WriteBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
     Move(Data[Channel]^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;


{ TCustomCircularReserveMultiBuffer }

constructor TCustomCircularReserveMultiBuffer.Create(
  const BufferSize: Integer);
begin
 FLatency := 0;
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer.SetLatency(const Value: Cardinal);
begin
 if Value >= FBufferSize
  then raise Exception.Create(RcStrLatencyTooHigh);
 
 if FLatency <> Value then
  begin
   FLatency := Value;
   LatencyChanged;
  end;
end;

function TCustomCircularReserveMultiBuffer.GetFreeSampleCount: Integer;
begin
 Result := inherited GetFreeSampleCount - FLatency;
 assert(Result >= 0);
end;

procedure TCustomCircularReserveMultiBuffer.LatencyChanged;
begin
 if FBufferSize - SamplesInBuffer < FLatency then
  begin
   FWriteBufferPos := FReadBufferPos - FLatency;
   if FWriteBufferPos < 0
    then FWriteBufferPos := FWriteBufferPos + FBufferSize;
  end; 
end;

{ TCustomCircularReserveMultiBuffer32 }

constructor TCustomCircularReserveMultiBuffer32.Create(
  const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularReserveMultiBuffer32.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer32.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularReserveMultiBuffer32.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer32.AllocateChannelData(
  Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Single));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularReserveMultiBuffer32.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
 inherited;
end;

function TCustomCircularReserveMultiBuffer32.ReadBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Single));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularReserveMultiBuffer32.WriteBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
     Move(Data[Channel]^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;

function TCustomCircularReserveMultiBuffer32.ReadReserveBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
begin

end;

function TCustomCircularReserveMultiBuffer32.WriteReserveBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
begin

end;


{ TCustomCircularReserveMultiBuffer64 }

constructor TCustomCircularReserveMultiBuffer64.Create(
  const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularReserveMultiBuffer64.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer64.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularReserveMultiBuffer64.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer64.AllocateChannelData(
  Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Double));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
end;

procedure TCustomCircularReserveMultiBuffer64.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
 inherited;
end;

function TCustomCircularReserveMultiBuffer64.ReadBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Double));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
end;

function TCustomCircularReserveMultiBuffer64.WriteBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Integer): Integer;
var
  Channel        : Integer;
  PartialSamples : Integer;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
     Move(Data[Channel]^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;
end;

function TCustomCircularReserveMultiBuffer64.ReadReserveBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
begin

end;

function TCustomCircularReserveMultiBuffer64.WriteReserveBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Integer): Integer;
begin

end;

end.
