unit DAV_DspCircularBuffer;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon;

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
    procedure BufferSizeChanged; virtual; abstract;

    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create(const BufferSize: Integer = 0); virtual;
    procedure Reset; virtual;

    property SamplesInBuffer: Integer read GetSamplesInBuffer;
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

implementation

uses
  SysUtils;

{ TCustomCircularBuffer }

constructor TCustomCircularBuffer.Create(const BufferSize: Integer = 0);
begin
 inherited Create;
 FBufferSize := BufferSize;
 if BufferSize > 0
  then BufferSizeChanged;
 ResetBufferPositions;
end;

function TCustomCircularBuffer.GetSamplesInBuffer: Integer;
begin
 if FWriteBufferPos >= FReadBufferPos
  then result := FWriteBufferPos - FReadBufferPos
  else result := FWriteBufferPos + (FBufferSize - FReadBufferPos);
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
  then result := SampleFrames
  else result := SamplesInBuffer;

 if FReadBufferPos + result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer^[FReadBufferPos], Data^[0], PartialSamples * SizeOf(Single));
   Move(FBuffer^[0], Data^[PartialSamples], (result - PartialSamples) * SizeOf(Single));
   FReadBufferPos := (result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer^[FReadBufferPos], Data^[0], result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + result;
  end;
end;

function TCustomCircularBuffer32.WriteBuffer(
  const Data: PDAVSingleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < (FBufferSize - SamplesInBuffer)
  then result := SampleFrames
  else result := (FBufferSize - SamplesInBuffer);

 if FWriteBufferPos + result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move(Data^[0], FBuffer^[FWriteBufferPos], PartialSamples * SizeOf(Single));
   Move(Data^[PartialSamples], FBuffer^[PartialSamples], (result - PartialSamples) * SizeOf(Single));
   FWriteBufferPos := (result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move(Data^[0], FBuffer^[FWriteBufferPos], result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + result;
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
  then result := SampleFrames
  else result := SamplesInBuffer;

 if FReadBufferPos + result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer^[FReadBufferPos], Data^[0], PartialSamples * SizeOf(Double));
   Move(FBuffer^[0], Data^[PartialSamples], (result - PartialSamples) * SizeOf(Double));
   FReadBufferPos := PartialSamples;

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer^[FReadBufferPos], Data^[0], result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + result;
  end;
end;

function TCustomCircularBuffer64.WriteBuffer(
  const Data: PDAVDoubleFixedArray; const SampleFrames: Integer): Integer;
var
  PartialSamples : Integer;
begin
 if SampleFrames < (FBufferSize - SamplesInBuffer)
  then result := SampleFrames
  else result := (FBufferSize - SamplesInBuffer);

 if FWriteBufferPos + result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move(Data^[0], FBuffer^[FWriteBufferPos], PartialSamples * SizeOf(Double));
   Move(Data^[PartialSamples], FBuffer^[PartialSamples], (result - PartialSamples) * SizeOf(Double));
   FWriteBufferPos := PartialSamples;

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move(Data^[0], FBuffer^[FWriteBufferPos], result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + result;
  end;
end;

end.
