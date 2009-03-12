unit DAV_ChannelDataCoder;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Types, DAV_Common, DAV_HalfFloat;

type
  TDAVByteArray = array [0..0] of Byte;
  PDAVByteArray = ^TDAVByteArray;

  TDitherType = (dtNone, dtUniform, dtTriangular, dtGauss);

  TCustomChannelDataCoder = class
  private
    procedure SetChannelCount(const Value: Cardinal);
    procedure SetBlockSize(const Value: Cardinal);
    procedure SetSampleFrames(const Value: Cardinal);
  protected
    FBlockSize    : Cardinal;
    FBlockBuffer  : PDAVByteArray;
    FSampleFrames : Cardinal;
    FChannelCount : Cardinal;
    function CorectBlocksize(const Value: Cardinal): Cardinal; virtual;
    procedure BlockSizeChanged; virtual;
    procedure ReallocateChannelMemory; virtual; abstract;
    procedure ChannelCountChanged; virtual; abstract;
    procedure SampleFramesChanged; virtual; abstract;
  public
    constructor Create; virtual;
    procedure SetBlockSizeAndChannelCount(const BlockSize, ChannelCount: Cardinal); virtual;

    procedure LoadFromStream(const Stream: TStream); virtual; abstract;
    procedure SaveToStream(const Stream: TStream); virtual; abstract;
    procedure LoadFromPointer(const Data: Pointer); virtual; abstract;
    procedure SaveToPointer(const Data: Pointer); virtual; abstract;

    property ChannelCount: Cardinal read FChannelCount write SetChannelCount;
    property BlockSize: Cardinal read FBlockSize write SetBlockSize;
    property SampleFrames: Cardinal read FSampleFrames write SetSampleFrames;
  end;

  TCustomChannel32DataCoder = class(TCustomChannelDataCoder)
  protected
    FChannelArray : array of PDAVSingleFixedArray;
    procedure ChannelCountChanged; override;
    procedure ReallocateChannelMemory; override;
  end;

  TCustomPCMChannel32DataCoder = class(TCustomChannel32DataCoder)
  protected
    procedure InterleaveData; virtual; abstract;
    procedure DeinterleaveData; virtual; abstract;
  public
    procedure LoadFromStream(const Stream: TStream); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure LoadFromPointer(const Data: Pointer); override;
    procedure SaveToPointer(const Data: Pointer); override;
  end;

  TCustomChannel32DataCoderFixed = class(TCustomPCMChannel32DataCoder)
  private
    FDitherType: TDitherType;
    procedure SetDitherType(const Value: TDitherType);
  public
    constructor Create; override;
    property Dither: TDitherType read FDitherType write SetDitherType default dtNone;
  end;

  TChannel32DataCoderFixed32 = class(TCustomChannel32DataCoderFixed)
  private
  protected
    function CorectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  end;

  TChannel32DataCoderFloat16 = class(TCustomPCMChannel32DataCoder)
  private
  protected
    function CorectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  end;

  TChannel32DataCoderFloat32 = class(TCustomPCMChannel32DataCoder)
  private
  protected
    function CorectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  end;

  TChannel32DataCoderFloat64 = class(TCustomPCMChannel32DataCoder)
  private
  protected
    function CorectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  end;

implementation

uses
  SysUtils;


{ TCustomChannelDataCoder }

constructor TCustomChannelDataCoder.Create;
begin
 FBlockSize := 16384;
 FChannelCount := 2;
 BlockSizeChanged;
end;

procedure TCustomChannelDataCoder.BlockSizeChanged;
begin
 ReallocMem(FBlockBuffer, FBlockSize);
end;

procedure TCustomChannelDataCoder.SetBlockSizeAndChannelCount(const BlockSize,
  ChannelCount: Cardinal);
begin
 Self.ChannelCount := ChannelCount;
 Self.BlockSize := BlockSize;
end;

function TCustomChannelDataCoder.CorectBlocksize(const Value: Cardinal): Cardinal;
begin
 result := Value;
end;

procedure TCustomChannelDataCoder.SetBlockSize(const Value: Cardinal);
var
  CorrectedBlocksize: Cardinal;
begin
 CorrectedBlocksize := CorectBlocksize(Value);
 if FBlockSize <> CorrectedBlocksize then
  begin
   FBlockSize := CorrectedBlocksize;
   BlockSizeChanged;
  end;
end;

procedure TCustomChannelDataCoder.SetChannelCount(const Value: Cardinal);
begin
 if FChannelCount <> Value then
  begin
   FChannelCount := Value;
   ChannelCountChanged;
  end;
end;

procedure TCustomChannelDataCoder.SetSampleFrames(const Value: Cardinal);
begin
 if FSampleFrames <> Value then
  begin
   FSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

{ TCustomChannel32DataCoder }

procedure TCustomChannel32DataCoder.ChannelCountChanged;
begin
 SetLength(FChannelArray, FChannelCount);
 ReallocateChannelMemory;
end;

procedure TCustomChannel32DataCoder.ReallocateChannelMemory;
var
  i : Integer;
begin
 for i := 0 to Length(FChannelArray) - 1
  do ReallocMem(FChannelArray[i], FSampleFrames * SizeOf(Single));
end;

{ TCustomPCMChannel32DataCoder }

procedure TCustomPCMChannel32DataCoder.LoadFromStream(const Stream: TStream);
begin
 Stream.Read(FBlockBuffer, FBlockSize);
 DeinterleaveData;
end;

procedure TCustomPCMChannel32DataCoder.LoadFromPointer(const Data: Pointer);
begin
 Move(Data^, FBlockBuffer, FBlockSize);
 DeinterleaveData;
end;

procedure TCustomPCMChannel32DataCoder.SaveToStream(const Stream: TStream);
begin
 InterleaveData;
 Stream.Write(FBlockBuffer, FBlockSize);
end;

procedure TCustomPCMChannel32DataCoder.SaveToPointer(const Data: Pointer);
begin
 InterleaveData;
 Move(FBlockBuffer, Data^, FBlockSize);
end;

{ TCustomChannel32DataCoderFixed }

constructor TCustomChannel32DataCoderFixed.Create;
begin
 inherited;
 FDitherType := dtNone;
end;

procedure TCustomChannel32DataCoderFixed.SetDitherType(const Value: TDitherType);
begin
 if FDitherType <> Value then
  begin
   FDitherType := Value;
  end;
end;

{ TChannel32DataCoderFixed32 }

function TChannel32DataCoderFixed32.CorectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Integer);
 result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFixed32.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(Integer));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := round(FChannelArray[Channel]^[Sample] * MaxInt);
end;

procedure TChannel32DataCoderFixed32.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
const
  CMaxIntInv : Single = 1 / MaxInt;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(Integer));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do FChannelArray[Channel]^[Sample] := PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] * CMaxIntInv;
end;

procedure TChannel32DataCoderFixed32.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(Integer);
end;

procedure TChannel32DataCoderFixed32.SampleFramesChanged;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(Integer);
 ReallocateChannelMemory;
end;


{ TChannel32DataCoderFloat16 }

function TChannel32DataCoderFloat16.CorectBlocksize(
  const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(THalfFloat);
 result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat16.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(THalfFloat));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do PDAVHalfFloatFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := SingleToHalfFloat(FChannelArray[Channel]^[Sample]);
end;

procedure TChannel32DataCoderFloat16.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(THalfFloat));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do FChannelArray[Channel]^[Sample] := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel]);
end;

procedure TChannel32DataCoderFloat16.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(THalfFloat);
end;

procedure TChannel32DataCoderFloat16.SampleFramesChanged;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(THalfFloat);
 ReallocateChannelMemory;
end;

{ TChannel32DataCoderFloat32 }

function TChannel32DataCoderFloat32.CorectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Single);
 result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat32.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(Single));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do PDAVSingleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel32DataCoderFloat32.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(Single));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do FChannelArray[Channel]^[Sample] := PDAVSingleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel];
end;

procedure TChannel32DataCoderFloat32.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(Single);
end;

procedure TChannel32DataCoderFloat32.SampleFramesChanged;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(Single);
 ReallocateChannelMemory;
end;

{ TChannel32DataCoderFloat64 }

function TChannel32DataCoderFloat64.CorectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Double);
 result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat64.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(Double));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do PDAVDoubleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel32DataCoderFloat64.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(SampleFrames = FBlocksize * FChannelCount * SizeOf(Double));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Sample := 0 to FSampleFrames - 1 do
  for Channel := 0 to FChannelCount
   do FChannelArray[Channel]^[Sample] := PDAVDoubleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel];
end;

procedure TChannel32DataCoderFloat64.BlockSizeChanged;
begin
 inherited;
 SampleFrames := FBlockSize div FChannelCount div SizeOf(Double);
end;

procedure TChannel32DataCoderFloat64.SampleFramesChanged;
begin
 BlockSize := FSampleFrames * ChannelCount * SizeOf(Double);
 ReallocateChannelMemory;
end;

end.
