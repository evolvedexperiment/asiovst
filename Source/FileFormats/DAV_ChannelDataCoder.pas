unit DAV_ChannelDataCoder;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Types, DAV_Common, DAV_HalfFloat;

type
  TDAVByteArray = array [0..0] of Byte;
  PDAVByteArray = ^TDAVByteArray;
  TDAVSmallIntArray = array [0..0] of Smallint;
  PDAVSmallIntArray = ^TDAVSmallIntArray;

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
    function CorrectBlocksize(const Value: Cardinal): Cardinal; virtual;
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
  private
    function GetChannelPointer(Index: Integer): PDAVSingleFixedArray;
  protected
    FChannelArray : array of PDAVSingleFixedArray;
    procedure ChannelCountChanged; override;
    procedure ReallocateChannelMemory; override;
  published
  public
    property ChannelPointer[Index: Integer]: PDAVSingleFixedArray read GetChannelPointer;
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
    procedure SetDitherType(const Value: TDitherType);
    procedure SetBits(const Value: Byte);
    procedure SetSampleSize(const Value: Byte);
    procedure CalculateScaleFactors;
  protected
    FDitherType  : TDitherType;
    FBits        : Byte;
    FSampleSize  : Byte;
    FScaleFactor : Array [0..1] of Single;
    procedure BitsChanged; virtual;
    procedure SampleSizeChanged; virtual;
  public
    constructor Create; override;
    procedure SetBitsAndSampleSize(const Bits, SampleSize: Byte);
    property Dither: TDitherType read FDitherType write SetDitherType default dtNone;
    property Bits: Byte read FBits write SetBits;
    property SampleSize: Byte read FSampleSize write SetSampleSize;
  end;

  TChannel32DataCoderFixed = class(TCustomChannel32DataCoderFixed)
  private
    procedure CalculateSampleFrames;
    procedure CalculateBlockSize;
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  published
    property Dither;
    property Bits;
  end;

  TChannel32DataCoderFloat16 = class(TCustomPCMChannel32DataCoder)
  private
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  end;

  TChannel32DataCoderFloat32 = class(TCustomPCMChannel32DataCoder)
  private
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

    procedure BlockSizeChanged; override;
    procedure SampleFramesChanged; override;
    procedure InterleaveData; override;
    procedure DeinterleaveData; override;
  end;

  TChannel32DataCoderFloat64 = class(TCustomPCMChannel32DataCoder)
  private
  protected
    function CorrectBlocksize(const Value: Cardinal): Cardinal; override;

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
 ChannelCountChanged;
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

function TCustomChannelDataCoder.CorrectBlocksize(const Value: Cardinal): Cardinal;
begin
 result := Value;
end;

procedure TCustomChannelDataCoder.SetBlockSize(const Value: Cardinal);
var
  CorrectedBlocksize: Cardinal;
begin
 CorrectedBlocksize := CorrectBlocksize(Value);
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

function TCustomChannel32DataCoder.GetChannelPointer(
  Index: Integer): PDAVSingleFixedArray;
begin
 if Index in [0..Length(FChannelArray) - 1]
  then result := FChannelArray[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
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
 Stream.Read(FBlockBuffer^[0], FBlockSize);
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
 FDitherType := dtNone;
 FBits       := 32;
 FSampleSize := 4;
 inherited;
 CalculateScaleFactors;
end;

procedure TCustomChannel32DataCoderFixed.CalculateScaleFactors;
begin
 FScaleFactor[0] := (1 shl Bits - 1) - 1;
 FScaleFactor[1] := 1 / FScaleFactor[0];
end;

procedure TCustomChannel32DataCoderFixed.BitsChanged;
begin
 if Bits > 8 * SampleSize then
  begin
   FSampleSize := (Bits + 7) div 8;
   SampleSizeChanged;
  end;
 CalculateScaleFactors;
end;

procedure TCustomChannel32DataCoderFixed.SampleSizeChanged;
begin
 if Bits > 8 * SampleSize then
  begin
   FBits := SampleSize * 8;
   BitsChanged;
  end;
 SampleFramesChanged;
end;

procedure TCustomChannel32DataCoderFixed.SetBits(const Value: Byte);
begin
 if Bits <> Value then
  begin
   FBits := Value;
   BitsChanged;
  end;
end;

procedure TCustomChannel32DataCoderFixed.SetBitsAndSampleSize(const Bits,
  SampleSize: Byte);
begin
 if (Bits > 8 * SampleSize)
  then raise Exception.Create('Number of bits must fit into the sample size!');

 FBits := Bits;
 FSampleSize := SampleSize;

 if BlockSize <> CorrectBlocksize(BlockSize)
  then BlockSize := CorrectBlocksize(BlockSize)
  else BlockSizeChanged;

 CalculateScaleFactors;
end;

procedure TCustomChannel32DataCoderFixed.SetDitherType(const Value: TDitherType);
begin
 if FDitherType <> Value then
  begin
   FDitherType := Value;
  end;
end;

procedure TCustomChannel32DataCoderFixed.SetSampleSize(const Value: Byte);
begin
 if SampleSize <> Value then
  begin
   FSampleSize := Value;
   SampleSizeChanged;
  end;
end;

{ TChannel32DataCoderFixed }

function TChannel32DataCoderFixed.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SampleSize;
 result := Granularity * (Value div Granularity);
end;

procedure TChannel32DataCoderFixed.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
  DataInt : Integer;
begin
 assert(FBlocksize = SampleFrames * FChannelCount * FSampleSize);
 assert(Length(FChannelArray) = Integer(FChannelCount));
 case SampleSize of
  2: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount
       do PDAVSmallIntArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
  3: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1 do
       begin
        DataInt := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);// shl 8;
        Move(DataInt, PDAVByteArray(FBlockBuffer)^[Sample * 3 * FChannelCount + Channel], 3);
       end;
  4: for Sample := 0 to FSampleFrames - 1 do
      for Channel := 0 to FChannelCount
       do PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := round(FChannelArray[Channel]^[Sample] * FScaleFactor[0]);
 end;
end;

procedure TChannel32DataCoderFixed.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(FBlocksize = (SampleFrames * FChannelCount * FSampleSize));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 case SampleSize of
  2: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := PDAVSmallIntArray(FBlockBuffer)^[Sample * FChannelCount + Channel] * FScaleFactor[1];
  3: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := (ShortInt(PDAVByteArray(FBlockBuffer)^[Sample * 3 * FChannelCount + Channel]) shl 16 +
                                              PDAVByteArray(FBlockBuffer)^[Sample * 3 * FChannelCount + Channel + 1] shl 8  +
                                              PDAVByteArray(FBlockBuffer)^[Sample * 3 * FChannelCount + Channel + 2]) * FScaleFactor[1];
  4: for Channel := 0 to FChannelCount - 1 do
      for Sample := 0 to FSampleFrames - 1
       do FChannelArray[Channel]^[Sample] := PIntegerArray(FBlockBuffer)^[Sample * FChannelCount + Channel] * FScaleFactor[1];
 end;
end;

procedure TChannel32DataCoderFixed.CalculateSampleFrames;
begin
 SampleFrames := FBlockSize div FChannelCount div SampleSize;
end;

procedure TChannel32DataCoderFixed.CalculateBlockSize;
begin
 BlockSize := FSampleFrames * ChannelCount * SampleSize;
end;

procedure TChannel32DataCoderFixed.BlockSizeChanged;
begin
 inherited;
 CalculateSampleFrames;
end;

procedure TChannel32DataCoderFixed.SampleFramesChanged;
begin
 CalculateBlockSize;
 ReallocateChannelMemory;
end;


{ TChannel32DataCoderFloat16 }

function TChannel32DataCoderFloat16.CorrectBlocksize(
  const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(THalfFloat);
 result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat16.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(THalfFloat)));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do PDAVHalfFloatFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := SingleToHalfFloat(FChannelArray[Channel]^[Sample]);
end;

procedure TChannel32DataCoderFloat16.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(THalfFloat)));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
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

function TChannel32DataCoderFloat32.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Single);
 result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat32.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Single)));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
    do PDAVSingleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel32DataCoderFloat32.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(FBlocksize = SampleFrames * FChannelCount * SizeOf(Single));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
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

function TChannel32DataCoderFloat64.CorrectBlocksize(const Value: Cardinal): Cardinal;
var
  Granularity : Cardinal;
begin
 Granularity := ChannelCount * SizeOf(Double);
 result := Granularity * ((Value + Granularity - 1) div Granularity);
end;

procedure TChannel32DataCoderFloat64.InterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Double)));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
   do PDAVDoubleFixedArray(FBlockBuffer)^[Sample * FChannelCount + Channel] := FChannelArray[Channel]^[Sample];
end;

procedure TChannel32DataCoderFloat64.DeinterleaveData;
var
  Sample  : Cardinal;
  Channel : Cardinal;
begin
 assert(FBlocksize = (SampleFrames * FChannelCount * SizeOf(Double)));
 assert(Length(FChannelArray) = Integer(FChannelCount));
 for Channel := 0 to FChannelCount - 1 do
  for Sample := 0 to FSampleFrames - 1
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
