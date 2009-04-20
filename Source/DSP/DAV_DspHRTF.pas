unit DAV_DspHrtf;

interface

{$I DAV_Compiler.INC}

uses
  Classes, Graphics, SysUtils, Contnrs, DAV_Common, DAV_ChunkClasses,
  DAV_HalfFloat, DAV_DspCommon;

type
  TSphereVector2D = record
    Azimuth : Single;   // 0..2*PI
    Polar   : Single;   // 0..PI
  end;

  THrirEncoding = (heInteger, heFloat);
  THrirHeader = record
    Position       : TSphereVector2D;  // position in spherical coordinates
    Flags          : Integer;        // not used yet
    SampleFrames   : Integer;        // samples per channel
    SampleRate     : Single;         // samplerate
    Encoding       : THrirEncoding;  // encoding (integer or float)
    BytesPerSample : Integer;        // bytes used for one sample
  end;

  TInterpolationType = (itNearest, itLinear);

  TCustomHrir = class(TDefinedChunk)
  private
    procedure SetSampleFrames(const Value: Integer);
    procedure SetBytesPerSample(const Value: Integer);
    function GetPosition: TSphereVector2D;
  protected
    FHrirHeader : THrirHeader;
    FBuffer     : array [0..1] of Pointer;
    procedure CreateBuffers; virtual;
    procedure MoveData32(Destination: PDAVSingleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    procedure MoveData64(Destination: PDAVDoubleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    procedure AssignData32(Source: PDAVSingleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    procedure AssignData64(Source: PDAVDoubleFixedArray; Index: Integer; SampleFrames: Integer); virtual;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload; override;
    constructor Create(Azimuth, Polar: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVHalfFloatFixedArray); reintroduce; overload; virtual;
    constructor Create(Azimuth, Polar: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVSingleFixedArray); reintroduce; overload; virtual;
    constructor Create(Azimuth, Polar: Single; const SampleRate: Single;
      const SampleFrames: Integer; const Left, Right: PDAVDoubleFixedArray); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SwapChannels;
    procedure MoveLeft32(Destination: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure MoveRight32(Destination: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure MoveLeft64(Destination: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    procedure MoveRight64(Destination: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignLeft32(Source: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignLeft64(Source: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignRight32(Source: PDAVSingleFixedArray; SampleFrames: Integer); virtual;
    procedure AssignRight64(Source: PDAVDoubleFixedArray; SampleFrames: Integer); virtual;
    class function GetClassChunkName: TChunkName; override;

    property Position: TSphereVector2D read GetPosition;
  published
    property Azimuth: Single read FHrirHeader.Position.Azimuth; 
    property Polar: Single read FHrirHeader.Position.Polar;
    property Encoding: THrirEncoding read FHrirHeader.Encoding;
    property BytesPerSample: Integer read FHrirHeader.BytesPerSample write SetBytesPerSample;
    property SampleFrames: Integer read FHrirHeader.SampleFrames write SetSampleFrames;
    property SampleRate: Single read FHrirHeader.SampleRate;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirGeneralInformationRecord = record
    Title     : ShortString;
    Date      : TDateTime;
    Context   : ShortString;
    Copyright : ShortString;
    Author    : ShortString;
    Notes     : ShortString;
  end;

  TCustomHrirGeneralInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FGeneralInformationRecord : THrirGeneralInformationRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property Title: string index 0 read GetString write SetString;
    property Date: TDateTime read FGeneralInformationRecord.Date write FGeneralInformationRecord.Date;
    property Context: string index 1 read GetString write SetString;
    property Copyright: string index 2 read GetString write SetString;
    property Author: string index 3 read GetString write SetString;
    property Notes: string index 4 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  {$Z1}
  THrirSexType = (stUnknown, stGeneric, stMale, stFemale);
  THrirSubjectRecord = record
    ID     : ShortString;
    Sex    : THrirSexType;
    Desc   : ShortString;
  end;

  TCustomHrirSubjectInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FSubjectRecord : THrirSubjectRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property ID: string index 0 read GetString write SetString;
    property Sex: THrirSexType read FSubjectRecord.Sex write FSubjectRecord.Sex;
    property Description: string index 1 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirRoomRecord = record
    X, Y, Z  : Single;
    RoomType : ShortString;
  end;

  TCustomHrirRoomInformation = class(TDefinedChunk)
  private
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    procedure SetZ(const Value: Single);
    function GetRoomType: string;
    procedure SetRoomType(const Value: string);
  protected
    FRoomRecord : THrirRoomRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property X: Single read FRoomRecord.X write SetX;
    property Y: Single read FRoomRecord.Y write SetY;
    property Z: Single read FRoomRecord.Z write SetZ;
    property RoomType: string read GetRoomType write SetRoomType;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirMicrophoneRecord = record
    MicType      : ShortString;
    Manufacturer : ShortString;
    Notes        : ShortString;
  end;

  TCustomHrirMicrophoneInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FMicrophoneRecord : THrirMicrophoneRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property MicType: string index 0 read GetString write SetString;
    property Manufacturer: string index 1 read GetString write SetString;
    property Notes: string index 2 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirOutboardRecord = record
    ADConverter : ShortString;
    DAConverter : ShortString;
    Amplifier   : ShortString;
    Loudspeaker : ShortString;
  end;

  TCustomHrirOutboardInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
  protected
    FOutboardRecord : THrirOutboardRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property ADConverter: string index 0 read GetString write SetString;
    property DAConverter: string index 1 read GetString write SetString;
    property Amplifier: string index 2 read GetString write SetString;
    property Loudspeaker: string index 3 read GetString write SetString;
  end;

  //////////////////////////////////////////////////////////////////////////////

  THrirMeasurementRecord = record
    Distance        : Single;
    MeasurementType : ShortString;
    MeasuredLength  : Integer;
    ExcitationType  : ShortString;
  end;

  TCustomHrirMeasurementInformation = class(TDefinedChunk)
  private
    function GetString(const Index: Integer): string;
    procedure SetString(const Index: Integer; const Value: string);
    procedure SetMeasuredLength(const Value: Integer);
  protected
    FMeasurementRecord : THrirMeasurementRecord;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property Distance: Single read FMeasurementRecord.Distance write FMeasurementRecord.Distance;
    property MeasurementType: string index 0 read GetString write SetString;
    property MeasuredLength: Integer read FMeasurementRecord.MeasuredLength write SetMeasuredLength;
    property ExcitationType: string index 1 read GetString write SetString;
  end;


(*
  //////////////////////////////////////////////////////////////////////////////

  THrirBitmapRecord = record
    Width   : Integer;
    Height  : Integer;
    Future1 : array[0..1023] of Integer; //Thumbnail
    Future2 : array[0..63] of Integer;
  end;
*)

  TCustomHrtfs = class(TChunkContainer)
  private
    function GetDate: TDateTime;
    function GetDistance: Single;
    function GetGeneralInfoString(const Index: Integer): String;
    function GetMeasuredLength: Integer;
    function GetMeasurementString(const Index: Integer): String;
    function GetMicString(const Index: Integer): String;
    function GetOutboardString(const Index: Integer): String;
    function GetRoomDim(const Index: Integer): Single;
    function GetRoomType: String;
    function GetSex: THrirSexType;
    function GetSubjectString(const Index: Integer): String;
    procedure SetDate(const Value: TDateTime);
    procedure SetDistance(const Value: Single);
    procedure SetGeneralInfoString(const Index: Integer; const Value: String);
    procedure SetMeasuredLength(const Value: Integer);
    procedure SetMeasurementString(const Index: Integer; const Value: String);
    procedure SetMicString(const Index: Integer; const Value: String);
    procedure SetOutboardString(const Index: Integer; const Value: String);
    procedure SetRoomDim(const Index: Integer; const Value: Single);
    procedure SetRoomType(const Value: String);
    procedure SetSex(const Value: THrirSexType);
    procedure SetSubjectString(const Index: Integer; const Value: String);
    function GetHrir(Index: Integer): TCustomHrir;
    function GetHrirCount: Integer;
  protected
    FGeneralInformation     : TCustomHrirGeneralInformation;
    FSubjectInformation     : TCustomHrirSubjectInformation;
    FRoomInformation        : TCustomHrirRoomInformation;
    FMicrophoneInformation  : TCustomHrirMicrophoneInformation;
    FOutboardInformation    : TCustomHrirOutboardInformation;
    FMeasurementInformation : TCustomHrirMeasurementInformation;
    FHrirList               : TObjectList;
    FSampleRate             : Single;
    FInterpolationType      : TInterpolationType;
    procedure ConvertStreamToChunk(ChunkClass: TCustomChunkClass;
      Stream: TStream); override;
    procedure Interpolate2Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure Interpolate2Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray); overload; virtual;
    procedure Interpolate3Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure Interpolate3Hrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray); overload; virtual;
    procedure FindNearestHrirs(const SpherePos: TSphereVector2D;
      var A, B, C: TCustomHrir);
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SwapChannels;
    procedure Clear;
    procedure ClearInformationChunks; virtual;
    procedure ClearHrirs; virtual;

    procedure InterpolateHrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure InterpolateHrir(const Azimuth, Polar: Single;
      const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray); overload; virtual;

    procedure GetHrirByIndex(const Index: Integer; const SampleFrames: Integer;
      const Left, Right: PDavSingleFixedArray); overload; virtual;
    procedure GetHrirByIndex(const Index: Integer; const SampleFrames: Integer;
      const Left, Right: PDavDoubleFixedArray); overload; virtual;

    class function GetClassChunkName: TChunkName; override;
    procedure AddChunk(Chunk: TCustomChunk); override;

    property Hrir[index: Integer]: TCustomHrir read GetHrir;
    property HrirCount: Integer read GetHrirCount; 
    property Title: String index 0 read GetGeneralInfoString write SetGeneralInfoString;
    property Date: TDateTime read GetDate write SetDate;
    property Context: String index 1 read GetGeneralInfoString write SetGeneralInfoString;
    property Copyright: String index 2 read GetGeneralInfoString write SetGeneralInfoString;
    property Author: String index 3 read GetGeneralInfoString write SetGeneralInfoString;
    property Notes: String index 4 read GetGeneralInfoString write SetGeneralInfoString;
    property SubjectID: String index 0 read GetSubjectString write SetSubjectString;
    property SubjectSex: THrirSexType read GetSex write SetSex;
    property SubjectDescription: String index 1 read GetSubjectString write SetSubjectString;
    property RoomType: String read GetRoomType write SetRoomType;
    property RoomLength: Single index 0 read GetRoomDim write SetRoomDim;
    property RoomWidth: Single index 1 read GetRoomDim write SetRoomDim;
    property RoomHeight: Single index 2 read GetRoomDim write SetRoomDim;
    property MicType: String index 0 read GetMicString write SetMicString;
    property MicManufacturer: String index 1 read GetMicString write SetMicString;
    property MicNotes: String index 2 read GetMicString write SetMicString;
    property ADConverter: String index 0 read GetOutboardString write SetOutboardString;
    property DAConverter: String index 1 read GetOutboardString write SetOutboardString;
    property Amplifier: String index 2 read GetOutboardString write SetOutboardString;
    property Loudspeaker: String index 3 read GetOutboardString write SetOutboardString;
    property Distance: Single read GetDistance write SetDistance;
    property MeasurementType: String index 0 read GetMeasurementString write SetMeasurementString;
    property ExcitationType: String index 1 read GetMeasurementString write SetMeasurementString;
    property MeasuredLength: Integer read GetMeasuredLength write SetMeasuredLength;
    property SampleRate: Single read FSampleRate write FSampleRate;
    property InterpolationType: TInterpolationType read FInterpolationType write FInterpolationType default itLinear;
  end;

  THrtfs = class(TCustomHrtfs)
  published
    property Title;
    property Date;
    property Context;
    property Copyright;
    property Author;
    property Notes;
    property SubjectID;
    property SubjectSex;
    property SubjectDescription;
    property RoomType;
    property RoomLength;
    property RoomWidth;
    property RoomHeight;
    property MicType;
    property MicManufacturer;
    property ADConverter;
    property DAConverter;
    property Amplifier;
    property Loudspeaker;
    property Distance;
    property MeasurementType;
    property ExcitationType;
    property MeasuredLength;
    property SampleRate;
    property InterpolationType;
  end;

(*
  public
    constructor Create; overload;
    constructor Create(FileName: TFileName); overload;
    destructor Destroy; override;
    procedure RotateField(Degree: Single);
    procedure GetHRTF(const Horizontal, Vertical, Depth: Single; var HRTF: THRTFArray);

    function Add(Item: THRTFContent): Integer; reintroduce; overload;
    procedure Add(Horizontal, Vertical, Depth: Single; Data: THRTFArray); overload;
    procedure RecalcHorizontalHRTF;
    property Items[Index: Integer]: THRTFContent read Get write Put; default;
    property HorizontalHRTF: THorizontalHRTF read FHorizontal write FHorizontal;
//    property CurrentHRTF: THRTFArray read FCurrent write FCurrent;
  end;
*)

implementation

uses
  Math, DAV_Complex;

resourcestring
  RCStrPositiveValueOnly = 'Value must be larger than 0!';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrChunkAlreadyExists = 'Chunk already exists';

{ Vector Geometry }

function GetOrthodromicDistance(A, B: TSphereVector2D): Single;
begin
  Result := arccos(sin(A.Polar) * sin(B.Polar) +
                   cos(A.Polar) * cos(B.Polar) * cos(B.Azimuth - A.Azimuth));
end;

function GetOrthodromicAngle(A, B: TSphereVector2D): Single;
begin
  Result := sin(A.Polar) * sin(B.Polar) +
            cos(A.Polar) * cos(B.Polar) * cos(B.Azimuth - A.Azimuth);
end;

function MakeSphereVector2D(const Azimuth, Polar: Single): TSphereVector2D;
begin
 result.Azimuth := Azimuth;
 result.Polar   := Polar;
end;


{ TCustomHrir }

constructor TCustomHrir.Create;
begin
 inherited;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);
 with FHrirHeader do
  begin
   SampleFrames   := 512;
   SampleRate     := 44100;
   Encoding       := heFloat;
   BytesPerSample := 4;
  end;
 CreateBuffers;
end;

constructor TCustomHrir.Create(Azimuth, Polar: Single;
  const SampleRate: Single; const SampleFrames: Integer; const Left,
  Right: PDAVHalfFloatFixedArray);
begin
 inherited Create;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);

 FHrirHeader.SampleFrames     := SampleFrames;
 FHrirHeader.Position.Azimuth := Azimuth;
 FHrirHeader.Position.Polar   := Polar;
 FHrirHeader.SampleRate       := SampleRate;
 with FHrirHeader do
  begin
   Encoding       := heFloat;
   BytesPerSample := 2;
  end;
 CreateBuffers;
 Move(Left^[0], PDAVHalfFloatFixedArray(FBuffer[0])^[0], SampleFrames * SizeOf(THalfFloat));
 Move(Right^[0], PDAVHalfFloatFixedArray(FBuffer[1])^[0], SampleFrames * SizeOf(THalfFloat));
 FChunkSize := GetChunkSize;
end;

constructor TCustomHrir.Create(Azimuth, Polar: Single;
  const SampleRate: Single; const SampleFrames: Integer;
  const Left, Right: PDAVSingleFixedArray);
begin
 inherited Create;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);

 FHrirHeader.SampleFrames     := SampleFrames;
 FHrirHeader.Position.Azimuth := Azimuth;
 FHrirHeader.Position.Polar   := Polar;
 FHrirHeader.SampleRate       := SampleRate;
 with FHrirHeader do
  begin
   Encoding       := heFloat;
   BytesPerSample := 4;
  end;
 CreateBuffers;
 Move(Left^[0], PDAVSingleFixedArray(FBuffer[0])^[0], SampleFrames * SizeOf(Single));
 Move(Right^[0], PDAVSingleFixedArray(FBuffer[1])^[0], SampleFrames * SizeOf(Single));
 FChunkSize := GetChunkSize;
end;

procedure TCustomHrir.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomHrir then
  with TCustomHrir(Dest) do
   begin
    FHrirHeader := Self.FHrirHeader;
    CreateBuffers;
    Self.MoveData32(FBuffer[0], 0, Self.SampleFrames * Self.BytesPerSample);
    Self.MoveData32(FBuffer[1], 1, Self.SampleFrames * Self.BytesPerSample);
   end;
end;

constructor TCustomHrir.Create(Azimuth, Polar: Single;
  const SampleRate: Single; const SampleFrames: Integer;
  const Left, Right: PDAVDoubleFixedArray);
begin
 inherited Create;
 FChunkName := GetClassChunkName;
 FillChar(FHrirHeader, SizeOf(THrirHeader), 0);

 FHrirHeader.SampleFrames     := SampleFrames;
 FHrirHeader.Position.Azimuth := Azimuth;
 FHrirHeader.Position.Polar   := Polar;
 FHrirHeader.SampleRate       := SampleRate;
 with FHrirHeader do
  begin
   Encoding       := heFloat;
   BytesPerSample := 8;
  end;
 CreateBuffers;
 Move(Left^[0], PDAVDoubleFixedArray(FBuffer[0])^[0], SampleFrames * SizeOf(Double));
 Move(Right^[0], PDAVDoubleFixedArray(FBuffer[1])^[0], SampleFrames * SizeOf(Double));
 FChunkSize := GetChunkSize;
end;

procedure TCustomHrir.CreateBuffers;
begin
 ReallocMem(FBuffer[0], SampleFrames * BytesPerSample);
 ReallocMem(FBuffer[1], SampleFrames * BytesPerSample);
end;

destructor TCustomHrir.Destroy;
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
 inherited;
end;

function TCustomHrir.GetChunkSize: Cardinal;
begin
 with FHrirHeader
  do result := SizeOf(FHrirHeader) + 2 * SampleFrames * BytesPerSample;
end;

class function TCustomHrir.GetClassChunkName: TChunkName;
begin
 result := 'hrir';
end;

function TCustomHrir.GetPosition: TSphereVector2D;
begin
 result := FHrirHeader.Position;
end;

procedure TCustomHrir.SwapChannels;
var
  Temp : Pointer;
begin
 Temp := FBuffer[0];
 FBuffer[0] := FBuffer[1];
 FBuffer[1] := Temp;
end;

procedure TCustomHrir.SetBytesPerSample(const Value: Integer);
begin
 if FHrirHeader.BytesPerSample <> Value then
  begin
   FHrirHeader.BytesPerSample := Value;
   CreateBuffers;
  end;
end;

procedure TCustomHrir.SetSampleFrames(const Value: Integer);
begin
 if FHrirHeader.SampleFrames <> Value then
  begin
   FHrirHeader.SampleFrames := Value;
   CreateBuffers;
  end;
end;

procedure TCustomHrir.MoveData32(Destination: PDAVSingleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 assert(Index in [0..1]);

 // eventually zero pad IR
 if Self.SampleFrames < SampleFrames then
  begin
   SampleFrames := Self.SampleFrames;
   FillChar(Destination^[Self.SampleFrames], (SampleFrames - Self.SampleFrames) *
     SizeOf(Single), 0);
  end;

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer[Index])^[Sample]);
    4 : move(FBuffer[Index]^, Destination^[0], SampleFrames * SizeOf(Single));
    8 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := PDAVDoubleFixedArray(FBuffer[Index])^[Sample];
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.MoveData64(Destination: PDAVDoubleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 assert(Index in [0..1]);

 // eventually zero pad IR
 if Self.SampleFrames < SampleFrames then
  begin
   SampleFrames := Self.SampleFrames;
   FillChar(Destination^[Self.SampleFrames], (SampleFrames - Self.SampleFrames) *
     SizeOf(Single), 0);
  end;

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := HalfFloatToSingle(PDAVHalfFloatFixedArray(FBuffer[Index])^[Sample]);
    4 : for Sample := 0 to SampleFrames - 1
         do Destination^[Sample] := PDAVSingleFixedArray(FBuffer[Index])^[Sample];
    8 : move(FBuffer[Index]^, Destination^[0], SampleFrames * SizeOf(Single));
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.MoveLeft32(Destination: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 MoveData32(Destination, 0, SampleFrames);
end;

procedure TCustomHrir.MoveRight32(Destination: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 MoveData32(Destination, 1, SampleFrames);
end;

procedure TCustomHrir.MoveLeft64(Destination: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 MoveData64(Destination, 0, SampleFrames);
end;

procedure TCustomHrir.MoveRight64(Destination: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 MoveData64(Destination, 1, SampleFrames);
end;

procedure TCustomHrir.AssignData32(Source: PDAVSingleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 assert(Index in [0..1]);

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : for Sample := 0 to SampleFrames - 1
         do PDAVHalfFloatFixedArray(FBuffer[Index])^[Sample] := SingleToHalfFloat(Source^[Sample]);
    4 : move(Source^[0], FBuffer[Index]^, SampleFrames * SizeOf(Single));
    8 : for Sample := 0 to SampleFrames - 1
         do PDAVDoubleFixedArray(FBuffer[Index])^[Sample] := Source^[Sample];
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.AssignData64(Source: PDAVDoubleFixedArray; Index,
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 assert(Index in [0..1]);

 // decode
 case Encoding of
  heFloat:
   case BytesPerSample of
    2 : raise Exception.Create('not yet implemented');
    4 : for Sample := 0 to SampleFrames - 1
         do PDAVSingleFixedArray(FBuffer[Index])^[Sample] := Source^[Sample];
    8 : move(Source^[0], FBuffer[Index]^, SampleFrames * SizeOf(Single));
    else raise Exception.Create('not yet implemented');
   end;
  heInteger : raise Exception.Create('not yet implemented');
 end;
end;

procedure TCustomHrir.AssignLeft32(Source: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 AssignData32(Source, 0, SampleFrames);
end;

procedure TCustomHrir.AssignRight32(Source: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 AssignData32(Source, 1, SampleFrames);
end;

procedure TCustomHrir.AssignLeft64(Source: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 AssignData64(Source, 0, SampleFrames);
end;

procedure TCustomHrir.AssignRight64(Source: PDAVDoubleFixedArray;
  SampleFrames: Integer);
begin
 AssignData64(Source, 1, SampleFrames);
end;

procedure TCustomHrir.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   // read header
   Read(FHrirHeader, SizeOf(THrirHeader));
   CreateBuffers;

   // check constraints
   assert(Integer(FChunkSize) - SizeOf(THrirHeader) >= 2 * SampleFrames * FHrirHeader.BytesPerSample);
   assert(Size - Position >= 2 * SampleFrames * BytesPerSample);

   // read data
   Read(FBuffer[0]^, SampleFrames * BytesPerSample);
   Read(FBuffer[1]^, SampleFrames * BytesPerSample);
  end;
end;

procedure TCustomHrir.SaveToStream(Stream: TStream);
begin
 FChunkSize := SizeOf(THrirHeader) + 2 * SampleFrames * SizeOf(Single);
 inherited;
 with Stream do
  begin
   // write header
   Write(FHrirHeader, SizeOf(THrirHeader));

   // read data
   Write(FBuffer[0]^, SampleFrames * BytesPerSample);
   Write(FBuffer[1]^, SampleFrames * BytesPerSample);
  end;
end;

{ TCustomHrirGeneralInformation }

constructor TCustomHrirGeneralInformation.Create;
begin
 inherited;
 with FGeneralInformationRecord do
  begin
   Title     := '';
   Date      := Now;
   Context   := '';
   Copyright := '';
   Author    := '';
   Notes     := '';
  end;
end;

function TCustomHrirGeneralInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FGeneralInformationRecord
  do result := 5 + Byte(Title[0]) + SizeOf(Date) + Byte(Context[0]) +
                   Byte(Copyright[0]) + Byte(Author[0]) + Byte(Notes[0]);
end;

class function TCustomHrirGeneralInformation.GetClassChunkName: TChunkName;
begin
 result := 'hrgi';
end;

function TCustomHrirGeneralInformation.GetString(const Index: Integer): string;
begin
 case Index of
  0 : result := FGeneralInformationRecord.Title;
  1 : result := FGeneralInformationRecord.Context;
  2 : result := FGeneralInformationRecord.Copyright;
  3 : result := FGeneralInformationRecord.Author;
  4 : result := FGeneralInformationRecord.Notes;
 end;
end;

procedure TCustomHrirGeneralInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FGeneralInformationRecord.Title := Value;
  1 : FGeneralInformationRecord.Context := Value;
  2 : FGeneralInformationRecord.Copyright := Value;
  3 : FGeneralInformationRecord.Author := Value;
  4 : FGeneralInformationRecord.Notes := Value;
 end;
end;

procedure TCustomHrirGeneralInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FGeneralInformationRecord do
  begin
   // read 'Title' string
   Read(StringSize, 1);
   assert(StringSize + 5 + SizeOf(Date) <= FChunkSize);
   SetLength(Title, StringSize);
   Read(Title[1], StringSize);

   // read date
   Read(Date, SizeOf(Date));

   // read 'Context' string
   Read(StringSize, 1);
   assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) <= FChunkSize);
   SetLength(Context, StringSize);
   Read(Context[1], StringSize);

   // read 'Copyright' string
   Read(StringSize, 1);
   assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) +
     Byte(Context[0]) <= FChunkSize);
   SetLength(Copyright, StringSize);
   Read(Copyright[1], StringSize);

   // read 'Author' string
   Read(StringSize, 1);
   assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) + Byte(Context[0]) +
     Byte(Copyright[0]) <= FChunkSize);
   SetLength(Author, StringSize);
   Read(Author[1], StringSize);

   // read 'Loudspeaker' string
   Read(StringSize, 1);
   assert(StringSize + 5 + SizeOf(Date) + Byte(Title[0]) + Byte(Context[0]) +
     Byte(Copyright[0]) + Byte(Author[0]) <= FChunkSize);
   SetLength(Notes, StringSize);
   Read(Notes[1], StringSize);
  end;
end;

procedure TCustomHrirGeneralInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FGeneralInformationRecord do
  begin
   // write 'Title' string
   Write(Title[0], 1);
   Write(Title[1], Integer(Title[0]));

   // write date
   Write(Date, SizeOf(Date));

   // write 'Context' string
   Write(Context[0], 1);
   Write(Context[1], Integer(Context[0]));

   // write 'Copyright' string
   Write(Copyright[0], 1);
   Write(Copyright[1], Integer(Copyright[0]));

   // write 'Author' string
   Write(Author[0], 1);
   Write(Author[1], Integer(Author[0]));

   // write 'Notes' string
   Write(Notes[0], 1);
   Write(Notes[1], Integer(Notes[0]));
  end;
end;

{ TCustomHrirSubjectInformation }

constructor TCustomHrirSubjectInformation.Create;
begin
 inherited;
 with FSubjectRecord do
  begin
   ID   := '';
   Sex  := stUnknown;
   Desc := '';
  end;
end;

function TCustomHrirSubjectInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FSubjectRecord
  do result := 2 + Byte(ID[0]) + SizeOf(Sex) + Byte(Desc[0]);
end;

class function TCustomHrirSubjectInformation.GetClassChunkName: TChunkName;
begin
 result := 'hrsi';
end;

function TCustomHrirSubjectInformation.GetString(const Index: Integer): string;
begin
 case Index of
  0 : result := FSubjectRecord.ID;
  1 : result := FSubjectRecord.Desc;
 end;
end;

procedure TCustomHrirSubjectInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FSubjectRecord.ID := Value;
  1 : FSubjectRecord.Desc := Value;
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 end;
end;

procedure TCustomHrirSubjectInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FSubjectRecord do
  begin
   // read 'ID' string
   Read(StringSize, 1);
   assert(StringSize + 2 <= FChunkSize);
   SetLength(ID, StringSize);
   Read(ID[1], StringSize);

   // read sex
   Read(FSubjectRecord.Sex, 1);

   // read 'Desc' string
   Read(StringSize, 1);
   assert(StringSize + 2 + Byte(ID[0]) <= FChunkSize);
   SetLength(Desc, StringSize);
   Read(Desc[1], StringSize);
  end;
end;

procedure TCustomHrirSubjectInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FSubjectRecord do
  begin
   // write 'ID' string
   Write(ID[0], 1);
   Write(ID[1], Integer(ID[0]));

   // write sex
   Write(Sex, 1);

   // write 'Desc' string
   Write(Desc[0], 1);
   Write(Desc[1], Integer(Desc[0]));
  end;
end;

{ TCustomHrirRoomInformation }

constructor TCustomHrirRoomInformation.Create;
begin
 inherited;
 with FRoomRecord do
  begin
   X := 0;
   Y := 0;
   Z := 0;
   RoomType := '';
  end;
end;

function TCustomHrirRoomInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FRoomRecord
  do result := 3 * SizeOf(Single) + Byte(RoomType[0]) + 1;
end;

class function TCustomHrirRoomInformation.GetClassChunkName: TChunkName;
begin
 result := 'hrri'
end;

function TCustomHrirRoomInformation.GetRoomType: string;
begin
 result := FRoomRecord.RoomType;
end;

procedure TCustomHrirRoomInformation.SetRoomType(const Value: string);
begin
 FRoomRecord.RoomType := Value;
end;

procedure TCustomHrirRoomInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FRoomRecord do
  begin
   // read dimensions
   Read(X, SizeOf(Single));
   Read(Y, SizeOf(Single));
   Read(Z, SizeOf(Single));

   // read 'RoomType' string
   Read(StringSize, 1);
   assert(StringSize + 3 * SizeOf(Single) <= FChunkSize);
   SetLength(RoomType, StringSize);
   Read(RoomType[1], StringSize);
  end;
end;

procedure TCustomHrirRoomInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FRoomRecord do
  begin
   // write dimensions
   Write(X, SizeOf(Single));
   Write(Y, SizeOf(Single));
   Write(Z, SizeOf(Single));

   // write 'RoomType' string
   Write(RoomType[0], 1);
   Write(RoomType[1], Integer(RoomType[0]));
  end;
end;

procedure TCustomHrirRoomInformation.SetX(const Value: Single);
begin
 if Value >= 0
  then FRoomRecord.X := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

procedure TCustomHrirRoomInformation.SetY(const Value: Single);
begin
 if Value >= 0
  then FRoomRecord.Y := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

procedure TCustomHrirRoomInformation.SetZ(const Value: Single);
begin
 if Value >= 0
  then FRoomRecord.Z := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

{ TCustomHrirMicrophoneInformation }

constructor TCustomHrirMicrophoneInformation.Create;
begin
 inherited;
 with FMicrophoneRecord do
  begin
   MicType := '';
   Manufacturer := '';
   Notes := '';
  end;
end;

function TCustomHrirMicrophoneInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FMicrophoneRecord
  do result := 3 + Byte(MicType[0]) + Byte(Manufacturer[0]) + Byte(Notes[0]);
end;

class function TCustomHrirMicrophoneInformation.GetClassChunkName: TChunkName;
begin
 result := 'hrmi';
end;

function TCustomHrirMicrophoneInformation.GetString(
  const Index: Integer): string;
begin
 case Index of
  0 : result := FMicrophoneRecord.MicType;
  1 : result := FMicrophoneRecord.Manufacturer;
  2 : result := FMicrophoneRecord.Notes;
 end;
end;

procedure TCustomHrirMicrophoneInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FMicrophoneRecord.MicType := Value;
  1 : FMicrophoneRecord.Manufacturer := Value;
  2 : FMicrophoneRecord.Notes := Value;
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 end;
end;

procedure TCustomHrirMicrophoneInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FMicrophoneRecord do
  begin
   // read 'MicType' string
   Read(StringSize, 1);
   assert(StringSize + 3 <= FChunkSize);
   SetLength(MicType, StringSize);
   Read(MicType[1], StringSize);

   // read 'Manufacturer' string
   Read(StringSize, 1);
   assert(StringSize + 3 + Byte(MicType[0]) <= FChunkSize);
   SetLength(Manufacturer, StringSize);
   Read(Manufacturer[1], StringSize);

   // read 'Notes' string
   Read(StringSize, 1);
   assert(StringSize + 3 + Byte(MicType[0]) + Byte(Manufacturer[0]) <= FChunkSize);
   SetLength(Notes, StringSize);
   Read(Notes[1], StringSize);
  end;
end;

procedure TCustomHrirMicrophoneInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FMicrophoneRecord do
  begin
   // write 'MicType' string
   Write(MicType[0], 1);
   Write(MicType[1], Integer(MicType[0]));

   // write 'Manufacturer' string
   Write(Manufacturer[0], 1);
   Write(Manufacturer[1], Integer(Manufacturer[0]));

   // write 'Notes' string
   Write(Notes[0], 1);
   Write(Notes[1], Integer(Notes[0]));
  end;
end;

{ TCustomHrirOutboardInformation }

constructor TCustomHrirOutboardInformation.Create;
begin
 inherited;
 with FOutboardRecord do
  begin
   ADConverter := '';
   DAConverter := '';
   Amplifier := '';
   Loudspeaker := '';
  end;
end;

function TCustomHrirOutboardInformation.GetChunkSize: Cardinal;
begin
 with FOutboardRecord
  do result := 4 + Byte(ADConverter[0]) + Byte(DAConverter[0]) +
       Byte(Amplifier[0]) + Byte(Loudspeaker[0]);
end;

class function TCustomHrirOutboardInformation.GetClassChunkName: TChunkName;
begin
 result := 'hroi';
end;

function TCustomHrirOutboardInformation.GetString(const Index: Integer): string;
begin
 with FOutboardRecord do
  case Index of
   0 : result := ADConverter;
   1 : result := DAConverter;
   2 : result := Amplifier;
   3 : result := Loudspeaker;
  end;
end;

procedure TCustomHrirOutboardInformation.SetString(const Index: Integer;
  const Value: string);
begin
 with FOutboardRecord do
  case Index of
   0 : ADConverter := Value;
   1 : DAConverter := Value;
   2 : Amplifier := Value;
   3 : Loudspeaker := Value;
   else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
  end;
end;

procedure TCustomHrirOutboardInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 inherited;
 with Stream, FOutboardRecord do
  begin
   // read 'ADConverter' string
   Read(StringSize, 1);
   assert(StringSize + 3 < FChunkSize);
   SetLength(ADConverter, StringSize);
   Read(ADConverter[1], StringSize);

   // read 'DAConverter' string
   Read(StringSize, 1);
   assert(StringSize + 3 + Byte(ADConverter[0]) < FChunkSize);
   SetLength(DAConverter, StringSize);
   Read(DAConverter[1], StringSize);

   // read 'Amplifier' string
   Read(StringSize, 1);
   assert(StringSize + 3 + Byte(ADConverter[0]) + Byte(DAConverter[0]) < FChunkSize);
   SetLength(Amplifier, StringSize);
   Read(Amplifier[1], StringSize);

   // read 'Loudspeaker' string
   Read(StringSize, 1);
   assert(StringSize + 3 + Byte(ADConverter[0]) + Byte(DAConverter[0]) + Byte(Amplifier[0]) < FChunkSize);
   SetLength(Loudspeaker, StringSize);
   Read(Loudspeaker[1], StringSize);
  end;
end;

procedure TCustomHrirOutboardInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 inherited;
 with Stream, FOutboardRecord do
  begin
   // write 'ADConverter' string
   Write(ADConverter[0], 1);
   Write(ADConverter[1], Integer(ADConverter[0]));

   // write 'DAConverter' string
   Write(DAConverter[0], 1);
   Write(DAConverter[1], Integer(DAConverter[0]));

   // write 'Amplifier' string
   Write(Amplifier[0], 1);
   Write(Amplifier[1], Integer(Amplifier[0]));

   // write 'Loudspeaker' string
   Write(Loudspeaker[0], 1);
   Write(Loudspeaker[1], Integer(Loudspeaker[0]));
  end;
end;

{ TCustomHrirMeasurementInformation }

constructor TCustomHrirMeasurementInformation.Create;
begin
 inherited;
 with FMeasurementRecord do
  begin
   Distance := 0;
   MeasurementType := '';
   MeasuredLength := 0;
   ExcitationType := '';
  end;
end;

function TCustomHrirMeasurementInformation.GetChunkSize: Cardinal;
begin
 // calculate chunk size
 with FMeasurementRecord
  do result := 2 + SizeOf(Distance) + Byte(MeasurementType[0]) +
       SizeOf(MeasuredLength) + Byte(ExcitationType[0]);
end;

class function TCustomHrirMeasurementInformation.GetClassChunkName: TChunkName;
begin
 result := 'hrme';
end;

function TCustomHrirMeasurementInformation.GetString(
  const Index: Integer): string;
begin
 case Index of
  0 : result := FMeasurementRecord.MeasurementType;
  1 : result := FMeasurementRecord.ExcitationType;
 end;
end;

procedure TCustomHrirMeasurementInformation.LoadFromStream(Stream: TStream);
var
  StringSize : Byte;
begin
 // load basic chunk information
 inherited LoadFromStream(Stream);

 with Stream, FMeasurementRecord do
  begin
   // read distance
   Read(Distance, SizeOf(Distance));

   // read 'MeasurementType' string
   Read(StringSize, 1);
   assert(StringSize + 2 + SizeOf(Distance) + SizeOf(MeasuredLength) < FChunkSize);
   SetLength(MeasurementType, StringSize);
   Read(MeasurementType[1], StringSize);

   // read measured length
   Read(MeasuredLength, SizeOf(MeasuredLength));

   // read 'ExcitationType' string
   Read(StringSize, 1);
   assert(StringSize + 2 + SizeOf(Distance) + SizeOf(MeasuredLength) +
     Byte(MeasurementType[0]) <= FChunkSize);
   SetLength(ExcitationType, StringSize);
   Read(ExcitationType[1], StringSize);
  end;
end;

procedure TCustomHrirMeasurementInformation.SaveToStream(Stream: TStream);
begin
 // save current chunk size
 FChunkSize := GetChunkSize;

 // store basic chunk information
 inherited SaveToStream(Stream);

 with Stream, FMeasurementRecord do
  begin
   // write distance
   Write(Distance, SizeOf(Distance));

   // write 'MeasurementType' string
   Write(MeasurementType[0], 1);
   Write(MeasurementType[1], Byte(MeasurementType[0]));

   // write measured length
   Write(MeasuredLength, SizeOf(MeasuredLength));

   // write 'ExcitationType' string
   Write(ExcitationType[0], 1);
   Write(ExcitationType[1], Byte(ExcitationType[0]));
  end;
end;

procedure TCustomHrirMeasurementInformation.SetMeasuredLength(
  const Value: Integer);
begin
 if Value > 0
  then FMeasurementRecord.MeasuredLength := Value
  else raise Exception.Create(RCStrPositiveValueOnly);
end;

procedure TCustomHrirMeasurementInformation.SetString(const Index: Integer;
  const Value: string);
begin
 case Index of
  0 : FMeasurementRecord.MeasurementType := Value;
  1 : FMeasurementRecord.ExcitationType := Value;
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
 end;
end;

{ TCustomHrtfs }

constructor TCustomHrtfs.Create;
begin
 inherited;
 RegisterChunkClasses([TCustomHrir, TCustomHrirGeneralInformation,
   TCustomHrirSubjectInformation, TCustomHrirRoomInformation,
   TCustomHrirMicrophoneInformation, TCustomHrirOutboardInformation,
   TCustomHrirMeasurementInformation]);
 FHrirList := TObjectList.Create(False);
 FInterpolationType := itLinear;
end;

destructor TCustomHrtfs.Destroy;
begin
 FreeAndNil(FHrirList);
 inherited;
end;

function TCustomHrtfs.GetChunkSize: Cardinal;
begin
 result := inherited GetChunkSize;
end;

class function TCustomHrtfs.GetClassChunkName: TChunkName;
begin
 result := 'HRTF';
end;

function TCustomHrtfs.GetDate: TDateTime;
begin
 if assigned(FGeneralInformation)
  then result := FGeneralInformation.Date
  else result := Now;
end;

function TCustomHrtfs.GetDistance: Single;
begin
 if assigned(FMeasurementInformation)
  then result := FMeasurementInformation.Distance
  else result := 0;
end;

function TCustomHrtfs.GetGeneralInfoString(const Index: Integer): String;
begin
 if assigned(FGeneralInformation) then
  with FGeneralInformation do
   case index of
    0 : result := Title;
    1 : result := Context;
    2 : result := Copyright;
    3 : result := Author;
    4 : result := Notes; 
    else result := '';
   end
 else result := '';
end;

procedure TCustomHrtfs.GetHrirByIndex(const Index: Integer;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
begin
 with TCustomHrir(FHrirList[Index]) do
  begin
   MoveLeft32(Left, SampleFrames);
   MoveRight32(Right, SampleFrames);
  end;
end;

function TCustomHrtfs.GetHrir(Index: Integer): TCustomHrir;
begin
 if Index in [0..(FHrirList.Count - 1)]
  then result := TCustomHrir(FHrirList[Index])
  else result := nil;
end;

procedure TCustomHrtfs.GetHrirByIndex(const Index: Integer;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
begin
 with TCustomHrir(FHrirList[Index]) do
  begin
   MoveLeft64(Left, SampleFrames);
   MoveRight64(Right, SampleFrames);
  end;
end;

function TCustomHrtfs.GetHrirCount: Integer;
begin
 result := FHrirList.Count;
end;

function TCustomHrtfs.GetMeasuredLength: Integer;
begin
 if assigned(FMeasurementInformation)
  then result := FMeasurementInformation.MeasuredLength
  else result := 0;
end;

function TCustomHrtfs.GetMeasurementString(const Index: Integer): String;
begin
 result := '';
 if assigned(FMeasurementInformation) then
  with FMeasurementInformation do
   case Index of
    0 : result := MeasurementType;
    1 : result := ExcitationType;
   end;
end;

function TCustomHrtfs.GetMicString(const Index: Integer): String;
begin
 result := '';
 if assigned(FMicrophoneInformation) then
  with FMicrophoneInformation do
   case Index of
    0 : result := MicType;
    1 : result := Manufacturer;
    2 : result := Notes;
   end;
end;

function TCustomHrtfs.GetOutboardString(const Index: Integer): String;
begin
 result := '';
 if assigned(FOutboardInformation) then
  with FOutboardInformation do
   case Index of
    0 : result := ADConverter;
    1 : result := DAConverter;
    2 : result := Amplifier;
    3 : result := Loudspeaker;
   end;
end;

function TCustomHrtfs.GetRoomDim(const Index: Integer): Single;
begin
 result := 0;
 if assigned(FRoomInformation) then
  with FRoomInformation do
   case Index of
    0 : result := X;
    1 : result := Y;
    2 : result := Z;
   end;
end;

function TCustomHrtfs.GetRoomType: String;
begin
 if assigned(FRoomInformation)
  then result := FRoomInformation.RoomType
  else result := '';
end;

function TCustomHrtfs.GetSex: THrirSexType;
begin
 if assigned(FSubjectInformation)
  then result := FSubjectInformation.Sex
  else result := stUnknown;
end;

function TCustomHrtfs.GetSubjectString(const Index: Integer): String;
begin
 if assigned(FSubjectInformation) then
  with FSubjectInformation do
   case index of
    0 : result := ID;
    1 : result := Description;
    else result := '';
   end
 else result := '';
end;

procedure TCustomHrtfs.SetDate(const Value: TDateTime);
begin
 if not assigned(FGeneralInformation) then
  begin
   FGeneralInformation := TCustomHrirGeneralInformation.Create;
   AddChunk(FGeneralInformation);
  end;
 FGeneralInformation.Date := Value;
end;

procedure TCustomHrtfs.SetDistance(const Value: Single);
begin
 if not assigned(FMeasurementInformation) then
  begin
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   AddChunk(FMeasurementInformation);
  end;
 FMeasurementInformation.Distance := Value;
end;

procedure TCustomHrtfs.SetGeneralInfoString(const Index: Integer;
  const Value: String);
begin
 if not assigned(FGeneralInformation) then
  begin
   if Value = '' then exit;
   FGeneralInformation := TCustomHrirGeneralInformation.Create;
   AddChunk(FGeneralInformation);
  end;
 if assigned(FGeneralInformation) then
  with FGeneralInformation do
   case index of
    0 : Title := Value;
    1 : Context := Value;
    2 : Copyright := Value;
    3 : Author := Value;
    4 : Notes := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetMeasuredLength(const Value: Integer);
begin
 if not assigned(FMeasurementInformation) then
  begin
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   AddChunk(FMeasurementInformation);
  end;
 FMeasurementInformation.MeasuredLength := Value;
end;

procedure TCustomHrtfs.SetMeasurementString(const Index: Integer;
  const Value: String);
begin
 if not assigned(FMeasurementInformation) then
  begin
   if Value = '' then exit;
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   AddChunk(FMeasurementInformation);
  end;
 if assigned(FMeasurementInformation) then
  with FMeasurementInformation do
   case index of
    0 : MeasurementType := Value;
    1 : ExcitationType := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetMicString(const Index: Integer; const Value: String);
begin
 if not assigned(FMicrophoneInformation) then
  begin
   if Value = '' then exit;
   FMicrophoneInformation := TCustomHrirMicrophoneInformation.Create;
   AddChunk(FMicrophoneInformation);
  end;
 if assigned(FMicrophoneInformation) then
  with FMicrophoneInformation do
   case index of
    0 : MicType := Value;
    1 : Manufacturer := Value;
    2 : Notes := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetOutboardString(const Index: Integer;
  const Value: String);
begin
 if not assigned(FOutboardInformation) then
  begin
   if Value = '' then exit;
   FOutboardInformation := TCustomHrirOutboardInformation.Create;
   AddChunk(FOutboardInformation);
  end;
 if assigned(FOutboardInformation) then
  with FOutboardInformation do
   case index of
    0 : ADConverter := Value;
    1 : DAConverter := Value;
    2 : Amplifier := Value;
    3 : Loudspeaker := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.SetRoomDim(const Index: Integer; const Value: Single);
begin
 if not assigned(FRoomInformation) then
  begin
   if Value = 0 then exit;
   FRoomInformation := TCustomHrirRoomInformation.Create;
   AddChunk(FRoomInformation);
  end;

 with FRoomInformation do
  case Index of
   0 : X := Value;
   1 : Y := Value;
   2 : Z := Value;
   else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
  end;
end;

procedure TCustomHrtfs.SetRoomType(const Value: String);
begin
 if not assigned(FRoomInformation) then
  begin
   if Value = '' then exit;
   FRoomInformation := TCustomHrirRoomInformation.Create;
   AddChunk(FRoomInformation);
  end;
 FRoomInformation.RoomType := Value;
end;

procedure TCustomHrtfs.SetSex(const Value: THrirSexType);
begin
 if not assigned(FSubjectInformation) then
  begin
   if Value = stUnknown then exit;
   FSubjectInformation := TCustomHrirSubjectInformation.Create;
   AddChunk(FSubjectInformation);
  end;
 FSubjectInformation.Sex := Value;
end;

procedure TCustomHrtfs.SetSubjectString(const Index: Integer;
  const Value: String);
begin
 if not assigned(FSubjectInformation) then
  begin
   if Value = '' then exit;
   FSubjectInformation := TCustomHrirSubjectInformation.Create;
   AddChunk(FSubjectInformation);
  end;
 if assigned(FSubjectInformation) then
  with FSubjectInformation do
   case index of
    0 : ID := Value;
    1 : Description := Value;
    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [index]);
   end;
end;

procedure TCustomHrtfs.InterpolateHrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
begin
  case FHrirList.Count of
   0 : raise Exception.Create('No HRIR found!');
   1 : with TCustomHrir(FHrirList[0]) do
        begin
         MoveLeft32(Left, SampleFrames);
         MoveRight32(Right, SampleFrames);
        end;
   2 : Interpolate2Hrir(Azimuth, Polar, SampleFrames, Left, Right);
  else Interpolate3Hrir(Azimuth, Polar, SampleFrames, Left, Right);
 end;
end;

procedure TCustomHrtfs.InterpolateHrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
begin
 case FHrirList.Count of
   0 : raise Exception.Create('No HRIR found!');
   1 : with TCustomHrir(FHrirList[0]) do
        begin
         MoveLeft64(Left, SampleFrames);
         MoveRight64(Right, SampleFrames);
        end;
   2 : Interpolate2Hrir(Azimuth, Polar, SampleFrames, Left, Right);
  else Interpolate3Hrir(Azimuth, Polar, SampleFrames, Left, Right);
 end;
end;

procedure TCustomHrtfs.Interpolate2Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
var
  TempData  : PDavSingleFixedArray;
  Hrirs     : Array [0..1] of TCustomHrir;
  HrirDist  : Double;
  Dist      : Array [0..1] of Single;
  Scale     : Array [0..1] of Single;
  SpherePos : TSphereVector2D;
  Sample    : Integer;
begin
 SpherePos := MakeSphereVector2D(Azimuth, Polar);
 Hrirs[0] := TCustomHrir(FHrirList[0]);
 Hrirs[1] := TCustomHrir(FHrirList[1]);

 // calculate vector distances
 HrirDist := GetOrthodromicDistance(Hrirs[0].Position, Hrirs[1].Position);
 Dist[0]  := GetOrthodromicDistance(Hrirs[0].Position, SpherePos);
 Dist[1]  := GetOrthodromicDistance(Hrirs[1].Position, SpherePos);

 case FInterpolationType of
  itNearest :
   begin
    // select nearest
    if Dist[1] < Dist[0]
     then Hrirs[0] := Hrirs[1];

    // move data nearest
    Hrirs[0].MoveLeft32(Left, SampleFrames);
    Hrirs[0].MoveRight32(Right, SampleFrames);
   end;
  itLinear :
   begin
    // calculate wheighting
    Scale[0] := (sqr(Dist[1]) + sqr(HrirDist) - sqr(Dist[0])) / HrirDist;
    Scale[1] := 1 - Scale[0];

    // allocate a temporary buffer
    ReallocMem(TempData, SampleFrames * SizeOf(Single));

    // linear interpolate left
    Hrirs[0].MoveLeft32(TempData, SampleFrames);
    Hrirs[1].MoveLeft32(Left, SampleFrames);
    for Sample := 0 to SampleFrames - 1
     do Left^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Left^[Sample];

    // linear interpolate right
    Hrirs[0].MoveRight32(TempData, SampleFrames);
    Hrirs[1].MoveRight32(Right, SampleFrames);
    for Sample := 0 to SampleFrames - 1
     do Right^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Right^[Sample];
   end;
 end;

end;

procedure TCustomHrtfs.Interpolate2Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
var
  TempData  : PDavDoubleFixedArray;
  Hrirs     : Array [0..1] of TCustomHrir;
  HrirDist  : Double;
  Dist      : Array [0..1] of Double;
  Scale     : Array [0..1] of Double;
  SpherePos : TSphereVector2D;
  Sample    : Integer;
begin
 SpherePos := MakeSphereVector2D(Azimuth, Polar);
 Hrirs[0] := TCustomHrir(FHrirList[0]);
 Hrirs[1] := TCustomHrir(FHrirList[1]);

 // calculate vector distances
 HrirDist := GetOrthodromicDistance(Hrirs[0].Position, Hrirs[1].Position);
 Dist[0]  := GetOrthodromicDistance(Hrirs[0].Position, SpherePos);
 Dist[1]  := GetOrthodromicDistance(Hrirs[1].Position, SpherePos);

 case FInterpolationType of
  itNearest :
   begin
    // select nearest
    if Dist[1] < Dist[0]
     then Hrirs[0] := Hrirs[1];

    // move data nearest
    Hrirs[0].MoveLeft64(Left, SampleFrames);
    Hrirs[0].MoveRight64(Right, SampleFrames);
   end;
  itLinear :
   begin
    // calculate wheighting
    Scale[0] := (sqr(Dist[1]) + sqr(HrirDist) - sqr(Dist[0])) / HrirDist;
    Scale[1] := 1 - Scale[0];

    // allocate a temporary buffer
    ReallocMem(TempData, SampleFrames * SizeOf(Double));
    try
     // linear interpolate left
     Hrirs[0].MoveLeft64(TempData, SampleFrames);
     Hrirs[1].MoveLeft64(Left, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Left^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Left^[Sample];

     // linear interpolate right
     Hrirs[0].MoveRight64(TempData, SampleFrames);
     Hrirs[1].MoveRight64(Right, SampleFrames);
     for Sample := 0 to SampleFrames - 1
      do Right^[Sample] := Scale[0] * TempData^[Sample] + Scale[1] * Right^[Sample];
    finally
     Dispose(TempData);
    end;
   end;
 end;
end;

procedure TCustomHrtfs.Interpolate3Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavSingleFixedArray);
var
  TempData  : Array [0..1] of PDavSingleFixedArray;
  Hrirs     : Array [0..2] of TCustomHrir;
  HrirPos   : Array [0..2] of TSphereVector2D;
  SpherePos : TSphereVector2D;
  DistA     : Array [0..2] of Double;
  PntA      : Array [0..2] of Double;
  Angles    : Array [0..2] of Double;
  HalfAng   : Array [0..2] of Double;
  Relations : Array [0..2] of Double;
  Scale     : Array [0..2] of Double;
  Sample    : Integer;
begin
 SpherePos := MakeSphereVector2D(Azimuth, Polar);
 FindNearestHrirs(SpherePos, Hrirs[0], Hrirs[1], Hrirs[2]);

 case FInterpolationType of
  itNearest :
   begin
    // move data nearest
    Hrirs[0].MoveLeft32(Left, SampleFrames);
    Hrirs[0].MoveRight32(Right, SampleFrames);
   end;
  itLinear :
   begin
    HrirPos[0] := Hrirs[0].Position;
    HrirPos[1] := Hrirs[1].Position;
    HrirPos[2] := Hrirs[2].Position;

    // check if a single Hrir is hit exactly
    if (SpherePos.Polar = Hrirs[0].Position.Polar) and
       (SpherePos.Azimuth = Hrirs[0].Position.Azimuth) then
     begin
      Hrirs[0].MoveLeft32(Left, SampleFrames);
      Hrirs[0].MoveRight32(Right, SampleFrames);
      Exit;
     end;

    // check only 1D interpolation
    if ((SpherePos.Polar   = Hrirs[0].Position.Polar) and
        (SpherePos.Polar   = Hrirs[1].Position.Polar)) or
       ((SpherePos.Azimuth = Hrirs[0].Position.Azimuth) and
        (SpherePos.Azimuth = Hrirs[1].Position.Azimuth)) then
     begin
      DistA[0] := GetOrthodromicDistance(Hrirs[1].Position, SpherePos);
      DistA[1] := GetOrthodromicDistance(Hrirs[0].Position, Hrirs[1].Position);

      assert(DistA[0] > 0);
      assert(DistA[1] > 0);

      // calculate wheighting
      Scale[0] := DistA[0] / DistA[1];
      Scale[1] := 1 - Scale[0];

      assert(Scale[0] >= 0);
      assert(Scale[1] >= 0);
      assert(Scale[0] <= 1);
      assert(Scale[1] <= 1);

      // allocate a temporary buffer
      GetMem(TempData[0], SampleFrames * SizeOf(Single));
      try
       // linear interpolate left
       Hrirs[0].MoveLeft32(TempData[0], SampleFrames);
       Hrirs[1].MoveLeft32(Left, SampleFrames);
       for Sample := 0 to SampleFrames - 1
        do Left^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Left^[Sample];

       // linear interpolate right
       Hrirs[0].MoveRight32(TempData[0], SampleFrames);
       Hrirs[1].MoveRight32(Right, SampleFrames);
       for Sample := 0 to SampleFrames - 1
        do Right^[Sample] := Scale[0] * TempData[0]^[Sample] + Scale[1] * Right^[Sample];
      finally
       Dispose(TempData[0]);
      end;
      Exit;
     end;

    // calculate orthodromic angle to desired position
    DistA[0] := GetOrthodromicAngle(Hrirs[0].Position, SpherePos);
    DistA[1] := GetOrthodromicAngle(Hrirs[1].Position, SpherePos);
    DistA[2] := GetOrthodromicAngle(Hrirs[2].Position, SpherePos);

    // calculate orthodromic angle between Hrirs
    PntA[0] := GetOrthodromicAngle(Hrirs[1].Position, Hrirs[2].Position);
    PntA[1] := GetOrthodromicAngle(Hrirs[2].Position, Hrirs[0].Position);
    PntA[2] := GetOrthodromicAngle(Hrirs[0].Position, Hrirs[1].Position);

    // calculate triangle angles (using spherical trigonometry)
    Angles[0] := arccos((PntA[0] - PntA[1] * PntA[2]) / (sqrt(1 - sqr(PntA[1])) * sqrt(1 - sqr(PntA[2]))));
    Angles[1] := arccos((PntA[1] - PntA[2] * PntA[0]) / (sqrt(1 - sqr(PntA[2])) * sqrt(1 - sqr(PntA[0]))));
    Angles[2] := arccos((PntA[2] - PntA[0] * PntA[1]) / (sqrt(1 - sqr(PntA[0])) * sqrt(1 - sqr(PntA[1]))));

    // calculate triangle angles between (using spherical trigonometry)
    HalfAng[0] := (DistA[1] - DistA[0] * PntA[2]);
    if HalfAng[0] <> 0
     then HalfAng[0] := arccos(Limit(HalfAng[0] / (sqrt(1 - sqr(DistA[0])) * sqrt(1 - sqr(PntA[2])))));

    HalfAng[1] := (DistA[2] - DistA[1] * PntA[0]);
    if HalfAng[1] <> 0
     then HalfAng[1] := arccos(Limit(HalfAng[1] / (sqrt(1 - sqr(DistA[1])) * sqrt(1 - sqr(PntA[0])))));

    HalfAng[2] := (DistA[0] - DistA[2] * PntA[1]);
    if HalfAng[2] <> 0
     then HalfAng[2] := arccos(Limit(HalfAng[2] / (sqrt(1 - sqr(DistA[2])) * sqrt(1 - sqr(PntA[1])))));

    // calculate relations
    Relations[0] := HalfAng[0] / Angles[0];
    Relations[1] := HalfAng[1] / Angles[1];
    Relations[2] := HalfAng[2] / Angles[2];

    // calculate scale factors
    Scale[0] := (1 - Relations[2]) *  Relations[1];
    Scale[1] := (1 - Relations[0]) *  Relations[2];
    Scale[2] := (1 - Relations[1]) *  Relations[0];

    // allocate a temporary buffer
    GetMem(TempData[0], SampleFrames * SizeOf(Single));
    try
     GetMem(TempData[1], SampleFrames * SizeOf(Single));
     try
      // linear interpolate left
      Hrirs[0].MoveLeft32(TempData[0], SampleFrames);
      Hrirs[1].MoveLeft32(TempData[1], SampleFrames);
      Hrirs[2].MoveLeft32(Left, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Left^[Sample] := Scale[0] * TempData[0]^[Sample] +
                           Scale[1] * TempData[1]^[Sample] +
                           Scale[2] * Left^[Sample];

      // linear interpolate right
      Hrirs[0].MoveRight32(TempData[0], SampleFrames);
      Hrirs[1].MoveRight32(TempData[1], SampleFrames);
      Hrirs[2].MoveRight32(Right, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Right^[Sample] := Scale[0] * TempData[0]^[Sample] +
                            Scale[1] * TempData[1]^[Sample] +
                            Scale[2] * Right^[Sample];
     finally
      Dispose(TempData[1]);
     end;
    finally
     Dispose(TempData[0]);
    end;
   end;
 end;
end;

procedure TCustomHrtfs.Interpolate3Hrir(const Azimuth, Polar: Single;
  const SampleFrames: Integer; const Left, Right: PDavDoubleFixedArray);
var
  TempData  : Array [0..1] of PDavDoubleFixedArray;
  Hrirs     : Array [0..2] of TCustomHrir;
  HrirPos   : Array [0..2] of TSphereVector2D;
  SpherePos : TSphereVector2D;
  DistA     : Array [0..2] of Double;
  PntA      : Array [0..2] of Double;
  Angles    : Array [0..2] of Double;
  HalfAng   : Array [0..2] of Double;
  Relations : Array [0..2] of Double;
  Scale     : Array [0..2] of Double;
  Sample    : Integer;
begin
 SpherePos := MakeSphereVector2D(Azimuth, Polar);
 FindNearestHrirs(SpherePos, Hrirs[0], Hrirs[1], Hrirs[2]);

 case FInterpolationType of
  itNearest :
   begin
    // move data nearest
    Hrirs[0].MoveLeft64(Left, SampleFrames);
    Hrirs[0].MoveRight64(Right, SampleFrames);
   end;
  itLinear :
   begin
    HrirPos[0] := Hrirs[0].Position;
    HrirPos[1] := Hrirs[1].Position;
    HrirPos[2] := Hrirs[2].Position;

    // calculate orthodromic angle to desired position
    DistA[0] := GetOrthodromicAngle(Hrirs[0].Position, SpherePos);
    DistA[1] := GetOrthodromicAngle(Hrirs[1].Position, SpherePos);
    DistA[2] := GetOrthodromicAngle(Hrirs[2].Position, SpherePos);

    // calculate orthodromic angle between Hrirs
    PntA[0] := GetOrthodromicAngle(Hrirs[1].Position, Hrirs[2].Position);
    PntA[1] := GetOrthodromicAngle(Hrirs[2].Position, Hrirs[0].Position);
    PntA[2] := GetOrthodromicAngle(Hrirs[0].Position, Hrirs[1].Position);

    // calculate triangle angles (using spherical trigonometry)
    Angles[0] := arccos((PntA[0] - PntA[1] * PntA[2]) / (sqrt(1 - sqr(PntA[1])) * sqrt(1 - sqr(PntA[2]))));
    Angles[1] := arccos((PntA[1] - PntA[2] * PntA[0]) / (sqrt(1 - sqr(PntA[2])) * sqrt(1 - sqr(PntA[0]))));
    Angles[2] := arccos((PntA[2] - PntA[0] * PntA[1]) / (sqrt(1 - sqr(PntA[0])) * sqrt(1 - sqr(PntA[1]))));

    // calculate triangle angles between (using spherical trigonometry)
    HalfAng[0] := (DistA[1] - DistA[0] * PntA[2]);
    if HalfAng[0] <> 0
     then HalfAng[0] := arccos(Limit(HalfAng[0] / (sqrt(1 - sqr(DistA[0])) * sqrt(1 - sqr(PntA[2])))));

    HalfAng[1] := (DistA[2] - DistA[1] * PntA[0]);
    if HalfAng[1] <> 0
     then HalfAng[1] := arccos(Limit(HalfAng[1] / (sqrt(1 - sqr(DistA[1])) * sqrt(1 - sqr(PntA[0])))));

    HalfAng[2] := (DistA[0] - DistA[2] * PntA[1]);
    if HalfAng[2] <> 0
     then HalfAng[2] := arccos(Limit(HalfAng[2] / (sqrt(1 - sqr(DistA[2])) * sqrt(1 - sqr(PntA[1])))));

    Relations[0] := HalfAng[0] / Angles[0];
    Relations[1] := HalfAng[1] / Angles[1];
    Relations[2] := HalfAng[2] / Angles[2];

    Scale[0] := (1 - Relations[2]) *  Relations[1];
    Scale[1] := (1 - Relations[0]) *  Relations[2];
    Scale[2] := (1 - Relations[1]) *  Relations[0];

    // allocate a temporary buffer
    GetMem(TempData[0], SampleFrames * SizeOf(Double));
    try
     GetMem(TempData[1], SampleFrames * SizeOf(Double));
     try
      // linear interpolate left
      Hrirs[0].MoveLeft64(TempData[0], SampleFrames);
      Hrirs[1].MoveLeft64(TempData[1], SampleFrames);
      Hrirs[2].MoveLeft64(Left, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Left^[Sample] := Scale[0] * TempData[0]^[Sample] +
                           Scale[1] * TempData[1]^[Sample] +
                           Scale[2] * Left^[Sample];

      // linear interpolate right
      Hrirs[0].MoveRight64(TempData[0], SampleFrames);
      Hrirs[1].MoveRight64(TempData[1], SampleFrames);
      Hrirs[2].MoveRight64(Right, SampleFrames);
      for Sample := 0 to SampleFrames - 1
       do Right^[Sample] := Scale[0] * TempData[0]^[Sample] +
                            Scale[1] * TempData[1]^[Sample] +
                            Scale[2] * Right^[Sample];
     finally
      Dispose(TempData[1]);
     end;
    finally
     Dispose(TempData[0]);
    end;
   end;
 end;
end;

procedure TCustomHrtfs.FindNearestHrirs(const SpherePos: TSphereVector2D;
  var A, B, C: TCustomHrir);
var
  i           : Integer;
  CurrentDist : Single;
  TempHrir    : TCustomHrir;
  Distances   : array [0..2] of Single;
begin
 assert(FHrirList.Count > 0);

 // initialize with first three HRIRs
 A := TCustomHrir(FHrirList[0]);
 B := TCustomHrir(FHrirList[1]);
 C := TCustomHrir(FHrirList[2]);

 // initialize with first three distances
 Distances[0] := GetOrthodromicDistance(A.Position, SpherePos);
 Distances[1] := GetOrthodromicDistance(B.Position, SpherePos);
 Distances[2] := GetOrthodromicDistance(C.Position, SpherePos);

 // order distances
 if Distances[1] < Distances[0] then
  begin
   // eventually swap HRIRs
   CurrentDist  := Distances[0];
   Distances[0] := Distances[1];
   Distances[1] := CurrentDist;

   TempHrir := A;
   A := B;
   B := TempHrir;
  end;
 if Distances[2] < Distances[0] then
  begin
   // eventually swap HRIRs
   CurrentDist  := Distances[0];
   Distances[0] := Distances[2];
   Distances[2] := CurrentDist;

   TempHrir := A;
   A := C;
   C := TempHrir;
  end;
 if Distances[2] < Distances[1] then
  begin
   // eventually swap HRIRs
   CurrentDist  := Distances[1];
   Distances[1] := Distances[2];
   Distances[2] := CurrentDist;

   TempHrir := B;
   B := C;
   C := TempHrir;
  end;

 // search for better distances..
 for i := 3 to FHrirList.Count - 1 do
  begin
   TempHrir := TCustomHrir(FHrirList[i]);
   CurrentDist := GetOrthodromicDistance(TempHrir.Position, SpherePos);

   // is first place?
   if CurrentDist < Distances[0] then
    begin
     Distances[2] := Distances[1];
     Distances[1] := Distances[0];
     Distances[0] := CurrentDist;
     C := B; B := A;
     A := TempHrir;
    end else

   // or second place?
   if CurrentDist < Distances[1] then
    begin
     Distances[2] := Distances[1];
     Distances[1] := CurrentDist;
     C := B;
     B := TempHrir;
    end else

   // or third place?
   if CurrentDist < Distances[2] then
    begin
     Distances[2] := CurrentDist;
     C := TempHrir;
    end;
  end;
end;

procedure TCustomHrtfs.SwapChannels;
var
  i : Integer;
begin
 for i := 0 to FHrirList.Count - 1
  do TCustomHrir(FHrirList[i]).SwapChannels;
end;

procedure TCustomHrtfs.AddChunk(Chunk: TCustomChunk);
begin
 inherited;
 if Chunk is TCustomHrir
  then FHrirList.Add(Chunk);
end;

procedure TCustomHrtfs.Clear;
begin
 FHrirList.Clear;
 FChunkList.Clear;
end;

procedure TCustomHrtfs.ClearHrirs;
var
  i : Integer;
begin
 i := 0;
 FHrirList.Clear;
 while i < FChunkList.Count do
  if FChunkList[i] is TCustomHrir
   then FChunkList.Delete(i)
   else inc(i);
end;

procedure TCustomHrtfs.ClearInformationChunks;
begin
 if assigned(FGeneralInformation)     then FreeAndNil(FGeneralInformation);
 if assigned(FSubjectInformation)     then FreeAndNil(FSubjectInformation);
 if assigned(FRoomInformation)        then FreeAndNil(FRoomInformation);
 if assigned(FMicrophoneInformation)  then FreeAndNil(FMicrophoneInformation);
 if assigned(FOutboardInformation)    then FreeAndNil(FOutboardInformation);
 if assigned(FMeasurementInformation) then FreeAndNil(FMeasurementInformation);
end;

procedure TCustomHrtfs.LoadFromStream(Stream: TStream);
begin
 Clear;
 FGeneralInformation     := nil;
 FSubjectInformation     := nil;
 FRoomInformation        := nil;
 FMicrophoneInformation  := nil;
 FOutboardInformation    := nil;
 FMeasurementInformation := nil;
 inherited;
end;

procedure TCustomHrtfs.ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream : TStream);
begin
 if ChunkClass = TCustomHrirGeneralInformation then
  begin
   if assigned(FGeneralInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FGeneralInformation := TCustomHrirGeneralInformation.Create;
   FGeneralInformation.ChunkFlags := ChunkFlags;
   FGeneralInformation.LoadFromStream(Stream);
   AddChunk(FGeneralInformation);
  end else
 if ChunkClass = TCustomHrirSubjectInformation then
  begin
   if assigned(FSubjectInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FSubjectInformation := TCustomHrirSubjectInformation.Create;
   FSubjectInformation.ChunkFlags := ChunkFlags;
   FSubjectInformation.LoadFromStream(Stream);
   AddChunk(FSubjectInformation);
  end else
 if ChunkClass = TCustomHrirRoomInformation then
  begin
   if assigned(FRoomInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FRoomInformation := TCustomHrirRoomInformation.Create;
   FRoomInformation.ChunkFlags := ChunkFlags;
   FRoomInformation.LoadFromStream(Stream);
   AddChunk(FRoomInformation);
  end else
 if ChunkClass = TCustomHrirMicrophoneInformation then
  begin
   if assigned(FMicrophoneInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FMicrophoneInformation := TCustomHrirMicrophoneInformation.Create;
   FMicrophoneInformation.ChunkFlags := ChunkFlags;
   FMicrophoneInformation.LoadFromStream(Stream);
   AddChunk(FMicrophoneInformation);
  end else
 if ChunkClass = TCustomHrirOutboardInformation then
  begin
   if assigned(FOutboardInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FOutboardInformation := TCustomHrirOutboardInformation.Create;
   FOutboardInformation.ChunkFlags := ChunkFlags;
   FOutboardInformation.LoadFromStream(Stream);
   AddChunk(FOutboardInformation);
  end else
 if ChunkClass = TCustomHrirMeasurementInformation then
  begin
   if assigned(FMeasurementInformation)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FMeasurementInformation := TCustomHrirMeasurementInformation.Create;
   FMeasurementInformation.ChunkFlags := ChunkFlags;
   FMeasurementInformation.LoadFromStream(Stream);
   AddChunk(FMeasurementInformation);
  end else inherited;
end;

end.
