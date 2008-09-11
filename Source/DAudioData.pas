unit DAudioData;

interface

{$REGION 'Documentation'}
////////////////////////////////////////////////////////////////////////////////
//
//  TAudioDataCollection
//  +----------------------------------------+
//  |                                        |
//  |  TAudioChannels                        |
//  |  +----------------------------------+  |
//  |  |                                  |  |
//  |  |  TAudioChannel                   |  |
//  |  |  +------------------------+---+  |  |
//  |  |  |                        | 1 |  |  |
//  |  |  |  TAudioChannelData     +---+  |  |
//  |  |  |  +------------------+      |  |  |
//  |  |  |  | actual DATA      |      |  |  |
//  |  |  |  +------------------+      |  |  |
//  |  |  |                            |  |  |
//  |  |  +------------------------+---+  |  |
//  |  |  |                        | 2 |  |  |
//  |  |  |  TAudioChannelData     +---+  |  |
//  |  |  |  +------------------+      |  |  |
//  |  |  |  | actual DATA      |      |  |  |
//  |  |  |  +------------------+      |  |  |
//  |  |  |                            |  |  |
//  |  |  +----------------------------+  |  |
//  |  |                                  |  |
//  |  +----------------------------------+  |
//  |                                        |
//  +----------------------------------------+
//
////////////////////////////////////////////////////////////////////////////////
{$ENDREGION}

uses
  Windows, Classes, DAVDCommon;

type
  {$REGION 'SampleRateSource classes'}
  TCustomSampleRateSource = class(TComponent)
  private
    procedure SetSampleRate(const Value: Double);
  protected
    fSampleRate     : Double;
    fSampleRateReci : Double;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    property SampleRateReciprocal: Double read fSampleRateReci;
    property SampleRate: Double read fSampleRate write SetSampleRate;
  end;

  TSampleRateSource = class(TCustomSampleRateSource)
  published
    property SampleRate;
  end;
  {$ENDREGION}

  {$REGION 'AudioObject classes'}
  TCustomAudioObject = class(TComponent)
  private
    fInternalSampleRateSource : TSampleRateSource;
    function GetSampleRate: Double;
    procedure SetSampleRate(const Value: Double);
    procedure SetSampleRateSource(const Value: TSampleRateSource);
  protected
    fSampleRateSource : TSampleRateSource;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
                                                         
    // properties:
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property SampleRateSource: TSampleRateSource read fSampleRateSource write SetSampleRateSource;
  end;

  TAudioObject = class(TCustomAudioObject)
  published
    property SampleRate;
    property SampleRateSource;
  end;
  {$ENDREGION}

  {$REGION 'AudioData classes'}

  ////////////////////////
  // TAudioData classes //
  ////////////////////////

  TCustomAudioData = class;
  TAudioData32 = class;
  TAudioData64 = class;

  TCustomAudioData = class(TCustomAudioObject)
  private
    fSampleCount           : Cardinal;
    fOnSampleFramesChanged : TNotifyEvent;
    procedure SetSampleCount(const Value: Cardinal);
  protected
    function GetSum: Double; virtual; abstract;
    function GetRMS: Double; virtual; abstract;
    function GetPeak: Double; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleFramesChanged(NewSampleFrames: Int64); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    // some processing functions
    procedure Add(Constant: Double); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure GenerateWhiteNoise(Amplitude: Double); virtual; abstract;
    procedure Mix(AudioData: TCustomAudioData); virtual; abstract;
    procedure Multiply(Factor: Double); virtual; abstract;
    procedure Exponentiate(Exponent: Double); virtual; abstract;
    procedure Rectify; virtual; abstract;
    procedure RemoveDC; virtual; abstract;

    property Sum: Double read GetSum;
    property RMS: Double read GetRMS;
    property Peak: Double read GetPeak;

    property SampleCount: Cardinal read fSampleCount write SetSampleCount;
    property OnSampleFramesChanged: TNotifyEvent read fOnSampleFramesChanged write fOnSampleFramesChanged;
  end;

  TAudioData32 = class(TCustomAudioData)
  private
    fChannelData : PAVDSingleFixedArray;
    function GetChannelData(Sample: Int64): Single;
    procedure SetChannelData(Sample: Int64; const Value: Single);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleFramesChanged(NewSampleFrames: Int64); override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Mix(AudioData: TCustomAudioData); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;

    // data access properties
    property ChannelData[Sample: Int64]: Single read GetChannelData write SetChannelData;
    property ChannelDataPointer: PAVDSingleFixedArray read fChannelData;

    property SampleRate;
    property SampleRateSource;
  end;

  TAudioData64 = class(TCustomAudioData)
  private
    fChannelData  : PAVDDoubleFixedArray;
    function GetChannelData(Sample: Int64): Double;
    procedure SetChannelData(Sample: Int64; const Value: Double);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleFramesChanged(NewSampleFrames: Int64); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Mix(AudioData: TCustomAudioData); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;

    // data access properties
    property ChannelData[Sample: Int64]: Double read GetChannelData write SetChannelData;
    property ChannelDataPointer: PAVDDoubleFixedArray read fChannelData;

    property SampleRate;
    property SampleRateSource;
  end;
  {$ENDREGION}

  {$REGION 'AudioChannel classes'}

  //////////////////////////////////
  // TAudioDataCollection classes //
  //////////////////////////////////

  TCustomAudioDataCollection = class;

  TCustomAudioChannels = class(TOwnedCollection);
  TAudioChannels32 = class(TCustomAudioChannels);
  TAudioChannels64 = class(TCustomAudioChannels);

  TCustomAudioChannel = class(TCollectionItem)
  private
    fDisplayName  : string;
    fChannelsList : TCustomAudioChannels;
    fSampleCount  : Cardinal;
    function GetAudioData: TCustomAudioDataCollection;
  protected
    function GetDisplayName: string; override;
    function GetSum: Double; virtual; abstract;
    function GetRMS: Double; virtual; abstract;
    function GetPeak: Double; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
    procedure SampleFramesChanged; virtual;

    property AudioData: TCustomAudioDataCollection read GetAudioData;
  public
    constructor Create(Collection: TCollection); override;

    // some processing functions
    procedure Add(Constant: Double); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure GenerateWhiteNoise(Amplitude: Double); virtual; abstract;
    procedure Multiply(Factor: Double); virtual; abstract;
    procedure Exponentiate(Exponent: Double); virtual; abstract;
    procedure Rectify; virtual; abstract;
    procedure RemoveDC; virtual; abstract;

    property Sum: Double read GetSum;
    property RMS: Double read GetRMS;
    property Peak: Double read GetPeak;
    property SampleCount: Cardinal read fSampleCount;
  published
    property DisplayName;
  end;

  TAudioChannel32 = class(TCustomAudioChannel)
  private
    fChannelData: TAudioData32;
    function GetChannelData(Sample: Int64): Single;
    procedure SetChannelData(Sample: Int64; const Value: Single);
    function GetChannelDataPointer: PAVDSingleFixedArray;
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleFramesChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;

    // data access properties
    property ChannelData[Sample: Int64]: Single read GetChannelData write SetChannelData;
    property ChannelDataPointer: PAVDSingleFixedArray read GetChannelDataPointer;
  end;

  TAudioChannel64 = class(TCustomAudioChannel)
  private
    fChannelData: TAudioData64;
    function GetChannelData(Sample: Int64): Double;
    procedure SetChannelData(Sample: Int64; const Value: Double);
    function GetChannelDataPointer: PAVDDoubleFixedArray;
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleFramesChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;

    // data access properties
    property ChannelData[Sample: Int64]: Double read GetChannelData write SetChannelData;
    property ChannelDataPointer: PAVDDoubleFixedArray read GetChannelDataPointer;
  end;
  {$ENDREGION}

  {$REGION 'AudioDataCollection classes'}
  TAudioDataCollectionClass = class of TCustomAudioDataCollection;
  TCustomAudioDataCollection = class(TCustomAudioObject)
  private
    fSampleFrames : Cardinal;
    function GetChannelCount: Integer;
    procedure SetChannelCount(const Value: Integer);
    procedure SetSampleFrames(const Value: Cardinal);
  protected
    fChannels : TCustomAudioChannels;
    procedure SampleFramesChanged; virtual;
    procedure CreateChannels; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AChannels: Integer; ASampleFrames: Int64); reintroduce; overload; virtual; abstract;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); virtual;
    procedure Clear; virtual;
    procedure GenerateWhiteNoise(Amplitude: Double); virtual;
    procedure Multiply(Factor: Double); virtual;
    procedure Exponentiate(Exponent: Double); virtual;
    procedure Rectify; virtual;
    procedure RemoveDC; virtual;

    property SampleFrames: Cardinal read fSampleFrames write SetSampleFrames;
    property Channels: TCustomAudioChannels read fChannels write fChannels;
    property ChannelCount: Integer read GetChannelCount write SetChannelCount;
  end;

  TCustomAudioDataCollection32 = class(TCustomAudioDataCollection)
  private
//    fChannelDataPointerList : array of PAVDSingleFixedArray;
    function GetAudioChannel(index: Integer): TAudioChannel32; virtual;
    function GetChannelDataPointerList(Channel: Integer): PAVDSingleFixedArray;
  protected
    procedure CreateChannels; override;
    property ChannelList[index: Integer]: TAudioChannel32 read GetAudioChannel; default;
  public
    constructor Create(AOwner: TComponent; AChannels: Integer; ASampleFrames: Int64); override;
    property ChannelDataPointerList[Channel: Integer]: PAVDSingleFixedArray read GetChannelDataPointerList;
  end;

  TAudioDataCollection32 = class(TCustomAudioDataCollection32)
  published
    property Channels;
    property SampleFrames;
    property SampleRate;
    property SampleRateSource;
  end;

  TCustomAudioDataCollection64 = class(TCustomAudioDataCollection)
  private
//    fChannelDataPointerList : array of PAVDDoubleFixedArray;
    function GetAudioChannel(index: Integer): TAudioChannel64; virtual;
    function GetChannelDataPointerList(Channel: Integer): PAVDDoubleFixedArray;
  protected
    procedure CreateChannels; override;
    property ChannelList[index: Integer]: TAudioChannel64 read GetAudioChannel; default;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AChannels: Integer; ASampleFrames: Int64); overload; override;
    property ChannelDataPointerList[Channel: Integer]: PAVDDoubleFixedArray read GetChannelDataPointerList;
  end;

  TAudioDataCollection64 = class(TCustomAudioDataCollection64)
  published
    property Channels;
    property SampleFrames;
    property SampleRate;
    property SampleRateSource;
  end;
  {$ENDREGION}

procedure Register;

implementation

uses
  SysUtils, Math;

{$REGION 'SampleRateSource implementation'}

{ TCustomSampleRateSource }

procedure TCustomSampleRateSource.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSampleRateSource then
  begin
   TCustomSampleRateSource(Dest).fSampleRate     := fSampleRate;
   TCustomSampleRateSource(Dest).fSampleRateReci := fSampleRateReci;
  end
 else inherited;
end;

constructor TCustomSampleRateSource.Create(AOwner: TComponent);
begin
 inherited;
 fSampleRate := 44100;
end;

procedure TCustomSampleRateSource.SetSampleRate(const Value: Double);
begin
 if fSampleRate <> abs(Value) then
  begin
   if Value = 0
    then raise Exception.Create('value must be larger than 0');
   fSampleRate     := abs(Value);
   fSampleRateReci := 1 / fSampleRate;
  end;
end;
{$ENDREGION}

{$REGION 'AudioObject implementation'}

{ TCustomAudioObject }

procedure TCustomAudioObject.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAudioObject then
  begin
   TCustomAudioObject(Dest).fInternalSampleRateSource := fInternalSampleRateSource;
   TCustomAudioObject(Dest).fSampleRateSource         := fSampleRateSource;
  end
 else inherited;
end;

constructor TCustomAudioObject.Create(AOwner: TComponent);
begin
 inherited;
 fInternalSampleRateSource := TSampleRateSource.Create(Self);
end;

destructor TCustomAudioObject.Destroy;
begin
 // in case the internal sample rate source is really internal, release it
 if fSampleRateSource = nil
  then FreeAndNil(fInternalSampleRateSource);
 inherited;
end;

function TCustomAudioObject.GetSampleRate: Double;
begin
 result := fInternalSampleRateSource.SampleRate;
end;

procedure TCustomAudioObject.SetSampleRate(const Value: Double);
begin
 // only allow writing in case the samplerate source is internal
 if fSampleRateSource = nil
  then fInternalSampleRateSource.SampleRate := Value;
end;

procedure TCustomAudioObject.SetSampleRateSource(const Value: TSampleRateSource);
var
  OldSampleRateSource    : TSampleRateSource;
  OldIntSampleRateSource : TSampleRateSource;
  NewIntSampleRateSource : TSampleRateSource;
begin
 if fSampleRateSource <> Value then
  begin
   // store old samplerate sources
   OldSampleRateSource    := fSampleRateSource;
   OldIntSampleRateSource := fInternalSampleRateSource;

   // set actual sample rate source
   fSampleRateSource      := Value;

   // check whether previously the sample rate source was purely internal
   if not assigned(OldSampleRateSource) then
    begin
     // set new internal sample rate source to the actual sample rate source
     fInternalSampleRateSource := fSampleRateSource;

     // release old purely internal sample rate source
     FreeAndNil(OldIntSampleRateSource);
    end else

   // check whether no external sample rate source is linked
   if not assigned(SampleRateSource) then
    begin
     // create purely internal sample rate source
     NewIntSampleRateSource := TSampleRateSource.Create(Self);

     // assign old sample source properties
     assert(OldSampleRateSource <> nil);
     NewIntSampleRateSource.Assign(OldSampleRateSource);

     // now actually link the new internal sample rate source
     fInternalSampleRateSource := NewIntSampleRateSource;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'AudioData implementation'}
{ TCustomAudioData }

constructor TCustomAudioData.Create(AOwner: TComponent);
begin
 inherited;
 SampleFramesChanged(0);
end;

procedure TCustomAudioData.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomAudioData then
  begin
   TCustomAudioData(Dest).SampleCount  := fSampleCount;
   TCustomAudioData(Dest).fOnSampleFramesChanged := fOnSampleFramesChanged;
  end;
end;

procedure TCustomAudioData.SampleFramesChanged(NewSampleFrames: Int64);
begin
 fSampleCount := NewSampleFrames;
 if assigned(fOnSampleFramesChanged)
  then fOnSampleFramesChanged(Self);
end;

procedure TCustomAudioData.SetSampleCount(const Value: Cardinal);
begin
 if fSampleCount <> Value then
  begin
   SampleFramesChanged(Value);
  end;
end;
{$ENDREGION}

{$REGION 'AudioData implementation'}

{ TAudioData32 }

destructor TAudioData32.Destroy;
begin
 if assigned(fChannelData) then
  begin
   Dispose(fChannelData);
   fChannelData := nil;
  end;
 inherited;
end;

procedure TAudioData32.Add(Constant: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := Constant + fChannelData^[Sample];
end;

procedure TAudioData32.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioData32
  then Move(fChannelData, TAudioData32(Dest).fChannelData, fSampleCount * SizeOf(Single))
  else
 if Dest is TAudioData64
  then ConvertSingleToDouble(PSingle(fChannelData),
                             PDouble(TAudioData64(Dest).fChannelData),
                             fSampleCount);
end;

procedure TAudioData32.Clear;
begin
 FillChar(fChannelData^, SampleCount * SizeOf(Single), 0);
end;

procedure TAudioData32.GenerateWhiteNoise(Amplitude: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := Amplitude * (2 * random - 1);
end;

function TAudioData32.GetChannelData(Sample: Int64): Single;
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then result := fChannelData[Sample]
  else raise Exception.Create('Sample out of range');
end;

function TAudioData32.GetPeak: Double;
var
  Sample       : Integer;
begin
 result := 0;
 if SampleCount = 0 then exit;

 for Sample := 0 to SampleCount - 1 do
  if abs(fChannelData^[Sample]) > result
   then result := abs(fChannelData^[Sample]);
end;

function TAudioData32.GetRMS: Double;
var
  Sample       : Integer;
  SquaredSum   : Double;
begin
 result := 0;
 if SampleCount = 0 then exit;

 SquaredSum := 0;
 for Sample := 0 to SampleCount - 1
  do SquaredSum := SquaredSum + sqr(fChannelData^[Sample]);
 result := sqrt(SquaredSum / SampleCount);
end;

function TAudioData32.GetSum: Double;
var
  Sample : Integer;
begin
 result := 0;
 if SampleCount = 0 then exit;

 for Sample := 0 to SampleCount - 1
  do result := result + fChannelData^[Sample];
end;

procedure TAudioData32.Mix(AudioData: TCustomAudioData);
var
  Sample : Integer;
begin
 if AudioData is TAudioData32 then
  with TAudioData32(AudioData) do
   for Sample := 0 to SampleCount - 1
    do fChannelData^[Sample] := fChannelData^[Sample] + Self.fChannelData^[Sample] else
 if AudioData is TAudioData64 then
  with TAudioData64(AudioData) do
   for Sample := 0 to SampleCount - 1
    do fChannelData^[Sample] := fChannelData^[Sample] + Self.fChannelData^[Sample];
end;

procedure TAudioData32.Multiply(Factor: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := fChannelData^[Sample] * Factor;
end;

procedure TAudioData32.Exponentiate(Exponent: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := sign(fChannelData^[Sample]) * Power(Abs(fChannelData^[Sample]), Exponent);
end;

procedure TAudioData32.Rectify;
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := Abs(fChannelData^[Sample]);
end;

procedure TAudioData32.RemoveDC;
var
  Sample       : Integer;
  DC           : Double;
begin
 if SampleCount = 0 then exit;

 DC := Sum / SampleCount;
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := fChannelData^[Sample] - DC;
end;

procedure TAudioData32.SampleFramesChanged(NewSampleFrames: Int64);
begin
 ReallocMem(fChannelData, NewSampleFrames * SizeOf(Single));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if NewSampleFrames > SampleCount
  then FillChar(fChannelData^[SampleCount], (NewSampleFrames - SampleCount) * SizeOf(Single), 0);

 inherited;
end;

procedure TAudioData32.SetChannelData(Sample: Int64; const Value: Single);
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then fChannelData[Sample] := Value
  else raise Exception.Create('Sample out of range');
end;

{ TAudioData64 }

destructor TAudioData64.Destroy;
begin
 if assigned(fChannelData) then
  begin
   Dispose(fChannelData);
   fChannelData := nil;
  end;
 inherited;
end;

procedure TAudioData64.Add(Constant: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := Constant + fChannelData^[Sample];
end;

procedure TAudioData64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioData64
  then Move(fChannelData, TAudioData64(Dest).fChannelData, fSampleCount * SizeOf(Double))
  else
 if Dest is TAudioData32
  then ConvertDoubleToSingle(PDouble(fChannelData),
                             PSingle(TAudioData32(Dest).fChannelData),
                             fSampleCount);
end;

procedure TAudioData64.Clear;
begin
 FillChar(fChannelData^, SampleCount * SizeOf(Double), 0);
end;

procedure TAudioData64.GenerateWhiteNoise(Amplitude: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := Amplitude * (2 * random - 1);
end;

function TAudioData64.GetChannelData(Sample: Int64): Double;
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then result := fChannelData[Sample]
  else raise Exception.Create('Sample out of range');
end;

function TAudioData64.GetPeak: Double;
var
  Sample       : Integer;
  SampleFrames : Integer;
begin
 result := 0;
 SampleFrames := SampleCount;
 if SampleFrames = 0 then exit;

 for Sample := 0 to SampleFrames - 1 do
  if abs(fChannelData^[Sample]) > result
   then result := abs(fChannelData^[Sample]);
end;

function TAudioData64.GetRMS: Double;
var
  Sample       : Integer;
  SampleFrames : Integer;
  SquaredSum   : Double;
begin
 result := 0;
 SampleFrames := SampleCount;
 if SampleFrames = 0 then exit;

 SquaredSum := 0;
 for Sample := 0 to SampleFrames - 1
  do SquaredSum := SquaredSum + sqr(fChannelData^[Sample]);
 result := sqrt(SquaredSum / SampleFrames);
end;

function TAudioData64.GetSum: Double;
var
  Sample       : Integer;
  SampleFrames : Integer;
begin
 result       := 0;
 SampleFrames := SampleCount;
 if SampleFrames = 0 then exit;

 for Sample := 0 to SampleFrames - 1
  do result := result + fChannelData^[Sample];
end;

procedure TAudioData64.Mix(AudioData: TCustomAudioData);
var
  Sample : Integer;
begin
 if AudioData is TAudioData32 then
  with TAudioData32(AudioData) do
   for Sample := 0 to SampleCount - 1
    do fChannelData^[Sample] := fChannelData^[Sample] + Self.fChannelData^[Sample] else
 if AudioData is TAudioData64 then
  with TAudioData64(AudioData) do
   for Sample := 0 to SampleCount - 1
    do fChannelData^[Sample] := fChannelData^[Sample] + Self.fChannelData^[Sample];
end;

procedure TAudioData64.Multiply(Factor: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := fChannelData^[Sample] * Factor;
end;

procedure TAudioData64.Exponentiate(Exponent: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := sign(fChannelData^[Sample]) * Power(Abs(fChannelData^[Sample]), Exponent);
end;

procedure TAudioData64.Rectify;
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := Abs(fChannelData^[Sample]);
end;

procedure TAudioData64.RemoveDC;
var
  Sample       : Integer;
  SampleFrames : Integer;
  DC           : Double;
begin
 SampleFrames := SampleCount;
 if SampleFrames = 0 then exit;

 DC := Sum / SampleFrames;
 for Sample := 0 to SampleFrames - 1
  do fChannelData^[Sample] := fChannelData^[Sample] - DC;
end;

procedure TAudioData64.SampleFramesChanged(NewSampleFrames: Int64);
begin
 ReallocMem(fChannelData, NewSampleFrames * SizeOf(Double));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if NewSampleFrames > SampleCount
  then FillChar(fChannelData^[SampleCount], (NewSampleFrames - SampleCount) * SizeOf(Double), 0);

 inherited;
end;

procedure TAudioData64.SetChannelData(Sample: Int64; const Value: Double);
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then fChannelData[Sample] := Value
  else raise Exception.Create('Sample out of range');
end;
{$ENDREGION}

{$REGION 'AudioChannel implementation'}

{ TCustomAudioChannel }

constructor TCustomAudioChannel.Create(Collection: TCollection);
begin
 inherited;
 fDisplayName := 'Channel ' + IntToStr(Collection.Count);
 SampleFramesChanged;
end;

procedure TCustomAudioChannel.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAudioChannel then
  begin
   TCustomAudioChannel(Dest).fDisplayName  := fDisplayName;
   fChannelsList.AssignTo(TCustomAudioChannel(Dest).fChannelsList);
  end
 else inherited;
end;

function TCustomAudioChannel.GetAudioData: TCustomAudioDataCollection;
begin
 assert(Collection is TCustomAudioChannels);
 assert(TCustomAudioChannels(Collection).Owner is TCustomAudioDataCollection);
 result := TCustomAudioDataCollection(TCustomAudioChannels(GetOwner).GetOwner);
end;

function TCustomAudioChannel.GetDisplayName: string;
begin
 result := fDisplayName;
end;

procedure TCustomAudioChannel.SampleFramesChanged;
begin
 fSampleCount := AudioData.SampleFrames;
end;

procedure TCustomAudioChannel.SetDisplayName(const Value: string);
begin
 fDisplayName := Value;
 inherited;
end;

{ TAudioChannel32 }

constructor TAudioChannel32.Create(Collection: TCollection);
begin
 inherited;
 fChannelData := TAudioData32.Create(AudioData);
 SampleFramesChanged;
end;

destructor TAudioChannel32.Destroy;
begin
 FreeAndNil(fChannelData);
 inherited;
end;

{$REGION 'TAudioChannel32 Wrapper'}
procedure TAudioChannel32.Add(Constant: Double);
begin
 fChannelData.Add(Constant);
end;

procedure TAudioChannel32.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioChannel32
  then fChannelData.AssignTo(TAudioChannel32(Dest).fChannelData) else
 if Dest is TAudioChannel64
  then fChannelData.AssignTo(TAudioChannel64(Dest).fChannelData);
end;

procedure TAudioChannel32.Clear;
begin
 fChannelData.Clear;
end;

procedure TAudioChannel32.GenerateWhiteNoise(Amplitude: Double);
begin
 fChannelData.GenerateWhiteNoise(Amplitude);
end;

function TAudioChannel32.GetChannelData(Sample: Int64): Single;
begin
 result := fChannelData.ChannelData[Sample];
end;

function TAudioChannel32.GetChannelDataPointer: PAVDSingleFixedArray;
begin
 result := fChannelData.ChannelDataPointer;
end;

function TAudioChannel32.GetPeak: Double;
begin
 result := fChannelData.GetPeak;
end;

function TAudioChannel32.GetRMS: Double;
begin
 result := fChannelData.GetRMS;
end;

function TAudioChannel32.GetSum: Double;
begin
 result := fChannelData.GetSum;
end;

procedure TAudioChannel32.Multiply(Factor: Double);
begin
 fChannelData.Multiply(Factor);
end;

procedure TAudioChannel32.Exponentiate(Exponent: Double);
begin
 fChannelData.Exponentiate(Exponent);
end;

procedure TAudioChannel32.Rectify;
begin
 fChannelData.Rectify;
end;

procedure TAudioChannel32.RemoveDC;
begin
 fChannelData.RemoveDC;
end;
{$ENDREGION}

procedure TAudioChannel32.SampleFramesChanged;
begin
 inherited;
 if assigned(fChannelData)
  then fChannelData.SampleCount := fSampleCount;
end;

procedure TAudioChannel32.SetChannelData(Sample: Int64; const Value: Single);
begin
 fChannelData.ChannelData[Sample] := Value;
end;

{ TAudioChannel64 }

constructor TAudioChannel64.Create(Collection: TCollection);
begin
 inherited;
 fChannelData := TAudioData64.Create(AudioData);
 SampleFramesChanged;
end;

destructor TAudioChannel64.Destroy;
begin
 FreeAndNil(fChannelData);
 inherited;
end;

{$REGION 'TAudioChannel64 Wrapper'}
procedure TAudioChannel64.Add(Constant: Double);
begin
 fChannelData.Add(Constant);
end;

procedure TAudioChannel64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioChannel64
  then fChannelData.AssignTo(TAudioChannel64(Dest).fChannelData) else
 if Dest is TAudioChannel32
  then fChannelData.AssignTo(TAudioChannel32(Dest).fChannelData);
end;

procedure TAudioChannel64.Clear;
begin
 fChannelData.Clear;
end;

procedure TAudioChannel64.GenerateWhiteNoise(Amplitude: Double);
begin
 fChannelData.GenerateWhiteNoise(Amplitude);
end;

function TAudioChannel64.GetChannelData(Sample: Int64): Double;
begin
 result := fChannelData.ChannelData[Sample];
end;

function TAudioChannel64.GetChannelDataPointer: PAVDDoubleFixedArray;
begin
 result := fChannelData.ChannelDataPointer;
end;

function TAudioChannel64.GetPeak: Double;
begin
 result := fChannelData.Peak;
end;

function TAudioChannel64.GetRMS: Double;
begin
 result := fChannelData.RMS;
end;

function TAudioChannel64.GetSum: Double;
begin
 result := fChannelData.Sum;
end;

procedure TAudioChannel64.Multiply(Factor: Double);
begin
 fChannelData.Multiply(Factor);
end;

procedure TAudioChannel64.Exponentiate(Exponent: Double);
begin
 fChannelData.Exponentiate(Exponent);
end;

procedure TAudioChannel64.Rectify;
begin
 fChannelData.Rectify;
end;

procedure TAudioChannel64.RemoveDC;
begin
 fChannelData.RemoveDC;
end;
{$ENDREGION}

procedure TAudioChannel64.SampleFramesChanged;
begin
 inherited;
 if assigned(fChannelData)
  then fChannelData.SampleCount := fSampleCount;
end;

procedure TAudioChannel64.SetChannelData(Sample: Int64; const Value: Double);
begin
 fChannelData.ChannelData[Sample] := Value;
end;
{$ENDREGION}

{$REGION 'AudioDataCollection implementation'}
{ TCustomAudioDataCollection }

constructor TCustomAudioDataCollection.Create(AOwner: TComponent);
begin
 inherited;
 CreateChannels;
end;

destructor TCustomAudioDataCollection.Destroy;
begin
 if assigned(fChannels) then FreeAndNil(fChannels);
 inherited;
end;

procedure TCustomAudioDataCollection.Add(Constant: Double);
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1
  do TCustomAudioChannel(fChannels.Items[ch]).Add(Constant);
end;

procedure TCustomAudioDataCollection.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomAudioDataCollection then
  begin
   TCustomAudioDataCollection(Dest).fSampleFrames := fSampleFrames;
   fChannels.AssignTo(TCustomAudioDataCollection(Dest).fChannels);
  end;
end;

procedure TCustomAudioDataCollection.Clear;
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1
  do TCustomAudioChannel(fChannels.Items[ch]).Clear;
end;

procedure TCustomAudioDataCollection.Exponentiate(Exponent: Double);
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1
  do TCustomAudioChannel(fChannels.Items[ch]).Exponentiate(Exponent);
end;

procedure TCustomAudioDataCollection.GenerateWhiteNoise(Amplitude: Double);
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1
  do TCustomAudioChannel(fChannels.Items[ch]).GenerateWhiteNoise(Amplitude);
end;

procedure TCustomAudioDataCollection.Multiply(Factor: Double);
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1
  do TCustomAudioChannel(fChannels.Items[ch]).Multiply(Factor);
end;

procedure TCustomAudioDataCollection.Rectify;
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1
  do TCustomAudioChannel(fChannels.Items[ch]).Rectify;
end;

procedure TCustomAudioDataCollection.RemoveDC;
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1
  do TCustomAudioChannel(fChannels.Items[ch]).RemoveDC;
end;

function TCustomAudioDataCollection.GetChannelCount: Integer;
begin
 if assigned(fChannels)
  then result := fChannels.Count
  else result := 0;
end;

procedure TCustomAudioDataCollection.SampleFramesChanged;
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1 do
  begin
   assert(fChannels.Items[ch] is TCustomAudioChannel);
   if TCustomAudioChannel(fChannels.Items[ch]).SampleCount <> fSampleFrames
    then TCustomAudioChannel(fChannels.Items[ch]).SampleFramesChanged;
  end;
end;

procedure TCustomAudioDataCollection.SetChannelCount(const Value: Integer);
begin
 // delete or add channels until the count matches the desired channel count
 while Channels.Count > Value do Channels.Delete(Channels.Count - 1);
 while Channels.Count < Value do Channels.Add;
end;

procedure TCustomAudioDataCollection.SetSampleFrames(const Value: Cardinal);
begin
 if fSampleFrames <> Value then
  begin
   fSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

{ TCustomAudioDataCollection32 }

constructor TCustomAudioDataCollection32.Create(AOwner: TComponent;
  AChannels: Integer; ASampleFrames: Int64);
begin
 inherited Create(AOwner);
 ChannelCount := AChannels;
 SampleFrames := ASampleFrames;
end;

procedure TCustomAudioDataCollection32.CreateChannels;
begin
 fChannels := TCustomAudioChannels.Create(Self, TAudioChannel32);
end;

function TCustomAudioDataCollection32.GetAudioChannel(index: Integer): TAudioChannel32;
begin
 if (Index < 0) or (Index >= fChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TAudioChannel32(fChannels.Items[index]);
end;

function TCustomAudioDataCollection32.GetChannelDataPointerList(
  Channel: Integer): PAVDSingleFixedArray;
begin
 result := ChannelList[Channel].ChannelDataPointer;
end;

{ TCustomAudioDataCollection64 }

constructor TCustomAudioDataCollection64.Create(AOwner: TComponent);
begin
 inherited;
 fChannels := TCustomAudioChannels.Create(Self, TAudioChannel64);
end;

constructor TCustomAudioDataCollection64.Create(AOwner: TComponent;
  AChannels: Integer; ASampleFrames: Int64);
begin
 Create(AOwner);
 ChannelCount := AChannels;
 SampleFrames := ASampleFrames;
end;

procedure TCustomAudioDataCollection64.CreateChannels;
begin
 fChannels := TCustomAudioChannels.Create(Self, TAudioChannel64);
end;

function TCustomAudioDataCollection64.GetAudioChannel(index: Integer): TAudioChannel64;
begin
 if (Index < 0) or (Index >= fChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TAudioChannel64(fChannels.Items[index]);
end;

function TCustomAudioDataCollection64.GetChannelDataPointerList(
  Channel: Integer): PAVDDoubleFixedArray;
begin
 result := ChannelList[Channel].ChannelDataPointer;
end;
{$ENDREGION}

////////////////////////////////////////////////////////////////////////////////

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TSampleRateSource]);
  RegisterComponents('ASIO/VST Basics', [TAudioData32]);
  RegisterComponents('ASIO/VST Basics', [TAudioData64]);
  RegisterComponents('ASIO/VST Basics', [TAudioDataCollection32]);
  RegisterComponents('ASIO/VST Basics', [TAudioDataCollection64]);
end;

end.
