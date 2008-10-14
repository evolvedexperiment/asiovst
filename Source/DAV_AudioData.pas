unit DAV_AudioData;

interface

{$I ASIOVST.INC}
{$IFDEF DELPHI10_UP} {$region 'Documentation'} {$ENDIF}
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
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

uses
  Windows, Classes, DAV_Common;

type
  {$IFDEF DELPHI10_UP} {$region 'SampleRateSource classes'} {$ENDIF}
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
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'AudioObject classes'} {$ENDIF}
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
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'AudioData classes'} {$ENDIF}

  ////////////////////////
  // TAudioData classes //
  ////////////////////////

  TCustomAudioData = class;
  TAudioData32 = class;
  TAudioData64 = class;

  TCustomAudioData = class(TCustomAudioObject)
  private
    FSampleCount           : Cardinal;
    FExternalData          : Boolean;
    FOnSampleFramesChanged : TNotifyEvent;
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
    procedure Mix(AudioDataCollection: TCustomAudioData); virtual; abstract;
    procedure Multiply(Factor: Double); virtual; abstract;
    procedure Exponentiate(Exponent: Double); virtual; abstract;
    procedure Rectify; virtual; abstract;
    procedure RemoveDC; virtual; abstract;

    property Sum: Double read GetSum;
    property RMS: Double read GetRMS;
    property Peak: Double read GetPeak;

    property ExternalData: Boolean read FExternalData;
    property SampleCount: Cardinal read FSampleCount write SetSampleCount;
    property OnSampleFramesChanged: TNotifyEvent read FOnSampleFramesChanged write FOnSampleFramesChanged;
  end;

  TAudioData32 = class(TCustomAudioData)
  private
    FChannelData : PDAVSingleFixedArray;
    function GetChannelData(Sample: Int64): Single;
    procedure SetChannelData(Sample: Int64; const Value: Single);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleFramesChanged(NewSampleFrames: Int64); override;
  public
    constructor Create(AOwner: TComponent; DataPtr: PDAVSingleFixedArray = nil); reintroduce; virtual;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Mix(AudioDataCollection: TCustomAudioData); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;

    // data access properties
    property ChannelData[Sample: Int64]: Single read GetChannelData write SetChannelData;
    property ChannelDataPointer: PDAVSingleFixedArray read FChannelData;

    property SampleRate;
    property SampleRateSource;
  end;

  TAudioData64 = class(TCustomAudioData)
  private
    FChannelData  : PDAVDoubleFixedArray;
    function GetChannelData(Sample: Int64): Double;
    procedure SetChannelData(Sample: Int64; const Value: Double);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleFramesChanged(NewSampleFrames: Int64); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent; DataPtr: PDAVDoubleFixedArray = nil); reintroduce; virtual;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); override;
    procedure Clear; override;
    procedure GenerateWhiteNoise(Amplitude: Double); override;
    procedure Mix(AudioDataCollection: TCustomAudioData); override;
    procedure Multiply(Factor: Double); override;
    procedure Exponentiate(Exponent: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;

    // data access properties
    property ChannelData[Sample: Int64]: Double read GetChannelData write SetChannelData;
    property ChannelDataPointer: PDAVDoubleFixedArray read FChannelData;

    property SampleRate;
    property SampleRateSource;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'AudioChannel classes'} {$ENDIF}

  //////////////////////////////////
  // TAudioDataCollection classes //
  //////////////////////////////////

  TCustomAudioDataCollection = class;

  TCustomAudioChannels = class(TOwnedCollection)
  private
    FOnChanged: TNotifyEvent;
  protected
    {$IFDEF Delphi6_Up}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$ELSE}
    procedure Update(Item: TCollectionItem); override;
    {$ENDIF}
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TAudioChannels32 = class(TCustomAudioChannels);
  TAudioChannels64 = class(TCustomAudioChannels);

  TCustomAudioChannel = class(TCollectionItem)
  private
    FDisplayName  : string;
    FChannelsList : TCustomAudioChannels;
    FSampleCount  : Cardinal;
    function GetAudioDataCollection: TCustomAudioDataCollection;
  protected
    function GetDisplayName: string; override;
    function GetSum: Double; virtual; abstract;
    function GetRMS: Double; virtual; abstract;
    function GetPeak: Double; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
    procedure SampleFramesChanged; virtual;

    property AudioDataCollection: TCustomAudioDataCollection read GetAudioDataCollection;
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
    property SampleCount: Cardinal read FSampleCount;
  published
    property DisplayName;
  end;

  TAudioChannel32 = class(TCustomAudioChannel)
  private
    FChannelData: TAudioData32;
    function GetChannelData(Sample: Int64): Single;
    procedure SetChannelData(Sample: Int64; const Value: Single);
    function GetChannelDataPointer: PDAVSingleFixedArray;
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
    property ChannelDataPointer: PDAVSingleFixedArray read GetChannelDataPointer;
  end;

  TAudioChannel64 = class(TCustomAudioChannel)
  private
    FChannelData: TAudioData64;
    function GetChannelData(Sample: Int64): Double;
    procedure SetChannelData(Sample: Int64; const Value: Double);
    function GetChannelDataPointer: PDAVDoubleFixedArray;
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
    property ChannelDataPointer: PDAVDoubleFixedArray read GetChannelDataPointer;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'AudioDataCollection classes'} {$ENDIF}
  TAudioDataCollectionClass = class of TCustomAudioDataCollection;
  TCustomAudioDataCollection = class(TCustomAudioObject)
  private
    FSampleFrames : Cardinal;
    FExternalData : Boolean;
    function GetChannelCount: Integer;
    procedure SetChannelCount(const Value: Integer);
    procedure SetSampleFrames(const Value: Cardinal);
  protected
    FChannels : TCustomAudioChannels;
    procedure SampleFramesChanged; virtual;
    procedure CreateChannels; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    property ExternalData: Boolean read FExternalData;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AChannels: Integer; ASampleFrames: Int64; DataPtr: Pointer = nil); reintroduce; overload; virtual; abstract;
    destructor Destroy; override;

    // some processing functions
    procedure Add(Constant: Double); virtual;
    procedure Clear; virtual;
    procedure GenerateWhiteNoise(Amplitude: Double); virtual;
    procedure Multiply(Factor: Double); virtual;
    procedure Exponentiate(Exponent: Double); virtual;
    procedure Rectify; virtual;
    procedure RemoveDC; virtual;

    property SampleFrames: Cardinal read FSampleFrames write SetSampleFrames;
    property Channels: TCustomAudioChannels read FChannels write FChannels;
    property ChannelCount: Integer read GetChannelCount write SetChannelCount;
  end;

  TCustomAudioDataCollection32 = class(TCustomAudioDataCollection)
  private
    FChannelDataPointerList : array of PDAVSingleFixedArray;
    function GetAudioChannel(index: Integer): TAudioChannel32; virtual;
    function GetChannelDataPointerList(Channel: Integer): PDAVSingleFixedArray;
    procedure RebuildChannelList(Sender: TObject);
  protected
    procedure CreateChannels; override;
    property ChannelList[index: Integer]: TAudioChannel32 read GetAudioChannel; default;
  public
    constructor Create(AOwner: TComponent; AChannels: Integer; ASampleFrames: Int64; DataPtr: Pointer = nil); override;
    property ChannelDataPointerList[Channel: Integer]: PDAVSingleFixedArray read GetChannelDataPointerList;
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
    FChannelDataPointerList : array of PDAVDoubleFixedArray;
    function GetAudioChannel(index: Integer): TAudioChannel64; virtual;
    function GetChannelDataPointerList(Channel: Integer): PDAVDoubleFixedArray;
    procedure RebuildChannelList(Sender: TObject);
  protected
    procedure CreateChannels; override;
    property ChannelList[index: Integer]: TAudioChannel64 read GetAudioChannel; default;
  public
    constructor Create(AOwner: TComponent; AChannels: Integer; ASampleFrames: Int64; DataPtr: Pointer = nil); override;
    property ChannelDataPointerList[Channel: Integer]: PDAVDoubleFixedArray read GetChannelDataPointerList;
  end;

  TAudioDataCollection64 = class(TCustomAudioDataCollection64)
  published
    property Channels;
    property SampleFrames;
    property SampleRate;
    property SampleRateSource;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

procedure Register;

implementation

uses
  SysUtils, Math;

{$IFDEF DELPHI10_UP} {$region 'SampleRateSource implementation'} {$ENDIF}

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
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'AudioObject implementation'} {$ENDIF}

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
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'AudioData implementation'} {$ENDIF}
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
   TCustomAudioData(Dest).SampleCount  := FSampleCount;
   TCustomAudioData(Dest).FOnSampleFramesChanged := FOnSampleFramesChanged;
  end;
end;

procedure TCustomAudioData.SampleFramesChanged(NewSampleFrames: Int64);
begin
 if not FExternalData then
  begin
   FSampleCount := NewSampleFrames;
   if assigned(FOnSampleFramesChanged)
    then FOnSampleFramesChanged(Self);
  end;
end;

procedure TCustomAudioData.SetSampleCount(const Value: Cardinal);
begin
 if (FSampleCount <> Value) and not FExternalData then
  begin
   SampleFramesChanged(Value);
  end;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'AudioData implementation'} {$ENDIF}

{ TAudioData32 }

constructor TAudioData32.Create(AOwner: TComponent;
  DataPtr: PDAVSingleFixedArray = nil);
begin
 FExternalData := DataPtr <> nil;
 if FExternalData
  then FChannelData := DataPtr;
 inherited Create(AOwner);
end;

destructor TAudioData32.Destroy;
begin
 if assigned(FChannelData) then
  begin
   if not FExternalData
    then Dispose(FChannelData);
   FChannelData := nil;
  end;
 inherited;
end;

procedure TAudioData32.Add(Constant: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := Constant + FChannelData^[Sample];
end;

procedure TAudioData32.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioData32
  then Move(FChannelData, TAudioData32(Dest).FChannelData, FSampleCount * SizeOf(Single))
  else
 if Dest is TAudioData64
  then ConvertSingleToDouble(PSingle(FChannelData),
                             PDouble(TAudioData64(Dest).FChannelData),
                             FSampleCount);
end;

procedure TAudioData32.Clear;
begin
 FillChar(FChannelData^, SampleCount * SizeOf(Single), 0);
end;

procedure TAudioData32.GenerateWhiteNoise(Amplitude: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := Amplitude * (2 * random - 1);
end;

function TAudioData32.GetChannelData(Sample: Int64): Single;
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then result := FChannelData[Sample]
  else raise Exception.Create('Sample out of range');
end;

function TAudioData32.GetPeak: Double;
var
  Sample       : Integer;
begin
 result := 0;
 if SampleCount = 0 then exit;

 for Sample := 0 to SampleCount - 1 do
  if abs(FChannelData^[Sample]) > result
   then result := abs(FChannelData^[Sample]);
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
  do SquaredSum := SquaredSum + sqr(FChannelData^[Sample]);
 result := sqrt(SquaredSum / SampleCount);
end;

function TAudioData32.GetSum: Double;
var
  Sample : Integer;
begin
 result := 0;
 if SampleCount = 0 then exit;

 for Sample := 0 to SampleCount - 1
  do result := result + FChannelData^[Sample];
end;

procedure TAudioData32.Mix(AudioDataCollection: TCustomAudioData);
var
  Sample : Integer;
begin
 if AudioDataCollection is TAudioData32 then
  with TAudioData32(AudioDataCollection) do
   for Sample := 0 to min(SampleCount, Self.SampleCount) - 1
    do FChannelData^[Sample] := FChannelData^[Sample] + Self.FChannelData^[Sample] else
 if AudioDataCollection is TAudioData64 then
  with TAudioData64(AudioDataCollection) do
   for Sample := 0 to min(SampleCount, Self.SampleCount) - 1
    do FChannelData^[Sample] := FChannelData^[Sample] + Self.FChannelData^[Sample];
end;

procedure TAudioData32.Multiply(Factor: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := FChannelData^[Sample] * Factor;
end;

procedure TAudioData32.Exponentiate(Exponent: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := sign(FChannelData^[Sample]) * Power(Abs(FChannelData^[Sample]), Exponent);
end;

procedure TAudioData32.Rectify;
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := Abs(FChannelData^[Sample]);
end;

procedure TAudioData32.RemoveDC;
var
  Sample       : Integer;
  DC           : Double;
begin
 if SampleCount = 0 then exit;

 DC := Sum / SampleCount;
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := FChannelData^[Sample] - DC;
end;

procedure TAudioData32.SampleFramesChanged(NewSampleFrames: Int64);
begin
 if ExternalData then
  begin
   ReallocMem(FChannelData, NewSampleFrames * SizeOf(Single));

   // check if new length is longer than the old length and fill with zeroes if necessary
   if NewSampleFrames > SampleCount
    then FillChar(FChannelData^[SampleCount], (NewSampleFrames - SampleCount) * SizeOf(Single), 0);
  end;
 inherited;
end;

procedure TAudioData32.SetChannelData(Sample: Int64; const Value: Single);
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then FChannelData[Sample] := Value
  else raise Exception.Create('Sample out of range');
end;

{ TAudioData64 }

constructor TAudioData64.Create(AOwner: TComponent;
  DataPtr: PDAVDoubleFixedArray = nil);
begin
 FExternalData := DataPtr <> nil;
 if FExternalData
  then FChannelData := DataPtr;
 inherited Create(AOwner);
end;

destructor TAudioData64.Destroy;
begin
 if assigned(FChannelData) then
  begin
   if not FExternalData
    then Dispose(FChannelData);
   FChannelData := nil;
  end;
 inherited;
end;

procedure TAudioData64.Add(Constant: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := Constant + FChannelData^[Sample];
end;

procedure TAudioData64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioData64
  then Move(FChannelData, TAudioData64(Dest).FChannelData, FSampleCount * SizeOf(Double))
  else
 if Dest is TAudioData32
  then ConvertDoubleToSingle(PDouble(FChannelData),
                             PSingle(TAudioData32(Dest).FChannelData),
                             FSampleCount);
end;

procedure TAudioData64.Clear;
begin
 FillChar(FChannelData^, SampleCount * SizeOf(Double), 0);
end;

procedure TAudioData64.GenerateWhiteNoise(Amplitude: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := Amplitude * (2 * random - 1);
end;

function TAudioData64.GetChannelData(Sample: Int64): Double;
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then result := FChannelData[Sample]
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
  if abs(FChannelData^[Sample]) > result
   then result := abs(FChannelData^[Sample]);
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
  do SquaredSum := SquaredSum + sqr(FChannelData^[Sample]);
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
  do result := result + FChannelData^[Sample];
end;

procedure TAudioData64.Mix(AudioDataCollection: TCustomAudioData);
var
  Sample : Integer;
begin
 if AudioDataCollection is TAudioData32 then
  with TAudioData32(AudioDataCollection) do
   for Sample := 0 to SampleCount - 1
    do FChannelData^[Sample] := FChannelData^[Sample] + Self.FChannelData^[Sample] else
 if AudioDataCollection is TAudioData64 then
  with TAudioData64(AudioDataCollection) do
   for Sample := 0 to SampleCount - 1
    do FChannelData^[Sample] := FChannelData^[Sample] + Self.FChannelData^[Sample];
end;

procedure TAudioData64.Multiply(Factor: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := FChannelData^[Sample] * Factor;
end;

procedure TAudioData64.Exponentiate(Exponent: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := sign(FChannelData^[Sample]) * Power(Abs(FChannelData^[Sample]), Exponent);
end;

procedure TAudioData64.Rectify;
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do FChannelData^[Sample] := Abs(FChannelData^[Sample]);
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
  do FChannelData^[Sample] := FChannelData^[Sample] - DC;
end;

procedure TAudioData64.SampleFramesChanged(NewSampleFrames: Int64);
begin
 if ExternalData then
  begin
   ReallocMem(FChannelData, NewSampleFrames * SizeOf(Double));

   // check if new length is longer than the old length and fill with zeroes if necessary
   if NewSampleFrames > SampleCount
    then FillChar(FChannelData^[SampleCount], (NewSampleFrames - SampleCount) * SizeOf(Double), 0);
  end;
 inherited;
end;

procedure TAudioData64.SetChannelData(Sample: Int64; const Value: Double);
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then FChannelData[Sample] := Value
  else raise Exception.Create('Sample out of range');
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'AudioChannel implementation'} {$ENDIF}

{ TCustomAudioChannels }

{$IFDEF Delphi6_Up}
procedure TCustomAudioChannels.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
{$ELSE}
procedure TCustomAudioChannels.Update(Item: TCollectionItem);
{$ENDIF}
begin
 inherited;
 if assigned(OnChanged)
  then OnChanged(Self);
end;

{ TCustomAudioChannel }

constructor TCustomAudioChannel.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName := 'Channel ' + IntToStr(Collection.Count);
 SampleFramesChanged;
end;

procedure TCustomAudioChannel.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAudioChannel then
  begin
   TCustomAudioChannel(Dest).FDisplayName  := FDisplayName;
   FChannelsList.AssignTo(TCustomAudioChannel(Dest).FChannelsList);
  end
 else inherited;
end;

function TCustomAudioChannel.GetAudioDataCollection: TCustomAudioDataCollection;
begin
 assert(Collection is TCustomAudioChannels);
 assert(TCustomAudioChannels(Collection).GetOwner is TCustomAudioDataCollection);
 result := TCustomAudioDataCollection(TCustomAudioChannels(GetOwner).GetOwner);
end;

function TCustomAudioChannel.GetDisplayName: string;
begin
 result := FDisplayName;
end;

procedure TCustomAudioChannel.SampleFramesChanged;
begin
 FSampleCount := AudioDataCollection.SampleFrames;
end;

procedure TCustomAudioChannel.SetDisplayName(const Value: string);
begin
 FDisplayName := Value;
 inherited;
end;

{ TAudioChannel32 }

constructor TAudioChannel32.Create(Collection: TCollection);
begin
 inherited;
 if AudioDataCollection.ExternalData then
  begin
   assert(AudioDataCollection is TCustomAudioDataCollection32);
   with TCustomAudioDataCollection32(AudioDataCollection)
    do FChannelData := TAudioData32.Create(AudioDataCollection, FChannelDataPointerList[FChannels.Count - 1]);
  end
 else FChannelData := TAudioData32.Create(AudioDataCollection);
 SampleFramesChanged;
end;

destructor TAudioChannel32.Destroy;
begin
 FreeAndNil(FChannelData);
 inherited;
end;

{$IFDEF DELPHI10_UP} {$region 'TAudioChannel32 Wrapper'} {$ENDIF}
procedure TAudioChannel32.Add(Constant: Double);
begin
 FChannelData.Add(Constant);
end;

procedure TAudioChannel32.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioChannel32
  then FChannelData.AssignTo(TAudioChannel32(Dest).FChannelData) else
 if Dest is TAudioChannel64
  then FChannelData.AssignTo(TAudioChannel64(Dest).FChannelData);
end;

procedure TAudioChannel32.Clear;
begin
 FChannelData.Clear;
end;

procedure TAudioChannel32.GenerateWhiteNoise(Amplitude: Double);
begin
 FChannelData.GenerateWhiteNoise(Amplitude);
end;

function TAudioChannel32.GetChannelData(Sample: Int64): Single;
begin
 result := FChannelData.ChannelData[Sample];
end;

function TAudioChannel32.GetChannelDataPointer: PDAVSingleFixedArray;
begin
 result := FChannelData.ChannelDataPointer;
end;

function TAudioChannel32.GetPeak: Double;
begin
 result := FChannelData.GetPeak;
end;

function TAudioChannel32.GetRMS: Double;
begin
 result := FChannelData.GetRMS;
end;

function TAudioChannel32.GetSum: Double;
begin
 result := FChannelData.GetSum;
end;

procedure TAudioChannel32.Multiply(Factor: Double);
begin
 FChannelData.Multiply(Factor);
end;

procedure TAudioChannel32.Exponentiate(Exponent: Double);
begin
 FChannelData.Exponentiate(Exponent);
end;

procedure TAudioChannel32.Rectify;
begin
 FChannelData.Rectify;
end;

procedure TAudioChannel32.RemoveDC;
begin
 FChannelData.RemoveDC;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

procedure TAudioChannel32.SampleFramesChanged;
begin
 inherited;
 if assigned(FChannelData)
  then FChannelData.SampleCount := FSampleCount;
end;

procedure TAudioChannel32.SetChannelData(Sample: Int64; const Value: Single);
begin
 FChannelData.ChannelData[Sample] := Value;
end;

{ TAudioChannel64 }

constructor TAudioChannel64.Create(Collection: TCollection);
begin
 inherited;
 if AudioDataCollection.ExternalData then
  begin
   assert(AudioDataCollection is TCustomAudioDataCollection64);
   with TCustomAudioDataCollection64(AudioDataCollection)
    do FChannelData := TAudioData64.Create(AudioDataCollection, FChannelDataPointerList[FChannels.Count - 1]);
  end
 else FChannelData := TAudioData64.Create(AudioDataCollection);
 SampleFramesChanged;
end;

destructor TAudioChannel64.Destroy;
begin
 FreeAndNil(FChannelData);
 inherited;
end;

{$IFDEF DELPHI10_UP} {$region 'TAudioChannel64 Wrapper'} {$ENDIF}
procedure TAudioChannel64.Add(Constant: Double);
begin
 FChannelData.Add(Constant);
end;

procedure TAudioChannel64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAudioChannel64
  then FChannelData.AssignTo(TAudioChannel64(Dest).FChannelData) else
 if Dest is TAudioChannel32
  then FChannelData.AssignTo(TAudioChannel32(Dest).FChannelData);
end;

procedure TAudioChannel64.Clear;
begin
 FChannelData.Clear;
end;

procedure TAudioChannel64.GenerateWhiteNoise(Amplitude: Double);
begin
 FChannelData.GenerateWhiteNoise(Amplitude);
end;

function TAudioChannel64.GetChannelData(Sample: Int64): Double;
begin
 result := FChannelData.ChannelData[Sample];
end;

function TAudioChannel64.GetChannelDataPointer: PDAVDoubleFixedArray;
begin
 result := FChannelData.ChannelDataPointer;
end;

function TAudioChannel64.GetPeak: Double;
begin
 result := FChannelData.Peak;
end;

function TAudioChannel64.GetRMS: Double;
begin
 result := FChannelData.RMS;
end;

function TAudioChannel64.GetSum: Double;
begin
 result := FChannelData.Sum;
end;

procedure TAudioChannel64.Multiply(Factor: Double);
begin
 FChannelData.Multiply(Factor);
end;

procedure TAudioChannel64.Exponentiate(Exponent: Double);
begin
 FChannelData.Exponentiate(Exponent);
end;

procedure TAudioChannel64.Rectify;
begin
 FChannelData.Rectify;
end;

procedure TAudioChannel64.RemoveDC;
begin
 FChannelData.RemoveDC;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

procedure TAudioChannel64.SampleFramesChanged;
begin
 inherited;
 if assigned(FChannelData)
  then FChannelData.SampleCount := FSampleCount;
end;

procedure TAudioChannel64.SetChannelData(Sample: Int64; const Value: Double);
begin
 FChannelData.ChannelData[Sample] := Value;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'AudioDataCollection implementation'} {$ENDIF}
{ TCustomAudioDataCollection }

constructor TCustomAudioDataCollection.Create(AOwner: TComponent);
begin
 inherited;
 CreateChannels;
end;

destructor TCustomAudioDataCollection.Destroy;
begin
 if assigned(FChannels) then FreeAndNil(FChannels);
 inherited;
end;

procedure TCustomAudioDataCollection.Add(Constant: Double);
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1
  do TCustomAudioChannel(FChannels.Items[ch]).Add(Constant);
end;

procedure TCustomAudioDataCollection.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomAudioDataCollection then
  begin
   TCustomAudioDataCollection(Dest).FSampleFrames := FSampleFrames;
   FChannels.AssignTo(TCustomAudioDataCollection(Dest).FChannels);
  end;
end;

procedure TCustomAudioDataCollection.Clear;
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1
  do TCustomAudioChannel(FChannels.Items[ch]).Clear;
end;

procedure TCustomAudioDataCollection.Exponentiate(Exponent: Double);
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1
  do TCustomAudioChannel(FChannels.Items[ch]).Exponentiate(Exponent);
end;

procedure TCustomAudioDataCollection.GenerateWhiteNoise(Amplitude: Double);
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1
  do TCustomAudioChannel(FChannels.Items[ch]).GenerateWhiteNoise(Amplitude);
end;

procedure TCustomAudioDataCollection.Multiply(Factor: Double);
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1
  do TCustomAudioChannel(FChannels.Items[ch]).Multiply(Factor);
end;

procedure TCustomAudioDataCollection.Rectify;
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1
  do TCustomAudioChannel(FChannels.Items[ch]).Rectify;
end;

procedure TCustomAudioDataCollection.RemoveDC;
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1
  do TCustomAudioChannel(FChannels.Items[ch]).RemoveDC;
end;

function TCustomAudioDataCollection.GetChannelCount: Integer;
begin
 if assigned(FChannels)
  then result := FChannels.Count
  else result := 0;
end;

procedure TCustomAudioDataCollection.SampleFramesChanged;
var
  ch : Integer;
begin
 for ch := 0 to FChannels.Count - 1 do
  begin
   assert(FChannels.Items[ch] is TCustomAudioChannel);
   if TCustomAudioChannel(FChannels.Items[ch]).SampleCount <> FSampleFrames
    then TCustomAudioChannel(FChannels.Items[ch]).SampleFramesChanged;
  end;
end;

procedure TCustomAudioDataCollection.SetChannelCount(const Value: Integer);
begin
 // delete or add channels until the count matches the desired channel count
 if not FExternalData then
  begin
   while Channels.Count > Value do Channels.Delete(Channels.Count - 1);
   while Channels.Count < Value do Channels.Add;
  end;
end;

procedure TCustomAudioDataCollection.SetSampleFrames(const Value: Cardinal);
begin
 if (FSampleFrames <> Value) and not FExternalData then
  begin
   FSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

{ TCustomAudioDataCollection32 }

constructor TCustomAudioDataCollection32.Create(AOwner: TComponent;
  AChannels: Integer; ASampleFrames: Int64; DataPtr: Pointer = nil);
var
  ch : Integer;
begin
 inherited Create(AOwner);
 FExternalData := DataPtr <> nil;
 if FExternalData then
  begin
   SetLength(FChannelDataPointerList, AChannels);
   FSampleFrames := ASampleFrames; 
   FChannels.Clear;
   for ch := 0 to AChannels - 1 do
    begin
     FChannelDataPointerList[ch] := DataPtr;
     FChannels.Add;
    end;
  end
 else
  begin
   ChannelCount := AChannels;
   SampleFrames := ASampleFrames;
  end;
end;

procedure TCustomAudioDataCollection32.CreateChannels;
begin
 FChannels := TCustomAudioChannels.Create(Self, TAudioChannel32);
 FChannels.OnChanged := RebuildChannelList;
end;

procedure TCustomAudioDataCollection32.RebuildChannelList(Sender: TObject);
var
  i : Integer;
begin
 if not FExternalData then
  begin
   SetLength(FChannelDataPointerList, FChannels.Count);
   for i := 0 to FChannels.Count - 1 do
    if FChannels.Items[i] is TAudioChannel32 then
     with TAudioChannel32(FChannels.Items[i])
      do FChannelDataPointerList[i] := ChannelDataPointer;
  end;
end;

function TCustomAudioDataCollection32.GetAudioChannel(index: Integer): TAudioChannel32;
begin
 if (Index < 0) or (Index >= FChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TAudioChannel32(FChannels.Items[index]);
end;

function TCustomAudioDataCollection32.GetChannelDataPointerList(
  Channel: Integer): PDAVSingleFixedArray;
begin
 result := ChannelList[Channel].ChannelDataPointer;
end;

{ TCustomAudioDataCollection64 }

constructor TCustomAudioDataCollection64.Create(AOwner: TComponent;
  AChannels: Integer; ASampleFrames: Int64; DataPtr: Pointer = nil);
var
  ch : Integer;
begin
 inherited Create(AOwner);
 FExternalData := DataPtr <> nil;
 if FExternalData then
  begin
   SetLength(FChannelDataPointerList, AChannels);
   FSampleFrames := ASampleFrames; 
   FChannels.Clear;
   for ch := 0 to AChannels - 1 do
    begin
     FChannelDataPointerList[ch] := DataPtr;
     FChannels.Add;
    end;
  end
 else
  begin
   ChannelCount := AChannels;
   SampleFrames := ASampleFrames;
  end;
end;

procedure TCustomAudioDataCollection64.CreateChannels;
begin
 FChannels := TCustomAudioChannels.Create(Self, TAudioChannel64);
 FChannels.OnChanged := RebuildChannelList;
end;

procedure TCustomAudioDataCollection64.RebuildChannelList(Sender: TObject);
var
  i : Integer;
begin
 if not FExternalData then
  begin
   SetLength(FChannelDataPointerList, FChannels.Count);
   for i := 0 to FChannels.Count - 1 do
    if FChannels.Items[i] is TAudioChannel64 then
     with TAudioChannel64(FChannels.Items[i])
      do FChannelDataPointerList[i] := ChannelDataPointer;
  end;
end;

function TCustomAudioDataCollection64.GetAudioChannel(index: Integer): TAudioChannel64;
begin
 if (Index < 0) or (Index >= FChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TAudioChannel64(FChannels.Items[index]);
end;

function TCustomAudioDataCollection64.GetChannelDataPointerList(
  Channel: Integer): PDAVDoubleFixedArray;
begin
 result := ChannelList[Channel].ChannelDataPointer;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

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
