unit DAudioData;

interface

uses
  Classes, DAVDCommon;

type
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

  TCustomAudioObject = class(TComponent)
  private
    fInternalSampleRateSource : TSampleRateSource;
    function GetSampleRate: Double;
    procedure SetSampleRate(const Value: Double);
    procedure SetSampleRateSource(const Value: TSampleRateSource);
  protected
    fSampleRateSource : TSampleRateSource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // properties:
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property SampleRateSource: TSampleRateSource read fSampleRateSource write SetSampleRateSource;
  end;

  TAudioObject = class(TCustomAudioObject)
  published
    property SampleRateSource;
  end;

  TCustomAudioData = class;
  TCustomAudioChannels = class(TOwnedCollection);
  TAudioChannels32 = class(TCustomAudioChannels);
  TAudioChannels64 = class(TCustomAudioChannels);

  TCustomAudioChannel = class(TCollectionItem)
  private
    fDisplayName  : string;
    fChannelData  : TCustomAudioChannels;
    fSampleCount : Cardinal;
    function GetAudioData: TCustomAudioData;
  protected
    function GetDisplayName: string; override;
    function GetSum: Double; virtual; abstract;
    function GetRMS: Double; virtual; abstract;
    function GetPeak: Double; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
    procedure SampleFramesChanged; virtual;
    property AudioData: TCustomAudioData read GetAudioData;
  public
    constructor Create(Collection: TCollection); override;

    // some processing functions
    procedure Clear; virtual; abstract;
    procedure Multiply(Factor: Double); virtual; abstract;
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
    fChannelData : PAVDSingleFixedArray;
    function GetChannelData(Sample: Int64): Single;
    procedure SetChannelData(Sample: Int64; const Value: Single);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleFramesChanged; override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Clear; override;
    procedure Multiply(Factor: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;

    // data acces properties
    property ChannelData[Sample: Int64]: Single read GetChannelData write SetChannelData;
    property ChannelDataPointer: PAVDSingleFixedArray read fChannelData;
  end;

  TAudioChannel64 = class(TCustomAudioChannel)
  private
    fChannelData  : PAVDDoubleFixedArray;
    function GetChannelData(Sample: Int64): Double;
    procedure SetChannelData(Sample: Int64; const Value: Double);
  protected
    function GetSum: Double; override;
    function GetRMS: Double; override;
    function GetPeak: Double; override;
    procedure SampleFramesChanged; override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Multiply(Factor: Double); override;
    procedure Rectify; override;
    procedure RemoveDC; override;
    procedure Clear; override;

    // data acces properties
    property ChannelData[Sample: Int64]: Double read GetChannelData write SetChannelData;
    property ChannelDataPointer: PAVDDoubleFixedArray read fChannelData;
  end;

  TCustomAudioData = class(TCustomAudioObject)
  private
    fSampleFrames : Cardinal;
    procedure SetSampleFrames(const Value: Cardinal);
  protected
    fChannels : TCustomAudioChannels;
    procedure SampleFramesChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SampleFrames: Cardinal read fSampleFrames write SetSampleFrames;
    property Channels: TCustomAudioChannels read fChannels write fChannels;
  end;

  TAudioData32 = class(TCustomAudioData)
  private
    function GetAudioChannel(index: Integer): TAudioChannel32; virtual;
  protected
    property ChannelList[index: Integer]: TAudioChannel32 read GetAudioChannel; default;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Channels;
    property SampleFrames;
    property SampleRate;
    property SampleRateSource;
  end;

  TAudioData64 = class(TCustomAudioData)
  private
    function GetAudioChannel(index: Integer): TAudioChannel64; virtual;
  protected
    property ChannelList[index: Integer]: TAudioChannel64 read GetAudioChannel; default;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Channels;
    property SampleFrames;
    property SampleRate;
    property SampleRateSource;
  end;

procedure Register;

implementation

uses
  SysUtils;

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

{ TCustomAudioObject }

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

{ TCustomAudioChannel }

procedure TCustomAudioChannel.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAudioChannel then
  begin
   TCustomAudioChannel(Dest).fDisplayName := fDisplayName;
   TCustomAudioChannel(Dest).fChannelData := fChannelData;
  end
 else inherited;
end;

constructor TCustomAudioChannel.Create(Collection: TCollection);
begin
 inherited;
 fDisplayName := 'Channel ' + IntToStr(Collection.Count);
 SampleFramesChanged;
end;

function TCustomAudioChannel.GetAudioData: TCustomAudioData;
begin
 assert(Collection is TCustomAudioChannels);
 assert(TCustomAudioChannels(Collection).Owner is TCustomAudioData);
 result := TCustomAudioData(TCustomAudioChannels(GetOwner).GetOwner);
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

procedure TAudioChannel32.Clear;
begin
 FillChar(fChannelData^, SampleCount * SizeOf(Single), 0);
end;

destructor TAudioChannel32.Destroy;
begin
 if assigned(fChannelData) then
  begin
   Dispose(fChannelData);
   fChannelData := nil;
  end;
 inherited;
end;

function TAudioChannel32.GetChannelData(Sample: Int64): Single;
begin
 if (Sample >= 0) and (Sample < SampleCount)
  then result := fChannelData[Sample]
  else raise Exception.Create('Sample out of range');
end;

function TAudioChannel32.GetPeak: Double;
var
  Sample       : Integer;
begin
 result := 0;
 if SampleCount = 0 then exit;

 for Sample := 0 to SampleCount - 1 do
  if abs(fChannelData^[Sample]) > result
   then result := abs(fChannelData^[Sample]);
end;

function TAudioChannel32.GetRMS: Double;
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

function TAudioChannel32.GetSum: Double;
var
  Sample : Integer;
begin
 result := 0;
 if SampleCount = 0 then exit;

 for Sample := 0 to SampleCount - 1
  do result := result + fChannelData^[Sample];
end;

procedure TAudioChannel32.Multiply(Factor: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := fChannelData^[Sample] * Factor;
end;

procedure TAudioChannel32.Rectify;
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := Abs(fChannelData^[Sample]);
end;

procedure TAudioChannel32.RemoveDC;
var
  Sample       : Integer;
  DC           : Double;
begin
 if SampleCount = 0 then exit;

 DC := Sum / SampleCount;
 for Sample := 0 to SampleCount - 1
  do fChannelData^[Sample] := fChannelData^[Sample] - DC;
end;

procedure TAudioChannel32.SampleFramesChanged;
begin
 ReallocMem(fChannelData, AudioData.SampleFrames * SizeOf(Single));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if AudioData.SampleFrames > SampleCount
  then FillChar(fChannelData^[SampleCount], (AudioData.SampleFrames - SampleCount) * SizeOf(Single), 0);

 inherited;
end;

procedure TAudioChannel32.SetChannelData(Sample: Int64; const Value: Single);
begin
 if (Sample >= 0) and (Sample < AudioData.SampleFrames)
  then fChannelData[Sample] := Value
  else raise Exception.Create('Sample out of range');
end;

{ TAudioChannel64 }

procedure TAudioChannel64.Clear;
begin
 FillChar(fChannelData^, AudioData.SampleFrames * SizeOf(Double), 0);
end;

destructor TAudioChannel64.Destroy;
begin
 if assigned(fChannelData) then
  begin
   Dispose(fChannelData);
   fChannelData := nil;
  end;
 inherited;
end;

function TAudioChannel64.GetChannelData(Sample: Int64): Double;
begin
 if (Sample >= 0) and (Sample < AudioData.SampleFrames)
  then result := fChannelData[Sample]
  else raise Exception.Create('Sample out of range');
end;

function TAudioChannel64.GetPeak: Double;
var
  Sample       : Integer;
  SampleFrames : Integer;
begin
 result := 0;
 SampleFrames := AudioData.SampleFrames;
 if SampleFrames = 0 then exit;

 for Sample := 0 to SampleFrames - 1 do
  if abs(fChannelData^[Sample]) > result
   then result := abs(fChannelData^[Sample]);
end;

function TAudioChannel64.GetRMS: Double;
var
  Sample       : Integer;
  SampleFrames : Integer;
  SquaredSum   : Double;
begin
 result := 0;
 SampleFrames := AudioData.SampleFrames;
 if SampleFrames = 0 then exit;

 SquaredSum := 0;
 for Sample := 0 to SampleFrames - 1
  do SquaredSum := SquaredSum + sqr(fChannelData^[Sample]);
 result := sqrt(SquaredSum / SampleFrames);
end;

function TAudioChannel64.GetSum: Double;
var
  Sample       : Integer;
  SampleFrames : Integer;
begin
 result       := 0;
 SampleFrames := AudioData.SampleFrames;
 if SampleFrames = 0 then exit;

 for Sample := 0 to SampleFrames - 1
  do result := result + fChannelData^[Sample];
end;

procedure TAudioChannel64.Multiply(Factor: Double);
var
  Sample : Integer;
begin
 for Sample := 0 to AudioData.SampleFrames - 1
  do fChannelData^[Sample] := fChannelData^[Sample] * Factor;
end;

procedure TAudioChannel64.Rectify;
var
  Sample : Integer;
begin
 for Sample := 0 to AudioData.SampleFrames - 1
  do fChannelData^[Sample] := Abs(fChannelData^[Sample]);
end;

procedure TAudioChannel64.RemoveDC;
var
  Sample       : Integer;
  SampleFrames : Integer;
  DC           : Double;
begin
 SampleFrames := AudioData.SampleFrames;
 if SampleFrames = 0 then exit;

 DC := Sum / SampleFrames;
 for Sample := 0 to SampleFrames - 1
  do fChannelData^[Sample] := fChannelData^[Sample] - DC;
end;

procedure TAudioChannel64.SampleFramesChanged;
begin
 ReallocMem(fChannelData, AudioData.SampleFrames * SizeOf(Double));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if AudioData.SampleFrames > SampleCount
  then FillChar(fChannelData^[SampleCount], (AudioData.SampleFrames - SampleCount) * SizeOf(Double), 0);

 inherited;
end;

procedure TAudioChannel64.SetChannelData(Sample: Int64; const Value: Double);
begin
 if (Sample >= 0) and (Sample < AudioData.SampleFrames)
  then fChannelData[Sample] := Value
  else raise Exception.Create('Sample out of range');
end;

{ TCustomAudioData }

constructor TCustomAudioData.Create(AOwner: TComponent);
begin
 inherited;
end;

destructor TCustomAudioData.Destroy;
begin
 if assigned(fChannels) then FreeAndNil(fChannels);
 inherited;
end;

procedure TCustomAudioData.SampleFramesChanged;
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

procedure TCustomAudioData.SetSampleFrames(const Value: Cardinal);
begin
 if fSampleFrames <> Value then
  begin
   fSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

{ TAudioData32 }

constructor TAudioData32.Create(AOwner: TComponent);
begin
 inherited;
 fChannels := TCustomAudioChannels.Create(Self, TAudioChannel32);
end;

function TAudioData32.GetAudioChannel(index: Integer): TAudioChannel32;
begin
 if (Index < 0) or (Index >= fChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TAudioChannel32(fChannels.Items[index]);
end;

{ TAudioData64 }

constructor TAudioData64.Create(AOwner: TComponent);
begin
 inherited;
 fChannels := TCustomAudioChannels.Create(Self, TAudioChannel64);
end;

function TAudioData64.GetAudioChannel(index: Integer): TAudioChannel64;
begin
 if (Index < 0) or (Index >= fChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TAudioChannel64(fChannels.Items[index]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TSampleRateSource]);
  RegisterComponents('ASIO/VST Basics', [TAudioData32]);
  RegisterComponents('ASIO/VST Basics', [TAudioData64]);
end;

end.
