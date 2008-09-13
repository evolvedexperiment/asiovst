unit DAV_ComplexData;

interface

uses
  Classes, DAV_Common, DAV_Complex, DAV_AudioData;

type
  TCustomComplexData = class;
  TCustomComplexChannels = class(TOwnedCollection);
  TComplexChannels32 = class(TCustomComplexChannels);
  TComplexChannels64 = class(TCustomComplexChannels);

  TCustomComplexChannel = class(TCollectionItem)
  private
    fDisplayName  : string;
    fChannelData  : TCustomComplexChannels;
    fBinCount     : Cardinal;
    function GetComplexData: TCustomComplexData;
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
    procedure BinCountChanged; virtual;
    property ComplexData: TCustomComplexData read GetComplexData;
  public
    constructor Create(Collection: TCollection); override;

    // some processing functions
    procedure Clear; virtual; abstract;

    property BinCount: Cardinal read fBinCount;
  published
    property DisplayName;
  end;

  TComplexChannel32 = class(TCustomComplexChannel)
  private
    fChannelData  : PAVDComplexSingleFixedArray;
    function GetChannelDataImaginary(Bin: Int64): Single;
    function GetChannelDataMagnitude(Bin: Int64): Single;
    function GetChannelDataPhase(Bin: Int64): Single;
    function GetChannelDataReal(Bin: Int64): Single;
    procedure SetChannelDataImaginary(Bin: Int64; const Value: Single);
    procedure SetChannelDataMagnitude(Bin: Int64; const Value: Single);
    procedure SetChannelDataPhase(Bin: Int64; const Value: Single);
    procedure SetChannelDataReal(Bin: Int64; const Value: Single);
  protected
    procedure BinCountChanged; override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Clear; override;

    // data access properties
    property ChannelDataReal[Sample: Int64]: Single read GetChannelDataReal write SetChannelDataReal;
    property ChannelDataImaginary[Sample: Int64]: Single read GetChannelDataImaginary write SetChannelDataImaginary;
    property ChannelDataMagnitude[Sample: Int64]: Single read GetChannelDataMagnitude write SetChannelDataMagnitude;
    property ChannelDataPhase[Sample: Int64]: Single read GetChannelDataPhase write SetChannelDataPhase;
    property ChannelDataPointer: PAVDComplexSingleFixedArray read fChannelData;
  end;

  TComplexChannel64 = class(TCustomComplexChannel)
  private
    fChannelData : PAVDComplexDoubleFixedArray;
    function GetChannelDataImaginary(Bin: Int64): Double;
    function GetChannelDataMagnitude(Bin: Int64): Double;
    function GetChannelDataPhase(Bin: Int64): Double;
    function GetChannelDataReal(Bin: Int64): Double;
    procedure SetChannelDataImaginary(Bin: Int64; const Value: Double);
    procedure SetChannelDataMagnitude(Bin: Int64; const Value: Double);
    procedure SetChannelDataPhase(Bin: Int64; const Value: Double);
    procedure SetChannelDataReal(Bin: Int64; const Value: Double);
  protected
    procedure BinCountChanged; override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Clear; override;

    // data acces properties
    property ChannelDataReal[Sample: Int64]: Double read GetChannelDataReal write SetChannelDataReal;
    property ChannelDataImaginary[Sample: Int64]: Double read GetChannelDataImaginary write SetChannelDataImaginary;
    property ChannelDataMagnitude[Sample: Int64]: Double read GetChannelDataMagnitude write SetChannelDataMagnitude;
    property ChannelDataPhase[Sample: Int64]: Double read GetChannelDataPhase write SetChannelDataPhase;
    property ChannelDataPointer: PAVDComplexDoubleFixedArray read fChannelData;
  end;

  TCustomComplexData = class(TAudioObject)
  private
    fBinCount : Cardinal;
    procedure SetBinCount(const Value: Cardinal);
  protected
    fChannels : TCustomComplexChannels;
    procedure BinCountChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BinCount: Cardinal read fBinCount write SetBinCount;
    property Channels: TCustomComplexChannels read fChannels write fChannels;
  end;

  TComplexData32 = class(TCustomComplexData)
  private
    function GetComplexChannel(index: Integer): TComplexChannel32; virtual;
  protected
    property ChannelList[index: Integer]: TComplexChannel32 read GetComplexChannel; default;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Channels;
    property BinCount;
    property SampleRate;
    property SampleRateSource;
  end;

  TComplexData64 = class(TCustomComplexData)
  private
    function GetComplexChannel(index: Integer): TComplexChannel64; virtual;
  protected
    property ChannelList[index: Integer]: TComplexChannel64 read GetComplexChannel; default;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Channels;
    property BinCount;
    property SampleRate;
    property SampleRateSource;
  end;

procedure Register;

implementation

uses
  SysUtils, Math;

{ TCustomComplexChannel }

procedure TCustomComplexChannel.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomComplexChannel then
  begin
   TCustomComplexChannel(Dest).fDisplayName := fDisplayName;
   TCustomComplexChannel(Dest).fChannelData := fChannelData;
  end
 else inherited;
end;

constructor TCustomComplexChannel.Create(Collection: TCollection);
begin
 inherited;
 fDisplayName := 'Channel ' + IntToStr(Collection.Count);
 BinCountChanged;
end;

function TCustomComplexChannel.GetComplexData: TCustomComplexData;
begin
 assert(Collection is TCustomComplexChannels);
 assert(TCustomComplexChannels(Collection).Owner is TCustomComplexData);
 result := TCustomComplexData(TCustomComplexChannels(GetOwner).GetOwner);
end;

function TCustomComplexChannel.GetDisplayName: string;
begin
 result := fDisplayName;
end;

procedure TCustomComplexChannel.BinCountChanged;
begin
 fBinCount := ComplexData.BinCount;
end;

procedure TCustomComplexChannel.SetDisplayName(const Value: string);
begin
 fDisplayName := Value;
 inherited;
end;

{ TComplexChannel32 }

procedure TComplexChannel32.Clear;
begin
 FillChar(fChannelData, BinCount * SizeOf(TComplexSingle), 0);
end;

destructor TComplexChannel32.Destroy;
begin
 if assigned(fChannelData) then
  begin
   Dispose(fChannelData);
   fChannelData := nil;
  end;
 inherited;
end;

function TComplexChannel32.GetChannelDataReal(Bin: Int64): Single;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := fChannelData[Bin].Re
  else raise Exception.Create('Bin out of range');
end;

function TComplexChannel32.GetChannelDataImaginary(Bin: Int64): Single;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := fChannelData[Bin].Im
  else raise Exception.Create('Bin out of range');
end;

function TComplexChannel32.GetChannelDataMagnitude(Bin: Int64): Single;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := sqrt(sqr(fChannelData[Bin].Re) + sqr(fChannelData[Bin].Im))
  else raise Exception.Create('Bin out of range');
end;

function TComplexChannel32.GetChannelDataPhase(Bin: Int64): Single;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := arctan2(fChannelData[Bin].Im, fChannelData[Bin].Re)
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.BinCountChanged;
begin
 ReallocMem(fChannelData, ComplexData.BinCount * SizeOf(TComplexSingle));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if ComplexData.BinCount > BinCount
  then FillChar(fChannelData[BinCount], (ComplexData.BinCount - BinCount) * SizeOf(TComplexSingle), 0);

 inherited;
end;

procedure TComplexChannel32.SetChannelDataReal(Bin: Int64; const Value: Single);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then fChannelData[Bin].Re := Value
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.SetChannelDataImaginary(Bin: Int64;
  const Value: Single);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then fChannelData[Bin].Im := Value
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.SetChannelDataMagnitude(Bin: Int64;
  const Value: Single);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then raise Exception.Create('Not supported yet!')
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.SetChannelDataPhase(Bin: Int64;
  const Value: Single);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then raise Exception.Create('Not supported yet!')
  else raise Exception.Create('Bin out of range');
end;

{ TComplexChannel64 }

procedure TComplexChannel64.Clear;
begin
 FillChar(fChannelData, ComplexData.BinCount * SizeOf(TComplexDouble), 0);
end;

destructor TComplexChannel64.Destroy;
begin
 if assigned(fChannelData) then
  begin
   Dispose(fChannelData);
   fChannelData := nil;
  end;
 inherited;
end;

function TComplexChannel64.GetChannelDataReal(Bin: Int64): Double;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := fChannelData[Bin].Re
  else raise Exception.Create('Bin out of range');
end;

function TComplexChannel64.GetChannelDataImaginary(Bin: Int64): Double;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := fChannelData[Bin].Im
  else raise Exception.Create('Bin out of range');
end;

function TComplexChannel64.GetChannelDataMagnitude(Bin: Int64): Double;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := sqrt(sqr(fChannelData[Bin].Re) + sqr(fChannelData[Bin].Im))
  else raise Exception.Create('Bin out of range');
end;

function TComplexChannel64.GetChannelDataPhase(Bin: Int64): Double;
begin
 if (Bin >= 0) and (Bin < BinCount)
  then result := arctan2(fChannelData[Bin].Im, fChannelData[Bin].Re)
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataReal(Bin: Int64; const Value: Double);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then fChannelData[Bin].Re := Value
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataImaginary(Bin: Int64;
  const Value: Double);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then fChannelData[Bin].Im := Value
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataMagnitude(Bin: Int64;
  const Value: Double);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then raise Exception.Create('Not supported yet!')
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataPhase(Bin: Int64;
  const Value: Double);
begin
 if (Bin >= 0) and (Bin < ComplexData.BinCount)
  then raise Exception.Create('Not supported yet!')
  else raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.BinCountChanged;
begin
 ReallocMem(fChannelData, ComplexData.BinCount * SizeOf(TComplexDouble));

 // check if new length is longer than the old length and fill with zeroes if necessary
 if ComplexData.BinCount > BinCount
  then FillChar(fChannelData[BinCount], (ComplexData.BinCount - BinCount) * SizeOf(TComplexDouble), 0);

 inherited;
end;

{ TCustomComplexData }

constructor TCustomComplexData.Create(AOwner: TComponent);
begin
 inherited;
end;

destructor TCustomComplexData.Destroy;
begin
 if assigned(fChannels) then FreeAndNil(fChannels);
 inherited;
end;

procedure TCustomComplexData.BinCountChanged;
var
  ch : Integer;
begin
 for ch := 0 to fChannels.Count - 1 do
  begin
   assert(fChannels.Items[ch] is TCustomComplexChannel);
   if TCustomComplexChannel(fChannels.Items[ch]).BinCount <> fBinCount
    then TCustomComplexChannel(fChannels.Items[ch]).BinCountChanged;
  end;
end;

procedure TCustomComplexData.SetBinCount(const Value: Cardinal);
begin
 if fBinCount <> Value then
  begin
   fBinCount := Value;
   BinCountChanged;
  end;
end;

{ TComplexData32 }

constructor TComplexData32.Create(AOwner: TComponent);
begin
 inherited;
 fChannels := TCustomComplexChannels.Create(Self, TComplexChannel32);
end;

function TComplexData32.GetComplexChannel(index: Integer): TComplexChannel32;
begin
 if (Index < 0) or (Index >= fChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TComplexChannel32(fChannels.Items[index]);
end;

{ TComplexData64 }

constructor TComplexData64.Create(AOwner: TComponent);
begin
 inherited;
 fChannels := TCustomComplexChannels.Create(Self, TComplexChannel64);
end;

function TComplexData64.GetComplexChannel(index: Integer): TComplexChannel64;
begin
 if (Index < 0) or (Index >= fChannels.Count)
  then raise Exception.Create('Index out of bounds')
  else result := TComplexChannel64(fChannels.Items[index]);
end;

////////////////////////////////////////////////////////////////////////////////

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TComplexData32]);
  RegisterComponents('ASIO/VST Basics', [TComplexData64]);
end;

end.
