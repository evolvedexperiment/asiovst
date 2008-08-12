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

  TAudioChannels = class(TCollectionItem)
  private
    fDisplayName : string;
    fChannelData : PAVDSingleFixedArray;
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  published
    property DisplayName;
  end;

  TCustomAudioData = class(TAudioObject)
  private
    fSampleFrames : Cardinal;
    fChannels     : TOwnedCollection;
    procedure SetSampleFrames(const Value: Cardinal);
  protected
    procedure SampleFramesChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SampleFrames: Cardinal read fSampleFrames write SetSampleFrames;
    property Channels: TOwnedCollection read fChannels write fChannels;
  end;

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

{ TAudioChannels }

procedure TAudioChannels.AssignTo(Dest: TPersistent);
begin
 if Dest is TAudioChannels then
  begin
   TAudioChannels(Dest).fDisplayName := fDisplayName;
   TAudioChannels(Dest).fChannelData := fChannelData; 
  end
 else inherited;
end;

function TAudioChannels.GetDisplayName: string;
begin
 result := fDisplayName;
end;

procedure TAudioChannels.SetDisplayName(const Value: string);
begin
 fDisplayName := Value;
 inherited;
end;

{ TCustomAudioData }

constructor TCustomAudioData.Create(AOwner: TComponent);
begin
 inherited;
 fChannels := TOwnedCollection.Create(Self, TAudioChannels);
end;

destructor TCustomAudioData.Destroy;
begin
 FreeAndNil(fChannels);
 inherited;
end;

procedure TCustomAudioData.SampleFramesChanged;
begin
 // do nothing yet
end;

procedure TCustomAudioData.SetSampleFrames(const Value: Cardinal);
begin
 if fSampleFrames <> Value then
  begin
   fSampleFrames := Value;
   SampleFramesChanged;
  end;
end;

end.
