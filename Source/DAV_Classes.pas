unit DAV_Classes;

// based on code found in the GLScene (see www.glscene.org)

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_SampleRateSource;

type
  // TNotifiablePersistent
  TNotifiablePersistent = class(TInterfacedPersistent)
  private
    FUpdateCount : Integer;
    FOnChange    : TNotifyEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    property UpdateCount: Integer read FUpdateCount;
  public
    procedure Changed; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDspPersistent = class(TNotifiablePersistent);

  TDspSampleRatePersistent = class(TDspPersistent)
  private
    FSampleRate : Double;
    procedure SetSampleRate(const Value: Double);
  protected
    procedure SampleRateChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

  // some interfaces

  {.$IFDEF DELPHI7_UP}
  IDspSink32 = interface(IInterface)
    procedure ProcessSample32(Input: Single);
  end;

  IDspSink64 = interface(IInterface)
    procedure ProcessSample64(Input: Double);
  end;

  IDspProcessor32 = interface(IInterface)
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
  end;

  IDspProcessor64 = interface(IInterface)
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;
  end;

  IDspGenerator32 = interface(IInterface)
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32: Single;
  end;

  IDspGenerator64 = interface(IInterface)
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64: Double;
  end;
  {.$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'AudioComponent classes'} {$ENDIF}
  TCustomAudioComponent = class(TComponent)
  private
    FInternalSampleRateSource : TSampleRateSource;
    function GetSampleRate: Double;
    procedure SetSampleRate(const Value: Double);
    procedure SetSampleRateSource(const Value: TSampleRateSource);
  protected
    FSampleRateSource : TSampleRateSource;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
                                                         
    // properties:
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property SampleRateSource: TSampleRateSource read FSampleRateSource write SetSampleRateSource;
  end;

  TAudioComponent = class(TCustomAudioComponent)
  published
    property SampleRate;
    property SampleRateSource;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  TDAVUpdateAbleObject = class(TPersistent)
  private
    FOwner : TPersistent;
    FUpdating : Integer;
    FOnNotifyChange : TNotifyEvent;
  public
    constructor Create(AOwner: TPersistent); virtual;

    procedure NotifyChange(Sender : TObject); virtual;
    function GetOwner : TPersistent; override;

    property Updating : Integer read FUpdating;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner : TPersistent read FOwner;
    property OnNotifyChange : TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
 end;

  TDAVCadenceAbleComponent = class (TComponent)
  public
    {$IFNDEF DELPHI_5_UP}
    procedure RemoveFreeNotification(AComponent: TComponent);
    {$ENDIF}
  end;

  TDAVUpdateAbleComponent = class (TDAVCadenceAbleComponent)
  public
    procedure NotifyChange(Sender : TObject); virtual;
  end;


implementation

uses
  SysUtils;

resourcestring
  RCStrInvalidSamplerate = 'Invalid Samplerate!';

{ TNotifiablePersistent }

procedure TNotifiablePersistent.AssignTo(Dest: TPersistent);
begin
 if Dest is TNotifiablePersistent then
  with TNotifiablePersistent(Dest) do
   begin
    inherited;
    FUpdateCount := Self.FUpdateCount;
    FOnChange    := Self.FOnChange;
   end
  else inherited;
end;

procedure TNotifiablePersistent.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TNotifiablePersistent.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TNotifiablePersistent.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TThreadPersistent.EndUpdate');
  Dec(FUpdateCount);
end;


{ TDspSampleRateObject }

constructor TDspSampleRatePersistent.Create;
begin
 inherited;
 FSampleRate := 44100;
end;

procedure TDspSampleRatePersistent.AssignTo(Dest: TPersistent);
begin
 if Dest is TDspSampleRatePersistent then
  with TDspSampleRatePersistent(Dest) do
   begin
    FSampleRate := Self.FSampleRate;
    inherited;
   end else inherited;
end;

procedure TDspSampleRatePersistent.SetSampleRate(const Value: Double);
begin
 if Value = 0
  then raise Exception.Create(RCStrInvalidSamplerate);

 if FSampleRate <> abs(Value) then
  begin
   FSampleRate := abs(Value);
   SampleRateChanged;
  end;
end;

procedure TDspSampleRatePersistent.SampleRateChanged;
begin
 Changed; 
end;


{$IFDEF DELPHI10_UP} {$region 'AudioComponent implementation'} {$ENDIF}

{ TCustomAudioComponent }

procedure TCustomAudioComponent.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomAudioComponent then
  begin
   TCustomAudioComponent(Dest).FInternalSampleRateSource := FInternalSampleRateSource;
   TCustomAudioComponent(Dest).FSampleRateSource         := FSampleRateSource;
  end
 else inherited;
end;

constructor TCustomAudioComponent.Create(AOwner: TComponent);
begin
 inherited;
 FInternalSampleRateSource := TSampleRateSource.Create(Self);
end;

destructor TCustomAudioComponent.Destroy;
begin
 // in case the internal sample rate source is really internal, release it
 if FSampleRateSource = nil
  then FreeAndNil(FInternalSampleRateSource);
 inherited;
end;

function TCustomAudioComponent.GetSampleRate: Double;
begin
 result := FInternalSampleRateSource.SampleRate;
end;

procedure TCustomAudioComponent.SetSampleRate(const Value: Double);
begin
 // only allow writing in case the samplerate source is internal
 if FSampleRateSource = nil
  then FInternalSampleRateSource.SampleRate := Value;
end;

procedure TCustomAudioComponent.SetSampleRateSource(const Value: TSampleRateSource);
var
  OldSampleRateSource    : TSampleRateSource;
  OldIntSampleRateSource : TSampleRateSource;
  NewIntSampleRateSource : TSampleRateSource;
begin
 if FSampleRateSource <> Value then
  begin
   // store old samplerate sources
   OldSampleRateSource    := FSampleRateSource;
   OldIntSampleRateSource := FInternalSampleRateSource;

   // set actual sample rate source
   FSampleRateSource      := Value;

   // check whether previously the sample rate source was purely internal
   if not assigned(OldSampleRateSource) then
    begin
     // set new internal sample rate source to the actual sample rate source
     FInternalSampleRateSource := FSampleRateSource;

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
     FInternalSampleRateSource := NewIntSampleRateSource;
    end;
  end;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}


{ TDAVUpdateAbleObject }

constructor TDAVUpdateAbleObject.Create(AOwner: TPersistent);
begin
 inherited Create;
 FOwner := AOwner;
end;

procedure TDAVUpdateAbleObject.BeginUpdate;
begin
 Inc(FUpdating);
end;

procedure TDAVUpdateAbleObject.EndUpdate;
begin
 Dec(FUpdating);
 if FUpdating <= 0 then
  begin
   Assert(FUpdating = 0);
   NotifyChange(Self);
  end;
end;

function TDAVUpdateAbleObject.GetOwner: TPersistent;
begin
 Result := Owner;
end;

procedure TDAVUpdateAbleObject.NotifyChange(Sender: TObject);
begin
 if (FUpdating = 0) and Assigned(Owner) then
  begin
   if Owner is TDAVUpdateAbleObject
    then TDAVUpdateAbleObject(Owner).NotifyChange(Self)
    else
   if Owner is TDAVUpdateAbleComponent
    then TDAVUpdateAbleComponent(Owner).NotifyChange(Self);
   if Assigned(FOnNotifyChange)
    then FOnNotifyChange(Self);
  end;
end;

{ TDAVCadenceAbleComponent }

procedure TDAVCadenceAbleComponent.RemoveFreeNotification(
  AComponent: TComponent);
begin
 Notification(AComponent, opRemove);
end;

{ TDAVUpdateAbleComponent }

procedure TDAVUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
 if Assigned(Owner) then
 if (Owner is TDAVUpdateAbleComponent) then
    (Owner as TDAVUpdateAbleComponent).NotifyChange(Self);
end;

end.
