unit DAV_Classes;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_SampleRateSource;

const
  CGUIDDspSink32 : TGUID = '{BF61746A-29F6-445C-A051-1FC0F75F9F3F}';
  CGUIDDspSink64 : TGUID = '{A636DBE8-FDFD-4B2C-ADC7-AC7A0F1F5975}';
  CGUIDDspProcessor32 : TGUID = '{7D9E5AC0-5AFA-4C18-9015-84F5C9338C0C}';
  CGUIDDspProcessor64 : TGUID = '{A331B34E-EFEB-42D5-8DBE-4120C7902E9E}';
  CGUIDDspGenerator32 : TGUID = '{46C2D222-361E-40E6-A21B-C05F6FFD4327}';
  CGUIDDspGenerator64 : TGUID = '{4701B987-CE0F-4E7A-BE9D-BA3899114EC0}';

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

  TDspPersistentClass = class of TDspPersistent;
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
    ['{BF61746A-29F6-445C-A051-1FC0F75F9F3F}']
    procedure ProcessSample32(Input: Single);
  end;

  IDspSink64 = interface(IInterface)
    ['{A636DBE8-FDFD-4B2C-ADC7-AC7A0F1F5975}'] // unique GUID
    procedure ProcessSample64(Input: Double);
  end;

  IDspProcessor32 = interface(IInterface)
    ['{7D9E5AC0-5AFA-4C18-9015-84F5C9338C0C}'] // unique GUID
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
  end;

  IDspProcessor64 = interface(IInterface)
    ['{A331B34E-EFEB-42D5-8DBE-4120C7902E9E}'] // unique GUID
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;
  end;

  IDspGenerator32 = interface(IInterface)
    ['{46C2D222-361E-40E6-A21B-C05F6FFD4327}'] // unique GUID
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32: Single;
  end;

  IDspGenerator64 = interface(IInterface)
    ['{4701B987-CE0F-4E7A-BE9D-BA3899114EC0}'] // unique GUID
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

  // the code below is based on code found in the GLScene (see www.glscene.org)

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

procedure RegisterDspProcessor32(AClass: TDspPersistentClass);
procedure RegisterDspProcessor64(AClass: TDspPersistentClass);
procedure RegisterDspProcessors32(AClasses: array of TDspPersistentClass);
procedure RegisterDspProcessors64(AClasses: array of TDspPersistentClass);

var
  GDspProcessors32 : array of TDspPersistentClass;
  GDspProcessors64 : array of TDspPersistentClass;

implementation

uses
  SysUtils;

resourcestring
  RCStrInvalidSamplerate = 'Invalid Samplerate!';
  RCStrDspProcessorDuplicate = 'DSP Processor registered twice! (%s)';
  RCStrNoIDspProcessor32 = 'Class %s does not support IDspProcessor32';
  RCStrNoIDspProcessor64 = 'Class %s does not support IDspProcessor64';

{ TNotifiablePersistent }

procedure TNotifiablePersistent.AssignTo(Dest: TPersistent);
begin
 if Dest is TNotifiablePersistent then
  with TNotifiablePersistent(Dest) do
   begin
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

function CheckDspProcessor32Class(AClass: TDspPersistentClass): Boolean;
var
  i : Integer;
begin
 try
  Result := True;

  // check if file format is already registered
  for i := 0 to Length(GDspProcessors32) - 1 do
   if GDspProcessors32[i] = AClass
    then raise Exception.CreateFmt(RCStrDspProcessorDuplicate, [AClass.ClassName]);

(*
  // check if the class supports the IDspProcessor32 interface
  if not Supports(AClass, CGUIDDspProcessor32)
   then raise Exception.CreateFmt(RCStrNoIDspProcessor32, [AClass.ClassName]);
*)
 except
  Result := False;
 end;
end;

function CheckDspProcessor64Class(AClass: TDspPersistentClass): Boolean;
var
  i : Integer;
begin
 try
  Result := True;

  // check if file format is already registered
  for i := 0 to Length(GDspProcessors64) - 1 do
   if GDspProcessors64[i] = AClass
    then raise Exception.CreateFmt(RCStrDspProcessorDuplicate, [AClass.ClassName]);

(*
  // check if the class supports the IDspProcessor64 interface
  if not Supports(AClass, CGUIDDspProcessor64)
   then raise Exception.CreateFmt(RCStrNoIDspProcessor64, [AClass.ClassName]);
*)
 except
  Result := False;
 end;
end;

procedure RegisterDspProcessor32(AClass: TDspPersistentClass);
begin
 Assert(CheckDspProcessor32Class(AClass));

 // add file format to list
 SetLength(GDspProcessors32, Length(GDspProcessors32) + 1);
 GDspProcessors32[Length(GDspProcessors32) - 1] := AClass;
end;

procedure RegisterDspProcessor64(AClass: TDspPersistentClass);
begin
 Assert(CheckDspProcessor64Class(AClass));

 // add file format to list
 SetLength(GDspProcessors64, Length(GDspProcessors64) + 1);
 GDspProcessors64[Length(GDspProcessors64) - 1] := AClass;
end;

procedure RegisterDspProcessors32(AClasses: array of TDspPersistentClass);
var
  i : Integer;
begin
 for i := 0 to Length(AClasses) - 1
  do RegisterDspProcessor32(AClasses[i]);
end;

procedure RegisterDspProcessors64(AClasses: array of TDspPersistentClass);
var
  i : Integer;
begin
 for i := 0 to Length(AClasses) - 1
  do RegisterDspProcessor64(AClasses[i]);
end;

end.
