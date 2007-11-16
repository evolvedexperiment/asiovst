unit DDspVoice;

interface

{$I ASIOVST.INC}

uses Classes, DAVDProcessingComponent;

type
  TDspVoiceInfo = class(TObject)
    NoteNr: Byte;
    NoteOnOffset: Longint;
    Velocity: Byte;
    Detune: Byte;
  end;

  TDspVoiceTrailingType = (vttAutomatic, vttManually);
  TDspVoice = class(TDataModule)
  protected
    fBypass:          Boolean;
    fEnabled:         Boolean;
    fSampleRate:      Single;
    fChannels:        Integer;

    {fTrailingSamples: Integer;
    fTrailingType:    TDspVoiceTrailingType;
    fIsVoiceOn:       Boolean;
    fIsAlive:         Boolean;
    fOffTrailingCounter: Integer;  }
    fVoiceInfo:       TDspVoiceInfo; 

    procedure SetBypass(const Value: Boolean);    virtual;
    procedure SetEnabled(const Value: Boolean);   virtual;
    procedure SetSampleRate(const Value: Single); virtual;
    procedure SetChannels(const Value: Integer);  virtual;
    {
    procedure SetTrailingSamples(const Value: Integer);  virtual;
    procedure SetTrailingType(const Value: TDspVoiceTrailingType);  virtual; }
  public
    constructor Create(AOwner: TComponent); reintroduce; overload;
    constructor Create(AOwner: TComponent; VoiceInfo: TDspVoiceInfo); reintroduce; overload; 
    destructor Destroy; override;

    procedure Init;  virtual;
    procedure Reset; virtual;

   { procedure ProcessMidiEvent(MidiEvent: TAVDMidiEvent); virtual;

    procedure VoiceOff; virtual;
    procedure UpdateTrailingSamples; virtual;

    procedure ProcessS(var Data: Single; const channel: integer); virtual;
    procedure ProcessD(var Data: Double; const channel: integer); virtual;
    procedure ProcessSA(var ProcessBuffer: TAVDSingleDynArray; const channel: integer); virtual;
    procedure ProcessDA(var ProcessBuffer: TAVDDoubleDynArray; const channel: integer); virtual;
    procedure ProcessSAA(var ProcessBuffer: TAVDArrayOfSingleDynArray); virtual;
    procedure ProcessDAA(var ProcessBuffer: TAVDArrayOfDoubleDynArray); virtual;

    property IsAlive:    Boolean read fIsAlive;
    property IsVoiceOn:  Boolean read fIsVoiceOn;  }
    property VoiceInfo: TDspVoiceInfo read fVoiceInfo;

    property Enabled:    Boolean read fEnabled    write SetEnabled    default true;
    property Bypass:     Boolean read fBypass     write SetBypass     default true;
    property Channels:   Integer read fChannels   write SetChannels   default 2;
    property SampleRate: Single  read fSampleRate write SetSampleRate;
  published
    
   {
    property TrailingType: TDspVoiceTrailingType read fTrailingType write SetTrailingType;
    property TrailingSamples: Integer read fTrailingSamples write SetTrailingSamples; }
  end;

implementation

uses Forms;

{ TDspVoice }

constructor TDspVoice.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 if (ClassType <> TDspVoice) and not (csDesigning in ComponentState) then
  try
   if not InitInheritedComponent(Self, TDataModule) then
     raise EResNotFound.CreateFmt('Resource %s not found', [ClassName]);
   try
    if Assigned(OnCreate) and OldCreateOrder then OnCreate(Self);
   except
    Forms.Application.HandleException(Self);
   end;
  except
  end;
end;

constructor TDspVoice.Create(AOwner: TComponent; VoiceInfo: TDspVoiceInfo);
begin
  inherited Create(AOwner);
  fVoiceInfo:=VoiceInfo;
end;

destructor TDspVoice.Destroy;
begin
  fVoiceInfo.free;
  inherited;
end;

procedure TDspVoice.Init;
begin

end;

procedure TDspVoice.Reset;
begin

end;

procedure TDspVoice.SetBypass(const Value: Boolean);
begin

end;

procedure TDspVoice.SetChannels(const Value: Integer);
begin

end;

procedure TDspVoice.SetEnabled(const Value: Boolean);
begin

end;

procedure TDspVoice.SetSampleRate(const Value: Single);
begin

end;

end.
