unit DDspEnvelope;

interface
               
{$I ASIOVST.inc}

uses DDspBaseComponent, DAVDCommon;

type
  TDspEnvelopeInputProcessing = (eipIgnoreInput, eipMultiply, eipAdd);
  TDspEnvelope = class(TDspBaseComponent)
  private
  protected
    FAttack: single;
    FDecay: single;
    FSustain: single;
    FRelease: single;
    FAttackSamples: Integer;
    FDecaySamples: Integer;
    FMaxAmplitude: single;

    FInverted: Boolean;
    FInputProcessing: TDspEnvelopeInputProcessing;
    fInternalCounter: array of integer;
    fNoteOff: Boolean;

    procedure SampleRateChanged; override;
    procedure BeforeDestroy; override;
    procedure Loaded; override;
    procedure SetAttack(const Value: single);
    procedure SetDecay(const Value: single);
    procedure SetSustain(const Value: single);
    procedure SetRelease(const Value: single);
    procedure SetMaxAmplitude(const Value: single);

    procedure SetInputProcessing(const Value: TDspEnvelopeInputProcessing);
    procedure SetInverted(const Value: Boolean);

    procedure TimeToSamples;

    procedure ChannelsChanged;   override;
    procedure Process(var Data: Single; const channel: integer); overload;
    procedure Process(var Data: Double; const channel: integer); overload;
  public
    procedure Init;  override;
    procedure Reset; override;
    procedure NoteOff; override;
    procedure NoteOn;
  published
    property Attack:  single read FAttack write SetAttack; // time in seconds
    property Decay:   single read FDecay write SetDecay; // time in seconds
    property Sustain: single read FSustain write SetSustain; // 0..1
    property Release: single read FRelease write SetRelease; // time in seconds

    property MaxAmplitude: single read FMaxAmplitude write SetMaxAmplitude; // -unlimited..unlimited

    property Inverted: Boolean read FInverted write SetInverted default false;
    property InputProcessing: TDspEnvelopeInputProcessing read FInputProcessing write SetInputProcessing default eipMultiply;
  end;

implementation


uses Math, dialogs;

{ TDspEnvelope }

procedure TDspEnvelope.Init;
begin         
  fStdProcessS  := Process;
  fStdProcessD  := Process;
  
  FInverted := false;
  FInputProcessing:=eipMultiply;
  fAttack:=0;
  fDecay:=0;
  fSustain:=0;
  fRelease:=0;
  fMaxAmplitude:=0;
  fNoteOff := false;
  Reset;
  TimeToSamples;
end; 

procedure TDspEnvelope.Loaded;
begin
  Reset;
  TimeToSamples;
end;

procedure TDspEnvelope.Reset;
begin
  ChannelsChanged;
  SampleRateChanged;
end;   

procedure TDspEnvelope.NoteOff;
begin
  fNoteOff:=true;
  ChannelsChanged;
end;

procedure TDspEnvelope.NoteOn;
begin
  fNoteOff:=false;
  ChannelsChanged;
end;

procedure TDspEnvelope.BeforeDestroy;
begin
  SetTrailingSamples(0);
  SetLength(fInternalCounter, 0);
end;

procedure TDspEnvelope.ChannelsChanged;
begin
  SetLength(fInternalCounter, fChannels);

  FillChar(fInternalCounter[0], fChannels * SizeOf(Single), 0);
end;

procedure TDspEnvelope.SampleRateChanged;
begin
  TimeToSamples;
end;

procedure TDspEnvelope.TimeToSamples;
begin
  FAttackSamples  := ceil(FAttack  * fSampleRate);
  FDecaySamples   := ceil(FDecay   * fSampleRate);
  SetTrailingSamples(ceil(FRelease * fSampleRate));
end;

procedure TDspEnvelope.SetAttack(const Value: single);
begin
  if FAttack<>Value then
  begin
    FAttack := Value;
    TimeToSamples;
  end;
end;

procedure TDspEnvelope.SetDecay(const Value: single);
begin
  if FDecay<>Value then
  begin
    FDecay := Value;
    TimeToSamples;
  end;
end;

procedure TDspEnvelope.SetSustain(const Value: single);
begin
  FSustain := Value;
end;

procedure TDspEnvelope.SetRelease(const Value: single);
begin
  if FRelease<>Value then
  begin
    FRelease := Value;
    TimeToSamples;
  end;
end;


procedure TDspEnvelope.SetMaxAmplitude(const Value: single);
begin
  FMaxAmplitude := Value;
end;

procedure TDspEnvelope.SetInputProcessing(const Value: TDspEnvelopeInputProcessing);
begin
  FInputProcessing := Value;
end;

procedure TDspEnvelope.SetInverted(const Value: Boolean);
begin
  FInverted := Value;
end;

procedure TDspEnvelope.Process(var Data: Single; const channel: integer);
var amp: single;
begin
  if fNoteOff then
  begin
    // Release
    amp := FSustain * (1-fInternalCounter[channel]/fTrailingSamples);

  end else if fInternalCounter[channel]>FDecaySamples+FAttackSamples then
  begin
    // Sustain
    amp := FSustain;
  end else if fInternalCounter[channel]>FAttackSamples then
  begin
    // Decay
    if FSustain=1 then
      amp := 1
    else begin
      amp := 1-(fInternalCounter[channel]-FAttackSamples)/FDecaySamples;
      amp := amp*(1-FSustain)+FSustain;
    end;
  end else begin
    // Attack
    amp := fInternalCounter[channel]/FAttackSamples;
  end;


  if FInverted then
    amp := MaxAmplitude*(1-amp)
  else
    amp := amp*MaxAmplitude;

  Data := Data*amp;
  inc(fInternalCounter[channel]);
end;

procedure TDspEnvelope.Process(var Data: Double; const channel: integer);
var amp: double;
begin
  if fNoteOff then
  begin
    // Release
    amp := FSustain * (1-fInternalCounter[channel]/fTrailingSamples);

  end else if fInternalCounter[channel]>FDecaySamples+FAttackSamples then
  begin
    // Sustain
    amp := FSustain;
  end else if fInternalCounter[channel]>FAttackSamples then
  begin
    // Decay
    if FSustain=1 then
      amp := 1
    else begin
      amp := 1-(fInternalCounter[channel]-FAttackSamples)/FDecaySamples;
      amp := amp*(1-FSustain)+FSustain;
    end;
  end else begin
    // Attack
    amp := fInternalCounter[channel]/FAttackSamples;
  end;


  if FInverted then
    amp := MaxAmplitude*(1-amp)
  else
    amp := amp*MaxAmplitude;

  Data := Data*amp;
  inc(fInternalCounter[channel]);
end;

end.
