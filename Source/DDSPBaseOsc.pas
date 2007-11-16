unit DDSPBaseOsc;

interface

uses DAVDCommon, DAVDComplex, DDspBaseComponent;

type
  TDspBaseOsc = class(TDspBaseComponent)
  protected
    FDCOffset:  single;
    FFrequency: single;
    FAmplitude: single;
    FAngle:     TComplexDouble;
    FPosition:  TComplexDoubleDynArray;

    procedure SetAmplitude(const Value: single); virtual;
    procedure SetDCOffset(const Value: single);  virtual;
    procedure SetFrequency(const Value: single); virtual;

    procedure SampleRateChanged; override;
    procedure FrequencyChanged;  virtual;
    procedure ChannelsChanged;   override; 
    procedure BeforeDestroy;     override;

    procedure Process(var Data: Single; const channel: integer); overload; virtual; abstract;
    procedure Process(var Data: Double; const channel: integer); overload; virtual; abstract;
  public
    procedure Init;  override;
    procedure Reset; override;
  published
    property Amplitude: single read FAmplitude write SetAmplitude; //  0..1
    property DCOffset:  single read FDCOffset  write SetDCOffset;  // -1..1
    property Frequency: single read FFrequency write SetFrequency; //  0..Samplerate
  end;

implementation

{ TDspBaseOsc }

procedure TDspBaseOsc.Init;
begin
  FFrequency   := 440;
  FDCOffset    := 0;
  FAmplitude   := 1;

  fStdProcessS  := Process;
  fStdProcessD  := Process;
  ChannelsChanged;
  FrequencyChanged;
end;

procedure TDspBaseOsc.Reset;
begin
  ChannelsChanged;
  FrequencyChanged;
end;

procedure TDspBaseOsc.BeforeDestroy;
begin
  setlength(FPosition,0);
end;

procedure TDspBaseOsc.SampleRateChanged;
begin
  FrequencyChanged;
  inherited;
end;

procedure TDspBaseOsc.FrequencyChanged;
begin
  GetSinCos(2*Pi*FFrequency/FSampleRate,FAngle.Im,FAngle.Re);
end;

procedure TDspBaseOsc.ChannelsChanged;
var i: integer;
begin
  setlength(FPosition,fChannels);

  for i:=0 to fChannels-1 do
  begin
    FPosition[i].Re := 0;
    FPosition[i].Im := -1;
  end;
end;

procedure TDspBaseOsc.SetAmplitude(const Value: single);
begin
  FAmplitude := Value;
end;

procedure TDspBaseOsc.SetDCOffset(const Value: single);
begin
  FDCOffset := Value;
end;

procedure TDspBaseOsc.SetFrequency(const Value: single);
begin
  if FFrequency<>Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

end.
