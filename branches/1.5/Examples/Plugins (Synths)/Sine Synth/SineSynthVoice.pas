unit SineSynthVoice;

interface

uses
  DAV_VSTModule, DAV_Complex, DAV_SynthUtils, DAV_DspSimpleOscillator;

{$i Consts.inc}

type
  TSineSynthVoice = class(TSynthVoice)
  private
    FOscillator: TSimpleOscillator;
    FVSTModule: TVSTModule;
    FFrequency: Single;
    procedure SetFrequency(const Frequency: Single);
  protected
    procedure FrequencyChanged; virtual;
    procedure SamplerateChanged; override;
  public
    constructor Create(VstModule: TVSTModule);
    destructor Destroy; override;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; virtual;

    property Frequency: Single read FFrequency write SetFrequency;
  end;

implementation

uses
  SysUtils, DAV_Types, SineSynthModule;

{ TSineSynthVoice }

constructor TSineSynthVoice.Create(VstModule: TVSTModule);
begin
  FVSTModule := VstModule;
  FOscillator := TSimpleOscillator.Create;
  if VstModule.SampleRate = 0 then
    SampleRate := 44100
  else
    SampleRate := VstModule.SampleRate;
end;

destructor TSineSynthVoice.Destroy;
begin
  FreeAndNil(FOscillator);
  inherited;
end;

procedure TSineSynthVoice.SetFrequency(const Frequency: Single);
begin
  if FFrequency <> Frequency then
  begin
    FFrequency := Frequency;
    FrequencyChanged;
  end;
end;

procedure TSineSynthVoice.FrequencyChanged;
begin
  FOscillator.Frequency := FFrequency;
end;

procedure TSineSynthVoice.SampleRateChanged;
begin
  FOscillator.SampleRate := SampleRate;
end;

procedure TSineSynthVoice.NoteOn(Frequency, Amplitude: Single);
begin
  FFrequency := Frequency;
  FOscillator.Frequency := FFrequency;
  FOscillator.Amplitude := Amplitude;
end;

procedure TSineSynthVoice.NoteOff;
begin
  FOscillator.Amplitude := 0
end;

function TSineSynthVoice.Process: Single;
begin
  Result := FOscillator.Sine;
  FOscillator.CalculateNextSample;
end;

end.
