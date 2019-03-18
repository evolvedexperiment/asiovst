unit VocoderVoice;

interface

uses
  DAV_VSTModule, DAV_Complex, DAV_SynthUtils;

{$i Consts.inc}

type
  TVocoderVoice = class(TSynthVoice)
  private
    FMidiKeyNr: Integer;
    FFrequency: Single;
    FAmplitude: Single;
    FVSTModule: TVSTModule;
    FAngle, FPosition: TComplex64;
  protected
    procedure FrequencyChanged; virtual;
    procedure SamplerateChanged; override;
  public
    constructor Create(VstModule: TVSTModule);
    destructor Destroy; override;
    procedure SetFrequency(const Value: Single); virtual;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; override;

    property Frequency: Single read FFrequency write SetFrequency;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Types, DAV_Math, VocoderModule;

{ TVocoderVoice }

constructor TVocoderVoice.Create(VstModule: TVSTModule);
begin
  FVSTModule := VstModule;
  if VstModule.SampleRate = 0 then
    SampleRate := 44100
  else
    SampleRate := VstModule.SampleRate;

  FPosition.Re := 0;
  FPosition.Im := -1;
end;

destructor TVocoderVoice.Destroy;
begin
  inherited;
end;

function TVocoderVoice.Process: Single;
begin
  Result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
  FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
  FPosition.Re := Result;

  if Result > 0 then
    Result := FAmplitude * Result - FAmplitude
  else
    Result := FAmplitude * Result + FAmplitude;
  Result := Result + FAmplitude * 0.01 * (random - 0.5)

(*
  if Result > 0 then
    Result := FAmplitude * Result
  else
    Result := -FAmplitude;
*)
end;

procedure TVocoderVoice.FrequencyChanged;
begin
  GetSinCos(2 * Pi * FFrequency * ReciprocalSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TVocoderVoice.SamplerateChanged;
begin
  FrequencyChanged;
end;

procedure TVocoderVoice.SetFrequency(const Value: Single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TVocoderVoice.NoteOn(Frequency, Amplitude: Single);
begin
  FFrequency := Frequency;
  FrequencyChanged;
  FAmplitude := Amplitude;
end;

procedure TVocoderVoice.NoteOff;
begin
  FAmplitude := 0;
end;

end.
