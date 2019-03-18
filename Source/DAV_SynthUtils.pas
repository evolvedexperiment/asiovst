unit DAV_SynthUtils;

interface

type
  TBasicSynthVoice = class(TObject);

  TSampleRateSynthVoice = class(TBasicSynthVoice)
  private
    FSampleRate: Single;
    FReciprocalSampleRate: Single;
    procedure SetSampleRate(const Value: Single);
  protected
    procedure SamplerateChanged; virtual;
    property ReciprocalSampleRate: Single read FReciprocalSampleRate;
  public
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TSynthVoice = class(TSampleRateSynthVoice)
  private
    FMidiKey: Integer;
    FVelocity: Integer;
  public
    property MidiKey: Integer read FMidiKey write FMidiKey;
    property Velocity: Integer read FVelocity write FVelocity;
  end;

function KeyToNote(Key: Integer): Integer;

implementation

uses
  SysUtils;

function KeyToNote(Key: Integer): Integer;
begin
  case Key of
    89:
      Result := 60;
    83:
      Result := 61;
    88:
      Result := 62;
    68:
      Result := 63;
    67:
      Result := 64;
    86:
      Result := 65;
    71:
      Result := 66;
    66:
      Result := 67;
    72:
      Result := 68;
    78:
      Result := 69;
    74:
      Result := 70;
    77:
      Result := 71;
    188:
      Result := 72;
    81:
      Result := 72;
    87:
      Result := 74;
    69:
      Result := 76;
    82:
      Result := 77;
    else
      Result := -1;
  end;
end;

{ TSampleRateSynthVoice }

procedure TSampleRateSynthVoice.SamplerateChanged;
begin
  Assert(SampleRate <> 0);
  FReciprocalSampleRate := 1 / Samplerate;
end;

procedure TSampleRateSynthVoice.SetSampleRate(const Value: Single);
begin
  if Value <= 0 then
    raise Exception.Create('Samplerate must be larger than 0!');
  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    SampleRateChanged;
  end;
end;


end.

