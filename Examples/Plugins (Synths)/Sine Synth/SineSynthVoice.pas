unit SineSynthVoice;

interface

uses
  DAV_VSTModule, DAV_Complex;

{$i Consts.inc}

type
  TSineSynthVoice = class(TObject)
  private
    FAmplitude, FFrequency : Single;
    FSampleRate            : Single;
    FSampleRateReciprocal  : Single;
    FAngle, FPosition      : TComplexDouble;
    FMidiKeyNr, FVelocity  : Integer;
    FVSTModule             : TVSTModule;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Frequency: Single);
  protected
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create(theModule: TVSTModule);
    destructor Destroy; override;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; virtual;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property MidiKeyNr: Integer read FMidiKeyNr write FMidiKeyNr;
    property Velocity: Integer read FVelocity write FVelocity;
  end;

implementation

uses
  SysUtils, DAV_Common, SineSynthModule;

{ TSineSynthVoice }

constructor TSineSynthVoice.Create(theModule: TVSTModule);
begin
 FVSTModule := theModule;
 if theModule.SampleRate = 0
  then SampleRate := 44100
  else SampleRate := theModule.SampleRate;
 FPosition.Re := 0;
 FPosition.Im := -1;
end;

destructor TSineSynthVoice.Destroy;
begin
 inherited;
end;

procedure TSineSynthVoice.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TSineSynthVoice.SetSampleRate(const Value: Single);
begin
 if Value <= 0
  then raise Exception.Create('Samplerate must be positive and larger than 0!');
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SamplerateChanged;
  end;
end;

function TSineSynthVoice.Process: Single;
begin
 result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
 FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
 FPosition.Re := result; result := result * FAmplitude;
end;

procedure TSineSynthVoice.SampleRateChanged;
begin
 FSampleRateReciprocal := 1 / FSampleRate;
end;

procedure TSineSynthVoice.SetFrequency(const Frequency: Single);
begin
 if FFrequency <> Frequency then
  begin
   FFrequency := Frequency;
   FrequencyChanged;
  end;
end;

procedure TSineSynthVoice.NoteOn(Frequency, Amplitude: Single);
begin
 FFrequency := Frequency;
 SetFrequency(Frequency);
 FAmplitude := Amplitude;
end;

procedure TSineSynthVoice.NoteOff;
begin
 FAmplitude := 0;
end;

end.
