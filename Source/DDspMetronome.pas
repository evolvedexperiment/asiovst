unit DDspMetronome;

interface

{$I ASIOVST.INC}

uses
  DAVDCommon, DAVDComplex;

type
  TMetronome = class
  private
    fAngle     : TComplexDouble;
    fPosition  : TComplexDouble;
    fVolume    : Single;
    fBeatPos   : Integer;
    fSamplesPerBeat : Single;
    fSamplesCount   : Single;
    fMetroVolume    : Single;
    fSampleRate: Double;
    fBeatsPerMinute: Double;
    procedure SetSampleRate(const Value: Double);
    procedure SetBeatsPerMinute(const Value: Double);
  public
    constructor Create;
    procedure SetSamplesPerBeat;
    function ProcessSample : Single;
    procedure Reset;
  published
    property BeatsPerMinute : Double read fBeatsPerMinute write SetBeatsPerMinute;
    property Samplerate : Double read fSampleRate write SetSampleRate;
  end;

implementation

{ TMetronome }

constructor TMetronome.Create;
begin
 fSampleRate     := 44100;
 fBeatsPerMinute := 120;
 fMetroVolume    := 1;
 fVolume         := 1;
 SetSamplesPerBeat;
 Reset;
end;

procedure TMetronome.Reset;
begin
 fSamplesCount := 0;
 fPosition.Re  := 1;
 fPosition.Im  := 0;
end;

function TMetronome.ProcessSample: Single;
begin
 result := fPosition.Re * fAngle.Re - fPosition.Im * fAngle.Im;
 fPosition.Im := fPosition.Im * fAngle.Re + fPosition.Re * fAngle.Im;
 fPosition.Re := result;
 if fBeatPos = 0
  then result := 2 * fPosition.Re * fPosition.Re - 1;
 result := fVolume * result * fMetroVolume;
 fMetroVolume  := 0.995 * fMetroVolume;
 fSamplesCount := fSamplesCount + 1;
 if fSamplesCount > fSamplesPerBeat then
  begin
   fMetroVolume  := 1;
   fSamplesCount := fSamplesCount - fSamplesPerBeat;
   fPosition.Re  := 1;
   fPosition.Im  := 0;
   if fBeatPos < 3
    then inc(fBeatPos)
    else fBeatPos := 0;
  end;
end;

procedure TMetronome.SetBeatsPerMinute(const Value: Double);
begin
 if fBeatsPerMinute <> Value then
  begin
   fBeatsPerMinute := Value;
   SetSamplesPerBeat;
  end;
end;

procedure TMetronome.SetSamplesPerBeat;
begin
 fSamplesPerBeat := 60 / fBeatsPerMinute * fSampleRate;
 GetSinCos(2000 * Pi / fSampleRate, fAngle.Im, fAngle.Re);
end;

procedure TMetronome.SetSampleRate(const Value: Double);
begin
 if fSampleRate <> Value then
  begin
   fSampleRate := Value;
   SetSamplesPerBeat;
  end;
end;

end.
