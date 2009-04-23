unit DAV_DspMetronome;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TMetronome = class(TDspObject)
  private
    FAngle          : TComplexDouble;
    FPosition       : TComplexDouble;
    FDecayFactor    : Single;
    FVolume         : Single;
    FBeatPos        : Integer;
    FSamplesPerBeat : Single;
    FSamplesCount   : Single;
    FMetroVolume    : Single;
    FSampleRate     : Double;
    FBeatsPerMinute : Double;
    procedure SetSampleRate(const Value: Double);
    procedure SetBeatsPerMinute(const Value: Double);
  public
    constructor Create;
    procedure SetSamplesPerBeat;
    function ProcessSample: Single;
    procedure Reset;
  published
    property BeatsPerMinute: Double read FBeatsPerMinute write SetBeatsPerMinute;
    property Samplerate: Double read FSampleRate write SetSampleRate;
  end;

implementation

{ TMetronome }

constructor TMetronome.Create;
begin
  FSampleRate := 44100;
  FBeatsPerMinute := 120;
  FMetroVolume := 1;
  FDecayFactor := 0.995;
  FVolume := 1;
  SetSamplesPerBeat;
  Reset;
end;

procedure TMetronome.Reset;
begin
  FSamplesCount := 0;
  FPosition.Re := 1;
  FPosition.Im := 0;
end;

function TMetronome.ProcessSample: Single;
begin
  Result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
  FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
  FPosition.Re := Result;
  if FBeatPos = 0 then Result := 2 * FPosition.Re * FPosition.Re - 1;
  Result := FVolume * Result * FMetroVolume;
  FMetroVolume := FDecayFactor * FMetroVolume;
  FSamplesCount := FSamplesCount + 1;
  if FSamplesCount > FSamplesPerBeat then
   begin
    FMetroVolume := 1;
    FSamplesCount := FSamplesCount - FSamplesPerBeat;
    FPosition.Re := 1;
    FPosition.Im := 0;
    if FBeatPos < 3
     then Inc(FBeatPos)
     else FBeatPos := 0;
   end;
end;

procedure TMetronome.SetBeatsPerMinute(const Value: Double);
begin
  if FBeatsPerMinute <> Value then
   begin
    FBeatsPerMinute := Value;
    SetSamplesPerBeat;
   end;
end;

procedure TMetronome.SetSamplesPerBeat;
begin
  FSamplesPerBeat := 60 / FBeatsPerMinute * FSampleRate;
  GetSinCos(2000 * Pi / FSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TMetronome.SetSampleRate(const Value: Double);
begin
  if FSampleRate <> Value then
   begin
    FSampleRate := Value;
    FDecayFactor := 0.995; // need to be samplerate independent in the future!
    SetSamplesPerBeat;
   end;
end;

end.
