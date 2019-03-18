unit SimpleSamplerVoice;

interface

uses
  DAV_VSTModule, DAV_Complex, DAV_SynthUtils;

{$i Consts.inc}

type
  TSimpleSamplerVoice = class(TSynthVoice)
  private
    FFrequency: Single;
    FAmplitude: Single;
    FVSTModule: TVSTModule;
    FSamplePos: Integer;
    FSampleFrac: Single;
    FSampleInc: Single;
    FMem: array [0..3] of Single;

    FAngle,
    FPosition: TComplex64;
    procedure SetFrequency(const Value: Single);
  protected
    procedure FrequencyChanged; virtual;
    procedure SamplerateChanged; override;
  public
    constructor Create(VstModule: TVSTModule);
    destructor Destroy; override;
    procedure NoteOn(Frequency, Amplitude: Single);
    procedure NoteOff;
    function Process: Single; override;

    property Frequency: Single read FFrequency write SetFrequency;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Math, DAV_Types, DAV_DspInterpolation,
  SimpleSamplerModule;

{ TSimpleSamplerVoice }

constructor TSimpleSamplerVoice.Create(VstModule: TVSTModule);
begin
  FVSTModule   := VstModule;
  if VstModule.SampleRate = 0 then
    SampleRate := 44100
  else
    SampleRate := VstModule.SampleRate;

  FPosition.Re :=  0;
  FPosition.Im := -1;
  FSamplePos :=  0;
  FSampleFrac :=  0;
  FSampleInc :=  0;
end;

destructor TSimpleSamplerVoice.Destroy;
begin
  inherited;
end;

function TSimpleSamplerVoice.Process: Single;
begin
  if TVSTSSModule(FVSTModule).SampleLength <= 0 then
  begin
    Result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
    FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
    FPosition.Re := Result; Result := Result * FAmplitude;
  end
  else
  begin
    Result := FAmplitude * Hermite32_asm(FSampleFrac, @FMem[0]);
    FSampleFrac := FSampleFrac + FSampleInc;
    while FSampleFrac >= 1 do
    begin
     inc(FSamplePos);
     if FSamplePos >= TVSTSSModule(FVSTModule).SampleLength then
       FSamplePos := 0;
     FSampleFrac := FSampleFrac - 1;
     Move(FMem[1], FMem[0], 12);
     FMem[3] := TVSTSSModule(FVSTModule).Sample[FSamplePos];
    end;
  end;
end;

procedure TSimpleSamplerVoice.SetFrequency(const Value: Single);
begin
  if (Value <= 0) then
    raise Exception.Create('Frequency must be larger than 0!');
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    FrequencyChanged;
  end;
end;

procedure TSimpleSamplerVoice.FrequencyChanged;
begin
  FSampleInc := Frequency / 440;
  GetSinCos(2 * Pi * FFrequency * ReciprocalSampleRate, FAngle.Im, FAngle.Re);
end;

procedure TSimpleSamplerVoice.SamplerateChanged;
begin
  FrequencyChanged;
end;

procedure TSimpleSamplerVoice.NoteOn(Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  FAmplitude := Amplitude;
end;

procedure TSimpleSamplerVoice.NoteOff;
begin
  FAmplitude := 0;
end;

end.
