unit DAV_StkModal;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK resonance model instrument.

  This class contains an excitation wavetable, an FEnvelope, an oscillator, and
  N resonances (non-sweeping BiQuad FFilters), where N is set during
  instantiation.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkEnvelope, DAV_StkBiquad,
  DAV_StkOnePole, DAV_StkLfo, DAV_StkWavePlayer;

const
  CMaxModes = 20;

type
  TStkModal = class(TStkControlableInstrument)
  private
    // Set the master AGain.
    procedure SetMasterGain(const AGain: Single);

    // Set the direct AGain.
    procedure SetDirectGain(const Value: Single);

  protected
    FEnvelope       : TStkEnvelope;
    FWave           : TStkWavePlayer;
    FFilters        : array[0..CMaxModes - 1] of TStkBiquad;
    FOnePole        : TStkOnePole;
    FVibrato        : TStkLfo;
    FNModes         : Integer;
    FVibratoGain    : Single;
    FMasterGain     : Single;
    FDirectGain     : Single;
    FStickHardness  : Single;
    FStrikePosition : Single;
    FBaseFrequency  : Single;
    FRadii          : array[0..CMaxModes - 1] of Single;
    FRatios         : array[0..CMaxModes - 1] of Single;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(const AFrequency: Single); override;
  public
    // Class constructor, taking the desired number of modes to create.
    constructor Create(const SampleRate: Single; const Modes: Integer = 4); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the ARatio and ARadius for a specified mode filter.
    procedure SetRatioAndRadius(AModeIndex: Integer; ARatio, ARadius: Single);

    // Set the AGain for a specified mode filter.
    procedure SetModeGain(const AModeIndex: Integer; const Value: Single);

    // Initiate a Strike with the given Amplitude (0.0 - 1.0).
    procedure Strike(const Amplitude: Single);

    // Damp modes with a given decay factor (0.0 - 1.0).
    procedure Damp(const Amplitude: Single);

    // Start a note with the given AFrequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;

    property DirectGain: Single read FDirectGain write SetDirectGain;
    property MasterGain: Single read FMasterGain write SetMasterGain;
  end;

implementation

uses
  SysUtils;

constructor TStkModal.Create(const SampleRate: Single; const Modes: Integer = 4);
var
  i: Integer;
begin
  inherited Create(SampleRate);
  if (FNModes <= 0) then
    FNModes := -FNModes;
  if (FNModes > CMaxModes) then
    FNModes := CMaxModes;
  // We don't make the excitation wave here yet, because we don't know
  // what it's going to be.

  for i := 0 to CMaxModes - 1 do
   begin
    FFilters[i] := TStkBiquad.Create(SampleRate);
    FFilters[i].setEqualGainZeroes;
   end;

  FEnvelope := TStkEnvelope.Create(SampleRate);
  FOnePole := TStkOnePole.Create(SampleRate);

  FVibrato := TStkLfo.Create(SampleRate);

  // Set some default values.
  FVibrato.Frequency := 6.0;
  FVibratoGain := 0.0;
  FDirectGain := 0.0;
  FMasterGain := 1.0;
  FBaseFrequency := 440.0;

  Clear;

  FStickHardness := 0.5;
  FStrikePosition := 0.561;
end;

destructor TStkModal.Destroy;
var
  i: Integer;
begin
 FreeAndNil(FEnvelope);
 FreeAndNil(FOnePole);
 FreeAndNil(FVibrato);
 for i := 0 to CMaxModes - 1 do FreeAndNil(FFilters[i]);
 inherited Destroy;
end;

procedure TStkModal.Clear;
var
  i: Integer;
begin
  FOnePole.Clear;
  for i := 0 to FNModes - 1 do
    FFilters[i].Clear;
end;

procedure TStkModal.SetFrequency;
var
  i: Integer;
begin
  FBaseFrequency := AFrequency;
  for i := 0 to FNModes - 1 do
    SetRatioAndRadius(i, FRatios[i], FRadii[i]);
end;

procedure TStkModal.SetRatioAndRadius;
var
  nyquist, temp: Single;
begin
  if (AModeIndex < 0) then
    exit
  else if (AModeIndex >= FNModes) then
    exit;

  nyquist := SampleRate * 0.5;
  if (ARatio * FBaseFrequency < nyquist) then
    FRatios[AModeIndex] := ARatio
  else
   begin
    temp := ARatio;
    while (temp * FBaseFrequency > nyquist) do
      temp := temp * 0.5;
    FRatios[AModeIndex] := temp;
   end;
  FRadii[AModeIndex] := ARadius;
  if (ARatio < 0) then
    temp := -ARatio
  else
    temp := ARatio * FBaseFrequency;

  FFilters[AModeIndex].setResonance(temp, ARadius);
end;

procedure TStkModal.SetMasterGain;
begin
  FMasterGain := AGain;
end;

procedure TStkModal.SetDirectGain(const Value: Single);
begin
  FDirectGain := Value;
end;

procedure TStkModal.SetModeGain(const AModeIndex: Integer; const Value: Single);
begin
  if not AModeIndex in [0..FNModes]
   then raise Exception.CreateFmt('Mode index out of bounds (%d)', [AModeIndex])
   else FFilters[AModeIndex].Gain := Value;
end;

procedure TStkModal.Strike(const Amplitude: Single);
var
  temp, AGain: Single;
  i: Integer;
begin
  AGain := Amplitude;
  if (Amplitude < 0.0) then
    AGain := 0.0
  else if (Amplitude > 1.0) then
    AGain := 1.0;

  FEnvelope.Rate := 1.0;
  FEnvelope.Target := AGain;
  FOnePole.setPole(1.0 - AGain);
  FEnvelope.Tick;
  FWave.reset;

  for i := 0 to FNModes - 1 do
   begin
    if (FRatios[i] < 0) then
      temp := -FRatios[i]
    else
      temp := FRatios[i] * FBaseFrequency;
    FFilters[i].setResonance(temp, FRadii[i]);
   end;
end;

procedure TStkModal.NoteOn(const Frequency, Amplitude: Single);
begin
  Strike(Amplitude);
  SetFrequency(Frequency);
end;

procedure TStkModal.NoteOff;
begin
  // This calls Damp, but inverts the meaning of Amplitude (high
  // Amplitude means fast damping).
  Damp(1.0 - (Amplitude * 0.03));
end;

procedure TStkModal.Damp;
var
  temp: Single;
  i: Integer;
begin
  for i := 0 to FNModes - 1 do
   begin
    if (FRatios[i] < 0) then
      temp := -FRatios[i]
    else
      temp := FRatios[i] * FBaseFrequency;
    FFilters[i].setResonance(temp, FRadii[i] * Amplitude);
   end;
end;

function TStkModal.Tick: Single;
var
  temp, temp2: Single;
  i: Integer;
begin
  temp := FMasterGain * FOnePole.Tick(FWave.Tick * FEnvelope.Tick);

  temp2 := 0.0;
  for i := 0 to FNModes - 1 do
    temp2 := temp2 + FFilters[i].Tick(temp);

  temp2 := temp2 - temp2 * FDirectGain;
  temp2 := temp2 + FDirectGain * temp;

  if (FVibratoGain <> 0.0) then
   begin
    // Calculate AM and apply to master out
    temp := 1.0 + (FVibrato.Tick * FVibratoGain);
    temp2 := temp * temp2;
   end;

  FLastOutput := temp2;
  Result := FLastOutput;
end;

procedure TStkModal.ControlChange;
begin
  // nothing in here yet!
end;

end.
