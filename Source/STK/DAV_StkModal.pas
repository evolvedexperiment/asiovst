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
  DAV_Stk, DAV_StkInstrument, DAV_StkEnvelope, DAV_StkBiquad, DAV_StkOnePole,
  DAV_StkLfo, DAV_StkWavePlayer;

const
  CMaxModes = 20;

type
  TStkModal = class(TStkInstrument)
  public
    // Class constructor, taking the desired number of modes to create.
    constructor Create(SampleRate: Single; modes: Integer = 4);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(AFrequency: Single);

    // Set the ARatio and ARadius for a specified mode filter.
    procedure SetRatioAndRadius(AModeIndex: Integer; ARatio, ARadius: Single);

    // Set the master AGain.
    procedure SetMasterGain(AGain: Single);

    // Set the direct AGain.
    procedure SetDirectGain(AGain: Single);

    // Set the AGain for a specified mode filter.
    procedure SetModeGain(AModeIndex: Integer; AGain: Single);

    // Initiate a Strike with the given AAmplitude (0.0 - 1.0).
    procedure Strike(AAmplitude: Single);

    // Damp modes with a given decay factor (0.0 - 1.0).
    procedure Damp(AAmplitude: Single);

    // Start a note with the given AFrequency and AAmplitude.
    procedure NoteOn(AFrequency, AAmplitude: Single);

    // Stop a note with the given AAmplitude (speed of decay).
    procedure NoteOff(AAmplitude: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(number: Integer; Value: Single);

  protected
    FEnvelope       : TEnvelope;
    FWave           : TWavePlayer;
    FFilters        : array[0..CMaxModes - 1] of TBiquad;
    FOnePole        : TOnePole;
    FVibrato        : TLfo;
    FNModes         : Integer;
    FVibratoGain    : Single;
    FMasterGain     : Single;
    FDirectGain     : Single;
    FStickHardness  : Single;
    FStrikePosition : Single;
    FBaseFrequency  : Single;
    FRadii          : array[0..CMaxModes - 1] of Single;
    FRatios         : array[0..CMaxModes - 1] of Single;
  end;

implementation

constructor TStkModal.Create;
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
    FFilters[i] := TBiquad.Create(srate);
    FFilters[i].setEqualGainZeroes;
   end;

  FEnvelope := TEnvelope.Create(srate);
  FOnePole := TOnePole.Create(srate);

  FVibrato := TLFO.Create(srate);

  // Set some default values.
  FVibrato.SetFrequency(6.0);
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
  inherited Destroy;
  FEnvelope.Free;
  FOnePole.Free;
  FVibrato.Free;
  for i := 0 to CMaxModes - 1 do
    FFilters[i].Free;
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

  nyquist := srate * 0.5;
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

procedure TStkModal.SetDirectGain;
begin
  FDirectGain := AGain;
end;

procedure TStkModal.SetModeGain;
begin
  if (AModeIndex < 0) then
    exit
  else if (AModeIndex >= FNModes) then
    exit;
  FFilters[AModeIndex].setGain(AGain);
end;

procedure TStkModal.Strike;
var
  temp, AGain: Single;
  i: Integer;
begin
  AGain := AAmplitude;
  if (AAmplitude < 0.0) then
    AGain := 0.0
  else if (AAmplitude > 1.0) then
    AGain := 1.0;

  FEnvelope.setRate(1.0);
  FEnvelope.setTarget(AGain);
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

procedure TStkModal.NoteOn;
begin
  Strike(AAmplitude);
  SetFrequency(AFrequency);
end;

procedure TStkModal.NoteOff;
begin
  // This calls Damp, but inverts the meaning of AAmplitude (high
  // AAmplitude means fast damping).
  Damp(1.0 - (AAmplitude * 0.03));
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
    FFilters[i].setResonance(temp, FRadii[i] * AAmplitude);
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

  lastOutput := temp2;
  Result := lastOutput;
end;

procedure TStkModal.ControlChange;
begin
end;

end.
