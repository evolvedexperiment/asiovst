unit DAV_StkSaxofony;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK faux conical bore reed instrument class.

  This class implements a "hybrid" digital waveguide instrument that can
  generate a variety of wind-like sounds. It has also been referred to as the
  "blowed string" model. The waveguide section is essentially that of a string,
  with one rigid and one lossy termination. The non-linear function is a reed
  table. The string can be "blown" at any point between the terminations,
  though just as with strings, it is impossible to excite the system at either
  end. If the excitation is placed at the string mid-point, the sound is that
  of a clarinet.  At points closer to the "bridge", the sound is closer to that
  of a saxophone. See Scavone (2002) for more details.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Reed Stiffness = 2
    - Reed Aperture = 26
    - FNoise Gain = 4
    - Blow FPosition = 11
    - FVibrato AFrequency = 29
    - FVibrato Gain = 1
    - Breath Pressure = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkDelayl, DAV_StkReedTable, DAV_StkOneZero,
  DAV_StkEnvelope, DAV_StkNoise, DAV_StkLfo;

type
  TStkSaxofony = class(TStkInstrument)
  protected
    FDelays: array[0..1] of TDelayl;
    FReedTable: TReedTable;
    FFilter: TOnezero;
    FEnvelope: TEnvelope;
    FNoise: TNoise;
    FVibrato: TLfo;
    FLength: longint;
    FOutputGain, FNoiseGain, FVibratoGain, FPosition: Single;
  public
    // Class constructor, taking the lowest desired playing AFrequency.
    constructor Create(SampleRate, LowestFrequency: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(AFrequency: Single);

    // Set the "blowing" FPosition between the air column terminations (0.0 - 1.0).
    procedure SetBlowPosition(APosition: Single);

    // Apply breath pressure to instrument with given Amplitude and Rate of increase.
    procedure StartBlowing(Amplitude, Rate: Single);

    // Decrease breath pressure with given Rate of decrease.
    procedure StopBlowing(Rate: Single);

    // Start a note with the given AFrequency and Amplitude.
    procedure NoteOn(AFrequency, Amplitude: Single);

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(Amplitude: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: Single);
  end;

implementation

constructor TSaxofony.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  // Initialize blowing FPosition to 0.2 of FLength / 2.
  FPosition := 0.2;
  FDelays[0] := TDelayl.Create(SampleRate, (1.0 - FPosition) * (FLength shr 1), FLength);
  FDelays[1] := TDelayl.Create(SampleRate, FPosition * (FLength shr 1), FLength);

  FReedTable := TReedTable.Create(SampleRate);
  FReedTable.setOffset(0.7);
  FReedTable.setSlope(0.3);
  FFilter := TOnezero.Create(SampleRate);
  FEnvelope := TEnvelope.Create(SampleRate);
  FNoise := TNoise.Create(SampleRate);

  FVibrato := TLfo.Create(SampleRate);
  FVibrato.SetFrequency(5.735);

  FOutputGain := 0.3;
  FNoiseGain := 0.2;
  FVibratoGain := 0.1;
end;

destructor TSaxofony.Destroy;
begin
  inherited Destroy;
  FDelays[0].Free;
  FDelays[1].Free;
  FReedTable.Free;
  FFilter.Free;
  FEnvelope.Free;
  FNoise.Free;
  FVibrato.Free;
end;

procedure TSaxofony.Clear;
begin
  FDelays[0].Clear;
  FDelays[1].Clear;
  FFilter.Tick(0.0);
end;

procedure TSaxofony.SetFrequency;
var
  delay, freakency: Single;
begin
  freakency := AFrequency;
  if (AFrequency <= 0.0) then
    freakency := 220.0;

  delay := (SampleRate / freakency) - 3.0;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;

  FDelays[0].setDelay((1.0 - FPosition) * delay);
  FDelays[1].setDelay(FPosition * delay);
end;

procedure TSaxofony.SetBlowPosition(APosition: Single);
var
  total_delay: Single;
begin
  if (FPosition = APosition) then
    exit;
  if (APosition < 0.0) then
    FPosition := 0.0
  else if (APosition > 1.0) then
    FPosition := 1.0
  else
    FPosition := APosition;

  total_delay := FDelays[0].getDelay;
  total_delay := total_delay + FDelays[1].getDelay;

  FDelays[0].setDelay((1.0 - FPosition) * total_delay);
  FDelays[1].setDelay(FPosition * total_delay);
end;

procedure TSaxofony.StartBlowing;
begin
  FEnvelope.setRate(Rate);
  FEnvelope.setTarget(Amplitude);
end;

procedure TSaxofony.StopBlowing;
begin
  FEnvelope.setRate(Rate);
  FEnvelope.setTarget(0.0);
end;

procedure TSaxofony.NoteOn;
begin
  SetFrequency(AFrequency);
  StartBlowing(0.55 + (Amplitude * 0.30), Amplitude * 0.005);
  FOutputGain := Amplitude + 0.001;
end;

procedure TSaxofony.NoteOff(Amplitude: Single);
begin
  StopBlowing(Amplitude * 0.01);
end;

function TSaxofony.Tick: Single;
var
  pressureDiff, breathPressure, temp: Single;
begin
  // Calculate the breath pressure (FEnvelope + FNoise + FVibrato)
  breathPressure := FEnvelope.Tick;
  breathPressure := breathPressure + breathPressure * FNoiseGain * FNoise.Tick;
  breathPressure := breathPressure + breathPressure * FVibratoGain *
    FVibrato.Tick;

  temp := -0.95 * FFilter.Tick(FDelays[0].lastOut);
  lastOutput := temp - FDelays[1].lastOut;
  pressureDiff := breathPressure - lastOutput;
  FDelays[1].Tick(temp);
  FDelays[0].Tick(breathPressure - (pressureDiff *
    FReedTable.Tick(pressureDiff)) - temp);

  lastOutput := lastOutput * FOutputGain;
  Result := lastOutput;
end;

procedure TSaxofony.controlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_ReedStiffness_) then // 2
    FReedTable.setSlope(0.1 + (0.4 * norm))
  else if (number = __SK_NoiseLevel_) then // 4
    FNoiseGain := (norm * 0.4)
  else if (number = 29) then // 29
    FVibrato.SetFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    FVibratoGain := (norm * 0.5)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    FEnvelope.setValue(norm)
  else if (number = 11) then // 11
    SetBlowPosition(norm)
  else if (number = 26) then // reed table offset
    FReedTable.setOffset(0.4 + (norm * 0.6));
end;

end.
