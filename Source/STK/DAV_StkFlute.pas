unit DAV_StkFlute;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK TFlute physical model class.

   This class implements a simple TFlute physical model, as discussed by
   Karjalainen, Smith, Waryznyk, etc.  The jet model uses a polynomial, a la
   Cook.

   This is a digital waveguide model, making its use possibly subject to
   patents held by Stanford University, Yamaha, and others.

   Control Change Numbers:
     - Jet Delay := 2
     - FNoise Gain := 4
     - FVibrato AFrequency := 11
     - FVibrato Gain := 1
     - Breath Pressure := 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkLfo, DAV_StkInstrument, DAV_StkJetTable, DAV_StkDelayl,
  DAV_StkOnePole, DAV_StkPoleZero, DAV_StkNoise, DAV_StkAdsr;

type
  TFlute = class(TInstrmnt)
  protected
    FJetDelay      : TDelayl;
    FBoreDelay     : TDelayl;
    FJetTable      : TJetTabl;
    FFilter        : TOnePole;
    FDCBlock       : TPoleZero;
    FNoise         : TNoise;
    FAdsr          : TADSR;
    FVibrato       : TLFO;
    FLength        : Integer;
    FLastFrequency : Single;
    FMaxPressure   : Single;
    FJetReflection : Single;
    FEndReflection : Single;
    FNoiseGain     : Single;
    FVibratoGain   : Single;
    FOutputGain    : Single;
    FJetRatio      : Single;
  public
    // Class constructor, taking the lowest desired playing AFrequency.
    constructor Create(sr, lowestFrequency: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(AFrequency: Single);

    // Set the reflection ACoefficient for the jet delay (-1.0 - 1.0).
    procedure SetJetReflection(ACoefficient: Single);

    // Set the reflection ACoefficient for the air column delay (-1.0 - 1.0).
    procedure SetEndReflection(ACoefficient: Single);

    // Set the FLength of the jet delay in terms of a ratio of jet delay to air column delay lengths.
    procedure SetJetDelay(aRatio: Single);

    // Apply breath velocity to instrument with given amplitude and rate of increase.
    procedure StartBlowing(amplitude, rate: Single);

    // Decrease breath velocity with given rate of decrease.
    procedure StopBlowing(rate: Single);

    // Start a note with the given AFrequency and amplitude.
    procedure NoteOn(AFrequency, amplitude: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(amplitude: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(number: integer; Value: Single);

  end;

implementation

constructor TFlute.Create;
begin
  inherited Create(sr);
  FLength := round(srate / lowestFrequency + 1);
  FBoreDelay := TDelayL.Create(srate, 100.0, FLength);
  FLength := FLength shr 1;
  FJetDelay := TDelayL.Create(srate, 49.0, FLength);
  FJetTable := TJetTabl.Create(srate);
  FFilter := TOnePole.Create(srate);
  FDCBlock := TPoleZero.Create(srate);
  FDCBlock.setBlockZero(0.99);
  FNoise := TNoise.Create(srate);
  FAdsr := TADSR.Create(srate);

  FVibrato := TLFO.Create(srate);
  FVibrato.SetFrequency(5.925);

  Clear;

  FFilter.setPole(0.7 - (0.1 * 22050.0 / srate));
  FFilter.setGain(-1.0);
  FAdsr.setAllTimes(0.005, 0.01, 0.8, 0.010);
  FEndReflection := 0.5;
  FJetReflection := 0.5;
  FNoiseGain := 0.15;             // Breath pressure random component.
  FVibratoGain := 0.05; // Breath periodic FVibrato component.
  FJetRatio := 0.32;

  FMaxPressure := 0.0;
  FLastFrequency := 220.0;
end;

destructor TFlute.Destroy;
begin
  inherited Destroy;
  FJetDelay.Free;
  FBoreDelay.Free;
  FJetTable.Free;
  FFilter.Free;
  FDCBlock.Free;
  FNoise.Free;
  FAdsr.Free;
  FVibrato.Free;
end;

procedure TFlute.Clear;
begin
  FJetDelay.Clear;
  FBoreDelay.Clear;
  FFilter.Clear;
  FDCBlock.Clear;
end;

procedure TFlute.SetFrequency;
var
  delay: Single;
begin
  FLastFrequency := AFrequency;
  if (AFrequency <= 0.0) then
    FLastFrequency := 220.0;

  // We're overblowing here.
  FLastFrequency := FLastFrequency * 0.66666;
  // Delay := FLength - approximate FFilter delay.
  delay := srate / FLastFrequency - 2.0;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;

  FBoreDelay.setDelay(delay);
  FJetDelay.setDelay(delay * FJetRatio);
end;

procedure TFlute.StartBlowing;
begin
  FAdsr.setAttackRate(rate);
  FMaxPressure := amplitude / 0.8;
  FAdsr.keyOn;
end;

procedure TFlute.StopBlowing;
begin
  FAdsr.setReleaseRate(rate);
  FAdsr.keyOff;
end;

procedure TFlute.NoteOn;
begin
  SetFrequency(AFrequency);
  StartBlowing(1.1 + (amplitude * 0.20), amplitude * 0.02);
  FOutputGain := amplitude + 0.001;
end;

procedure TFlute.NoteOff;
begin
  StopBlowing(amplitude * 0.02);
end;

procedure TFlute.SetJetReflection;
begin
  FJetReflection := ACoefficient;
end;

procedure TFlute.SetEndReflection;
begin
  FEndReflection := ACoefficient;
end;

procedure TFlute.SetJetDelay;
var
  temp: Single;
begin
  // Delay := FLength - approximate FFilter delay.
  temp := srate / FLastFrequency - 2.0;
  FJetRatio := aRatio;
  FJetDelay.setDelay(temp * aRatio); // Scaled by ratio.
end;

function TFlute.Tick: Single;
var
  temp, pressureDiff, breathPressure: Single;
begin
  // Calculate the breath pressure (envelope + FNoise + FVibrato)
  breathPressure := FMaxPressure * FAdsr.Tick;
  breathPressure := breathpressure + breathPressure * FNoiseGain * FNoise.Tick;
  breathPressure := breathpressure + breathPressure * FVibratoGain *
    FVibrato.Tick;

  temp := FFilter.Tick(FBoreDelay.lastOut);
  temp := FDCBlock.Tick(temp); // Block DC on reflection.

  pressureDiff := breathPressure - (FJetReflection * temp);
  pressureDiff := FJetDelay.Tick(pressureDiff);
  pressureDiff := FJetTable.Tick(pressureDiff) + (FEndReflection * temp);
  lastOutput := 0.3 * FBoreDelay.Tick(pressureDiff);

  lastOutput := lastoutput * FOutputGain;
  Result := lastOutput;

end;

procedure TFlute.ControlChange;
var
  norm: Single;
begin
  norm := Value; // * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_JetDelay_) then // 2
    SetJetDelay((0.08 + (0.48 * norm)))
  else if (number = __SK_NoiseLevel_) then // 4
    FNoiseGain := (norm * 0.4)
  else if (number = __SK_ModFrequency_) then // 11
    FVibrato.SetFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    FVibratoGain := (norm * 0.4)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    FAdsr.setTarget(norm);
end;

end.
