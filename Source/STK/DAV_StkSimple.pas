unit DAV_StkSimple;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK wavetable/FNoise instrument.

  This class combines a looped wave, a FNoise source, a FBiQuad resonance FFilter,
  a one-pole FFilter, and an FADSR envelope to create some interesting sounds.

  Control Change Numbers:
    - FFilter Pole Position = 2
    - FNoise/Pitched Cross-Fade = 4
    - Envelope Rate = 11
    - Gain = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkAdsr, DAV_StkWavePlayer, DAV_StkOnePole,
  DAV_StkBiquad, DAV_StkNoise;

type
  TStkSimple = class(TStkInstrmnt)
  protected
    FADSR          : TAdsr;
    FLoop          : TWavePlayer;
    FFilter        : TOnePole;
    FBiQuad        : TBiquad;
    FNoise         : TNoise;
    FBaseFrequency : Single;
    FLoopGain      : Single;
  public
    // Class constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: Single);

    // Start envelope toward "on" target.
    procedure keyOn;

    // Start envelope toward "off" target.
    procedure keyOff;

    // Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: Single);

    // Compute one output sample.
    function tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: Integer; Value: Single);
  end;

implementation

constructor TStkSimple.Create;
begin
  inherited Create(SampleRate);
  FADSR := TAdsr.Create(srate);
  FBaseFrequency := 440.0;

  FLoop := TWavePlayer.Create(srate, 'impuls10.raw');

  FFilter := TOnePole.Create(srate, 0.5);
  FNoise := TNoise.Create(srate);
  FBiQuad := TBiquad.Create(srate);

  setFrequency(FBaseFrequency);
  FLoopGain := 0.5;
end;

destructor TStkSimple.Destroy;
begin
  inherited Destroy;
  FADSR.Free;
  FLoop.Free;
  FFilter.Free;
  FBiQuad.Free;
end;

procedure TStkSimple.keyOn;
begin
  FADSR.keyOn;
end;

procedure TStkSimple.keyOff;
begin
  FADSR.keyOff;
end;

procedure TStkSimple.noteOn;
begin
  keyOn;
  setFrequency(frequency);
  FFilter.setGain(amplitude);
end;

procedure TStkSimple.noteOff;
begin
  keyOff;
end;

procedure TStkSimple.setFrequency;
begin
  FBiQuad.setResonance(frequency, 0.98, True);
  FLoop.setFrequency(frequency);
end;

function TStkSimple.tick: Single;
begin
  lastOutput := FLoopGain * FLoop.tick;
  FBiQuad.tick(FNoise.tick);
  lastOutput := lastoutput + (1.0 - FLoopGain) * FBiQuad.lastOut;
  lastOutput := FFilter.tick(lastOutput);
  lastOutput := lastoutput * FADSR.tick;
  Result := lastOutput;
end;

procedure TStkSimple.controlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then // 2
    FFilter.setPole(0.99 * (1.0 - (norm * 2.0)))
  else if (number = __SK_NoiseLevel_) then // 4
    FLoopGain := norm
  else if (number = __SK_ModFrequency_) then
   begin // 11
    norm := norm / (0.2 * srate);
    FADSR.setAttackRate(norm);
    FADSR.setDecayRate(norm);
    FADSR.setReleaseRate(norm);
   end
  else if (number = __SK_AfterTouch_Cont_) then // 128
    FADSR.setTarget(norm);
end;

end.
