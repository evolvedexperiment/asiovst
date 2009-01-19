unit DAV_StkResonate;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK FNoise driven formant FFilter.

  This instrument contains a FNoise source, which excites a biquad resonance
  FFilter, with volume controlled by an ADSR.

  Control Change Numbers:
    - Resonance Frequency (0-Nyquist) := 2
    - Pole Radii := 4
    - Notch Frequency (0-Nyquist) := 11
    - Zero Radii := 1
    - Envelope Gain := 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkAdsr, DAV_StkBiquad, DAV_StkNoise;

type
  TStkResonate = class(TStkInstrmnt)
  protected
    ADSR: TAdsr;
    FFilter: TBiquad;
    FNoise: TNoise;
    FPoleFrequency, FPoleRadius, FZeroFrequency, FZeroRadius: Single;
  public
    constructor Create(SampleRate: Single); override;
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the FFilter for a resonance at the given frequency (Hz) and radius.
    procedure setResonance(frequency, radius: Single);

    // Set the FFilter for a notch at the given frequency (Hz) and radius.
    procedure setNotch(frequency, radius: Single);

    // Set the FFilter zero coefficients for contant resonance gain.
    procedure setEqualGainZeroes;

    // Initiate the envelope with a key-on event.
    procedure keyOn;

    // Signal a key-off event to the envelope.
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

constructor TStkResonate.Create;
begin
  inherited Create(SampleRate);
  adsr := TAdsr.Create(srate);
  FNoise := TNoise.Create(srate);
  FFilter := TBiquad.Create(srate);
  FPoleFrequency := 4000.0;
  FPoleRadius := 0.95;
  // Set the FFilter parameters.
  FFilter.setResonance(FPoleFrequency, FPoleRadius, True);
  FZeroFrequency := 0.0;
  FZeroRadius := 0.0;
end;

destructor TStkResonate.Destroy;
begin
  inherited Destroy;
  adsr.Free;
  FFilter.Free;
  FNoise.Free;
end;

procedure TStkResonate.keyOn;
begin
  adsr.keyOn();
end;

procedure TStkResonate.keyOff;
begin
  adsr.keyOff();
end;

procedure TStkResonate.noteOn;
begin
  adsr.setTarget(amplitude);
  keyOn();
  setResonance(frequency, FPoleRadius);
end;

procedure TStkResonate.noteOff;
begin
  keyOff();
end;

procedure TStkResonate.setResonance;
begin
  FPoleFrequency := frequency;
  if (frequency < 0.0) then
    FPoleFrequency := 0.0;
  FPoleRadius := radius;
  if (radius < 0.0) then
    FPoleRadius := 0.0
  else if (radius >= 1.0) then
    FPoleRadius := 0.9999;
  FFilter.setResonance(FPoleFrequency, FPoleRadius, True);
end;

procedure TStkResonate.setNotch;
begin
  FZeroFrequency := frequency;
  if (frequency < 0.0) then
    FZeroFrequency := 0.0;
  FZeroRadius := radius;
  if (radius < 0.0) then
    FZeroRadius := 0.0;
  FFilter.setNotch(FZeroFrequency, FZeroRadius);
end;

procedure TStkResonate.setEqualGainZeroes;
begin
  FFilter.setEqualGainZeroes;
end;

function TStkResonate.tick: Single;
begin
  lastOutput := FFilter.tick(FNoise.tick());
  lastOutput := lastOutput * adsr.tick();
  Result := lastOutput;
end;

procedure TStkResonate.controlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = 2) then // 2
    setResonance(norm * srate * 0.5, FPoleRadius)
  else if (number = 4) then // 4
    setResonance(FPoleFrequency, norm * 0.9999)
  else if (number = 11) then // 11
    setNotch(norm * srate * 0.5, FZeroRadius)
  else if (number = 1) then
    setNotch(FZeroFrequency, norm)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

procedure TStkResonate.Clear;
begin
  FFilter.Clear;
end;

end.
