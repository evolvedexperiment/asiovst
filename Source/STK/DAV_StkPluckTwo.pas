unit DAV_StkPluckTwo;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK enhanced plucked string model class.

  This class implements an enhanced two-string, plucked physical model, a la
  Jaffe-Smith, Smith, and others.

  TStkPluckTwo is an abstract class, with no excitation specified. Therefore, it
  can't be directly instantiated.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkDelayl, DAV_StkDelaya, DAV_StkOneZero;

type
  TStkPluckTwo = class(TStkInstrument)
  protected
    FDelayLine, FDelayline2: TDelaya;
    FCombDelay: TDelayl;
    FFilter, FFilter2: TOneZero;
    FLength: Longint;
    FLoopGain, FBaseLoopGain, FLastFrequency, FLastLength,
    FDetuning, FPluckAmplitude, FPluckPosition: Single;
  public
    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(SampleRate, lowestFrequency: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular Frequency.
    procedure setFrequency(Frequency: Single);

    // Detune the two strings by the given factor.  A value of 1.0 produces unison strings.
    procedure setDetune(detune: Single);

    // Efficient combined setting of Frequency and FDetuning.
    procedure setFreqAndDetune(Frequency, detune: Single);

    // Set the pluck or "excitation" position along the string (0.0 - 1.0).
    procedure setPluckPosition(position: Single);

    // Set the base loop gain.
  {
    The actual loop gain is set according to the Frequency.
    Because of high-Frequency loop FFilter roll-off, higher
    Frequency settings have greater loop gains.
  }
    procedure setBaseLoopGain(aGain: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: Single);

    // Virtual (abstract) tick function is implemented by subclasses.
    function tick: Single;
  end;

implementation

constructor TStkPluckTwo.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / lowestFrequency + 1);
  FBaseLoopGain := 0.995;
  FLoopGain := 0.999;
  FDelayLine := TDelaya.Create(SampleRate, (FLength / 2.0), FLength);
  FDelayline2 := TDelaya.Create(SampleRate, (FLength / 2.0), FLength);
  FCombDelay := TDelayl.Create(SampleRate, (FLength / 2.0), FLength);
  FFilter := TOneZero.Create(SampleRate);
  FFilter2 := TOneZero.Create(SampleRate);
  FPluckAmplitude := 0.3;
  FPluckPosition := 0.4;
  FDetuning := 0.995;
  FLastFrequency := lowestFrequency * 2.0;
  FLastLength := FLength * 0.5;
end;

destructor TStkPluckTwo.Destroy;
begin
  inherited Destroy;
  FDelayLine.Free;
  FDelayline2.Free;
  FCombDelay.Free;
  FFilter.Free;
  FFilter2.Free;
end;

procedure TStkPluckTwo.Clear;
begin
  FDelayLine.Clear;
  FDelayline2.Clear;
  FCombDelay.Clear;
  FFilter.Clear;
  FFilter2.Clear;
end;

procedure TStkPluckTwo.setFrequency;
var
  delay: Single;
begin
  FLastFrequency := Frequency;
  if (FLastFrequency <= 0.0) then
    FLastFrequency := 220.0;

  // Delay := FLength - approximate FFilter delay.
  FLastLength := (SampleRate / FLastFrequency);
  delay := (FLastLength / FDetuning) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;
  FDelayLine.setDelay(delay);

  delay := (FLastLength * FDetuning) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;
  FDelayline2.setDelay(delay);

  FLoopGain := FBaseLoopGain + (Frequency * 0.000005);
  if (FLoopGain > 1.0) then
    FLoopGain := 0.99999;
end;

procedure TStkPluckTwo.setDetune;
begin
  FDetuning := detune;
  if (FDetuning <= 0.0) then
    FDetuning := 0.1;
  FDelayLine.setDelay((FLastLength / FDetuning) - 0.5);
  FDelayline2.setDelay((FLastLength * FDetuning) - 0.5);
end;

procedure TStkPluckTwo.setFreqAndDetune;
begin
  FDetuning := detune;
  setFrequency(Frequency);
end;

procedure TStkPluckTwo.setPluckPosition;
begin
  FPluckPosition := position;
  if (position < 0.0) then
    FPluckPosition := 0.0
  else if (position > 1.0) then
    FPluckPosition := 1.0;
end;

procedure TStkPluckTwo.setBaseLoopGain;
begin
  FBaseLoopGain := aGain;
  FLoopGain := FBaseLoopGain + (FLastFrequency * 0.000005);
  if (FLoopGain > 0.99999) then
    FLoopGain := 0.99999;
end;

procedure TStkPluckTwo.noteOff;
begin
  FLoopGain := (1.0 - amplitude) * 0.5;
end;

function TStkPluckTwo.tick: Single;
begin
  Result := 0;
end;

end.
