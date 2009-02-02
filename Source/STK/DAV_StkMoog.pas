unit DAV_StkMoog;

{  STK moog-like swept filter sampling synthesis class.

   This instrument uses one attack wave, one looped wave, and an ADSR envelope
   (inherited from the Sampler class) and adds two sweepable formant (FormSwep)
   FFilters.

    Control Change Numbers:
       - Filter Q = 2
       - Filter Sweep Rate = 4
       - Vibrato AFrequency = 11
       - Vibrato Gain = 1
       - Gain = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkWavePlayer, DAV_StkSampler, DAV_StkFormantSweep;

type
  TStkMoog = class(TStkSampler)
  protected
    FFilters    : array[0..1] of TFormantSweep;
    FModDepth   : Single;
    FFilterQ    : Single;
    FFilterRate : Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Set instrument parameters for a particular AFrequency.
    procedure SetFrequency(AFrequency: Single);

    // Start a note with the given AFrequency and AAmplitude.
    procedure NoteOn(AFrequency, AAmplitude: Single);

    // Set the modulation (vibrato) speed in Hz.
    procedure SetModulationSpeed(mSpeed: Single);

    // Set the modulation (vibrato) depth.
    procedure SetModulationDepth(mDepth: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by Number and value (0.0 - 128.0).
    procedure ControlChange(Number: Integer; Value: Single);
  end;

implementation

constructor TStkMoog.Create;
begin
  inherited Create(SampleRate);
  attacks[0] := TWavePlayer.Create(srate, 'mandpluk.wav');
  loops[0] := TWavePlayer.Create(srate, 'impuls20.wav');
  loops[1] := TWavePlayer.Create(srate, 'sinewave.wav');
  loops[0].SetOneShot(False);
  loops[1].SetOneShot(False);
  loops[1].SetFrequency(6.122);
  FFilters[0] := TFormantSweep.Create(srate);
  FFilters[0].setTargets(0.0, 0.7);
  FFilters[1] := TFormantSweep.Create(srate);
  FFilters[1].setTargets(0.0, 0.7);
  adsr.setAllTimes(0.001, 1.5, 0.6, 0.250);
  FFilterQ := 0.85;
  FFilterRate := 0.0001;
  FModDepth := 0.0;
end;

destructor TStkMoog.Destroy;
begin
  inherited Destroy;
  attacks[0].Free;
  loops[0].Free;
  loops[1].Free;
  FFilters[0].Free;
  FFilters[1].Free;
end;

procedure TStkMoog.SetFrequency;
var
  rate: Single;
begin
  baseFrequency := AFrequency;
  if (AFrequency <= 0.0) then
    baseFrequency := 220.0;
//  rate:=attacks[0].getSize * 0.01 * baseFrequency / sampleRate;
//  attacks[0].setRate( rate );
  attacks[0].SetFrequency(basefrequency);
  loops[0].SetFrequency(baseFrequency);
end;

procedure TStkMoog.NoteOn;
var
  temp: Single;
begin
  SetFrequency(AFrequency);
  keyOn;
  attackGain := AAmplitude * 0.5;
  loopGain := AAmplitude;

  temp := FFilterQ + 0.05;
  FFilters[0].setStates(2000.0, temp);
  FFilters[1].setStates(2000.0, temp);

  temp := FFilterQ + 0.099;
  FFilters[0].setTargets(AFrequency, temp);
  FFilters[1].setTargets(AFrequency, temp);

  FFilters[0].setSweepRate(FFilterRate * 22050.0 / srate);
  FFilters[1].setSweepRate(FFilterRate * 22050.0 / srate);
end;

procedure TStkMoog.SetModulationSpeed;
begin
  loops[1].SetFrequency(mSpeed);
end;

procedure TStkMoog.SetModulationDepth;
begin
  FModDepth := mDepth * 0.5;
end;

function TStkMoog.Tick: Single;
var
  temp: Single;
begin
  if (FModDepth <> 0.0) then
   begin
    temp := loops[1].Tick * FModDepth;
    loops[0].SetFrequency(baseFrequency * (1.0 + temp));
   end;

  temp := inherited Tick;
  temp := FFilters[0].Tick(temp);
  lastOutput := FFilters[1].Tick(temp);
  Result := lastOutput * 3.0;
end;

procedure TStkMoog.ControlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (Number = __SK_FilterQ_) then // 2
    FFilterQ := 0.80 + (0.1 * norm)
  else if (Number = __SK_FilterSweepRate_) then // 4
    FFilterRate := norm * 0.0002
  else if (Number = __SK_ModFrequency_) then // 11
    SetModulationSpeed(norm * 12.0)
  else if (Number = __SK_ModWheel_) then // 1
    SetModulationDepth(norm)
  else if (Number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

end.
