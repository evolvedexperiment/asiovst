unit DAV_StkMoog;

{
/***************************************************/
/*! \class TMoog
    \brief STK moog-like swept filter sampling synthesis class.

    This instrument uses one attack wave, one
    looped wave, and an ADSR envelope (inherited
    from the Sampler class) and adds two sweepable
    formant (FormSwep) filters.

    Control Change Numbers:
       - Filter Q:=2
       - Filter Sweep Rate:=4
       - Vibrato Frequency:=11
       - Vibrato Gain:=1
       - Gain:=128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, waveplayer, sampler, formswep;

type
  TMoog = class(TSampler)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Set the modulation (vibrato) speed in Hz.
    procedure setModulationSpeed(mSpeed: MY_FLOAT);

  //! Set the modulation (vibrato) depth.
    procedure setModulationDepth(mDepth: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    filters: array[0..1] of tformswep;
    modDepth, filterQ, filterRate: MY_FLOAT;
  end;

implementation

constructor TMoog.Create;
begin
  inherited Create(sr);
  attacks[0] := TWavePlayer.Create(srate, 'c:\stk\mandpluk.wav');
  loops[0] := TWavePlayer.Create(srate, 'c:\stk\impuls20.wav');
  loops[1] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  loops[0].SetOneShot(False);
  loops[1].SetOneShot(False);
  loops[1].setFrequency(6.122);
  filters[0] := TFormSwep.Create(srate);
  filters[0].setTargets(0.0, 0.7);
  filters[1] := TFormSwep.Create(srate);
  filters[1].setTargets(0.0, 0.7);
  adsr.setAllTimes(0.001, 1.5, 0.6, 0.250);
  filterQ := 0.85;
  filterRate := 0.0001;
  modDepth := 0.0;
end;

destructor TMoog.Destroy;
begin
  inherited Destroy;
  attacks[0].Free;
  loops[0].Free;
  loops[1].Free;
  filters[0].Free;
  filters[1].Free;
end;

procedure TMoog.setFrequency;
var
  rate: my_float;
begin
  baseFrequency := frequency;
  if (frequency <= 0.0) then
    baseFrequency := 220.0;
//  rate:=attacks[0].getSize * 0.01 * baseFrequency / sampleRate;
//  attacks[0].setRate( rate );
  attacks[0].setFrequency(basefrequency);
  loops[0].setFrequency(baseFrequency);
end;

procedure TMoog.noteOn;
var
  temp: my_float;
begin
  setFrequency(frequency);
  keyOn;
  attackGain := amplitude * 0.5;
  loopGain := amplitude;

  temp := filterQ + 0.05;
  filters[0].setStates(2000.0, temp);
  filters[1].setStates(2000.0, temp);

  temp := filterQ + 0.099;
  filters[0].setTargets(frequency, temp);
  filters[1].setTargets(frequency, temp);

  filters[0].setSweepRate(filterRate * 22050.0 / srate);
  filters[1].setSweepRate(filterRate * 22050.0 / srate);
end;

procedure TMoog.setModulationSpeed;
begin
  loops[1].setFrequency(mSpeed);
end;

procedure TMoog.setModulationDepth;
begin
  modDepth := mDepth * 0.5;
end;

function TMoog.tick: my_float;
var
  temp: my_float;
begin
  if (modDepth <> 0.0) then
   begin
    temp := loops[1].tick * modDepth;
    loops[0].setFrequency(baseFrequency * (1.0 + temp));
   end;

  temp := inherited tick;
  temp := filters[0].tick(temp);
  lastOutput := filters[1].tick(temp);
  Result := lastOutput * 3.0;
end;

procedure TMoog.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_FilterQ_) then // 2
    filterQ := 0.80 + (0.1 * norm)
  else if (number = __SK_FilterSweepRate_) then // 4
    filterRate := norm * 0.0002
  else if (number = __SK_ModFrequency_) then // 11
    setModulationSpeed(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    setModulationDepth(norm)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

end.
