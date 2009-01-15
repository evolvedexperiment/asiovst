unit DAV_BandedWG;

{
/***************************************************/
/*! \class TBandedWG
    \brief Banded waveguide modeling class.

    This class uses banded waveguide techniques to
    model a variety of sounds, including bowed
    bars, glasses, and bowls.  For more
    information, see Essl, G. and Cook, P. "Banded
    Waveguides: Towards Physical Modelling of Bar
    Percussion Instruments", Proceedings of the
    1999 International Computer Music Conference.

    Control Change Numbers:
       - Bow Pressure:=2
       - Bow Motion:=4
       - Strike Position:=8 
       - Vibrato Frequency:=11
       - Gain:=1
       - Bow Velocity:=128
       - Set Striking:=64
       - Instrument Presets:=16
         - Uniform Bar:=0
         - Tuned Bar:=1
         - Glass Harmonica:=2
         - Tibetan Bowl:=3

    by Georg Essl, 1999 - 2002.
    Modified for Stk 4.0 by Gary Scavone.
*/
/***************************************************/
}
interface

uses
  DAV_Stk, DAV_Instrmnt, DAV_Delayl, DAV_Bowtabl, DAV_Adsr, DAV_Biquad, Math;

const
  MAX_BANDED_MODES = 20;

type
  TBandedWG = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set strike position (0.0 - 1.0).
    procedure setStrikePosition(position: MY_FLOAT);

  //! Select a preset.
    procedure setPreset(preset: integer);

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Apply bow velocity/pressure to instrument with given amplitude and rate of increase.
    procedure startBowing(amplitude, rate: MY_FLOAT);

  //! Decrease bow velocity/breath pressure with given rate of decrease.
    procedure stopBowing(rate: MY_FLOAT);

  //! Pluck the instrument with given amplitude.
    procedure pluck(amp: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    doPluck, trackVelocity: boolean;
    nModes, presetModes: integer;
    bowTabl: tbowtabl;
    ADSR: tadsr;

    bandpass: array[0..MAX_BANDED_MODES - 1] of tbiquad;
    delay: array[0..MAX_BANDED_MODES - 1] of tdelayl;
    freakency, baseGain, maxVelocity: my_float;
    gains, basegains, excitation, modes: array[0..MAX_BANDED_MODES - 1] of my_float;
    integrationConstant, velocityInput, bowVelocity, bowTarget,
    strikeAmp, bowPosition: my_float;
    strikePosition: integer;
  end;

implementation

constructor TBandedWG.Create;
var
  i: integer;
begin
  inherited Create(sr);
  doPluck := True;
  for i := 0 to MAX_BANDED_MODES - 1 do
   begin
    delay[i] := TDelayL.Create(srate);
    bandpass[i] := TBiQuad.Create(srate);
   end;

  bowTabl := TBowTabl.Create(srate);
  bowTabl.setSlope(3.0);

  adsr := TADSR.Create(srate);
  adsr.setAllTimes(0.02, 0.005, 0.9, 0.01);

  freakency := 220.0;
  setPreset(0);

  bowPosition := 0;
  baseGain := 0.999;

  integrationConstant := 0.0;
  trackVelocity := False;

  bowVelocity := 0.0;
  bowTarget := 0.0;

  strikeAmp := 0.0;
end;

destructor TBandedWG.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  bowTabl.Free;
  adsr.Free;
  for i := 0 to MAX_BANDED_MODES - 1 do
   begin
    delay[i].Free;
    bandpass[i].Free;
   end;
end;

procedure TBandedWG.Clear;
var
  i: integer;
begin
  for i := 0 to nModes - 1 do
   begin
    delay[i].Clear;
    bandpass[i].Clear;
   end;
end;

procedure TBandedWG.setPreset;
var
  i: integer;
begin
  case preset of
    1 : // Tuned Bar
     begin
      presetModes := 4;
      modes[0] := 1.0;
      modes[1] := 4.0198391420;
      modes[2] := 10.7184986595;
      modes[3] := 18.0697050938;

      for i := 0 to presetModes - 1 do
       begin
        basegains[i] := power(0.999, i + 1);
        excitation[i] := 1.0;
       end;

     end;

    2 : // Glass Harmonica
     begin
      presetModes := 5;
      modes[0] := 1.0;
      modes[1] := 2.32;
      modes[2] := 4.25;
      modes[3] := 6.63;
      modes[4] := 9.38;
    // modes[5]:= 12.22;

      for i := 0 to presetModes - 1 do
       begin
        basegains[i] := power(0.999, i + 1);
        excitation[i] := 1.0;
       end;

     end;

    3 : // Tibetan Prayer Bowl (ICMC'02)
     begin
      presetModes := 12;
      modes[0] := 0.996108344;
      basegains[0] := 0.999925960128219;
      excitation[0] := 11.900357 / 10.0;
      modes[1] := 1.0038916562;
      basegains[1] := 0.999925960128219;
      excitation[1] := 11.900357 / 10.;
      modes[2] := 2.979178;
      basegains[2] := 0.999982774366897;
      excitation[2] := 10.914886 / 10.;
      modes[3] := 2.99329767;
      basegains[3] := 0.999982774366897;
      excitation[3] := 10.914886 / 10.;
      modes[4] := 5.704452;
      basegains[4] := 1.0; //0.999999999999999999987356406352;
      excitation[4] := 42.995041 / 10.;
      modes[5] := 5.704452;
      basegains[5] := 1.0; //0.999999999999999999987356406352;
      excitation[5] := 42.995041 / 10.;
      modes[6] := 8.9982;
      basegains[6] := 1.0; //0.999999999999999999996995497558225;
      excitation[6] := 40.063034 / 10.;
      modes[7] := 9.01549726;
      basegains[7] := 1.0; //0.999999999999999999996995497558225;
      excitation[7] := 40.063034 / 10.;
      modes[8] := 12.83303;
      basegains[8] := 0.999965497558225;
      excitation[8] := 7.063034 / 10.;
      modes[9] := 12.807382;
      basegains[9] := 0.999965497558225;
      excitation[9] := 7.063034 / 10.;
      modes[10] := 17.2808219;
      basegains[10] := 0.9999999999999999999965497558225;
      excitation[10] := 57.063034 / 10.;
      modes[11] := 21.97602739726;
      basegains[11] := 0.999999999999999965497558225;
      excitation[11] := 57.063034 / 10.;

     end;

  else // Uniform Bar
   begin
    presetModes := 4;
    modes[0] := 1.0;
    modes[1] := 2.756;
    modes[2] := 5.404;
    modes[3] := 8.933;

    for i := 0 to presetModes - 1 do
     begin
      basegains[i] := power(0.9, i + 1);
      excitation[i] := 1.0;
     end;

   end;
   end;

  nModes := presetModes;
  setFrequency(freakency);
end;

procedure TBandedWG.setFrequency;
var
  radius, base, length: my_float;
  i: integer;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0
  else
  if (freakency > 1568.0) then
    freakency := 1568.0;
  base := srate / freakency;
  for i := 0 to presetModes - 1 do
   begin
    // Calculate the delay line lengths for each mode.
    length := round(base / modes[i]);
    if (length > 2.0) then
     begin
      delay[i].setDelay(length);
      gains[i] := basegains[i];
     end else
     begin
      nModes := i;
      break;
     end;
    //  cerr << endl;

    // Set the bandpass filter resonances
    radius := 1.0 - PI * 32 / srate; //freakency * modes[i] / Stk::sampleRate/32;
    if (radius < 0.0) then
      radius := 0.0;
    bandpass[i].setResonance(freakency * modes[i], radius, True);

    delay[i].Clear;
    bandpass[i].Clear;
   end;

  //int olen:=(int)(delay[0].getDelay);
  //strikePosition:=(int)(strikePosition*(length/modes[0])/olen);
end;

procedure TBandedWG.setStrikePosition;
begin
  strikePosition := round(delay[0].getDelay * position / 2.0);
end;

procedure TBandedWG.startBowing;
begin
  adsr.setRate(rate);
  adsr.keyOn;
  maxVelocity := 0.03 + (0.1 * amplitude);
end;

procedure TBandedWG.stopBowing;
begin
  adsr.setRate(rate);
  adsr.keyOff;
end;

procedure TBandedWG.pluck;
var
  i, j: integer;
  min_len: my_float;
begin
  min_len := delay[nModes - 1].getDelay;
  for i := 0 to nModes - 1 do
    for j := 0 to round(delay[i].getDelay / min_len) - 1 do
      delay[i].tick(excitation[i] * amp / nModes);
end;

procedure TBandedWG.noteOn;
begin
  setFrequency(frequency);
  if (doPluck) then
    pluck(amplitude)
  else
    startBowing(amplitude, amplitude * 0.001);
end;

procedure TBandedWG.noteOff;
begin
  if (not doPluck) then
    stopBowing((1.0 - amplitude) * 0.005);
end;

function TBandedWG.tick: MY_FLOAT;
var
  k: integer;
  Data, input: my_float;
begin
  if (doPluck) then
    input := 0.0//  input:=strikeAmp/nModes;
//  strikeAmp:=0.0;

  else
   begin
    if (integrationConstant = 0.0) then
      velocityInput := 0.0
    else
      velocityInput := integrationConstant * velocityInput;

    for k := 0 to nModes - 1 do
      velocityInput := velocityinput + (baseGain * delay[k].lastOut);

    if (trackVelocity) then
     begin
      bowVelocity := bowvelocity * 0.9995;
      bowVelocity := bowvelocity + bowTarget;
      bowTarget := bowtarget * 0.995;
     end
    else
      bowVelocity := adsr.tick * maxVelocity;

    input := bowVelocity - velocityInput;
    input := input * bowTabl.tick(input);
    input := input / nModes;
   end;

  Data := 0.0;
  for k := 0 to nModes - 1 do
   begin
    bandpass[k].tick(input + gains[k] * delay[k].lastOut);
    delay[k].tick(bandpass[k].lastOut);
    Data := Data + bandpass[k].lastOut;
   end;

  //lastOutput:=data * nModes;
  lastOutput := Data * 4;
  Result := lastOutput;
end;

procedure TBandedWG.controlChange;
var
  norm: my_float;
  i: integer;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_BowPressure_) then
   begin // 2
    if (norm = 0.0) then
      doPluck := True
    else
     begin
      doPluck := False;
      bowTabl.setSlope(10.0 - (9.0 * norm));
     end;
   end
  else if (number = 4) then
   begin // 4
    if (not trackVelocity) then
      trackVelocity := True;
    bowTarget := bowtarget + 0.005 * (norm - bowPosition);
    bowPosition := norm;
    //adsr.setTarget(bowPosition);
   end
  else if (number = 8) then // 8
    setStrikePosition(norm)
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    //bowTarget += 0.02 * (norm - bowPosition);
    //bowPosition:=norm;
    if (trackVelocity) then
      trackVelocity := False;
    maxVelocity := 0.13 * norm;
    adsr.setTarget(norm);
   end
  else if (number = __SK_ModWheel_) then
   begin // 1
    //    baseGain:=0.9989999999 + (0.001 * norm );
    baseGain := 0.8999999999999999 + (0.1 * norm);
    //  cerr << "Yuck!" << endl;
    for i := 0 to nModes - 1 do
      gains[i] := basegains[i] * baseGain;
    //      gains[i]= pow(baseGain, (int)(delay[i].getDelay+i));
   end
  else if (number = __SK_ModFrequency_) then // 11
    integrationConstant := norm
  else if (number = __SK_Sustain_) then
   begin // 64
    if (norm < 0.5) then
      doPluck := True
    else
      doPluck := False;
   end
  else if (number = __SK_Portamento_) then
   begin // 65
    if (norm < 0.5) then
      trackVelocity := False
    else
      trackVelocity := True;
   end
  else if (number = __SK_ProphesyRibbon_) then // 16
    setPreset(round(Value * 3));
end;

end.
