unit DAV_StkFMVoices;

{
/***************************************************/
/*! \class TFMVoices
    \brief STK singing FM synthesis instrument.

    This class implements 3 carriers and a common
    modulator, also referred to as algorithm 6 of
    the TX81Z.

    \code
    Algorithm 6 is :
                        /.1 -\
                     4-|-.2 - +. Out
                        \.3 -/
    \endcode

    Control Change Numbers: 
       - Vowel:=2
       - Spectral Tilt:=4
       - LFO Speed:=11
       - LFO Depth:=1
       - ADSR 2 & 4 Target:=128

    The basic Chowning/Stanford FM patent expired
    in 1995, but there exist follow-on patents,
    mostly assigned to Yamaha.  If you are of the
    type who should worry about this (making
    money) worry away.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer, DAV_StkPhoneMes;

type
  TFMVoices = class(TFM)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    currentVowel: integer;
    ph: tphonemes;
    mods, tilt: array[0..2] of my_float;
  end;

implementation

constructor TFMVoices.Create;
begin
  inherited Create(sr);
  ph := tphonemes.Create(sr);
  waves[0] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  waves[1] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  waves[2] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  waves[3] := TWavePlayer.Create(srate, 'c:\stk\fwavblnk.wav');
  waves[0].SetOneShot(False);
  waves[1].SetOneShot(False);
  waves[2].SetOneShot(False);
  waves[3].SetOneShot(False);

  setRatio(0, 2.00);
  setRatio(1, 4.00);
  setRatio(2, 12.0);
  setRatio(3, 1.00);

  gains[3] := __TFM_gains[80];

  adsr[0].setAllTimes(0.05, 0.05, __TFM_susLevels[15], 0.05);
  adsr[1].setAllTimes(0.05, 0.05, __TFM_susLevels[15], 0.05);
  adsr[2].setAllTimes(0.05, 0.05, __TFM_susLevels[15], 0.05);
  adsr[3].setAllTimes(0.01, 0.01, __TFM_susLevels[15], 0.5);

  twozero.setGain(0.0);
  modDepth := 0.005;
  currentVowel := 0;
  tilt[0] := 1.0;
  tilt[1] := 0.5;
  tilt[2] := 0.2;
  mods[0] := 1.0;
  mods[1] := 1.1;
  mods[2] := 1.1;
  baseFrequency := 110.0;
  setFrequency(110.0);
end;

destructor TFMVoices.Destroy;
begin
  inherited Destroy;
  ph.Free;
end;

procedure TFMVoices.setFrequency;
var
  temp, temp2: my_float;
  i, tempi: integer;
begin
  temp2 := 0.0;
  tempi := 0;
  i := 0;

  if (currentVowel < 32) then
   begin
    i := currentVowel;
    temp2 := 0.9;
   end
  else if (currentVowel < 64) then
   begin
    i := currentVowel - 32;
    temp2 := 1.0;
   end
  else if (currentVowel < 96) then
   begin
    i := currentVowel - 64;
    temp2 := 1.1;
   end
  else if (currentVowel <= 128) then
   begin
    i := currentVowel - 96;
    temp2 := 1.2;
   end;

  baseFrequency := frequency;
  temp := (temp2 * ph.formantFrequency(i, 0) / baseFrequency) + 0.5;
  tempi := round(temp);
  setRatio(0, tempi);
  temp := (temp2 * ph.formantFrequency(i, 1) / baseFrequency) + 0.5;
  tempi := round(temp);
  setRatio(1, tempi);
  temp := (temp2 * ph.formantFrequency(i, 2) / baseFrequency) + 0.5;
  tempi := round(temp);
  setRatio(2, tempi);
  gains[0] := 1.0;
  gains[1] := 1.0;
  gains[2] := 1.0;
end;

procedure TFMVoices.noteOn;
begin
  setFrequency(frequency);
  tilt[0] := amplitude;
  tilt[1] := amplitude * amplitude;
  tilt[2] := tilt[1] * amplitude;
  keyOn;
end;

function TFMVoices.tick: MY_FLOAT;
var
  temp, temp2: my_float;
begin
  temp := gains[3] * adsr[3].tick * waves[3].tick;
  temp2 := vibrato.tick * modDepth * 0.1;

  waves[0].setFrequency(baseFrequency * (1.0 + temp2) * ratios[0]);
  waves[1].setFrequency(baseFrequency * (1.0 + temp2) * ratios[1]);
  waves[2].setFrequency(baseFrequency * (1.0 + temp2) * ratios[2]);
  waves[3].setFrequency(baseFrequency * (1.0 + temp2) * ratios[3]);

  waves[0].addPhaseOffset(temp * mods[0]);
  waves[1].addPhaseOffset(temp * mods[1]);
  waves[2].addPhaseOffset(temp * mods[2]);
  waves[3].addPhaseOffset(twozero.lastOut);
  twozero.tick(temp);
  temp := gains[0] * tilt[0] * adsr[0].tick * waves[0].tick;
  temp := temp + gains[1] * tilt[1] * adsr[1].tick * waves[1].tick;
  temp := temp + gains[2] * tilt[2] * adsr[2].tick * waves[2].tick;

  Result := temp * 0.33;
end;

procedure TFMVoices.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then // 2
    gains[3] := __TFM_gains[round(norm * 99.9)]
  else if (number = __SK_FootControl_) then
   begin // 4
    currentVowel := round(norm * 128.0);
    setFrequency(baseFrequency);
   end
  else if (number = __SK_ModFrequency_) then // 11
    setModulationSpeed(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    setModulationDepth(norm)
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    tilt[0] := norm;
    tilt[1] := norm * norm;
    tilt[2] := tilt[1] * norm;
   end;
end;

end.
