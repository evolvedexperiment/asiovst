unit DAV_StkRhodey;

{
/***************************************************/
/*! \class TRhodey
    \brief STK Fender Rhodes electric piano FM
           synthesis instrument.

    This class implements two simple FM Pairs
    summed together, also referred to as algorithm
    5 of the TX81Z.

    \code
    Algorithm 5 is :  4.3--\
                             + -. Out
                      2.1--/
    \endcode

    Control Change Numbers: 
       - Modulator Index One:=2
       - Crossfade of Outputs:=4
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

uses stk, fm, waveplayer;

type
  TRhodey = class(TFM)
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
  end;

implementation

constructor TRhodey.Create;
var
  i: integer;
begin
  inherited Create(sr);
  waves[0] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  waves[1] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  waves[2] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  waves[3] := TWavePlayer.Create(srate, 'c:\stk\fwavblnk.wav');
  waves[0].SetOneShot(False);
  waves[1].SetOneShot(False);
  waves[2].SetOneShot(False);
  waves[3].SetOneShot(False);

  setRatio(0, 1.0);
  setRatio(1, 0.5);
  setRatio(2, 1.0);
  setRatio(3, 15.0);

  gains[0] := __TFM_gains[99];
  gains[1] := __TFM_gains[90];
  gains[2] := __TFM_gains[99];
  gains[3] := __TFM_gains[67];

  adsr[0].setAllTimes(0.001, 1.50, 0.0, 0.04);
  adsr[1].setAllTimes(0.001, 1.50, 0.0, 0.04);
  adsr[2].setAllTimes(0.001, 1.00, 0.0, 0.04);
  adsr[3].setAllTimes(0.001, 0.25, 0.0, 0.04);

  twozero.setGain(1.0);
end;

destructor TRhodey.Destroy;
begin
  inherited Destroy;
end;

procedure TRhodey.setFrequency;
var
  i: integer;
begin
  baseFrequency := frequency * 2.0;
  for i := 0 to nOperators - 1 do
    waves[i].setFrequency(baseFrequency * ratios[i]);
end;

procedure TRhodey.noteOn;
begin
  gains[0] := amplitude * __TFM_gains[99];
  gains[1] := amplitude * __TFM_gains[90];
  gains[2] := amplitude * __TFM_gains[99];
  gains[3] := amplitude * __TFM_gains[67];
  setFrequency(frequency);
  keyOn;
end;

function TRhodey.tick: my_float;
var
  temp, temp2: my_float;
begin
  temp := gains[1] * adsr[1].tick * waves[1].tick;
  temp := temp * control1;
  waves[0].addPhaseOffset(temp);
  waves[3].addPhaseOffset(twozero.lastOut);
  temp := gains[3] * adsr[3].tick * waves[3].tick;
  twozero.tick(temp);
  waves[2].addPhaseOffset(temp);
  temp := (1.0 - (control2 * 0.5)) * gains[0] * adsr[0].tick * waves[0].tick;
  temp := temp + control2 * 0.5 * gains[2] * adsr[2].tick * waves[2].tick;
  // Calculate amplitude modulation and apply it to output.
  temp2 := vibrato.tick * modDepth;
  temp := temp * (1.0 + temp2);
  lastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.
