unit DAV_StkTubeBell;
{
/***************************************************/
/*! \class TTubeBell
    \brief STK tubular bell (orchestral chime) FM
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
uses stk,fm,waveplayer;

type TTubeBell=class(TFM)
 public
  //! Class constructor.
  constructor create(sr:my_float);

  //! Class destructor.
  destructor destroy;

  //! Start a note with the given frequency and amplitude.
  procedure noteOn(frequency,amplitude:MY_FLOAT);

  //! Compute one output sample.
  function tick:MY_FLOAT;
end;

implementation

constructor TTubeBell.create;
var i:integer;
begin
 inherited create(sr);
  waves[0]:=TWavePlayer.create(srate,'c:\stk\sinewave.wav');
  waves[1]:=TWavePlayer.create(srate,'c:\stk\sinewave.wav');
  waves[2]:=TWavePlayer.create(srate,'c:\stk\sinewave.wav');
  waves[3]:=TWavePlayer.create(srate,'c:\stk\fwavblnk.wav');
  waves[0].SetOneShot(false);
  waves[1].SetOneShot(false);
  waves[2].SetOneShot(false);
  waves[3].SetOneShot(false);

  setRatio(0, 1.0   * 0.995);
  setRatio(1, 1.414 * 0.995);
  setRatio(2, 1.0   * 1.005);
  setRatio(3, 1.414 * 1.000);

  gains[0]:=__TFM_gains[94];
  gains[1]:=__TFM_gains[76];
  gains[2]:=__TFM_gains[99];
  gains[3]:=__TFM_gains[71];

  adsr[0].setAllTimes( 0.005, 4.0, 0.0, 0.04);
  adsr[1].setAllTimes( 0.005, 4.0, 0.0, 0.04);
  adsr[2].setAllTimes( 0.001, 2.0, 0.0, 0.04);
  adsr[3].setAllTimes( 0.004, 4.0, 0.0, 0.04);

  twozero.setGain( 0.5 );
  vibrato.setFrequency( 2.0 );
end;

destructor TTubeBell.destroy;
begin
 inherited destroy;
end;

procedure TTubeBell.noteOn;
begin
  gains[0]:=amplitude * __TFM_gains[94];
  gains[1]:=amplitude * __TFM_gains[76];
  gains[2]:=amplitude * __TFM_gains[99];
  gains[3]:=amplitude * __TFM_gains[71];
  setFrequency(frequency);
  keyOn;
end;

function TTubeBell.tick:MY_FLOAT;
var temp,temp2:MY_FLOAT;
begin
  temp:=gains[1] * adsr[1].tick * waves[1].tick;
  temp:=temp * control1;
  waves[0].addPhaseOffset(temp);

  waves[3].addPhaseOffset(twozero.lastOut);
  temp:=gains[3] * adsr[3].tick * waves[3].tick;
  twozero.tick(temp);
  temp:=temp * control1;
  waves[2].addPhaseOffset(temp);

  temp:=gains[0] * adsr[0].tick * waves[0].tick;
//  temp:=( 1.0 - (control2 * 0.5)) * gains[0] * adsr[0].tick * waves[0].tick;
  temp:={temp+control2 * 0.5 * }temp+gains[2] * adsr[2].tick * waves[2].tick;

  // Calculate amplitude modulation and apply it to output.
  temp2:=vibrato.tick * modDepth;
  temp:=temp * (1.0 + temp2);
  lastOutput:=temp * 0.5;
  result:= lastOutput;
end;

end.

