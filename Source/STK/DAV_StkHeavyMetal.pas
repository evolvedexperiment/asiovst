unit DAV_StkHeavyMetal;

{
/***************************************************/
/*! \class THevyMetl
    \brief STK heavy metal FM synthesis instrument.

    This class implements 3 cascade operators with
    feedback modulation, also referred to as
    algorithm 3 of the TX81Z.

    \code
    Algorithm 3 is :    4--\
                    3-.2-- + -.1-.Out
    \endcode

    Control Change Numbers: 
       - Total Modulator Index:=2
       - Modulator Crossfade:=4
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

uses stk, fm, Windows, waveplayer;

type
  THevyMetl = class(TFM)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;
  end;

implementation

constructor THevyMetl.Create;
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
  setRatio(0, 1.0 * 1.000);
  setRatio(1, 4.0 * 0.999);
  setRatio(2, 3.0 * 1.001);
  setRatio(3, 0.5 * 1.002);

  gains[0] := __TFM_gains[92];
  gains[1] := __TFM_gains[76];
  gains[2] := __TFM_gains[91];
  gains[3] := __TFM_gains[68];

  adsr[0].setAllTimes(0.001, 0.001, 1.0, 0.01);
  adsr[1].setAllTimes(0.001, 0.010, 1.0, 0.50);
  adsr[2].setAllTimes(0.010, 0.005, 1.0, 0.20);
  adsr[3].setAllTimes(0.030, 0.010, 0.2, 0.20);

  twozero.setGain(2.0);
  vibrato.setFrequency(5.5);
  modDepth := 0.0;
end;

destructor THevyMetl.Destroy;
begin
  inherited Destroy;
end;

procedure THevyMetl.noteOn;
begin
  gains[0] := amplitude * __TFM_gains[92];
  gains[1] := amplitude * __TFM_gains[76];
  gains[2] := amplitude * __TFM_gains[91];
  gains[3] := amplitude * __TFM_gains[68];
  setFrequency(frequency);
  keyOn;
end;

function THevyMetl.tick: my_float;
var
  temp: my_float;
begin
  temp := vibrato.tick * modDepth * 0.2;
  waves[0].setFrequency(baseFrequency * (1.0 + temp) * ratios[0]);
  waves[1].setFrequency(baseFrequency * (1.0 + temp) * ratios[1]);
  waves[2].setFrequency(baseFrequency * (1.0 + temp) * ratios[2]);
  waves[3].setFrequency(baseFrequency * (1.0 + temp) * ratios[3]);

  temp := gains[2] * adsr[2].tick * waves[2].tick;
  waves[1].addPhaseOffset(temp);

  waves[3].addPhaseOffset(twozero.lastOut);
  temp := (1.0 - (control2 * 0.5)) * gains[3] * adsr[3].tick * waves[3].tick;
  twozero.tick(temp);

  temp := temp + (control2 * 0.5 * gains[1] * adsr[1].tick * waves[1].tick);
  temp := temp * control1;

  waves[0].addPhaseOffset(temp);
  temp := gains[0] * adsr[0].tick * waves[0].tick;

  lastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.
