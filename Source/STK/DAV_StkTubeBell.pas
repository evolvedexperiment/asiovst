unit DAV_StkTubeBell;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK tubular bell (orchestral chime) FM synthesis instrument.

  This class implements two simple FM Pairs summed together, also referred to
  as algorithm 5 of the TX81Z.

    Algorithm 5 is :  4.3--\
                             + -. Out
                      2.1--/

  Control Change Numbers:
    - Modulator Index One:=2
    - Crossfade of Outputs:=4
    - LFO Speed:=11
    - LFO Depth:=1
    - ADSR 2 & 4 Target:=128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha. If you are of the type who
  should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkFm, DAV_StkWavePlayer;

type
  TTubeBell = class(TFM)
  public
    constructor Create(SampleRate: Single); override;
    destructor Destroy; override;

    // Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: Single);

    // Compute one output sample.
    function tick: Single;
  end;

implementation

constructor TTubeBell.Create;
var
  i: integer;
begin
  inherited Create(SampleRate);
  waves[0] := TWavePlayer.Create(srate, 'sinewave.wav');
  waves[1] := TWavePlayer.Create(srate, 'sinewave.wav');
  waves[2] := TWavePlayer.Create(srate, 'sinewave.wav');
  waves[3] := TWavePlayer.Create(srate, 'fwavblnk.wav');
  waves[0].SetOneShot(False);
  waves[1].SetOneShot(False);
  waves[2].SetOneShot(False);
  waves[3].SetOneShot(False);

  setRatio(0, 1.0 * 0.995);
  setRatio(1, 1.414 * 0.995);
  setRatio(2, 1.0 * 1.005);
  setRatio(3, 1.414 * 1.000);

  gains[0] := __TFM_gains[94];
  gains[1] := __TFM_gains[76];
  gains[2] := __TFM_gains[99];
  gains[3] := __TFM_gains[71];

  adsr[0].setAllTimes(0.005, 4.0, 0.0, 0.04);
  adsr[1].setAllTimes(0.005, 4.0, 0.0, 0.04);
  adsr[2].setAllTimes(0.001, 2.0, 0.0, 0.04);
  adsr[3].setAllTimes(0.004, 4.0, 0.0, 0.04);

  twozero.setGain(0.5);
  vibrato.setFrequency(2.0);
end;

destructor TTubeBell.Destroy;
begin
  inherited Destroy;
end;

procedure TTubeBell.noteOn;
begin
  gains[0] := amplitude * __TFM_gains[94];
  gains[1] := amplitude * __TFM_gains[76];
  gains[2] := amplitude * __TFM_gains[99];
  gains[3] := amplitude * __TFM_gains[71];
  setFrequency(frequency);
  keyOn;
end;

function TTubeBell.tick: Single;
var
  temp, temp2: Single;
begin
  temp := gains[1] * adsr[1].tick * waves[1].tick;
  temp := temp * control1;
  waves[0].addPhaseOffset(temp);

  waves[3].addPhaseOffset(twozero.lastOut);
  temp := gains[3] * adsr[3].tick * waves[3].tick;
  twozero.tick(temp);
  temp := temp * control1;
  waves[2].addPhaseOffset(temp);

  temp := gains[0] * adsr[0].tick * waves[0].tick;
//  temp:=( 1.0 - (control2 * 0.5)) * gains[0] * adsr[0].tick * waves[0].tick;
  temp :={temp+control2 * 0.5 * }temp + gains[2] * adsr[2].tick * waves[2].tick;

  // Calculate amplitude modulation and apply it to output.
  temp2 := vibrato.tick * modDepth;
  temp := temp * (1.0 + temp2);
  lastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.

