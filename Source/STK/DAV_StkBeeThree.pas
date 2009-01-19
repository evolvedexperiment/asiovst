unit DAV_StkBeeThree;

{ STK Hammond-oid organ FM synthesis instrument.

  This class implements a simple 4 operator topology, also referred to as
  algorithm 8 of the TX81Z.

  Algorithm 8 is :
                    1 --.
                    2 -\|
                        +. Out
                    3 -/|
                    4 --

  Control Change Numbers:
    - Operator 4 (feedback) Gain = 2
    - Operator 3 Gain = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - ADSR 2 & 4 Target = 128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha.
  If you are of the type who should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkFm, DAV_StkWaveplayer;

type
  TStkBeeThree = class(TStkFM)
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(Frequency, Amplitude: Single);

    // Compute one output sample.
    function Tick: Single;
  end;

implementation

{ TStkBeeThree }

constructor TStkBeeThree.Create(SampleRate: Single);
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

  setRatio(0, 0.999);
  setRatio(1, 1.997);
  setRatio(2, 3.006);
  setRatio(3, 6.009);

  gains[0] := __TFM_gains[95];
  gains[1] := __TFM_gains[95];
  gains[2] := __TFM_gains[99];
  gains[3] := __TFM_gains[95];

  adsr[0].setAllTimes(0.005, 0.003, 1.0, 0.01);
  adsr[1].setAllTimes(0.005, 0.003, 1.0, 0.01);
  adsr[2].setAllTimes(0.005, 0.003, 1.0, 0.01);
  adsr[3].setAllTimes(0.005, 0.001, 0.4, 0.03);

  twozero.setGain(0.1);
end;

destructor TStkBeeThree.Destroy;
begin
  inherited Destroy;
end;

procedure TStkBeeThree.noteOn(Frequency, Amplitude: Single);
begin
  gains[0] := Amplitude * __TFM_gains[95];
  gains[1] := Amplitude * __TFM_gains[95];
  gains[2] := Amplitude * __TFM_gains[99];
  gains[3] := Amplitude * __TFM_gains[95];
  setFrequency(Frequency);
  keyOn;
end;

function TStkBeeThree.tick: Single;
var
  temp: Single;
begin
  if (modDepth > 0.0) then
   begin
    temp := 1.0 + (modDepth * vibrato.tick * 0.1);
    waves[0].setFrequency(baseFrequency * temp * ratios[0]);
    waves[1].setFrequency(baseFrequency * temp * ratios[1]);
    waves[2].setFrequency(baseFrequency * temp * ratios[2]);
    waves[3].setFrequency(baseFrequency * temp * ratios[3]);
   end;

  waves[3].addPhaseOffset(twozero.lastOut);
  temp := control1 * 2.0 * gains[3] * adsr[3].tick * waves[3].tick;
  twozero.tick(temp);

  temp := temp + control2 * 2.0 * gains[2] * adsr[2].tick * waves[2].tick;
  temp := temp + gains[1] * adsr[1].tick * waves[1].tick;
  temp := temp + gains[0] * adsr[0].tick * waves[0].tick;
  lastOutput := temp * 0.125;
  Result := lastOutput;
end;

end.
