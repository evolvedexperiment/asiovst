unit DAV_StkPercFlut;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK percussive flute FM synthesis instrument.

  This class implements algorithm 4 of the TX81Z.

  Algorithm 4 is :   4.3--\
                       2-- + -.1-.Out

  Control Change Numbers:
    - Total Modulator Index = 2
    - Modulator Crossfade = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - ADSR 2 & 4 Target = 128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha. If you are of the type who
  should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkFm, DAV_StkWavePlayer;

type
  TStkPercFlut = class(TStkFM)
  public
    // Class constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(Frequency: Single);

    // Start a note with the given Frequency and amplitude.
    procedure NoteOn(Frequency, amplitude: Single);

    // Compute one output sample.
    function Tick: Single;
  end;

implementation

constructor TStkPercFlut.Create;
var
  i: integer;
begin
  inherited Create(SampleRate);
  waves[0] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  waves[1] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  waves[2] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  waves[3] := TWavePlayer.Create(SampleRate, 'fwavblnk.wav');
  waves[0].SetOneShot(False);
  waves[1].SetOneShot(False);
  waves[2].SetOneShot(False);
  waves[3].SetOneShot(False);

  setRatio(0, 1.50 * 1.000);
  setRatio(1, 3.00 * 0.995);
  setRatio(2, 2.99 * 1.005);
  setRatio(3, 6.00 * 0.997);
  gains[0] := __TFM_gains[99];
  gains[1] := __TFM_gains[71];
  gains[2] := __TFM_gains[93];
  gains[3] := __TFM_gains[85];
  adsr[0].setAllTimes(0.05, 0.05, __TFM_susLevels[14], 0.05);
  adsr[1].setAllTimes(0.02, 0.50, __TFM_susLevels[13], 0.5);
  adsr[2].setAllTimes(0.02, 0.30, __TFM_susLevels[11], 0.05);
  adsr[3].setAllTimes(0.02, 0.05, __TFM_susLevels[13], 0.01);
  twozero.setGain(0.0);
  modDepth := 0.005;
end;

destructor TStkPercFlut.Destroy;
begin
  inherited Destroy;
end;

procedure TStkPercFlut.SetFrequency;
begin
  baseFrequency := Frequency;
end;

procedure TStkPercFlut.NoteOn;
begin
  gains[0] := amplitude * __TFM_gains[99] * 0.5;
  gains[1] := amplitude * __TFM_gains[71] * 0.5;
  gains[2] := amplitude * __TFM_gains[93] * 0.5;
  gains[3] := amplitude * __TFM_gains[85] * 0.5;
  SetFrequency(Frequency);
  keyOn;
end;

function TStkPercFlut.Tick: Single;
var
  temp: Single;
begin
  temp := vibrato.Tick * modDepth * 0.2;
  waves[0].SetFrequency(baseFrequency * (1.0 + temp) * ratios[0]);
  waves[1].SetFrequency(baseFrequency * (1.0 + temp) * ratios[1]);
  waves[2].SetFrequency(baseFrequency * (1.0 + temp) * ratios[2]);
  waves[3].SetFrequency(baseFrequency * (1.0 + temp) * ratios[3]);
  waves[3].addPhaseOffset(twozero.lastOut);
  temp := gains[3] * adsr[3].Tick * waves[3].Tick;
  twozero.Tick(temp);
  waves[2].addPhaseOffset(temp);
  temp := (1.0 - (control2 * 0.5)) * gains[2] * adsr[2].Tick * waves[2].Tick;
  temp := temp + control2 * 0.5 * gains[1] * adsr[1].Tick * waves[1].Tick;
  temp := temp * control1;
  waves[0].addPhaseOffset(temp);
  temp := gains[0] * adsr[0].Tick * waves[0].Tick;
  lastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.