unit DAV_StkHeavyMetal;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK heavy metal Fm synthesis instrument.

   This class implements 3 cascade operators with feedback modulation, also
   referred to as algorithm 3 of the TX81Z.

   Algorithm 3 is :    4-\
                    3->2--+->1->Out

   Control Change Numbers:
     - Total Modulator Index:=2
     - Modulator Crossfade:=4
     - LFO Speed:=11
     - LFO Depth:=1
     - ADSR 2 & 4 Target:=128

   The basic Chowning/Stanford StkFm patent expired in 1995, but there exist
   follow-on patents, mostly assigned to Yamaha. If you are of the type who
   should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkFm, DAV_StkWavePlayer, Windows;

type
  TStkHeavyMetal = class(TStkFM)
  public
    constructor Create(SampleRate: Single); override;
    destructor Destroy; override;

    procedure NoteOn(Frequency, Amplitude: Single); override;
    function Tick: Single; override;
  end;

implementation

constructor TStkHeavyMetal.Create;
var
  i: Integer;
begin
  inherited Create(SampleRate);
  Waves[0] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  Waves[1] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  Waves[2] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  Waves[3] := TWavePlayer.Create(SampleRate, 'fwavblnk.wav');
  Waves[0].SetOneShot(False);
  Waves[1].SetOneShot(False);
  Waves[2].SetOneShot(False);
  Waves[3].SetOneShot(False);
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

destructor TStkHeavyMetal.Destroy;
begin
  inherited Destroy;
end;

procedure TStkHeavyMetal.NoteOn;
begin
  gains[0] := Amplitude * __TFM_gains[92];
  gains[1] := Amplitude * __TFM_gains[76];
  gains[2] := Amplitude * __TFM_gains[91];
  gains[3] := Amplitude * __TFM_gains[68];
  setFrequency(Frequency);
  keyOn;
end;

function TStkHeavyMetal.Tick: Single;
var
  temp: Single;
begin
  temp := vibrato.Tick * modDepth * 0.2;
  Waves[0].setFrequency(baseFrequency * (1.0 + temp) * ratios[0]);
  Waves[1].setFrequency(baseFrequency * (1.0 + temp) * ratios[1]);
  Waves[2].setFrequency(baseFrequency * (1.0 + temp) * ratios[2]);
  Waves[3].setFrequency(baseFrequency * (1.0 + temp) * ratios[3]);

  temp := gains[2] * adsr[2].Tick * Waves[2].Tick;
  Waves[1].addPhaseOffset(temp);

  Waves[3].addPhaseOffset(twozero.lastOut);
  temp := (1.0 - (control2 * 0.5)) * gains[3] * adsr[3].Tick * Waves[3].Tick;
  twozero.Tick(temp);

  temp := temp + (control2 * 0.5 * gains[1] * adsr[1].Tick * Waves[1].Tick);
  temp := temp * control1;

  Waves[0].addPhaseOffset(temp);
  temp := gains[0] * adsr[0].Tick * Waves[0].Tick;

  lastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.
