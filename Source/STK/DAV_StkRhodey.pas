unit DAV_StkRhodey;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK Fender Rhodes electric piano FM synthesis instrument.

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
  TStkRhodey = class(TStkFM)
  public
    // Class constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(Frequency: Single);

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(Frequency, Amplitude: Single);

    // Compute one output sample.
    function Tick: Single;
  end;

implementation

constructor TStkRhodey.Create;
var
  i: integer;
begin
  inherited Create(SampleRate);
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

destructor TStkRhodey.Destroy;
begin
  inherited Destroy;
end;

procedure TStkRhodey.SetFrequency;
var
  i: integer;
begin
  baseFrequency := Frequency * 2.0;
  for i := 0 to nOperators - 1 do
    waves[i].SetFrequency(baseFrequency * ratios[i]);
end;

procedure TStkRhodey.NoteOn;
begin
  gains[0] := Amplitude * __TFM_gains[99];
  gains[1] := Amplitude * __TFM_gains[90];
  gains[2] := Amplitude * __TFM_gains[99];
  gains[3] := Amplitude * __TFM_gains[67];
  SetFrequency(Frequency);
  keyOn;
end;

function TStkRhodey.Tick: Single;
var
  temp, temp2: Single;
begin
  temp := gains[1] * adsr[1].Tick * waves[1].Tick;
  temp := temp * control1;
  waves[0].addPhaseOffset(temp);
  waves[3].addPhaseOffset(twozero.lastOut);
  temp := gains[3] * adsr[3].Tick * waves[3].Tick;
  twozero.Tick(temp);
  waves[2].addPhaseOffset(temp);
  temp := (1.0 - (control2 * 0.5)) * gains[0] * adsr[0].Tick * waves[0].Tick;
  temp := temp + control2 * 0.5 * gains[2] * adsr[2].Tick * waves[2].Tick;
  // Calculate Amplitude modulation and apply it to output.
  temp2 := vibrato.Tick * modDepth;
  temp := temp * (1.0 + temp2);
  lastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.
