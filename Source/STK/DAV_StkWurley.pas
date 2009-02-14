unit DAV_StkWurley;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK Wurlitzer electric piano FM synthesis instrument.

  This class implements two simple FM Pairs summed together, also referred to
  as algorithm 5 of the TX81Z.

  Algorithm 5 is :  4.3--\
                          + -. Out
                    2.1--/

  Control Change Numbers:
    - Modulator Index One = 2
    - Crossfade of Outputs = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - FAdsr 2 & 4 Target = 128

  The basic Chowning/Stanford FM patent expired in 1995, but there exist
  follow-on patents, mostly assigned to Yamaha. If you are of the type who
  should worry about this (making money) worry away.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer, Windows;

type
  TStkWurley = class(TStkFM)
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;
    procedure SetFrequency(Frequency: Single);

    // Set instrument parameters for a particular Frequency.
    procedure NoteOn(Frequency, Amplitude: Single);

    // Start a note with the given Frequency and Amplitude.
    function Tick: Single; // Compute one output sample.
  end;

implementation

constructor TStkWurley.Create;
var
  i: Integer;
begin
  inherited Create(SampleRate);
  FWaves[0] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[1] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[2] := TWavePlayer.Create(SampleRate, 'sinewave.wav');
  FWaves[3] := TWavePlayer.Create(SampleRate, 'fwavblnk.wav');
  FWaves[0].SetOneShot(False);
  FWaves[1].SetOneShot(False);
  FWaves[2].SetOneShot(False);
  FWaves[3].SetOneShot(False);

  setRatio(0, 1.0);
  setRatio(1, 4.0);
  setRatio(2, -510.0);
  setRatio(3, -510.0);

  FGains[0] := __TFM_gains[99];
  FGains[1] := __TFM_gains[82];
  FGains[2] := __TFM_gains[92];
  FGains[3] := __TFM_gains[68];

  FAdsr[0].setAllTimes(0.001, 1.50, 0.0, 0.04);
  FAdsr[1].setAllTimes(0.001, 1.50, 0.0, 0.04);
  FAdsr[2].setAllTimes(0.001, 0.25, 0.0, 0.04);
  FAdsr[3].setAllTimes(0.001, 0.15, 0.0, 0.04);

  twozero.setGain(2.0);
  vibrato.SetFrequency(5.5);
end;

destructor TStkWurley.Destroy;
begin
  inherited Destroy;
end;

procedure TStkWurley.SetFrequency;
begin
  baseFrequency := Frequency;
  FWaves[0].SetFrequency(baseFrequency * ratios[0]);
  FWaves[1].SetFrequency(baseFrequency * ratios[1]);
  FWaves[2].SetFrequency(ratios[2]);  // Note here a 'fixed resonance'.
  FWaves[3].SetFrequency(ratios[3]);
end;

procedure TStkWurley.NoteOn;
begin
  FGains[0] := Amplitude * __TFM_gains[99];
  FGains[1] := Amplitude * __TFM_gains[82];
  FGains[2] := Amplitude * __TFM_gains[82];
  FGains[3] := Amplitude * __TFM_gains[68];
  SetFrequency(Frequency);
  keyOn;
end;

function TStkWurley.Tick: Single;
var
  temp, temp2: Single;
begin
  temp := FGains[1] * FAdsr[1].Tick * FWaves[1].Tick;
  temp := temp * control1;
  FWaves[0].addPhaseOffset(temp);
  FWaves[3].addPhaseOffset(twozero.lastOut);
  temp := FGains[3] * FAdsr[3].Tick * FWaves[3].Tick;
  twozero.Tick(temp);
  FWaves[2].addPhaseOffset(temp);
  temp := (1.0 - (control2 * 0.5)) * FGains[0] * FAdsr[0].Tick * FWaves[0].Tick;
  temp := temp + control2 * 0.5 * FGains[2] * FAdsr[2].Tick * FWaves[2].Tick;
 // Calculate Amplitude modulation and apply it to output.
  temp2 := vibrato.Tick * modDepth;
  temp := temp * (1.0 + temp2);
  lastOutput := temp * 0.5;
  Result := lastOutput;
end;

end.
