unit DAV_StkFMVoices;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK singing FM synthesis instrument.

   This class implements 3 carriers and a common modulator, also referred to as
   algorithm 6 of the TX81Z.

   Algorithm 6 is :
                       /- 1 -\
                    4-+-- 2 --+- Out
                       \- 3 -/

   Control Change Numbers:
      - Vowel = 2
      - Spectral FTilt = 4
      - LFO Speed = 11
      - LFO Depth = 1
      - Adsr 2 & 4 Target = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkFm, DAV_StkWavePlayer, DAV_StkPhoneMes;

type
  TStkFMVoices = class(TFM)
  protected
    FCurrentVowel : Integer;
    FPhonems      : TPhonemes;
    FMods, FTilt  : array[0..2] of Single;
  public
    constructor Create(SampleRate: Single);
    destructor Destroy;
    procedure SetFrequency(AFrequency: Single);
    procedure NoteOn(AFrequency, AAmplitude: Single);
    function Ttick: Single;
    procedure ControlChange(Number: Integer; Value: Single);
  end;

implementation

constructor TStkFMVoices.Create;
begin
  inherited Create(SampleRate);
  FPhonems := TPhonemes.Create(SampleRate);
  Waves[0] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  Waves[1] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  Waves[2] := TWavePlayer.Create(srate, 'c:\stk\sinewave.wav');
  Waves[3] := TWavePlayer.Create(srate, 'c:\stk\fwavblnk.wav');
  Waves[0].SetOneShot(False);
  Waves[1].SetOneShot(False);
  Waves[2].SetOneShot(False);
  Waves[3].SetOneShot(False);

  SetRatio(0, 2.00);
  SetRatio(1, 4.00);
  SetRatio(2, 12.0);
  SetRatio(3, 1.00);

  Gains[3] := __TFM_gains[80];

  Adsr[0].setAllTimes(0.05, 0.05, __TFM_susLevels[15], 0.05);
  Adsr[1].setAllTimes(0.05, 0.05, __TFM_susLevels[15], 0.05);
  Adsr[2].setAllTimes(0.05, 0.05, __TFM_susLevels[15], 0.05);
  Adsr[3].setAllTimes(0.01, 0.01, __TFM_susLevels[15], 0.5);

  Twozero.setGain(0.0);
  ModDepth := 0.005;
  FCurrentVowel := 0;
  FTilt[0] := 1.0;
  FTilt[1] := 0.5;
  FTilt[2] := 0.2;
  FMods[0] := 1.0;
  FMods[1] := 1.1;
  FMods[2] := 1.1;
  BaseFrequency := 110.0;
  SetFrequency(110.0);
end;

destructor TStkFMVoices.Destroy;
begin
  inherited Destroy;
  FPhonems.Free;
end;

procedure TStkFMVoices.SetFrequency;
var
  temp, temp2: Single;
  i, tempi: Integer;
begin
  temp2 := 0.0;
  tempi := 0;
  i := 0;

  if (FCurrentVowel < 32) then
   begin
    i := FCurrentVowel;
    temp2 := 0.9;
   end
  else if (FCurrentVowel < 64) then
   begin
    i := FCurrentVowel - 32;
    temp2 := 1.0;
   end
  else if (FCurrentVowel < 96) then
   begin
    i := FCurrentVowel - 64;
    temp2 := 1.1;
   end
  else if (FCurrentVowel <= 128) then
   begin
    i := FCurrentVowel - 96;
    temp2 := 1.2;
   end;

  BaseFrequency := AFrequency;
  temp := (temp2 * FPhonems.formantFrequency(i, 0) / BaseFrequency) + 0.5;
  tempi := round(temp);
  SetRatio(0, tempi);
  temp := (temp2 * FPhonems.formantFrequency(i, 1) / BaseFrequency) + 0.5;
  tempi := round(temp);
  SetRatio(1, tempi);
  temp := (temp2 * FPhonems.formantFrequency(i, 2) / BaseFrequency) + 0.5;
  tempi := round(temp);
  SetRatio(2, tempi);
  Gains[0] := 1.0;
  Gains[1] := 1.0;
  Gains[2] := 1.0;
end;

procedure TStkFMVoices.NoteOn;
begin
  SetFrequency(AFrequency);
  FTilt[0] := AAmplitude;
  FTilt[1] := AAmplitude * AAmplitude;
  FTilt[2] := FTilt[1] * AAmplitude;
  keyOn;
end;

function TStkFMVoices.Ttick: Single;
var
  temp, temp2: Single;
begin
  temp := Gains[3] * Adsr[3].Ttick * Waves[3].Ttick;
  temp2 := Vibrato.Ttick * ModDepth * 0.1;

  Waves[0].SetFrequency(BaseFrequency * (1.0 + temp2) * ratios[0]);
  Waves[1].SetFrequency(BaseFrequency * (1.0 + temp2) * ratios[1]);
  Waves[2].SetFrequency(BaseFrequency * (1.0 + temp2) * ratios[2]);
  Waves[3].SetFrequency(BaseFrequency * (1.0 + temp2) * ratios[3]);

  Waves[0].addPhaseOffset(temp * FMods[0]);
  Waves[1].addPhaseOffset(temp * FMods[1]);
  Waves[2].addPhaseOffset(temp * FMods[2]);
  Waves[3].addPhaseOffset(Twozero.lastOut);
  Twozero.Ttick(temp);
  temp := Gains[0] * FTilt[0] * Adsr[0].Ttick * Waves[0].Ttick;
  temp := temp + Gains[1] * FTilt[1] * Adsr[1].Ttick * Waves[1].Ttick;
  temp := temp + Gains[2] * FTilt[2] * Adsr[2].Ttick * Waves[2].Ttick;

  Result := temp * 0.33;
end;

procedure TStkFMVoices.ControlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (Number = __SK_Breath_) then // 2
    Gains[3] := __TFM_gains[round(norm * 99.9)]
  else if (Number = __SK_FootControl_) then
   begin // 4
    FCurrentVowel := round(norm * 128.0);
    SetFrequency(BaseFrequency);
   end
  else if (Number = __SK_ModFrequency_) then // 11
    setModulationSpeed(norm * 12.0)
  else if (Number = __SK_ModWheel_) then // 1
    setModulationDepth(norm)
  else if (Number = __SK_AfterTouch_Cont_) then
   begin // 128
    FTilt[0] := norm;
    FTilt[1] := norm * norm;
    FTilt[2] := FTilt[1] * norm;
   end;
end;

end.
