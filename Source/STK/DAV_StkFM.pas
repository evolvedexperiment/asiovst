unit DAV_StkFM;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK abstract TFM synthesis base class.

  This class controls an arbitrary number of FWaves and envelopes, determined
  via a constructor argument.

  Control Change Numbers:
    - Control One = 2
    - Control Two = 4
    - LFO Speed = 11
    - LFO Depth = 1
    - FAdsr 2 & 4 Target = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkAdsr, DAV_StkTwoZero, DAV_StkLfo,
  DAV_StkWavePlayer;

const
  CMaxOperators = 20;

type
  TFM = class(TInstrmnt)
  protected
    FAdsr           : array[0..CMaxOperators - 1] of TAdsr;
    FWaves          : array[0..CMaxOperators - 1] of TWaveplayer;
    FVibrato        : TLfo;
    FTwoZero        : TTwozero;
    FNOperators     : Integer;
    FModDepth       : Single;
    FControl1       : Single;
    FControl2       : Single;
    FBaseFrequency  : Single;
    FGains          : array[0..CMaxOperators - 1] of Single;
    FRatios         : array[0..CMaxOperators - 1] of Single;
    __TFM_gains     : array[0..99] of Single;
    __TFM_susLevels : array[0..15] of Single;
    __TFM_attTimes  : array[0..31] of Single;
  public
    // Class constructor, taking the number of wave/envelope Operators to control.
    constructor Create(SampleRate: Single; Operators: Integer = 4);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all wave and envelope states.
    procedure Clear;

    // Load the rawwave filenames in FWaves.
    procedure LoadWave(waveindex: Integer; filename: string);

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(frequency: Single);

    // Set the frequency ratio for the specified wave.
    procedure SetRatio(waveIndex: Integer; ratio: Single);

    // Set the gain for the specified wave.
    procedure SetGain(waveIndex: Integer; gain: Single);

    // Set the modulation speed in Hz.
    procedure SetModulationSpeed(mSpeed: Single);

    // Set the modulation depth.
    procedure SetModulationDepth(mDepth: Single);

    // Set the value of FControl1.
    procedure SetControl1(cVal: Single);

    // Set the value of FControl2.
    procedure SetControl2(cVal: Single);

    // Start envelopes toward "on" targets.
    procedure KeyOn;

    // Start envelopes toward "off" targets.
    procedure KeyOff;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(amplitude: Single);

    // Pure virtual function ... must be defined in subclasses.
    function Tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(number: Integer; Value: Single);

  end;

implementation

{ TFM }

procedure TFM.Clear;
begin

end;

procedure TFM.ControlChange(number: Integer; Value: Single);
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then // 2
    SetControl1(norm)
  else if (number = __SK_FootControl_) then // 4
    SetControl2(norm)
  else if (number = __SK_ModFrequency_) then // 11
    SetModulationSpeed(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    SetModulationDepth(norm)
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    //FAdsr[0].setTarget( norm );
    FAdsr[1].setTarget(norm);
    //FAdsr[2].setTarget( norm );
    FAdsr[3].setTarget(norm);
   end;
end;

constructor TFM.Create(SampleRate: Single; Operators: Integer);
var
  i: Integer;
  temp: Single;
begin
  inherited Create(SampleRate);
  if (FNOperators <= 0) then
    FNOperators := 4;

  FTwoZero := TTwozero.Create(SampleRate);
  FTwoZero.setB2(-1.0);
  FTwoZero.SetGain(0.0);

  FVibrato := TLfo.Create(SampleRate);
  FVibrato.SetFrequency(6.0);

  for i := 0 to FNOperators - 1 do
   begin
    FRatios[i] := 1.0;
    FGains[i] := 1.0;
    FAdsr[i] := TAdsr.Create(SampleRate);
    ;
   end;

  FModDepth := 0.0;
  FControl1 := 1.0;
  FControl2 := 1.0;
  FBaseFrequency := 440.0;

  temp := 1.0;
  for i := 99 downto 0 do
   begin
    __TFM_gains[i] := temp;
    temp := temp * 0.933033;
   end;

  temp := 1.0;
  for i := 15 downto 0 do
   begin
    __TFM_susLevels[i] := temp;
    temp := temp * 0.707101;
   end;

  temp := 8.498186;
  for i := 0 to 31 do
   begin
    __TFM_attTimes[i] := temp;
    temp := temp * 0.707101;
   end;

end;

destructor TFM.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  FVibrato.Free;
  FTwoZero.Free;
  for i := 0 to FNOperators - 1 do
   begin
    FAdsr[i].Free;
    FWaves[i].Free;
   end;
end;

procedure TFM.KeyOff;
var
  i: Integer;
begin
  for i := 0 to FNOperators - 1 do
    FAdsr[i].KeyOff;
end;

procedure TFM.KeyOn;
var
  i: Integer;
begin
  for i := 0 to FNOperators - 1 do
    FAdsr[i].KeyOn;
end;

procedure TFM.LoadWave(waveIndex: Integer; filename: string);
begin
  FWaves[waveIndex] := TWaveplayer.Create(srate, filename);
end;

procedure TFM.NoteOff(amplitude: Single);
begin
  KeyOff;
end;

procedure TFM.SetControl1(cVal: Single);
begin
  FControl1 := cVal * 2.0;
end;

procedure TFM.SetControl2(cVal: Single);
begin
  FControl2 := cVal * 2.0;
end;

procedure TFM.SetFrequency(frequency: Single);
var
  i: Integer;
begin
  FBaseFrequency := frequency;
  for i := 0 to FNOperators - 1 do
    FWaves[i].SetFrequency(FBaseFrequency * FRatios[i]);
end;

procedure TFM.SetGain(waveIndex: Integer; gain: Single);
begin
  if (waveIndex < 0) then
    exit
  else if (waveIndex >= FNOperators) then
    exit;
  FGains[waveIndex] := gain;
end;

procedure TFM.SetModulationDepth(mDepth: Single);
begin
  FModDepth := mDepth;
end;

procedure TFM.SetModulationSpeed(mSpeed: Single);
begin
  FVibrato.SetFrequency(mSpeed);
end;

procedure TFM.SetRatio(waveIndex: Integer; ratio: Single);
begin
  if (waveIndex < 0) or (waveIndex >= FNOperators) then
    exit;
  FRatios[waveIndex] := ratio;
  if (ratio > 0.0) then
    FWaves[waveIndex].SetFrequency(FBaseFrequency * ratio)
  else
    FWaves[waveIndex].SetFrequency(ratio);
end;

function TFM.Tick: Single;
begin
  Result := 0;
end;

end.
