unit DAV_StkFM;

{
/***************************************************/
/*! \class TFM
    \brief STK abstract TFM synthesis base class.

    This class controls an arbitrary number of
    waves and envelopes, determined via a
    constructor argument.

    Control Change Numbers:
       - Control One:=2
       - Control Two:=4
       - LFO Speed:=11
       - LFO Depth:=1
       - ADSR 2 & 4 Target:=128

    The basic Chowning/Stanford TFM patent expired
    in 1995, but there exist follow-on patents,
    mostly assigned to Yamaha.  If you are of the
    type who should worry about this (making
    money) worry away.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkAdsr, DAV_StkTwoZero, DAV_StkLfo,
  DAV_StkWavePlayer;

const
  maxOperators = 20;

type
  TFM = class(TInstrmnt)
  public
  //! Class constructor, taking the number of wave/envelope operators to control.
    constructor Create(sr: my_float; operators: integer = 4);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all wave and envelope states.
    procedure Clear;

  //! Load the rawwave filenames in waves.
    procedure loadWave(waveindex: integer; filename: string);

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: my_float);

  //! Set the frequency ratio for the specified wave.
    procedure setRatio(waveIndex: integer; ratio: MY_FLOAT);

  //! Set the gain for the specified wave.
    procedure setGain(waveIndex: integer; gain: MY_FLOAT);

  //! Set the modulation speed in Hz.
    procedure setModulationSpeed(mSpeed: MY_FLOAT);

  //! Set the modulation depth.
    procedure setModulationDepth(mDepth: MY_FLOAT);

  //! Set the value of control1.
    procedure setControl1(cVal: MY_FLOAT);

  //! Set the value of control2.
    procedure setControl2(cVal: MY_FLOAT);

  //! Start envelopes toward "on" targets.
    procedure keyOn;

  //! Start envelopes toward "off" targets.
    procedure keyOff;

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Pure virtual function ... must be defined in subclasses.
    function tick: my_float;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    adsr: array[0..maxoperators - 1] of tadsr;
    waves: array[0..maxoperators - 1] of twaveplayer;
    vibrato: tlfo;
    TwoZero: ttwozero;
    nOperators: integer;
    modDepth, control1, control2, baseFrequency: my_float;
    gains, ratios: array[0..maxoperators - 1] of my_float;
    __TFM_gains: array[0..99] of my_float;
    __TFM_susLevels: array[0..15] of my_float;
    __TFM_attTimes: array[0..31] of my_float;
  end;

implementation

{ TFM }

procedure TFM.Clear;
begin

end;

procedure TFM.controlChange(number: integer; Value: MY_FLOAT);
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then // 2
    setControl1(norm)
  else if (number = __SK_FootControl_) then // 4
    setControl2(norm)
  else if (number = __SK_ModFrequency_) then // 11
    setModulationSpeed(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    setModulationDepth(norm)
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    //adsr[0].setTarget( norm );
    adsr[1].setTarget(norm);
    //adsr[2].setTarget( norm );
    adsr[3].setTarget(norm);
   end;
end;

constructor TFM.Create(sr: my_float; operators: integer);
var
  i: integer;
  temp: my_float;
begin
  inherited Create(sr);
  if (nOperators <= 0) then
    nOperators := 4;

  twozero := TTwoZero.Create(sr);
  twozero.setB2(-1.0);
  twozero.setGain(0.0);

  vibrato := TLFO.Create(sr);
  vibrato.setFrequency(6.0);

  for i := 0 to nOperators - 1 do
   begin
    ratios[i] := 1.0;
    gains[i] := 1.0;
    adsr[i] := TADSR.Create(sr);
    ;
   end;

  modDepth := 0.0;
  control1 := 1.0;
  control2 := 1.0;
  baseFrequency := 440.0;

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
  i: integer;
begin
  inherited Destroy;
  vibrato.Free;
  twozero.Free;
  for i := 0 to nOperators - 1 do
   begin
    adsr[i].Free;
    waves[i].Free;
   end;
end;

procedure TFM.keyOff;
var
  i: integer;
begin
  for i := 0 to nOperators - 1 do
    adsr[i].keyOff;
end;

procedure TFM.keyOn;
var
  i: integer;
begin
  for i := 0 to nOperators - 1 do
    adsr[i].keyOn;
end;

procedure TFM.loadWave(waveIndex: Integer; filename: string);
begin
  waves[waveIndex] := TWavePlayer.Create(srate, filename);
end;

procedure TFM.noteOff(amplitude: MY_FLOAT);
begin
  keyOff;
end;

procedure TFM.setControl1(cVal: MY_FLOAT);
begin
  control1 := cVal * 2.0;
end;

procedure TFM.setControl2(cVal: MY_FLOAT);
begin
  control2 := cVal * 2.0;
end;

procedure TFM.setFrequency(frequency: my_float);
var
  i: integer;
begin
  baseFrequency := frequency;
  for i := 0 to nOperators - 1 do
    waves[i].setFrequency(baseFrequency * ratios[i]);
end;

procedure TFM.setGain(waveIndex: integer; gain: MY_FLOAT);
begin
  if (waveIndex < 0) then
    exit
  else if (waveIndex >= nOperators) then
    exit;
  gains[waveIndex] := gain;
end;

procedure TFM.setModulationDepth(mDepth: MY_FLOAT);
begin
  modDepth := mDepth;
end;

procedure TFM.setModulationSpeed(mSpeed: MY_FLOAT);
begin
  vibrato.setFrequency(mSpeed);
end;

procedure TFM.setRatio(waveIndex: integer; ratio: MY_FLOAT);
begin
  if (waveIndex < 0) or (waveIndex >= nOperators) then
    exit;
  ratios[waveIndex] := ratio;
  if (ratio > 0.0) then
    waves[waveIndex].setFrequency(baseFrequency * ratio)
  else
    waves[waveIndex].setFrequency(ratio);
end;

function TFM.tick: my_float;
begin
  Result := 0;
end;

end.
