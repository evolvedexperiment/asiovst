unit DAV_StkModalBar;

{/***************************************************/
/*! \class TModalBar
    \brief STK resonant bar instrument class.

    This class implements a number of different
    struck bar instruments.  It inherits from the
    Modal class.

    Control Change Numbers:
       - Stick Hardness:=2
       - Stick Position:=4
       - Vibrato Gain:=11
       - Vibrato Frequency:=1
       - Volume:=128
       - Modal Presets:=3
         - Marimba:=0
         - Vibraphone:=1
         - Agogo:=2
         - Wood1:=3
         - Reso:=4
         - Wood2:=5
         - Beats:=6
         - Two Fixed:=7
         - Clump:=8

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, modal, waveplayer, Math;

  // Presets:
  //     First line:  relative modal frequencies (negative number is
  //                  a fixed mode that doesn't scale with frequency
  //     Second line: resonances of the modes
  //     Third line:  mode volumes
  //     Fourth line: stickHardness, strikePosition, and direct stick
  //                  gain (mixed directly into the output)
const
  presets: array[0..8, 0..3, 0..3] of single =
    (((1.0, 3.99, 10.65, -2443),    // Marimba
    (0.9996, 0.9994, 0.9994, 0.999),
    (0.04, 0.01, 0.01, 0.008),
    (0.429688, 0.445312, 0.093750, 0)),

    ((1.0, 2.01, 3.9, 14.37),     // Vibraphone
    (0.99995, 0.99991, 0.99992, 0.9999),
    (0.025, 0.015, 0.015, 0.015),
    (0.390625, 0.570312, 0.078125, 0)),
    ((1.0, 4.08, 6.669, -3725.0),    // Agogo
    (0.999, 0.999, 0.999, 0.999),
    (0.06, 0.05, 0.03, 0.02),
    (0.609375, 0.359375, 0.140625, 0)),
    ((1.0, 2.777, 7.378, 15.377),    // Wood1
    (0.996, 0.994, 0.994, 0.99),
    (0.04, 0.01, 0.01, 0.008),
    (0.460938, 0.375000, 0.046875, 0)),
    ((1.0, 2.777, 7.378, 15.377),    // Reso
    (0.99996, 0.99994, 0.99994, 0.9999),
    (0.02, 0.005, 0.005, 0.004),
    (0.453125, 0.250000, 0.101562, 0)),
    ((1.0, 1.777, 2.378, 3.377),    // Wood2
    (0.996, 0.994, 0.994, 0.99),
    (0.04, 0.01, 0.01, 0.008),
    (0.312500, 0.445312, 0.109375, 0)),
    ((1.0, 1.004, 1.013, 2.377),    // Beats
    (0.9999, 0.9999, 0.9999, 0.999),
    (0.02, 0.005, 0.005, 0.004),
    (0.398438, 0.296875, 0.070312, 0)),
    ((1.0, 4.0, -1320.0, -3960.0),    // 2Fix
    (0.9996, 0.999, 0.9994, 0.999),
    (0.04, 0.01, 0.01, 0.008),
    (0.453125, 0.453125, 0.070312, 0)),
    ((1.0, 1.217, 1.475, 1.729),    // Clump
    (0.999, 0.999, 0.999, 0.999),
    (0.03, 0.03, 0.03, 0.03),
    (0.390625, 0.570312, 0.078125, 0)));

type
  TModalBar = class(TModal)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set stick hardness (0.0 - 1.0).
    procedure setStickHardness(hardness: MY_FLOAT);

  //! Set stick position (0.0 - 1.0).
    procedure setStrikePosition(position: MY_FLOAT);

  //! Select a bar preset (currently modulo 9).
    procedure setPreset(preset: integer);

  //! Set the modulation (vibrato) depth.
//  procedure setModulationDepth(mDepth:MY_FLOAT);

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);
  end;

implementation

constructor TModalBar.Create;
begin
  inherited Create(sr);
  wave := TWavePlayer.Create(srate, 'c:\stk\marmstk1.wav');
  wave.SetOneShot(False);
  wave.setFrequency(22050);
  // Set the resonances for preset 0 (marimba).
  setPreset(0);
end;

destructor TModalBar.Destroy;
begin
  inherited Destroy;
  wave.Free;
end;

procedure TModalBar.setStickHardness;
begin
  stickHardness := hardness;
  if (hardness < 0.0) then
    stickHardness := 0.0
  else if (hardness > 1.0) then
    stickHardness := 1.0;
  wave.setFrequency((0.25 * power(4.0, stickHardness)));
  masterGain := 0.1 + (1.8 * stickHardness);
end;

procedure TModalBar.setStrikePosition;
var
  temp, temp2: my_float;
begin
  strikePosition := position;
  if (position < 0.0) then
    strikePosition := 0.0
  else if (position > 1.0) then
    strikePosition := 1.0;

  // Hack only first three modes.
  temp2 := position * PI;
  temp := sin(temp2);
  setModeGain(0, 0.12 * temp);

  temp := sin(0.05 + (3.9 * temp2));
  setModeGain(1, -0.03 * temp);

  temp := sin(-0.05 + (11 * temp2));
  setModeGain(2, 0.11 * temp);
end;

procedure TModalBar.setPreset;
var
  i, temp: integer;
begin
  temp := (preset mod 9);
  for i := 0 to nModes - 1 do
   begin
    setRatioAndRadius(i, presets[temp][0][i], presets[temp][1][i]);
    setModeGain(i, presets[temp][2][i]);
   end;

  setStickHardness(presets[temp][3][0]);
  setStrikePosition(presets[temp][3][1]);
  directGain := presets[temp][3][2];

  if (temp = 1) then // vibraphone
    vibratoGain := 0.2
  else
    vibratoGain := 0.0;
end;

procedure TModalBar.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_StickHardness_) then // 2
    setStickHardness(norm)
  else if (number = __SK_StrikePosition_) then // 4
    setStrikePosition(norm)
  else if (number = __SK_ProphesyRibbon_) then // 3
    setPreset(round(Value))
{  else if (number = __SK_ModWheel_) then // 1
    directGain:=norm}
  else if (number = 11) then // 11
    vibratoGain := norm * 0.3
  else if (number = __SK_ModFrequency_) then // 1
    vibrato.setFrequency(norm * 12.0)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    envelope.setTarget(norm);
end;

end.

