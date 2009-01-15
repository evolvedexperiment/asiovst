unit DAV_StkRagamat;

interface

uses stk, instrmnt, reverb, jcrev, drone, sitar, tabla, voicdrum;

const
  ragaUp: array[0..1, 0..11] of integer =
    ((57, 60, 62, 64, 65, 68, 69, 71, 72, 76, 77, 81),
    (52, 54, 55, 57, 59, 60, 63, 64, 66, 67, 71, 72));
  ragaDown: array[0..1, 0..11] of integer =
    ((57, 60, 62, 64, 65, 67, 69, 71, 72, 76, 79, 81),
    (48, 52, 53, 55, 57, 59, 60, 64, 66, 68, 70, 72));

type
  TRagamat = class(tinstrmnt)
  public
    constructor Create(sr: my_float);
    destructor Destroy;
    procedure noteOn(instrument, amplitude: MY_FLOAT);
    procedure controlChange(number: integer; Value: MY_FLOAT);
    procedure tick(var i1, i2: single);
  protected
    port: integer;
    ragaStep, ragaPoint, voicNote: integer;
    t60, drone_prob, note_prob, drum_prob, voic_prob: my_float;
    droneFreqs: array[0..2] of my_float;
    tempo, counter, key: integer;
    drones: array[0..2] of tdrone;
    tabla: ttabla;
    voicdrums: tvoicdrum;
    sitar: tsitar;
    reverbs: array[0..1] of tjcrev;
  end;

implementation

function float_random(max: MY_FLOAT): MY_FLOAT;
 // Return random float between 0.0 and max
begin
  Result := random * max
end;

destructor TRagamat.Destroy;
begin
  inherited Destroy;
  drones[0].Free;
  drones[1].Free;
  drones[2].Free;
  sitar.Free;
  tabla.Free;
  voicDrums.Free;
  reverbs[0].Free;
  reverbs[1].Free;
end;

constructor TRagamat.Create;
begin
  inherited Create(sr);
  t60 := 4.0;  // in seconds
  drone_prob := 0.01;
  note_prob := 0.0;
  drum_prob := 0.0;
  voic_prob := 0.0;
  droneFreqs[0] := 55.0;
  droneFreqs[1] := 82.5;
  droneFreqs[2] := 220.0;
  tempo := 3000;
  counter := 3000;
  key := 0;
  ragaPoint := 6;
  port := -1;

  drones[0] := TDrone.Create(srate, 50.0);
  drones[1] := TDrone.Create(srate, 50.0);
  drones[2] := TDrone.Create(srate, 50.0);
  sitar := TSitar.Create(srate, 50.0);
  voicDrums := TVoicDrum.Create(srate);
  tabla := TTabla.Create(srate);

  reverbs[0] := TJCRev.Create(srate, t60);
  reverbs[0].setEffectMix(0.5);
  reverbs[1] := TJCRev.Create(srate, 2.0);
  reverbs[1].setEffectMix(0.2);

  drones[0].noteOn(droneFreqs[0], 0.1);
  drones[1].noteOn(droneFreqs[1], 0.1);
  drones[2].noteOn(droneFreqs[2], 0.1);

{ MY_FLOAT outSamples[2];
 for (i=0;i<srate;i++) begin /* warm everybody up a little */
   outSamples[0]:=reverbs[0].tick(drones[0].tick + drones[2].tick);
   outSamples[1]:=reverbs[1].tick(1.5 * drones[1].tick);
   output.tickFrame(outSamples);
 end;}
end;

procedure TRagamat.process(var i1, i2: single);
var
  temp, rateScaler: my_float;
begin
{ i1:=reverbs[0].tick(sitar.tick);
 i2:=i1;
 counter:=counter-1;
 if (counter<=0) then
 begin
  rateScaler:=22050.0/srate;
  counter:=round(tempo/rateScaler);
  sitar.noteOn(Midi2Pitch[ragaUp[key][ragaPoint]],0.5+float_random(0.3))
 end;
 exit;

 //x}
  i1 := reverbs[0].tick(drones[0].tick + drones[2].tick + sitar.tick);
  i2 := reverbs[1].tick(1.5 * drones[1].tick + 0.5 * voicDrums.tick + 0.5 * tabla.tick);
 // mix a little left to right and back
  temp := i1;
  i1 := i1 + 0.3 * i2;
  i2 := i2 + 0.3 * temp;
  counter := counter - 1;
  if (counter <= 0) then
   begin
    rateScaler := 22050.0 / srate;
    counter := round(tempo / rateScaler);
    if (float_random(1.0) < drone_prob) then
      drones[0].noteOn(droneFreqs[0] + random(10), 0.1);
    if (float_random(1.0) < drone_prob) then
      drones[1].noteOn(droneFreqs[1] + random(10), 0.1);
    if (float_random(1.0) < drone_prob) then
      drones[2].noteOn(droneFreqs[2] + random(10), 0.1);
    if (float_random(1.0) < note_prob) then
     begin
      temp := float_random(1.0);
      if (temp < 0.1) then
        ragaStep := 0
      else if (temp < 0.5) then
        ragaStep := 1
      else
        ragaStep := -1;
      ragaPoint := ragapoint + ragaStep;
      if (ragaPoint < 0) then
        ragaPoint := ragapoint - (2 * ragaStep);
      if (ragaPoint > 11) then
        ragaPoint := 11;
      if (ragaStep > 0) then
        sitar.noteOn(Midi2Pitch[ragaUp[key][ragaPoint]], 0.05 + float_random(0.3))
      else
        sitar.noteOn(Midi2Pitch[ragaDown[key][ragaPoint]], 0.05 + float_random(0.3));
     end;
    if (float_random(1.0) < voic_prob) then
     begin
      voicNote := round(float_random(11));
      voicDrums.noteOn(voicNote, 0.3 + (0.4 * drum_prob) + float_random(
        0.3 * voic_prob));
     end;
    if (float_random(1.0) < drum_prob) then
     begin
      voicNote := round(float_random(TABLA_NUMWAVES));
      tabla.noteOn(voicNote, 0.2 + (0.2 * drum_prob) + float_random(0.6 * drum_prob));
     end;
   end;
end;

procedure TRagamat.controlChange(number: integer; Value: MY_FLOAT);
var
  norm: my_float;
begin
  norm := Value;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then // 2
    drone_prob := norm
  else if (number = __SK_FootControl_) then // 4
    note_prob := norm
  else if (number = __SK_ModFrequency_) then // 11
    voic_prob := norm
  else if (number = __SK_ModWheel_) then // 1
    drum_prob := norm
  else if (number = 3) then // 3
    tempo := round(11025 - (norm * 128 * 70))
  else if (number = __SK_AfterTouch_Cont_) then
    if norm < 0.5 then
     begin
      key := 1;
      droneFreqs[0] := 55.0;
      droneFreqs[1] := 82.5;
      droneFreqs[2] := 220.0;
     end else
     begin
      key := 0;
      droneFreqs[0] := 82.5;
      droneFreqs[1] := 123.5;
      droneFreqs[2] := 330.0;
     end// 128
  ;
end;

procedure TRagamat.noteOn(instrument, amplitude: MY_FLOAT);
begin
  sitar.noteOn(instrument, amplitude);
end;

end.
