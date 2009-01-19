unit DAV_StkRagamat;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkReverb, DAV_StkJCRev, DAV_StkDrone,
  DAV_StkSitar, DAV_StkTabla, DAV_StkVoiceDrum;

const
  ragaUp: array[0..1, 0..11] of Integer =
    ((57, 60, 62, 64, 65, 68, 69, 71, 72, 76, 77, 81),
    (52, 54, 55, 57, 59, 60, 63, 64, 66, 67, 71, 72));
  ragaDown: array[0..1, 0..11] of Integer =
    ((57, 60, 62, 64, 65, 67, 69, 71, 72, 76, 79, 81),
    (48, 52, 53, 55, 57, 59, 60, 64, 66, 68, 70, 72));

type
  TStkRagamat = class(TStkInstrument)
  protected
    FPport: Integer;
    FRagaStep, ragaPoint, voicNote: Integer;
    FT60, FDrone_prob, FNote_prob, FDrum_prob, FVoic_prob: Single;
    FDroneFreqs: array[0..2] of Single;
    FTempo, counter, key: Integer;
    FDrones: array[0..2] of TDrone;
    FTabla: TTabla;
    FVoicdrums: TVoicdrum;
    FSitar: TSitar;
    FReverbs: array[0..1] of TJCRev;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;
    procedure noteOn(instrument, amplitude: Single);
    procedure controlChange(number: Integer; Value: Single);
    procedure tick(var i1, i2: Single);
  end;

implementation

function float_random(max: Single): Single;
 // Return random float between 0.0 and max
begin
  Result := random * max;
end;

destructor TStkRagamat.Destroy;
begin
  inherited Destroy;
  FDrones[0].Free;
  FDrones[1].Free;
  FDrones[2].Free;
  FSitar.Free;
  FTabla.Free;
  FVoicdrums.Free;
  FReverbs[0].Free;
  FReverbs[1].Free;
end;

constructor TStkRagamat.Create;
begin
  inherited Create(SampleRate);
  FT60 := 4.0;  // in seconds
  FDrone_prob := 0.01;
  FNote_prob := 0.0;
  FDrum_prob := 0.0;
  FVoic_prob := 0.0;
  FDroneFreqs[0] := 55.0;
  FDroneFreqs[1] := 82.5;
  FDroneFreqs[2] := 220.0;
  FTempo := 3000;
  counter := 3000;
  key := 0;
  ragaPoint := 6;
  FPport := -1;

  FDrones[0] := TDrone.Create(srate, 50.0);
  FDrones[1] := TDrone.Create(srate, 50.0);
  FDrones[2] := TDrone.Create(srate, 50.0);
  FSitar := TSitar.Create(srate, 50.0);
  FVoicdrums := TVoicdrum.Create(srate);
  FTabla := TTabla.Create(srate);

  FReverbs[0] := TJCRev.Create(srate, FT60);
  FReverbs[0].setEffectMix(0.5);
  FReverbs[1] := TJCRev.Create(srate, 2.0);
  FReverbs[1].setEffectMix(0.2);

  FDrones[0].noteOn(FDroneFreqs[0], 0.1);
  FDrones[1].noteOn(FDroneFreqs[1], 0.1);
  FDrones[2].noteOn(FDroneFreqs[2], 0.1);

{ Single outSamples[2];
 for (i=0;i<srate;i++) begin /* warm everybody up a little */
   outSamples[0]:=FReverbs[0].tick(FDrones[0].tick + FDrones[2].tick);
   outSamples[1]:=FReverbs[1].tick(1.5 * FDrones[1].tick);
   output.tickFrame(outSamples);
 end;}
end;

procedure TStkRagamat.process(var i1, i2: Single);
var
  temp, rateScaler: Single;
begin
{ i1:=FReverbs[0].tick(FSitar.tick);
 i2:=i1;
 counter:=counter-1;
 if (counter<=0) then
 begin
  rateScaler:=22050.0/srate;
  counter:=round(FTempo/rateScaler);
  FSitar.noteOn(Midi2Pitch[ragaUp[key][ragaPoint]],0.5+float_random(0.3))
 end;
 exit;

 //x}
  i1 := FReverbs[0].tick(FDrones[0].tick + FDrones[2].tick + FSitar.tick);
  i2 := FReverbs[1].tick(1.5 * FDrones[1].tick + 0.5 *
    FVoicdrums.tick + 0.5 * FTabla.tick);
 // mix a little left to right and back
  temp := i1;
  i1 := i1 + 0.3 * i2;
  i2 := i2 + 0.3 * temp;
  counter := counter - 1;
  if (counter <= 0) then
   begin
    rateScaler := 22050.0 / srate;
    counter := round(FTempo / rateScaler);
    if (float_random(1.0) < FDrone_prob) then
      FDrones[0].noteOn(FDroneFreqs[0] + random(10), 0.1);
    if (float_random(1.0) < FDrone_prob) then
      FDrones[1].noteOn(FDroneFreqs[1] + random(10), 0.1);
    if (float_random(1.0) < FDrone_prob) then
      FDrones[2].noteOn(FDroneFreqs[2] + random(10), 0.1);
    if (float_random(1.0) < FNote_prob) then
     begin
      temp := float_random(1.0);
      if (temp < 0.1) then
        FRagaStep := 0
      else if (temp < 0.5) then
        FRagaStep := 1
      else
        FRagaStep := -1;
      ragaPoint := ragapoint + FRagaStep;
      if (ragaPoint < 0) then
        ragaPoint := ragapoint - (2 * FRagaStep);
      if (ragaPoint > 11) then
        ragaPoint := 11;
      if (FRagaStep > 0) then
        FSitar.noteOn(Midi2Pitch[ragaUp[key][ragaPoint]], 0.05 +
          float_random(0.3))
      else
        FSitar.noteOn(Midi2Pitch[ragaDown[key][ragaPoint]],
          0.05 + float_random(0.3));
     end;
    if (float_random(1.0) < FVoic_prob) then
     begin
      voicNote := round(float_random(11));
      FVoicdrums.noteOn(voicNote, 0.3 + (0.4 * FDrum_prob) +
        float_random(0.3 * FVoic_prob));
     end;
    if (float_random(1.0) < FDrum_prob) then
     begin
      voicNote := round(float_random(TABLA_NUMWAVES));
      FTabla.noteOn(voicNote, 0.2 + (0.2 * FDrum_prob) +
        float_random(0.6 * FDrum_prob));
     end;
   end;
end;

procedure TStkRagamat.controlChange(number: Integer; Value: Single);
var
  norm: Single;
begin
  norm := Value;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then // 2
    FDrone_prob := norm
  else if (number = __SK_FootControl_) then // 4
    FNote_prob := norm
  else if (number = __SK_ModFrequency_) then // 11
    FVoic_prob := norm
  else if (number = __SK_ModWheel_) then // 1
    FDrum_prob := norm
  else if (number = 3) then // 3
    FTempo := round(11025 - (norm * 128 * 70))
  else if (number = __SK_AfterTouch_Cont_) then
    if norm < 0.5 then
     begin
      key := 1;
      FDroneFreqs[0] := 55.0;
      FDroneFreqs[1] := 82.5;
      FDroneFreqs[2] := 220.0;
     end else
     begin
      key := 0;
      FDroneFreqs[0] := 82.5;
      FDroneFreqs[1] := 123.5;
      FDroneFreqs[2] := 330.0;
     end// 128
  ;
end;

procedure TStkRagamat.noteOn(instrument, amplitude: Single);
begin
  FSitar.noteOn(instrument, amplitude);
end;

end.
