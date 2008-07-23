unit PianoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAVDCommon, DVSTEffect, DVSTModule,
  DVSTCustomModule;

const
  cNumVoices  =     32;  // Max Polyphony
  cSustain    =    128;
  cSilence    = 0.0001;  // Voice Choking
  cWaveLength = 422414;  // Wave Data Bytes

type
  TVoice = record        // Voice State
    delta : Integer;     // Sample Playback
    frac  : Integer;
    pos   : Integer;
    stop  : Integer;
    loop  : Integer;

    env   : Single;      // Envelope
    dec   : Single;

    f0    : Single;      // First-order LPF
    f1    : Single;
    ff    : Single;

    outl  : Single;
    outr  : Single;
    note  : Integer;     // remember what note triggered this
  end;

  TKeyGroup = record     // keygroup
    root : Integer;      // MIDI Root Note
    high : Integer;      // Highest Note
    pos  : Integer;
    stop : Integer;
    loop : Integer;
  end;

  TPianoDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    function VSTModuleOutputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
  private
    fInvSampleRate   : Double;
    fSize            : Integer;
    fSizeVelocity    : Single;
    fMuffVelocity    : Single;
    fVolume          : Single;
    fVelSens         : Single;
    fFine            : Single;
    fRandom          : Single;
    fStretch         : Single;
    fMuffle          : Single;
    fCPos            : Integer;
    fSustain         : Integer;
    fActiveVoices    : Integer;
    fNotePos         : Integer;

    fCDepth          : Single;
    fCMax            : Byte;
    fTrim            : Single;
    fWidth           : Single;
    fPoly            : Integer;
    fNotes           : array [0..128, 0..2] of Byte;
    fVoices          : array [0..cNumVoices - 1] of TVoice;
    fKeyGroup        : array [0..33] of TKeyGroup;
    fCombFilter      : PAVDSingleFixedArray;
    procedure Update;
    procedure noteOn(Note, Velocity: Integer);
  public
  end;

implementation

{$R *.DFM}

uses
  Math, PianoData;

procedure TPianoDataModule.VSTModuleCreate(Sender: TObject);
var
  v : Integer;
begin
(*
 Parameter[ 0] := 0.5;  // Decay
 Parameter[ 1] := 0.5;  // Release
 Parameter[ 2] := 0.5;  // Hardness

 Parameter[ 3] := 0.5;  // Vel > Hard
 Parameter[ 4] := 1.0;  // Muffle
 Parameter[ 5] := 0.5;  // Vel > Muffle

 Parameter[ 6] := 0.33; // Vel Curve
 Parameter[ 7] := 0.50; // Stereo
 Parameter[ 8] := 0.33; // Max fPoly

 Parameter[ 9] := 0.5;  // Tune
 Parameter[10] := 0.0;  // Random
 Parameter[11] := 0.5;  // Stretch
*)

 fCMax := $7F;

 with Programs[0] do
  begin
   Parameter[ 0] := 0.5;
   Parameter[ 1] := 0.5;
   Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 0.803;
   Parameter[ 5] := 0.251;
   Parameter[ 6] := 0.376;
   Parameter[ 7] := 0.5;
   Parameter[ 8] := 0.33;
   Parameter[ 9] := 0.5;
   Parameter[10] := 0.246;
   Parameter[11] := 0.5;
  end;

 with Programs[1] do
  begin
   Parameter[ 0] := 0.5;
   Parameter[ 1] := 0.5;
   Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 0.751;
   Parameter[ 5] := 0;
   Parameter[ 6] := 0.452;
   Parameter[ 7] := 0;
   Parameter[ 8] := 0;
   Parameter[ 9] := 0.5;
   Parameter[10] := 0;
   Parameter[11] := 0.5;
  end;

 with Programs[2] do
  begin
   Parameter[ 0] := 0.902;
   Parameter[ 1] := 0.399;
   Parameter[ 2] := 0.623;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 1;
   Parameter[ 5] := 0.331;
   Parameter[ 6] := 0.299;
   Parameter[ 7] := 0.499;
   Parameter[ 8] := 0.33;
   Parameter[ 9] := 0.5;
   Parameter[10] := 0;
   Parameter[11] := 0.5;
  end;

 with Programs[3] do
  begin
   Parameter[ 0] := 0.399;
   Parameter[ 1] := 0.251;
   Parameter[ 2] := 1;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 0.672;
   Parameter[ 5] := 0.124;
   Parameter[ 6] := 0.127;
   Parameter[ 7] := 0.249;
   Parameter[ 8] := 0.33;
   Parameter[ 9] := 0.5;
   Parameter[10] := 0.283;
   Parameter[11] := 0.667;
  end;

 with Programs[4] do
  begin
   Parameter[ 0] := 0.648;
   Parameter[ 1] := 0.5;
   Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 0.298;
   Parameter[ 5] := 0.602;
   Parameter[ 6] := 0.55;
   Parameter[ 7] := 0.85;
   Parameter[ 8] := 0.356;
   Parameter[ 9] := 0.5;
   Parameter[10] := 0.339;
   Parameter[11] := 0.66;
  end;

 with Programs[5] do
  begin
   Parameter[ 0] := 0.500;
   Parameter[ 1] := 0.602;
   Parameter[ 2] := 0;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 0.304;
   Parameter[ 5] := 0.2;
   Parameter[ 6] := 0.336;
   Parameter[ 7] := 0.651;
   Parameter[ 8] := 0.33;
   Parameter[ 9] := 0.5;
   Parameter[10] := 0.317;
   Parameter[11] := 0.5;
  end;

 with Programs[6] do
  begin
   Parameter[ 0] := 0.45;
   Parameter[ 1] := 0.598;
   Parameter[ 2] := 0.626;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 0.603;
   Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.174;
   Parameter[ 7] := 0.331;
   Parameter[ 8] := 0.33;
   Parameter[ 9] := 0.5;
   Parameter[10] := 0.421;
   Parameter[11] := 0.801;
  end;

 with Programs[7] do
  begin
   Parameter[ 0] := 0.05;
   Parameter[ 1] := 0.957;
   Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.5;
   Parameter[ 4] := 0.299;
   Parameter[ 5] := 1;
   Parameter[ 6] := 0;
   Parameter[ 7] := 0.5;
   Parameter[ 8] := 0.33;
   Parameter[ 9] := 0.45;
   Parameter[10] := 0.718;
   Parameter[11] := 0;
  end;

(*
  waves = pianoData;
*)

 //Waveform data and keymapping is hard-wired in *this* version
 with fKeyGroup[ 0] do begin root := 36; high := 37;  pos := 0;      Stop := 36275;  loop := 14774; end;
 with fKeyGroup[ 1] do begin root := 40; high := 41;  pos := 36278;  Stop := 83135;  loop := 16268; end;
 with fKeyGroup[ 2] do begin root := 43; high := 45;  pos := 83137;  Stop := 146756; loop := 33541; end;
 with fKeyGroup[ 3] do begin root := 48; high := 49;  pos := 146758; Stop := 204997; loop := 21156; end;
 with fKeyGroup[ 4] do begin root := 52; high := 53;  pos := 204999; Stop := 244908; loop := 17191; end;
 with fKeyGroup[ 5] do begin root := 55; high := 57;  pos := 244910; Stop := 290978; loop := 23286; end;
 with fKeyGroup[ 6] do begin root := 60; high := 61;  pos := 290980; Stop := 342948; loop := 18002; end;
 with fKeyGroup[ 7] do begin root := 64; high := 65;  pos := 342950; Stop := 391750; loop := 19746; end;
 with fKeyGroup[ 8] do begin root := 67; high := 69;  pos := 391752; Stop := 436915; loop := 22253; end;
 with fKeyGroup[ 9] do begin root := 72; high := 73;  pos := 436917; Stop := 468807; loop := 8852;  end;
 with fKeyGroup[10] do begin root := 76; high := 77;  pos := 468809; Stop := 492772; loop := 9693;  end;
 with fKeyGroup[11] do begin root := 79; high := 81;  pos := 492774; Stop := 532293; loop := 10596; end;
 with fKeyGroup[12] do begin root := 84; high := 85;  pos := 532295; Stop := 560192; loop := 6011;  end;
 with fKeyGroup[13] do begin root := 88; high := 89;  pos := 560194; Stop := 574121; loop := 3414;  end;
 with fKeyGroup[14] do begin root := 93; high := 999; pos := 574123; Stop := 586343; loop := 2399;  end;

 //initialize...
for v := 0 to cNumVoices - 1 do
 begin
  fVoices[v].env := 0.0;
  fVoices[v].dec := 0.99; // all notes off
 end;
(*
 fNotes[0]       := EVENTS_DONE;
*)

 fVolume         := 0.2;
 fMuffle         := 160.0;
 fCPos           := 0;
 fSustain        := 0;
 fActiveVoices   := 0;

 Update;
 GetMem(fCombFilter, 256 * SizeOf(Single));
end;

procedure TPianoDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if assigned(fCombFilter) then Dispose(fCombFilter);
end;

function TPianoDataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index: Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
begin
 result := False;
 if (index < numOutputs) then
  begin
   if (index mod 2 = 1)
    then vLabel := 'Piano R'
    else vLabel := 'Piano L';
   Flags := [vppIsActive];
   if (index < 2) then Flags := Flags + [vppIsStereo]; // make channel 1+2 stereo
   result := True;
  end;
end;

procedure TPianoDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 Update;
end;

procedure TPianoDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  event, frame, frames, v : Integer;
  x, l, r : Single;
  i       : Integer;
begin
 Event := 0;
 Frame := 0;

(*
 float* out0 = Outputs[0, Sample];
 float* out1 = Outputs[1, Sample];
*)
 while (Frame < sampleFrames) do
  begin
   frames := fNotes[Event][0];
   if (frames > sampleFrames)
    then frames := sampleFrames;
   frames := frames - Frame;
   Frame  := Frame + frames;

(*
   while (--frames >= 0)
    begin
     fVoice *V = fVoice;
     l := 0.0;
     r := 0.0;

     for v := 0 to fActiveVoices - 1 do
      begin
       V.frac := V.frac + V.delta;  // integer-based linear interpolation
       V.pos  := V.pos + V.frac shr 16;
       V.frac := V.frac and $FFFF;
       if (V.pos > V.end)
        then V.pos := V.pos - V.loop;

       i := waves[V.pos] + ((V.frac * (waves[V.pos + 1] - waves[V.pos])) shr 16);
       x := V.env * i / 32768;

       //x = V.env * (*(float * )&i - 3.0);       // fast int.float

       V.env := sqr(V.env);                       // Envelope
       V.f0  := V.f0 + V.ff * (x + V.f1 - V.f0);  // Muffle filter
       V.f1  := x;

       l     := l + V.outl * V.f0;
       r     := r + V.outr * V.f0;

{
if(!(l > -2.0) || !(l < 2.0))
begin
  printf("what is this shit?   %ld,  %f,  %f\n", i, x, V.f0);
  l = 0.0;
end;
if(!(r > -2.0) || !(r < 2.0))
begin
  r = 0.0;
end;
}

       V++;
      end;

     comb[fCPos] := l + r;
     inc(fCPos);
     fCPos := fCPos and cmax;
     x := fCDepth * comb[fCPos];  // Stereo Simulator

     Outputs[0, Sample] := l + x;
     Outputs[1, Sample] := r - x;
    end;
*)

   if (Frame < SampleFrames) then
    begin
     noteOn(fNotes[event][1], fNotes[event][2]);
     inc(event);
    end;
  end;
 for v := 0 to fActiveVoices - 1 do
  if fVoices[v].env < cSilence then
   begin
    dec(fActiveVoices);
    fVoices[v] := fVoices[fActiveVoices];
   end;
 fNotePos := 0;
end;

procedure TPianoDataModule.noteOn(Note, Velocity : Integer);
var
  l  : Single;
  v, vl, k, s : Integer;
begin
 l  := 99;
 vl := 0;

 if Velocity > 0 then
  begin
   if (fActiveVoices < fPoly) then //add a note
    begin
     vl := fActiveVoices;
     inc(fActiveVoices);
    end
   else //steal a note
    begin
     for v := 0 to fPoly - 1 do  //find quietest voice
      begin
       if (fVoices[v].env < l) then
        begin
         l  := fVoices[v].env;
         vl := v;
        end;
      end;
    end;

   k := sqr(Note - 60);
   l := fFine + fRandom * ((k mod 13) - 6.5);  // Random & Fine tune
   if (Note > 60) then l := l + fStretch * k;  // Stretch

   s := fSize;
   if (Velocity > 40)
    then s := s + round(fSizeVelocity * (Velocity - 40));  

   k := 0;
   while (Note > (fKeyGroup[k].high + s)) do Inc(k);  // Find Keygroup

   l := l + (Note - fKeyGroup[k].root);               // Pitch
   l := 22050.0 * fInvSampleRate * exp(0.05776226505 * l);
   fVoices[vl].delta := round(65536.0 * l);
   fVoices[vl].frac  := 0;
   fVoices[vl].pos   := fKeyGroup[k].pos;
   fVoices[vl].stop  := fKeyGroup[k].stop;
   fVoices[vl].loop  := fKeyGroup[k].loop;

   fVoices[vl].env   := (0.5 + fVelSens) * Power(0.0078 * Velocity, fVelSens); // Velocity

   l := 50.0 + sqr(Parameter[4]) * fMuffle + fMuffVelocity * (Velocity - 64); // Muffle
   if l < (55 + 0.25 * Note) then l := 55 + 0.25 * Note;
   if l > 210 then l := 210;
   fVoices[vl].ff := sqr(l) * fInvSampleRate;
   fVoices[vl].f0 := 0;
   fVoices[vl].f1 := 0;

   fVoices[vl].Note := Note;          // Note -> Pan
   if (Note <  12) then Note := 12;
   if (Note > 108) then Note := 108;
   l := fVolume * fTrim;
   fVoices[vl].outr := l + l * fWidth * (Note - 60);
   fVoices[vl].outl := l + l - fVoices[vl].outr;

   if (Note < 44) then Note := 44;    // limit max decay length
   l := 2.0 * Parameter[0];
   if (l < 1)
    then l := l + 0.25 - 0.5 * Parameter[0];
   fVoices[vl].dec := exp(-fInvSampleRate * exp(-0.6 + 0.033 * Note - l));
  end
 else // Note off
  begin
   for v := 0 to cNumVoices - 1 do
    if fVoices[v].Note = Note then    // any voices playing that Note?
     if (fSustain = 0) then
      begin
       if (Note < 94) or (Note = fSustain) // no release on highest Notes
        then fVoices[v].dec := exp(-fInvSampleRate * exp(2 + 0.017 * Note - 2.0 * Parameter[1]));
      end
     else fVoices[v].Note := fSustain;
 end;
end;

procedure TPianoDataModule.VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
var
  npos, v : Integer;
begin
 with MidiEvent do
  case MidiData[0] and $F0 of //status byte (all channels)
   $80: begin // Note off
         fNotes[fNotePos, 0] := deltaFrames;            // Delta
         fNotes[fNotePos, 1] := midiData[1] and $7F;    // Note
         fNotes[fNotePos, 2] := 0;                      // Velocity
         inc(fNotePos);
        end;

   $90: begin // Note on
         fNotes[fNotePos, 0] := deltaFrames;            // Delta
         fNotes[fNotePos, 1] := midiData[1] and $7F;    // Note
         fNotes[fNotePos, 2] := midiData[2] and $7F;    // Velocity
         inc(fNotePos);
        end;

   $B0: case midiData[1] of                             // Controller
         $01,                                           // Mod Wheel
         $43: fMuffle := 0.01 * sqr(127 - midiData[2]); // Soft Pedal

         $07: fVolume := 0.00002 * sqr(midiData[2]);    // Volume
         $40,                                           // Sustain Pedal
         $42: begin                                     // Sustenuto Pedal
               fSustain := midiData[2] and $40;
               if (fSustain = 0) then
                begin
                 fNotes[fNotePos, 0] := deltaFrames;
                 fNotes[fNotePos, 1] := fSustain;       // end all sustained Notes
                 fNotes[fNotePos, 2] := 0;
                 inc(fNotePos);
                end;
              end;

        else  // all Notes off
         if (midiData[1] > $7A) then
          begin
           for v := 0 to cNumVoices - 1 do
            begin
             fVoices[v].dec := 0.99;
             fSustain       := 0;
             fMuffle        := 160.0;
            end;
          end;
        end;
   $C0: // Program Change
      if (midiData[1] < numPrograms) then SetProgram(midiData[1]);
  end;
end;

procedure TPianoDataModule.VSTModuleResume(Sender: TObject);
begin
 fInvSampleRate := 1 / SampleRate;

 if (SampleRate > 64000.0)
   then fCMax := $FF
   else fCMax := $7F;
 FillChar(fCombFilter[0], SizeOf(Single) * 256, 0);

 wantEvents(1);
end;

procedure TPianoDataModule.Update;  //parameter change
begin
 fSize         := round(12 * Parameter[2] - 6);
 fSizeVelocity := 0.12 * Parameter[3];
 fMuffVelocity := sqr(Parameter[5]) * 5;

 fVelSens := 1 + 2 * Parameter[6];
 if (Parameter[6] < 0.25)
  then fVelSens := fVelSens - (0.75 - 3.0 * Parameter[6]);

 fFine    := Parameter[9] - 0.5;
 fRandom  := 0.077 * sqr(Parameter[10]);
 fStretch := 0.000434 * (Parameter[11] - 0.5);

 fCDepth  := sqr(Parameter[7]);
 fTrim    := 1.50 - 0.79 * fCDepth;
 fWidth   := 0.04 * Parameter[7];
 if (fWidth > 0.03) then fWidth := 0.03;

 fPoly    := 8 + round(24.9 * Parameter[8]);
end;

(*
procedure TPianoDataModule.fillpatch(long p, char *name, float p0, float p1, float p2, float p3, float p4,
                                     float p5, float p6, float p7, float p8, float p9, float p10,float p11)
begin
  strcpy(Programs[p].name, name);
  Programs[p].Parameter[ 0] := p0;  Programs[p].Parameter[ 1] := p1;
  Programs[p].Parameter[ 2] := p2;  Programs[p].Parameter[ 3] := p3;
  Programs[p].Parameter[ 4] := p4;  Programs[p].Parameter[ 5] := p5;
  Programs[p].Parameter[ 6] := p6;  Programs[p].Parameter[ 7] := p7;
  Programs[p].Parameter[ 8] := p8;  Programs[p].Parameter[ 9] := p9;
  Programs[p].Parameter[10] := p10; Programs[p].Parameter[11] := p11;
end;
*)


end.

(*
void mdaPiano::getParameterDisplay(VstInt32 index, char *text)
begin
  char string[16];

  switch(index)
  begin
    case  4: sprintf(string, "%.0f", 100.0 - 100.0 * Parameter[index]); break;
    case  7: sprintf(string, "%.0f", 200.0 * Parameter[index]); break;
    case  8: sprintf(string, "%ld", fPoly); break;
    case 10: sprintf(string, "%.1f",  50.0 * Parameter[index] * Parameter[index]); break;
    case  2:
    case  9:
    case 11: sprintf(string, "%+.1f", 100.0 * Parameter[index] -  50.0); break;
    default: sprintf(string, "%.0f", 100.0 * Parameter[index]);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;


void mdaPiano::guiGetDisplay(VstInt32 index, char *label)
begin
  getParameterName(index,  label);
  strcat(label, " = ");
  getParameterDisplay(index, label + strlen(label));
  getParameterLabel(index, label + strlen(label));
end;
*)
