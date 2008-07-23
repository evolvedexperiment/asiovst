unit EPianoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAVDCommon, DVSTEffect, DVSTModule,
  DVSTCustomModule;

const
  cNumParams  =     12;  // Number of Parameters
  cNumProgs   =      8;  // Number of Programs
  cNumOuts    =      2;  // Number of Outputs
  cNumVoices  =     32;  // Max Polyphony
  cSustain    =    128;
  cSilence    = 0.0001;  // Voice Choking
  cWaveLength = 422414;  // Wave Data Bytes

type
  TVoice = record  //voice state
    delta : Integer;  //sample playback
    frac  : Integer;
    pos   : Integer;
    stop  : Integer;
    loop  : Integer;

    env   : Single;  //envelope
    dec   : Single;

    f0    : Single;   //first-order LPF
    f1    : Single;
    ff    : Single;

    outl  : Single;
    outr  : Single;
    note  : Integer; //remember what note triggered this
  end;

  TKeyGroup = record  // keygroup
    root : Integer;   // MIDI Root Note
    high : Integer;   // Highest Note
    pos  : Integer;
    stop : Integer;
    loop : Integer;
  end;

  TEPianoDataModule = class(TVSTModule)
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleCreate(Sender: TObject);
    function VSTModuleOutputProperties(Sender: TObject; const Index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    fVoices          : array [0..cNumVoices - 1] of TVoice;
    fKeyGroup        : array [0..33] of TKeyGroup;
    fInvSampleRate   : Double;
    fSize            : Integer;
    fSizeVelocity    : Single;
    fTreble          : Single;
    fTrebleFrequency : Single;
    fDeltaLFO        : Single;
    fVelSens         : Single;
    fWidth           : Single;
    fPoly            : Integer;
    fFine            : Single;
    fRandom          : Single;
    fStretch         : Single;
    fOverdrive       : Single;
    fVolume          : Single;
    fMuffle          : Single;
    fMuffleVelocity  : Single;
    fLFO             : array [0..1] of Single;
    fSustain         : Integer;
    fActiveVoices    : Integer;
    fWaves           : PAVDSingleFixedArray;
    procedure Resume;
    procedure Update;
    procedure NoteOn(Note, Velocity: Integer);
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EPianoData, DVSTBasicModule;

procedure TEPianoDataModule.VSTModuleCreate(Sender: TObject);
var
  k, v    : Integer;
  p       : array [0..1] of Integer;
  xf, dxf : Single;
begin
 fInvSampleRate := 1 / SampleRate;
(*
  programs = new mdaEPianoProgram[NPROGS];
  if(programs)
  begin
    //fill patches...
    long i=0;
    fillpatch(i++, "Default", 0.500, 0.500, 0.500, 0.500, 0.500, 0.650, 0.250, 0.500, 0.50, 0.500, 0.146, 0.000);
    fillpatch(i++, "Bright", 0.500, 0.500, 1.000, 0.800, 0.500, 0.650, 0.250, 0.500, 0.50, 0.500, 0.146, 0.500);
    fillpatch(i++, "Mellow", 0.500, 0.500, 0.000, 0.000, 0.500, 0.650, 0.250, 0.500, 0.50, 0.500, 0.246, 0.000);
    fillpatch(i++, "Autopan", 0.500, 0.500, 0.500, 0.500, 0.250, 0.650, 0.250, 0.500, 0.50, 0.500, 0.246, 0.000);
    fillpatch(i++, "Tremolo", 0.500, 0.500, 0.500, 0.500, 0.750, 0.650, 0.250, 0.500, 0.50, 0.500, 0.246, 0.000);
    fillpatch(i++, " ", 0.500, 0.500, 0.500, 0.500, 0.500, 0.650, 0.250, 0.500, 0.50, 0.500, 0.146, 0.000);
    fillpatch(i++, " ", 0.500, 0.500, 0.500, 0.500, 0.500, 0.650, 0.250, 0.500, 0.50, 0.500, 0.146, 0.000);
    fillpatch(i++, " ", 0.500, 0.500, 0.500, 0.500, 0.500, 0.650, 0.250, 0.500, 0.50, 0.500, 0.146, 0.000);
    setProgram(0);
  }

  waves = cEPianoData;
*)

  //Waveform data and keymapping
  fKeyGroup[ 0].root := 36;     fKeyGroup[ 0].high :=  39; // C1
  fKeyGroup[ 3].root := 43;     fKeyGroup[ 3].high :=  45; // G1
  fKeyGroup[ 6].root := 48;     fKeyGroup[ 6].high :=  51; // C2
  fKeyGroup[ 9].root := 55;     fKeyGroup[ 9].high :=  57; // G2
  fKeyGroup[12].root := 60;     fKeyGroup[12].high :=  63; // C3
  fKeyGroup[15].root := 67;     fKeyGroup[15].high :=  69; // G3
  fKeyGroup[18].root := 72;     fKeyGroup[18].high :=  75; // C4
  fKeyGroup[21].root := 79;     fKeyGroup[21].high :=  81; // G4
  fKeyGroup[24].root := 84;     fKeyGroup[24].high :=  87; // C5
  fKeyGroup[27].root := 91;     fKeyGroup[27].high :=  93; // G5
  fKeyGroup[30].root := 96;     fKeyGroup[30].high := 999; // C6

  with fKeyGroup[ 0] do begin pos := 0;      stop := 8476;   loop :=  4400; end;
  with fKeyGroup[ 1] do begin pos := 8477;   stop := 16248;  loop :=  4903; end;
  with fKeyGroup[ 2] do begin pos := 16249;  stop := 34565;  loop :=  6398; end;
  with fKeyGroup[ 3] do begin pos := 34566;  stop := 41384;  loop :=  3938; end;
  with fKeyGroup[ 4] do begin pos := 41385;  stop := 45760;  loop :=  1633; end; // was 1636;
  with fKeyGroup[ 5] do begin pos := 45761;  stop := 65211;  loop :=  5245; end;
  with fKeyGroup[ 6] do begin pos := 65212;  stop := 72897;  loop :=  2937; end;
  with fKeyGroup[ 7] do begin pos := 72898;  stop := 78626;  loop :=  2203; end; // was 2204;
  with fKeyGroup[ 8] do begin pos := 78627;  stop := 100387; loop :=  6368; end;
  with fKeyGroup[ 9] do begin pos := 100388; stop := 116297; loop := 10452; end;
  with fKeyGroup[10] do begin pos := 116298; stop := 127661; loop :=  5217; end; // was 5220;
  with fKeyGroup[11] do begin pos := 127662; stop := 144113; loop :=  3099; end;
  with fKeyGroup[12] do begin pos := 144114; stop := 152863; loop :=  4284; end;
  with fKeyGroup[13] do begin pos := 152864; stop := 173107; loop :=  3916; end;
  with fKeyGroup[14] do begin pos := 173108; stop := 192734; loop :=  2937; end;
  with fKeyGroup[15] do begin pos := 192735; stop := 204598; loop :=  4732; end;
  with fKeyGroup[16] do begin pos := 204599; stop := 218995; loop :=  4733; end;
  with fKeyGroup[17] do begin pos := 218996; stop := 233801; loop :=  2285; end;
  with fKeyGroup[18] do begin pos := 233802; stop := 248011; loop :=  4098; end;
  with fKeyGroup[19] do begin pos := 248012; stop := 265287; loop :=  4099; end;
  with fKeyGroup[20] do begin pos := 265288; stop := 282255; loop :=  3609; end;
  with fKeyGroup[21] do begin pos := 282256; stop := 293776; loop :=  2446; end;
  with fKeyGroup[22] do begin pos := 293777; stop := 312566; loop :=  6278; end;
  with fKeyGroup[23] do begin pos := 312567; stop := 330200; loop :=  2283; end;
  with fKeyGroup[24] do begin pos := 330201; stop := 348889; loop :=  2689; end;
  with fKeyGroup[25] do begin pos := 348890; stop := 365675; loop :=  4370; end;
  with fKeyGroup[26] do begin pos := 365676; stop := 383661; loop :=  5225; end;
  with fKeyGroup[27] do begin pos := 383662; stop := 393372; loop :=  2811; end;
  with fKeyGroup[28] do begin pos := 383662; stop := 393372; loop :=  2811; end; //ghost
  with fKeyGroup[29] do begin pos := 393373; stop := 406045; loop :=  4522; end;
  with fKeyGroup[30] do begin pos := 406046; stop := 414486; loop :=  2306; end;
  with fKeyGroup[31] do begin pos := 406046; stop := 414486; loop :=  2306; end; //ghost
  with fKeyGroup[32] do begin pos := 414487; stop := 422408; loop :=  2169; end;

  //extra xfade looping...
  for k := 0 to 27 do
   begin
    p[0] := fKeyGroup[k].stop;
    p[1] := fKeyGroup[k].stop - fKeyGroup[k].loop;

    xf   := 1;
    dxf  := -0.02;

    while xf > 0 do
     begin
      fWaves[p[0]] := round((1 - xf) * fWaves[p[0]] + xf * fWaves[p[1]]);
      dec(p[0]);
      dec(p[1]);
      xf := xf + dxf;
     end;
   end;

  //initialise...
  for v := 0 to cNumVoices - 1 do
   begin
    fVoices[v].Env := 0.0;
    fVoices[v].Dec := 0.99; //all notes off
   end;

 fVolume       := 0.2;
 fMuffle       := 160.0;
 fSustain      := 0;
 fActivevoices := 0;

(*
  notes[0] = EVENTS_DONE;
  tl         := 0.0;
  tr         := 0.0;

  guiUpdate = 0;

  suspend();
*)
 fLFO[0]    := 0.0;
 fLFO[1]    := 1.0;
 fDeltaLFO  := 0.0;

 Update;
end;

procedure TEPianoDataModule.VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
var
  npos : Integer;
begin
(*
  long npos=0;

  for (long i=0; i<ev.numEvents; i++)
  begin
    if((ev.events[i]).type != kVstMidiType) continue;
    VstMidiEvent* event = (VstMidiEvent* )ev.events[i];
    char* midiData = event.midiData;

    case MidiEvent.midiData[0] and $F0 of //status byte (all channels)
     $80: begin // Note off
           notes[npos++] = event.deltaFrames;  // Delta
           notes[npos++] = midiData[1] and $7F; // Note
           notes[npos++] = 0;                   // Vel
          end;

     $90: begin // Note on
           notes[npos++] = event.deltaFrames;  // Delta
           notes[npos++] = midiData[1] & $7F;   // Note
           notes[npos++] = midiData[2] & $7F;   // Vel
          end;

     $B0: case MidiEvent.midiData[1] of         // Controller
           $01: begin //mod wheel
                 modwhl := 0.0078 * MidiEvent.midiData[2];
                 if (modwhl > 0.05) then //over-ride pan/trem depth
                  begin
                   rmod := modwhl;
                   lmod := modwhl; // LFO depth
                   if (Parameter[4] < 0.5)
                    then rmod = -rmod;
                  end;
                end;

           $07: volume := 0.00002 * sqr(MidiEvent.midiData[2]); // Volume

           $40,           // Sustain Pedal
           $42: begin     // Sustenuto Pedal
                 fSustain := MidiEvent.midiData[2] and $40;
                 if (sustain = 0) then
                  begin
                   notes[npos++] = event.deltaFrames;
                   notes[npos++] = SUSTAIN; //end all sustained notes
                   notes[npos++] = 0;
                  end;
                end;

            else  //all notes off
             begin
              if MidiEvent.midiData[1] > $7A then
               begin
                for v := 0 to cNumVoices - 1
                 do fVoices[v].dec := 0.99;
                fSustain := 0;
                fMuffle  := 160;
               end;
             end;
          end;

     $C0: if (MidiEvent.midiData[1] < NPROGS)              //program change
           then setProgram(MidiEvent.midiData[1]);
    end;

    if (npos>EVENTBUFFER) npos -= 3; //discard events if buffer full!!
    event++; //?
  end;
  notes[npos] = EVENTS_DONE;
  return 1;
*)
end;

procedure TEPianoDataModule.Update;  //parameter change
begin
 fSize   := round(12 * Parameter[2] - 6);

 fTreble := 4 * sqr(Parameter[3]) - 1.0; // Treble Gain
 if (Parameter[3] > 0.5)
  then fTrebleFrequency := 14000
  else fTrebleFrequency := 5000;         // Treble Frequency
 fTrebleFrequency := 1.0 - exp(-fInvSampleRate * fTrebleFrequency);

(*
 rmod := 2 * Parameter[4] - 1.0; // LFO depth
 lmod := rmod;
 if (Parameter[4] < 0.5) then rmod := -rmod;
*)

 fDeltaLFO := 6.283 * fInvSampleRate * Exp(6.22 * Parameter[5] - 2.61); // LFO rate

 fVelSens := 1 + 2 * Parameter[6];
 if (Parameter[6] < 0.25)
  then fVelSens := fVelSens - (0.75 - 3.0 * Parameter[6]);

 fWidth     := 0.03 * Parameter[7];
 fPoly      := 1 + round(31.9 * Parameter[8]);
 fFine      := Parameter[9] - 0.5;
 fRandom    := 0.077 * sqr(Parameter[10]);
 fStretch   := 0.0; //0.000434 * (Parameter[11] - 0.5); parameter re-used for fOverdrive!
 fOverdrive := 1.8 * Parameter[11];
end;

procedure TEPianoDataModule.Resume;
begin
 fInvSampleRate := 1 / SampleRate;
 fDeltaLFO := 6.283 * fInvSampleRate * exp(6.22 * Parameter[5] - 2.61); // LFO rate
 WantEvents(1);
end;

function TEPianoDataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index: Integer;
  var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
begin
 vLabel := 'ePiano';
 Flags := [vppIsStereo];
end;

procedure TEPianoDataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i, event, frame, frames, v : Integer;
  x, l, r, od                : Single;
  note, vel                  : Integer;
begin
  event := 0;
  frame := 0;
  od    := fOverdrive;
 (*
  float* out0 = outputs[0];
  float* out1 = outputs[1];

  while (frame < sampleFrames) do
   begin
    frames = notes[event++];
    if (frames > sampleFrames)
     then frames := sampleFrames;
    frames := frames - frame;
    frame  := frame + frames;

    while (--frames >= 0) do
     begin
      VOICE *V = voice;
      l := 0.0;
      r := 0.0;

      for v :=0 to fActiveVoices - 1 do
       begin
        V.frac := V.frac + V.delta;  //integer-based linear interpolation
        V.pos  := V.pos  + (V.frac shr 16);
        V.frac := V.frac and $FFFF;
        if(V.pos > V.end) V.pos -= V.loop;
        //i = waves[V.pos];
        //i = (i << 7) + (V.frac >> 9) * (waves[V.pos + 1] - i) + $40400000;  //not working on intel mac !?!
    //x = V.env * (*(float * )&i - 3.0);  //fast int.float
  //x = V.env * (float)i / 32768.0;
  i = waves[V.pos] + ((V.frac * (waves[V.pos + 1] - waves[V.pos])) >> 16);
  x = V.env * (float)i / 32768.0;

    V.env = V.env * V.dec;  //envelope

        if(x>0.0) begin x -= od * x * x;  if(x < -V.env) x = -V.env; end; //+= 0.5 * x * x; end; //fOverdrive

        l += V.outl * x;
        r += V.outr * x;

        V++;
       end;
      tl := tl + fTrebleFrequency * (l - tl);  // Treble Boost
      tr := tr + fTrebleFrequency * (r - tr);
      r  := r + fTreble * (r - tr);
      l  := l + fTreble * (l - tl);

      fLFO[0] := fLFO[0] + fDeltaLFO * fLFO[1];   // LFO for tremolo and autopan
      fLFO[1] := fLFO[1] - fDeltaLFO * fLFO[0];
      l       := l + l * lmod * fLFO[1];
      r       := r + r * rmod * fLFO[1];  // worth making all these local variables?

      *out0++ = l;
      *out1++ = r;
    end;
*)
    if (frame < sampleFrames) then
     begin
      if (fActiveVoices = 0) and (Parameter[4] > 0.5) then
       begin
        fLFO[0] := -0.7071;
        fLFO[1] :=  0.7071;
       end;              // reset LFO phase - good idea?
(*
      note := notes[event++];
      vel  := notes[event++];
*)
      noteOn(note, vel);
     end;

(*
 if (abs(tl) < 1E-10) then tl := 0; //anti-denormal
 if (abs(tr) < 1E-10) then tr := 0;
*)

 for v := 0 to fActiveVoices - 1 do
  if (fVoices[v].env < cSilence) then
   begin
    dec(fActiveVoices);
    fVoices[v] := fVoices[fActiveVoices];
   end;
(*
 notes[0] := EVENTS_DONE;  //mark events buffer as done
*)
end;

procedure TEPianoDataModule.NoteOn(Note, Velocity : Integer);
var
  l           : Single;
  v, vl, k, s : Integer;
begin
 l  := 99.0;
 vl :=0;
 if (Velocity > 0) then
   begin
    if (fActiveVoices < fPoly) then //add a note
     begin
      vl := fActiveVoices;
      inc(fActiveVoices);
      fVoices[vl].f0 := 0;
      fVoices[vl].f1 := 0;
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

    k := sqr(note - 60);
    l := fFine + fRandom * ((k mod 13) - 6.5);  // Random & Fine tune
    if (note > 60)
     then l := l + fStretch * k;                      // Stretch

    s := fSize;
    if (velocity > 40)
     then s := s + round(fSizeVelocity * (velocity - 40));

    k := 0;
    while (note > (fKeyGroup[k].high + s)) do k := k + 3;  //find keygroup
    l := l + (note - fKeyGroup[k].root); // Pitch
    l := 32000 * fInvSampleRate * exp(0.05776226505 * l);
    fVoices[vl].delta := round(65536.0 * l);
    fVoices[vl].frac  := 0;

    if (velocity > 48) then Inc(k); // Mid Velocity Sample
    if (velocity > 80) then Inc(k); // High Velocity Sample
    fVoices[vl].pos  := fKeyGroup[k].pos;
    fVoices[vl].stop := fKeyGroup[k].stop - 1;
    fVoices[vl].loop := fKeyGroup[k].loop;

    fVoices[vl].env  := (3 + 2 * fVelSens) * Power(0.0078 * velocity, fVelSens); //velocity

    if (note > 60)
     then fVoices[vl].env := fVoices[vl].env * exp(0.01 * (60 - note));   // new! high notes quieter

    l := 50.0 + sqr(Parameter[4]) * fMuffle + fMuffleVelocity * (velocity - 64); // Muffle
    if (l < (55.0 + 0.4 * note)) then l := 55.0 + 0.4 * note;
    if (l > 210.0) then l := 210.0;
    fVoices[vl].ff := sqr(l) * fInvSampleRate;
    fVoices[vl].note := note; //note.pan
    if (note <  12) then note := 12;
    if (note > 108) then note := 108;
    l := fVolume;

    fVoices[vl].outr := l + l * fWidth * (note - 60);
    fVoices[vl].outl := l + l - fVoices[vl].outr;

    if (Note < 44) then Note := 44; // limit max decay length
    fVoices[vl].dec := exp(-fInvSampleRate * exp(-1 + 0.03 * note - 2 * Parameter[0]));
   end
  else //note off
   begin
    for v := 0 to cNumVoices - 1 do
     if (fVoices[v].note = note) then // any voices playing that note?
      begin
       if (fSustain = 0)
        then fVoices[v].dec  := exp(-fInvSampleRate * exp(6 + 0.01 * note - 5 * Parameter[1]))
        else fVoices[v].note := cSustain;
     end;
   end;
end;

procedure TEPianoDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 fInvSampleRate := 1 / SampleRate;
end;

end.

(*
void mdaEPiano::fillpatch(long p, char *name, float p0, float p1, float p2, float p3, float p4,
                      float p5, float p6, float p7, float p8, float p9, float p10,float p11)
begin
  strcpy(programs[p].name, name);
  programs[p].Parameter[0] = p0;  programs[p].Parameter[1] = p1;
  programs[p].Parameter[2] = p2;  programs[p].Parameter[3] = p3;
  programs[p].Parameter[4] = p4;  programs[p].Parameter[5] = p5;
  programs[p].Parameter[6] = p6;  programs[p].Parameter[7] = p7;
  programs[p].Parameter[8] = p8;  programs[p].Parameter[9] = p9;
  programs[p].Parameter[10]= p10; programs[p].Parameter[11] = p11;
end;


void mdaEPiano::getParameterDisplay(VstInt32 index, char *text)
begin
  char string[16];
  
  switch(index)
  begin
    case  2:
    case  3: 
    case  9: sprintf(string, "%.0f", 100.0 * Parameter[index] - 50.0); break;
   
    case  4: if(Parameter[index] > 0.5) 
               sprintf(string, "Trem %.0f", 200.0 * Parameter[index] - 100.0);
             else
               sprintf(string, "Pan %.0f", 100.0 - 200.0 * Parameter[index]); break;

    case  5: sprintf(string, "%.2f", (float)exp(6.22 * Parameter[5] - 2.61)); break; //LFO Hz
    case  7: sprintf(string, "%.0f", 200.0 * Parameter[index]); break;
    case  8: sprintf(string, "%ld", fPoly); break;
    case 10: sprintf(string, "%.1f",  50.0 * Parameter[index] * Parameter[index]); break;
    case 11: sprintf(string, "%.1f", 100.0 * Parameter[index]); break;
    default: sprintf(string, "%.0f", 100.0 * Parameter[index]);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;


void mdaEPiano::guiGetDisplay(VstInt32 index, char *label)
begin
  getParameterName(index,  label);
  strcat(label, " = ");
  getParameterDisplay(index, label + strlen(label));
  getParameterLabel(index, label + strlen(label));
end;
*)
