unit DX10DM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TDX10DataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
  private
    fTune     : Single;
    fRatF     : Single;
    fRati     : Integer;
    fRatio    : Single;
    fDepth    : Array [0..1] of Single;
    fVelSens  : Single;
    fVibrato  : Single;
    fCAtt     : Single;
    fCDec     : Single;
    fCRel     : Single;
    fLFO      : Array [0..1] of Single;
    fMDec     : Single;
    fMRel     : Single;
    fRich     : Single;
    fModMix   : Single;
    fDeltaLFO : Single;
    procedure Update;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDX10DataModule.VSTModuleCreate(Sender: TObject);
var
  i : Integer;
begin
 i  := 0;

(*
 programs = new mdaDX10Program[NPROGS];
 if(programs)
  begin                                //Att     Dec     Rel   | Rat C   Rat    Att     Dec     Sus     Rel     Vel   | Vib     Oct     Fine    fRich    Thru    LFO
    fillpatch(i++, "Bright E.Piano", 0.000, 0.650, 0.441, 0.842, 0.329, 0.230, 0.800, 0.050, 0.800, 0.900, 0.000, 0.500, 0.500, 0.447, 0.000, 0.414);
    fillpatch(i++, "Jazz E.Piano",   0.000, 0.500, 0.100, 0.671, 0.000, 0.441, 0.336, 0.243, 0.800, 0.500, 0.000, 0.500, 0.500, 0.178, 0.000, 0.500);
    fillpatch(i++, "E.Piano Pad",    0.000, 0.700, 0.400, 0.230, 0.184, 0.270, 0.474, 0.224, 0.800, 0.974, 0.250, 0.500, 0.500, 0.428, 0.836, 0.500);
    fillpatch(i++, "Fuzzy E.Piano",  0.000, 0.700, 0.400, 0.320, 0.217, 0.599, 0.670, 0.309, 0.800, 0.500, 0.263, 0.507, 0.500, 0.276, 0.638, 0.526);
    fillpatch(i++, "Soft Chimes",    0.400, 0.600, 0.650, 0.760, 0.000, 0.390, 0.250, 0.160, 0.900, 0.500, 0.362, 0.500, 0.500, 0.401, 0.296, 0.493);
    fillpatch(i++, "Harpsichord",    0.000, 0.342, 0.000, 0.280, 0.000, 0.880, 0.100, 0.408, 0.740, 0.000, 0.000, 0.600, 0.500, 0.842, 0.651, 0.500);
    fillpatch(i++, "Funk Clav",      0.000, 0.400, 0.100, 0.360, 0.000, 0.875, 0.160, 0.592, 0.800, 0.500, 0.000, 0.500, 0.500, 0.303, 0.868, 0.500);
    fillpatch(i++, "Sitar",          0.000, 0.500, 0.704, 0.230, 0.000, 0.151, 0.750, 0.493, 0.770, 0.500, 0.000, 0.400, 0.500, 0.421, 0.632, 0.500);
    fillpatch(i++, "Chiff Organ",    0.600, 0.990, 0.400, 0.320, 0.283, 0.570, 0.300, 0.050, 0.240, 0.500, 0.138, 0.500, 0.500, 0.283, 0.822, 0.500);
    fillpatch(i++, "Tinkle",         0.000, 0.500, 0.650, 0.368, 0.651, 0.395, 0.550, 0.257, 0.900, 0.500, 0.300, 0.800, 0.500, 0.000, 0.414, 0.500);
    fillpatch(i++, "Space Pad",      0.000, 0.700, 0.520, 0.230, 0.197, 0.520, 0.720, 0.280, 0.730, 0.500, 0.250, 0.500, 0.500, 0.336, 0.428, 0.500);
    fillpatch(i++, "Koto",           0.000, 0.240, 0.000, 0.390, 0.000, 0.880, 0.100, 0.600, 0.740, 0.500, 0.000, 0.500, 0.500, 0.526, 0.480, 0.500);
    fillpatch(i++, "Harp",           0.000, 0.500, 0.700, 0.160, 0.000, 0.158, 0.349, 0.000, 0.280, 0.900, 0.000, 0.618, 0.500, 0.401, 0.000, 0.500);
    fillpatch(i++, "Jazz Guitar",    0.000, 0.500, 0.100, 0.390, 0.000, 0.490, 0.250, 0.250, 0.800, 0.500, 0.000, 0.500, 0.500, 0.263, 0.145, 0.500);
    fillpatch(i++, "Steel Drum",     0.000, 0.300, 0.507, 0.480, 0.730, 0.000, 0.100, 0.303, 0.730, 1.000, 0.000, 0.600, 0.500, 0.579, 0.000, 0.500);
    fillpatch(i++, "Log Drum",       0.000, 0.300, 0.500, 0.320, 0.000, 0.467, 0.079, 0.158, 0.500, 0.500, 0.000, 0.400, 0.500, 0.151, 0.020, 0.500);
    fillpatch(i++, "Trumpet",        0.000, 0.990, 0.100, 0.230, 0.000, 0.000, 0.200, 0.450, 0.800, 0.000, 0.112, 0.600, 0.500, 0.711, 0.000, 0.401);
    fillpatch(i++, "Horn",           0.280, 0.990, 0.280, 0.230, 0.000, 0.180, 0.400, 0.300, 0.800, 0.500, 0.000, 0.400, 0.500, 0.217, 0.480, 0.500);
    fillpatch(i++, "Reed 1",         0.220, 0.990, 0.250, 0.170, 0.000, 0.240, 0.310, 0.257, 0.900, 0.757, 0.000, 0.500, 0.500, 0.697, 0.803, 0.500);
    fillpatch(i++, "Reed 2",         0.220, 0.990, 0.250, 0.450, 0.070, 0.240, 0.310, 0.360, 0.900, 0.500, 0.211, 0.500, 0.500, 0.184, 0.000, 0.414);
    fillpatch(i++, "Violin",         0.697, 0.990, 0.421, 0.230, 0.138, 0.750, 0.390, 0.513, 0.800, 0.316, 0.467, 0.678, 0.500, 0.743, 0.757, 0.487);
    fillpatch(i++, "Chunky Bass",    0.000, 0.400, 0.000, 0.280, 0.125, 0.474, 0.250, 0.100, 0.500, 0.500, 0.000, 0.400, 0.500, 0.579, 0.592, 0.500);
    fillpatch(i++, "E.Bass",         0.230, 0.500, 0.100, 0.395, 0.000, 0.388, 0.092, 0.250, 0.150, 0.500, 0.200, 0.200, 0.500, 0.178, 0.822, 0.500);
    fillpatch(i++, "Clunk Bass",     0.000, 0.600, 0.400, 0.230, 0.000, 0.450, 0.320, 0.050, 0.900, 0.500, 0.000, 0.200, 0.500, 0.520, 0.105, 0.500);
    fillpatch(i++, "Thick Bass",     0.000, 0.600, 0.400, 0.170, 0.145, 0.290, 0.350, 0.100, 0.900, 0.500, 0.000, 0.400, 0.500, 0.441, 0.309, 0.500);
    fillpatch(i++, "Sine Bass",      0.000, 0.600, 0.490, 0.170, 0.151, 0.099, 0.400, 0.000, 0.900, 0.500, 0.000, 0.400, 0.500, 0.118, 0.013, 0.500);
    fillpatch(i++, "Square Bass",    0.000, 0.600, 0.100, 0.320, 0.000, 0.350, 0.670, 0.100, 0.150, 0.500, 0.000, 0.200, 0.500, 0.303, 0.730, 0.500);
    fillpatch(i++, "Upright Bass 1", 0.300, 0.500, 0.400, 0.280, 0.000, 0.180, 0.540, 0.000, 0.700, 0.500, 0.000, 0.400, 0.500, 0.296, 0.033, 0.500);
    fillpatch(i++, "Upright Bass 2", 0.300, 0.500, 0.400, 0.360, 0.000, 0.461, 0.070, 0.070, 0.700, 0.500, 0.000, 0.400, 0.500, 0.546, 0.467, 0.500);
    fillpatch(i++, "Harmonics",      0.000, 0.500, 0.500, 0.280, 0.000, 0.330, 0.200, 0.000, 0.700, 0.500, 0.000, 0.500, 0.500, 0.151, 0.079, 0.500);
    fillpatch(i++, "Scratch",        0.000, 0.500, 0.000, 0.000, 0.240, 0.580, 0.630, 0.000, 0.000, 0.500, 0.000, 0.600, 0.500, 0.816, 0.243, 0.500);
    fillpatch(i++, "Syn Tom",        0.000, 0.355, 0.350, 0.000, 0.105, 0.000, 0.000, 0.200, 0.500, 0.500, 0.000, 0.645, 0.500, 1.000, 0.296, 0.500);

    setProgram(0);
  end;
*)

 //initialise...
(*
 for i := 0 to NVOICES - 1 do
  begin
   voice[i].env   := 0;
   voice[i].car   := 0;
   voice[i].dcar  := 0;
   voice[i].mod0  := 0;
   voice[i].mod1  := 0;
   voice[i].dmod  := 0;
   voice[i].fCDec := 0.99; //all notes off
  end;
 notes[0] = EVENTS_DONE;
*)
 fLFO[0]       := 0;
 fLFO[1]       := 0;
 fDeltaLFO     := 0;
(*
 fModWheel     := 0;
 fPitchBend    := 1;
 fVolume       := 0.0035;
 fSustain      := 0
 fActiveVoices := 0;
 K             := 0;

  Update;
  Suspend;
*)
end;

procedure TDX10DataModule.Update;
var
  ifs : Single;
begin
 ifs := 1 / SampleRate;

 fTune := (8.175798915644 * ifs * Power(2, trunc(Parameter[11] * 6.9) - 2.0));

 fRati := trunc(40.1 * sqr(Parameter[3]));
 if (Parameter[4] < 0.5)
  then fRatF := 0.2 * sqr(Parameter[4])
  else
   case round(8.9 * Parameter[4]) of
      4: fRatF := 0.25;
      5: fRatF := 0.33333333;
      6: fRatF := 0.50;
      7: fRatF := 0.66666667; 
    else fRatF := 0.75;
   end;
 fRatio := 1.570796326795 * (fRati + fRatF);

 fDepth[0] := 0.0002 * sqr(Parameter[5]);
 fDepth[1] := 0.0002 * sqr(Parameter[7]);

 fVelSens := Parameter[9];
 fVibrato := 0.001 * sqr(Parameter[10]);

 fCAtt := 1 - exp(-ifs * exp(8 - 8 * Parameter[0]));
 if Parameter[1] > 0.98
  then fCDec := 1
  else fCDec := exp(-ifs * exp(5 - 8 * Parameter[1]));
 fCRel :=       exp(-ifs * exp(5 - 5 * Parameter[2]));
 fMDec := 1.0 - exp(-ifs * exp(6 - 7 * Parameter[6]));
 fMRel := 1.0 - exp(-ifs * exp(5 - 8 * Parameter[8]));

 fRich     := 0.5 - 3 * sqr(Parameter[13]);  // -1.0 + 2 * Parameter[13];
 fModMix   := 0.25 * sqr(Parameter[14]);
 fDeltaLFO := 628.3 * ifs * 25 * sqr(Parameter[15]); // these params not in original DX10
end;

procedure TDX10DataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  event, frame, frames, v : Integer;
  o, x, e, mw, w, m       : Single;
  k, note, vel            : Integer;
begin
(*
 float* out1 = outputs[0];
 float* out2 = outputs[1];
 event := 0;
 frame := 0;
 mw    := MW;
 w     := fRich;
 m     := fModMix;
 k     := K;

 if (fActiveVoices > 0) or (notes[event] < sampleFrames) then //detect & bypass completely empty blocks
  begin
    while(frame<sampleFrames)
    begin
      frames = notes[event++];
      if(frames>sampleFrames) frames = sampleFrames;
      frames -= frame;
      frame += frames;

      while(--frames>=0)  //would be faster with voice loop outside frame loop!
      begin                   //but then each voice would need it's own LFO...
        VOICE *V = voice;
        o = 0.0;

        if(--k<0)
        begin
          fLFO[0] += fDeltaLFO * fLFO[1]; //sine LFO
          fLFO[1] -= fDeltaLFO * fLFO[0];
          mw = fLFO[1] * (fModWheel + fVibrato);
          k=100;
        end;

        for (v=0; v<NVOICES; v++) //for each voice
        begin
          e := V->env;
          if (e > SILENCE) then //**** this is the synth ****
          begin
            V->env  := e * V->fCDec; //decay & release
            V->cenv := V->cenv + V->fCAtt * (e - V->cenv); //attack

            x = V->dmod * V->mod0 - V->mod1; //could add more modulator blocks like
            V->mod1 := V->mod0;               //this for a wider range of FM sounds
            V->mod0 := x;
            V->menv := V->menv + V->fMDec * (V->mlev - V->menv);

            x := V->car + V->dcar + x * V->menv + mw; //carrier phase
            while (x >  1) do x := x - 2;  //wrap phase
            while (x < -1) do x := x + 2;
            V->car = x;
            o := o + V->cenv * (m * V->mod1 + (x + x * x * x * (w * x * x - 1.0 - w))); 
          end;      //amp env //mod thru-mix //5th-order sine approximation

         ///  xx = x * x;
         ///  x + x + x * xx * (xx - 3.0);

          V++;
        end;
        *out1++ = o;
        *out2++ = o;
      end;

      if(frame<sampleFrames) //next note on/off
      begin
        long note := notes[event++];
        long vel  := notes[event++];
        noteOn(note, vel);
      end;
    end;
  
    fActiveVoices = NVOICES;
    for(v=0; v<NVOICES; v++)
    begin
      if(voice[v].env < SILENCE)  //choke voices that have finished
      begin
        voice[v].env = voice[v].cenv = 0.0;
        fActiveVoices--;
      end;
      if(voice[v].menv < SILENCE) voice[v].menv = voice[v].mlev = 0.0;
    end;
  end;
  else //completely empty block
  begin
    while(--sampleFrames >= 0)
    begin
      *out1++ = 0.0;
      *out2++ = 0.0;
    end;
  end;
  K  := k;
  MW := mw; //remember these so vibrato speed not buffer size dependant!
  notes[0] = EVENTS_DONE;
*)
end;

procedure TDX10DataModule.VSTModuleResume(Sender: TObject);
begin
(*
  DECLARE_VST_DEPRECATED (wantEvents) ();
  fLFO[0] = 0;
  fLFO[1] = 1; //reset LFO phase
*)
end;

end.

(*
void mdaDX10::fillpatch(long Power, char *name, 
                     float p0,  float p1,  float p2,  float p3,  float p4,  float p5, 
                     float p6,  float p7,  float p8,  float p9,  float p10, float p11, 
                     float p12, float p13, float p14, float p15)
begin
  strcpy(programs[Power].name, name);
  programs[Power].Parameter[0] = p0;    programs[Power].Parameter[1] = p1;
  programs[Power].Parameter[2] = p2;    programs[Power].Parameter[3] = p3;
  programs[Power].Parameter[4] = p4;    programs[Power].Parameter[5] = p5;
  programs[Power].Parameter[6] = p6;    programs[Power].Parameter[7] = p7;
  programs[Power].Parameter[8] = p8;    programs[Power].Parameter[9] = p9;
  programs[Power].Parameter[10] = p10;  programs[Power].Parameter[11] = p11;
  programs[Power].Parameter[12] = p12;  programs[Power].Parameter[13] = p13;
  programs[Power].Parameter[14] = p14;  programs[Power].Parameter[15] = p15;
end;

bool mdaDX10::getOutputProperties(VstInt32 index, VstPinProperties* properties)
begin
  if(index < NOUTS)
  begin
    sprintf(properties->label, "DX10");
    properties->flags = kVstPinIsActive;
    if(index<2) properties->flags |= kVstPinIsStereo; //make channel 1+2 stereo
    return true;
  end;
  return false;
end;

bool mdaDX10::getProgramNameIndexed(VstInt32 category, VstInt32 index, char* text)
begin
  if(index < NPROGS)
  begin
    strcpy(text, programs[index].name);
    return true;
  end;
  return false;
end;


bool mdaDX10::copyProgram(VstInt32 destination)
begin
  if(destination<NPROGS)
  begin
    programs[destination] = programs[curProgram];
    return true;
  end;
  return false;
end;

void mdaDX10::getParameterDisplay(VstInt32 index, char *text)
begin
  char string[16];
  
  switch(index)
  begin
    case  3: sprintf(string, "%.0f", fRati); break;
    case  4: sprintf(string, "%.3f", fRatF); break;
    case 11: sprintf(string, "%ld", (long)(Parameter[index] * 6.9) - 3); break;
    case 12: sprintf(string, "%.0f", 200.0 * Parameter[index] - 100.0); break;
    case 15: sprintf(string, "%.2f", 25.0 * Parameter[index] * Parameter[index]); break;
    default: sprintf(string, "%.0f", 100.0 * Parameter[index]);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;

void mdaDX10::noteOn(long note, long velocity)
begin
  float l = 1.0;
  long  v, vl=0;

  if (velocity > 0) 
  begin
   for(v=0; v<NVOICES; v++)  //find quietest voice
    begin
     if (voice[v].env < l) then
      begin
       l := voice[v].env;
       vl := v;
      end;
    end;

    l := exp(0.05776226505 * ((float)note + Parameter[12] + Parameter[12] - 1.0));
    voice[vl].note := note;                         //fine tuning
    voice[vl].car  := 0.0;
    voice[vl].dcar := fTune * fPitchBend * l; //pitch bend not updated during note as a bit tricky...

    if(l>50.0) l = 50.0; //key tracking
    l := l * (64 + fVelSens * (velocity - 64)); //vel sens
    voice[vl].menv := fDepth[0] * l;
    voice[vl].mlev := fDepth[1] * l;
    voice[vl].fMDec := fMDec;

    voice[vl].dmod := fRatio * voice[vl].dcar; //sine oscillator
    voice[vl].mod0 := 0.0;
    voice[vl].mod1 := (float)sin(voice[vl].dmod);
    voice[vl].dmod := 2.0 * (float)cos(voice[vl].dmod);
                     //scale fVolume with richness
    voice[vl].env  := (1.5 - Parameter[13]) * fVolume * (velocity + 10);
    voice[vl].fCAtt := fCAtt;
    voice[vl].cenv := 0.0;
    voice[vl].fCDec := fCDec;
  end;
  else //note off
  begin
    for(v=0; v<NVOICES; v++) if(voice[v].note==note) //any voices playing that note?
    begin
      if(fSustain==0)
      begin
        voice[v].fCDec = fCRel; //release phase
        voice[v].env  = voice[v].cenv;
        voice[v].fCAtt = 1.0;
        voice[v].mlev = 0.0;
        voice[v].fMDec = fMRel;
      end;
      else voice[v].note = fSustain;
    end;
  end;
end;


VstInt32 mdaDX10::processEvents(VstEvents* ev)
begin
  long npos=0;
  
  for (long i=0; i<ev->numEvents; i++)
  begin
    if((ev->events[i])->type != kVstMidiType) continue;
    VstMidiEvent* event = (VstMidiEvent* )ev->events[i];
    char* midiData = event->midiData;
    
    switch(midiData[0] & 0xf0) //status byte (all channels)
    begin
      case 0x80: //note off
        notes[npos++] = event->deltaFrames; //delta
        notes[npos++] = midiData[1] & 0x7F; //note
        notes[npos++] = 0;                  //vel
        break;

      case 0x90: //note on
        notes[npos++] = event->deltaFrames; //delta
        notes[npos++] = midiData[1] & 0x7F; //note
        notes[npos++] = midiData[2] & 0x7F; //vel
        break;

      case 0xB0: //controller
        switch(midiData[1])
        begin
          case 0x01:  //mod wheel
            fModWheel = 0.00000005 * (float)(midiData[2] * midiData[2]);
            break;
          
          case 0x07:  //fVolume
            fVolume = 0.00000035 * (float)(midiData[2] * midiData[2]);
            break;
         
          case 0x40:  //fSustain
            fSustain = midiData[2] & 0x40;
            if(fSustain==0)
            begin
              notes[npos++] = event->deltaFrames;
              notes[npos++] = fSustain; //end all sustained notes
              notes[npos++] = 0;
            end;
            break;

          default:  //all notes off
            if(midiData[1]>0x7A) 
            begin
              for(long v=0; v<NVOICES; v++) voice[v].fCDec=0.99;
              fSustain = 0;
            end;
            break;
        end;
        break;

      case 0xC0: //program change
        if(midiData[1]<NPROGS) setProgram(midiData[1]);
        break;
      
      case 0xE0: //pitch bend
        fPitchBend = (float)(midiData[1] + 128 * midiData[2] - 8192);
        if(fPitchBend>0.0) fPitchBend = 1.0 + 0.000014951 * fPitchBend; 
                  else fPitchBend = 1.0 + 0.000013318 * fPitchBend; 
        break;
      
      default: break;
    end;

    if(npos>EVENTBUFFER) npos -= 3; //discard events if buffer full!!
    event++;
  end;
  notes[npos] = EVENTS_DONE;
  return 1;
end;
*)
