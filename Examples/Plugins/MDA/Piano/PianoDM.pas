unit PianoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TPianoDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  Math, PianoData;

procedure TPianoDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
 Parameter[ 0] := 0.5;  // Decay
 Parameter[ 1] := 0.5;  // Release
 Parameter[ 2] := 0.5;  // Hardness

 Parameter[ 3] := 0.5;  // Vel>Hard
 Parameter[ 4] := 1.0;  // Muffle
 Parameter[ 5] := 0.5;  // Vel>Muff

 Parameter[ 6] := 0.33; // Vel Curve
 Parameter[ 7] := 0.50; // Stereo
 Parameter[ 8] := 0.33; // Max Poly

 Parameter[ 9] := 0.5;  // Tune
 Parameter[10] := 0.0;  // Random
 Parameter[11] := 0.5;  // Stretch
*)
end;

procedure TPianoDataModule.VSTModuleResume(Sender: TObject);
begin
(*
  Fs = getSampleRate();
  iFs = 1.0 / Fs;
  if(Fs > 64000.0) cmax = 0xFF; else cmax = 0x7F;
  memset(comb, 0, sizeof(float) * 256);

  DECLARE_VST_DEPRECATED (wantEvents) ();
*)
end;

end.

(*
mdaPiano::mdaPiano(audioMasterCallback audioMaster) : AudioEffectX(audioMaster, NPROGS, NPARAMS)
begin
  Fs = 44100.0;  iFs = 1.0/Fs;  cmax = 0x7F;  //just in case...
  
  programs = new mdaPianoProgram[NPROGS];
  if(programs)
  begin
    //fill patches...
    long i=0;
    fillpatch(i++, "mda Piano",        0.500, 0.500, 0.500, 0.5, 0.803, 0.251, 0.376, 0.500, 0.330, 0.500, 0.246, 0.500);
    fillpatch(i++, "Plain Piano",      0.500, 0.500, 0.500, 0.5, 0.751, 0.000, 0.452, 0.000, 0.000, 0.500, 0.000, 0.500);
    fillpatch(i++, "Compressed Piano", 0.902, 0.399, 0.623, 0.5, 1.000, 0.331, 0.299, 0.499, 0.330, 0.500, 0.000, 0.500);
    fillpatch(i++, "Dance Piano",      0.399, 0.251, 1.000, 0.5, 0.672, 0.124, 0.127, 0.249, 0.330, 0.500, 0.283, 0.667);
    fillpatch(i++, "Concert Piano",    0.648, 0.500, 0.500, 0.5, 0.298, 0.602, 0.550, 0.850, 0.356, 0.500, 0.339, 0.660);
    fillpatch(i++, "Dark Piano",       0.500, 0.602, 0.000, 0.5, 0.304, 0.200, 0.336, 0.651, 0.330, 0.500, 0.317, 0.500);
    fillpatch(i++, "School Piano",     0.450, 0.598, 0.626, 0.5, 0.603, 0.500, 0.174, 0.331, 0.330, 0.500, 0.421, 0.801);
    fillpatch(i++, "Broken Piano",     0.050, 0.957, 0.500, 0.5, 0.299, 1.000, 0.000, 0.500, 0.330, 0.450, 0.718, 0.000);
   
    setProgram(0);
  end;

  if(audioMaster)
  begin
    setNumInputs(0);        
    setNumOutputs(NOUTS);
    canProcessReplacing();
    isSynth();
    setUniqueID('MDAp');  ///
  end;

  waves = pianoData;


  //Waveform data and keymapping is hard-wired in *this* version
  kgrp[ 0].root = 36;  kgrp[ 0].high = 37;  kgrp[ 0].pos = 0;       kgrp[ 0].end = 36275;   kgrp[ 0].loop = 14774;
  kgrp[ 1].root = 40;  kgrp[ 1].high = 41;  kgrp[ 1].pos = 36278;   kgrp[ 1].end = 83135;   kgrp[ 1].loop = 16268;
  kgrp[ 2].root = 43;  kgrp[ 2].high = 45;  kgrp[ 2].pos = 83137;   kgrp[ 2].end = 146756;  kgrp[ 2].loop = 33541;
  kgrp[ 3].root = 48;  kgrp[ 3].high = 49;  kgrp[ 3].pos = 146758;  kgrp[ 3].end = 204997;  kgrp[ 3].loop = 21156;
  kgrp[ 4].root = 52;  kgrp[ 4].high = 53;  kgrp[ 4].pos = 204999;  kgrp[ 4].end = 244908;  kgrp[ 4].loop = 17191;
  kgrp[ 5].root = 55;  kgrp[ 5].high = 57;  kgrp[ 5].pos = 244910;  kgrp[ 5].end = 290978;  kgrp[ 5].loop = 23286;
  kgrp[ 6].root = 60;  kgrp[ 6].high = 61;  kgrp[ 6].pos = 290980;  kgrp[ 6].end = 342948;  kgrp[ 6].loop = 18002;
  kgrp[ 7].root = 64;  kgrp[ 7].high = 65;  kgrp[ 7].pos = 342950;  kgrp[ 7].end = 391750;  kgrp[ 7].loop = 19746;
  kgrp[ 8].root = 67;  kgrp[ 8].high = 69;  kgrp[ 8].pos = 391752;  kgrp[ 8].end = 436915;  kgrp[ 8].loop = 22253;
  kgrp[ 9].root = 72;  kgrp[ 9].high = 73;  kgrp[ 9].pos = 436917;  kgrp[ 9].end = 468807;  kgrp[ 9].loop = 8852;
  kgrp[10].root = 76;  kgrp[10].high = 77;  kgrp[10].pos = 468809;  kgrp[10].end = 492772;  kgrp[10].loop = 9693;
  kgrp[11].root = 79;  kgrp[11].high = 81;  kgrp[11].pos = 492774;  kgrp[11].end = 532293;  kgrp[11].loop = 10596;
  kgrp[12].root = 84;  kgrp[12].high = 85;  kgrp[12].pos = 532295;  kgrp[12].end = 560192;  kgrp[12].loop = 6011;
  kgrp[13].root = 88;  kgrp[13].high = 89;  kgrp[13].pos = 560194;  kgrp[13].end = 574121;  kgrp[13].loop = 3414;
  kgrp[14].root = 93;  kgrp[14].high = 999; kgrp[14].pos = 574123;  kgrp[14].end = 586343;  kgrp[14].loop = 2399;
 
  //initialise...
  for(long v=0; v<NVOICES; v++) 
  begin
    voice[v].env = 0.0;
    voice[v].dec = 0.99; //all notes off
  end;
  notes[0] = EVENTS_DONE;
  volume = 0.2;
  muff = 160.0;
  cpos = sustain = activevoices = 0;
  comb = new float[256];

  guiUpdate = 0;

  update();
  suspend();
end;


void mdaPiano::update()  //parameter change
begin
  size = (long)(12.0 * Parameter[2] - 6.0);
  sizevel = 0.12 * Parameter[3];
  muffvel = Parameter[5] * Parameter[5] * 5.0;
  
  velsens = 1.0 + Parameter[6] + Parameter[6];
  if(Parameter[6] < 0.25) velsens -= 0.75 - 3.0 * Parameter[6];
  
  fine = Parameter[9] - 0.5;
  random = 0.077 * Parameter[10] * Parameter[10];
  stretch = 0.000434 * (Parameter[11] - 0.5);
  
  cdep = Parameter[7] * Parameter[7];
  trim = 1.50 - 0.79 * cdep;
  width = 0.04 * Parameter[7];  if(width > 0.03) width = 0.03;
  
  poly = 8 + (long)(24.9 * Parameter[8]);
end;

mdaPiano::~mdaPiano ()  //destroy any buffers...
begin
  if(programs) delete [] programs;
  if(comb) delete[] comb;
end;


void mdaPiano::setProgram(VstInt32 program)
begin
  long i;

  mdaPianoProgram *p = &programs[program];
  curProgram = program;
  for(i=0; i<NPARAMS; i++) Parameter[i] = p->Parameter[i];
  update();
end;


void mdaPiano::setParameter(VstInt32 index, float value)
begin
  mdaPianoProgram *p = &programs[curProgram];
  Parameter[index] = p->Parameter[index] = value;
  update();

//  if(editor) editor->postUpdate(); //For GUI

  guiUpdate = index + 0x100 + (guiUpdate & 0xFFFF00);
end;


void mdaPiano::fillpatch(long p, char *name, float p0, float p1, float p2, float p3, float p4,
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


float mdaPiano::getParameter(VstInt32 index)     begin return Parameter[index]; end;
void  mdaPiano::setProgramName(char *name)   begin strcpy(programs[curProgram].name, name); end;
void  mdaPiano::getProgramName(char *name)   begin strcpy(name, programs[curProgram].name); end;
void  mdaPiano::setBlockSize(VstInt32 blockSize) begin  AudioEffectX::setBlockSize(blockSize); end;


bool mdaPiano::getOutputProperties(VstInt32 index, VstPinProperties* properties)
begin
  if(index<NOUTS)
  begin
    if(index) sprintf(properties->label, "Piano R");
         else sprintf(properties->label, "Piano L");
    properties->flags = kVstPinIsActive;
    if(index<2) properties->flags |= kVstPinIsStereo; //make channel 1+2 stereo
    return true;
  end;
  return false;
end;


bool mdaPiano::getProgramNameIndexed(VstInt32 category, VstInt32 index, char* text)
begin
  if(index<NPROGS)
  begin
    strcpy(text, programs[index].name);
    return true;
  end;
  return false;
end;


bool mdaPiano::copyProgram(VstInt32 destination)
begin
  if(destination<NPROGS)
  begin
    programs[destination] = programs[curProgram];
    return true;
  end;
  return false;
end;


VstInt32 mdaPiano::canDo(char* text)
begin
  if(strcmp(text, "receiveVstEvents") == 0) return 1;
  if(strcmp(text, "receiveVstMidiEvent") == 0) return 1;
  return -1;
end;


void mdaPiano::getParameterDisplay(VstInt32 index, char *text)
begin
  char string[16];

  switch(index)
  begin
    case  4: sprintf(string, "%.0f", 100.0 - 100.0 * Parameter[index]); break;
    case  7: sprintf(string, "%.0f", 200.0 * Parameter[index]); break;
    case  8: sprintf(string, "%ld", poly); break;
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



void mdaPiano::process(float **inputs, float **outputs, VstInt32 sampleFrames)
begin
  float* out0 = outputs[0];
  float* out1 = outputs[1];
  long event=0, frame=0, frames, v;
  float x, l, r;
  long i;

  while(frame<sampleFrames)
  begin
    frames = notes[event++];
    if(frames>sampleFrames) frames = sampleFrames;
    frames -= frame;
    frame += frames;

    while(--frames>=0)
    begin
      VOICE *V = voice;
      l = r = 0.0;

      for(v=0; v<activevoices; v++)
      begin
        V->frac += V->delta;  //integer-based linear interpolation
        V->pos += V->frac >> 16;
        V->frac &= 0xFFFF;
        if(V->pos > V->end) V->pos -= V->loop;
        i = waves[V->pos];
        i = (i << 7) + (V->frac >> 9) * (waves[V->pos + 1] - i) + 0x40400000;
        x = V->env * (*(float * )&i - 3.0);  //fast int->float
        
        V->env = V->env * V->dec;  //envelope
        V->f0 += V->ff * (x + V->f1 - V->f0);  //muffle filter
        V->f1 = x;

        l += V->outl * V->f0;
        r += V->outr * V->f0;
 
        V++;
      end;
      comb[cpos] = l + r;
      ++cpos &= cmax;
      x = cdep * comb[cpos];  //stereo simulator

      *out0++ += l + x;
      *out1++ += r - x;
    end;

    if(frame<sampleFrames)
    begin
      long note = notes[event++];
      long vel  = notes[event++];
      noteOn(note, vel);
    end;
  end;
  for(v=0; v<activevoices; v++) if(voice[v].env < SILENCE) voice[v] = voice[--activevoices];
  notes[0] = EVENTS_DONE;  //mark events buffer as done
end;


void mdaPiano::processReplacing(float **inputs, float **outputs, VstInt32 sampleFrames)
begin
  float* out0 = outputs[0];
  float* out1 = outputs[1];
  long event=0, frame=0, frames, v;
  float x, l, r;
  long i;

  while(frame<sampleFrames)
  begin
    frames = notes[event++];
    if(frames>sampleFrames) frames = sampleFrames;
    frames -= frame;
    frame += frames;

    while(--frames>=0)
    begin
      VOICE *V = voice;
      l = r = 0.0;

      for(v=0; v<activevoices; v++)
      begin
        V->frac += V->delta;  //integer-based linear interpolation
        V->pos += V->frac >> 16;
        V->frac &= 0xFFFF;
        if(V->pos > V->end) V->pos -= V->loop;
        //i = (i << 7) + (V->frac >> 9) * (waves[V->pos + 1] - i) + 0x40400000;   //not working on intel mac !?!
  i = waves[V->pos] + ((V->frac * (waves[V->pos + 1] - waves[V->pos])) >> 16);
  x = V->env * (float)i / 32768.0;
        //x = V->env * (*(float * )&i - 3.0);  //fast int->float
        
        V->env = V->env * V->dec;  //envelope
        V->f0 += V->ff * (x + V->f1 - V->f0);  //muffle filter
        V->f1 = x;

        l += V->outl * V->f0;
        r += V->outr * V->f0;
 
 if(!(l > -2.0) || !(l < 2.0))
 begin
   printf("what is this shit?   %ld,  %f,  %f\n", i, x, V->f0);
   l = 0.0;
 end;  
if(!(r > -2.0) || !(r < 2.0))
 begin
   r = 0.0;
 end;  
   
        V++;
      end;
      comb[cpos] = l + r;
      ++cpos &= cmax;
      x = cdep * comb[cpos];  //stereo simulator

      *out0++ = l + x;
      *out1++ = r - x;
    end;

    if(frame<sampleFrames)
    begin
      long note = notes[event++];
      long vel  = notes[event++];
      noteOn(note, vel);
    end;
  end;
  for(v=0; v<activevoices; v++) if(voice[v].env < SILENCE) voice[v] = voice[--activevoices];
  notes[0] = EVENTS_DONE;  //mark events buffer as done
end;


void mdaPiano::noteOn(long note, long velocity)
begin
  float l=99.0;
  long  v, vl=0, k, s;
  
  if(velocity>0) 
  begin
    if(activevoices < poly) //add a note
    begin
      vl = activevoices;
      activevoices++;
    end;
    else //steal a note
    begin
      for(v=0; v<poly; v++)  //find quietest voice
      begin
        if(voice[v].env < l) begin l = voice[v].env;  vl = v; end;
      end;
    end;

    k = (note - 60) * (note - 60);
    l = fine + random * ((float)(k % 13) - 6.5);  //random & fine tune
    if(note > 60) l += stretch * (float)k; //stretch

    s = size;
    if(velocity > 40) s += (long)(sizevel * (float)(velocity - 40));  

    k = 0;
    while(note > (kgrp[k].high + s)) k++;  //find keygroup
    
    l += (float)(note - kgrp[k].root); //pitch
    l = 22050.0 * iFs * (float)exp(0.05776226505 * l);
    voice[vl].delta = (long)(65536.0 * l);
    voice[vl].frac = 0;
    voice[vl].pos = kgrp[k].pos;
    voice[vl].end = kgrp[k].end;
    voice[vl].loop = kgrp[k].loop;

    voice[vl].env = (0.5 + velsens) * (float)pow(0.0078 * velocity, velsens); //velocity
    
    l = 50.0 + Parameter[4] * Parameter[4] * muff + muffvel * (float)(velocity - 64); //muffle
    if(l < (55.0 + 0.25 * (float)note)) l = 55.0 + 0.25 * (float)note;
    if(l > 210.0) l = 210.0;
    voice[vl].ff = l * l * iFs;
    voice[vl].f0 = voice[vl].f1 = 0.0;

    voice[vl].note = note; //note->pan
    if(note <  12) note = 12;
    if(note > 108) note = 108;
    l = volume * trim;
    voice[vl].outr = l + l * width * (float)(note - 60);
    voice[vl].outl = l + l - voice[vl].outr;

    if(note < 44) note = 44; //limit max decay length
    l = 2.0 * Parameter[0];
    if(l < 1.0) l += 0.25 - 0.5 * Parameter[0];
    voice[vl].dec = (float)exp(-iFs * exp(-0.6 + 0.033 * (double)note - l));
  end;
  else //note off
  begin
    for(v=0; v<NVOICES; v++) if(voice[v].note==note) //any voices playing that note?
    begin
      if(sustain==0)
      begin
        if(note < 94 || note == SUSTAIN) //no release on highest notes
          voice[v].dec = (float)exp(-iFs * exp(2.0 + 0.017 * (double)note - 2.0 * Parameter[1]));
      end;
      else voice[v].note = SUSTAIN;
    end;
  end;
end;


VstInt32 mdaPiano::processEvents(VstEvents* ev)
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
          case 0x43:  //soft pedal
            muff = 0.01 * (float)((127 - midiData[2]) * (127 - midiData[2]));  
            break;
          
          case 0x07:  //volume
            volume = 0.00002 * (float)(midiData[2] * midiData[2]);
            break;
         
          case 0x40:  //sustain pedal
          case 0x42:  //sustenuto pedal
            sustain = midiData[2] & 0x40;
            if(sustain==0)
            begin
              notes[npos++] = event->deltaFrames;
              notes[npos++] = SUSTAIN; //end all sustained notes
              notes[npos++] = 0;
            end;
            break;

          default:  //all notes off
            if(midiData[1]>0x7A) 
            begin  
              for(long v=0; v<NVOICES; v++) voice[v].dec=0.99;
              sustain = 0;
              muff = 160.0;
            end;
            break;
        end;
        break;

      case 0xC0: //program change
        if(midiData[1]<NPROGS) setProgram(midiData[1]);
        break;
      
      default: break;
    end;

    if(npos>EVENTBUFFER) npos -= 3; //discard events if buffer full!!
    event++; //?
  end;
  notes[npos] = EVENTS_DONE;
  return 1;
end;
*)
