unit JX10;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TJX10DataModule = class(TVSTModule)
  private
  public
  end;

implementation

{$R *.DFM}

end.

(*
mdaJX10Program::mdaJX10Program()
{
  Parameter[0]  = 0.00; //OSC Mix
  Parameter[1]  = 0.25; //OSC Tune
  Parameter[2]  = 0.50; //OSC Fine

  Parameter[3]  = 0.00; //OSC Mode
  Parameter[4]  = 0.35; //OSC Rate
  Parameter[5]  = 0.50; //OSC Bend

  Parameter[6]  = 1.00; //VCF Freq
  Parameter[7]  = 0.15; //VCF Reso
  Parameter[8]  = 0.75; //VCF <Env

  Parameter[9]  = 0.00; //VCF <LFO
  Parameter[10] = 0.50; //VCF <Vel
  Parameter[11] = 0.00; //VCF Att

  Parameter[12] = 0.30; //VCF Dec
  Parameter[13] = 0.00; //VCF Sus
  Parameter[14] = 0.25; //VCF Rel

  Parameter[15] = 0.00; //ENV Att
  Parameter[16] = 0.50; //ENV Dec
  Parameter[17] = 1.00; //ENV Sus
  
  Parameter[18] = 0.30; //ENV Rel
  Parameter[19] = 0.81; //LFO Rate
  Parameter[20] = 0.50; //Vibrato
  
  Parameter[21] = 0.00; //Noise   - not present in original patches
  Parameter[22] = 0.50; //Octave
  Parameter[23] = 0.50; //Tuning
  strcpy (name, "Empty Patch");  
}


mdaJX10::mdaJX10(audioMasterCallback audioMaster) : AudioEffectX(audioMaster, NPROGS, NPARAMS)
{
  long i=0;
  Fs = 44100.0;

  programs = new mdaJX10Program[NPROGS];
  if(programs)
  {
    fillpatch(i++, "5th Sweep Pad", 1.0, 0.37, 0.25, 0.3, 0.32, 0.5, 0.9, 0.6, 0.12, 0.0, 0.5, 0.9, 0.89, 0.9, 0.73, 0.0, 0.5, 1.0, 0.71, 0.81, 0.65, 0.0, 0.5, 0.5);
    fillpatch(i++, "Echo Pad [SA]", 0.88, 0.51, 0.5, 0.0, 0.49, 0.5, 0.46, 0.76, 0.69, 0.1, 0.69, 1.0, 0.86, 0.76, 0.57, 0.3, 0.8, 0.68, 0.66, 0.79, 0.13, 0.25, 0.45, 0.5);
    fillpatch(i++, "Space Chimes [SA]", 0.88, 0.51, 0.5, 0.16, 0.49, 0.5, 0.49, 0.82, 0.66, 0.08, 0.89, 0.85, 0.69, 0.76, 0.47, 0.12, 0.22, 0.55, 0.66, 0.89, 0.34, 0.0, 1.0, 0.5);
    fillpatch(i++, "Solid Backing", 1.0, 0.26, 0.14, 0.0, 0.35, 0.5, 0.3, 0.25, 0.7, 0.0, 0.63, 0.0, 0.35, 0.0, 0.25, 0.0, 0.5, 1.0, 0.3, 0.81, 0.5, 0.5, 0.5, 0.5);
    fillpatch(i++, "Velocity Backing [SA]", 0.41, 0.5, 0.79, 0.0, 0.08, 0.32, 0.49, 0.01, 0.34, 0.0, 0.93, 0.61, 0.87, 1.0, 0.93, 0.11, 0.48, 0.98, 0.32, 0.81, 0.5, 0.0, 0.5, 0.5);
    fillpatch(i++, "Rubber Backing [ZF]", 0.29, 0.76, 0.26, 0.0, 0.18, 0.76, 0.35, 0.15, 0.77, 0.14, 0.54, 0.0, 0.42, 0.13, 0.21, 0.0, 0.56, 0.0, 0.32, 0.2, 0.58, 0.22, 0.53, 0.5);
    fillpatch(i++, "808 State Lead", 1.0, 0.65, 0.24, 0.4, 0.34, 0.85, 0.65, 0.63, 0.75, 0.16, 0.5, 0.0, 0.3, 0.0, 0.25, 0.17, 0.5, 1.0, 0.03, 0.81, 0.5, 0.0, 0.68, 0.5);
    fillpatch(i++, "Mono Glide", 0.0, 0.25, 0.5, 1.0, 0.46, 0.5, 0.51, 0.0, 0.5, 0.0, 0.0, 0.0, 0.3, 0.0, 0.25, 0.37, 0.5, 1.0, 0.38, 0.81, 0.62, 0.0, 0.5, 0.5);
    fillpatch(i++, "Detuned Techno Lead", 0.84, 0.51, 0.15, 0.45, 0.41, 0.42, 0.54, 0.01, 0.58, 0.21, 0.67, 0.0, 0.09, 1.0, 0.25, 0.2, 0.85, 1.0, 0.3, 0.83, 0.09, 0.4, 0.49, 0.5);
    fillpatch(i++, "Hard Lead [SA]", 0.71, 0.75, 0.53, 0.18, 0.24, 1.0, 0.56, 0.52, 0.69, 0.19, 0.7, 1.0, 0.14, 0.65, 0.95, 0.07, 0.91, 1.0, 0.15, 0.84, 0.33, 0.0, 0.49, 0.5);
    fillpatch(i++, "Bubble", 0.0, 0.25, 0.43, 0.0, 0.71, 0.48, 0.23, 0.77, 0.8, 0.32, 0.63, 0.4, 0.18, 0.66, 0.14, 0.0, 0.38, 0.65, 0.16, 0.48, 0.5, 0.0, 0.67, 0.5);
    fillpatch(i++, "Monosynth", 0.62, 0.26, 0.51, 0.79, 0.35, 0.54, 0.64, 0.39, 0.51, 0.65, 0.0, 0.07, 0.52, 0.24, 0.84, 0.13, 0.3, 0.76, 0.21, 0.58, 0.3, 0.0, 0.36, 0.5);
    fillpatch(i++, "Moogcury Lite", 0.81, 1.0, 0.21, 0.78, 0.15, 0.35, 0.39, 0.17, 0.69, 0.4, 0.62, 0.0, 0.47, 0.19, 0.37, 0.0, 0.5, 0.2, 0.33, 0.38, 0.53, 0.0, 0.12, 0.5);
    fillpatch(i++, "Gangsta Whine", 0.0, 0.51, 0.52, 0.96, 0.44, 0.5, 0.41, 0.46, 0.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.25, 0.15, 0.5, 1.0, 0.32, 0.81, 0.49, 0.0, 0.83, 0.5);
    fillpatch(i++, "Higher Synth [ZF]", 0.48, 0.51, 0.22, 0.0, 0.0, 0.5, 0.5, 0.47, 0.73, 0.3, 0.8, 0.0, 0.1, 0.0, 0.07, 0.0, 0.42, 0.0, 0.22, 0.21, 0.59, 0.16, 0.98, 0.5);
    fillpatch(i++, "303 Saw Bass", 0.0, 0.51, 0.5, 0.83, 0.49, 0.5, 0.55, 0.75, 0.69, 0.35, 0.5, 0.0, 0.56, 0.0, 0.56, 0.0, 0.8, 1.0, 0.24, 0.26, 0.49, 0.0, 0.07, 0.5);
    fillpatch(i++, "303 Square Bass", 0.75, 0.51, 0.5, 0.83, 0.49, 0.5, 0.55, 0.75, 0.69, 0.35, 0.5, 0.14, 0.49, 0.0, 0.39, 0.0, 0.8, 1.0, 0.24, 0.26, 0.49, 0.0, 0.07, 0.5);
    fillpatch(i++, "Analog Bass", 1.0, 0.25, 0.2, 0.81, 0.19, 0.5, 0.3, 0.51, 0.85, 0.09, 0.0, 0.0, 0.88, 0.0, 0.21, 0.0, 0.5, 1.0, 0.46, 0.81, 0.5, 0.0, 0.27, 0.5);
    fillpatch(i++, "Analog Bass 2", 1.0, 0.25, 0.2, 0.72, 0.19, 0.86, 0.48, 0.43, 0.94, 0.0, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.61, 1.0, 0.32, 0.81, 0.5, 0.0, 0.27, 0.5);
    fillpatch(i++, "Low Pulses", 0.97, 0.26, 0.3, 0.0, 0.35, 0.5, 0.8, 0.4, 0.52, 0.0, 0.5, 0.0, 0.77, 0.0, 0.25, 0.0, 0.5, 1.0, 0.3, 0.81, 0.16, 0.0, 0.0, 0.5);
    fillpatch(i++, "Sine Infra-Bass", 0.0, 0.25, 0.5, 0.65, 0.35, 0.5, 0.33, 0.76, 0.53, 0.0, 0.5, 0.0, 0.3, 0.0, 0.25, 0.0, 0.55, 0.25, 0.3, 0.81, 0.52, 0.0, 0.14, 0.5);
    fillpatch(i++, "Wobble Bass [SA]", 1.0, 0.26, 0.22, 0.64, 0.82, 0.59, 0.72, 0.47, 0.34, 0.34, 0.82, 0.2, 0.69, 1.0, 0.15, 0.09, 0.5, 1.0, 0.07, 0.81, 0.46, 0.0, 0.24, 0.5);
    fillpatch(i++, "Squelch Bass", 1.0, 0.26, 0.22, 0.71, 0.35, 0.5, 0.67, 0.7, 0.26, 0.0, 0.5, 0.48, 0.69, 1.0, 0.15, 0.0, 0.5, 1.0, 0.07, 0.81, 0.46, 0.0, 0.24, 0.5);
    fillpatch(i++, "Rubber Bass [ZF]", 0.49, 0.25, 0.66, 0.81, 0.35, 0.5, 0.36, 0.15, 0.75, 0.2, 0.5, 0.0, 0.38, 0.0, 0.25, 0.0, 0.6, 1.0, 0.22, 0.19, 0.5, 0.0, 0.17, 0.5);
    fillpatch(i++, "Soft Pick Bass", 0.37, 0.51, 0.77, 0.71, 0.22, 0.5, 0.33, 0.47, 0.71, 0.16, 0.59, 0.0, 0.0, 0.0, 0.25, 0.04, 0.58, 0.0, 0.22, 0.15, 0.44, 0.33, 0.15, 0.5);
    fillpatch(i++, "Fretless Bass", 0.5, 0.51, 0.17, 0.8, 0.34, 0.5, 0.51, 0.0, 0.58, 0.0, 0.67, 0.0, 0.09, 0.0, 0.25, 0.2, 0.85, 0.0, 0.3, 0.81, 0.7, 0.0, 0.0, 0.5);
    fillpatch(i++, "Whistler", 0.23, 0.51, 0.38, 0.0, 0.35, 0.5, 0.33, 1.0, 0.5, 0.0, 0.5, 0.0, 0.29, 0.0, 0.25, 0.68, 0.39, 0.58, 0.36, 0.81, 0.64, 0.38, 0.92, 0.5);
    fillpatch(i++, "Very Soft Pad", 0.39, 0.51, 0.27, 0.38, 0.12, 0.5, 0.35, 0.78, 0.5, 0.0, 0.5, 0.0, 0.3, 0.0, 0.25, 0.35, 0.5, 0.8, 0.7, 0.81, 0.5, 0.0, 0.5, 0.5);
    fillpatch(i++, "Pizzicato", 0.0, 0.25, 0.5, 0.0, 0.35, 0.5, 0.23, 0.2, 0.75, 0.0, 0.5, 0.0, 0.22, 0.0, 0.25, 0.0, 0.47, 0.0, 0.3, 0.81, 0.5, 0.8, 0.5, 0.5);
    fillpatch(i++, "Synth Strings", 1.0, 0.51, 0.24, 0.0, 0.0, 0.35, 0.42, 0.26, 0.75, 0.14, 0.69, 0.0, 0.67, 0.55, 0.97, 0.82, 0.7, 1.0, 0.42, 0.84, 0.67, 0.3, 0.47, 0.5);
    fillpatch(i++, "Synth Strings 2", 0.75, 0.51, 0.29, 0.0, 0.49, 0.5, 0.55, 0.16, 0.69, 0.08, 0.2, 0.76, 0.29, 0.76, 1.0, 0.46, 0.8, 1.0, 0.39, 0.79, 0.27, 0.0, 0.68, 0.5);
    fillpatch(i++, "Leslie Organ", 0.0, 0.5, 0.53, 0.0, 0.13, 0.39, 0.38, 0.74, 0.54, 0.2, 0.0, 0.0, 0.55, 0.52, 0.31, 0.0, 0.17, 0.73, 0.28, 0.87, 0.24, 0.0, 0.29, 0.5);
    fillpatch(i++, "Click Organ", 0.5, 0.77, 0.52, 0.0, 0.35, 0.5, 0.44, 0.5, 0.65, 0.16, 0.0, 0.0, 0.0, 0.18, 0.0, 0.0, 0.75, 0.8, 0.0, 0.81, 0.49, 0.0, 0.44, 0.5);
    fillpatch(i++, "Hard Organ", 0.89, 0.91, 0.37, 0.0, 0.35, 0.5, 0.51, 0.62, 0.54, 0.0, 0.0, 0.0, 0.37, 0.0, 1.0, 0.04, 0.08, 0.72, 0.04, 0.77, 0.49, 0.0, 0.58, 0.5);
    fillpatch(i++, "Bass Clarinet", 1.0, 0.51, 0.51, 0.37, 0.0, 0.5, 0.51, 0.1, 0.5, 0.11, 0.5, 0.0, 0.0, 0.0, 0.25, 0.35, 0.65, 0.65, 0.32, 0.79, 0.49, 0.2, 0.35, 0.5);
    fillpatch(i++, "Trumpet", 0.0, 0.51, 0.51, 0.82, 0.06, 0.5, 0.57, 0.0, 0.32, 0.15, 0.5, 0.21, 0.15, 0.0, 0.25, 0.24, 0.6, 0.8, 0.1, 0.75, 0.55, 0.25, 0.69, 0.5);
    fillpatch(i++, "Soft Horn", 0.12, 0.9, 0.67, 0.0, 0.35, 0.5, 0.5, 0.21, 0.29, 0.12, 0.6, 0.0, 0.35, 0.36, 0.25, 0.08, 0.5, 1.0, 0.27, 0.83, 0.51, 0.1, 0.25, 0.5);
    fillpatch(i++, "Brass Section", 0.43, 0.76, 0.23, 0.0, 0.28, 0.36, 0.5, 0.0, 0.59, 0.0, 0.5, 0.24, 0.16, 0.91, 0.08, 0.17, 0.5, 0.8, 0.45, 0.81, 0.5, 0.0, 0.58, 0.5);
    fillpatch(i++, "Synth Brass", 0.4, 0.51, 0.25, 0.0, 0.3, 0.28, 0.39, 0.15, 0.75, 0.0, 0.5, 0.39, 0.3, 0.82, 0.25, 0.33, 0.74, 0.76, 0.41, 0.81, 0.47, 0.23, 0.5, 0.5);
    fillpatch(i++, "Detuned Syn Brass [ZF]", 0.68, 0.5, 0.93, 0.0, 0.31, 0.62, 0.26, 0.07, 0.85, 0.0, 0.66, 0.0, 0.83, 0.0, 0.05, 0.0, 0.75, 0.54, 0.32, 0.76, 0.37, 0.29, 0.56, 0.5);
    fillpatch(i++, "Power PWM", 1.0, 0.27, 0.22, 0.0, 0.35, 0.5, 0.82, 0.13, 0.75, 0.0, 0.0, 0.24, 0.3, 0.88, 0.34, 0.0, 0.5, 1.0, 0.48, 0.71, 0.37, 0.0, 0.35, 0.5);
    fillpatch(i++, "Water Velocity [SA]", 0.76, 0.51, 0.35, 0.0, 0.49, 0.5, 0.87, 0.67, 1.0, 0.32, 0.09, 0.95, 0.56, 0.72, 1.0, 0.04, 0.76, 0.11, 0.46, 0.88, 0.72, 0.0, 0.38, 0.5);
    fillpatch(i++, "Ghost [SA]", 0.75, 0.51, 0.24, 0.45, 0.16, 0.48, 0.38, 0.58, 0.75, 0.16, 0.81, 0.0, 0.3, 0.4, 0.31, 0.37, 0.5, 1.0, 0.54, 0.85, 0.83, 0.43, 0.46, 0.5);
    fillpatch(i++, "Soft E.Piano", 0.31, 0.51, 0.43, 0.0, 0.35, 0.5, 0.34, 0.26, 0.53, 0.0, 0.63, 0.0, 0.22, 0.0, 0.39, 0.0, 0.8, 0.0, 0.44, 0.81, 0.51, 0.0, 0.5, 0.5);
    fillpatch(i++, "Thumb Piano", 0.72, 0.82, 1.0, 0.0, 0.35, 0.5, 0.37, 0.47, 0.54, 0.0, 0.5, 0.0, 0.45, 0.0, 0.39, 0.0, 0.39, 0.0, 0.48, 0.81, 0.6, 0.0, 0.71, 0.5);
    fillpatch(i++, "Steel Drums [ZF]", 0.81, 0.76, 0.19, 0.0, 0.18, 0.7, 0.4, 0.3, 0.54, 0.17, 0.4, 0.0, 0.42, 0.23, 0.47, 0.12, 0.48, 0.0, 0.49, 0.53, 0.36, 0.34, 0.56, 0.5);       
    
    fillpatch(58,  "Car Horn", 0.57, 0.49, 0.31, 0.0, 0.35, 0.5, 0.46, 0.0, 0.68, 0.0, 0.5, 0.46, 0.3, 1.0, 0.23, 0.3, 0.5, 1.0, 0.31, 1.0, 0.38, 0.0, 0.5, 0.5);
    fillpatch(59,  "Helicopter", 0.0, 0.25, 0.5, 0.0, 0.35, 0.5, 0.08, 0.36, 0.69, 1.0, 0.5, 1.0, 1.0, 0.0, 1.0, 0.96, 0.5, 1.0, 0.92, 0.97, 0.5, 1.0, 0.0, 0.5);
    fillpatch(60,  "Arctic Wind", 0.0, 0.25, 0.5, 0.0, 0.35, 0.5, 0.16, 0.85, 0.5, 0.28, 0.5, 0.37, 0.3, 0.0, 0.25, 0.89, 0.5, 1.0, 0.89, 0.24, 0.5, 1.0, 1.0, 0.5);
    fillpatch(61,  "Thip", 1.0, 0.37, 0.51, 0.0, 0.35, 0.5, 0.0, 1.0, 0.97, 0.0, 0.5, 0.02, 0.2, 0.0, 0.2, 0.0, 0.46, 0.0, 0.3, 0.81, 0.5, 0.78, 0.48, 0.5);
    fillpatch(62,  "Synth Tom", 0.0, 0.25, 0.5, 0.0, 0.76, 0.94, 0.3, 0.33, 0.76, 0.0, 0.68, 0.0, 0.59, 0.0, 0.59, 0.1, 0.5, 0.0, 0.5, 0.81, 0.5, 0.7, 0.0, 0.5);
    fillpatch(63,  "Squelchy Frog", 0.5, 0.41, 0.23, 0.45, 0.77, 0.0, 0.4, 0.65, 0.95, 0.0, 0.5, 0.33, 0.5, 0.0, 0.25, 0.0, 0.7, 0.65, 0.18, 0.32, 1.0, 0.0, 0.06, 0.5);
    
    //for testing...
    //fillpatch(0, "Monosynth", 0.62, 0.26, 0.51, 0.79, 0.35, 0.54, 0.64, 0.39, 0.51, 0.65, 0.0, 0.07, 0.52, 0.24, 0.84, 0.13, 0.3, 0.76, 0.21, 0.58, 0.3, 0.0, 0.36, 0.5);

    setProgram(0);
  }

  if(audioMaster)
  {
    setNumInputs(0);        
    setNumOutputs(NOUTS);
    canProcessReplacing();
    isSynth();
    setUniqueID('MDAj');  ///
  }

  //initialise...
  for(long v=0; v<NVOICES; v++) 
  {
    voice[v].dp   = voice[v].dp2   = 1.0;
    voice[v].saw  = voice[v].p     = voice[v].p2    = 0.0;
    voice[v].env  = voice[v].envd  = voice[v].envl  = 0.0;
    voice[v].fenv = voice[v].fenvd = voice[v].fenvl = 0.0;
    voice[v].f0   = voice[v].f1    = voice[v].f2    = 0.0;
    voice[v].note = 0;
  }
  notes[0] = EVENTS_DONE;
  lfo = modwhl = filtwhl = press = fzip = 0.0; 
  rezwhl = pbend = ipbend = 1.0;
  volume = 0.0005;
  K = mode = lastnote = sustain = activevoices = 0;
  noise = 22222;

  update();
  suspend();
}


void mdaJX10::update()  //parameter change
{
  double ifs = 1.0 / Fs;

  mode = (long)(7.9 * Parameter[3]);
  noisemix = Parameter[21] * Parameter[21];
  voltrim = (3.2 - Parameter[0] - 1.5 * noisemix) * (1.5 - 0.5 * Parameter[7]);
  noisemix *= 0.06;
  oscmix = Parameter[0];

  semi = (float)floor(48.0 * Parameter[1]) - 24.0;
  cent = 15.876 * Parameter[2] - 7.938;
  cent = 0.1 * (float)floor(cent * cent * cent);
  detune = (float)pow(1.059463094359, - semi - 0.01 * cent);
  tune = -23.376 - 2.0 * Parameter[23] - 12.0 * (float)floor(Parameter[22] * 4.9);
  tune = Fs * (float)pow(1.059463094359, tune);

  vibrato = pwmdep = 0.2 * (Parameter[20] - 0.5) * (Parameter[20] - 0.5);
  if(Parameter[20]<0.5) vibrato = 0.0;

  lfoHz = (float)exp(7.0 * Parameter[19] - 4.0);
  dlfo = lfoHz * (float)(ifs * TWOPI * KMAX); 

  filtf = 8.0 * Parameter[6] - 1.5;
  filtq  = (1.0 - Parameter[7]) * (1.0 - Parameter[7]); ////// + 0.02;
  filtlfo = 2.5 * Parameter[9] * Parameter[9];
  filtenv = 12.0 * Parameter[8] - 6.0;
  filtvel = 0.1 * Parameter[10] - 0.05;
  if(Parameter[10]<0.05) { veloff = 1; filtvel = 0; } else veloff = 0;

  att = 1.0 - (float)exp(-ifs * exp(5.5 - 7.5 * Parameter[15]));
  dec = 1.0 - (float)exp(-ifs * exp(5.5 - 7.5 * Parameter[16]));
  sus = Parameter[17];
  rel = 1.0 - (float)exp(-ifs * exp(5.5 - 7.5 * Parameter[18]));
  if(Parameter[18]<0.01) rel = 0.1; //extra fast release

  ifs *= KMAX; //lower update rate...

  fatt = 1.0 - (float)exp(-ifs * exp(5.5 - 7.5 * Parameter[11]));
  fdec = 1.0 - (float)exp(-ifs * exp(5.5 - 7.5 * Parameter[12]));
  fsus = Parameter[13] * Parameter[13];
  frel = 1.0 - (float)exp(-ifs * exp(5.5 - 7.5 * Parameter[14]));

  if(Parameter[4]<0.02) glide = 1.0; else
  glide = 1.0 - (float)exp(-ifs * exp(6.0 - 7.0 * Parameter[4]));
  glidedisp = (6.604 * Parameter[5] - 3.302);
  glidedisp *= glidedisp * glidedisp;
}


void mdaJX10::setSampleRate(float sampleRate)
{
  AudioEffectX::setSampleRate(sampleRate);
  Fs = sampleRate;
 
  dlfo = lfoHz * (float)(TWOPI * KMAX) / Fs; 
}


void mdaJX10::resume()
{  
  DECLARE_VST_DEPRECATED (wantEvents) ();
}


void mdaJX10::suspend() //Used by Logic (have note off code in 3 places now...)
{
  for(long v=0; v<NVOICES; v++)
  {
    voice[v].envl = voice[v].env = 0.0; 
    voice[v].envd = 0.99;
    voice[v].note = 0;
    voice[v].f0 = voice[v].f1 = voice[v].f2 = 0.0;
  }
}


mdaJX10::~mdaJX10()  //destroy any buffers...
{
  if(programs) delete[] programs;
}


void mdaJX10::setProgram(VstInt32 program)
{
  long i;

  mdaJX10Program *p = &programs[program];
  curProgram = program;
  for(i=0; i<NPARAMS; i++) Parameter[i] = p->Parameter[i];
  update();
} //may want all notes off here - but this stops use of patches as snapshots!


void mdaJX10::setParameter(VstInt32 index, float value)
{
  mdaJX10Program *p = &programs[curProgram];
  Parameter[index] = p->Parameter[index] = value;
  update();

  ///if(editor) editor->postUpdate();
}


void mdaJX10::fillpatch(long p, char *name,
                      float p0,  float p1,  float p2,  float p3,  float p4,  float p5, 
                      float p6,  float p7,  float p8,  float p9,  float p10, float p11,
                      float p12, float p13, float p14, float p15, float p16, float p17, 
                      float p18, float p19, float p20, float p21, float p22, float p23)
{
  strcpy(programs[p].name, name);
  programs[p].Parameter[0]  = p0;   programs[p].Parameter[1]  = p1;
  programs[p].Parameter[2]  = p2;   programs[p].Parameter[3]  = p3;
  programs[p].Parameter[4]  = p4;   programs[p].Parameter[5]  = p5;
  programs[p].Parameter[6]  = p6;   programs[p].Parameter[7]  = p7;
  programs[p].Parameter[8]  = p8;   programs[p].Parameter[9]  = p9;
  programs[p].Parameter[10] = p10;  programs[p].Parameter[11] = p11;
  programs[p].Parameter[12] = p12;  programs[p].Parameter[13] = p13;
  programs[p].Parameter[14] = p14;  programs[p].Parameter[15] = p15;
  programs[p].Parameter[16] = p16;  programs[p].Parameter[17] = p17;
  programs[p].Parameter[18] = p18;  programs[p].Parameter[19] = p19;
  programs[p].Parameter[20] = p20;  programs[p].Parameter[21] = p21;
  programs[p].Parameter[22] = p22;  programs[p].Parameter[23] = p23;  
}


float mdaJX10::getParameter(VstInt32 index)     { return Parameter[index]; }
void  mdaJX10::setProgramName(char *name)   { strcpy(programs[curProgram].name, name); }
void  mdaJX10::getProgramName(char *name)   { strcpy(name, programs[curProgram].name); }
void  mdaJX10::setBlockSize(VstInt32 blockSize) {  AudioEffectX::setBlockSize(blockSize); }
bool  mdaJX10::getEffectName(char* name)    { strcpy(name, "mda JX10 Synth"); return true; }
bool  mdaJX10::getVendorString(char* text)  {  strcpy(text, "maxim digital audio"); return true; }
bool  mdaJX10::getProductString(char* text) { strcpy(text, "mda JX10 Synth"); return true; }


bool mdaJX10::getOutputProperties(VstInt32 index, VstPinProperties* properties)
{
  if(index<NOUTS)
  {
    sprintf(properties->label, "JX10", index + 1);
    properties->flags = kVstPinIsActive;
    if(index<2) properties->flags |= kVstPinIsStereo; //make channel 1+2 stereo
    return true;
  }
  return false;
}


bool mdaJX10::getProgramNameIndexed(VstInt32 category, VstInt32 index, char* text)
{
  if(index<NPROGS)
  {
    strcpy(text, programs[index].name);
    return true;
  }
  return false;
}


bool mdaJX10::copyProgram(VstInt32 destination)
{
  if(destination<NPROGS)
  {
    programs[destination] = programs[curProgram];
    return true;
  }
  return false;
}


VstInt32 mdaJX10::canDo(char* text)
{
  if(!strcmp (text, "receiveVstEvents")) return 1;
  if(!strcmp (text, "receiveVstMidiEvent"))  return 1;
  return -1;
}


void mdaJX10::getParameterName(VstInt32 index, char *label)
{
  switch (index)
  {
    case  0: strcpy(label, "OSC Mix "); break;
    case  1: strcpy(label, "OSC Tune"); break;
    case  2: strcpy(label, "OSC Fine"); break;
    
    case  3: strcpy(label, "Glide   "); break;
    case  4: strcpy(label, "Gld Rate"); break;
    case  5: strcpy(label, "Gld Bend"); break;
    
    case  6: strcpy(label, "VCF Freq"); break;
    case  7: strcpy(label, "VCF Reso"); break;
    case  8: strcpy(label, "VCF Env "); break;
    
    case  9: strcpy(label, "VCF LFO "); break;
     case 10: strcpy(label, "VCF Vel "); break;
     case 11: strcpy(label, "VCF Att "); break;

    case 12: strcpy(label, "VCF Dec "); break;
    case 13: strcpy(label, "VCF Sus "); break;
     case 14: strcpy(label, "VCF Rel "); break;
     
    case 15: strcpy(label, "ENV Att "); break;
    case 16: strcpy(label, "ENV Dec "); break;
    case 17: strcpy(label, "ENV Sus "); break;

    case 18: strcpy(label, "ENV Rel "); break;
    case 19: strcpy(label, "LFO Rate"); break;
    case 20: strcpy(label, "Vibrato "); break;

    case 21: strcpy(label, "Noise   "); break;
    case 22: strcpy(label, "Octave  "); break;
    default: strcpy(label, "Tuning  ");
  }
}


void mdaJX10::getParameterDisplay(VstInt32 index, char *text)
{
  char string[16];
  
  switch(index)
  {
    case  0: sprintf(string, "%4.0f:%2.0f", 100.0-50.0*Parameter[index], 50.0*Parameter[index]); break;
    case  1: sprintf(string, "%.0f", semi); break;
    case  2: sprintf(string, "%.1f", cent); break; 
    case  3: switch(mode)
             { case  0:
               case  1: strcpy(string, "POLY    "); break;
               case  2: strcpy(string, "P-LEGATO"); break;
               case  3: strcpy(string, "P-GLIDE "); break;
               case  4:
               case  5: strcpy(string, "MONO    "); break;
               case  6: strcpy(string, "M-LEGATO"); break;
               default: strcpy(string, "M-GLIDE "); break; } break;
    case  5: sprintf(string, "%.2f", glidedisp); break;
    case  6: sprintf(string, "%.1f", 100.0 * Parameter[index]); break;
    case  8:
    case 23: sprintf(string, "%.1f", 200.0 * Parameter[index] - 100.0); break;
    case 10: if(Parameter[index]<0.05) strcpy(string, "   OFF  ");
               else sprintf(string, "%.0f", 200.0 * Parameter[index] - 100.0); break;
    case 19: sprintf(string, "%.3f", lfoHz); break;
    case 20: if(Parameter[index]<0.5) sprintf(string, "PWM %3.0f", 100.0 - 200.0 * Parameter[index]);
               else sprintf(string, "%7.0f", 200.0 * Parameter[index] - 100.0); break;
    case 22: sprintf(string, "%d", (long)(Parameter[index] * 4.9) - 2); break;
    default: sprintf(string, "%.0f", 100.0 * Parameter[index]);
  }
  string[8] = 0;
  strcpy(text, (char * )string);
}


void mdaJX10::getParameterLabel(VstInt32 index, char *label)
{
  switch(index)
  {
    case  1: 
    case  5: strcpy(label, "   semi "); break;
    case  2:
    case 23: strcpy(label, "   cent "); break;
    case  3: 
    case 22: strcpy(label, "        "); break;
    case 19: strcpy(label, "     Hz "); break;
    default: strcpy(label, "      % ");
  }
}


void mdaJX10::process(float **inputs, float **outputs, VstInt32 sampleFrames)
{
  float* out1 = outputs[0];
  float* out2 = outputs[1];
  long event=0, frame=0, frames, v;
  float o, e, vib, pwm, pb=pbend, ipb=ipbend, gl=glide;
  float x, y, hpf=0.997, min=1.0, w=0.0, ww=noisemix;
  float ff, fe=filtenv, fq=filtq * rezwhl, fx=1.97-0.85*fq, fz=fzip;
  long k=K;
  unsigned long r;

  vib = (float)sin(lfo);
  ff = filtf + filtwhl + (filtlfo + press) * vib; //have to do again here as way that
  pwm = 1.0 + vib * (modwhl + pwmdep);           //below triggers on k was too cheap!
  vib = 1.0 + vib * (modwhl + vibrato);

  if(activevoices>0 || notes[event]<sampleFrames)
  {    
    while(frame<sampleFrames)
    {
      frames = notes[event++];
      if(frames>sampleFrames) frames = sampleFrames;
      frames -= frame;
      frame += frames;

      while(--frames>=0)
      {
        VOICE *V = voice;
        o = 0.0;
        
        noise = (noise * 196314165) + 907633515;
        r = (noise & 0x7FFFFF) + 0x40000000; //generate noise + fast convert to float
        w = *(float * )&r;
        w = ww * (w - 3.0);

        if(--k<0)
        {
          lfo += dlfo;
          if(lfo>PI) lfo -= TWOPI;
          vib = (float)sin(lfo);
          ff = filtf + filtwhl + (filtlfo + press) * vib;
          pwm = 1.0 + vib * (modwhl + pwmdep);
          vib = 1.0 + vib * (modwhl + vibrato);
          k = KMAX;
        }

        for(v=0; v<NVOICES; v++)  //for each voice
        { 
          e = V->env;
          if(e > SILENCE)
          { //Sinc-Loop Oscillator
            x = V->p + V->dp;
            if(x > min) 
            {
              if(x > V->pmax) 
              { 
                x = V->pmax + V->pmax - x;  
                V->dp = -V->dp; 
              }
              V->p = x;
              x = V->sin0 * V->sinx - V->sin1; //sine osc
              V->sin1 = V->sin0;
              V->sin0 = x;
              x = x / V->p;
            }
            else
            { 
              V->p = x = - x;  
              V->dp = V->period * vib * pb; //set period for next cycle
              V->pmax = (float)floor(0.5 + V->dp) - 0.5;
              V->dc = -0.5 * V->lev / V->pmax;
              V->pmax *= PI;
              V->dp = V->pmax / V->dp;
              V->sin0 = V->lev * (float)sin(x);
              V->sin1 = V->lev * (float)sin(x - V->dp);
              V->sinx = 2.0 * (float)cos(V->dp);
              if(x*x > .1) x = V->sin0 / x; else x = V->lev; //was 0.01;
            }
            
            y = V->p2 + V->dp2; //osc2
            if(y > min) 
            { 
              if(y > V->pmax2) 
              { 
                y = V->pmax2 + V->pmax2 - y;  
                V->dp2 = -V->dp2; 
              }
              V->p2 = y;
              y = V->sin02 * V->sinx2 - V->sin12;
              V->sin12 = V->sin02;
              V->sin02 = y;
              y = y / V->p2;
            }
            else
            {
              V->p2 = y = - y;  
              V->dp2 = V->period * V->detune * pwm * pb;
              V->pmax2 = (float)floor(0.5 + V->dp2) - 0.5;
              V->dc2 = -0.5 * V->lev2 / V->pmax2;
              V->pmax2 *= PI;
              V->dp2 = V->pmax2 / V->dp2;
              V->sin02 = V->lev2 * (float)sin(y);
              V->sin12 = V->lev2 * (float)sin(y - V->dp2);
              V->sinx2 = 2.0 * (float)cos(V->dp2);
              if(y*y > .1) y = V->sin02 / y; else y = V->lev2;
            }
            V->saw = V->saw * hpf + V->dc + x - V->dc2 - y;  //integrated sinc = saw
            x = V->saw + w;
            V->env += V->envd * (V->envl - V->env);

            if(k==KMAX) //filter freq update at LFO rate
            {
              if((V->env+V->envl)>3.0) { V->envd=dec; V->envl=sus; } //envelopes
              V->fenv += V->fenvd * (V->fenvl - V->fenv);
              if((V->fenv+V->fenvl)>3.0) { V->fenvd=fdec; V->fenvl=fsus; }

              fz += 0.005 * (ff - fz); //filter zipper noise filter
              y = V->fc * (float)exp(fz + fe * V->fenv) * ipb; //filter cutoff
              if(y<0.005) y=0.005;
              V->ff = y;
 
              V->period += gl * (V->target - V->period); //glide
              if(V->target < V->period) V->period += gl * (V->target - V->period);
            }

            if(V->ff > fx) V->ff = fx; //stability limit

            V->f0 += V->ff * V->f1; //state-variable filter
            V->f1 -= V->ff * (V->f0 + fq * V->f1 - x - V->f2);
            V->f1 -= 0.2 * V->f1 * V->f1 * V->f1; //soft limit  //was 0.08
            V->f2 = x;
            
            o += V->env * V->f0;
          }
          V++;
        }

        *out1++ += o;
        *out2++ += o;
      }

      if(frame<sampleFrames)
      {
        long note = notes[event++];
        long vel  = notes[event++];
        noteOn(note, vel);
      }
    }
  
    activevoices = NVOICES;
    for(v=0; v<NVOICES; v++)
    {
      if(voice[v].env<SILENCE)  //choke voices
      {
        voice[v].env = voice[v].envl = 0.0;
        voice[v].f0 = voice[v].f1 = voice[v].f2 = 0.0;
        activevoices--;
      }
    }
  }
  notes[0] = EVENTS_DONE;  //mark events buffer as done
  fzip = fz;
  K = k;
}


void mdaJX10::processReplacing(float **inputs, float **outputs, VstInt32 sampleFrames)
{
  float* out1 = outputs[0];
  float* out2 = outputs[1];
  long event=0, frame=0, frames, v;
  float o, e, vib, pwm, pb=pbend, ipb=ipbend, gl=glide;
  float x, y, hpf=0.997, min=1.0, w=0.0, ww=noisemix;
  float ff, fe=filtenv, fq=filtq * rezwhl, fx=1.97-0.85*fq, fz=fzip;
  long k=K;
  unsigned long r;

  vib = (float)sin(lfo);
  ff = filtf + filtwhl + (filtlfo + press) * vib; //have to do again here as way that
  pwm = 1.0 + vib * (modwhl + pwmdep);           //below triggers on k was too cheap!
  vib = 1.0 + vib * (modwhl + vibrato);

  if(activevoices>0 || notes[event]<sampleFrames)
  {    
    while(frame<sampleFrames)
    {
      frames = notes[event++];
      if(frames>sampleFrames) frames = sampleFrames;
      frames -= frame;
      frame += frames;

      while(--frames>=0)
      {
        VOICE *V = voice;
        o = 0.0;
        
        noise = (noise * 196314165) + 907633515;
        r = (noise & 0x7FFFFF) + 0x40000000; //generate noise + fast convert to float
        w = *(float * )&r;
        w = ww * (w - 3.0);

        if(--k<0)
        {
          lfo += dlfo;
          if(lfo>PI) lfo -= TWOPI;
          vib = (float)sin(lfo);
          ff = filtf + filtwhl + (filtlfo + press) * vib;
          pwm = 1.0 + vib * (modwhl + pwmdep);
          vib = 1.0 + vib * (modwhl + vibrato);
          k = KMAX;
        }

        for(v=0; v<NVOICES; v++)  //for each voice
        { 
          e = V->env;
          if(e > SILENCE)
          { //Sinc-Loop Oscillator
            x = V->p + V->dp;
            if(x > min) 
            {
              if(x > V->pmax) 
              { 
                x = V->pmax + V->pmax - x;  
                V->dp = -V->dp; 
              }
              V->p = x;
              x = V->sin0 * V->sinx - V->sin1; //sine osc
              V->sin1 = V->sin0;
              V->sin0 = x;
              x = x / V->p;
            }
            else
            { 
              V->p = x = - x;  
              V->dp = V->period * vib * pb; //set period for next cycle
              V->pmax = (float)floor(0.5 + V->dp) - 0.5;
              V->dc = -0.5 * V->lev / V->pmax;
              V->pmax *= PI;
              V->dp = V->pmax / V->dp;
              V->sin0 = V->lev * (float)sin(x);
              V->sin1 = V->lev * (float)sin(x - V->dp);
              V->sinx = 2.0 * (float)cos(V->dp);
              if(x*x > .1) x = V->sin0 / x; else x = V->lev; //was 0.01;
            }
            
            y = V->p2 + V->dp2; //osc2
            if(y > min) 
            { 
              if(y > V->pmax2) 
              { 
                y = V->pmax2 + V->pmax2 - y;  
                V->dp2 = -V->dp2; 
              }
              V->p2 = y;
              y = V->sin02 * V->sinx2 - V->sin12;
              V->sin12 = V->sin02;
              V->sin02 = y;
              y = y / V->p2;
            }
            else
            {
              V->p2 = y = - y;  
              V->dp2 = V->period * V->detune * pwm * pb;
              V->pmax2 = (float)floor(0.5 + V->dp2) - 0.5;
              V->dc2 = -0.5 * V->lev2 / V->pmax2;
              V->pmax2 *= PI;
              V->dp2 = V->pmax2 / V->dp2;
              V->sin02 = V->lev2 * (float)sin(y);
              V->sin12 = V->lev2 * (float)sin(y - V->dp2);
              V->sinx2 = 2.0 * (float)cos(V->dp2);
              if(y*y > .1) y = V->sin02 / y; else y = V->lev2;
            }
            V->saw = V->saw * hpf + V->dc + x - V->dc2 - y;  //integrated sinc = saw
            x = V->saw + w;
            V->env += V->envd * (V->envl - V->env);

            if(k==KMAX) //filter freq update at LFO rate
            {
              if((V->env+V->envl)>3.0) { V->envd=dec; V->envl=sus; } //envelopes
              V->fenv += V->fenvd * (V->fenvl - V->fenv);
              if((V->fenv+V->fenvl)>3.0) { V->fenvd=fdec; V->fenvl=fsus; }

              fz += 0.005 * (ff - fz); //filter zipper noise filter
              y = V->fc * (float)exp(fz + fe * V->fenv) * ipb; //filter cutoff
              if(y<0.005) y=0.005;
              V->ff = y;
 
              V->period += gl * (V->target - V->period); //glide
              if(V->target < V->period) V->period += gl * (V->target - V->period);
            }

            if(V->ff > fx) V->ff = fx; //stability limit
            
            V->f0 += V->ff * V->f1; //state-variable filter
            V->f1 -= V->ff * (V->f0 + fq * V->f1 - x - V->f2);
            V->f1 -= 0.2 * V->f1 * V->f1 * V->f1; //soft limit

            V->f2 = x;
            
            o += V->env * V->f0;
          }
          V++;
        }

        *out1++ = o;
        *out2++ = o;
      }

      if(frame<sampleFrames)
      {
        long note = notes[event++];
        long vel  = notes[event++];
        noteOn(note, vel);
      }
    }
  
    activevoices = NVOICES;
    for(v=0; v<NVOICES; v++)
    {
      if(voice[v].env<SILENCE)  //choke voices
      {
        voice[v].env = voice[v].envl = 0.0;
        voice[v].f0 = voice[v].f1 = voice[v].f2 = 0.0;
        activevoices--;
      }
    }
  }
  else //empty block
  {
    while(--sampleFrames >= 0)
    {
      *out1++ = 0.0;
      *out2++ = 0.0;
    }
  }
  notes[0] = EVENTS_DONE;  //mark events buffer as done
  fzip = fz;
  K = k;
}


void mdaJX10::noteOn(long note, long velocity)
{
  float p, l=100.0; //louder than any envelope!
  long  v=0, tmp, held=0;
  
  if(velocity>0) //note on
  {
    if(veloff) velocity = 80;
    
    if(mode & 4) //monophonic
    {
      if(voice[0].note > 0) //legato pitch change
      {
        for(tmp=(NVOICES-1); tmp>0; tmp--)  //queue any held notes
        {
          voice[tmp].note = voice[tmp - 1].note;
        }
        p = tune * (float)exp(-0.05776226505 * ((double)note + ANALOG * (double)v));
        while(p<3.0 || (p * detune)<3.0) p += p;
        voice[v].target = p;
        if((mode & 2)==0) voice[v].period = p;
        voice[v].fc = (float)exp(filtvel * (float)(velocity - 64)) / p;
        voice[v].env += SILENCE + SILENCE; ///was missed out below if returned?
        voice[v].note = note;
        return;
      }
    }
    else //polyphonic 
    {
      for(tmp=0; tmp<NVOICES; tmp++)  //replace quietest voice not in attack
      {
        if(voice[tmp].note > 0) held++;
        if(voice[tmp].env<l && voice[tmp].envl<2.0) { l=voice[tmp].env;  v=tmp; }
      }
    }  
    p = tune * (float)exp(-0.05776226505 * ((double)note + ANALOG * (double)v));
    while(p<3.0 || (p * detune)<3.0) p += p;
    voice[v].target = p;
    voice[v].detune = detune;
  
    tmp = 0;
    if(mode & 2)
    {
      if((mode & 1) || held) tmp = note - lastnote; //glide
    }
    voice[v].period = p * (float)pow(1.059463094359, (double)tmp - glidedisp);
    if(voice[v].period<3.0) voice[v].period = 3.0; //limit min period

    voice[v].note = lastnote = note;

    voice[v].fc = (float)exp(filtvel * (float)(velocity - 64)) / p; //filter tracking

    voice[v].lev = voltrim * volume * (0.004 * (float)((velocity + 64) * (velocity + 64)) - 8.0);
    voice[v].lev2 = voice[v].lev * oscmix;

    if(Parameter[20]<0.5) //force 180 deg phase difference for PWM
    {
      if(voice[v].dp>0.0)
      {
        p = voice[v].pmax + voice[v].pmax - voice[v].p;
        voice[v].dp2 = -voice[v].dp;
      }
      else
      {
        p = voice[v].p;
        voice[v].dp2 = voice[v].dp;
      }
      voice[v].p2 = voice[v].pmax2 = p + PI * voice[v].period;

      voice[v].dc2 = 0.0;
      voice[v].sin02 = voice[v].sin12 = voice[v].sinx2 = 0.0;
    }

    if(mode & 4) //monophonic retriggering
    {
      voice[v].env += SILENCE + SILENCE;
    }
    else
    {
      //if(Parameter[15] < 0.28) 
      //{
      //  voice[v].f0 = voice[v].f1 = voice[v].f2 = 0.0; //reset filter
      //  voice[v].env = SILENCE + SILENCE;
      //  voice[v].fenv = 0.0;
      //}
      //else 
        voice[v].env += SILENCE + SILENCE; //anti-glitching trick
    }
    voice[v].envl  = 2.0;
    voice[v].envd  = att;
    voice[v].fenvl = 2.0;
    voice[v].fenvd = fatt;
  }
  else //note off
  {
    if((mode & 4) && (voice[0].note==note)) //monophonic (and current note)
    {
      for(v=(NVOICES-1); v>0; v--)
      {
        if(voice[v].note>0) held = v; //any other notes queued?
      }
      if(held>0)
      {
        voice[v].note = voice[held].note;
        voice[held].note = 0;
        
        p = tune * (float)exp(-0.05776226505 * ((double)voice[v].note + ANALOG * (double)v));
        while(p<3.0 || (p * detune)<3.0) p += p;
        voice[v].target = p;
        if((mode & 2)==0) voice[v].period = p;
        voice[v].fc = 1.0 / p;
      }
      else
      {
        voice[v].envl  = 0.0;
        voice[v].envd  = rel;
        voice[v].fenvl = 0.0;
        voice[v].fenvd = frel;
        voice[v].note  = 0;
      }
    }
    else //polyphonic
    {
      for(v=0; v<NVOICES; v++) if(voice[v].note==note) //any voices playing that note?
      {
        if(sustain==0)
        {
          voice[v].envl  = 0.0;
          voice[v].envd  = rel;
          voice[v].fenvl = 0.0;
          voice[v].fenvd = frel;
          voice[v].note  = 0;
        }
        else voice[v].note = SUSTAIN;
      }
    }
  }
}


VstInt32 mdaJX10::processEvents(VstEvents* ev)
{
  long npos=0;

  for (long i=0; i<ev->numEvents; i++)
  {
    if((ev->events[i])->type != kVstMidiType) continue;
    VstMidiEvent* event = (VstMidiEvent* )ev->events[i];
    char* midiData = event->midiData;
    
    switch(midiData[0] & 0xf0) //status byte (all channels)
    {
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
        {
          case 0x01:  //mod wheel
            modwhl = 0.000005 * (float)(midiData[2] * midiData[2]);
            break;
          case 0x02:  //filter +
          case 0x4A:
            filtwhl = 0.02 * (float)(midiData[2]);
            break;
          case 0x03:  //filter -
            filtwhl = -0.03 * (float)(midiData[2]);
            break;

          case 0x07:  //volume
            volume = 0.00000005 * (float)(midiData[2] * midiData[2]);
            break;

          case 0x10:  //resonance
          case 0x47:
            rezwhl = 0.0065 * (float)(154 - midiData[2]);
            break;

          case 0x40:  //sustain
            sustain = midiData[2] & 0x40;
            if(sustain==0)
            {
              notes[npos++] = event->deltaFrames;
              notes[npos++] = SUSTAIN; //end all sustained notes
              notes[npos++] = 0;
            }
            break;

          default:  //all notes off
            if(midiData[1]>0x7A) 
            {  
              for(long v=0; v<NVOICES; v++) 
              { 
                voice[v].envl = voice[v].env = 0.0; 
                voice[v].envd = 0.99;
                voice[v].note = 0;
                //could probably reset some more stuff here for safety!
              }
              sustain = 0;
            }
            break;
        }
        break;

      case 0xC0: //program change
        if(midiData[1]<NPROGS) setProgram(midiData[1]);
        break;

      case 0xD0: //channel aftertouch
        press = 0.00001 * (float)(midiData[1] * midiData[1]);
        break;
      
      case 0xE0: //pitch bend
        ipbend = (float)exp(0.000014102 * (double)(midiData[1] + 128 * midiData[2] - 8192));
        pbend = 1.0 / ipbend;
        break;
      
      default: break;
    }

    if(npos>EVENTBUFFER) npos -= 3; //discard events if buffer full!!
    event++;
  }
  notes[npos] = EVENTS_DONE;
  return 1;
}
*)
