unit LoudnessDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TLoudnessDataModule = class(TVSTModule)
    procedure LoudnessDataModuleParameterProperties2CustomParameterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
  public
  end;

implementation

{$R *.DFM}

procedure TLoudnessDataModule.LoudnessDataModuleParameterProperties2CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'ON'
  else PreDefined := 'OFF';
end;

end.

(*
float loudness[14][3] = { {402,  0.0025,  0.00},  //-60dB
                          {334,  0.0121,  0.00},
                          {256,  0.0353,  0.00},
                          {192,  0.0900,  0.00},
                          {150,  0.2116,  0.00},
                          {150,  0.5185,  0.00},
                          {  1,  0     ,  0.00},  //0dB
                          {33.7,    5.5,  1.00},
                          {92,      8.7,  0.62},
                          {63.7,   18.4,  0.44},
                          {42.9,   48.2,  0.30},
                          {37.6, 116.2f,  0.18},
                          {22.9, 428.7f,  0.09},  //+60dB
                          {   0,      0,  0.00}  };


mdaLoudnessProgram::mdaLoudnessProgram() ///default program settings
{
  param[0] = 0.7;  //loudness
  param[1] = 0.5;  //output
  param[2] = 0.35f;  //link
  strcpy(name, "Equal Loudness Contours");  //re. Stevens-Davis @ 100dB
}

mdaLoudness::mdaLoudness(audioMasterCallback audioMaster): AudioEffectX(audioMaster, NPROGS, NPARAMS)
{
  programs = new mdaLoudnessProgram[numPrograms];
  setProgram(0);

  suspend();
}


void mdaLoudness::resume() ///update internal parameters...
{
  float f, tmp;
  long  i;

  tmp = param[0] + param[0] - 1;
  igain = 60 * tmp * tmp;
  if(tmp<0) igain *= -1;

  tmp = param[1] + param[1] - 1;
  ogain = 60 * tmp * tmp;
  if (tmp < 0) ogain *= -1;

  f = 0.1f * igain + 6;  //coefficient index + fractional part
  i = (long)f;
  f -= (float)i;

  tmp := loudness[i][0];  A0 := tmp + f * (loudness[i + 1][0] - tmp);
  tmp := loudness[i][1];  A1 := tmp + f * (loudness[i + 1][1] - tmp);
  tmp := loudness[i][2];  A2 := tmp + f * (loudness[i + 1][2] - tmp);

  A0 := 1 - Exp(-6.283153 * A0 / getSampleRate());

  if(igain>0)
  {
    //if(mode==0) suspend();  //don't click when switching mode
    mode=1;
  }
  else
  {
    //if(mode==1) suspend();
    mode=0;
  }

  tmp = ogain;
  if(param[2]>0.5f)  //linked gain
  {
    tmp -= igain;  if(tmp>0) tmp = 0;  //limit max gain
  }
  gain = (float)pow(10, 0.05f * tmp);
}


void mdaLoudness::suspend() ///clear any buffers...
{
  Z0 = Z1 = Z2 = Z3 = 0;
}


mdaLoudness::~mdaLoudness() ///destroy any buffers...
{
  if(programs) delete[] programs;
}

void mdaLoudness::setProgram(VstInt32 program)
{
  int i=0;

  mdaLoudnessProgram *p = &programs[program];
  curProgram = program;
  setProgramName(p->name);
  for(i=0; i<NPARAMS; i++) param[i] = p->param[i];
  resume();
}

void  mdaLoudness::setParameter(VstInt32 index, float value)
{
  programs[curProgram].param[index] = param[index] = value; //bug was here!
  resume();
}

float mdaLoudness::getParameter(VstInt32 index) { return param[index]; }
void  mdaLoudness::setProgramName(char *name) { strcpy(programName, name); }
void  mdaLoudness::getProgramName(char *name) { strcpy(name, programName); }


void mdaLoudness::getParameterDisplay(VstInt32 index, char *text)
{
   char string[16];

  switch(index)
  {
    case  0: sprintf(string, "%.1f", igain); break;
    default: sprintf(string, "%.1f", ogain); break;
  }
  string[8] = 0;
  strcpy(text, (char * )string);
}


void mdaLoudness::process(float **inputs, float **outputs, VstInt32 sampleFrames)
{
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, b, c, d;
  float z0=Z0, z1=Z1, z2=Z2, z3=Z3;
  float a0=A0, a1=A1, a2=A2, g=gain;

  --in1;
  --in2;
  --out1;
  --out2;

  if(mode==0) //cut
  {
    while(--sampleFrames >= 0)
    {
      a = *++in1;
      b = *++in2;
      c = out1[1];
      d = out2[1];

      z0 += a0 * (a - z0 + 0.3f * z1);  a -= z0;
      z1 += a0 * (a - z1);              a -= z1;
                                        a -= z0 * a1;
      z2 += a0 * (b - z2 + 0.3f * z1);  b -= z2;
      z3 += a0 * (b - z3);              b -= z3;
                                        b -= z2 * a1;
      c += a * g;
      d += b * g;
      
      *++out1 = c;
      *++out2 = d;
    }
  }
  else //boost
  {
    while(--sampleFrames >= 0)
    {
      a = *++in1;
      b = *++in2;
      c = out1[1];
      d = out2[1];

      z0 += a0 * (a  - z0);
      z1 += a0 * (z0 - z1);   a += a1 * (z1 - a2 * z0);

      z2 += a0 * (b  - z2);
      z3 += a0 * (z2 - z3);   b += a1 * (z3 - a2 * z2);

      c += a * g;
      d += b * g;

      *++out1 = c;
      *++out2 = d;
    }
  }
  if(fabs(z1)<1.0e-10 || fabs(z1)>100) { Z0 = Z1 = 0; } else { Z0 = z0; Z1 = z1; } //catch denormals
  if(fabs(z3)<1.0e-10 || fabs(z3)>100) { Z2 = Z3 = 0; } else { Z2 = z2; Z3 = z3; }
}


void mdaLoudness::processReplacing(float **inputs, float **outputs, VstInt32 sampleFrames)
{
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, b, c, d;
  float z0=Z0, z1=Z1, z2=Z2, z3=Z3;
  float a0=A0, a1=A1, a2=A2, g=gain;

  --in1;
  --in2;
  --out1;
  --out2;

  if(mode==0) //cut
  {
    while(--sampleFrames >= 0)
    {
      a = *++in1;
      b = *++in2;

      z0 += a0 * (a - z0 + 0.3f * z1);  a -= z0;
      z1 += a0 * (a - z1);              a -= z1;
                                        a -= z0 * a1;
      z2 += a0 * (b - z2 + 0.3f * z1);  b -= z2;
      z3 += a0 * (b - z3);              b -= z3;
                                        b -= z2 * a1;
      c = a * g;
      d = b * g;
      
      *++out1 = c;
      *++out2 = d;
    }
  }
  else //boost
  {
    while(--sampleFrames >= 0)
    {
      a = *++in1;
      b = *++in2;

      z0 += a0 * (a  - z0);
      z1 += a0 * (z0 - z1);   a += a1 * (z1 - a2 * z0);

      z2 += a0 * (b  - z2);
      z3 += a0 * (z2 - z3);   b += a1 * (z3 - a2 * z2);

      c = g * a;
      d = g * b;

      *++out1 = c;
      *++out2 = d;
    }
  }
  if(fabs(z1)<1.0e-10 || fabs(z1)>100) { Z0 = Z1 = 0; } else { Z0 = z0; Z1 = z1; } //catch denormals
  if(fabs(z3)<1.0e-10 || fabs(z3)>100) { Z2 = Z3 = 0; } else { Z2 = z2; Z3 = z3; }
}
*)