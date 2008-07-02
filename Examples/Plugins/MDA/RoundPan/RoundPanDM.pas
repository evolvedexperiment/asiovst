unit RoundPanDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TRoundPanDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
  private
  public
  end;

implementation

{$R *.DFM}

procedure TRoundPanDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
(*
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, c, d, x=0.5, y=(float)0.7854;  
  float ph, dph, fourpi=(float)12.566371;
  
  ph = phi;
  dph = dphi;
  
  --in1;  
  --in2;  
  --out1;
  --out2;
  while(--sampleFrames >= 0)
  {
    a = x * (*++in1 + *++in2); //process from here...
        
    c = (float)(a * -sin((x * ph) - y)); // output
    d = (float)(a * sin((x * ph) + y));
        
    ph = ph + dph;

    *++out1 = c;
    *++out2 = d;
  }
  if(ph<0.0) ph = ph + fourpi; else if(ph>fourpi) ph = ph - fourpi;
  phi = ph;
*)
end;

end.

(*

C Source:

mdaRoundPan::mdaRoundPan(audioMasterCallback audioMaster)  : AudioEffectX(audioMaster, 1, 2)  // programs, parameters
{
  //inits here!
  fParam1 = (float)0.5; //pan
  fParam2 = (float)0.8; //auto

  //size = 1500;
  //bufpos = 0;
  //buffer = new float[size];
  //buffer2 = new float[size];

  suspend();    // flush buffer

  //calcs here!
  phi = 0.0;
  dphi = (float)(5.0 / getSampleRate());
}

void mdaRoundPan::setParameter(VstInt32 index, float value)
{
  switch(index)
  {
    case 0: fParam1 = value; phi = (float)(6.2831853 * (fParam1 - 0.5)); break;
    case 1: fParam2 = value; break;
  }
  //calcs here
  if (fParam2>0.55)
  {
    dphi = (float)(20.0 * (fParam2 - 0.55) / getSampleRate());
  }
  else
  {
    if (fParam2<0.45)
    {
      dphi = (float)(-20.0 * (0.45 - fParam2) / getSampleRate());
    }
    else
    {
      dphi = 0.0;
    }
  }
}

mdaRoundPan::~mdaRoundPan()
{
  //if(buffer) delete buffer;
  //if(buffer2) delete buffer2;
}

void mdaRoundPan::suspend()
{
  //memset(buffer, 0, size * sizeof(float));
  //memset(buffer2, 0, size * sizeof(float));
}

void mdaRoundPan::setProgramName(char *name)
{
  strcpy(programName, name);
}

void mdaRoundPan::getProgramName(char *name)
{
  strcpy(name, programName);
}

float mdaRoundPan::getParameter(VstInt32 index)
{
  float v=0;

  switch(index)
  {
    case 0: v = fParam1; break;
    case 1: v = fParam2; break;
  }
  return v;
}

void mdaRoundPan::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)(360.0 * (fParam1 - 0.5)), text); break;
    case 1: long2string((long)(57.296 * dphi * getSampleRate()), text); break;
  }
}

*)
