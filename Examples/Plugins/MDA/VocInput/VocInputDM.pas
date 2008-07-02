unit VocInputDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TVocInputDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
  private
  public
  end;

implementation

{$R *.DFM}

procedure TVocInputDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
(*
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, b;
  float ds=pstep, s=sawbuf, n=noise;
  float l0=lbuf0, l1=lbuf1, l2=lbuf2, l3=lbuf3; 
  float le=lenv, he=henv, et=lfreq*0.1f, lf=lfreq, v=vuv, mn=minp, mx=maxp;
  float rootm=39.863137f;
  long  tr=track;

  --in1;
  --in2;
  --out1;
  --out2;
  while(--sampleFrames >= 0)
  {
    a = *++in1; 
    b = *++in2;

    l0 -= lf * (l1 + a);       //fundamental filter (peaking 2nd-order 100Hz lpf)
    l1 -= lf * (l1 - l0);

    b = l0; if(b<0.0f) b = -b;
    le -= et * (le - b);       //fundamental level

    b = (a + 0.03f) * v;
    if(b<0.0f) b = -b;
    he -= et * (he - b);       //overall level (+ constant so >f0 when quiet)

    l3 += 1.0f;
    if(tr>0)                   //pitch tracking
    {
      if(l1>0.0f && l2<=0.0f)  //found +ve zero crossing
      {
        if(l3>mn && l3<mx)     //...in allowed range
        {
          mn = 0.6f * l3;       //new max pitch to discourage octave jumps!
          l2 = l1 / (l1 - l2);   //fractional period...
          ds = pmult / (l3 - l2); //new period

          if(tr==2)            //quantize pitch
          {
            ds = rootm * (float)(log10(ds) - root);
            ds = (float)pow(1.0594631, floor(ds + 0.5) + rootm * root);   
          }
        }
        l3 = l2;               //restart period measurement
      }
      l2=l1;                   //remember previous sample
    }
  
    b = 0.00001f * (float)((rand() & 32767) - 16384);  //sibilance
    if(le>he) b *= s * n;                    //...or modulated breath noise
    b += s; s += ds; if(s>0.5f) s-=1.0f;     //badly aliased sawtooth!

    *++out1 = a;
    *++out2 = b;
  }
  sawbuf=s;

  if(fabs(he)>1.0e-10) henv = he; else henv=0.0f; //catch denormals
  if(fabs(l1)>1.0e-10) { lbuf0=l0; lbuf1=l1; lenv=le; } else { lbuf0 = lbuf1= lenv = 0.0f; }
  lbuf2=l2, lbuf3=l3;
  if(tr) pstep=ds; 
*)
end;

end.