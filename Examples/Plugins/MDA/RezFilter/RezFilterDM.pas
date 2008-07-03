unit RezFilterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TRezFilterDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
  private
    fBuffer : Array [0..2] of Single;
  public
  end;

implementation

{$R *.DFM}

procedure TRezFilterDataModule.VSTModuleCreate(Sender: TObject);
begin
{
  //inits here!
  Parameter[0] = 0.33f; //f
  Parameter[1] = 0.70f; //q
  Parameter[2] = 0.50f; //a
  Parameter[3] = 0.85f; //fenv
  Parameter[4] = 0.00f; //att
  Parameter[5] = 0.50f; //rel
  Parameter[6] = 0.70f; //lfo
  Parameter[7] = 0.40f; //rate
  Parameter[8] = 0.00f; //trigger
  Parameter[9] = 0.75f; //max freq

  suspend();    // flush buffer
  setParameter(2, 0.5f); //go and set initial values!
}
end;

procedure TRezFilterDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
(*
  //calcs here
  fff := 1.5 * sqr(Parameter[0]) - 0.15;
  fq  := 0.99 * Power(Parameter[1], 0.3); // was 0.99 *
  fg  := 0.5 * Power(10, 2 * Parameter[2] - 1);

  fmax = 0.99 + 0.3 * Parameter[1];
  if (fmax > (1.3 * Parameter[9]))
   then fmax := 1.3 * Parameter[9];
  //fmax = 1;
  //fq *= 1 + 0.2 * Parameter[9];

  fenv := 2 * sqr(0.5 - Parameter[3]);
  fenv := (Parameter[3] > 0.5)? fenv : -fenv;
  att  := Power(10.0, -0.01 - 4.0 * Parameter[4]);
  rel  := 1 - Power(10.0, -2.00 - 4.0 * Parameter[5]);

  lfomode := 0;
  flfo := 2 * sqr(Parameter[6] - 0.5f);
  dphi := (6.2832f * Power(10, 3 * Parameter[7] - 1.5) / SampleRate);
  if (Parameter[6] < 0.5) then
   begin
    lfomode := 1;
    dphi    := 0.15915 * dphi;
    flfo    := flfo * 0.001;
   end; //S&H

  if (Parameter[8] < 0.1)
   then tthr := 0;
   else tthr := 3 * sqr(Parameter[8]);
*)
end;

procedure TRezFilterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
(*
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a;
  float f, i, ff=fff, fe=fenv, q=fq, g=fg, e=env, tmp;
  float b0=buf0, b1=buf1, b2=buf2, at=att, re=rel, fm=fmax;
  float fl=flfo, dph=dphi, ph=phi, bl=bufl, th=tthr, e2=env2;
  int lm=lfomode, ta=tatt, tt=ttrig;

  --in1;
  --in2;
  --out1;
  --out2;

  if(th==0.f)
  {
    while(--sampleFrames >= 0)
    {
      a = *++in1 + *++in2;
    
      i = (a>0)? a : -a; //envelope
      e = (i>e)?  e + at * (i - e) : e * re;
    
      if(lm==0) bl = fl * (float)sin(ph); //lfo
      else if(ph>1.f) { bl = fl*(rand() % 2000 - 1000); ph=0.f; }
      ph += dph;

      f = ff + fe * e + bl; //freq
      if(f<0.f) i=0.f; else i=(f>fm)? fm : f;
 //     o = 1.f - i; 

 //     tmp = g*a + q*(1.f + (1.f/o)) * (b0-b1);
 //     b0 = o * (b0 - tmp) + tmp; 
 //     b1 = o * (b1 - b0) + b0;
      
      tmp = q + q * (1.0f + i * (1.0f + 1.1f * i));
      //tmp = q + q/(1.0008 - i);
      b0 += i * (g * a - b0 + tmp * (b0 - b1));
      b1 += i * (b0 - b1);

      
  //    b2 = o * (b2 - b1) + b1; 

      *++out1 = b1;
      *++out2 = b1;
    }
  }
  else
  {
    while(--sampleFrames >= 0)
    {
      a = *++in1 + *++in2;

      i := abs(a);  //envelope

      if i > e
       then e := i
       else e := e * re;

      if (e > th) then
       begin
        if (tt==0) then
         begin
          ta :=1;
          if (lm==1)
           then ph=2.f;
           else tt=1;
         end
        else tt=0;
       end;
      if (ta==1) then
       begin
        e2 += at*(1.f-e2);
        if (e2 > 0.999)
         then ta := 0;
       end
      else e2 := e2 * re;

      if(lm==0) bl = fl * (float)sin(ph); //lfo
      else if(ph>1.f) { bl = fl*(rand() % 2000 - 1000); ph=0.f; }
      ph += dph;

      f = ff + fe * e + bl; //freq
      if(f<0.f) i=0.f; else i=(f>fm)? fm : f;
     
  //    o = 1.f - i; 

  tmp = q + q * (1.0f + i * (1.0f + 1.1f * i));
  //tmp = q + q/(1.0008 - i);
  b0 += i * (g * a - b0 + tmp * (b0 - b1));
  b1 += i * (b0 - b1);


  //    tmp = g*a + q*(1.f + (1.f/o)) * (b0-b1);  //what about (q + q/f)*
  //    b0 = o * (b0 - tmp) + tmp;                // ^ what about div0 ?
  //    b1 = o * (b1 - b0) + b0;
  //    b2 = o * (b2 - b1) + b1;


      *++out1 = b1;
      *++out2 = b1;
    }
  }
  if (abs(b0) < 1E-10) then
   begin
    fBuffer[0] := 0;
    fBuffer[1] := 0;
    fBuffer[2] := 0;
   end
  else
   begin
    fBuffer[0] := b0;
    fBuffer[1] := b1;
    fBuffer[2] := b2;
   end;
  env   := e;
  env2  := e2;
  bufl  := bl;
  tatt  := ta;
  ttrig := tt;
  phi   := (float)fmod(ph, 2 * Pi);
*)
end;

procedure TRezFilterDataModule.VSTModuleSuspend(Sender: TObject);
begin
 fBuffer[0] := 0;
 fBuffer[1] := 0;
 fBuffer[2] := 0;
end;

end.

(*
void mdaRezFilter::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)(100 * Parameter[0]), text); break;
    case 1: long2string((long)(100 * Parameter[1]), text); break;
    case 2: long2string((long)(40 * Parameter[2] - 20),text); break;
    case 3: long2string((long)(200 * Parameter[3] - 100), text); break;
    case 4: float2strng((float)(-301.0301 / (getSampleRate() * log10(1.0 - att))),text); break;
    case 5: float2strng((float)(-301.0301 / (getSampleRate() * log10(rel))),text); break;
    case 6: long2string((long)(200 * Parameter[6] - 100), text); break;
    case 7: float2strng((float)pow(10.0f, 4.f*Parameter[7] - 2.f), text); break;
    case 8: if(tthr==0.f) strcpy(text, "FREE RUN"); 
            else long2string((long)(20*log10(0.5*tthr)), text); break;
    case 9: long2string((long)(100 * Parameter[9]), text); break;
  }
}
*)
