unit DynamicsDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TDynamicsDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer;
      var Value: Single);
  private
  public
  end;

implementation

{$R *.DFM}

procedure TDynamicsDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
(*
  //calcs here
  mode=0;
  thr = (float)pow(10.f, 2.f * fParam1 - 2.f);
  rat = 2.5f * fParam2 - 0.5f;
  if(rat>1.0) { rat = 1.f + 16.f*(rat-1.f) * (rat - 1.f); mode = 1; }
  if(rat<0.0) { rat = 0.6f*rat; mode=1; }
  trim = (float)pow(10.f, 2.f * fParam3); //was  - 1.f);
  att = (float)pow(10.f, -0.002f - 2.f * fParam4);
  rel = (float)pow(10.f, -2.f - 3.f * fParam5);

  if(fParam6>0.98) lthr=0.f; //limiter
  else { lthr=0.99f*(float)pow(10.0f,int(30.0*fParam6 - 20.0)/20.f);
         mode=1; }

  if(fParam7<0.02) { xthr=0.f; } //expander
  else { xthr=(float)pow(10.f,3.f * fParam7 - 3.f); mode=1; }
  xrat = 1.f - (float)pow(10.f, -2.f - 3.3f * fParam9);
  irel = (float)pow(10.0,-2.0/getSampleRate());
  gatt = (float)pow(10.f, -0.002f - 3.f * fParam8);

  if(rat<0.0f && thr<0.1f) rat *= thr*15.f;

  dry = 1.0f - fParam10;  trim *= fParam10; //fx mix
*)
end;

procedure TDynamicsDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
(*
  float a, b, i, j, g, e=env, e2=env2, ra=rat, re=(1.f-rel), at=att, ga=gatt;
  float tr=trim, th=thr, lth=lthr, xth=xthr, ge=genv, y=dry;
*)

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
  end;

(*
  if(mode) //comp/gate/lim
  {
    if(lth==0.f) lth=1000.f;
    while(--sampleFrames >= 0)
    {
      a = *++in1;
      b = *++in2;
  
      i = (a<0.f)? -a : a;
      j = (b<0.f)? -b : b;
      i = (j>i)? j : i;
        
      e = (i>e)? e + at * (i - e) : e * re;
      e2 = (i>e)? i : e2 * re; //ir;
      
      g = (e>th)? tr / (1.f + ra * ((e/th) - 1.f)) : tr;

      if(g<0.f) g=0.f;
      if(g*e2>lth) g = lth/e2; //limit

      ge = (e>xth)? ge + ga - ga * ge : ge * xrat; //gate

      *++out1 = a * (g * ge + y);  
      *++out2 = b * (g * ge + y);  
    }
  }
  else //compressor only
  {
    while(--sampleFrames >= 0)
    {
      a = *++in1;
      b = *++in2;
  
      i = (a<0.f)? -a : a;
      j = (b<0.f)? -b : b;
      i = (j>i)? j : i; //get peak level
        
      e = (i>e)? e + at * (i - e) : e * re; //envelope
      g = (e>th)? tr / (1.f + ra * ((e/th) - 1.f)) : tr; //gain

      *++out1 = a * (g + y); //vca
      *++out2 = b * (g + y);  
    }
  }
*)

(*
  if(e <1.0e-10) env =0.f; else env =e;
  if(e2<1.0e-10) env2=0.f; else env2=e2;
  if(ge<1.0e-10) genv=0.f; else genv=ge;
*)
end;

end.

(*
mdaDynamics::mdaDynamics(audioMasterCallback audioMaster)	: AudioEffectX(audioMaster, 1, 10)	// 1 program, 4 parameters
{
  Parameter[0] := 0.60; //thresh     ///Note : special version for ardislarge
  Parameter[1] := 0.40; //ratio
  Parameter[2] := 0.10; //level      ///was 0.6
  Parameter[3] := 0.18; //attack
  Parameter[4] := 0.55; //release
  Parameter[5] := 1.00; //Limiter
  Parameter[6] := 0.00; //gate thresh
  Parameter[7] := 0.10; //gate attack
  Parameter[8] := 0.50; //gate decay
  Parameter[9] := 1.00; //fx mix

  setParameter(6, 0.f); //initial settings
}

void mdaDynamics::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)(40.0*Parameter[0] - 40.0),text); break;
    case 1: if(Parameter[1]>0.58) 
            { if(Parameter[1]<0.62) strcpy(text, "Limit"); 
              else float2strng(-rat,text); }
            else 
            { if(Parameter[1]<0.2) float2strng(0.5f+2.5f*Parameter[1],text); 
              else float2strng(1.f/(1.f-rat),text); } break;
    case 2: long2string((long)(40.0*Parameter[2] - 0.0),text); break; ///was -20.0
    case 3: long2string((long)(-301030.1 / (getSampleRate() * log10(1.0 - att))),text); break;
    case 4: long2string((long)(-301.0301 / (getSampleRate() * log10(1.0 - rel))),text); break;
    case 5: if(lthr==0.f) strcpy(text, "OFF"); 
            else long2string((long)(30.0*Parameter[5] - 20.0),text); break;
    case 6: if(xthr==0.f) strcpy(text, "OFF"); 
            else long2string((long)(60.0*Parameter[6] - 60.0),text); break;
    case 7: long2string((long)(-301030.1 / (getSampleRate() * log10(1.0 - gatt))),text); break;
    case 8: long2string((long)(-1806.0 / (getSampleRate() * log10(xrat))),text); break;
    case 9: long2string((long)(100.0*Parameter[9]),text); break;

  }
}
*)
