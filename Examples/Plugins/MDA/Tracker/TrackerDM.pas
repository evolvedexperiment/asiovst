unit TrackerDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TTrackerDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure TrackerDataModuleParameterProperties0CustomParameterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fMode      : Single;
    fThreshold : Single;
    fMax       : Single;
    fTrans     : Single;
    fWet       : Single;
    fDyn       : Single;
    fRel       : Single;
    function filterFreq(Hz: Double): Double;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TTrackerDataModule.TrackerDataModuleParameterProperties0CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0: PreDefined := 'SINE';
  1: PreDefined := 'SQUARE';
  2: PreDefined := 'SAW';
  3: PreDefined := 'RING';
  4: PreDefined := 'EQ';
 end;
end;

procedure TTrackerDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
 //inits here!
 Parameter[0] := 0; //fMode
 Parameter[1] := 1; //Dynamics
 Parameter[2] := 1; //Mix
 Parameter[3] := 0.97; //Tracking
 Parameter[4] := 0.5; //Trnspose
 Parameter[5] := 0.8; //Maximum Hz
 Parameter[6] := 0.5; //Trigger dB
 Parameter[7] := 0.5; //Output

 dphi := 100 / SampleRate; //initial pitch
 min  := SampleRate / 30.0; //lower limit
 res1 := cos(0.01); //p
 res2 := sin(0.01); //q

 setParameter(0, 0.0f);
*)
end;

function TTrackerDataModule.filterFreq(Hz: Double) : Double;
var
  j, k, r : Double;
begin
 r := 0.999;
 j := r * r - 1;
 k := 2 - 2 * sqr(r) * cos(0.647 * Hz / SampleRate);
 result := (sqrt(k * k - 4 * j * j) - k) / (2 * j);
end;

procedure TTrackerDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
(*
 //calcs here
 fMode := round(Parameter[0]*4.9);
 fo := filterFreq(50.f);
 fi := (1 - fo) * (1 - fo);
 ddphi := sqr(Parameter[3]);
 fThreshold := Power(10, 3 * Parameter[6] - 3.8);
 fMax := round(SampleRate / Power(10, 1.6 + 2.2 * Parameter[5]));
 fTrans := Power(1.0594631, round(72 * Parameter[4] - 36));
 fWet := Power(10, 2 * Parameter[7] - 1);
 if (fMode < 4) then
  begin
   fDyn := fWet * 0.6 * Parameter[2] * Parameter[1];
   fDry := fWet * sqrt(1 - Parameter[2]);
   fWet := fWet * 0.3 * Parameter[2] * (1 - Parameter[1]);
  end
 else
  begin
   fDry := fWet * (1 - Parameter[2]);
   fWet := fWet * (0.02 * Parameter[2] - 0.004);
   fDyn := 0;
  end;
*)
 fRel := Power(10, -10 / SampleRate);
end;

procedure TTrackerDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
  end;
(*
  float a, b, x, t=fThreshold, p=fPhi, dp=dphi, ddp=ddphi, tmp, tmp2;
  float o=fo, i=fi, b1=buf1, b2=buf2, twopi=6.2831853f;
  float we=fWet, dr=fDry, bo=fBold, r1=res1, r2=res2, b3=buf3, b4=buf4;
  float sw=saw, dsw=dsaw, dy=fDyn, e=fEnv, re=rel;
  long  m=fMax, n=num, s=sig, mn=min, mo=fMode;

  while(--sampleFrames >= 0)
  {
    a = *++in1;
    b = *++in2;
    x = a;// + b;

    tmp = (x>0.f)? x : -x; //dynamics envelope
    e = (tmp>e)? 0.5f*(tmp + e) : e * re;

    b1 = o*b1 + i*x; 
    b2 = o*b2 + b1; //low-pass filter
    
    if(b2>t) //if >thresh
    {  
      if(s<1) //and was <thresh
      {
        if(n<mn) //not long ago
        {
          tmp2 = b2 / (b2 - bo); //update period
          tmp = fTrans*twopi/(n + dn - tmp2); 
          dp = dp + ddp * (tmp - dp); 
          dn = tmp2;
          dsw = 0.3183098f * dp;
          if(fMode==4) 
          {
            r1 = cos(4.f*dp); //resonator
            r2 = sin(4.f*dp);
          }  
        }
        n=0; //restart period measurement
      }
      s=1;
    }
    else 
    {
      if(n>m) s=0; //now <thresh 
    }
    n++;
    bo=b2;

    p = fmod(p+dp,twopi);
    switch(mo)
    { //sine
      case 0: x=sin(p); break; 
      //square
      case 1: x=(sin(p)>0.f)? 0.5f : -0.5f; break; 
      //saw
      case 2: sw = fmod(sw+dsw,2.0f); x = sw - 1.f; break; 
      //ring
      case 3: x *= sin(p); break;
      //filt
      case 4: x += (b3 * r1) - (b4 * r2); 
              b4 = 0.996f * ((b3 * r2) + (b4 * r1)); 
              b3 = 0.996f * x; break; 
    }    
    x *= (we + dy * e); 
    *++out1 = a;//dr*a + x;
    *++out2 = dr*b + x;
  }
  if(fabs(b1)<1.0e-10) {buf1=0.f; buf2=0.f; buf3=0.f; buf4=0.f; } 
  else {buf1=b1; buf2=b2; buf3=b3; buf4=b4;}
  fPhi=p; dphi=dp; sig=s; fBold=bo;
  num=(n>100000)? 100000: n; 
  fEnv=e; saw=sw; dsaw=dsw; res1=r1; res2=r2;
*)
end;

end.

(*

void mdaTracker::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 1: long2string((long)(100 * Parameter[1]), text); break;
    case 2: long2string((long)(100 * Parameter[2]), text); break;
    case 3: long2string((long)(100 * Parameter[3]), text); break;
    case 4: long2string((long)(72 * Parameter[4] - 36), text); break;
    case 5: long2string((long)(SampleRate / fMax), text); break;
    case 6: long2string((long)(60*Parameter[6] - 60), text); break;
    case 7: long2string((long)(40*Parameter[7] - 20), text); break;
  }
}

*)
