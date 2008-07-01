unit SubSynthDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TSubSynthDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TSubSynthDataModule.VSTModuleOpen(Sender: TObject);
begin
 (*
 //inits here!
 fParam1 := 0.0; //type
 fParam2 := 0.3; //level
 fParam3 := 0.6; //tune
 fParam4 := 1.0; //dry mix
 fParam5 := 0.6; //thresh
 fParam6 := 0.65; //release

 resume();
 *)
end;

procedure TSubSynthDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 (*
  dvd = 1.0;
  phs = 1.0;
  osc = 0.0; //oscillator phase
  typ = int(3.5 * fParam1);
  filti = (typ == 3)? 0.018f : (float)pow(10.0,-3.0 + (2.0 * fParam3));
  filto = 1.0f - filti;
  wet = fParam2;
  dry = fParam4;
  thr = Power(10, -3.0 + (3.0 * fParam5));
  rls = (float)(1.0 - Power(10.0, -2.0 - (3.0 * fParam6)));
  dphi = (float)(0.456159 * Power(10.0,-2.5 + (1.5 * fParam3)));
 *)
end;

procedure TSubSynthDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  a, b, c, d   : Single;
  we, dr, fi   : Single;
  fo, f1, f2   : Single;
  f3, f4, sub  : Single;
  rl, th, dv   : Single;
  ph, phii     : Single;
  dph, os, en  : Single;
begin
(*
 float *in1 = inputs[0];
 float *in2 = inputs[1];
 float *out1 = outputs[0];
 float *out2 = outputs[1];

  dph  := dphi;
  rl   := rls;
  phii := phi;
  en   := env;
  os   := osc;
  th   := thr;
  dv   := dvd;
  ph   := phs;
  we   := wet;
  dr   := dry;
  f1   := filt1;
  f2   := filt2;
  f3   := filt3;
  f4   := filt4;

  fi = filti;
  fo = filto;

  --in1;
 --in2;
 --out1;
 --out2;
 while(--sampleFrames >= 0)
 {
  a = *++in1;  
  b = *++in2; //process from here...
  
    f1 = (fo * f1) + (fi * (a + b));
    f2 = (fo * f2) + (fi * f1);

    sub = f2;
    if (sub > th)
    {
      sub = 1.0;        
    }
    else
    {
      if(sub < -th)
      {
        sub = -1.0;
      }
      else
      {
        sub = 0.0;
      }
    }
    
    if((sub * dv) < 0) //octave divider
    {
      dv = -dv; if(dv < 0.) ph = -ph;
    }

    if(typ == 1) //divide
    {
      sub = ph * sub;
    }
    if(typ == 2) //invert
    {
      sub = (float)(ph * f2 * 2.0);
    }
    if(typ == 3) //osc
    {
      if (f2 > th) {en = 1.0; } 
      else {en = en * rl;}
      sub = (float)(en * sin(phii));
      phii = (float)fmod( phii + dph, 6.283185f );
    }
    
    f3 = (fo * f3) + (fi * sub);
    f4 = (fo * f4) + (fi * f3);

  c = (a * dr) + (f4 * we); // output
  d = (b * dr) + (f4 * we);

    *++out1 = c;
  *++out2 = d;
 }
  if (abs(f1) <1E-10) then filt1 := 0 else filt1 := f1;
  if (abs(f2) <1E-10) then filt2 := 0 else filt2 := f2;
  if (abs(f3) <1E-10) then filt3 := 0 else filt3 := f3;
  if (abs(f4) <1E-10) then filt4 := 0 else filt4 := f4;

  dvd := dv;
  phs := ph;
  osc := os;
  phi := phii;
  env := en;
*)
end;

procedure TSubSynthDataModule.VSTModuleResume(Sender: TObject);
begin
//  phi = env = filt1 = filt2 = filt3 = filt4 = filti = filto = 0.0f;
end;

end.