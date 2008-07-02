unit ThruZeroDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TThruZeroDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
  private
    fBuffer : array [0..1] of TAVDSingleFixedArray;
  public
  end;

implementation

{$R *.DFM}

procedure TThruZeroDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
(*
  float *in1 = inputs[0];
  float *in2 = inputs[1];
  float *out1 = outputs[0];
  float *out2 = outputs[1];
  float a, b, f=fb, f1=fb1, f2=fb2, ph=phi;
  float ra=rat, de=dep, we=wet, dr=dry, ds=deps, dm=dem;
  long  tmp, tmpi, bp=bufpos;
  float tmpf, dpt;

  --in1;
  --in2;
  --out1;
  --out2;
  while(--sampleFrames >= 0)
  {
    a = *++in1;    
    b = *++in2; 
    
    ph += ra; 
    if(ph>1.0f) ph -= 2.0f;
    
    bp--; bp &= 0x7FF;
    *(buffer  + bp) := a + f * f1;
    *(buffer2 + bp) := b + f * f2;

    //ds := 0.995f * (ds - de) + de;          //smoothed depth change ...try inc not mult
    dpt  := tmpf = dm + de * (1.0f - ph * ph); //delay mod shape
    tmp  := int(tmpf);
    tmpf := tmpf - tmp;
    tmp  := (tmp + bp) & 0x7FF;
    tmpi := (tmp + 1) & 0x7FF;

    f1 := *(buffer  + tmp);  //try adding a constant to reduce denormalling
    f2 := *(buffer2 + tmp);
    f1 := tmpf * (*(buffer  + tmpi) - f1) + f1; //linear interpolation
    f2 := tmpf * (*(buffer2 + tmpi) - f2) + f2;

    a := a * dr - f1 * we;
    b := b * dr - f2 * we;

    *++out1 = a;
    *++out2 = b;
  }
  if (abs(f1) > 1E-10) then
   begin
    fb1 = f1;
    fb2 = f2;
   end
  else
   begin
    fb1 = 0.0f;
    fb2 = 0.0f; //catch denormals
   end;
  phi = ph;
  deps = ds;
  bufpos = bp;
*)
end;

procedure TThruZeroDataModule.VSTModuleResume(Sender: TObject);
begin
(*
  rat := Power(10, 3 * Parameter[0] - 2) * 2 / SampleRate;
  dep := 2000.0 * sqr(Parameter[1]);
  dem := dep - dep * Parameter[4];
  dep := dep - dem;
  
  wet := Parameter[2];
  dry := 1 - wet;
  if Parameter[0] < 0.01f then
   begin
    rat := 0;
    phi := 0;
   end; 
  fb := 1.9 * Parameter[3] - 0.95;
*)
end;

procedure TThruZeroDataModule.VSTModuleSuspend(Sender: TObject);
begin
  if assigned(fBuffer[0]) then FillChar(fBuffer[0], BUFMAX * SizeOf(Single), 0);
  if assigned(fBuffer[1]) then FillChar(fBuffer[1], BUFMAX * SizeOf(Single), 0);
end;

end.