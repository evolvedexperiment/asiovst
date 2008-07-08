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
    procedure VSTModuleDestroy(Sender: TObject);
  private
    fBuffer : array [0..1] of TAVDSingleFixedArray;
  public
  end;

implementation

{$R *.DFM};

procedure TThruZeroDataModule.VSTModuleDestroy(Sender: TObject);
begin
 if fBuffer[0] then Dispose(fBuffer[0]);
 if fBuffer[1] then Dispose(fBuffer[1]);
end;

procedure TThruZeroDataModule.VSTModuleProcess(const Inputs,
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
  float a, b, f=fb, f1=fb1, f2=fb2, ph=phi;
  float ra=rat, de=dep, we=wet, dr=dry, ds=deps, dm=dem;
  long  tmp, tmpi, bp=bufpos;
  float tmpf, dpt;

  while(--sampleFrames >= 0)
  {
    a = *++in1;    
    b = *++in2; 
    
    ph := ph + ra; 
    if ph > 1
     then ph := ph - 2;

    bp--; bp := bp and $7FF;
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
  end;
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

(*
mdaThruZeroProgram::mdaThruZeroProgram() ///default program settings
begin
  param[0] = 0.30f;  //rate
  param[1] = 0.43f;  //depth
  param[2] = 0.47f;  //mix
  param[3] = 0.30f;  //feedback
  param[4] = 1.00f;  //minimum delay to stop LF buildup with feedback
end;

mdaThruZero::mdaThruZero(audioMasterCallback audioMaster): AudioEffectX(audioMaster, NPROGS, NPARAMS)
begin
  programs = new mdaThruZeroProgram[numPrograms];
  setProgram(0);
  
  ///differences from default program...
  programs[1].param[0] = 0.50f;
  programs[1].param[1] = 0.20f;
  programs[1].param[2] = 0.47f;
  strcpy(programs[1].name,"Phase Canceller");
  programs[2].param[0] = 0.60f;
  programs[2].param[1] = 0.60f;
  programs[2].param[2] = 0.35f;
  programs[2].param[4] = 0.70f;
  strcpy(programs[2].name,"Chorus Doubler");
  programs[3].param[0] = 0.75f;
  programs[3].param[1] = 1.00f;
  programs[3].param[2] = 0.50f;
  programs[3].param[3] = 0.75f;
  programs[3].param[4] = 1.00f;
  strcpy(programs[3].name,"Mad Modulator");

  ///initialise...
  bufpos  = 0;
  buffer  = new float[BUFMAX];
  buffer2 = new float[BUFMAX];
  phi = fb = fb1 = fb2 = deps = 0.0f;

  suspend();
end;

void  mdaThruZero::setParameter(VstInt32 index, float value)
begin 
  if(index==3) phi=0.0f; //reset cycle
  param[index] = value; resume(); 
end;

void mdaThruZero::getParameterDisplay(VstInt32 index, char *text)
begin
   char string[16];

  switch(index)
  begin
    case  0: if(param[0]<0.01f) strcpy (string, "-");
             else sprintf(string, "%.2f", (float)pow(10.0f ,2.0f - 3.0f * param[index])); break;
    case  1: sprintf(string, "%.2f", 1000.f * dep / getSampleRate()); break;
    case  3: sprintf(string, "%.0f", 200.0f * param[index] - 100.0f); break;
    default: sprintf(string, "%.0f", 100.0f * param[index]);
  end;
  string[8] = 0;
  strcpy(text, (char * )string);
end;

*)