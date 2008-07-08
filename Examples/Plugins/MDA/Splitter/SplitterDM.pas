unit SplitterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TSplitterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFreqLevelModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fBuffer : array [0..1, 0..1] of Single;
  public
  end;

implementation

{$R *.DFM}

procedure TSplitterDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'NORMAL';
  1 : PreDefined := 'INVERSE';
  2 : PreDefined := 'NORM/INV';
  3 : PreDefined := 'INV/NORM';
 end;
end;

procedure TSplitterDataModule.ParameterFreqLevelModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
    0: PreDefined := 'BELOW';
    1: PreDefined := 'ALL';
  else PreDefined := 'ABOVE';
 end;
end;

procedure TSplitterDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
  programs = new mdaSplitterProgram[numPrograms];
  setProgram(0);
  
  ///differences from default program...
  programs[1].Parameter[2] = 0.50;
  programs[1].Parameter[4] = 0.25;
  strcpy(programs[1].name,"Pass Peaks Only");
  programs[2].Parameter[0] = 0.60;
  strcpy(programs[2].name,"Stereo Crossover");
  
  Parameter[0] := 0.10;  // Mode
  Parameter[1] := 0.50;  // Freq
  Parameter[2] := 0.25;  // Freq Mode
  Parameter[3] := 0.50;  // Level (was 2)
  Parameter[4] := 0.50;  // Level Mode
  Parameter[5] := 0.50;  // Envelope
  Parameter[6] := 0.50;  // Gain
*)
 VSTModuleSuspend(Sender);
end;

procedure TSplitterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample       : Integer;
  a, b         : Single;
  a0, a1       : Single;
  b0, b1       : Single;
  aa, bb, ee,
  e, at, re,
  l, lx, px    : Single;
  il, ir,
  ol, or_      : Single;
begin
  a0 := fBuffer[0, 0];
  a1 := fBuffer[0, 1];
  b0 := fBuffer[1, 0];
  b1 := fBuffer[1, 1];
(*
  fx := ff;
  float aa, bb, ee, e = env, at = fAttack, re = fRelease, l = fLevel, lx = ll, px = pp;
  float il = i2l, ir = i2r, ol = o2l, or_ = o2r;
*)

 for Sample := 0 to SampleFrames - 1 do
  begin
    a := Inputs[0, Sample];
    b := Inputs[1, Sample];

(*
    a0 := a0 + f * (a - a0 - a1);  //frequency split
    a1 := a1 + f * a0;
    aa := a1 + fx * a;

    b0 := b0 + f * (b - b0 - b1);
    b1 := b1 + f * b0;
    bb := b1 + fx * b;

    ee := -(aa + bb);

    if (ee > l)
     then e := e + at * (px - e);  //fLevel split
    e := e * re;

    a := il * a + ol  * aa * (e + lx);
    b := ir * b + or_ * bb * (e + lx);
*)

   Outputs[0, Sample] := a;
   Outputs[1, Sample] := b;
  end;

(*
  fEnv := e;
  if (abs(e) < 1E-10)
   then fEnv := 0.0;
  fBufer[0, 0] := a0;
  fBufer[0, 1] := a1;
  fBufer[1, 0] := b0;
  fBufer[1, 1] := b1;

  if (abs(a0) < 1E-10) then
*)
   begin
    fBuffer[0, 0] := 0;
    fBuffer[0, 1] := 0;
    fBuffer[1, 0] := 0;
    fBuffer[1, 1] := 0;
   end;  //catch denormals
end;

procedure TSplitterDataModule.VSTModuleResume(Sender: TObject);
var
  tmp : Integer;
begin
(*
  fFreq     := Parameter[1];
  fFreqDisp := Power(10, 2 + 2 * fFreq);     // frequency
  fFreq     := 5.5 * fFreqDisp / SampleRate;
  if (fFreq > 1) then fFreq := 1;

  ff  := -1;                                 // above
  tmp := round(2.9 * Parameter[2]);          // frequency switching
  if(tmp==0) ff = 0.0;     //below
  if(tmp==1) fFreq = 0.001; //all

  fLevelDisp := 40 * Parameter[3] - 40;      // Level
  fLevel := Power(10.0, 0.05 * fLevelDisp + 0.3);

  ll = 0.0;                                  // above
  tmp = (long)(2.9 * Parameter[4]);          // Level switching
  if (tmp = 0) then ll := -1;                // below
  if (tmp = 1) then fLevel := 0;             // all

  pp := -1;  //phase correction
  if (ff = ll) then pp = 1.0;
  if (ff = 0) and (ll = -1)
   then ll := -ll;

  fAttack = 0.05 - 0.05 * Parameter[5];
  fRelease = 1.0 - (float)exp(-6.0 - 4.0 * Parameter[5]); //envelope
  if (fAttack  > 0.02)   then fAttack  := 0.02;
  if (fRelease < 0.9995) then fRelease := 0.9995;

  i2l := Power(10, 2 * Parameter[6] - 1);  //gain 
  i2r := Power(10, 2 * Parameter[6] - 1);  //gain
  o2l := Power(10, 2 * Parameter[6] - 1);  //gain
  o2r := Power(10, 2 * Parameter[6] - 1);  //gain

  fMode = (long)(3.9 * Parameter[0]);  //output routing
  case round(fMode) of
     0: i2l  =  0.0;  i2r  =  0.0;  break;
     1: o2l *= -1.0;  o2r *= -1.0;  break;
     2: i2l  =  0.0;  o2r *= -1.0;  break;
   else o2l *= -1.0;  i2r  =  0.0;  break;
  end;
*)
end;

procedure TSplitterDataModule.VSTModuleSuspend(Sender: TObject);
begin
(*
 fEnv := 0;
 fBufer[0, 0] := 0;
 fBufer[0, 1] := 0;
 fBufer[1, 0] := 0;
 fBufer[1, 1] := 0;
*)
end;

end.

(*
void mdaSplitter::getParameterDisplay(VstInt32 index, char *text)
{
   char string[16];

  switch(index)
  {
    case  1: sprintf(string, "%.0f", fFreqDisp); break;
    case  3: sprintf(string, "%.0f", fLevelDisp); break;
    case  5: sprintf(string, "%.0f", (float)Power(10.0, 1.0 + 2.0 * Parameter[index])); break;
    case  6: sprintf(string, "%.1f", 40.0 * Parameter[index] - 20.0); break;
    default: switch((long)(2.9 * Parameter[index]))
             {
                case  0: strcpy (string, "BELOW"); break;
                case  1: strcpy (string, "ALL"); break;
                default: strcpy (string, "ABOVE"); break;
             } break;
  }
  string[8] = 0;
  strcpy(text, (char * )string);
}

*)
