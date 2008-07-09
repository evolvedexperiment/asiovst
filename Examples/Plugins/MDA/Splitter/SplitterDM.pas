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
    fBuffer    : array [0..1, 0..1] of Single;
    fFreq, ff  : Single;
    fFreqDisp  : Single;
    fLevel, ll : Single;
    fLevelDisp : Single;
    fEnv, pp   : Single;
    fAttack    : Single;
    fRelease   : Single;
    i2l        : Single;
    i2r        : Single;
    o2l        : Single;
    o2r        : Single;
    fMode      : Integer;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

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
  
  Parameter[0] := 0.1;   // Mode
  Parameter[1] := 0.5;   // Freq
  Parameter[2] := 0.25;  // Freq Mode
  Parameter[3] := 0.5;   // Level (was 2)
  Parameter[4] := 0.5;   // Level Mode
  Parameter[5] := 0.5;   // Envelope
  Parameter[6] := 0.5;   // Gain
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
  lv, lx, px   : Single;
  il, ir, fx,
  l, r         : Single;
begin
  a0 := fBuffer[0, 0];
  a1 := fBuffer[0, 1];
  b0 := fBuffer[1, 0];
  b1 := fBuffer[1, 1];
  e  := fEnv;
  at := fAttack;
  re := fRelease;
  fx := ff;
  lv := fLevel;
  lx := ll;
  px := pp;
  il := i2l;
  ir := i2r;
  l  := o2l;
  r  := o2r;

 for Sample := 0 to SampleFrames - 1 do
  begin
    a := Inputs[0, Sample];
    b := Inputs[1, Sample];

    a0 := a0 + fFreq * (a - a0 - a1);  //frequency split
    a1 := a1 + fFreq * a0;
    aa := a1 + fx * a;

    b0 := b0 + fFreq * (b - b0 - b1);
    b1 := b1 + fFreq * b0;
    bb := b1 + fx * b;

    ee := -(aa + bb);

    if (ee > lv)
     then e := e + at * (px - e);  //fLevel split
    e := e * re;

    a := il * a + l * aa * (e + lx);
    b := ir * b + r * bb * (e + lx);

   Outputs[0, Sample] := a;
   Outputs[1, Sample] := b;
  end;

  fEnv := e;
  if (abs(e) < 1E-10)
   then fEnv := 0.0;
  fBuffer[0, 0] := a0;
  fBuffer[0, 1] := a1;
  fBuffer[1, 0] := b0;
  fBuffer[1, 1] := b1;

  if (abs(a0) < 1E-10) then
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
  fFreq     := Parameter[1];
  fFreqDisp := Power(10, 2 + 2 * fFreq);      // Frequency
  fFreq     := 5.5 * fFreqDisp / SampleRate;
  if (fFreq > 1) then fFreq := 1;

  ff  := -1;                                  // Above
  tmp := round(2.9 * Parameter[2]);           // Frequency Switching
  if tmp = 0 then ff := 0.0;                  // Below
  if tmp = 1 then fFreq := 0.001;             // All

  fLevelDisp := 40 * Parameter[3] - 40;       // Level
  fLevel := Power(10.0, 0.05 * fLevelDisp + 0.3);

  ll := 0.0;                                  // Above
  tmp := round(2.9 * Parameter[4]);           // Level Switching
  if (tmp = 0) then ll := -1;                 // Below
  if (tmp = 1) then fLevel := 0;              // All

  pp := -1;                                   // Phase Correction
  if (ff = ll) then pp := 1;
  if (ff = 0) and (ll = -1)
   then ll := -ll;

  fAttack  := 0.05 - 0.05 * Parameter[5];
  fRelease := 1 - exp(-6 - 4 * Parameter[5]); // Envelope
  if (fAttack  > 0.02)   then fAttack  := 0.02;
  if (fRelease < 0.9995) then fRelease := 0.9995;

  i2l := Power(10, 2 * Parameter[6] - 1);     // Gain
  i2r := i2l;
  o2l := i2l;
  o2r := i2l;

  fMode := round(3.9 * Parameter[0]);         // Output Routing
  case round(fMode) of
     0: begin i2l :=   0 ;  i2r :=   0 ; end;
     1: begin o2l := -o2l;  o2r := -o2r; end;
     2: begin i2l :=   0 ;  o2r := -o2r; end;
   else begin o2l := -o2l;  i2r :=   0 ; end;
  end;
end;

procedure TSplitterDataModule.VSTModuleSuspend(Sender: TObject);
begin
 fEnv := 0;
 fBuffer[0, 0] := 0;
 fBuffer[0, 1] := 0;
 fBuffer[1, 0] := 0;
 fBuffer[1, 1] := 0;
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
