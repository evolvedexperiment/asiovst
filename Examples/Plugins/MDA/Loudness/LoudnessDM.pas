unit LoudnessDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule;

type
  TLoudnessDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure ParameterLinkDisplay( Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fIsBoost : Boolean;
    fGain    : Single;
    fIGain   : Single;
    fOGain   : Single;
    fCoeffs  : Array [0..2] of Single;
    fState   : Array [0..1, 0..1] of Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

const
  cLoudness : Array [0..13, 0..2] of Single =
    ( (402,  0.0025,  0.00),  //-60dB
      (334,  0.0121,  0.00),
      (256,  0.0353,  0.00),
      (192,  0.0900,  0.00),
      (150,  0.2116,  0.00),
      (150,  0.5185,  0.00),
      (  1,  0     ,  0.00),  //0dB
      (33.7,    5.5,  1.00),
      (92,      8.7,  0.62),
      (63.7,   18.4,  0.44),
      (42.9,   48.2,  0.30),
      (37.6,  116.2,  0.18),
      (22.9,  428.7,  0.09),  //+60dB
      (   0,      0,  0.00)  );


procedure TLoudnessDataModule.VSTModuleCreate(Sender: TObject);
begin
 VSTModuleSuspend(Sender);
(*
  Parameter[0] = 0.7;  //loudness
  Parameter[1] = 0.5;  //output
  Parameter[2] = 0.35; //link
*)
end;

procedure TLoudnessDataModule.ParameterLinkDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'ON'
  else PreDefined := 'OFF';
end;

procedure TLoudnessDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  z0, z1,
  z2, z3  : Single;
begin
  if (fIsBoost = False) then //cut
   for Sample := 0 to SampleFrames - 1 do
    begin
     z0 := z0 + fCoeffs[0] * (Inputs[0, Sample] - z0 + 0.3 * z1);
     Inputs[0, Sample] := Inputs[0, Sample] - z0;
     z1 := z1 + fCoeffs[0] * (Inputs[0, Sample] - z1);
     Inputs[0, Sample] := Inputs[0, Sample] - z1;
     Inputs[0, Sample] := Inputs[0, Sample] - z0 * fCoeffs[1];

     z2 := z2 + fCoeffs[0] * (Inputs[1, Sample] - z2 + 0.3 * z1);
     Inputs[1, Sample] := Inputs[1, Sample] - z2;
     z3 := z3 + fCoeffs[0] * (Inputs[1, Sample] - z3);
     Inputs[1, Sample] := Inputs[1, Sample] - z3;
     Inputs[1, Sample] := Inputs[1, Sample] - z2 * fCoeffs[1];

     Outputs[0, Sample] := Inputs[0, Sample] * fGain;
     Outputs[1, Sample] := Inputs[1, Sample] * fGain;
    end
  else //boost
   for Sample := 0 to SampleFrames - 1 do
    begin
      z0 := z0 + fCoeffs[0] * (Inputs[0, Sample]  - z0);
      z1 := z1 + fCoeffs[0] * (z0 - z1);
      Inputs[0, Sample]  := Inputs[0, Sample] + fCoeffs[1] * (z1 - fCoeffs[2] * z0);

      z2 := z2 + fCoeffs[0] * (Inputs[1, Sample]  - z2);
      z3 := z3 + fCoeffs[0] * (z2 - z3);
      Inputs[1, Sample] := Inputs[1, Sample] + fCoeffs[1] * (z3 - fCoeffs[2] * z2);

     Outputs[0, Sample] := Inputs[0, Sample] * fGain;
     Outputs[1, Sample] := Inputs[1, Sample] * fGain;
    end;

  if (abs(z1) < 1E-10) or (abs(z1) > 100) then
   begin
    fState[0, 0] := 0;
    fState[0, 1] := 0;
   end
  else
   begin
    fState[0, 0] := z0;
    fState[0, 1] := z1;
   end; //catch denormals
  if (abs(z3) < 1E-10) or (abs(z3) > 100) then
   begin
    fState[1, 0] := 0;
    fState[1, 1] := 0;
   end
  else
   begin
    fState[1, 0] := z2;
    fState[1, 1] := z3;
   end;
end;

procedure TLoudnessDataModule.VSTModuleResume(Sender: TObject);
var
  f, tmp : Single;
  i      : Integer;
begin
 tmp   := sqr(Parameter[0]) - 1;
 fIGain := 60 * sqr(tmp);
 if (tmp < 0) then fIGain := -fIGain ;
 tmp := Parameter[1] + Parameter[1] - 1;
 fOGain := 60 * sqr(tmp);
 if (tmp < 0)
  then fOGain := -fOGain;

 f := 0.1 * fIGain + 6;  //coefficient index + fractional part
 i := round(f);
 f := f - i;

 tmp := cLoudness[i][0];  fCoeffs[0] := tmp + f * (cLoudness[i + 1][0] - tmp);
 tmp := cLoudness[i][1];  fCoeffs[1] := tmp + f * (cLoudness[i + 1][1] - tmp);
 tmp := cLoudness[i][2];  fCoeffs[2] := tmp + f * (cLoudness[i + 1][2] - tmp);

 fCoeffs[0] := 1 - Exp(-6.283153 * fCoeffs[0] / SampleRate);

 if (fIGain > 0) then
  begin
   //if not fIsBoost then suspend();  //don't click when switching mode
   fIsBoost := True;
  end
 else
  begin
   //if fIsBoost then suspend();
   fIsBoost := False;
  end;

  tmp := fOGain;
  if (Parameter[2] > 0.5) then //linked gain
   begin
    tmp := tmp - fIGain;
    if (tmp > 0)
     then tmp := 0;  //limit max gain
   end
  else fGain := Power(10, 0.05 * tmp);
end;

procedure TLoudnessDataModule.VSTModuleSuspend(Sender: TObject);
begin
 fState[0, 0] := 0;
 fState[0, 1] := 0;
 fState[1, 0] := 0;
 fState[1, 1] := 0;
end;

end.

(*
void mdaLoudness::getParameterDisplay(VstInt32 index, char *text)
{
   char string[16];

  switch(index)
  {
    case  0: sprintf(string, "%.1f", igain); break;
    default: sprintf(string, "%.1f", ogain); break;
  }
  string[8] = 0;
  strcpy(text, (char * )string);
}

*)
