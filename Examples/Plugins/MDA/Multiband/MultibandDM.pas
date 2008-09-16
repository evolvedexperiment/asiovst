unit MultibandDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TMultibandDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParameterOutputDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterGainDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fDriveL    : Single;
    fTrimL     : Single;
    fAttackL   : Single;
    fReleaseL  : Single;
    fDriveM    : Single;
    fTrimM     : Single;
    fAttackM   : Single;
    fReleaseM  : Single;
    fDriveH    : Single;
    fTrimH     : Single;
    fAttackH   : Single;
    fReleaseH  : Single;
    fSLev      : Single;
    fGainL     : Single;
    fGainM     : Single;
    fGainH     : Single;
    fFeedback  : Array [0..2] of Single;
    fFi        : Array [0..1, 0..1] of Single;
    fMSwap     : Boolean;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TMultibandDataModule.ParameterGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(40 * Parameter[Index] - 20, ffGeneral, 2, 2);
end;

procedure TMultibandDataModule.ParameterOutputDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Low';
  1 : PreDefined := 'Mid';
  2 : PreDefined := 'High';
  3 : PreDefined := 'Output';
 end;
end;

procedure TMultibandDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[ 6] := 0.5;   // L trim   (2)
 Parameter[ 7] := 0.5;   // M trim
 Parameter[ 8] := 0.5;   // H trim
{
 //inits here!
 Parameter[ 0] := 1.00;  // Listen: L/M/H/out
 Parameter[ 1] := 0.50;  // xover1
 Parameter[ 2] := 0.50;  // xover2
 Parameter[ 3] := 0.45;  // L drive  (1)
 Parameter[ 4] := 0.45;  // M drive
 Parameter[ 5] := 0.45;  // H drive
 Parameter[ 9] := 0.22;  // attack   (3)
 Parameter[10] := 0.60;  // release  (4)
 Parameter[11] := 0.50;  // width
 Parameter[12] := 0.40;  // MS swap*/
}
end;

procedure TMultibandDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //calcs here
 fDriveL   := Power(10, (2.5 * Parameter[3]) - 1);
 fTrimL    := 0.5 + (4 - 2 * Parameter[9]) * (sqr(Parameter[3]) * Parameter[3]);
 fTrimL    := fTrimL * Power(10, 2.0 * Parameter[6] - 1);
 fAttackL  := Power(10, -0.05 -(2.5 * Parameter[9]));
 fReleaseL := Power(10, -2 - (3.5 * Parameter[10]));

 fDriveM   := Power(10, (2.5 * Parameter[4]) - 1);
 fTrimM    := 0.5 + (4 - 2 * Parameter[9]) * (sqr(Parameter[4]) * Parameter[4]);
 fTrimM    := fTrimM * Power(10, 2 * Parameter[7] - 1);
 fAttackM  := Power(10, -0.05 -(2 * Parameter[9]));
 fReleaseM := Power(10, -2 - (3 * Parameter[10]));

 fDriveH   := Power(10, (2.5 * Parameter[5]) - 1);
 fTrimH    := 0.5 + (4 - 2 * Parameter[9]) * (sqr(Parameter[5]) * Parameter[5]);
 fTrimH    := fTrimH * Power(10, 2 * Parameter[8] - 1);
 fAttackH  := Power(10, -0.05 -(1.5 * Parameter[9]));
 fReleaseH := Power(10, -2 - (2.5 * Parameter[10]));

 case round(Parameter[0] * 10) of
     0: begin fTrimM := 0; fTrimH := 0; fSLev := 0; end;
  1, 2: begin fTrimL := 0; fTrimH := 0; fSLev := 0; end;
  3, 4: begin fTrimL := 0; fTrimM := 0; fSLev := 0; end;
   else fSLev := Parameter[11];
 end;

 fFi[0, 0] := Power(10, Parameter[1] - 1.70); fFi[1, 0] := (1 - fFi[0, 0]);
 fFi[0, 1] := Power(10, Parameter[2] - 1.05); fFi[1, 1] := (1 - fFi[0, 1]);

 fMSwap := (Parameter[12] > 0.5);
end;

procedure TMultibandDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample              : Integer;
  a, b, c, d, m, h, s : Single;
  tmp1, tmp2, tmp3    : Single;
  l, sl               : Single;
  f1i, f1o, f2i, f2o  : Single;
  b1, b2              : Single;
  g1, d1, t1, a1, r1  : Single;
  g2, d2, t2, a2, r2  : Single;
  g3, d3, t3, a3, r3  : Single;
  ms                  : Boolean;
begin
 l   := fFeedback[2];
 sl  := fSLev;
 f1i := fFi[0, 0];
 f1o := fFi[1, 0];
 f2i := fFi[0, 1];
 f2o := fFi[1, 1];
 b1  := fFeedback[0];
 b2  := fFeedback[1];
 g1  := fGainL;
 d1  := fDriveL;
 t1  := fTrimL;
 a1  := fAttackL;
 r1  := 1 - fReleaseL;
 g2  :=fGainM;
 d2 := fDriveM;
 t2 := fTrimM;
 a2 := fAttackM;
 r2 := 1 - fReleaseM;
 g3  :=fGainH;
 d3 := fDriveH;
 t3 := fTrimH;
 a3 := fAttackH;
 r3 := 1 - fReleaseH;
 ms := fMSwap;

 for Sample := 0 to SampleFrames - 1 do
  begin
   a := Inputs[0, Sample];
   b := Inputs[1, Sample]; //process from here...

   if ms
    then b := -b
    else b := b;

   s  := (a - b) * sl; //keep stereo component for later
   a  := a + b;
   b2 := (f2i * a) + (f2o * b2); //crossovers
   b1 := (f1i * b2) + (f1o * b1);
   l  := (f1i * b1) + (f1o * l);
   m  := b2 - l;
   h  := a - b2;

   tmp1 := abs(l);  //l
   if (tmp1 > g1)
    then g1 := g1 + a1 * (tmp1 - g1)
    else g1 := g1 * r1;
   tmp1 := 1 / (1 + d1 * g1);

   tmp2 := abs(m);
   if (tmp2 > g2)
    then g2 := g2 + a2 * (tmp2 - g2)
    else g2 := g2 * r2;
   tmp2 := 1 / (1 + d2 * g2);

   tmp3 := abs(h);
   if (tmp3 > g3)
    then g3 := g3 + a3 * (tmp3 - g3)
    else g3 := g3 * r3;
   tmp3 := 1 / (1 + d3 * g3);

   a := (l * tmp1 * t1) + (m * tmp2 * t2) + (h * tmp3 * t3);
   c := a + s; // output
   if ms
    then d := s - a
    else d := a - s;

   Outputs[0, Sample] := c;
   Outputs[1, Sample] := d;
  end;

  if (g1 < 1E-10) then fGainL := 0 else fGainL := g1;
  if (g2 < 1E-10) then fGainM := 0 else fGainM := g2;
  if (g3 < 1E-10) then fGainH := 0 else fGainH := g3;

  if (abs(b1) < 1E-10) then
   begin
    fFeedback[0] := 0;
    fFeedback[1] := 0;
    fFeedback[2] := 0;
   end
  else
   begin
    fFeedback[0] := b1;
    fFeedback[1] := b2;
    fFeedback[2] := l;
   end;

(*
 // do not use this code, it's just an example on how a VCA would look like
 fGain := (1 / (1 + d1 * abs(l)) ); //VCAs
 if (g1 > fGain)
  then g1 := g1 - a1 * (g1 - g)
  else g1 := g1 + r1 * (g - g1);
*)
end;

end.

(*
void mdaMultiBand::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 1: long2string((long)(SampleRate * fFi[0,0] * (0.098 + 0.09 * fFi[0,0] + 0.5 * Power(fFi[0,0], 8.2))), text); break;
    case 2: long2string((long)(SampleRate * fFi[0,1] * (0.015 + 0.15 * fFi[0,1] + 0.9 * Power(fFi[0,1], 8.2))), text); break;
    case 3: long2string((long)(30.0 * Parameter[3]), text); break;
    case 4: long2string((long)(30.0 * Parameter[4]), text); break;
    case 5: long2string((long)(30.0 * Parameter[5]), text); break;
    case 6: long2string((long)(40.0 * Parameter[6] - 20.0), text); break;
    case 7: long2string((long)(40.0 * Parameter[7] - 20.0), text); break;
    case 8: long2string((long)(40.0 * Parameter[8] - 20.0), text); break;
    case 9: long2string((long)(-301030.1 / (SampleRate * log10(1.0 - fAttackM))),text); break;
    case 10: long2string((long)(-301.0301 / (SampleRate * log10(1.0 - fReleaseM))),text); break;
    case 11: long2string((long)(200.0 * Parameter[11]), text); break;
    case 12: if(fMSwap) strcpy(text, "S"); 
                  else strcpy(text, "M"); break;
  }
}

*)
