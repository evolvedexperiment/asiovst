unit DynamicsDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TDynamicsDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
  private
    // calcs here
    fMode              : Single;
    fThreshold         : Single;
    fRate              : Single;
    fTrim              : Single;
    fAttack            : Single;
    fRelease           : Single;
    fLimiterThreshold  : Single;
    fExpanderThreshold : Single;
    fExpanderRate      : Single;
    fIntRelease        : Single;
    fGateAttack        : Single;
    fDry               : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDynamicsDataModule.VSTModuleCreate(Sender: TObject);
begin
(*
 Parameter[0] := 0.60; // thresh     ///Note : special version for ardislarge
 Parameter[1] := 0.40; // ratio
 Parameter[2] := 0.10; // level      ///was 0.6
 Parameter[3] := 0.18; // attack
 Parameter[4] := 0.55; // release
 Parameter[5] := 1.00; // Limiter
 Parameter[6] := 0.00; // gate thresh
 Parameter[7] := 0.10; // gate attack
 Parameter[8] := 0.50; // gate decay
 Parameter[9] := 1.00; // fx mix

 setParameter(6, 0.f); //initial settings
*)
end;

procedure TDynamicsDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 // calcs here
 fMode      := 0;
 fThreshold := Power(10, 2 * Parameter[0] - 2);
 fRate      := 2.5 * Parameter[1] - 0.5;
 if (fRate > 1) then
  begin
   fRate := 1 + 16 * sqr(fRate - 1);
   fMode := 1;
  end else
 if (fRate < 0) then
  begin
   fRate := 0.6 * fRate;
   fMode := 1;
  end;
 fTrim    := Power(10, 2 * Parameter[2]); //was  - 1.f);
 fAttack  := Power(10, -0.002 - 2 * Parameter[3]);
 fRelease := Power(10, -2 - 3 * Parameter[4]);

 if (Parameter[5] > 0.98)
  then fLimiterThreshold := 0 // Limiter
  else
   begin
    fLimiterThreshold  := 0.99 * Power(10, round(30 * Parameter[5] - 20) / 20);
    fMode := 1;
   end;

 if (Parameter[6] < 0.02)
  then fExpanderThreshold := 0 // Expander
  else
   begin
    fExpanderThreshold  := Power(10, 3 * Parameter[6] - 3);
    fMode := 1;
   end;
 fExpanderRate := 1 - Power(10, -2 - 3.3 * Parameter[8]);
 fIntRelease := Power(10, -2 / SampleRate);
 fGateAttack := Power(10, -0.002 - 3 * Parameter[7]);

 if (fRate < 0.0) and (fThreshold < 0.1)
  then fRate := fRate * fThreshold * 15;

 fDry   := 1 - Parameter[9];
 fTrim := fTrim * Parameter[9]; //fx mix
end;

procedure TDynamicsDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample: Integer;
  a, b, i, j, g, e, e2, ra, re, at, ga : Single;
  tr, th, lth, xth, ge, y : Single;
begin
(*
  e   := env;
  e2  := env2;
  ra  := fRate;
  re  := (1 - fRelease);
  at  := fAttack;
  ga  := fGateAttack;
  tr  := fTrim;
  th  := fThreshold;
  lth := fLimiterThreshold;
  xth := fExpanderThreshold;
  ge  := genv;
  y   := fDry;
*)

 if fMode > 0 then //comp/gate/lim
  begin
   if lth = 0 then lth := 1000;
   for Sample := 0 to SampleFrames - 1 do
    begin
     i := max(abs(Inputs[0, Sample]), abs(Inputs[1, Sample]));

     if (i > e)
      then e := e + at * (i - e)
      else e := e * re;
     if (i > e)
      then e2 := i
      else e2 := e2 * re; //ir;

     if e > th
      then g := tr / (1 + ra * ((e / th) - 1))
      else g := tr;

     if g < 0 then g := 0;
     if g * e2 > lth then g := lth / e2; //limit

     if e > xth
      then ge := ge + ga - ga * ge
      else ge := ge * fExpanderRate; //gate

     Outputs[0, Sample] := Inputs[0, Sample] * (g * ge + y);
     Outputs[1, Sample] := Inputs[1, Sample] * (g * ge + y);
    end;
  end
 else //compressor only
  begin
   for Sample := 0 to SampleFrames - 1 do
    begin
      i := max(abs(Inputs[0, Sample]), abs(Inputs[1, Sample])); //get peak level

      if i > e
       then e := e + at * (i - e)
       else e := e * re;                            // Envelope
      if e > th
       then g := tr / (1 + ra * ((e / th) - 1))
       else g := tr;                                // Gain

      Outputs[0, Sample] := Inputs[0, Sample] * (g + y); //vca
      Outputs[1, Sample] := Inputs[1, Sample] * (g + y);
    end;
  end;

(*
  if (e  < 1E-10) then env  := 0 else env  := e;
  if (e2 < 1E-10) then env2 := 0 else env2 := e2;
  if (ge < 1E-10) then genv := 0 else genv := ge;
*)         
end;

end.

(*
void mdaDynamics::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 0: long2string((long)(40.0*Parameter[0] - 40.0),text); break;
    case 1: if(Parameter[1]>0.58) 
            begin if(Parameter[1]<0.62) strcpy(text, "Limit"); 
              else float2strng(-fRate,text); end;
            else 
            begin if(Parameter[1]<0.2) float2strng(0.5f+2.5f*Parameter[1],text);
              else float2strng(1.f/(1.f-fRate),text); end; break;
    case 2: long2string((long)(40.0*Parameter[2] - 0.0),text); break; ///was -20.0
    case 3: long2string((long)(-301030.1 / (SampleRate * log10(1.0 - fAttack))),text); break;
    case 4: long2string((long)(-301.0301 / (SampleRate * log10(1.0 - fRelease))),text); break;
    case 5: if(fLimiterThreshold==0.f) strcpy(text, "OFF");
            else long2string((long)(30 * Parameter[5] - 20.0),text); break;
    case 6: if(fExpanderThreshold==0.f) strcpy(text, "OFF");
            else long2string((long)(60.0 * Parameter[6] - 60.0),text); break;
    case 7: long2string((long)(-301030.1 / (SampleRate * log10(1.0 - fGateAttack))),text); break;
    case 8: long2string((long)(-1806 / (SampleRate * log10(fExpanderRate))),text); break;
    case 9: long2string((long)(100 * Parameter[9]),text); break;

  end;
end;
*)
