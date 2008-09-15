unit DubDelayDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TDubDelayDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimiterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fEnv          : Single;
    fEnv2         : Single;
    fRatio        : Single;
    fAttack       : Single;
    fRelease      : Single;
    fIRelease     : Single;
    fGainAtt      : Single;
    fTrim         : Single;
    fThreshold    : Single;
    fLowThreshold : Single;
    fXThreshold   : Single;
    fXRate        : Single;
    fGEnv         : Single;
    fDry          : Single;
    fMode         : Integer;
    procedure RatioChanged;
    procedure TrimChanged;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDubDelayDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] := Amp_to_dB(-16); // Thresh
 Parameter[2] := Amp_to_dB(  4); // Level
 Parameter[9] := 100;            // FX Mix

(*
 Parameter[1] := 0.40;   // Ratio
 Parameter[3] := 0.18;   // Attack
 Parameter[4] := 0.55;   // Release
 Parameter[5] := 1.00;   // Limiter
 Parameter[6] := 0.00;   // Gate Thresh
 Parameter[7] := 0.10;   // Gate Attack
 Parameter[8] := 0.50;   // Gate Decay
*)
end;

procedure TDubDelayDataModule.ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRelease := Power(10, -2 - 3 * Value);
end;

procedure TDubDelayDataModule.ParameterLimiterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Value > 0.98)
  then fLowThreshold := 0  // Limiter
  else
   begin
    fLowThreshold := 0.99 * Power(10, round(30 * Value - 20) / 20);
    fMode := 1;
   end;
end;

procedure TDubDelayDataModule.ParameterGateThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Value < 0.02)
  then fXThreshold := 0    // Expander
  else
   begin
    fXThreshold  := Power (10, 3 * Value - 3);
    fMode := 1;
   end;
end;

procedure TDubDelayDataModule.ParameterGateReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fXRate    := 1 - Power(10, -2 - 3.3 * Value);
end;

procedure TDubDelayDataModule.ParameterGateAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fGainAtt  := Power(10, -0.002 - 3 * Value);
end;

procedure TDubDelayDataModule.ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fAttack := Power(10, -0.002 - 2 * Value);
end;

procedure TDubDelayDataModule.ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDry  := 1 - Value;
 TrimChanged;
end;

procedure TDubDelayDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 TrimChanged;
end;

procedure TDubDelayDataModule.ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRatio := 2.5 * Value - 0.5;
 RatioChanged;
end;

procedure TDubDelayDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fThreshold := dB_to_Amp(Value);
 RatioChanged;
end;

procedure TDubDelayDataModule.TrimChanged;
begin
 fTrim := dB_to_Amp(Parameter[2]) * 0.01 * Parameter[9]; //fx mix
end;

procedure TDubDelayDataModule.RatioChanged;
begin
 if (fRatio < 0) and (fThreshold < 0.1)
  then fRatio := fRatio * fThreshold * 15;

 if (fRatio > 1) then
  begin
   fRatio := 1 + 16 * sqr(fRatio - 1);
   fMode := 1;
  end;
 if (fRatio < 0) then
  begin
   fRatio := 0.6 * fRatio;
   fMode := 1;
  end;
end;

procedure TDubDelayDataModule.VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fMode := 0;
 fIRelease := Power(10, -2 / SampleRate);
end;

procedure TDubDelayDataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample                         : Integer;
  i, j, g, e, e2, ra, re, at, ga : Single;
  tr, th, lth, xth, ge           : Single;
begin
  e   := fEnv;
  e2  := fEnv2;
  ra  := fRatio;
  re  := 1 - fRelease;
  at  := fAttack;
  ga  := fGainAtt;
  tr  := fTrim;
  th  := fThreshold;
  lth := fLowThreshold;
  xth := fXThreshold;
  ge  := fGEnv;

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
  end;

 if (fMode > 0)  then //comp/gate/lim
  begin
   if (lth = 0) then lth := 1000;
    for Sample := 0 to SampleFrames - 1 do
     begin
      i := abs(Inputs[0, Sample]);
      j := abs(Inputs[1, Sample]);
      if (j > i)
       then i := j;

      if i > e
       then e := e + at * (i - e)
       else e := e * re;
      if i > e
       then e2 := i
       else e2 := e2 * re; //ir;

      if (e > th)
       then g := tr / (1 + ra * ((e / th) - 1))
       else g := tr;

      if (g < 0) then g := 0;
      if (g * e2 > lth)
       then g := lth / e2; //limit

      if (e > xth)
       then ge := ge + ga - ga * ge
       else ge := ge * fXRate; //gate

      Outputs[0, Sample] := Inputs[0, Sample] * (g * ge + fDry);
      Outputs[1, Sample] := Inputs[1, Sample] * (g * ge + fDry);
     end;
  end
 else //compressor only
  begin
   for Sample := 0 to SampleFrames - 1 do
    begin
     i := abs(Inputs[0, Sample]);
     j := abs(Inputs[1, Sample]);
     if j > i
      then i := j; //get peak level

     if i > e
      then e := e + at * (i - e)
      else e := e * re;           // Envelope
     if e > th
      then g := tr / (1 + ra * ((e / th) - 1))
      else g := tr;                            // Gain

     Outputs[0, Sample] := Inputs[0, Sample] * (g + fDry); //vca
     Outputs[1, Sample] := Inputs[1, Sample] * (g + fDry);
    end;
  end;

  if (e  < 1E-10) then fEnv  := 0 else fEnv  := e;
  if (e2 < 1E-10) then fEnv2 := 0 else fEnv2 := e2;
  if (ge < 1E-10) then fGEnv := 0 else fGEnv := ge;
end;

end.

(*
void mdaDynamics::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 1: if (Parameter[1] > 0.58)
            begin if(Parameter[1] < 0.62) strcpy(text, "Limit");
              else float2strng(-fRatio, text); end;
            else
            begin if(Parameter[1] < 0.2) float2strng(0.5 + 2.5 * Parameter[1], text);
              else float2strng(1 / (1 - fRatio), text); end; break;
    case 3: long2string((long)(-301030.1 / (SampleRate * log10(1.0 - fAttack))),text); break;
    case 4: long2string((long)(-301.0301 / (SampleRate * log10(1.0 - fRelease))),text); break;
    case 5: if (fLowThreshold = 0)
             then strcpy(text, "OFF");
             else long2string((long)(30.0*Parameter[5] - 20.0),text); break;
    case 6: if (fXThreshold = 0)
             then strcpy(text, "OFF");
             else long2string((long)(60.0*Parameter[6] - 60.0),text); break;
    case 7: long2string((long)(-301030.1 / (SampleRate * log10(1.0 - fGainAtt))),text); break;
    case 8: long2string((long)(-1806.0 / (SampleRate * log10(fXRate))),text); break;

  end;
end;
*)
