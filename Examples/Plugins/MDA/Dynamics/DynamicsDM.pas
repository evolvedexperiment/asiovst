unit DynamicsDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TDynamicsDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateChangeRelease(Sender: TObject; const Index: Integer; var Value: Single);
    procedure Parameter0Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter1Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter2Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter3Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter4Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter5Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter6Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter7Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter8Display(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure Parameter9Display(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fMode              : Single;
    fThreshold         : Single;
    fRatio             : Single;
    fTrim              : Single;
    fAttack            : Single;
    fRelease           : Single;
    fLimiterThreshold  : Single;
    fExpanderThreshold : Single;
    fExpanderRatio     : Single;
    fIntRelease        : Single;
    fGateAttack        : Single;
    fDry               : Single;
    fEnv               : Array [0..2] of Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math;

procedure TDynamicsDataModule.ParameterGateChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fGateAttack := Power(10, -0.002 - 3 * Value);
end;

procedure TDynamicsDataModule.ParameterGateChangeRelease(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fExpanderRatio := 1 - Power(10, -2 - 3.3 * Value);
end;

procedure TDynamicsDataModule.Parameter0Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(40 * Parameter[0] - 40));
end;

procedure TDynamicsDataModule.Parameter1Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if Parameter[1] > 0.58 then
  if Parameter[1] < 0.62
   then PreDefined := 'Limit'
   else PreDefined := FloatToStr(-fRatio)
 else
  if(Parameter[1] < 0.2)
   then PreDefined := FloatToStr(0.5 + 2.5 * Parameter[1])
   else PreDefined := FloatToStr(1 / (1 - fRatio));
end;

procedure TDynamicsDataModule.Parameter2Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(40 * Parameter[2] - 0)); ///was -20.0
end;

procedure TDynamicsDataModule.Parameter3Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(-301030.1 / (SampleRate * log10(1.0 - fAttack))));
end;

procedure TDynamicsDataModule.Parameter4Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(-301.0301 / (SampleRate * log10(1.0 - fRelease))));
end;

procedure TDynamicsDataModule.Parameter5Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if fLimiterThreshold = 0
  then PreDefined := 'OFF'
  else IntToStr(round(30 * Parameter[5] - 20.0));
end;

procedure TDynamicsDataModule.Parameter6Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if fExpanderThreshold = 0
  then PreDefined := 'OFF'
  else PreDefined := IntToStr(round(60 * Parameter[6] - 60.0));
end;

procedure TDynamicsDataModule.Parameter7Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(-301030.1 / (SampleRate * log10(1.0 - fGateAttack))));
end;

procedure TDynamicsDataModule.Parameter8Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(-1806 / (SampleRate * log10(fExpanderRatio))));
end;

procedure TDynamicsDataModule.Parameter9Display(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(100 * Parameter[9]));
end;

procedure TDynamicsDataModule.ParameterAttackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fAttack  := Power(10, -0.002 - 2 * Value);
end;

procedure TDynamicsDataModule.ParameterReleaseChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fRelease := Power(10, -2 - 3 * Value);
end;

procedure TDynamicsDataModule.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] := 0.60; // Threshold   ///Note : special version for ardislarge
 Parameter[1] := 0.40; // Ratio
 Parameter[2] := 0.10; // Level      ///was 0.6
 Parameter[3] := 0.18; // Attack
 Parameter[4] := 0.55; // Release
 Parameter[5] := 1.00; // Limiter
 Parameter[6] := 0.00; // Gate Threshold
 Parameter[7] := 0.10; // Gate Attack
 Parameter[8] := 0.50; // Gate Decay
 Parameter[9] := 1.00; // FX Mix
end;

procedure TDynamicsDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 // calcs here
 fMode      := 0;
 fThreshold := Power(10, 2 * Parameter[0] - 2);
 fRatio      := 2.5 * Parameter[1] - 0.5;
 if (fRatio > 1) then
  begin
   fRatio := 1 + 16 * sqr(fRatio - 1);
   fMode := 1;
  end else
 if (fRatio < 0) then
  begin
   fRatio := 0.6 * fRatio;
   fMode := 1;
  end;
 fTrim   := Power(10, 2 * Parameter[2]); //was - 1);

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
 fIntRelease := Power(10, -2 / SampleRate);

 if (fRatio < 0) and (fThreshold < 0.1)
  then fRatio := fRatio * fThreshold * 15;

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
  e   := fEnv[0];
  e2  := fEnv[1];
  ge  := fEnv[2];
  ra  := fRatio;
  re  := (1 - fRelease);
  at  := fAttack;
  ga  := fGateAttack;
  tr  := fTrim;
  th  := fThreshold;
  lth := fLimiterThreshold;
  xth := fExpanderThreshold;
  y   := fDry;

 if fMode > 0 then // Comp / Gate / Limiter
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
     if g * e2 > lth then g := lth / e2; // Limit

     if e > xth
      then ge := ge + ga - ga * ge
      else ge := ge * fExpanderRatio; // Gate

     Outputs[0, Sample] := Inputs[0, Sample] * (g * ge + y);
     Outputs[1, Sample] := Inputs[1, Sample] * (g * ge + y);
    end;
  end
 else // Compressor only
  begin
   for Sample := 0 to SampleFrames - 1 do
    begin
     i := max(abs(Inputs[0, Sample]), abs(Inputs[1, Sample])); // Get peak level

     if i > e
      then e := e + at * (i - e)
      else e := e * re;                            // Envelope
     if e > th
      then g := tr / (1 + ra * ((e / th) - 1))
      else g := tr;                                // Gain

     Outputs[0, Sample] := Inputs[0, Sample] * (g + y); // VCA
     Outputs[1, Sample] := Inputs[1, Sample] * (g + y);
    end;
  end;

  if (e  < 1E-10) then fEnv[0] := 0 else fEnv[0] := e;
  if (e2 < 1E-10) then fEnv[1] := 0 else fEnv[1] := e2;
  if (ge < 1E-10) then fEnv[2] := 0 else fEnv[2] := ge;
end;

end.
