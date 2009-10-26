unit LA4029DM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DSPDynamics, DAV_DSPLevelingAmplifier, DAV_DSPFilterButterworth;

type
  TLA4029DataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacingBypass(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleSoftBypass(Sender: TObject; isBypass: Boolean);
    procedure ParamAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamHPFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamVUMeterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamVUMeterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamVUSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLSKFBChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLA4029s            : TLevelingAmplifier;
    FOutLevel           : Double;
    FInLevel            : Double;
    FLevelFallOff_ms    : Double;
    FLevelFallOffFactor : Double;
    FMix                : array [0..1] of Double;
    FHighpass           : TButterworthHighpassFilter;
    function GetGRReduction: Double;
    function GetInLevel_dB: Double;
    function GetOutLevel_dB: Double;
    function GetGRReduction_dB: Double;
    procedure CalculateLevelFallOff;
    procedure SetLevelFallOff_ms(const Value: Double);
  published
    property InLevel: Double read FInLevel;
    property InLevel_dB: Double read GetInLevel_dB;
    property OutLevel: Double read FOutLevel;
    property OutLevel_dB: Double read GetOutLevel_dB;
    property GRReduction: Double read GetGRReduction;
    property GRReduction_dB: Double read GetGRReduction_dB;
    property LevelFallOff_ms: Double read FLevelFallOff_ms write SetLevelFallOff_ms;
  end;

implementation

{$R *.DFM}

uses
  Dialogs, Math, EditorFrm, DAV_Common, DAV_Math, DAV_DspFilter;

procedure TLA4029DataModule.VSTModuleOpen(Sender: TObject);
begin
 FLA4029s  := TLevelingAmplifier.Create;
 FHighpass := TButterworthHighpassFilter.Create;
 with FHighpass do
  begin
   SampleRate := Samplerate;
   Order      := 1;
   SetFilterValues(5, 0);
  end;

 Parameter[ 0] :=   0;
 Parameter[ 1] :=   0;
 Parameter[ 2] :=   0;
 Parameter[ 3] :=   1;
 Parameter[ 4] := 100;
 Parameter[ 5] :=  10;
 Parameter[ 6] :=   5;
 Parameter[ 7] := 100;
 Parameter[ 8] :=   0;
 Parameter[ 9] :=  50;
 Parameter[10] :=  10;
 Parameter[11] :=   1;
end;

procedure TLA4029DataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLA4029s);
 FreeAndNil(FHighpass);
end;

procedure TLA4029DataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmLA4029.Create(Self);
end;

procedure TLA4029DataModule.SKLInputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLA4029s)
  then FLA4029s.Input_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmLA4029 then
  with EditorForm as TFmLA4029 do
   if DialInput.Position <> Value then
    begin
     DialInput.Position := Value;
     UpdateInput;
    end;
end;

procedure TLA4029DataModule.SKLOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLA4029s)
  then FLA4029s.Output_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmLA4029 then
  with EditorForm as TFmLA4029 do
   if DialOutput.Position <> Value then
    begin
     DialOutput.Position := Value;
     UpdateOutput;
    end;
end;

procedure TLA4029DataModule.SKLSKFBChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLA4029s)
  then FLA4029s.Knee := 0.1 * Value;

 // update GUI if necessary
 if EditorForm is TFmLA4029 then
  with EditorForm as TFmLA4029 do
   if DialKnee.Position <> Value then
    begin
     DialKnee.Position := Value;
     UpdateKnee;
    end;
end;

procedure TLA4029DataModule.SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLA4029s)
  then FLA4029s.Ratio := 1 / Value;

 // update GUI if necessary
 if EditorForm is TFmLA4029 then
  with EditorForm as TFmLA4029 do
   if DialRatio.Position <> Log10(Value) then
    begin
     DialRatio.Position := Log10(Value);
     UpdateRatio;
    end;
end;

procedure TLA4029DataModule.SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLA4029s)
  then FLA4029s.Release_ms := Value;

 // update GUI if necessary
 if EditorForm is TFmLA4029 then
  with EditorForm as TFmLA4029 do
   if DialRelease.Position <> Log10(Value) then
    begin
     DialRelease.Position := Log10(Value);
     UpdateRelease;
    end;
end;

procedure TLA4029DataModule.ParamOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value < 0.5 then
  begin
   OnProcess := VSTModuleProcess;
   OnProcessReplacing := VSTModuleProcess;
   OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacing;
  end
 else
  begin
   OnProcess := VSTModuleProcessBypass;
   OnProcessReplacing := VSTModuleProcessBypass;
   OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacingBypass;
  end;
end;

procedure TLA4029DataModule.ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1 then PreDefined := 'µs';
end;

procedure TLA4029DataModule.CalculateLevelFallOff;
begin
 FLevelFallOffFactor := exp(-ln2 / (FLevelFallOff_ms * 0.001 * SampleRate));
end;

function TLA4029DataModule.GetGRReduction: Double;
begin
 if assigned(FLA4029s)
  then result := FLA4029s.GainReductionFactor
  else result := 1;
end;

function TLA4029DataModule.GetGRReduction_dB: Double;
begin
 if assigned(FLA4029s)
  then result := FLA4029s.GainReduction_dB
  else result := 0;
end;

function TLA4029DataModule.GetInLevel_dB: Double;
begin
 result := Amp_to_dB(FInLevel);
end;

function TLA4029DataModule.GetOutLevel_dB: Double;
begin
 result := Amp_to_dB(FOutLevel);
end;

procedure TLA4029DataModule.ParamVUMeterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(EditorForm) then
  with EditorForm as TFmLA4029 do
   begin
    UpdateLevelState;
   end;
end;

procedure TLA4029DataModule.ParamOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TLA4029DataModule.ParamVUSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 LevelFallOff_ms := Value;
end;

procedure TLA4029DataModule.ParamHPOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 assert(round(Value) >= 0);
 if assigned(FHighpass)
  then FHighpass.Order := round(Value);
end;

procedure TLA4029DataModule.ParamHPOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(Parameter[Index]));
end;

procedure TLA4029DataModule.ParamHPFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FHighpass)
  then FHighpass.Frequency := Value;
end;

procedure TLA4029DataModule.ParamVUMeterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0: Predefined := 'Input';
  1: Predefined := 'Gain Reduction';
  2: Predefined := 'Output';
 end;
end;

procedure TLA4029DataModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := sqrt(0.01 * Value);
 FMix[1] := 1 - FMix[0];
 if Assigned(EditorForm) then
  with EditorForm as TFmLA4029 do
   begin
    UpdateMix;
   end;
end;

procedure TLA4029DataModule.ParamAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1
  then PreDefined := FloatToStrF(1E3 * Parameter[Index], ffFixed, 3, 1)
  else PreDefined := FloatToStrF(      Parameter[Index], ffFixed, 3, 1);
end;

procedure TLA4029DataModule.ParamRatioDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := '1:' + PreDefined;
end;

procedure TLA4029DataModule.SetLevelFallOff_ms(const Value: Double);
begin
 if FLevelFallOff_ms <> Value then
  begin
   FLevelFallOff_ms := Value;
   CalculateLevelFallOff;
  end;
end;

procedure TLA4029DataModule.SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLA4029s)
  then FLA4029s.Attack_ms := Value;

 // update GUI if necessary
 if EditorForm is TFmLA4029 then
  with EditorForm as TFmLA4029 do
   if DialAttack.Position <> Log10(Value) then
    begin
     DialAttack.Position := Log10(Value);
     UpdateAttack;
    end;
end;

function SimpleDiode(x: Single): Single;
begin
 Result := 0.5 * (abs(x) + x);
end;

procedure TLA4029DataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
  d : Double;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   d := Inputs[0, i] + Inputs[1, i];
   FInLevel := FLevelFallOffFactor * (FInLevel + SimpleDiode(abs(d) - FInLevel));
   FLA4029s.Sidechain(FHighpass.ProcessSample64(d));

   Outputs[0, i] := FMix[0] * FLA4029s.ProcessSample64(Inputs[0, i]) + FMix[1] * Inputs[0, i];
   Outputs[1, i] := FMix[0] * FLA4029s.ProcessSample64(Inputs[1, i]) + FMix[1] * Inputs[1, i];

   d := Outputs[0, i] + Outputs[1, i];
   FOutLevel := FLevelFallOffFactor * (FOutLevel + SimpleDiode(d - FOutLevel));
  end;
end;

procedure TLA4029DataModule.VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
  d : Double;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   d := Inputs[0, i] + Inputs[1, i];
   FInLevel := FLevelFallOffFactor * (FInLevel + SimpleDiode(abs(d) - FInLevel));
   FLA4029s.Sidechain(FHighpass.ProcessSample64(d));

   Outputs[0, i] := FMix[0] * FLA4029s.ProcessSample64(Inputs[0, i]) + FMix[1] * Inputs[0, i];
   Outputs[1, i] := FMix[0] * FLA4029s.ProcessSample64(Inputs[1, i]) + FMix[1] * Inputs[1, i];

   d := Outputs[0, i] + Outputs[1, i];
   FOutLevel := FLevelFallOffFactor * (FOutLevel + SimpleDiode(d - FOutLevel));
  end;
end;

procedure TLA4029DataModule.VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TLA4029DataModule.VSTModuleProcessDoubleReplacingBypass(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Double));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Double));
end;

procedure TLA4029DataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FLA4029s)
  then FLA4029s.SampleRate := SampleRate;
 if Assigned(FHighpass)
  then FHighpass.SampleRate := SampleRate;
 CalculateLevelFallOff;
end;

procedure TLA4029DataModule.VSTModuleSoftBypass(Sender: TObject;
  isBypass: Boolean);
begin
// Parameter[0] := Integer(isBypass);
end;

end.
