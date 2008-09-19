unit LA1701DM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPDynamics, DAV_DSPLevelingAmplifier, DAV_DspButterworthFilter;

type
  TLA1701DataModule = class(TVSTModule)
    procedure ParamAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamHPFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamVUMeterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamVUMeterDisplayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamVUSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLSKFBChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacingBypass(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleSoftBypass(Sender: TObject; isBypass: Boolean);
  private
    fLA1701s            : TLevelingAmplifier;
    fOutLevel           : Double;
    fInLevel            : Double;
    fLevelFallOff_ms    : Double;
    fLevelFallOffFactor : Double;
    fMix                : array [0..1] of Double;
    fHighpass           : TButterworthHP;
    function GetGRReduction: Double;
    function GetInLevel_dB: Double;
    function GetOutLevel_dB: Double;
    function GetGRReduction_dB: Double;
    procedure CalculateLevelFallOff;
    procedure SetLevelFallOff_ms(const Value: Double);
  published
    property InLevel: Double read fInLevel;
    property InLevel_dB: Double read GetInLevel_dB;
    property OutLevel: Double read fOutLevel;
    property OutLevel_dB: Double read GetOutLevel_dB;
    property GRReduction: Double read GetGRReduction;
    property GRReduction_dB: Double read GetGRReduction_dB;
    property LevelFallOff_ms: Double read fLevelFallOff_ms write SetLevelFallOff_ms;
  end;

implementation

{$R *.DFM}

uses
  Dialogs, Math, EditorFrm, DAV_DspFilter;

procedure TLA1701DataModule.VSTModuleCreate(Sender: TObject);
begin
 fLA1701s  := TLevelingAmplifier.Create;
 fHighpass := TButterworthHP.Create;
 with fHighpass do
  begin
   SampleRate := Samplerate;
   Order      := 1;
   Bandwidth  := 1;
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

procedure TLA1701DataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fLA1701s);
 FreeAndNil(fHighpass);
end;

procedure TLA1701DataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmLA1701.Create(Self);
end;

procedure TLA1701DataModule.SKLInputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA1701s.Input_dB := Value;

 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
   if DialInput.Position <> Value then
    begin
     DialInput.Position := Value;
     UpdateInput;
    end;
end;

procedure TLA1701DataModule.SKLOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA1701s.Output_dB := Value;

 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
   if DialOutput.Position <> Value then
    begin
     DialOutput.Position := Value;
     UpdateOutput;
    end;
end;

procedure TLA1701DataModule.SKLSKFBChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA1701s.Knee := 0.1 * Value;

 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
   if DialKnee.Position <> Value then
    begin
     DialKnee.Position := Value;
     UpdateKnee;
    end;
end;

procedure TLA1701DataModule.SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA1701s.Ratio := 1 / Value;

 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
   if DialRatio.Position <> Log10(Value) then
    begin
     DialRatio.Position := Log10(Value);
     UpdateRatio;
    end;
end;

procedure TLA1701DataModule.SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA1701s.Release_ms := Value;

 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
   if DialRelease.Position <> Log10(Value) then
    begin
     DialRelease.Position := Log10(Value);
     UpdateRelease;
    end;
end;

procedure TLA1701DataModule.ParamOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
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

procedure TLA1701DataModule.ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1 then PreDefined := 'μs';
end;

procedure TLA1701DataModule.CalculateLevelFallOff;
begin
 fLevelFallOffFactor := exp(-ln2 / (fLevelFallOff_ms * 0.001 * SampleRate));
end;

function TLA1701DataModule.GetGRReduction: Double;
begin
 if assigned(fLA1701s)
  then result := fLA1701s.GainReductionFactor
  else result := 1;
end;

function TLA1701DataModule.GetGRReduction_dB: Double;
begin
 if assigned(fLA1701s)
  then result := fLA1701s.GainReduction_dB
  else result := 0;
end;

function TLA1701DataModule.GetInLevel_dB: Double;
begin
 result := Amp_to_dB(fInLevel);
end;

function TLA1701DataModule.GetOutLevel_dB: Double;
begin
 result := Amp_to_dB(fOutLevel);
end;

procedure TLA1701DataModule.ParamVUMeterDisplayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
   begin
    UpdateLevelState;
   end;
end;

procedure TLA1701DataModule.ParamOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TLA1701DataModule.ParamVUSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 LevelFallOff_ms := Value;
end;

procedure TLA1701DataModule.ParamHPOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 assert(round(Value) >= 0);
 if assigned(fHighpass)
  then fHighpass.Order := round(Value);
end;

procedure TLA1701DataModule.ParamHPOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(Parameter[Index]));
end;

procedure TLA1701DataModule.ParamHPFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(fHighpass)
  then fHighpass.Frequency := Value;
end;

procedure TLA1701DataModule.ParamVUMeterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0: Predefined := 'Input';
  1: Predefined := 'Gain Reduction';
  2: Predefined := 'Output';
 end;
end;

procedure TLA1701DataModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fMix[0] := sqrt(0.01 * Value);
 fMix[1] := 1 - fMix[0];
 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
   begin
    UpdateMix;
   end;
end;

procedure TLA1701DataModule.ParamAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1
  then PreDefined := FloatToStrF(1E3 * Parameter[Index], ffFixed, 3, 1)
  else PreDefined := FloatToStrF(      Parameter[Index], ffFixed, 3, 1);
end;

procedure TLA1701DataModule.ParamRatioDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := '1:' + PreDefined;
end;

procedure TLA1701DataModule.SetLevelFallOff_ms(const Value: Double);
begin
 if fLevelFallOff_ms <> Value then
  begin
   fLevelFallOff_ms := Value;
   CalculateLevelFallOff;
  end;
end;

procedure TLA1701DataModule.SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA1701s.Attack_ms := Value;

 if Assigned(EditorForm) then
  with EditorForm as TFmLA1701 do
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

procedure TLA1701DataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
  d : Double;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   d := Inputs[0, i] + Inputs[1, i];
   fInLevel := fLevelFallOffFactor * (fInLevel + SimpleDiode(abs(d) - fInLevel));
   fLA1701s.Sidechain(fHighpass.ProcessSample(d));

   Outputs[0, i] := fMix[0] * fLA1701s.ProcessSample(Inputs[0, i]) + fMix[1] * Inputs[0, i];
   Outputs[1, i] := fMix[0] * fLA1701s.ProcessSample(Inputs[1, i]) + fMix[1] * Inputs[1, i];

   d := Outputs[0, i] + Outputs[1, i];
   fOutLevel := fLevelFallOffFactor * (fOutLevel + SimpleDiode(d - fOutLevel));
  end;
end;

procedure TLA1701DataModule.VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
  d : Double;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   d := Inputs[0, i] + Inputs[1, i];
   fInLevel := fLevelFallOffFactor * (fInLevel + SimpleDiode(abs(d) - fInLevel));
   fLA1701s.Sidechain(fHighpass.ProcessSample(d));

   Outputs[0, i] := fMix[0] * fLA1701s.ProcessSample(Inputs[0, i]) + fMix[1] * Inputs[0, i];
   Outputs[1, i] := fMix[0] * fLA1701s.ProcessSample(Inputs[1, i]) + fMix[1] * Inputs[1, i];

   d := Outputs[0, i] + Outputs[1, i];
   fOutLevel := fLevelFallOffFactor * (fOutLevel + SimpleDiode(d - fOutLevel));
  end;
end;

procedure TLA1701DataModule.VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TLA1701DataModule.VSTModuleProcessDoubleReplacingBypass(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Double));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Double));
end;

procedure TLA1701DataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(fLA1701s)
  then fLA1701s.SampleRate := SampleRate;
 if Assigned(fHighpass)
  then fHighpass.SampleRate := SampleRate;
 CalculateLevelFallOff;
end;

procedure TLA1701DataModule.VSTModuleSoftBypass(Sender: TObject;
  isBypass: Boolean);
begin
// Parameter[0] := Integer(isBypass);
end;

end.
