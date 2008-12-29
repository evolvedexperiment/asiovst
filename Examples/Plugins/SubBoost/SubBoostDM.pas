unit SubBoostDM;

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter;

type
  TProcessType = (ptDistort, ptDivide, ptInvert, ptKeyOsc);

  TSubBoostDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);

    procedure ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
  private
    FInputFilter  : TButterworthLP;
    FOutputFilter : TButterworthLP;

    FPhi          : Single;
    FEnv          : Single;
    FDivide       : Single;
    FPhase        : Single;
    FOsc          : Single;
    FType         : TProcessType;
    FWet          : Single;
    FDry          : Single;
    FThreshold    : Single;
    FRelease      : Single;
    FDeltaPhi     : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, SubBoostGUI, DAV_VSTCustomModule;

procedure TSubBoostDataModule.VSTModuleOpen(Sender: TObject);
begin
 FInputFilter  := TButterworthLP.Create;
 FOutputFilter := TButterworthLP.Create;
 FOutputFilter.Order := 1;

 Parameter[0] :=  0;    // Type
 Parameter[1] := 30;    // Level
 Parameter[2] := 0.6;   // Tune
 Parameter[3] := 10;    // Dry Mix
 Parameter[4] := -24;   // Threshold
 Parameter[5] := 0.65;  // Release

 VSTModuleResume(Sender);
end;

procedure TSubBoostDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FInputFilter);
 FreeAndNil(FOutputFilter);
end;

procedure TSubBoostDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmSubBoost.Create(Self);
end;

procedure TSubBoostDataModule.ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FWet := 0.01 * Value;
end;

procedure TSubBoostDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Parameter[4]);
end;

procedure TSubBoostDataModule.ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
// PreDefined := IntToStr(round(0.0726 * SampleRate * Power(10, -2.5 + (1.5 * Parameter[index]))));
 PreDefined := FloatToStrF(FInputFilter.Frequency, ffGeneral, 3, 3);
end;

procedure TSubBoostDataModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FInputFilter.Order := round(Value);
end;

procedure TSubBoostDataModule.ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FInputFilter.Frequency := Value;
 FOutputFilter.Frequency := Value;
 if assigned(EditorForm) then
  with TFmSubBoost(EditorForm) do
   begin
    UpdateTune;
   end;
end;

procedure TSubBoostDataModule.ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(-301.03 / (SampleRate * log10(FRelease))));
end;

procedure TSubBoostDataModule.ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FRelease := 1 - Power(10, -2 - (3 * Parameter[5]));
end;

procedure TSubBoostDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of 
  0: PreDefined := 'Distort';
  1: PreDefined := 'Divide';
  2: PreDefined := 'Invert';
  3: PreDefined := 'Key Osc.';
 end;
end;

procedure TSubBoostDataModule.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDry := 0.01 * Value;
end;

procedure TSubBoostDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FDivide := 1;
  FPhase  := 1;
  FOsc    := 0; // Oscillator phase
  FType   := TProcessType(round(Parameter[0]));
(*
  if FType = ptKeyOsc
   then fFilterIn := 0.018
   else fFilterIn := Power(10, -3 + (2 * Parameter[2]));
  fFilterOut := 1 - fFilterIn;
*)
  FDeltaPhi  := 0.456159 * Power(10, -2.5 + (1.5 * Parameter[2]));
end;

procedure TSubBoostDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  FilteredIn   : Single;
  SubBass      : Single;
  rl, th, dv   : Single;
  ph, phii     : Single;
  dph, os, en  : Single;

  Sample       : Integer;

const
  CDenormalThreshold: Single = 1E-16;  
begin
 dph  := FDeltaPhi;
 rl   := FRelease;
 phii := FPhi;
 en   := FEnv;
 os   := FOsc;
 th   := FThreshold;
 dv   := FDivide;
 ph   := FPhase;

 for Sample := 0 to SampleFrames - 1 do
  begin
   // Input Filter
   FilteredIn := FInputFilter.ProcessSample(Inputs[0, Sample] + Inputs[1, Sample]);


   if FilteredIn * dv < 0 then     // Octave Divider
    begin
     dv := -dv;
     if dv < 0 then ph := -ph;
    end;

   case FType of
    ptDistort : if FilteredIn > th then SubBass := 1 else
                 if FilteredIn < -th
                  then SubBass := -1
                  else SubBass := 0;
    ptDivide  : if FilteredIn > th then SubBass := ph else
                 if FilteredIn < -th
                  then SubBass := ph
                  else SubBass := 0;
    ptInvert  : SubBass := ph * FilteredIn * 2;  // Invert
    ptKeyOsc  : begin                    // Osc
                 if (FilteredIn > th)
                  then en := 1
                  else en := en * rl;
                 SubBass  := (en * sin(phii));
                 phii     := f_mod(phii + dph, 2 * Pi);
                end;
    else SubBass := 0;            
   end;

   // Output Filter
   SubBass := FOutputFilter.ProcessSample(SubBass);

   // Output
   Outputs[0, Sample] := Inputs[0, Sample] * FDry + SubBass * FWet;
   Outputs[1, Sample] := Inputs[1, Sample] * FDry + SubBass * FWet;
  end;

 FDivide := dv;
 FPhase  := ph;
 FOsc    := os;
 FPhi    := phii;
 FEnv    := en;
end;

procedure TSubBoostDataModule.VSTModuleResume(Sender: TObject);
begin
 FPhi     := 0;
 FEnv     := 0;
end;

procedure TSubBoostDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FInputFilter.SampleRate := SampleRate;
 FOutputFilter.SampleRate := SampleRate;
end;

end.
