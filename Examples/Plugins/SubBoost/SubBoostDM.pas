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
    procedure VSTModuleCreate(Sender: TObject);

    procedure ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure ParameterTuneChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure SubBoostDataModuleParameterProperties6ParameterChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    fInputFilter  : TButterworthLP;
    fOutputFilter : TButterworthLP;

    fPhi          : Single;
    fEnv          : Single;
    fDivide       : Single;
    fPhase        : Single;
    fOsc          : Single;
    fType         : TProcessType;
    fWet          : Single;
    fDry          : Single;
    fThreshold    : Single;
    fRelease      : Single;
    fDeltaPhi     : Single;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, SubBoostGUI, DAV_VSTCustomModule;

procedure TSubBoostDataModule.ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fWet := 0.01 * Value;
end;

procedure TSubBoostDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fThreshold := dB_to_Amp(Parameter[4]);
end;

procedure TSubBoostDataModule.ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
// PreDefined := IntToStr(round(0.0726 * SampleRate * Power(10, -2.5 + (1.5 * Parameter[index]))));
 PreDefined := FloatToStrF(fInputFilter.Frequency, ffGeneral, 3, 3);
end;

procedure TSubBoostDataModule.SubBoostDataModuleParameterProperties6ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fInputFilter.Order := round(Value);
end;

procedure TSubBoostDataModule.ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fInputFilter.Frequency := Value;
 fOutputFilter.Frequency := Value;
 if assigned(EditorForm) then
  with TFmSubBoost(EditorForm) do
   begin
    UpdateTune;
   end;
end;

procedure TSubBoostDataModule.ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(-301.03 / (SampleRate * log10(fRelease))));
end;

procedure TSubBoostDataModule.ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRelease := 1 - Power(10, -2 - (3 * Parameter[5]));
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
 fDry := 0.01 * Value;
end;

procedure TSubBoostDataModule.VSTModuleCreate(Sender: TObject);
begin
 fInputFilter  := TButterworthLP.Create;
 fOutputFilter := TButterworthLP.Create;
 fOutputFilter.Order := 1;

 Parameter[0] :=  0;    // Type
 Parameter[1] := 30;    // Level
 Parameter[2] := 0.6;   // Tune
 Parameter[3] := 10;    // Dry Mix
 Parameter[4] := -24;   // Threshold
 Parameter[5] := 0.65;  // Release

 VSTModuleResume(Sender);
end;

procedure TSubBoostDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fInputFilter);
 FreeAndNil(fOutputFilter);
end;

procedure TSubBoostDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmSubBoost.Create(Self);
end;

procedure TSubBoostDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  fDivide := 1;
  fPhase  := 1;
  fOsc    := 0; // Oscillator phase
  fType   := TProcessType(round(Parameter[0]));
(*
  if fType = ptKeyOsc
   then fFilterIn := 0.018
   else fFilterIn := Power(10, -3 + (2 * Parameter[2]));
  fFilterOut := 1 - fFilterIn;
*)
  fDeltaPhi  := 0.456159 * Power(10, -2.5 + (1.5 * Parameter[2]));
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
 dph  := fDeltaPhi;
 rl   := fRelease;
 phii := fPhi;
 en   := fEnv;
 os   := fOsc;
 th   := fThreshold;
 dv   := fDivide;
 ph   := fPhase;

 for Sample := 0 to SampleFrames - 1 do
  begin
   // Input Filter
   FilteredIn := fInputFilter.ProcessSample(Inputs[0, Sample] + Inputs[1, Sample]);


   if FilteredIn * dv < 0 then     // Octave Divider
    begin
     dv := -dv;
     if dv < 0 then ph := -ph;
    end;

   case fType of
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
   end;

   // Output Filter
   SubBass := fOutputFilter.ProcessSample(SubBass);

   // Output
   Outputs[0, Sample] := Inputs[0, Sample] * fDry + SubBass * fWet;
   Outputs[1, Sample] := Inputs[1, Sample] * fDry + SubBass * fWet;
  end;

 fDivide := dv;
 fPhase  := ph;
 fOsc    := os;
 fPhi    := phii;
 fEnv    := en;
end;

procedure TSubBoostDataModule.VSTModuleResume(Sender: TObject);
begin
 fPhi     := 0;
 fEnv     := 0;
end;

procedure TSubBoostDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 fInputFilter.SampleRate := SampleRate;
 fOutputFilter.SampleRate := SampleRate;
end;

end.
