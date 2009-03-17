unit PlateReverbModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspPlateReverb;

type
  TPlateReverbVST = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessReplacing(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPreDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputDiffusionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChangeDiffusion(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModulationChange(Sender: TObject; const Index: Integer; var Value: Single);
  protected
    FPlateReverb : TPlateReverb;
    FCrossover   : Single;
    FState       : Single;
    FMix         : array [0..1] of Single;
  public
    property Dry: Single read FMix[0] write FMix[0];
    property Wet: Single read FMix[1] write FMix[1];
  end;

implementation

{$R *.DFM}

uses
  Math, PlateReverbGUI, DAV_VSTCustomModule;

procedure TPlateReverbVST.VSTModuleOpen(Sender: TObject);
begin
 FPlateReverb := TPlateReverb.Create;
 FPlateReverb.SampleRate := SampleRate;
 FCrossover := 0.01;

 // default parameter
 Parameter[0] := 50;
 Parameter[1] := 50;
 Parameter[2] := 10;
 Parameter[3] := 50;
 Parameter[4] := 13000;
 Parameter[5] := 75;
 Parameter[6] := 70;
 Parameter[7] := 50;

 // default preset
 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);

(*
 // preset 1
 with Programs[1] do
  begin
   Parameter[0] := 0.5;
   Parameter[1] := 0.6;
   Parameter[2] := 0.4;
   Parameter[3] := 0.5;
   Parameter[4] := 0;
   Parameter[5] := 0;
   Parameter[6] := 1;
  end;

 // preset 2
 with Programs[2] do
  begin
   Parameter[0] := 0.2;
   Parameter[1] := 0.6;
   Parameter[2] := 0.8;
   Parameter[3] := 1;
   Parameter[4] := 0;
   Parameter[5] := 1;
   Parameter[6] := 1;
  end;
*)

 // preset 3
 with Programs[3] do
  begin
   Parameter[0] := 100 * random;
   Parameter[1] := 100 * random;
   Parameter[2] := 100 * random;
   Parameter[3] := 100 * random;
   Parameter[4] := 100 * random;
   Parameter[5] := 100 * random;
   Parameter[6] := 100 * random;
   Parameter[7] := 100 * random;
  end;
end;

procedure TPlateReverbVST.ParameterDryChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Dry := 0.01 * Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDry;
end;

procedure TPlateReverbVST.ParameterWetChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Wet := 0.01 * Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateWet;
end;

procedure TPlateReverbVST.ParameterModulationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPlateReverb.Modulation := 0.01 * Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateModulation;
end;

procedure TPlateReverbVST.ParameterPreDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPlateReverb.PreDelay := 1E-3 * Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdatePreDelay;
end;

procedure TPlateReverbVST.ParameterDecayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPlateReverb.Decay := 0.0025 * Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDecay;
end;

procedure TPlateReverbVST.ParameterDampingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPlateReverb.DampingFrequency := Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDampingFrequency;
end;

procedure TPlateReverbVST.ParameterInputDiffusionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPlateReverb.InputDiffusion := 0.01 * Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateInputDiffusion;
end;

procedure TPlateReverbVST.ParameterDecayChangeDiffusion(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
  FPlateReverb.DecayDiffusion := 0.01 * Value;
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDecayDiffusion;
end;

procedure TPlateReverbVST.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FPlateReverb);
end;

procedure TPlateReverbVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmPlateReverb.Create(Self);
end;

procedure TPlateReverbVST.VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i: Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FState := FPlateReverb.ProcessSample(FCrossover * FState + (1 - FCrossover) * (Inputs[0, i] + Inputs[1, i]) * CHalf32);

   // Calculate output MIXING with anything already there
   Outputs[0, i] := Outputs[0, i] + FMix[0] * Inputs[0, i] + FMix[1] * FPlateReverb.OutputLeft;
   Outputs[1, i] := Outputs[1, i] + FMix[0] * Inputs[1, i] + FMix[1] * FPlateReverb.OutputRight;
  end;
end;

procedure TPlateReverbVST.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i: Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FState := FPlateReverb.ProcessSample(FCrossover * FState + (1 - FCrossover) * (Inputs[0, i] + Inputs[1, i]) * CHalf32);

   // Calculate output REPLACING with anything already there
   Outputs[0, i] := FMix[0] * Inputs[0, i] + FMix[1] * FPlateReverb.OutputLeft;
   Outputs[1, i] := FMix[0] * Inputs[1, i] + FMix[1] * FPlateReverb.OutputRight;
  end;
end;

procedure TPlateReverbVST.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 FPlateReverb.SampleRate := SampleRate;
end;

end.
