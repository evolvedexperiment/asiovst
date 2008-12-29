unit PhaserDM;

interface

uses 
  Windows, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule, DAV_DspPhaser;

type
  TPhaserModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure PMDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMMinimumChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMMaximumChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PMStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPhaser : array [0..1] of TPhaser;
  public
  end;

implementation

{$R *.DFM}

uses
  PhaserFrm;

procedure TPhaserModule.VSTModuleOpen(Sender: TObject);
begin
 FPhaser[0] := TPhaser.Create;
 FPhaser[1] := TPhaser.Create;
end;

procedure TPhaserModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FPhaser[0]);
 FreeAndNil(FPhaser[1]);
end;

procedure TPhaserModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TPhaserForm.Create(Self);
end;

procedure TPhaserModule.PMDepthChange(Sender: TObject; const Index: Integer;
  var Value: Single);
begin
 if Assigned(FPhaser[0]) and Assigned(FPhaser[1]) then
  begin
   FPhaser[0].Depth := 0.01 * Value;
   FPhaser[1].Depth := FPhaser[0].Depth;
   if EditorForm is TPhaserForm then
    with TPhaserForm(EditorForm) do
     if SBDepth.Position <> round(FPhaser[0].Depth)
      then SBDepth.Position := round(FPhaser[0].Depth);
  end;
end;

procedure TPhaserModule.PMFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FPhaser[0]) and Assigned(FPhaser[1]) then
  begin
   FPhaser[0].Feedback := 0.01 * Value;
   FPhaser[1].Feedback := FPhaser[0].Feedback;
   if EditorForm is TPhaserForm then
    with TPhaserForm(EditorForm) do
     if SBFeedback.Position <> round(FPhaser[0].Feedback)
      then SBFeedback.Position := round(FPhaser[0].Feedback);
  end;
end;

procedure TPhaserModule.PMMinimumChange(Sender: TObject; const Index: Integer;
  var Value: Single);
begin
 if Assigned(FPhaser[0]) and Assigned(FPhaser[1]) then
  begin
   FPhaser[0].Minimum := Value;
   FPhaser[1].Minimum := FPhaser[0].Minimum;
   if EditorForm is TPhaserForm then
    with TPhaserForm(EditorForm) do
     if SBMinimum.Position <> round(FPhaser[0].Minimum)
      then SBMinimum.Position := round(FPhaser[0].Minimum);
  end;
end;

procedure TPhaserModule.PMMaximumChange(Sender: TObject; const Index: Integer;
  var Value: Single);
begin
 if Assigned(FPhaser[0]) and Assigned(FPhaser[1]) then
  begin
   FPhaser[0].Maximum := Value;
   FPhaser[1].Maximum := FPhaser[0].Maximum;
   if EditorForm is TPhaserForm then
    with TPhaserForm(EditorForm) do
     if SBMaximum.Position <> round(FPhaser[0].Maximum)
      then SBMaximum.Position := round(FPhaser[0].Maximum);
  end;
end;

procedure TPhaserModule.PMRateChange(Sender: TObject; const Index: Integer;
  var Value: Single);
begin
 if Assigned(FPhaser[0]) and Assigned(FPhaser[1]) then
  begin
   FPhaser[0].Rate := Value;
   FPhaser[1].Rate := FPhaser[0].Rate;
   if EditorForm is TPhaserForm then
    with TPhaserForm(EditorForm) do
     if SBRate.Position <> round(FPhaser[0].Rate)
      then SBRate.Position := round(FPhaser[0].Rate);
  end;
end;

procedure TPhaserModule.PMStagesChange(Sender: TObject; const Index: Integer;
  var Value: Single);
begin
 if Assigned(FPhaser[0]) and Assigned(FPhaser[1]) then
  begin
   FPhaser[0].Stages := round(Value);
   FPhaser[1].Stages := FPhaser[0].Stages;
   if EditorForm is TPhaserForm then
    with TPhaserForm(EditorForm) do
     if SBDepth.Position <> FPhaser[0].Stages
      then SBDepth.Position := FPhaser[0].Stages;
  end;
end;

procedure TPhaserModule.VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var i: Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   outputs[0, i] := FPhaser[0].Process(inputs[0, i]);
   outputs[1, i] := FPhaser[0].Process(inputs[1, i]);
  end;
end;

procedure TPhaserModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var i: Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   outputs[0, i] := FPhaser[0].Process(inputs[0, i]);
   outputs[1, i] := FPhaser[0].Process(inputs[1, i]);
  end;
end;

end.
