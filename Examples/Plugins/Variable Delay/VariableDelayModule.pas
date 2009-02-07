unit VariableDelayModule;

interface

uses
  Windows, Types, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspVariableDelay;

type
  TVariableDelayVST = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDryMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FVariDelay    : array [0..1] of TCustomVariableDelay32;
    FMix          : array [0..1] of Single;
  end;

implementation

{$R *.DFM}

uses
  VariableDelayGUI, DAV_VSTCustomModule;

procedure TVariableDelayVST.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
(*
 FVariDelay[0] := TVariableDelay32Hermite.Create; // Hermite
 FVariDelay[1] := TVariableDelay32Linear.Create;  // vs. Linear
*)

 FVariDelay[0] := TVariableDelay32Allpass.Create; // Hermite
 FVariDelay[1] := TVariableDelay32Allpass.Create;  // vs. Linear

 Parameter[0] := 100;
 Parameter[1] := 0;
 Parameter[2] := 100;
end;

procedure TVariableDelayVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TVariableDelayVST.SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FVariDelay) - 1
  do FVariDelay[Channel].Delay := 1E-4 * Value;

 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateDelayLength;
end;

procedure TVariableDelayVST.ParameterWetMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;

 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateWetMix;
end;

procedure TVariableDelayVST.ParamDryMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.01 * Value;

 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateDryMix;
end;

procedure TVariableDelayVST.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FVariDelay) - 1
   do Outputs[Channel, Sample] := FMix[0] * Inputs[Channel, Sample] +
        FMix[1] * FVariDelay[Channel].ProcessSample(Inputs[Channel, Sample]);
end;

procedure TVariableDelayVST.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FVariDelay) - 1
   do Outputs[Channel, Sample] := FMix[0] * Inputs[Channel, Sample] +
        FMix[1] * FVariDelay[Channel].ProcessSample(Inputs[Channel, Sample]);
end;

end.
