unit BarberpoleShifterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFrequencyShifter;

type
  TBarberpoleShifterDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMultiChannel(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterMixChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCoeffsChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTransitionBWChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFreqShifter : array of TBodeFrequencyShifter32;
    FMix         : array [0..2] of Single;
    procedure ChooseProcess;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, BarberpoleShifterGUI;

procedure TBarberpoleShifterDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FFreqShifter, numInputs);

 ChooseProcess;

 for Channel := 0 to Length(FFreqShifter) - 1
  do FFreqShifter[Channel] := TBodeFrequencyShifter32.Create;

 Parameter[0] := 0.2;
 Parameter[1] := 40;
 Parameter[2] := 16;
 Parameter[3] := 0.02; 

(*
 Programs[0].Parameter[0] := 10;
 Programs[1].Parameter[0] := 0.1;
 Programs[2].Parameter[0] := 1000;
*)
end;

procedure TBarberpoleShifterDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1
  do FreeAndNil(FFreqShifter[Channel]);
end;

procedure TBarberpoleShifterDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].Frequency := Value;

 if EditorForm is TFmBarberpoleShifter
  then TFmBarberpoleShifter(EditorForm).UpdateFrequency;
end;

procedure TBarberpoleShifterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := max(0, -(0.01 * Value));
 FMix[2] := max(0, (0.01 * Value));
 FMix[1] := 1 - max(FMix[0], FMix[2]);

 if EditorForm is TFmBarberpoleShifter
  then TFmBarberpoleShifter(EditorForm).UpdateMix;
end;

procedure TBarberpoleShifterDataModule.ParameterCoeffsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].CoefficientCount := round(Value);
end;

procedure TBarberpoleShifterDataModule.ParameterTransitionBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].TransitionBandwidth := Value;
end;

procedure TBarberpoleShifterDataModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMultiChannel;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TBarberpoleShifterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmBarberpoleShifter.Create(Self);
end;

procedure TBarberpoleShifterDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
   Outputs[0, Sample] := FMix[1] * Inputs[0, Sample] +
     FMix[0] * Down + FMix[2] * Up;
  end;
end;

procedure TBarberpoleShifterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
   Outputs[0, Sample] := FMix[1] * Inputs[0, Sample] +
     FMix[0] * Down + FMix[2] * Up;

   FFreqShifter[1].ProcessSample(Inputs[1, Sample], Up, Down);
   Outputs[1, Sample] := FMix[1] * Inputs[1, Sample] +
     FMix[0] * Down + FMix[2] * Up;
  end;
end;

procedure TBarberpoleShifterDataModule.VSTModuleProcessMultiChannel(
  const Inputs, Outputs: TDAVArrayOfSingleDynArray;
  const SampleFrames: Integer);
var
  Channel  : Integer;
  Sample   : Integer;
  Up, Down : Single;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    FFreqShifter[Channel].ProcessSample(Inputs[Channel, Sample], Up, Down);
    Outputs[Channel, Sample] := FMix[1] * Inputs[Channel, Sample] +
      FMix[0] * Down + FMix[2] * Up;
   end;
end;

procedure TBarberpoleShifterDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].SampleRate := SampleRate;
end;

end.
