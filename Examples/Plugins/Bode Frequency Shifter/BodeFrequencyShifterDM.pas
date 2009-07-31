unit BodeFrequencyShifterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFrequencyShifter;

type
  TBodeFrequencyShifterDataModule = class(TVSTModule)
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
    FFreqShifter     : array of TBodeFrequencyShifter32;
    FUpMix, FDownMix : Single; 
    procedure ChooseProcess;
  public
  end;

implementation

{$R *.DFM}

uses
  BodeFrequencyShifterGUI;

procedure TBodeFrequencyShifterDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FFreqShifter, numInputs);

 ChooseProcess;

 for Channel := 0 to Length(FFreqShifter) - 1
  do FFreqShifter[Channel] := TBodeFrequencyShifter32.Create;

 Parameter[0] := 100;
 Parameter[1] := 100;
 Parameter[2] := 12; 
 Parameter[3] := 0.1; 

(*
 Programs[0].Parameter[0] := 10;
 Programs[1].Parameter[0] := 0.1;
 Programs[2].Parameter[0] := 1000;
*)
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1
  do FreeAndNil(FFreqShifter[Channel]);
end;

procedure TBodeFrequencyShifterDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].Frequency := Value;

 if EditorForm is TFmBodeFrequencyShifter
  then TFmBodeFrequencyShifter(EditorForm).UpdateFrequency;
end;

procedure TBodeFrequencyShifterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FUpMix   := 0.5 * ((0.01 * Value) + 1);
 FDownMix := 1 - FUpMix;
 FUpMix   := sqrt(FUpMix);
 FDownMix := sqrt(FDownMix);

 if EditorForm is TFmBodeFrequencyShifter
  then TFmBodeFrequencyShifter(EditorForm).UpdateMix;
end;

procedure TBodeFrequencyShifterDataModule.ParameterCoeffsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].CoefficientCount := round(Value);
end;

procedure TBodeFrequencyShifterDataModule.ParameterTransitionBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].TransitionBandwidth := Value;
end;

procedure TBodeFrequencyShifterDataModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMultiChannel;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmBodeFrequencyShifter.Create(Self);
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
   Outputs[0, Sample] := FUpMix * Up + FDownMix * Down;
  end;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
   Outputs[0, Sample] := FUpMix * Up + FDownMix * Down;

   FFreqShifter[1].ProcessSample(Inputs[1, Sample], Up, Down);
   Outputs[1, Sample] := FUpMix * Up + FDownMix * Down;
  end;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleProcessMultiChannel(
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
    Outputs[Channel, Sample] := FUpMix * Up + FDownMix * Down;
   end;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].SampleRate := SampleRate;
end;

end.