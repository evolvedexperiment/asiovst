unit RingModulatorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspRingModulator;

type
  TRingModulatorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMultiChannel(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FRingMod : array of TAutoRingModulator32;
    procedure ChooseProcess;
  public
  end;

implementation

{$R *.DFM}

uses
  RingModulatorGUI;

procedure TRingModulatorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FRingMod, numInputs);

 ChooseProcess;

 for Channel := 0 to Length(FRingMod) - 1
  do FRingMod[Channel] := TAutoRingModulator32.Create;

 Parameter[0] := 10;

 Programs[0].Parameter[0] := 10;
 Programs[1].Parameter[0] := 0.1;
 Programs[2].Parameter[0] := 1000;
end;

procedure TRingModulatorDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1
  do FreeAndNil(FRingMod[Channel]);
end;

procedure TRingModulatorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmRingModulator.Create(Self);
end;

procedure TRingModulatorDataModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMultiChannel;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TRingModulatorDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1 do
  if assigned(FRingMod[Channel])
   then FRingMod[Channel].Frequency := Value;

 if EditorForm is TFmRingModulator
  then TFmRingModulator(EditorForm).UpdateFrequency;
end;

procedure TRingModulatorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Outputs[0, Sample] := FRingMod[0].ProcessSample32(Inputs[0, Sample]);
end;

procedure TRingModulatorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FRingMod[0].ProcessSample32(Inputs[0, Sample]);
   Outputs[1, Sample] := FRingMod[1].ProcessSample32(Inputs[1, Sample]);
  end;
end;

procedure TRingModulatorDataModule.VSTModuleProcessMultiChannel(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FRingMod[Channel].ProcessSample32(Inputs[Channel, Sample]);
end;

procedure TRingModulatorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRingMod) - 1 do
  if assigned(FRingMod[Channel])
   then FRingMod[Channel].SampleRate := SampleRate;
end;

end.