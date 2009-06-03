unit VariableDelayModule;

interface

uses
  Windows, Types, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_DspVariableDelay;

type
  TVariableDelayVST = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDryMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FCriticalSection : TCriticalSection;
    FVariDelay       : array [0..1] of TCustomVariableDelay32;
    FMix             : array [0..1] of Single;
  end;

implementation

{$R *.DFM}

uses
  VariableDelayGUI, DAV_VSTCustomModule;

procedure TVariableDelayVST.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TVariableDelayVST.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TVariableDelayVST.VSTModuleOpen(Sender: TObject);
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
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FVariDelay) - 1 do
   if assigned(FVariDelay[Channel])
    then FVariDelay[Channel].Delay := 1E-4 * Value;
 finally
   FCriticalSection.Leave;
 end;

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
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   for Channel := 0 to Length(FVariDelay) - 1
    do Outputs[Channel, Sample] := FMix[0] * Inputs[Channel, Sample] +
         FMix[1] * FVariDelay[Channel].ProcessSample(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVariableDelayVST.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   for Channel := 0 to Length(FVariDelay) - 1
    do Outputs[Channel, Sample] := FMix[0] * Inputs[Channel, Sample] +
         FMix[1] * FVariDelay[Channel].ProcessSample(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVariableDelayVST.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FVariDelay) - 1 do
   if assigned(FVariDelay[Channel])
    then FVariDelay[Channel].SampleRate := abs(SampleRate);
 finally
   FCriticalSection.Leave;
 end;
end;

end.
