unit SimpleGateDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSimpleGateDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SGDMThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
  private
    FSimpleGates : Array [0..1] of TClassicGate;
  public
  end;

implementation

{$R *.DFM}

uses
  EditorFrm;

procedure TSimpleGateDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleGates[0] := TClassicGate.Create;
 FSimpleGates[1] := TClassicGate.Create;

 Parameter[0] := -10;
end;

procedure TSimpleGateDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleGates[0]);
 FreeAndNil(FSimpleGates[1]);
end;

procedure TSimpleGateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleGateDataModule.SGDMThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleGates[0]) then FSimpleGates[0].Threshold_dB := Value;
 if assigned(FSimpleGates[1]) then FSimpleGates[1].Threshold_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateScrollBar;
end;

procedure TSimpleGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleGates[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSimpleGates[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSimpleGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FSimpleGates[0]) then FSimpleGates[0].SampleRate := SampleRate;
 if assigned(FSimpleGates[1]) then FSimpleGates[1].SampleRate := SampleRate;
end;

end.
