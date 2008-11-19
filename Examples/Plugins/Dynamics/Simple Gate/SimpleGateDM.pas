unit SimpleGateDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSimpleGateDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SGDMThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    fSimpleGates : Array [0..1] of TClassicGate;
  public
  end;

implementation

{$R *.DFM}

uses
  EditorFrm;

procedure TSimpleGateDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSimpleGates[0] := TClassicGate.Create;
 fSimpleGates[1] := TClassicGate.Create;
end;

procedure TSimpleGateDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSimpleGates[0]);
 FreeAndNil(fSimpleGates[1]);
end;

procedure TSimpleGateDataModule.SGDMThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleGates[0].Threshold_dB := Value;
 fSimpleGates[1].Threshold_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateScrollBar;
end;

procedure TSimpleGateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fSimpleGates[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fSimpleGates[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TSimpleGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 fSimpleGates[0].SampleRate := SampleRate;
 fSimpleGates[1].SampleRate := SampleRate;
end;

end.
