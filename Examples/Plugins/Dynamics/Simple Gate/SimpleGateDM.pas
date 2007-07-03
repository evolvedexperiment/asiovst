unit SimpleGateDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DDSPBase, DVSTModule, DDynamics;

type
  TSimpleGateDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray;
      sampleframes: Integer);
    procedure SimpleGateDataModuleParameterProperties0ParameterChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    fSimpleGates : Array [0..1] of TSimpleGate;
  public
  end;

implementation

{$R *.DFM}

uses
  EditorFrm;

procedure TSimpleGateDataModule.SimpleGateDataModuleParameterProperties0ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleGates[0].Threshold := Value;
 fSimpleGates[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if ScrollBar.Position <> Round(Value) then
    begin
     ScrollBar.Position := Round(Value);
     LbdB.Caption := IntToStr(ScrollBar.Position) + ' dB';
    end;
end;

procedure TSimpleGateDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSimpleGates[0] := TSimpleGate.Create;
 fSimpleGates[1] := TSimpleGate.Create;
end;

procedure TSimpleGateDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSimpleGates[0]);
 FreeAndNil(fSimpleGates[1]);
end;

procedure TSimpleGateDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
    Outputs[0,i] := fSimpleGates[0].ProcessSample(Inputs[0,i]);
    Outputs[1,i] := fSimpleGates[0].ProcessSample(Inputs[1,i]);
  end;
end;

end.
