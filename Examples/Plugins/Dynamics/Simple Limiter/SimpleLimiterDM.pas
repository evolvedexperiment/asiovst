unit SimpleLimiterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DDSPBase, DVSTModule, DDynamics;

type
  TSimpleLimiterDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray;
      sampleframes: Integer);
    procedure SimpleLimiterDataModuleParameterProperties0ParameterChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure SimpleLimiterDataModuleParameterProperties1ParameterChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    fSimpleLimiters : Array [0..1] of TSimpleLimiter;
  public
  end;

implementation

{$R *.DFM}

uses
  EditorFrm;

procedure TSimpleLimiterDataModule.SimpleLimiterDataModuleParameterProperties0ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleLimiters[0].Threshold := Value;
 fSimpleLimiters[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBThreshold.Position <> Round(Value) then
    begin
     SBThreshold.Position := Round(Value);
     LbdB.Caption := IntToStr(SBThreshold.Position) + ' dB';
    end;
end;

procedure TSimpleLimiterDataModule.SimpleLimiterDataModuleParameterProperties1ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleLimiters[0].Ratio := 1 / Value;
 fSimpleLimiters[1].Ratio := 1 / Value;
end;

procedure TSimpleLimiterDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSimpleLimiters[0] := TSimpleLimiter.Create;
 fSimpleLimiters[1] := TSimpleLimiter.Create;
end;

procedure TSimpleLimiterDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSimpleLimiters[0]);
 FreeAndNil(fSimpleLimiters[1]);
end;

procedure TSimpleLimiterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleLimiterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
    Outputs[0,i] := fSimpleLimiters[0].ProcessSample(Inputs[0,i]);
    Outputs[1,i] := fSimpleLimiters[1].ProcessSample(Inputs[1,i]);
  end;
end;

end.
