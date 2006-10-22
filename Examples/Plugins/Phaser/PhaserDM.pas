unit PhaserDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule, DPhaser;

type
  TPhaserModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VST2ModuleCreate(Sender: TObject);
    procedure VST2ModuleProcess(const inputs, outputs: TArrayOfSingleArray; sampleframes: Integer);
    procedure PhaserModuleParameterProperties0ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PhaserModuleParameterProperties1ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PhaserModuleParameterProperties2ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PhaserModuleParameterProperties3ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PhaserModuleParameterProperties4ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure PhaserModuleParameterProperties5ParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fPhaser : array [0..1] of TPhaser;
  public
  end;

implementation

{$R *.DFM}

uses
  PhaserFrm;

procedure TPhaserModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TPhaserForm.Create(nil);
  (GUI As TPhaserForm).PhaserModule := Self;
end;

procedure TPhaserModule.PhaserModuleParameterProperties0ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(fPhaser[0]) and Assigned(fPhaser[1]) then
  begin
   fPhaser[0].Depth:=0.01*Value;
   fPhaser[1].Depth:=fPhaser[0].Depth;
  end;
end;

procedure TPhaserModule.PhaserModuleParameterProperties1ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(fPhaser[0]) and Assigned(fPhaser[1]) then
  begin
   fPhaser[0].Feedback:=Value;
   fPhaser[1].Feedback:=fPhaser[0].Feedback;
  end;
end;

procedure TPhaserModule.PhaserModuleParameterProperties2ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(fPhaser[0]) and Assigned(fPhaser[1]) then
  begin
   fPhaser[0].Minimum:=Value;
   fPhaser[1].Minimum:=fPhaser[0].Minimum;
  end;
end;

procedure TPhaserModule.PhaserModuleParameterProperties3ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(fPhaser[0]) and Assigned(fPhaser[1]) then
  begin
   fPhaser[0].Maximum:=Value;
   fPhaser[1].Maximum:=fPhaser[0].Maximum;
  end;
end;

procedure TPhaserModule.PhaserModuleParameterProperties4ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(fPhaser[0]) and Assigned(fPhaser[1]) then
  begin
   fPhaser[0].Rate:=Value;
   fPhaser[1].Rate:=fPhaser[0].Rate;
  end;
end;

procedure TPhaserModule.PhaserModuleParameterProperties5ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(fPhaser[0]) and Assigned(fPhaser[1]) then
  begin
   fPhaser[0].Stages:=round(Value);
   fPhaser[1].Stages:=round(Value);
  end;
end;

procedure TPhaserModule.VST2ModuleCreate(Sender: TObject);
begin
 fPhaser[0]:=TPhaser.Create;
 fPhaser[1]:=TPhaser.Create;
end;

procedure TPhaserModule.VST2ModuleProcess(const inputs, outputs: TArrayOfSingleArray; sampleframes: Integer);
var i: Integer;
begin
 for i:=0 to sampleframes-1 do
  begin
   outputs[0,i]:=fPhaser[0].Process(inputs[0,i]);
   outputs[1,i]:=fPhaser[0].Process(inputs[1,i]);
  end;
end;

end.