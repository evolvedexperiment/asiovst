unit OpAmpModule;

{$I ASIOVST.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows,{$ENDIF}
  SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TVSTOpAmp = class(TVSTModule)
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure VSTModuleProcessDoubleReplacing(const inputs, outputs: TDAVArrayOfDoubleDynArray; const sampleframes: Integer);
    procedure VSTModuleInitialize(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
    fGain   : Double;
  public
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, OpAmpGUI;

procedure TVSTOpAmp.VSTModuleProcess(const inputs,
  outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[j, i] := Tanh2a(fGain * Inputs[j, i]);
end;

procedure TVSTOpAmp.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[j, i] := Tanh2a(fGain * Inputs[j, i]);
end;

procedure TVSTOpAmp.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TVSTOpAmp.VSTModuleInitialize(Sender: TObject);
begin
 Parameter[0] := 1;
end;

procedure TVSTOpAmp.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var i : Integer;
begin
 fGain := 2 * dB_to_Amp(Value);
 if Assigned(fEditorForm) then
  with fEditorForm As TVSTGUI do
   begin
    i := Round(10 * Value);
    if SBGain.Position <> i
     then SBGain.Position := i;
   end;   
end;

{$IFDEF FPC}
initialization
  {$i OpAmpModule.lrs}
{$ENDIF}

end.
