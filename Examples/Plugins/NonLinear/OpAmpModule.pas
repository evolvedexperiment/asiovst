unit OpAmpModule;

{$IFDEF FPC}
 {$MODE DELPHI}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

interface

uses {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows,{$ENDIF}
     Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule;

type
  TVSTOpAmp = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleProcess(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure VSTModuleProcessDoubleReplacing(const inputs, outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
    procedure VSTModuleInitialize(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fGain   : Double;
  public
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses Math, OpAmpGUI;

procedure TVSTOpAmp.VST_EditOpen(Sender: TObject; var GUI: TForm);
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TVSTOpAmp.VSTModuleProcess(const inputs,
  outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i, j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to sampleframes - 1
   do Outputs[j, i] := Tanh2a(fGain * Inputs[j, i]);
end;

procedure TVSTOpAmp.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
var i,j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to sampleframes - 1
   do Outputs[j, i] := Tanh2a(fGain * Inputs[j, i]);
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
