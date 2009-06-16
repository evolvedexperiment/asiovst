unit OpAmpModule;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows,{$ENDIF}
  SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule, DAV_VSTCustomModule,
  DAV_VSTParameters;

type
  { TVSTOpAmp }
  TVSTOpAmp = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: THandle);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FGain : Double;
  public
    property Gain: Double read FGain;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Approximations, OpAmpGUI;

{ TVSTOpAmp }

procedure TVSTOpAmp.VSTModuleOpen(Sender: TObject);
begin
 FGain := 1;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcessReplacing := VSTModuleProcess;
 OnParameterChange := VSTModuleParameterChange;
 {$ENDIF}

 Parameter[0] := 1;
end;

procedure TVSTOpAmp.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: THandle);
begin
 GUI := TVSTGUI.Create(Self);
 with TVSTGUI(GUI) do
  begin
   LbGain.Caption  := 'OpAmp Gain';
   SbGain.Max      := 1000;
   SbGain.Min      := 100;
   SbGain.Position := 100;
  end;
end;

procedure TVSTOpAmp.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[j, i] := FastTanhOpt5TermFPU(FGain * Inputs[j, i]);
end;

procedure TVSTOpAmp.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 for j := 0 to min(numOutputs, numInputs) - 1 do
  for i := 0 to SampleFrames - 1
   do Outputs[j, i] := FastTanhOpt5TermFPU(FGain * Inputs[j, i]);
end;

procedure TVSTOpAmp.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  i : Integer;
begin
 FGain := 2 * dB_to_Amp(Value);

 // eventually update GUI
 if FEditorForm is TVSTGUI
  then TVSTGUI(FEditorForm).UpdateGain;
end;

{$IFDEF FPC}
initialization
  {$i OpAmpModule.lrs}
{$ENDIF}

end.
