unit DiracPitchShifterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DiracInterface;

type
  TDiracPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPitchFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FDirac : array [0..1] of TDiracLE;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, Dialogs, DiracPitchShifterGUI, DAV_VSTCustomModule;

procedure TDiracPitchShifterModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FDirac[ch] := TDiracLE.Create;
 Parameter[0] := 1;
end;

procedure TDiracPitchShifterModule.ParameterPitchFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1 do
  begin
   FDirac[ch].PitchFactor := Power(2, Value / 12);
//   Value := FDirac[ch].PitchFactor;
  end;
 if EditorForm is TFmDiracPitchShifter
  then TFmDiracPitchShifter(EditorForm).UpdateSemitones;
end;

procedure TDiracPitchShifterModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FreeAndNil(FDirac[ch]);
end;

procedure TDiracPitchShifterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmDiracPitchShifter.Create(Self);
end;

procedure TDiracPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FDirac[ch].ProcessBuffer(@Inputs[ch, 0], @Outputs[ch, 0], SampleFrames);
end;

procedure TDiracPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FDirac[ch].SampleRate := SampleRate;
end;

end.
