unit FrequencyDomainPitchShifterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFrequencyDomainPitchshifter;

type
  TFrequencyDomainPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPitchFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPitchShifter : array [0..1] of TFrequencyDomainPitchShifter32;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, Dialogs, FrequencyDomainPitchShifterGUI, DAV_VSTCustomModule;

procedure TFrequencyDomainPitchShifterModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FPitchShifter[ch] := TFrequencyDomainPitchShifter32.Create;
 Parameter[0] := 1;
end;

procedure TFrequencyDomainPitchShifterModule.ParameterPitchFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FPitchShifter[ch].Pitch := Power(2, Value / 12);
 if EditorForm is TFmFrequencyDomainPitchShifter
  then TFmFrequencyDomainPitchShifter(EditorForm).UpdateSemitones;
end;

procedure TFrequencyDomainPitchShifterModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FreeAndNil(FPitchShifter[ch]);
end;

procedure TFrequencyDomainPitchShifterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmFrequencyDomainPitchShifter.Create(Self);
end;

procedure TFrequencyDomainPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FPitchShifter[ch].ProcessSamples(@Inputs[ch, 0], @Outputs[ch, 0], SampleFrames);
end;

procedure TFrequencyDomainPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FPitchShifter[ch].SampleRate := SampleRate;
end;

end.
