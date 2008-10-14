unit ButterworthDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter, DAV_VstWindowSizer;

type
  TButterworthLPModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fFilter  : array [0..1] of TButterworthLP;
  end;

implementation

{$R *.DFM}

uses
  Math, ButterworthGUI;

procedure TButterworthLPModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(fFilter[ch]) then fFilter[ch].Order := max(2, 2 * round(0.5 * Value));
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm) do
   begin
    UpdateOrder;
   end;
end;

procedure TButterworthLPModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(fFilter[ch]) then fFilter[ch].Frequency := Value;
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm) do
   begin
    UpdateFrequency;
   end;
end;

procedure TButterworthLPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   fFilter[ch] := TButterworthLP.Create;
   fFilter[ch].SetFilterValues(1000, 0);
  end;
end;

procedure TButterworthLPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmButterworth.Create(Self);
end;

procedure TButterworthLPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FreeAndNil(fFilter[ch]);
end;

procedure TButterworthLPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TButterworthLPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TButterworthLPModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do fFilter[ch].SampleRate := SampleRate;
end;

end.
