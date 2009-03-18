unit ButterworthDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter, DAV_VstWindowSizer;

type
  TButterworthLPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter  : array [0..1] of TButterworthLowPassFilter;
  end;

implementation

{$R *.DFM}

uses
  Math, ButterworthGUI;

procedure TButterworthLPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   FFilter[ch] := TButterworthLowPassFilter.Create;
   FFilter[ch].SetFilterValues(1000, 0);
  end;
end;

procedure TButterworthLPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FreeAndNil(FFilter[ch]);
end;

procedure TButterworthLPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmButterworth.Create(Self);
end;

procedure TButterworthLPModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   if assigned(FFilter[ch])
    then FFilter[ch].Order := round(Value);
  end;
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
  if assigned(FFilter[ch])
   then FFilter[ch].Frequency := Value;
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm) do
   begin
    UpdateFrequency;
   end;
end;

procedure TButterworthLPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := FFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TButterworthLPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := FFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TButterworthLPModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FFilter[ch].SampleRate := SampleRate;
end;

end.
