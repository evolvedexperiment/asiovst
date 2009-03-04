unit ButterworthDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter, DAV_VstWindowSizer;

type
  TButterworthHPModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter  : array [0..1] of TButterworthHP;
  end;

implementation

{$R *.DFM}

uses
  Math, ButterworthGUI;

procedure TButterworthHPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   FFilter[ch] := TButterworthHP.Create;
   FFilter[ch].SetFilterValues(20, 0);
  end;
 Parameter[0] := 20;
 Parameter[1] := 2;
end;

procedure TButterworthHPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FreeAndNil(FFilter[ch]);
end;

procedure TButterworthHPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmButterworth.Create(Self);
end;

procedure TButterworthHPModule.ParamOrderChange(
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

procedure TButterworthHPModule.ParamFrequencyChange(
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

procedure TButterworthHPModule.VSTModuleProcess(const Inputs,
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

procedure TButterworthHPModule.VSTModuleProcessDoubleReplacing(const Inputs,
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

procedure TButterworthHPModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FFilter[ch].SampleRate := SampleRate;
end;

end.
