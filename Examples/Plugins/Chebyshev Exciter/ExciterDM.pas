unit ExciterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter, DAV_DspWaveshaper;

type
  TExciterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSourceLowpassFilter    : array [0..1, 0..1] of TButterworthLowPassFilter;
    FSourceHighpassFilter   : array [0..1, 0..1] of TButterworthHighPassFilter;
    FSplitterHighpassFilter : array [0..1, 0..1] of TButterworthHighPassFilter;
    FMix                    : array [0..1] of Single;
    FOverdriveGain          : Single;
    FChebyshevWaveshaper    : TChebyshevWaveshaperSquarelShape;
    procedure InvertMix;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_VSTCustomModule, ExciterGUI, DAV_VSTModuleWithPrograms;

procedure TExciterDataModule.VSTModuleOpen(Sender: TObject);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FSourceLowpassFilter[ch, i]    := TButterworthLowPassFilter.Create;
    FSourceHighpassFilter[ch, i]   := TButterworthHighPassFilter.Create;
    FSplitterHighpassFilter[ch, i] := TButterworthHighPassFilter.Create;
   end;
 FChebyshevWaveshaper := TChebyshevWaveshaperSquarelShape.Create;

 Parameter[0] := 8000;
 Parameter[1] := 4;
 Parameter[2] := 50;
 Parameter[3] := 50;

 with Programs[0] do
  begin
   Parameter[0] := 8000;
   Parameter[1] := 4;
   Parameter[2] := 50;
   Parameter[3] := 50;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 10000;
   Parameter[1] := 2;
   Parameter[2] := 80;
   Parameter[3] := 70;
  end;
end;

procedure TExciterDataModule.VSTModuleClose(Sender: TObject);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FreeAndNil(FSourceLowpassFilter[ch, i]);
    FreeAndNil(FSourceHighpassFilter[ch, i]);
    FreeAndNil(FSplitterHighpassFilter[ch, i]);
   end;
 FreeAndNil(FChebyshevWaveshaper);
end;

procedure TExciterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmExciter.Create(Self);
end;

procedure TExciterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  sample, ch : Integer;
  Input      : Double;
  Source     : Double;
  Low, High  : Double;
const
  cDenorm = 1E-31;
begin
 for sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    Input  := cDenorm + Inputs[ch, sample];
    Low    := FSourceLowpassFilter[ch, 1].ProcessSample(
              FSourceLowpassFilter[ch, 0].ProcessSample(Input));
    Source := FChebyshevWaveshaper.ProcessSample(FOverdriveGain * Low);
    Source := FSourceHighpassFilter[ch, 1].ProcessSample(
              FSourceHighpassFilter[ch, 0].ProcessSample(Source));

    High  := FSplitterHighpassFilter[ch, 1].ProcessSample(
             FSplitterHighpassFilter[ch, 0].ProcessSample(Input));

    Outputs[ch, sample] := Low + FMix[0] * High + FMix[1] * Source;
  end;
end;

procedure TExciterDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  sample, ch : Integer;
  Input      : Double;
  Source     : Double;
  Low, High  : Double;
const
  cDenorm = 1E-31;
begin
 for sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    Input  := cDenorm + Inputs[ch, sample];
    Low    := FSourceLowpassFilter[ch, 1].ProcessSample(
              FSourceLowpassFilter[ch, 0].ProcessSample(Input));
    Source := FChebyshevWaveshaper.ProcessSample(FOverdriveGain * Low);
    Source := FSourceHighpassFilter[ch, 1].ProcessSample(
              FSourceHighpassFilter[ch, 0].ProcessSample(cDenorm + Source));

    High  := FSplitterHighpassFilter[ch, 1].ProcessSample(
             FSplitterHighpassFilter[ch, 0].ProcessSample(Input));

    Outputs[ch, sample] := Low + FMix[0] * High + FMix[1] * Source;
  end;
end;

procedure TExciterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FSourceLowpassFilter[ch, i].SampleRate    := SampleRate;
    FSourceHighpassFilter[ch, i].SampleRate   := SampleRate;
    FSplitterHighpassFilter[ch, i].SampleRate := SampleRate;
   end;
 FChebyshevWaveshaper.Order := round(min(22000, 0.48 * SampleRate) / ParameterByName['Tune'] + 0.5);
end;

procedure TExciterDataModule.InvertMix;
begin
 if round(ParameterByName['Order']) mod 2 = 1
  then FMix[0] := -abs(FMix[0])
  else FMix[0] := abs(FMix[0]);
end;

procedure TExciterDataModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;
 FMix[0] := 1 - FMix[1];
 FMix[1] := 2 * FMix[1];
 InvertMix;

 if EditorForm is TFmExciter then
  with TFmExciter(EditorForm) do
   begin
    UpdateMix;
   end;
end;

procedure TExciterDataModule.ParamShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FChebyshevWaveshaper.Shape := 2 - (0.01 * Value);
 FOverdriveGain := 1.4 - 0.4 * (0.01 * Value);

 if EditorForm is TFmExciter then
  with TFmExciter(EditorForm) do
   begin
    UpdateShape;
   end;
end;

procedure TExciterDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FSourceLowpassFilter[ch, i].Order    := round(Value);
    FSourceHighpassFilter[ch, i].Order   := round(Value);
    FSplitterHighpassFilter[ch, i].Order := round(Value);
   end;

 InvertMix;

 if EditorForm is TFmExciter then
  with TFmExciter(EditorForm) do
   begin
    UpdateOrder;
   end;
end;

procedure TExciterDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch, i : Integer;
begin
 assert(Value > 0);
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FSourceLowpassFilter[ch, i].Frequency    := Value;
    FSourceHighpassFilter[ch, i].Frequency   := Value;
    FSplitterHighpassFilter[ch, i].Frequency := Value;
   end;
 FChebyshevWaveshaper.Order := round(min(22000, 0.48 * SampleRate) / Value + 0.5);
 if EditorForm is TFmExciter then
  with TFmExciter(EditorForm) do
   begin
    UpdateTune;
   end;
end;

end.
