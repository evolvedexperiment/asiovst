unit ExciterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter, DAV_DspWaveshaper;

type
  TExciterDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    fSourceLowpassFilter    : array [0..1, 0..1] of TButterworthLP;
    fSourceHighpassFilter   : array [0..1, 0..1] of TButterworthHP;
    fSplitterHighpassFilter : array [0..1, 0..1] of TButterworthHP;
    fMix                    : array [0..1] of Single;
    fOverdriveGain          : Single;
    fChebyshevWaveshaper    : TChebyshevWaveshaperSquarelShape;
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
    fSourceLowpassFilter[ch, i]    := TButterworthLP.Create;
    fSourceHighpassFilter[ch, i]   := TButterworthHP.Create;
    fSplitterHighpassFilter[ch, i] := TButterworthHP.Create;
   end;
 fChebyshevWaveshaper := TChebyshevWaveshaperSquarelShape.Create;

 Parameter[0] := 4000;
 Parameter[1] := 4;
 Parameter[2] := 50;
 Parameter[3] := 50;
end;

procedure TExciterDataModule.VSTModuleClose(Sender: TObject);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    FreeAndNil(fSourceLowpassFilter[ch, i]);
    FreeAndNil(fSourceHighpassFilter[ch, i]);
    FreeAndNil(fSplitterHighpassFilter[ch, i]);
   end;
 FreeAndNil(fChebyshevWaveshaper);
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
    Low    := fSourceLowpassFilter[ch, 1].ProcessSample(
              fSourceLowpassFilter[ch, 0].ProcessSample(Input));
    Source := fChebyshevWaveshaper.ProcessSample(fOverdriveGain * Low);
    Source := fSourceHighpassFilter[ch, 1].ProcessSample(
              fSourceHighpassFilter[ch, 0].ProcessSample(Source));

    High  := fSplitterHighpassFilter[ch, 1].ProcessSample(
             fSplitterHighpassFilter[ch, 0].ProcessSample(Input));

    Outputs[ch, sample] := Low + fMix[0] * High + fMix[1] * Source;
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
    Low    := fSourceLowpassFilter[ch, 1].ProcessSample(
              fSourceLowpassFilter[ch, 0].ProcessSample(Input));
    Source := fChebyshevWaveshaper.ProcessSample(fOverdriveGain * Low);
    Source := fSourceHighpassFilter[ch, 1].ProcessSample(
              fSourceHighpassFilter[ch, 0].ProcessSample(cDenorm + Source));

    High  := fSplitterHighpassFilter[ch, 1].ProcessSample(
             fSplitterHighpassFilter[ch, 0].ProcessSample(Input));

    Outputs[ch, sample] := Low + fMix[0] * High + fMix[1] * Source;
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
    fSourceLowpassFilter[ch, i].SampleRate    := SampleRate;
    fSourceHighpassFilter[ch, i].SampleRate   := SampleRate;
    fSplitterHighpassFilter[ch, i].SampleRate := SampleRate;
   end;
 fChebyshevWaveshaper.Order := round(min(22000, 0.48 * SampleRate) / ParameterByName['Tune'] + 0.5);
end;

procedure TExciterDataModule.InvertMix;
begin
 if round(ParameterByName['Order']) mod 2 = 1
  then fMix[0] := -abs(fMix[0])
  else fMix[0] := abs(fMix[0]);
end;

procedure TExciterDataModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fMix[1] := 0.01 * Value;
 fMix[0] := 1 - fMix[1];
 fMix[1] := 2 * fMix[1];
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
 fChebyshevWaveshaper.Shape := 2 - (0.01 * Value);
 fOverdriveGain := 1.4 - 0.4 * (0.01 * Value);

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
    fSourceLowpassFilter[ch, i].Order    := round(Value);
    fSourceHighpassFilter[ch, i].Order   := round(Value);
    fSplitterHighpassFilter[ch, i].Order := round(Value);
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
    fSourceLowpassFilter[ch, i].Frequency    := Value;
    fSourceHighpassFilter[ch, i].Frequency   := Value;
    fSplitterHighpassFilter[ch, i].Frequency := Value;
   end;
 fChebyshevWaveshaper.Order := round(min(22000, 0.48 * SampleRate) / Value + 0.5);
 if EditorForm is TFmExciter then
  with TFmExciter(EditorForm) do
   begin
    UpdateTune;
   end;
end;

end.
