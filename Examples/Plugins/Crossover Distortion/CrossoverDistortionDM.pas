unit CrossoverDistortionDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter;

type
  TCrossoverDistortionDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowDistChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHighDistChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fLowpassFilter  : array [0..1, 0..1] of TButterworthLP;
    fHighpassFilter : array [0..1, 0..1] of TButterworthHP;
    fLowMix         : array [0..1] of Single;
    fHighMix        : array [0..1] of Single;
    procedure InvertHighMix;
  public
  end;

implementation

{$R *.DFM}

uses
  CrossoverDistortionGUI, DAV_VSTCustomModule;

procedure TCrossoverDistortionDataModule.VSTModuleOpen(Sender: TObject);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    fLowpassFilter[ch, i]  := TButterworthLP.Create;
//    fLowpassFilter[ch, i].Order := 4;
    fHighpassFilter[ch, i] := TButterworthHP.Create;
//    fHighpassFilter[ch, i].Order := 4;
   end;
end;

procedure TCrossoverDistortionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  sample, ch : Integer;
  Low, High  : Double;
begin
 for sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    // using Linkwitz-Riley crossover filters
    Low  := fLowpassFilter[ch, 1].ProcessSample(
            fLowpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));
    High := fHighpassFilter[ch, 1].ProcessSample(
            fHighpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));

    Outputs[ch, sample] := fLowMix[0]  * Low  + FastTanhOpt5(fLowMix[1]  * Low) +
                           fHighMix[0] * High + FastTanhOpt5(fHighMix[1] * High);
  end;
end;

procedure TCrossoverDistortionDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  sample, ch : Integer;
  Low, High  : Double;
begin
 for sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    // using Linkwitz-Riley crossover filters
    Low  := fLowpassFilter[ch, 1].ProcessSample(
            fLowpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));
    High := fHighpassFilter[ch, 1].ProcessSample(
            fHighpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));

    Outputs[ch, sample] := fLowMix[0]  * Low  + fLowMix[1]  * FastTanhOpt5(Low) +
                           fHighMix[0] * High + fHighMix[1] * FastTanhOpt5(High);
  end;
end;

procedure TCrossoverDistortionDataModule.ParamLowDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLowMix[1] := 0.01 * Value;
 fLowMix[0] := 1 - fLowMix[1];
 if EditorForm is TFmCrossoverDistortion then
  with TFmCrossoverDistortion(EditorForm) do
   begin
    UpdateLowDistortion;
   end;
end;

procedure TCrossoverDistortionDataModule.InvertHighMix;
begin
 if round(ParameterByName['Crossover Order']) mod 2 = 1 then
  begin
   fHighMix[0] := -abs(fHighMix[0]);
   fHighMix[1] := -abs(fHighMix[1]);
  end
 else
  begin
   fHighMix[0] := abs(fHighMix[0]);
   fHighMix[1] := abs(fHighMix[1]);
  end;
end;

procedure TCrossoverDistortionDataModule.ParamHighDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fHighMix[1] := 0.01 * Value;
 fHighMix[0] := 1 - fHighMix[1];
 InvertHighMix;
 if EditorForm is TFmCrossoverDistortion then
  with TFmCrossoverDistortion(EditorForm) do
   begin
    UpdateHighDistortion;
   end;
end;

procedure TCrossoverDistortionDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    fLowpassFilter[ch, i].Order  := round(Value);
    fHighpassFilter[ch, i].Order := round(Value);
   end;
 InvertHighMix;  
 if EditorForm is TFmCrossoverDistortion then
  with TFmCrossoverDistortion(EditorForm) do
   begin
    UpdateOrder;
   end;
end;

procedure TCrossoverDistortionDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch, i : Integer;
begin
 for ch := 0 to numInputs - 1 do
  for i := 0 to 1 do
   begin
    fLowpassFilter[ch, i].Frequency := Value;
    fHighpassFilter[ch, i].Frequency := Value;
   end;
 if EditorForm is TFmCrossoverDistortion then
  with TFmCrossoverDistortion(EditorForm) do
   begin
    UpdateFrequency;
   end;
end;

procedure TCrossoverDistortionDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(fLowpassFilter[0]);
 FreeAndNil(fLowpassFilter[1]);
 FreeAndNil(fHighpassFilter[0]);
 FreeAndNil(fHighpassFilter[1]);
end;

procedure TCrossoverDistortionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmCrossoverDistortion.Create(Self);
end;

end.
