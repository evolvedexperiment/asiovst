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
    FLowpassFilter  : array [0..1, 0..1] of TButterworthLP;
    FHighpassFilter : array [0..1, 0..1] of TButterworthHP;
    FLowMix         : array [0..1] of Single;
    FHighMix        : array [0..1] of Single;
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
    FLowpassFilter[ch, i]  := TButterworthLP.Create;
//    FLowpassFilter[ch, i].Order := 4;
    FHighpassFilter[ch, i] := TButterworthHP.Create;
//    FHighpassFilter[ch, i].Order := 4;
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
    Low  := FLowpassFilter[ch, 1].ProcessSample(
            FLowpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));
    High := FHighpassFilter[ch, 1].ProcessSample(
            FHighpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));

    Outputs[ch, sample] := FLowMix[0]  * Low  + FastTanhOpt5(FLowMix[1]  * Low) +
                           FHighMix[0] * High + FastTanhOpt5(FHighMix[1] * High);
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
    Low  := FLowpassFilter[ch, 1].ProcessSample(
            FLowpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));
    High := FHighpassFilter[ch, 1].ProcessSample(
            FHighpassFilter[ch, 0].ProcessSample(Inputs[ch, sample]));

    Outputs[ch, sample] := FLowMix[0]  * Low  + FLowMix[1]  * FastTanhOpt5(Low) +
                           FHighMix[0] * High + FHighMix[1] * FastTanhOpt5(High);
  end;
end;

procedure TCrossoverDistortionDataModule.ParamLowDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLowMix[1] := 0.01 * Value;
 FLowMix[0] := 1 - FLowMix[1];
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
   FHighMix[0] := -abs(FHighMix[0]);
   FHighMix[1] := -abs(FHighMix[1]);
  end
 else
  begin
   FHighMix[0] := abs(FHighMix[0]);
   FHighMix[1] := abs(FHighMix[1]);
  end;
end;

procedure TCrossoverDistortionDataModule.ParamHighDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHighMix[1] := 0.01 * Value;
 FHighMix[0] := 1 - FHighMix[1];
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
    FLowpassFilter[ch, i].Order  := round(Value);
    FHighpassFilter[ch, i].Order := round(Value);
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
    FLowpassFilter[ch, i].Frequency := Value;
    FHighpassFilter[ch, i].Frequency := Value;
   end;
 if EditorForm is TFmCrossoverDistortion then
  with TFmCrossoverDistortion(EditorForm) do
   begin
    UpdateFrequency;
   end;
end;

procedure TCrossoverDistortionDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLowpassFilter[0]);
 FreeAndNil(FLowpassFilter[1]);
 FreeAndNil(FHighpassFilter[0]);
 FreeAndNil(FHighpassFilter[1]);
end;

procedure TCrossoverDistortionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmCrossoverDistortion.Create(Self);
end;

end.
