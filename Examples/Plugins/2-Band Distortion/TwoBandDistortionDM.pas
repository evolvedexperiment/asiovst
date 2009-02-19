unit TwoBandDistortionDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPFilterLinkwitzRiley;

type
  TTwoBandDistortionDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowDistChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHighDistChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLinkwitzRiley  : array [0..1] of TLinkwitzRiley;
    FLowMix         : array [0..1] of Single;
    FHighMix        : array [0..1] of Single;
    procedure InvertHighMix;
  public
  end;

implementation

{$R *.DFM}

uses
  TwoBandDistortionGUI, DAV_Approximations, DAV_VSTCustomModule;

procedure TTwoBandDistortionDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1
  do FLinkwitzRiley[Channel] := TLinkwitzRiley.Create;

 // Initial Parameters
 Parameter[0] := 1000;
 Parameter[1] := 2;
 Parameter[2] := 10;
 Parameter[3] := 10;
end;

procedure TTwoBandDistortionDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLinkwitzRiley[0]);
 FreeAndNil(FLinkwitzRiley[1]);
end;

procedure TTwoBandDistortionDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmTwoBandDistortion.Create(Self);
end;

procedure TTwoBandDistortionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample    : Integer;
  Channel   : Integer;
  Low, High : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to 1 do
   begin
    // using Linkwitz-Riley TwoBand filters
    FLinkwitzRiley[Channel].ProcessSample(Inputs[Channel, sample], Low, High);

    Outputs[Channel, Sample] := FLowMix[0]  * Low  + FastTanhOpt5TermFPU(FLowMix[1]  * Low) +
                                FHighMix[0] * High + FastTanhOpt5TermFPU(FHighMix[1] * High);
  end;
end;

procedure TTwoBandDistortionDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  Sample    : Integer;
  Channel   : Integer;
  Low, High : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to 1 do
   begin
    // using Linkwitz-Riley TwoBand filters
    FLinkwitzRiley[Channel].ProcessSample(Inputs[Channel, sample], Low, High);

    Outputs[Channel, Sample] := FLowMix[0]  * Low  + FastTanhOpt5TermFPU(FLowMix[1]  * Low) +
                                FHighMix[0] * High + FastTanhOpt5TermFPU(FHighMix[1] * High);
  end;
end;

procedure TTwoBandDistortionDataModule.ParamLowDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLowMix[1] := 0.01 * Value;
 FLowMix[0] := 1 - FLowMix[1];
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateLowDistortion;
end;

procedure TTwoBandDistortionDataModule.InvertHighMix;
begin
 if round(ParameterByName['Order']) mod 2 = 1 then
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

procedure TTwoBandDistortionDataModule.ParamHighDistChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FHighMix[1] := 0.01 * Value;
 FHighMix[0] := 1 - FHighMix[1];
 InvertHighMix;
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateHighDistortion;
end;

procedure TTwoBandDistortionDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FLinkwitzRiley[ch].Order := round(Value);
 InvertHighMix;
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateOrder;
end;

procedure TTwoBandDistortionDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FLinkwitzRiley[ch].Frequency := Value;
 if EditorForm is TFmTwoBandDistortion then
  with TFmTwoBandDistortion(EditorForm)
   do UpdateFrequency;
end;

end.
