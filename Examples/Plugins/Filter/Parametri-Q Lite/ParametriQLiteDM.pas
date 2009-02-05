unit ParametriQLiteDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilter, DAV_DspPolyphaseFilter, DAV_DspPolyphaseUpsampler,
  DAV_DspPolyphaseDownsampler;

type
  TParametriQLiteDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FFilters     : array [0..1, 0..9] of TCustomIIRFilter;
    FUpSampler   : array [0..1] of TPolyphaseUpsampler64;
    FDownSampler : array [0..1] of TPolyphaseDownSampler64;
    FPeaks       : array [0..1, 0..1] of Single;
    FGains       : array [0..1] of Single;
    procedure SetFilterClass(Index: Integer; const Value: TIIRFilterClass);
    function GetFilterClass(Index: Integer): TIIRFilterClass;
    function GetDownSampler(Index: Integer): TPolyphaseDownSampler64;
    function GetUpSampler(Index: Integer): TPolyphaseUpSampler64;
    function GetInputPeakLevel: Single;
    function GetOutputPeakLevel: Single;
    function GetFilter(Index: Integer): TCustomIIRFilter;
  public
    property FilterClass[Index: Integer]: TIIRFilterClass read GetFilterClass write SetFilterClass;
    property Filter[Index: Integer]: TCustomIIRFilter read GetFilter;
    property DownSampler[Index: Integer]: TPolyphaseDownSampler64 read GetDownSampler;
    property UpSampler[Index: Integer]: TPolyphaseUpSampler64 read GetUpSampler;
    property InputPeakLevel: Single read GetInputPeakLevel;
    property OutputPeakLevel: Single read GetOutputPeakLevel;
  end;

implementation

{$R *.DFM}

uses
  ParametriQLiteGUI, DAV_VSTModuleWithPrograms;

procedure TParametriQLiteDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel, Band : Integer;
begin
 FGains[0] := 0.01125;
 FGains[1] := 100;
 for Channel := 0 to Length(FFilters) - 1 do
  begin
   FUpSampler[Channel] := TPolyphaseUpsampler64.Create;
   FUpSampler[Channel].Transition := 0.499;
   FUpSampler[Channel].NumberOfCoefficients := 6;
   FDownSampler[Channel] := TPolyphaseDownSampler64.Create;
   FDownSampler[Channel].Transition := 0.499;
   FDownSampler[Channel].NumberOfCoefficients := 6;
   for Band := 0 to Length(FFilters[Channel]) - 1 do
    begin
     FFilters[Channel, Band] := TSimpleGainFilter.Create;
     FFilters[Channel, Band].SampleRate := SampleRate;
     FFilters[Channel, Band].Frequency := 1000;
     FFilters[Channel, Band].Gain := 0;
    end;
  end;

 // Initial Parameters
 Parameter[ 0] := 0;
 Parameter[ 1] := 60;
 Parameter[ 2] := 2;
 Parameter[ 3] := 0;
 Parameter[ 4] := 2;
 Parameter[ 5] := 180;
 Parameter[ 6] := 2;
 Parameter[ 7] := 0;
 Parameter[ 8] := 1;
 Parameter[ 9] := 350;
 Parameter[10] := 2;
 Parameter[11] := 0;
 Parameter[12] := 1;
 Parameter[13] := 700;
 Parameter[14] := 2;
 Parameter[15] := 0;
 Parameter[16] := 1;
 Parameter[17] := 1800;
 Parameter[18] := 2;
 Parameter[19] := 0;
 Parameter[20] := 1;
 Parameter[21] := 4000;
 Parameter[22] := 2;
 Parameter[23] := 0;
 Parameter[24] := 1;
 Parameter[25] := 8000;
 Parameter[26] := 2;
 Parameter[27] := 0;
 Parameter[28] := 1;
 Parameter[29] := 12000;
 Parameter[30] := 2;
 Parameter[31] := 0;
 Parameter[32] := 3;
 Parameter[33] := 0;
 SetProgramParameters(0, FParameter);
 SetProgramParameters(1, FParameter);
 SetProgramParameters(2, FParameter);
 SetProgramParameters(3, FParameter);
end;

procedure TParametriQLiteDataModule.VSTModuleClose(Sender: TObject);
var
  Channel, Band : Integer;
begin
 for Channel := 0 to Length(FFilters) - 1 do
  for Band := 0 to Length(FFilters[Channel]) - 1
   do FreeAndNil(FFilters[Channel, Band]);
end;

procedure TParametriQLiteDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmParametriQLite.Create(Self);
end;

procedure TParametriQLiteDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
  Band    : Integer;
  Temp    : TDAV2DoubleArray;
const
  CPeakRelease : Single = 0.99999;
begin
 for Channel := 0 to Length(FFilters) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    // Peak Meter (Input)
    Temp[0] := CHalf32 * (abs(Inputs[Channel, Sample]) - FPeaks[Channel, 0]);
    FPeaks[Channel, 0] := CPeakRelease * (FPeaks[Channel, 0] + Temp[0] + abs(Temp[0]));

    FUpSampler[Channel].ProcessSample(FGains[0] * Inputs[Channel, Sample] + CDenorm64, Temp);
    for Band := 0 to Length(FFilters[Channel]) - 1 do
     with FFilters[Channel, Band] do
      begin
       Temp[0] := ProcessSample(Temp[0]);
       Temp[1] := ProcessSample(Temp[1]);
      end;
    Temp[0] := FastTanhOpt5asm(Temp[0]);
    Temp[1] := FastTanhOpt5asm(Temp[1]);
    Outputs[Channel, Sample] := FGains[1] * FDownSampler[Channel].ProcessSample(Temp);

    // Peak Meter (Output)
    Temp[1] := CHalf32 * (abs(Outputs[Channel, Sample]) - FPeaks[Channel, 1]);
    FPeaks[Channel, 1] := CPeakRelease * (FPeaks[Channel, 1] + Temp[1] + abs(Temp[1]));
   end;
end;

procedure TParametriQLiteDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleDynArray;
  const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
  Band    : Integer;
  Temp    : TDAV2DoubleArray;
const
  CPeakRelease : Single = 0.99999;
begin
 for Channel := 0 to Length(FFilters) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    // Peak Meter (Input)
    Temp[0] := CHalf32 * (abs(Inputs[Channel, Sample]) - FPeaks[Channel, 0]);
    FPeaks[Channel, 0] := CPeakRelease * (FPeaks[Channel, 0] + Temp[0] + abs(Temp[0]));

    FUpSampler[Channel].ProcessSample(FGains[0] * Inputs[Channel, Sample] + CDenorm64, Temp);
    for Band := 0 to Length(FFilters[Channel]) - 1 do
     with FFilters[Channel, Band] do
      begin
       Temp[0] := ProcessSample(Temp[0]);
       Temp[1] := ProcessSample(Temp[1]);
      end;
    Temp[0] := FastTanhOpt5asm(Temp[0]);
    Temp[1] := FastTanhOpt5asm(Temp[1]);
    Outputs[Channel, Sample] := FGains[1] * FDownSampler[Channel].ProcessSample(Temp);

    // Peak Meter (Output)
    Temp[1] := CHalf32 * (abs(Outputs[Channel, Sample]) - FPeaks[Channel, 1]);
    FPeaks[Channel, 1] := CPeakRelease * (FPeaks[Channel, 1] + Temp[1] + abs(Temp[1]));
   end;
end;

procedure TParametriQLiteDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel, Band : Integer;
  Transition    : Single;
begin
 for Channel := 0 to Length(FFilters) - 1 do
  begin
   // calculate up/downsampler transition bandwidth
   if 20000 / SampleRate < 0.5
    then Transition := 20000 / SampleRate
    else Transition := 0.499;
   FUpSampler[Channel].Transition := Transition;
   FDownSampler[Channel].Transition := Transition;

   for Band := 0 to Length(FFilters[Channel]) - 1
    do FFilters[Channel, Band].SampleRate := SampleRate;
  end;
end;

procedure TParametriQLiteDataModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 Band := (Index - 1) div 4;
 FFilters[0, Band].Gain := Value;
 FFilters[1, Band].Gain := Value;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateGain(Band);
end;

procedure TParametriQLiteDataModule.ParameterInputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGains[0] := 0.01125 * dB_to_Amp(Value);
end;

procedure TParametriQLiteDataModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGains[1] := 100 * dB_to_Amp(Value);
end;

procedure TParametriQLiteDataModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 Band := (Index - 1) div 4;
 case round(Value) of
  0 : FilterClass[Band] := TSimpleGainFilter;
  1 : FilterClass[Band] := TSimplePeakFilter;
  2 : FilterClass[Band] := TSimpleLowShelfFilter;
  3 : FilterClass[Band] := TSimpleHighShelfFilter;
  4 : FilterClass[Band] := TSimpleLowpassFilter;
  5 : FilterClass[Band] := TSimpleHighpassFilter;
  6 : FilterClass[Band] := TSimpleBandpass;
  7 : FilterClass[Band] := TSimpleNotch;
  8 : FilterClass[Band] := TSimpleAllpassFilter;
 end;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateFilterType(Band);
end;

procedure TParametriQLiteDataModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Bypass';
  1 : PreDefined := 'Peak';
  2 : PreDefined := 'LoShlf';
  3 : PreDefined := 'HiShlf';
  4 : PreDefined := 'LoPass';
  5 : PreDefined := 'HiPass';
  6 : PreDefined := 'Bndpss';
  7 : PreDefined := 'Notch';
  8 : PreDefined := 'Allpass';
 end;
end;

procedure TParametriQLiteDataModule.SetFilterClass(Index: Integer;
  const Value: TIIRFilterClass);
var
  OldFilter : TCustomIIRFilter;
  Channel   : Integer;
begin
 if (Index >= 0) and (Index < Length(FFilters[0])) then
  for Channel := 0 to Length(FFilters) - 1 do
   if TIIRFilterClass(FFilters[Channel, Index].ClassType) <> Value then
    begin
     OldFilter := FFilters[Channel, Index];
     FFilters[Channel, Index] := Value.Create;
     FFilters[Channel, Index].Assign(OldFilter);
     if assigned(OldFilter) then FreeAndNil(OldFilter);
    end else
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TParametriQLiteDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 Band := (Index - 1) div 4;
 FFilters[0, Band].Frequency := 0.5 * Value;
 FFilters[1, Band].Frequency := 0.5 * Value;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateFrequency(Band);
end;

function TParametriQLiteDataModule.GetDownSampler(
  Index: Integer): TPolyphaseDownSampler64;
begin
 if (Index >= 0) and (Index < Length(FFilters))
  then result := FDownSampler[0]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TParametriQLiteDataModule.GetFilter(Index: Integer): TCustomIIRFilter;
begin
 if (Index >= 0) and (Index < Length(FFilters[0]))
  then result := FFilters[0, Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TParametriQLiteDataModule.GetFilterClass(
  Index: Integer): TIIRFilterClass;
begin
 if (Index >= 0) and (Index < Length(FFilters[0]))
  then result := TIIRFilterClass(FFilters[0, Index].ClassType)
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TParametriQLiteDataModule.GetInputPeakLevel: Single;
const
  CdBFactor : Single = 6.020487696;
begin
 if FPeaks[0, 0] > FPeaks[1, 0]
  then result := FPeaks[0, 0]
  else result := FPeaks[1, 0];
 result := CdBFactor * f_Log2Continous5(result);
end;

function TParametriQLiteDataModule.GetOutputPeakLevel: Single;
const
  CdBFactor : Single = 6.020487696;
begin
 if FPeaks[0, 1] > FPeaks[1, 1]
  then result := FPeaks[0, 1]
  else result := FPeaks[1, 1];
 result := CdBFactor * f_Log2Continous5(result);
end;

function TParametriQLiteDataModule.GetUpSampler(
  Index: Integer): TPolyphaseUpSampler64;
begin
 if (Index >= 0) and (Index < Length(FFilters))
  then result := FUpSampler[0]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TParametriQLiteDataModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band: Integer;
begin
 Band := (Index - 1) div 4;
 FFilters[0, Band].BandWidth := Value;
 FFilters[1, Band].BandWidth := Value;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateBandwidth(Band);
end;

end.