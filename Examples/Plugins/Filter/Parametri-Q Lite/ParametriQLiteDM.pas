unit ParametriQLiteDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilter, DAV_DspPolyphaseFilter, DAV_DspPolyphaseUpsampler,
  DAV_DspPolyphaseDownsampler;

type
  TParametriQLiteDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
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
    FGains       : array [0..1] of Single;
    function GetFilterClass(Index: Integer): TIIRFilterClass;
    procedure SetFilterClass(Index: Integer; const Value: TIIRFilterClass);
    function GetDownSampler(Index: Integer): TPolyphaseDownSampler64;
    function GetUpSampler(Index: Integer): TPolyphaseUpSampler64;
  public
    property FilterClass[Index: Integer]: TIIRFilterClass read GetFilterClass write SetFilterClass;
    property DownSampler[Index: Integer]: TPolyphaseDownSampler64 read GetDownSampler;
    property UpSampler[Index: Integer]: TPolyphaseUpSampler64 read GetUpSampler;
  end;

implementation

{$R *.DFM}

uses
  ParametriQLiteGUI;

procedure TParametriQLiteDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmParametriQLite.Create(Self);
end;

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
   FUpSampler[Channel].NumberOfCoefficients := 4;
   FDownSampler[Channel] := TPolyphaseDownSampler64.Create;
   FDownSampler[Channel].Transition := 0.499;
   FDownSampler[Channel].NumberOfCoefficients := 4;
   for Band := 0 to Length(FFilters[Channel]) - 1 do
    begin
     FFilters[Channel, Band] := TSimpleGainFilter.Create;
     FFilters[Channel, Band].SampleRate := SampleRate;
     FFilters[Channel, Band].Frequency := 1000;
     FFilters[Channel, Band].Gain := 0;
    end;
  end;
end;

procedure TParametriQLiteDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
  Band    : Integer;
  Temp    : TDAV2DoubleArray;
begin
 for Channel := 0 to Length(FFilters) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
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
begin
 for Channel := 0 to Length(FFilters) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    FUpSampler[0].ProcessSample(FGains[0] * Inputs[Channel, Sample], Temp);
    for Band := 0 to Length(FFilters[Channel]) - 1 do
     with FFilters[Channel, Band] do
      begin
       Temp[0] := ProcessSample(Temp[0]);
       Temp[1] := ProcessSample(Temp[1]);
      end;
    Temp[0] := FastTanhOpt5asm(Temp[0]);
    Temp[1] := FastTanhOpt5asm(Temp[1]);
    Outputs[Channel, Sample] := FGains[1] * FDownSampler[Channel].ProcessSample(Temp);
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
begin
 FFilters[0, (Index - 1) div 4].Gain := Value;
 FFilters[1, (Index - 1) div 4].Gain := Value;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateGain(Index);
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
begin
 case round(Value) of
  0 : FilterClass[(Index - 1) div 4] := TSimpleGainFilter;
  1 : FilterClass[(Index - 1) div 4] := TSimplePeakFilter;
  2 : FilterClass[(Index - 1) div 4] := TSimpleLowShelfFilter;
  3 : FilterClass[(Index - 1) div 4] := TSimpleHighShelfFilter;
  4 : FilterClass[(Index - 1) div 4] := TSimpleLowpassFilter;
  5 : FilterClass[(Index - 1) div 4] := TSimpleHighpassFilter;
  6 : FilterClass[(Index - 1) div 4] := TSimpleBandpass;
  7 : FilterClass[(Index - 1) div 4] := TSimpleNotch;
  8 : FilterClass[(Index - 1) div 4] := TSimpleAllpassFilter;
 end;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateFilterType(Index);
end;

procedure TParametriQLiteDataModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Bypass';
  1 : PreDefined := 'Peak';
  2 : PreDefined := 'LowShelf';
  3 : PreDefined := 'HighShelf';
  4 : PreDefined := 'Lowpass';
  5 : PreDefined := 'Highpass';
  6 : PreDefined := 'Bandpass';
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
begin
 FFilters[0, (Index - 1) div 4].Frequency := 0.5 * Value;
 FFilters[1, (Index - 1) div 4].Frequency := 0.5 * Value;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateFrequency(Index);
end;

function TParametriQLiteDataModule.GetDownSampler(
  Index: Integer): TPolyphaseDownSampler64;
begin
 if (Index >= 0) and (Index < Length(FFilters))
  then result := FDownSampler[0]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TParametriQLiteDataModule.GetFilterClass(
  Index: Integer): TIIRFilterClass;
begin
 if (Index >= 0) and (Index < Length(FFilters[0]))
  then result := TIIRFilterClass(FFilters[0, Index].ClassType)
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
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
begin
 FFilters[0, (Index - 1) div 4].BandWidth := Value;
 FFilters[1, (Index - 1) div 4].BandWidth := Value;
 if EditorForm is TFmParametriQLite
  then TFmParametriQLite(EditorForm).UpdateBandwidth(Index);
end;

procedure TParametriQLiteDataModule.VSTModuleClose(Sender: TObject);
var
  Channel, Band : Integer;
begin
 for Channel := 0 to Length(FFilters) - 1 do
  for Band := 0 to Length(FFilters[Channel]) - 1
   do FreeAndNil(FFilters[Channel, Band]);
end;

end.