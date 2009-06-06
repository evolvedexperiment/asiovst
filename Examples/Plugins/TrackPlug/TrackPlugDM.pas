unit TrackPlugDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_DspFilter,
  DAV_DspButterworthFilter, DAV_DspFilterBasics, DAV_DspDynamics,
  DAV_DspLightweightDynamics,
  DAV_VSTModule;

type
  TTrackPlugModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterDCFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDcFilterChangeOrder(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEqTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterEqFilterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEqFilterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEqTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FDCFilter   : array [0..1] of TButterworthHighPassFilter;
    FEqFilter   : array [0..1, 0..4] of TCustomBandwidthIIRFilter;
    FGate       : array [0..1] of TCustomKneeCompressor;
    FCompressor : array [0..1] of TCustomKneeCompressor;
    function GetFilter(Index: Integer): TCustomIIRFilter;
    function GetFilterClass(Index: Integer): TBandwidthIIRFilterClass;
    procedure SetFilterClass(Index: Integer;
      const Value: TBandwidthIIRFilterClass);
  public
    property FilterClass[Index: Integer]: TBandwidthIIRFilterClass read GetFilterClass write SetFilterClass;
    property Filter[Index: Integer]: TCustomIIRFilter read GetFilter;
  end;

implementation

{$R *.DFM}

uses
  Math, TrackPlugGUI;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TTrackPlugModule.VSTModuleOpen(Sender: TObject);
var
  Channel, Band : Integer;
begin
 // create DC filters
 for Channel := 0 to Length(FDCFilter) - 1 do
  begin
   FDCFilter[Channel] := TButterworthHighPassFilter.Create(1);
   FDCFilter[Channel].SampleRate := SampleRate;
  end;

 // create EQ filters
 for Channel := 0 to Length(FEqFilter) - 1 do
  for Band := 0 to Length(FEqFilter[Channel]) - 1 do
   begin
    FEqFilter[Channel, Band] := TBasicPeakFilter.Create;
    FEqFilter[Channel, Band].SampleRate := SampleRate;
   end;

 // create Gate
 for Channel := 0 to Length(FGate) - 1 do
  begin
   FGate[Channel] := TLightweightSoftKneeCompressor.Create;
   FGate[Channel].SampleRate := SampleRate;
  end;

 // create compressor
 for Channel := 0 to Length(FCompressor) - 1 do
  begin
   FCompressor[Channel] := TLightweightSoftKneeCompressor.Create;
   FCompressor[Channel].SampleRate := SampleRate;
  end;

 // DC filter
 Parameter[ 0] := 5;
 Parameter[ 1] := 1;

 // EQ filter 1
 Parameter[ 2] := 100;
 Parameter[ 3] := 0;
 Parameter[ 4] := 1;
 Parameter[ 5] := 2;

 // EQ filter 2
 Parameter[ 6] := 315;
 Parameter[ 7] := 0;
 Parameter[ 8] := 1;
 Parameter[ 9] := 3;

 // EQ filter 3
 Parameter[10] := 1000;
 Parameter[11] := 0;
 Parameter[12] := 1;
 Parameter[13] := 3;

 // EQ filter 4
 Parameter[14] := 3150;
 Parameter[15] := 0;
 Parameter[16] := 1;
 Parameter[17] := 3;

 // EQ filter 5
 Parameter[18] := 10000;
 Parameter[19] := 0;
 Parameter[20] := 1;
 Parameter[21] := 6;

 // gate
 Parameter[22] := 1.5;
 Parameter[23] := 7.5;
 Parameter[24] := -10;
 Parameter[25] := 0.2;
 Parameter[26] := 6;

 // compressor
 Parameter[27] := 15;
 Parameter[28] := 75;
 Parameter[29] := -10;
 Parameter[30] := 5;
 Parameter[31] := 2;
 Parameter[32] := 6;
 Parameter[33] := 0;
 Parameter[34] := 100;
end;

procedure TTrackPlugModule.VSTModuleClose(Sender: TObject);
var
  Channel, Band : Integer;
begin
 // free DC filters
 for Channel := 0 to Length(FDCFilter) - 1
  do FreeAndNil(FDCFilter[Channel]);

 // free EQ filters
 for Channel := 0 to Length(FEqFilter) - 1 do
  for Band := 0 to Length(FEqFilter[Channel]) - 1
   do FreeAndNil(FEqFilter[Channel, Band]);

 for Channel := 0 to Length(FGate) - 1
  do FreeAndNil(FGate[Channel])

 // free compressor
 for Channel := 0 to Length(FCompressor) - 1
  do FreeAndNil(FCompressor[Channel])
end;

procedure TTrackPlugModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmTrackPlug.Create(Self);
end;

function TTrackPlugModule.GetFilter(Index: Integer): TCustomIIRFilter;
begin
 if (Index >= 0) and (Index < Length(FEqFilter[0]))
  then result := FEqFilter[0, Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TTrackPlugModule.GetFilterClass(
  Index: Integer): TBandwidthIIRFilterClass;
begin
 if (Index >= 0) and (Index < Length(FEqFilter[0]))
  then result := TBandwidthIIRFilterClass(FEqFilter[0, Index].ClassType)
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TTrackPlugModule.ParameterDCFilterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDCFilter) - 1 do
  if assigned(FDCFilter[Channel])
   then FDCFilter[Channel].Frequency := Value;
end;

procedure TTrackPlugModule.ParameterDcFilterChangeOrder(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDCFilter) - 1 do
  if assigned(FDCFilter[Channel])
   then FDCFilter[Channel].Order := round(Value);
end;

procedure TTrackPlugModule.ParameterEqTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : Predefined := 'Bypass';
  1 : Predefined := 'Lowcut';
  2 : Predefined := 'Lowshelf';
  3 : Predefined := 'Peak';
  4 : Predefined := 'Bandpass';
  5 : Predefined := 'Notch';
  6 : Predefined := 'Highshelf';
  7 : Predefined := 'Highcut';
 end;
end;

procedure TTrackPlugModule.ParameterEqFilterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, Band : Integer;
begin
 Band := (Index - 2) div 4;

 for Channel := 0 to Length(FEqFilter) - 1 do
  if assigned(FEqFilter[Channel, Band])
   then FEqFilter[Channel, Band].Frequency := Value;
end;

procedure TTrackPlugModule.ParameterEqFilterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, Band : Integer;
begin
 Band := (Index - 2) div 4;

 for Channel := 0 to Length(FEqFilter) - 1 do
  if assigned(FEqFilter[Channel, Band])
   then FEqFilter[Channel, Band].Gain := Value;
end;

procedure TTrackPlugModule.ParameterFilterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, Band : Integer;
begin
 Band := (Index - 2) div 4;

 for Channel := 0 to Length(FEqFilter) - 1 do
  if assigned(FEqFilter[Channel, Band])
   then FEqFilter[Channel, Band].Bandwidth := Value;
end;

procedure TTrackPlugModule.SetFilterClass(Index: Integer;
  const Value: TBandwidthIIRFilterClass);
var
  OldFilter : TCustomIIRFilter;
  Channel   : Integer;
begin
 if (Index >= 0) and (Index < Length(FEqFilter[0])) then
  for Channel := 0 to Length(FEqFilter) - 1 do
   if assigned(FEqFilter[Channel, Index]) then
    if TBandwidthIIRFilterClass(FEqFilter[Channel, Index].ClassType) <> Value then
     begin
      OldFilter := FEqFilter[Channel, Index];
      FEqFilter[Channel, Index] := Value.Create;
      FEqFilter[Channel, Index].Assign(OldFilter);
      if assigned(OldFilter) then FreeAndNil(OldFilter);
     end else
   else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TTrackPlugModule.ParameterEqTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 2) div 4;

 if assigned(FEqFilter[0, Band]) then
  case round(Value) of
    0 : FilterClass[Band] := TBasicGainFilter;
    1 : FilterClass[Band] := TBasicLowcutFilter;
    2 : FilterClass[Band] := TBasicLowShelfFilter;
    3 : FilterClass[Band] := TBasicPeakFilter;
    4 : FilterClass[Band] := TBasicBandpassFilter;
    5 : FilterClass[Band] := TBasicNotchFilter;
    6 : FilterClass[Band] := TBasicHighShelfFilter;
    7 : FilterClass[Band] := TBasicHighpassFilter;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].Attack := Value;
   if assigned(FCompressor[1])
    then FCompressor[1].Attack := FCompressor[0].Attack;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].Release := Value;
   if assigned(FCompressor[1])
    then FCompressor[1].Release := FCompressor[0].Release;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].Threshold_dB := Value;
   if assigned(FCompressor[1])
    then FCompressor[1].Threshold_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].Ratio := Value;
   if assigned(FCompressor[1])
    then FCompressor[1].Ratio := Value;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].Knee_dB := Value;
   if assigned(FCompressor[1])
    then FCompressor[1].Knee_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].AutoMakeUp := Boolean(round(Value));
   FCompressor[1].AutoMakeUp := FCompressor[0].AutoMakeUp;
  end;
end;

procedure TTrackPlugModule.ParameterCompressorMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TTrackPlugModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FGate[0]) then
  begin
   FGate[0].Attack := Value;
   if assigned(FGate[1])
    then FGate[1].Attack := FGate[0].Attack;
  end;
end;

procedure TTrackPlugModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FGate[0]) then
  begin
   FGate[0].Release := Value;
   if assigned(FGate[1])
    then FGate[1].Release := FGate[0].Release;
  end;
end;

procedure TTrackPlugModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FGate[0]) then
  begin
   FGate[0].Threshold_dB := Value;
   if assigned(FGate[1])
    then FGate[1].Threshold_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FGate[0]) then
  begin
   FGate[0].Ratio := Value;
   if assigned(FGate[1])
    then FGate[1].Ratio := Value;
  end;
end;

procedure TTrackPlugModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FGate[0]) then
  begin
   FGate[0].Knee_dB := Value;
   if assigned(FGate[1])
    then FGate[1].Knee_dB := Value;
  end;
end;

procedure TTrackPlugModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := 'µs' else
 if Val >= 1000
  then PreDefined := 's';
end;

procedure TTrackPlugModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := FloatToStrF(RoundTo(1E3 * Val, -2), ffGeneral, 3, 3) else
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

procedure TTrackPlugModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TTrackPlugModule.ParameterCompressorMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].MakeUpGain_dB := Value;
   FCompressor[1].MakeUpGain_dB := FCompressor[0].MakeUpGain_dB;
  end;
end;

procedure TTrackPlugModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TTrackPlugModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TTrackPlugModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TTrackPlugModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TTrackPlugModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel, Band : Integer;
begin
 // DC filters
 for Channel := 0 to Length(FDCFilter) - 1 do
  if assigned(FDCFilter[Channel])
   then FDCFilter[Channel].SampleRate := SampleRate;

 // EQ filters
 for Channel := 0 to Length(FEqFilter) - 1 do
  for Band := 0 to Length(FEqFilter[Channel]) - 1 do
   if assigned(FEqFilter[Channel, Band])
    then FEqFilter[Channel, Band].SampleRate := SampleRate;

 // compressor
 for Channel := 0 to Length(FCompressor) - 1 do
  if assigned(FCompressor[Channel])
   then FCompressor[Channel].SampleRate := SampleRate;
end;

procedure TTrackPlugModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
  CurrentSample   : Double;
  Band            : Integer;
begin
 for Channel := 0 to Length(FDCFilter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    // DC filter first
    CurrentSample := FDCFilter[Channel].ProcessSample(Inputs[Channel, Sample]);

    // process EQ filters
    for Band := 0 to Length(FEqFilter) - 1
     do CurrentSample := FEqFilter[Channel, Band].ProcessSample(CurrentSample);

    CurrentSample := FGate[Channel].ProcessSample(CurrentSample);

    CurrentSample := FCompressor[Channel].ProcessSample(CurrentSample);

    Outputs[Channel, Sample] := CurrentSample;
   end;
end;

end.
