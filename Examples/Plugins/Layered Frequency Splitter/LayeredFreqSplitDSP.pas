unit LayeredFreqSplitDSP;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAV_Types, DAV_VSTModule, DAV_DspFilterLinkwitzRiley;

type
  TLayeredFreqSplitModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure ParameterFrequencyChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure ParameterLayersChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterIntegerDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOrderChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FLinkwitzRileyFilters : array [0..1, 0..9] of TLinkwitzRiley;
    FLayers               : Integer;
  public

  end;

implementation

{$R *.DFM}

uses
  LayeredFreqSplitGUI;

procedure TLayeredFreqSplitModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
  LayerIndex   : Integer;
begin
 EditorFormClass := TFmLayeredFreqSplit;
 for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
  for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1 do
   begin
    FLinkwitzRileyFilters[ChannelIndex, LayerIndex] := TLinkwitzRiley.Create;
    with FLinkwitzRileyFilters[ChannelIndex, LayerIndex] do
     begin
      SampleRate := Self.SampleRate;
      Frequency := 1000;
     end;
   end;

 Parameter[0] := 1000;
 Parameter[1] := 4;
 Parameter[2] := 1;
end;

procedure TLayeredFreqSplitModule.ParameterLayersChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLayers := Round(Value) - 1;
end;

procedure TLayeredFreqSplitModule.ParameterIntegerDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TLayeredFreqSplitModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := IntToStr(2 * Round(Parameter[Index]));
end;

procedure TLayeredFreqSplitModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  LayerIndex   : Integer;
begin
 for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
  for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1
   do FLinkwitzRileyFilters[ChannelIndex, LayerIndex].Order := Round(Value);
end;

procedure TLayeredFreqSplitModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  LayerIndex   : Integer;
begin
 for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
  for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1
   do FLinkwitzRileyFilters[ChannelIndex, LayerIndex].Frequency := Value;
end;

procedure TLayeredFreqSplitModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  LayerIndex   : Integer;
  Data         : Double;
  Low, High    : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
   begin
    Data := Inputs[ChannelIndex, SampleIndex];
    for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1 do
     begin
      FLinkwitzRileyFilters[ChannelIndex, LayerIndex].ProcessSample(Data, Low, High);
      Data := Low + High;
      if LayerIndex = FLayers
       then Outputs[ChannelIndex, SampleIndex] := Data;
     end;
   end;
end;

end.
