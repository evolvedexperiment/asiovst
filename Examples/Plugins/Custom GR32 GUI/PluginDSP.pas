unit PluginDSP;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFilter, DAV_DspFilterButterworth, DAV_DspDynamics,
  DAV_DspLightweightDynamics;

type
  TPluginDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLowpassFilter : array of TButterworthLowPassFilterAutomatable;
    FCompressor    : TLightweightSoftKneeLimiter;
  public

  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  PluginGUI, DAV_Common, DAV_VSTCustomModule;

procedure TPluginDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 Assert(numInputs = numOutputs);

 SetLength(FLowpassFilter, numInputs);
 for ChannelIndex := 0 to Length(FLowpassFilter) - 1 do
  begin
   FLowpassFilter[ChannelIndex] := TButterworthLowPassFilterAutomatable.Create(3);
   FLowpassFilter[ChannelIndex].SampleRate := SampleRate
  end;

 FCompressor := TLightweightSoftKneeLimiter.Create;

 // set editor form
 EditorFormClass := TFmCustomGr32Gui;

 // default parameters
 Parameter[0] := 1000;
end;

procedure TPluginDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 // free lowpass filters
 for ChannelIndex := 0 to Length(FLowpassFilter) - 1
  do FreeAndNil(FLowpassFilter[ChannelIndex]);

 FreeAndNil(FCompressor);
end;

procedure TPluginDataModule.ParameterFrequencyLabel(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'k' + PreDefined;
end;

procedure TPluginDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Threshold_dB := Value;

 // update GUI
 if EditorForm is TFmCustomGr32Gui
  then TFmCustomGr32Gui(EditorForm).ParameterUpdate(Index);
end;

procedure TPluginDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Attack := Value;

 // update GUI
 if EditorForm is TFmCustomGr32Gui
  then TFmCustomGr32Gui(EditorForm).ParameterUpdate(Index);
end;

procedure TPluginDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor)
  then FCompressor.Release := Value;

 // update GUI
 if EditorForm is TFmCustomGr32Gui
  then TFmCustomGr32Gui(EditorForm).ParameterUpdate(Index);
end;

procedure TPluginDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FLowpassFilter) - 1 do
  if Assigned(FLowpassFilter[ChannelIndex])
   then FLowpassFilter[ChannelIndex].Frequency := Value;

 // update GUI
 if EditorForm is TFmCustomGr32Gui
  then TFmCustomGr32Gui(EditorForm).ParameterUpdate(Index);
end;

procedure TPluginDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 3, 3);
end;

procedure TPluginDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to Length(FLowpassFilter) - 1 do
   begin
    if Assigned(FLowpassFilter[ChannelIndex])
     then FLowpassFilter[ChannelIndex].SampleRate := Abs(SampleRate)
   end;
end;

procedure TPluginDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
  Max, Gain    : Single;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := FLowpassFilter[0].ProcessSample64(Inputs[0, SampleIndex]);
   Max := Abs(Outputs[0, SampleIndex]);
   for ChannelIndex := 1 to Length(FLowpassFilter) - 1 do
    begin
     Outputs[ChannelIndex, SampleIndex] := FLowpassFilter[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
     if Abs(Outputs[ChannelIndex, SampleIndex]) > Max
      then Max := Abs(Outputs[ChannelIndex, SampleIndex]);
    end;

   FCompressor.InputSample(Max);
   Gain := FCompressor.GainReductionFactor;

   for ChannelIndex := 0 to Length(FLowpassFilter) - 1 do
    begin
     Outputs[ChannelIndex, SampleIndex] := Gain * Inputs[ChannelIndex, SampleIndex] +
       CHalf32 * Outputs[ChannelIndex, SampleIndex];
    end;
  end;
end;

end.
