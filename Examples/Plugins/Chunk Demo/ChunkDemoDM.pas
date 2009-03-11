unit ChunkDemoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilterLinkwitzRiley, DAV_DspDynamics, DAV_DspGranularPitchshifter,
  DAV_DspButterworthFilter;

const
  CNumChannels = 2;

type
  TChunkDemoDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterAlphaChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBetaChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGammaChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDeltaChange(Sender: TObject; const Index: Integer; var Value: Single);
    function VSTModuleGetChunkParameter(Sender: TObject; const Index: Integer): Single;
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPitchShifter : array [0..CNumChannels - 1] of TDspGranularPitchShifter32;
    FCrossover    : array [0..CNumChannels - 1] of TLinkwitzRiley;
    FAliasFilter  : array [0..CNumChannels - 1] of TButterworthLowPassFilter;
    FHighpass     : array [0..CNumChannels - 1] of TButterworthHighPassFilter;
    FState        : array [0..CNumChannels - 1] of Single;
    FMix          : array [0..4] of Single;
    FCompressor   : TLightweightSoftKneeCompressor;
  public
  end;

implementation

{$R *.DFM}

uses
  ChunkDemoGUI, DAV_VSTCustomModule;

procedure TChunkDemoDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 FCompressor := TLightweightSoftKneeCompressor.Create;
 FCompressor.Attack := 5;
 FCompressor.Release := 50;
 for Channel := 0 to CNumChannels - 1 do
  begin
   FPitchShifter[Channel] := TDspGranularPitchShifter32.Create;
   FCrossover[Channel]    := TLinkwitzRiley.Create;
   FAliasFilter[Channel]  := TButterworthLowPassFilter.Create;
   FHighpass[Channel]     := TButterworthHighPassFilter.Create;

   // initial settings
   FAliasFilter[Channel].Order := 3;
   FAliasFilter[Channel].Frequency := 0.38 * SampleRate;
   FHighpass[Channel].Order := 1;
   FHighpass[Channel].Frequency := 20;
   FCrossover[Channel].Order := 1;
   FPitchShifter[Channel].Semitones := 5;
   FPitchShifter[Channel].Granularity := 0.15;
   FPitchShifter[Channel].Stages := 2;
  end;

 Parameter[0] := 50;
 Parameter[1] := 50;
 Parameter[2] := 50;
 Parameter[3] := 50;

 with Programs[0] do
  begin
   Parameter[0] := 50;
   Parameter[1] := 50;
   Parameter[2] := 50;
   Parameter[3] := 50;
  end;
 with Programs[0] do
  begin
   Parameter[0] := 40;
   Parameter[1] := 60;
   Parameter[2] := 40;
   Parameter[3] := 60;
  end;
end;

procedure TChunkDemoDataModule.ParameterBetaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressor.Ratio        := 2 + sqr(0.01 * Value) * 98;
 FCompressor.Knee_dB      := 0.1 * Value;
 FCompressor.Threshold_dB := -20 + 0.1 * Value;

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateBeta;
end;

procedure TChunkDemoDataModule.ParameterGammaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[2] := 0.01 * Value;
 FMix[3] := 1 - FMix[2];
 FCompressor.MakeUpGain_dB := 0.2 * Value;

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateDelta;
end;

procedure TChunkDemoDataModule.ParameterDeltaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.1 + 0.009 * Value;
 FMix[1] := 1 - FMix[0];

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateGamma;
end;

procedure TChunkDemoDataModule.ParameterAlphaChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNumChannels - 1 do
  begin
   FCrossover[Channel].Frequency := sqr(sqr(0.01 * Value)) * 20000;
  end;

 Chunk.Position := Index * SizeOf(Single);
 Chunk.Write(Value, SizeOf(Single));

 if EditorForm is TFmChunkDemo then
  with TFmChunkDemo(EditorForm)
   do UpdateAlpha;
end;

procedure TChunkDemoDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 FreeAndNil(FCompressor);
 for Channel := 0 to CNumChannels - 1 do
  begin
   FreeAndNil(FPitchShifter[Channel]);
   FreeAndNil(FCrossover[Channel]);
   FreeAndNil(FAliasFilter[Channel]);
  end;
end;

procedure TChunkDemoDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmChunkDemo.Create(Self);
end;

function TChunkDemoDataModule.VSTModuleGetChunkParameter(Sender: TObject;
  const Index: Integer): Single;
begin
 Chunk.Position := Index * SizeOf(Single);
 Chunk.Read(result, SizeOf(Single));
end;

procedure TChunkDemoDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //
end;

procedure TChunkDemoDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample    : Integer;
  Low, High : array [0..1] of Single;
begin
 assert(CNumChannels = 2);
 for Sample := 0 to SampleFrames - 1 do
  begin
   FCrossover[0].ProcessSample(Inputs[0, Sample], Low[0], High[0]);
   FCrossover[1].ProcessSample(Inputs[1, Sample], Low[1], High[1]);
   FState[0] := FPitchShifter[0].Process(FAliasFilter[0].ProcessSample(FState[0] + High[0]));
   FState[1] := FPitchShifter[1].Process(FAliasFilter[1].ProcessSample(FState[1] + High[1]));

   Outputs[0, Sample] := Low[0] + FMix[0] * High[0] + FMix[1] * FState[1];
   Outputs[1, Sample] := Low[1] + FMix[0] * High[1] + FMix[1] * FState[0];

   // compress delayed signal
   FCompressor.InputSample(FMix[2] * (Inputs[0, Sample] + Outputs[1, Sample]) +
                           FMix[3] * (FState[1] + FState[0]));
   FState[0] := FHighpass[0].ProcessSample(FCompressor.GainSample(FState[0]));
   FState[1] := FHighpass[1].ProcessSample(FCompressor.GainSample(FState[1]));
  end;
end;

procedure TChunkDemoDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCompressor.SampleRate := SampleRate;
 for Channel := 0 to CNumChannels - 1 do
  begin
   FPitchShifter[Channel].SampleRate := SampleRate;
   FCrossover[Channel].SampleRate := SampleRate;
   FAliasFilter[Channel].SampleRate := SampleRate;
   FAliasFilter[Channel].Frequency := 0.38 * SampleRate;
  end;
end;

end.