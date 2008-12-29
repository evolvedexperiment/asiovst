unit SimpleCompressor2DM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSoftKneeCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSimpleCompressors : Array [0..1] of TSoftKneeCompressor;
    function GetCompressor(Index: Integer): TSoftKneeCompressor;
  public
    property Compressor[Index: Integer]: TSoftKneeCompressor read GetCompressor;
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TSoftKneeCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleCompressors[0] := TSoftKneeCompressor.Create;
 FSimpleCompressors[1] := TSoftKneeCompressor.Create;
end;

procedure TSoftKneeCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleCompressors[0]);
 FreeAndNil(FSimpleCompressors[1]);
end;

procedure TSoftKneeCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TEditorForm.Create(Self);
end;

procedure TSoftKneeCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Threshold_dB := Value;

 // update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSoftKneeCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Ratio := 1 / Value;

 // update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRatio;
end;

procedure TSoftKneeCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Release := Value;

 // update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSoftKneeCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Attack := Value;

 // update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

function TSoftKneeCompressorDataModule.GetCompressor(
  Index: Integer): TSoftKneeCompressor;
begin
 if Index in [0..1]
  then result := FSimpleCompressors[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TSoftKneeCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleCompressors[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := FSimpleCompressors[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TSoftKneeCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 FSimpleCompressors[0].SampleRate := SampleRate;
 FSimpleCompressors[1].SampleRate := SampleRate;
end;

end.
