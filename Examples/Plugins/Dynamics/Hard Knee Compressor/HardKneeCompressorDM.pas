unit HardKneeCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspDynamics;

type
  THardKneeCompressorDataModule = class(TVSTModule)
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
    FSimpleCompressors : Array [0..1] of TSimpleCompressor;
    function GetCompressor(Index: Integer): TSimpleCompressor;
  public
    property Compressor[Index: Integer]: TSimpleCompressor read GetCompressor;
  end;

implementation

{$R *.DFM}

uses
  Math, HardKneeCompressorGUI;

procedure THardKneeCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleCompressors[0] := TSimpleCompressor.Create;
 FSimpleCompressors[1] := TSimpleCompressor.Create;
end;

procedure THardKneeCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleCompressors[0]);
 FreeAndNil(FSimpleCompressors[1]);
end;

procedure THardKneeCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmHardKneeCompressor.Create(Self);
end;

procedure THardKneeCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Threshold_dB := Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateThreshold;
end;

procedure THardKneeCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Ratio := 1 / Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateRatio;
end;

procedure THardKneeCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Release := Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateRelease;
end;

procedure THardKneeCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 // update compressor parameter on all channels
 for ch := 0 to 1 do
  if assigned(FSimpleCompressors[ch])
   then FSimpleCompressors[ch].Attack := Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateAttack;
end;

function THardKneeCompressorDataModule.GetCompressor(
  Index: Integer): TSimpleCompressor;
begin
 if Index in [0..1]
  then result := FSimpleCompressors[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure THardKneeCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleCompressors[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSimpleCompressors[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure THardKneeCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 FSimpleCompressors[0].SampleRate := SampleRate;
 FSimpleCompressors[1].SampleRate := SampleRate;
end;

end.
