unit SimpleCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSimpleCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    FSimpleCompressors : Array [0..1] of TSimpleCompressor;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm;

procedure TSimpleCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleCompressors[0] := TSimpleRMSCompressor.Create;
 FSimpleCompressors[1] := TSimpleRMSCompressor.Create;
end;

procedure TSimpleCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleCompressors[0]);
 FreeAndNil(FSimpleCompressors[1]);
end;

procedure TSimpleCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Threshold_dB := Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSimpleCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Ratio := 1 / Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Ratio := 1 / Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRatio;
end;

procedure TSimpleCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Release := Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Release := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSimpleCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].Attack := Value;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].Attack := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSimpleCompressorDataModule.VSTModuleProcess(const Inputs,
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

procedure TSimpleCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FSimpleCompressors[0])
  then FSimpleCompressors[0].SampleRate := SampleRate;
 if assigned(FSimpleCompressors[1])
  then FSimpleCompressors[1].SampleRate := SampleRate;
end;

end.
