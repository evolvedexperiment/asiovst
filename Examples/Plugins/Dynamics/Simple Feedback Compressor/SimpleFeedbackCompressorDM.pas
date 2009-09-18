unit SimpleFeedbackCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSimpleFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSimpleFeedbackCompressors : Array [0..1] of TCustomFeedbackCompressor;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to Length(FSimpleFeedbackCompressors) - 1 do
  begin
   FSimpleFeedbackCompressors[i] := TSimpleFeedbackCompressor.Create;
   FSimpleFeedbackCompressors[i].AutoMakeUp := True;
  end;

 // initial parameters 
 Parameter[0] := 0;
 Parameter[1] := 1;
 Parameter[2] := 5;
 Parameter[3] := 40;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleFeedbackCompressors[0]);
 FreeAndNil(FSimpleFeedbackCompressors[1]);
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleFeedbackCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[0].Threshold_dB := Value;
 if assigned(FSimpleFeedbackCompressors[1])
  then FSimpleFeedbackCompressors[1].Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if DialThreshold.Position <> Round(Value) then
    begin
     DialThreshold.Position := Round(Value);
     UpdateThreshold;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleFeedbackCompressors[0]) then
  begin
   FSimpleFeedbackCompressors[0].Ratio := 1 / Value;
   FSimpleFeedbackCompressors[1].Ratio := FSimpleFeedbackCompressors[0].Ratio;
  end;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if DialRatio.Position <> Round(100 * Log10(Value)) then
    begin
     DialRatio.Position := Round(100 * Log10(Value));
     UpdateRatio;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[0].Release := Value;
 if assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[1].Release := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if DialRelease.Position <> Round(Value) then
    begin
     DialRelease.Position := Round(1000 * Log10(Value));
     UpdateRelease;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  SampleDuration_ms : Single;
begin
 SampleDuration_ms := 1000 / SampleRate;
 if Value < 3 * SampleDuration_ms
  then Value := 3 * SampleDuration_ms;

 if assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[0].Attack := Value;
 if assigned(FSimpleFeedbackCompressors[1])
  then FSimpleFeedbackCompressors[1].Attack := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleFeedbackCompressors[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSimpleFeedbackCompressors[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
var
  SampleDuration_ms : Single;
begin
 if assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[0].SampleRate := SampleRate;
 if assigned(FSimpleFeedbackCompressors[1])
  then FSimpleFeedbackCompressors[1].SampleRate := SampleRate;

 SampleDuration_ms := 1000 / SampleRate;
 if Parameter[3] < 3 * SampleDuration_ms
  then Parameter[3] := 3 * SampleDuration_ms;
end;

end.
