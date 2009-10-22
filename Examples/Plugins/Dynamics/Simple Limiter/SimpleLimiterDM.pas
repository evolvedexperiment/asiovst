unit SimpleLimiterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSimpleLimiterDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SGDMThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSimpleLimiters : Array [0..1] of TLimiter;
  public
  end;

implementation

{$R *.DFM}

uses
  EditorFrm;

procedure TSimpleLimiterDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleLimiters[0] := TLimiter.Create;
 FSimpleLimiters[1] := TLimiter.Create;

 Parameter[0] := -10;
end;

procedure TSimpleLimiterDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleLimiters[0]);
 FreeAndNil(FSimpleLimiters[1]);
end;

procedure TSimpleLimiterDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleLimiterDataModule.SGDMThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].Threshold_dB := Value;
 if assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].Threshold_dB := Value;

 // eventually update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSimpleLimiterDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].Release := Value;
 if assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].Release := Value;

 // eventually update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSimpleLimiterDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].Attack := Value;
 if assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].Attack := Value;

 // eventually update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSimpleLimiterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleLimiters[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := FSimpleLimiters[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TSimpleLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].SampleRate := SampleRate;
 if assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].SampleRate := SampleRate;
end;

end.
