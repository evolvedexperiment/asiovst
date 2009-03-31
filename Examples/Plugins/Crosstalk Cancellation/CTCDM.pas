unit CTCDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspCrosstalkCancellation;

type
  TCTCDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParamSpeakerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDistanceDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamListenerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRecursionStepsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamAttenuationChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParamDistanceLabel(Sender: TObject; const Index: Integer;
      var PreDefined: string);
  private
    FSemaphore : Integer;
    FCrosstalkCancellation: TCrosstalkCancellation32;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, CTCGui;

procedure TCTCDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TCTCDataModule.VSTModuleOpen(Sender: TObject);
begin
 FCrosstalkCancellation := TCrosstalkCancellation32.Create;

 Parameter[0] := 100;
 Parameter[1] := 100;
 Parameter[2] := 3;
 Parameter[3] := -1;
end;

procedure TCTCDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCrosstalkCancellation);
end;

procedure TCTCDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmCTC.Create(Self);
end;

procedure TCTCDataModule.ParamRecursionStepsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  if assigned(FCrosstalkCancellation)
   then FCrosstalkCancellation.StageCount := round(Value);
 finally
  dec(FSemaphore);
 end;
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateRecursionSteps;
end;

procedure TCTCDataModule.ParamDistanceDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 100
  then PreDefined := FloatToStrF(RoundTo(0.01 * Parameter[Index], -2),
                       ffGeneral, 3, 3);
end;

procedure TCTCDataModule.ParamDistanceLabel(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
  if Parameter[Index] >= 100
  then PreDefined := 'm'
  else PreDefined := 'cm'
end;

procedure TCTCDataModule.ParamAttenuationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.Attenuation := dB_to_Amp(Value);
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateAttenuation;
end;

procedure TCTCDataModule.ParamListenerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.ListenerDistance := Value;
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateListenerDistance;
end;

procedure TCTCDataModule.ParamSpeakerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.SpeakerDistance := Value;
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateSpeakerDistance;
end;

procedure TCTCDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Cardinal;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FCrosstalkCancellation.ProcessStereo(Inputs[0, Sample], Inputs[1, Sample],
      Outputs[0, Sample], Outputs[1, Sample]);
   end;
 finally
  Dec(FSemaphore);
 end;
end;

procedure TCTCDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Cardinal;
  Data   : array [0..1] of Single;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FCrosstalkCancellation.ProcessStereo(Inputs[0, Sample], Inputs[1, Sample],
      Data[0], Data[1]);
    Outputs[0, Sample] := Data[0];
    Outputs[1, Sample] := Data[1];
   end;
 finally
  Dec(FSemaphore);
 end;
end;

procedure TCTCDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.SampleRate := SampleRate;
end;

end.
