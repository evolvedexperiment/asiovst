unit StkChorusDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_StkChorus;

type
  TStkChorusModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamModDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FChorus    : TStkChorus;
    FSemaphore : Integer;
  public
  end;

implementation

{$R *.DFM}

uses
  StkChorusGUI;

procedure TStkChorusModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TStkChorusModule.VSTModuleOpen(Sender: TObject);
var
  Params : TDAVSingleDynArray;
begin
 FChorus := TStkChorus.Create(SampleRate, 1);
 Parameter[0] := 500;
 Parameter[1] :=  30;
 Parameter[2] :=   0;
 SetLength(Params, numParams);
 Params[0] := Parameter[0];
 Params[1] := Parameter[1];
 Params[2] := Parameter[2];
 Programs[0].SetParameters(Params);
 Params[0] := 400;
 Params[1] :=  25;
 Params[2] :=   1;
 Programs[1].SetParameters(Params);
 Params[0] := 450;
 Params[1] :=  28;
 Params[2] :=   2;
 Programs[2].SetParameters(Params);
 Params[0] := 200;
 Params[1] :=  33;
 Params[2] :=   0;
 Programs[3].SetParameters(Params);
 Params[0] := 600;
 Params[1] :=  60;
 Params[2] :=   2;
 Programs[4].SetParameters(Params);
end;

procedure TStkChorusModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FChorus);
end;

procedure TStkChorusModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmStkChorus.Create(Self);
end;

const
  CFixMix: array [0..1] of Single = (0.2, 0.8);

procedure TStkChorusModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FChorus.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FChorus.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FChorus.LastOutputRight;
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkChorusModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FChorus.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FChorus.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FChorus.LastOutputRight;
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkChorusModule.ParamModDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 inc(FSemaphore);
 try
  FChorus.ModDepth := 0.001 * Value;
 finally
  dec(FSemaphore);
 end;
 if EditorForm is TFmStkChorus
  then TFmStkChorus(EditorForm).UpdateModDepth;
end;

procedure TStkChorusModule.ParamModFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 inc(FSemaphore);
 try
  FChorus.ModFrequency := 0.001 * Value;
 finally
  dec(FSemaphore);
 end;
 if EditorForm is TFmStkChorus
  then TFmStkChorus(EditorForm).UpdateModFreq;
end;

procedure TStkChorusModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FChorus.EffectMix := 0.01 * Value;

 if EditorForm is TFmStkChorus
  then TFmStkChorus(EditorForm).UpdateEffectMix;
end;

procedure TStkChorusModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FChorus.SampleRate := SampleRate;
end;

end.
