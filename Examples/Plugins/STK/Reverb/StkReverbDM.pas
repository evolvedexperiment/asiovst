unit StkReverbDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_StkJCReverb, DAV_StkNReverb;

type
  TStkReverbModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParamT60Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessNetwork(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessJC(const Inputs, Outputs: TDAVArrayOfSingleDynArray;
      const SampleFrames: Integer);
  private
    FNReverb   : TStkNReverb;
    FJCReverb  : TStkJCReverb;
    FSemaphore : Integer;
  public
  end;

implementation

{$R *.DFM}

uses
  StkReverbGUI, DAV_StkReverb;

procedure TStkReverbModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TStkReverbModule.VSTModuleOpen(Sender: TObject);
begin
 FNReverb := TStkNReverb.Create;
 FJCReverb := TStkJCReverb.Create;
 Parameter[0] := 500;
 Parameter[1] := 30;
end;

procedure TStkReverbModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FNReverb);
 FreeAndNil(FJCReverb);
end;

procedure TStkReverbModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmStkReverb.Create(Self);
end;

const
  CFixMix: array [0..1] of Single = (0.2, 0.8);

procedure TStkReverbModule.VSTModuleProcessNetwork(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FNReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FNReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FNReverb.LastOutputRight;
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkReverbModule.VSTModuleProcessJC(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FJCReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FJCReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FJCReverb.LastOutputRight;
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkReverbModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FNReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FNReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FNReverb.LastOutputRight;
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TStkReverbModule.ParamT60Change(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 inc(FSemaphore);
 try
  FNReverb.T60 := 0.001 * Value;
 finally
  dec(FSemaphore);
 end;
 if EditorForm is TFmStkReverb
  then TFmStkReverb(EditorForm).UpdateT60;
end;

procedure TStkReverbModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FNReverb.EffectMix := 0.01 * Value;

 if EditorForm is TFmStkReverb
  then TFmStkReverb(EditorForm).UpdateT60;
end;

procedure TStkReverbModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FNReverb.SampleRate := SampleRate;
end;

end.