unit SimpleChorusDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspChorus;

type
  TSimpleChorusModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure ParamDriftChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FChorus    : Array [0..1] of TDspChorus32;
    FSemaphore : Integer;
    function GetChorus(Index: Integer): TDspChorus32;
  public
    property Chorus[Index: Integer]: TDspChorus32 read GetChorus;
  end;

implementation

{$R *.DFM}

uses
  SimpleChorusGUI, DAV_VSTCustomModule;

procedure TSimpleChorusModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 FSemaphore := 0;
 for Channel := 0 to 1 do
  begin
   FChorus[Channel] := TDspChorus32.Create;
   FChorus[Channel].SampleRate := SampleRate;
  end;
 Parameter[0] :=  0.2;
 Parameter[1] :=  4;
 Parameter[2] :=  5;
 Parameter[3] := 50;
 Parameter[4] :=  8;
end;

procedure TSimpleChorusModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FChorus[0]);
 FreeAndNil(FChorus[1]);
end;

procedure TSimpleChorusModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSimpleChorus.Create(Self);
end;

function TSimpleChorusModule.GetChorus(Index: Integer): TDspChorus32;
begin
 if Index in [0..1]
  then result := FChorus[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TSimpleChorusModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FChorus[0]) then FChorus[0].Speed := Value;
  if assigned(FChorus[1]) then FChorus[1].Speed := Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleChorus then
  with TFmSimpleChorus(EditorForm)
   do UpdateSpeed;
end;

procedure TSimpleChorusModule.ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FChorus[0]) then FChorus[0].Stages := round(Value);
  if assigned(FChorus[1]) then FChorus[1].Stages := round(Value);
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleChorus then
  with TFmSimpleChorus(EditorForm)
   do UpdateStages;
end;

procedure TSimpleChorusModule.ParamDriftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FChorus[0]) then FChorus[0].Drift := 0.01 * Value;
  if assigned(FChorus[1]) then FChorus[1].Drift := 0.01 * Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleChorus then
  with TFmSimpleChorus(EditorForm)
   do UpdateDrift;
end;

procedure TSimpleChorusModule.ParamDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FChorus[0]) then FChorus[0].Depth := 0.01 * Value;
  if assigned(FChorus[1]) then FChorus[1].Depth := 0.01 * Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleChorus then
  with TFmSimpleChorus(EditorForm)
   do UpdateDepth;
end;

procedure TSimpleChorusModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FChorus[0]) then FChorus[0].Mix := 0.01 * Value;
  if assigned(FChorus[1]) then FChorus[1].Mix := 0.01 * Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleChorus then
  with TFmSimpleChorus(EditorForm)
   do UpdateMix;
end;

procedure TSimpleChorusModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhOpt5asm(FChorus[Channel].Process(Inputs[Channel, Sample]))
 finally
  Dec(FSemaphore);
 end;
end;

procedure TSimpleChorusModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FChorus[0]) then FChorus[0].SampleRate := SampleRate;
  if assigned(FChorus[1]) then FChorus[1].SampleRate := SampleRate;
 finally
  Dec(FSemaphore);
 end;
end;

end.
