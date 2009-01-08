unit BarberpoleFlangerDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspBarberpole;

type
  TBarberpoleFlangerModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterAlgorithmDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterAlgorithmChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FBarberpole   : Array [0..1] of TDspBarberpole32;
    FSemaphore : Integer;
    function GetBarberpole(Index: Integer): TDspBarberpole32;
  public
    property Barberpole[Index: Integer]: TDspBarberpole32 read GetBarberpole;
  end;

implementation

{$R *.DFM}

uses
  BarberpoleFlangerGUI, DAV_VSTCustomModule;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TBarberpoleFlangerModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 FSemaphore := 0;
 for Channel := 0 to 1 do
  begin
   FBarberpole[Channel] := TDspBarberpole32.Create;
   FBarberpole[Channel].SampleRate := SampleRate;
  end;
 Parameter[0] :=  2;
 Parameter[1] :=  0.2;
 Parameter[2] :=  5;
 Parameter[3] := 50;
 with Programs[0] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.2;
   Parameter[2] :=  5;
   Parameter[3] := 50;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.02;
   Parameter[2] :=  2;
   Parameter[3] := 50;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.04;
   Parameter[2] :=  4;
   Parameter[3] := 50;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.62;
   Parameter[2] :=  4.5;
   Parameter[3] := 50;
  end;
end;

procedure TBarberpoleFlangerModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FBarberpole[0]);
 FreeAndNil(FBarberpole[1]);
end;

procedure TBarberpoleFlangerModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmBarberpoleFlanger.Create(Self);
end;

function TBarberpoleFlangerModule.GetBarberpole(Index: Integer): TDspBarberpole32;
begin
 if Index in [0..1]
  then result := FBarberpole[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarberpoleFlangerModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FBarberpole[0]) then FBarberpole[0].Speed := Value;
  if assigned(FBarberpole[1]) then FBarberpole[1].Speed := Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateSpeed;
end;

procedure TBarberpoleFlangerModule.ParamStagesDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(Parameter[Index]));
end;

procedure TBarberpoleFlangerModule.ParameterAlgorithmChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FBarberpole[0]) then FBarberpole[0].Direction := TBarberpoleDirection(round(Value));
  if assigned(FBarberpole[1]) then FBarberpole[1].Direction := TBarberpoleDirection(round(Value));
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateAlgorithm;
end;

procedure TBarberpoleFlangerModule.ParameterAlgorithmDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Up';
  1 : PreDefined := 'Down';
  2 : PreDefined := 'Up (Inv.)';
  3 : PreDefined := 'Down (Inv.)';
 end;
end;

procedure TBarberpoleFlangerModule.ParamStagesChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FBarberpole[0]) then FBarberpole[0].Stages := round(Value);
  if assigned(FBarberpole[1]) then FBarberpole[1].Stages := round(Value);
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateStages;
end;

procedure TBarberpoleFlangerModule.ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FBarberpole[0]) then FBarberpole[0].Depth := 0.01 * Value;
  if assigned(FBarberpole[1]) then FBarberpole[1].Depth := 0.01 * Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateDepth;
end;

procedure TBarberpoleFlangerModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FBarberpole[0]) then FBarberpole[0].Mix := 0.01 * Value;
  if assigned(FBarberpole[1]) then FBarberpole[1].Mix := 0.01 * Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateMix;
end;

procedure TBarberpoleFlangerModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhOpt5asm(FBarberpole[Channel].Process(Inputs[Channel, Sample]))
 finally
  Dec(FSemaphore);
 end;
end;

procedure TBarberpoleFlangerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FBarberpole[0]) then FBarberpole[0].SampleRate := SampleRate;
  if assigned(FBarberpole[1]) then FBarberpole[1].SampleRate := SampleRate;
 finally
  Dec(FSemaphore);
 end;
end;

end.
