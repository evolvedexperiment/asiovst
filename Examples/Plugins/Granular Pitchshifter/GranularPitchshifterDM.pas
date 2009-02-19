unit GranularPitchShifterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspGranularPitchShifter;

type
  TGranularPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParamSemitonesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamGranularityChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FGranularPitchShifter : Array [0..1] of TDspGranularPitchShifter32;
    FSemaphore            : Integer;
    function GetGranularPitchShifter(Index: Integer): TDspGranularPitchShifter32;
  public
    property GranularPitchShifter[Index: Integer]: TDspGranularPitchShifter32 read GetGranularPitchShifter;
  end;

implementation

{$R *.DFM}

uses
  GranularPitchShifterGUI, DAV_Approximations, DAV_VSTCustomModule;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TGranularPitchShifterModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 FSemaphore := 0;
 for Channel := 0 to 1 do
  begin
   FGranularPitchShifter[Channel] := TDspGranularPitchShifter32.Create;
   FGranularPitchShifter[Channel].SampleRate := SampleRate;
  end;
 Parameter[0] :=  0.01;
 Parameter[1] :=  1024 / SampleRate;
 Parameter[2] :=  2;
 Parameter[3] := 100;
(*
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
*)
end;

procedure TGranularPitchShifterModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FGranularPitchShifter[0]);
 FreeAndNil(FGranularPitchShifter[1]);
end;

procedure TGranularPitchShifterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmGranularPitchShifter.Create(Self);
end;

function TGranularPitchShifterModule.GetGranularPitchShifter(Index: Integer): TDspGranularPitchShifter32;
begin
 if Index in [0..1]
  then result := FGranularPitchShifter[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGranularPitchShifterModule.ParamSemitonesChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Semitones := Value;
  if assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Semitones := Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateSemitones;
end;

procedure TGranularPitchShifterModule.ParamStagesDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(Parameter[Index]));
end;

procedure TGranularPitchShifterModule.ParamStagesChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Stages := round(Value);
  if assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Stages := round(Value);
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateStages;
end;

procedure TGranularPitchShifterModule.ParamGranularityChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Granularity := 1E-3 * Value;
  if assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Granularity := 1E-3 * Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateGranularity;
end;

procedure TGranularPitchShifterModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
//  if assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Mix := 0.01 * Value;
//  if assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Mix := 0.01 * Value;
 finally
  Dec(FSemaphore);
 end;
(*
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateMix;
*)
end;

procedure TGranularPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FGranularPitchShifter[Channel].Process(Inputs[Channel, Sample]);
 finally
  Dec(FSemaphore);
 end;
end;

procedure TGranularPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].SampleRate := SampleRate;
  if assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].SampleRate := SampleRate;
 finally
  Dec(FSemaphore);
 end;
end;

end.
