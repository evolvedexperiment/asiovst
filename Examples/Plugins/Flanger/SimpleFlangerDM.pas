unit SimpleFlangerDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspVibrato;

type
  TSimpleFlangerModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FVibrato    : Array [0..1] of TDspVibrato32;
    FSemaphore : Integer;
    function GetFlanger(Index: Integer): TDspVibrato32;
  public
    property Flanger[Index: Integer]: TDspVibrato32 read GetFlanger;
  end;

implementation

{$R *.DFM}

uses
  SimpleFlangerGUI, DAV_VSTCustomModule, DAV_Approximations;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TSimpleFlangerModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 FSemaphore := 0;
 for Channel := 0 to 1 do
  begin
   FVibrato[Channel] := TDspVibrato32.Create;
   FVibrato[Channel].SampleRate := SampleRate;
  end;
 Parameter[0] :=  5;
 Parameter[1] :=  0.2;
 Parameter[2] := 50;
 with Programs[0] do
  begin
   Parameter[0] :=  5;
   Parameter[1] :=  0.2;
   Parameter[2] := 50;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.02;
   Parameter[2] := 50;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  4;
   Parameter[1] :=  0.04;
   Parameter[2] := 50;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  4.5;
   Parameter[1] :=  0.62;
   Parameter[2] := 50;
  end;
end;

procedure TSimpleFlangerModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FVibrato[0]);
 FreeAndNil(FVibrato[1]);
end;

procedure TSimpleFlangerModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSimpleFlanger.Create(Self);
end;

function TSimpleFlangerModule.GetFlanger(Index: Integer): TDspVibrato32;
begin
 if Index in [0..1]
  then result := FVibrato[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TSimpleFlangerModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FVibrato[0]) then FVibrato[0].Speed := Value;
  if assigned(FVibrato[1]) then FVibrato[1].Speed := Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleFlanger then
  with TFmSimpleFlanger(EditorForm)
   do UpdateSpeed;
end;

procedure TSimpleFlangerModule.ParamDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FVibrato[0]) then FVibrato[0].Depth := 0.01 * Value;
  if assigned(FVibrato[1]) then FVibrato[1].Depth := 0.01 * Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleFlanger then
  with TFmSimpleFlanger(EditorForm)
   do UpdateDepth;
end;

procedure TSimpleFlangerModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhOpt5TermFPU(Inputs[Channel, Sample] + FVibrato[Channel].Process(Inputs[Channel, Sample]))
 finally
  Dec(FSemaphore);
 end;
end;

procedure TSimpleFlangerModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhOpt5TermFPU(FVibrato[Channel].Process(Inputs[Channel, Sample]))
 finally
  Dec(FSemaphore);
 end;
end;

procedure TSimpleFlangerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FVibrato[0]) then FVibrato[0].SampleRate := SampleRate;
  if assigned(FVibrato[1]) then FVibrato[1].SampleRate := SampleRate;
 finally
  Dec(FSemaphore);
 end;
end;

end.
