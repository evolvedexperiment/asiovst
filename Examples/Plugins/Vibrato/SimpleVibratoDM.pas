unit SimpleVibratoDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspVibrato;

type
  TSimpleVibratoModule = class(TVSTModule)
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
    function GetVibrato(Index: Integer): TDspVibrato32;
  public
    property Vibrato[Index: Integer]: TDspVibrato32 read GetVibrato;
  end;

implementation

{$R *.DFM}

uses
  SimpleVibratoGUI, DAV_Approximations, DAV_VSTCustomModule;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TSimpleVibratoModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 FSemaphore := 0;
 for Channel := 0 to 1 do
  begin
   FVibrato[Channel] := TDspVibrato32.Create;
   FVibrato[Channel].SampleRate := SampleRate;
  end;
 Parameter[0] :=  0.2;
 Parameter[1] :=  2;
 with Programs[0] do
  begin
   Parameter[0] :=  0.2;
   Parameter[1] :=  2;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  0.02;
   Parameter[1] :=  1;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  0.04;
   Parameter[1] :=  4;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  0.62;
   Parameter[1] :=  4;
  end;
 with Programs[4] do
  begin
   Parameter[0] :=  1.3;
   Parameter[1] :=  8;
  end;
 with Programs[5] do
  begin
   Parameter[0] :=  2.5;
   Parameter[1] :=  10;
  end;
 with Programs[6] do
  begin
   Parameter[0] :=  2.5;
   Parameter[1] :=  10;
  end;
 with Programs[7] do
  begin
   Parameter[0] :=  0.33;
   Parameter[1] :=  12;
  end;
end;

procedure TSimpleVibratoModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FVibrato[0]);
 FreeAndNil(FVibrato[1]);
end;

procedure TSimpleVibratoModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSimpleVibrato.Create(Self);
end;

function TSimpleVibratoModule.GetVibrato(Index: Integer): TDspVibrato32;
begin
 if Index in [0..1]
  then result := FVibrato[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TSimpleVibratoModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 while FSemaphore > 0 do Sleep(1);
 Inc(FSemaphore);
 try
  if assigned(FVibrato[0]) then FVibrato[0].Speed := Value;
  if assigned(FVibrato[1]) then FVibrato[1].Speed := Value;
 finally
  Dec(FSemaphore);
 end;
 if EditorForm is TFmSimpleVibrato then
  with TFmSimpleVibrato(EditorForm)
   do UpdateSpeed;
end;

procedure TSimpleVibratoModule.ParamDepthChange(
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
 if EditorForm is TFmSimpleVibrato then
  with TFmSimpleVibrato(EditorForm)
   do UpdateDepth;
end;

procedure TSimpleVibratoModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FVibrato[Channel].Process(Inputs[Channel, Sample])
 finally
  Dec(FSemaphore);
 end;
end;

procedure TSimpleVibratoModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel, Sample : Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhOpt5Term(FVibrato[Channel].Process(Inputs[Channel, Sample]))
 finally
  Dec(FSemaphore);
 end;
end;

procedure TSimpleVibratoModule.VSTModuleSampleRateChange(Sender: TObject;
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
