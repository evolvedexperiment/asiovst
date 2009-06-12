unit RenaissanceBassCloneDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_DspPsychoacousticBassEnhancer;

type
  TResurrectionBassCloneModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParameterIntensityChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterdBDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterAddOriginalBassDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterAddOriginalBassChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FRenaissanceBass : array [0..1] of TResurrectionBass;
    FCriticalSection : TCriticalSection;
  public
  end;

implementation

uses
  Math, DAV_Approximations, RenaissanceBassCloneGUI;

{$R *.DFM}

procedure TResurrectionBassCloneModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TResurrectionBassCloneModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TResurrectionBassCloneModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 // create & setup upward compressor
 for Channel := 0 to Length(FRenaissanceBass) - 1 do
  begin
   FRenaissanceBass[Channel] := TResurrectionBass.Create;
   FRenaissanceBass[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 80;
 Parameter[1] := 1;
 Parameter[2] := 20;
 Parameter[3] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[4] do
  begin
   Parameter[0] := 75;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
end;

procedure TResurrectionBassCloneModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRenaissanceBass) - 1
  do FreeAndNil(FRenaissanceBass[Channel]);
end;

procedure TResurrectionBassCloneModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmRenaissanceBassClone.Create(Self);
end;

procedure TResurrectionBassCloneModule.ParameterAddOriginalBassDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TResurrectionBassCloneModule.ParameterdBDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] = 0
  then PreDefined := '-oo'
  else PreDefined := FloatToStrF(RoundTo(Amp_to_dB(Parameter[Index]), -2), ffGeneral, 3, 3);
end;

procedure TResurrectionBassCloneModule.ParameterAddOriginalBassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].AddOriginalBass := Boolean(round(Value));
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.ParameterIntensityChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].Intensity := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].Gain := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].Frequency := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].SampleRate := SampleRate;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel   : Integer;
  Sample    : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FRenaissanceBass[Channel].Process(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel   : Integer;
  Sample    : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FRenaissanceBass[Channel].Process(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

end.
