unit MaxxBassCloneDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_DspPsychoAcousticBassEnhancer;

type
  TMaxxBassCloneModule = class(TVSTModule)
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassSelectChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOriginalBassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMaxxbassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterListenChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterResponseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParameterInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterdBDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterListenDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterHighpassDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FMaxxBass        : array [0..1] of TCustomMaxxBass;
    FCriticalSection : TCriticalSection;
    procedure CalculateGains;
  public
  end;

implementation

uses
  Math, DAV_Approximations, MaxxBassCloneGUI;

{$R *.DFM}

procedure TMaxxBassCloneModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 // create & setup upward compressor
 for Channel := 0 to Length(FMaxxBass) - 1 do
  begin
   FMaxxBass[Channel] := TCustomMaxxBass.Create;
   FMaxxBass[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 80;
 Parameter[1] := 1;
 Parameter[2] := 20;
 Parameter[3] := 0;
 Parameter[4] := -15;
 Parameter[5] := 1;
 Parameter[6] := 1;
 Parameter[7] := 0;
 Parameter[8] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 1;
   Parameter[2] := 20;
   Parameter[3] := 0;
   Parameter[4] := -15;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := 0;
   Parameter[8] := 0;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 1.5;
   Parameter[2] := 15;
   Parameter[3] := 1;
   Parameter[4] := -15;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := dB_to_Amp(-10);
   Parameter[8] := 0;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 1.5;
   Parameter[2] := 15;
   Parameter[3] := 0;
   Parameter[4] := -15;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := dB_to_Amp(-12);
   Parameter[8] := 0;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 1.8;
   Parameter[2] := 15;
   Parameter[3] := 0;
   Parameter[4] := -10;
   Parameter[5] := 1;
   Parameter[6] := 1;
   Parameter[7] := dB_to_Amp(-10);
   Parameter[8] := 0;
  end;
 with Programs[4] do
  begin
   Parameter[0] := 75;
   Parameter[1] := 1.8;
   Parameter[2] := 10;
   Parameter[3] := 0;
   Parameter[4] := -10;
   Parameter[5] := 1;
   Parameter[6] := dB_to_Amp(-4);
   Parameter[7] := 1;
   Parameter[8] := 0;
  end;
end;

procedure TMaxxBassCloneModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FMaxxBass) - 1
  do FreeAndNil(FMaxxBass[Channel]);
end;

procedure TMaxxBassCloneModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TMaxxBassCloneModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TMaxxBassCloneModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmMaxxBassClone.Create(Self);
end;

procedure TMaxxBassCloneModule.ParameterHighpassDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := '16 Hz DC Filter';
  1 : PreDefined := '12 dB/oct';
  2 : PreDefined := '24 dB/oct';
 end;
end;

procedure TMaxxBassCloneModule.ParameterListenDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Audio';
  1 : PreDefined := 'Original Bass';
  2 : PreDefined := 'MaxxBass';
 end;
end;

procedure TMaxxBassCloneModule.ParameterdBDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] = 0
  then PreDefined := '-oo'
  else PreDefined := FloatToStrF(RoundTo(Amp_to_dB(Parameter[Index]), -2), ffGeneral, 3, 3);
end;

procedure TMaxxBassCloneModule.ParameterHighpassSelectChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel])
    then FMaxxBass[Channel].HighpassSelect := THighpassSelect(round(Value));
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.ParameterInputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel])
    then FMaxxBass[Channel].InputLevel := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.ParameterDecayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel])
    then FMaxxBass[Channel].Decay := 0.5 * dB_to_Amp(Value);
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.ParameterResponseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
const
  CScale : Single = 0.36787945032;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel])
    then FMaxxBass[Channel].Response := Value * CScale;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel])
    then FMaxxBass[Channel].Ratio := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.ParameterOriginalBassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure TMaxxBassCloneModule.ParameterMaxxbassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure TMaxxBassCloneModule.ParameterListenChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure TMaxxBassCloneModule.CalculateGains;
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel]) then
    case round(Parameter[8]) of
     0 : begin
          FMaxxBass[Channel].OriginalBassLevel := Parameter[6];
          FMaxxBass[Channel].MaxxBassLevel := Parameter[7];
          FMaxxBass[Channel].HighFrequencyLevel := 1;
         end;
     1 : begin
          FMaxxBass[Channel].OriginalBassLevel := 1;
          FMaxxBass[Channel].MaxxBassLevel := 0;
          FMaxxBass[Channel].HighFrequencyLevel := 0;
         end;
     2 : begin
          FMaxxBass[Channel].OriginalBassLevel := 0;
          FMaxxBass[Channel].MaxxBassLevel := 1;
          FMaxxBass[Channel].HighFrequencyLevel := 0;
         end;
    end;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel])
    then FMaxxBass[Channel].Frequency := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   if assigned(FMaxxBass[Channel])
    then FMaxxBass[Channel].SampleRate := SampleRate;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel   : Integer;
  Sample    : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FMaxxBass[Channel].Process(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

procedure TMaxxBassCloneModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel   : Integer;
  Sample    : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FMaxxBass) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FMaxxBass[Channel].Process(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

end.
