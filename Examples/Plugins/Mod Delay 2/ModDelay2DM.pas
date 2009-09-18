unit ModDelay2DM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_DspModDelay;

type
  TModDelay2Module = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterLowpassDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowpassLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowpassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FGain            : array [0..1] of Single;
    FModDelay        : array [0..1] of TModDelay32;
    FCriticalSection : TCriticalSection;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, ModDelay2GUI;

procedure TModDelay2Module.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TModDelay2Module.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TModDelay2Module.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FModDelay) - 1 do
  begin
   FModDelay[Channel] := TModDelay32.Create;
   FModDelay[Channel].SampleRate := SampleRate
  end;

 // Delay Left
 Parameter[ 0] := 0;
 Parameter[ 1] := 25;
 Parameter[ 2] := 22000;
 Parameter[ 3] := 20;
 Parameter[ 4] := 20;
 Parameter[ 5] := 2;
 Parameter[ 6] := 10;
 // Delay Right
 Parameter[ 7] := 0;
 Parameter[ 8] := 25;
 Parameter[ 9] := 22000;
 Parameter[10] := 20;
 Parameter[11] := 20;
 Parameter[12] := 2;
 Parameter[13] := -10;

 with Programs[0] do
  begin
   // Delay Left
   Parameter[ 0] := 0;
   Parameter[ 1] := 25;
   Parameter[ 2] := 22000;
   Parameter[ 3] := 20;
   Parameter[ 4] := 20;
   Parameter[ 5] := 2;
   Parameter[ 6] := 10;
   // Delay Right
   Parameter[ 7] := 0;
   Parameter[ 8] := 25;
   Parameter[ 9] := 22000;
   Parameter[10] := 20;
   Parameter[11] := 20;
   Parameter[12] := 2;
   Parameter[13] := -10;
  end;
 with Programs[1] do
  begin
   // Delay Left
   Parameter[ 0] := 0;
   Parameter[ 1] := 44;
   Parameter[ 2] := 12000;
   Parameter[ 3] := 33;
   Parameter[ 4] := 10;
   Parameter[ 5] := 2;
   Parameter[ 6] := 90;
   // Delay Right
   Parameter[ 7] := 0;
   Parameter[ 8] := 42;
   Parameter[ 9] := 12000;
   Parameter[10] := 33;
   Parameter[11] := 10;
   Parameter[12] := 2;
   Parameter[13] := -90;
  end;
end;

procedure TModDelay2Module.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FModDelay) - 1
   do FreeAndNil(FModDelay[0]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.ParameterGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TModDelay2Module.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FModDelay[Index div 7])
   then FModDelay[Index div 7].Mix := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.ParameterDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FModDelay[Index div 7])
   then FModDelay[Index div 7].Depth := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.ParameterRateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FModDelay[Index div 7])
   then FModDelay[Index div 7].Rate := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.ParameterFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FModDelay[Index div 7])
   then FModDelay[Index div 7].Feedback := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.ParameterDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FModDelay[Index div 7])
   then FModDelay[Index div 7].Delay := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain[Index div 7] := dB_to_Amp(Value);
end;

procedure TModDelay2Module.ParameterLowpassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FModDelay[Index div 7])
   then FModDelay[Index div 7].LowpassFrequency := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.ParameterLowpassLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq: Single;
begin
 Freq := FModDelay[Index div 7].LowpassFrequency;
 if Freq < 1000
  then PreDefined := 'Hz' else
 if Freq <= 20000
  then PreDefined := 'kHz'
  else PreDefined := '';
end;

procedure TModDelay2Module.ParameterLowpassDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq: Single;
begin
 Freq := FModDelay[Index div 7].LowpassFrequency;
 if Freq < 1000
  then PreDefined := FloatToStrF(Freq, ffGeneral, 3, 3) else
 if Freq < 20000
  then PreDefined := FloatToStrF(0.001 * Freq, ffGeneral, 3, 3)
  else PreDefined := 'off';
end;

procedure TModDelay2Module.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmModDelay2.Create(Self);
end;

procedure TModDelay2Module.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FModDelay) - 1 do
   if assigned(FModDelay[Channel])
    then FModDelay[Channel].Samplerate := SampleRate;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TModDelay2Module.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to min(BlockSize, SampleFrames) - 1 do
   begin
    Outputs[0, Sample] := FModDelay[0].ProcessSample32(FGain[0] * Inputs[0, Sample]);
    Outputs[1, Sample] := FModDelay[1].ProcessSample32(FGain[1] * Inputs[1, Sample]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
