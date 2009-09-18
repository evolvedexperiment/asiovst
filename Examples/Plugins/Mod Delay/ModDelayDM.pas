unit ModDelayDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspModDelay;

type
  TModDelayModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterLowpassDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowpassLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowpassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ModDelayModuleParameterProperties0CustomParameterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FGain     : Single;
    FModDelay : TModDelay32;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, ModDelayGUI;

procedure TModDelayModule.VSTModuleOpen(Sender: TObject);
begin
 FModDelay := TModDelay32.Create;
 FModDelay.SampleRate := SampleRate;

 Parameter[0] := -3;
 Parameter[1] := 25;
 Parameter[2] := 22000;
 Parameter[3] := 20;
 Parameter[4] := 20;
 Parameter[5] := 2;
 Parameter[6] := 10;
end;

procedure TModDelayModule.ModDelayModuleParameterProperties0CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TModDelayModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay)
  then FModDelay.Mix := Value; 
end;

procedure TModDelayModule.ParameterDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay)
  then FModDelay.Depth := Value;
end;

procedure TModDelayModule.ParameterRateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay)
  then FModDelay.Rate := Value;
end;

procedure TModDelayModule.ParameterFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay)
  then FModDelay.Feedback := Value;
end;

procedure TModDelayModule.ParameterDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay)
  then FModDelay.Delay := Value;
end;

procedure TModDelayModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain := dB_to_Amp(Value);
end;

procedure TModDelayModule.ParameterLowpassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay)
  then FModDelay.LowpassFrequency := Value;
end;

procedure TModDelayModule.ParameterLowpassLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq: Single;
begin
 Freq := FModDelay.LowpassFrequency;
 if Freq < 1000
  then PreDefined := 'Hz' else
 if Freq <= 20000
  then PreDefined := 'kHz'
  else PreDefined := '';
end;

procedure TModDelayModule.ParameterLowpassDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq: Single;
begin
 Freq := FModDelay.LowpassFrequency;
 if Freq < 1000
  then PreDefined := FloatToStrF(Freq, ffGeneral, 3, 3) else
 if Freq < 20000
  then PreDefined := FloatToStrF(0.001 * Freq, ffGeneral, 3, 3)
  else PreDefined := 'off';
end;

procedure TModDelayModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FModDelay);
end;

procedure TModDelayModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmModDelay.Create(Self);
end;

procedure TModDelayModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FModDelay)
  then FModDelay.Samplerate := SampleRate;
end;

procedure TModDelayModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Outputs[0, Sample] := FModDelay.ProcessSample32(FGain * Inputs[0, Sample]);
end;

end.
