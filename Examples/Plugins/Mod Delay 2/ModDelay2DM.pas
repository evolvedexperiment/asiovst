unit ModDelay2DM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilterButterworth, DAV_DspModDelay;

type
  TModDelay2Module = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterLowpassDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowpassLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLowpassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ModDelay2ModuleParameterProperties0CustomParameterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDepthChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FGain     : array [0..1] of Single;
    FFilter   : array [0..1] of TButterworthLowpassFilter;
    FModDelay : array [0..1] of TModDelay32;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, ModDelay2GUI;

procedure TModDelay2Module.VSTModuleOpen(Sender: TObject);
var
  Band : Integer;
begin
 for Band := 0 to Length(FModDelay) - 1 do
  begin
   FModDelay[Band] := TModDelay32.Create;
   FModDelay[Band].SampleRate := SampleRate
  end;

 for Band := 0 to Length(FFilter) - 1 do
  begin
   FFilter[Band] := TButterworthLowpassFilter.Create(1);
   FFilter[Band].SampleRate := SampleRate
  end;

 Parameter[0] := 0;
 Parameter[1] := 25;
 Parameter[2] := 22000;
 Parameter[7] := 0;
 Parameter[8] := 25;
 Parameter[9] := 22000;
end;

procedure TModDelay2Module.ModDelay2ModuleParameterProperties0CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TModDelay2Module.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay[Index div 7])
  then FModDelay[Index div 7].Mix := Value; 
end;

procedure TModDelay2Module.ParameterDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay[Index div 7])
  then FModDelay[Index div 7].Depth := Value;
end;

procedure TModDelay2Module.ParameterRateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay[Index div 7])
  then FModDelay[Index div 7].Rate := Value;
end;

procedure TModDelay2Module.ParameterFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay[Index div 7])
  then FModDelay[Index div 7].Feedback := Value;
end;

procedure TModDelay2Module.ParameterDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FModDelay[Index div 7])
  then FModDelay[Index div 7].Delay := Value;
end;

procedure TModDelay2Module.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain[Index div 7] := dB_to_Amp(Value);
end;

procedure TModDelay2Module.ParameterLowpassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FFilter[Index div 7])
  then FFilter[Index div 7].Frequency := Value;
end;

procedure TModDelay2Module.ParameterLowpassLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq: Single;
begin
 Freq := FFilter[Index div 7].Frequency;
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
 Freq := FFilter[Index div 7].Frequency;
 if Freq < 1000
  then PreDefined := FloatToStrF(Freq, ffGeneral, 3, 3) else
 if Freq < 20000
  then PreDefined := FloatToStrF(0.001 * Freq, ffGeneral, 3, 3)
  else PreDefined := 'off';
end;

procedure TModDelay2Module.VSTModuleClose(Sender: TObject);
var
  Band : Integer;
begin
 for Band := 0 to Length(FModDelay) - 1
  do FreeAndNil(FModDelay[0]);

 for Band := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[0]);
end;

procedure TModDelay2Module.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmModDelay2.Create(Self);
end;

procedure TModDelay2Module.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Band : Integer;
begin
 for Band := 0 to Length(FModDelay) - 1 do
  if assigned(FModDelay[Band])
   then FModDelay[Band].Samplerate := SampleRate;

 for Band := 0 to Length(FModDelay) - 1 do
  if assigned(FFilter[Band])
   then FFilter[Band].Samplerate := SampleRate;
end;

procedure TModDelay2Module.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FModDelay[0].ProcessSample(
                         FFilter[0].ProcessSample(FGain[0] * Inputs[0, Sample])) +
                         FModDelay[1].ProcessSample(
                         FFilter[1].ProcessSample(FGain[1] * Inputs[0, Sample]));
  end;
end;

end.
