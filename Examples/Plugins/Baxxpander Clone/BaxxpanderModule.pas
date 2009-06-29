unit BaxxpanderModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common,
  DAV_DspFilterButterworth, DAV_VSTModule;

{$I DAV_Compiler.inc}

type
  TBaxxpanderModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcessNormal(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterDryWetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleProcessSaturated(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FButterworthSplitter : array [0..1] of TButterworthSplitBandFilter;
    FGains               : array [0..4] of Single;
    procedure CalculateGains;
  public
  end;

implementation

{$R *.DFM}

uses
  DAV_Approximations, DAV_DspWaveshaper; // BaxxpanderGui;

procedure TBaxxpanderModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  begin
   FButterworthSplitter[Channel] := TButterworthSplitBandFilter.Create(1);
   with FButterworthSplitter[Channel] do
    begin
     SampleRate := Self.SampleRate;
     Frequency := 250;
    end;
  end;

 Parameter[0] := 100;
 Parameter[1] := 100;
 Parameter[2] := 100;
 Parameter[3] := 1;
 Parameter[4] := 100;
end;

procedure TBaxxpanderModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FButterworthSplitter)
  do FreeAndNil(FButterworthSplitter[Channel]);
end;

procedure TBaxxpanderModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
//  GUI := TFmBaxxpanderGui.Create(Self);
end;

procedure TBaxxpanderModule.ParameterDryWetChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure TBaxxpanderModule.ParameterMixerChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure TBaxxpanderModule.ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure TBaxxpanderModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TBaxxpanderModule.ParameterShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;
end;

procedure TBaxxpanderModule.ParameterOnOffChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0.5
  then OnProcess := VSTModuleProcessSaturated
  else OnProcess := VSTModuleProcessNormal;

 OnProcessReplacing := OnProcess;
end;

procedure TBaxxpanderModule.CalculateGains;
begin
 FGains[0] := 1 - 0.002 * Parameter[0];
 FGains[1] := (1 + 0.0015 * Parameter[2]) * 12 * (1 - FGains[0]);
 FGains[2] := (0.5 + 0.005 * Parameter[2]) * 1 - FGains[0];
 FGains[4] := 0.01 * (0.25 + Parameter[4]);
 FGains[3] := 0.75 * FGains[1] * (0.005 * Parameter[1] / (0.01 + FGains[4]));
end;

procedure TBaxxpanderModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  begin
   FButterworthSplitter[Channel].SampleRate := SampleRate;
  end;
end;

procedure TBaxxpanderModule.VSTModuleProcessNormal(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
  Low, High       : Single;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    FButterworthSplitter[Channel].ProcessSample(Inputs[Channel, Sample], Low, High);
    Outputs[Channel, Sample] := FGains[0] * Inputs[Channel, Sample] +
                                FGains[1] * Low + FGains[2] * High;
   end;
end;

procedure TBaxxpanderModule.VSTModuleProcessSaturated(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
  Low, High       : Single;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    FButterworthSplitter[Channel].ProcessSample(Inputs[Channel, Sample], Low, High);
    Outputs[Channel, Sample] := FGains[0] * Inputs[Channel, Sample] +
                                FGains[3] * FastTanhContinousError5(FGains[4] * Low) + FGains[2] * High;
   end;
end;

end.
