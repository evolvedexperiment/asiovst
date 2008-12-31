unit ImageDM;

interface

uses
  Windows, Messages, SysUtils, Classes, DAV_Common, DAV_VSTModule;

type
  TImageMode = (imSMLR, imMSLR, imLRLR, imLRMS);
  TImageDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure ParamModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterSWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSPanChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMPanChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FIntParam  : Array [0..1, 0..1] of Single;
    FPan       : Single;
    FWidth     : Single;
    FDepth     : Single;
    FBalance   : Single;
    FOutGain   : Single;
    FImageMode : TImageMode;
    procedure SetBalance(const Value: Single);
    procedure SetDepth(const Value: Single);
    procedure SetPan(const Value: Single);
    procedure SetWidth(const Value: Single);
    procedure SetOutputGain(const Value: Single);
    procedure SetImageMode(const Value: TImageMode);
  protected
    procedure UpdateInternalParameters; virtual;
    procedure UpdateInternalParametersLRLR; virtual;
    procedure UpdateInternalParametersLRMS; virtual;
    procedure UpdateInternalParametersMSLR; virtual;
    procedure UpdateInternalParametersSMLR; virtual;
  public
    property Balance: Single read FBalance write SetBalance;
    property Depth: Single read FDepth write SetDepth;
    property ImageMode: TImageMode read FImageMode write SetImageMode;
    property OutputGain: Single read FOutGain write SetOutputGain;
    property Pan: Single read FPan write SetPan;
    property Width: Single read FWidth write SetWidth;
  end;

implementation

{$R *.DFM}

procedure TImageDataModule.ParamModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[0]) of
  0: PreDefined := 'SM->LR';
  1: PreDefined := 'MS->LR';
  2: PreDefined := 'LR->LR';
  3: PreDefined := 'LR->MS';
 end;
end;

procedure TImageDataModule.SetBalance(const Value: Single);
begin
 if FBalance <> Value then
  begin
   FBalance := Value;
   UpdateInternalParameters;
  end;
end;

procedure TImageDataModule.SetDepth(const Value: Single);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   UpdateInternalParameters;
  end;
end;

procedure TImageDataModule.SetImageMode(const Value: TImageMode);
begin
 if FImageMode <> Value then
  begin
   FImageMode := Value;
   UpdateInternalParameters;
  end;
end;

procedure TImageDataModule.SetOutputGain(const Value: Single);
begin
 if FOutGain <> Value then
  begin
   FOutGain := Value;
   UpdateInternalParameters;
  end;
end;

procedure TImageDataModule.SetPan(const Value: Single);
begin
 if FPan <> Value then
  begin
   FPan := Value;
   UpdateInternalParameters;
  end;
end;

procedure TImageDataModule.SetWidth(const Value: Single);
begin
 if FWidth <> Value then
  begin
   FWidth := Value;
   UpdateInternalParameters;
  end;
end;

procedure TImageDataModule.ParameterSWidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Width := 4 * Parameter[1] - 2;
end;

procedure TImageDataModule.ParameterSPanChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Balance := 2 * Parameter[2];
end;

procedure TImageDataModule.ParameterModeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ImageMode := TImageMode(round(Value));
end;

procedure TImageDataModule.ParameterMLevelChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Depth := 4 * Parameter[3] - 2;
end;

procedure TImageDataModule.ParameterMPanChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Pan := 2 * Parameter[4];
end;

procedure TImageDataModule.ParameterOutputGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 OutputGain := dB_to_Amp(Parameter[5]);
end;

procedure TImageDataModule.UpdateInternalParameters;
begin
 case ImageMode of
  imSMLR: UpdateInternalParametersSMLR;
  imMSLR: UpdateInternalParametersMSLR;
  imLRLR: UpdateInternalParametersLRLR;
  imLRMS: UpdateInternalParametersLRMS;
 end;
end;

procedure TImageDataModule.UpdateInternalParametersSMLR;
begin
 FIntParam[0, 0] :=  FOutGain * FDepth * FPan;
 FIntParam[0, 1] := -FOutGain * FWidth * FBalance;
 FIntParam[1, 0] :=  FOutGain * FDepth * (2 - FPan);
 FIntParam[1, 1] :=  FOutGain * FWidth * (2 - FBalance);
end;

procedure TImageDataModule.UpdateInternalParametersMSLR;
begin
 FIntParam[0, 0] := -FOutGain * FWidth * FBalance;
 FIntParam[0, 1] :=  FOutGain * FDepth * FPan;
 FIntParam[1, 0] :=  FOutGain * FWidth * (2 - FBalance);
 FIntParam[1, 1] :=  FOutGain * FDepth * (2 - FPan);
end;

procedure TImageDataModule.UpdateInternalParametersLRLR;
begin
 FIntParam[0, 0] := 0.5 * FOutGain * (FDepth * FPan + FWidth * FBalance);
 FIntParam[0, 1] := 0.5 * FOutGain * (FDepth * FPan - FWidth * FBalance);
 FIntParam[1, 0] := 0.5 * FOutGain * (FDepth * (2 - FPan) - FWidth * (2 - FBalance));
 FIntParam[1, 1] := 0.5 * FOutGain * (FDepth * (2 - FPan) + FWidth * (2 - FBalance));
end;

procedure TImageDataModule.UpdateInternalParametersLRMS;
begin
 FIntParam[0, 0] :=  0.5 * FOutGain * FPan * FBalance;
 FIntParam[0, 1] := -0.5 * FOutGain * FPan * (2 - FBalance);
 FIntParam[1, 0] :=  0.5 * FOutGain * (2 - FPan) * FBalance;
 FIntParam[1, 1] :=  0.5 * FOutGain * (2 - FPan) * (2 - FBalance);
end;

procedure TImageDataModule.VSTModuleOpen(Sender: TObject);
begin
 // Initial Parameters
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 0;
 Parameter[5] := 0;

 UpdateInternalParameters;
end;

procedure TImageDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FIntParam[0, 0] * Inputs[0, Sample] + FIntParam[0, 1] * Inputs[1, Sample];
   Outputs[1, Sample] := FIntParam[1, 1] * Inputs[1, Sample] + FIntParam[1, 0] * Inputs[0, Sample];
  end;
end;

procedure TImageDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FIntParam[0, 0] * Inputs[0, Sample] + FIntParam[0, 1] * Inputs[1, Sample];
   Outputs[1, Sample] := FIntParam[1, 1] * Inputs[1, Sample] + FIntParam[1, 0] * Inputs[0, Sample];
  end;
end;

end.
