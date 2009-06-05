unit MBCDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilter, DAV_DspFilterLinkwitzRiley, DAV_DspDynamics,
  DAV_DspLightweightDynamics;

type
  TBandState = (bsNone, bsMute, bsBypass); 
  TMBCDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacingLimiter(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessLimiter(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure MBCDMLowFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDCLowOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDCHighOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMMidGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMLowThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMMidThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMMidRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMLowRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMLowAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMMidAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMLowReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMMidReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure MBCDMHighReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLimiterChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FLowSplit     : array [0..1] of TLinkwitzRiley;
    FHighSplit    : array [0..1] of TLinkwitzRiley;
    FInputPeak    : array [0..7] of Single;
    FOutputPeak   : array [0..7] of Single;
    FLowState     : TBandState;
    FMidState     : TBandState;
    FHighState    : TBandState;
    FLowComp      : TLightweightSoftKneeCompressor;
    FMidComp      : TLightweightSoftKneeCompressor;
    FHighComp     : TLightweightSoftKneeCompressor;
    FMeterRelease : Single;
    function GetHighGainReduction: Single;
    function GetLowGainReduction: Single;
    function GetMidGainReduction: Single;
  public
    property LowGainReduction: Single read GetLowGainReduction;
    property MidGainReduction: Single read GetMidGainReduction;
    property HighGainReduction: Single read GetHighGainReduction;
    property InputPeakLeft: Single read FInputPeak[0];
    property InputPeakRight: Single read FInputPeak[1];
    property OutputPeakLeft: Single read FOutputPeak[0];
    property OutputPeakRight: Single read FOutputPeak[1];
    property LowInputPeakLeft: Single read FInputPeak[2];
    property LowInputPeakRight: Single read FInputPeak[3];
    property LowOutputPeakLeft: Single read FOutputPeak[2];
    property LowOutputPeakRight: Single read FOutputPeak[3];
    property MidInputPeakLeft: Single read FInputPeak[4];
    property MidInputPeakRight: Single read FInputPeak[5];
    property MidOutputPeakLeft: Single read FOutputPeak[4];
    property MidOutputPeakRight: Single read FOutputPeak[5];
    property HighInputPeakLeft: Single read FInputPeak[6];
    property HighInputPeakRight: Single read FInputPeak[7];
    property HighOutputPeakLeft: Single read FOutputPeak[6];
    property HighOutputPeakRight: Single read FOutputPeak[7];
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, MBCGUI;

procedure TMBCDataModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to 1 do
  begin
   FLowSplit[i]  := TLinkwitzRiley.Create;
   FHighSplit[i] := TLinkwitzRiley.Create;
  end;
 FLowComp      := TLightweightSoftKneeCompressor.Create;
 FMidComp      := TLightweightSoftKneeCompressor.Create;
 FHighComp     := TLightweightSoftKneeCompressor.Create;
 FLowState     := bsNone;
 FMidState     := bsNone;
 FHighState    := bsNone;
 Parameter[ 0] := 0;
 Parameter[ 1] := 200;
 Parameter[ 2] := 2;
 Parameter[ 3] := -6;
 Parameter[ 4] := 4;
 Parameter[ 5] := 0.01;
 Parameter[ 6] := 0.1;
 Parameter[ 7] := 0;
 Parameter[ 8] := -6;
 Parameter[ 9] := 4;
 Parameter[10] := 0.01;
 Parameter[11] := 0.1;
 Parameter[12] := 0;
 Parameter[13] := 6000;
 Parameter[14] := 2;
 Parameter[15] := -6;
 Parameter[16] := 4;
 Parameter[17] := 0.01;
 Parameter[18] := 0.1;
 FMeterRelease := 0.9999;
end;

procedure TMBCDataModule.VSTModuleClose(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to 1 do
   begin
    FreeAndNil(FLowSplit[i]);
    FreeAndNil(FHighSplit[i]);
   end;
 FreeAndNil(FLowComp);
 FreeAndNil(FMidComp);
 FreeAndNil(FHighComp);
end;

procedure TMBCDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmMBC.Create(Self);
end;

procedure TMBCDataModule.MBCDMLowFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  i : Integer;
begin
 for i := 0 to Length(FLowSplit) - 1 do
  if assigned(FLowSplit[i])
   then FLowSplit[i].Frequency := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    if Value < 1000
     then LbLowFreqHz.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' Hz'
     else LbLowFreqHz.Caption := FloatToStrF(0.001 * Value, ffGeneral, 3, 2) + 'kHz';
    if SbLowFreq.Position <> Round(10000 * FreqLogToLinear(Value))
     then SbLowFreq.Position := Round(10000 * FreqLogToLinear(Value));
   end;
end;

procedure TMBCDataModule.MBCDCLowOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  i : Integer;
begin
 for i := 0 to Length(FLowSplit) - 1 do
  if assigned(FLowSplit[i])
   then FLowSplit[i].Order := round(Value);
end;

procedure TMBCDataModule.MBCDMLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLowComp)
  then FLowComp.MakeUpGain_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbLowGaindB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlLowGain.Position <> Value
     then DlLowGain.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMMidGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FMidComp)
  then FMidComp.MakeUpGain_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbMidGaindB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlMidGain.Position <> Value
     then DlMidGain.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMHighGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FHighComp)
  then FHighComp.MakeUpGain_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbHighGaindB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlHighGain.Position <> Value
     then DlHighGain.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMLowThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLowComp)
  then FLowComp.Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbLowThresholddB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlLowThreshold.Position <> Value
     then DlLowThreshold.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMMidThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FMidComp)
  then FMidComp.Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbMidThresholddB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlMidThreshold.Position <> Value
     then DlMidThreshold.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMHighThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FHighComp)
  then FHighComp.Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbHighThresholddB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlHighThreshold.Position <> Value
     then DlHighThreshold.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMLowRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLowComp)
  then FLowComp.Ratio := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbLowRatioValue.Caption := '1:' + FloatToStrF(Value, ffGeneral, 3, 2);
    if DlLowRatio.Position <> Log10(Value)
     then DlLowRatio.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMMidRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FMidComp)
  then FMidComp.Ratio := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbMidRatioValue.Caption := '1:' + FloatToStrF(Value, ffGeneral, 3, 2);
    if DlMidRatio.Position <> Log10(Value)
     then DlMidRatio.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMHighRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FHighComp)
  then FHighComp.Ratio := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbHighRatioValue.Caption := '1:' + FloatToStrF(Value, ffGeneral, 3, 2);
    if DlHighRatio.Position <> Log10(Value)
     then DlHighRatio.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMLowAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLowComp)
  then FLowComp.Attack := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbLowAttackValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlLowAttack.Position <> Log10(Value)
     then DlLowAttack.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMMidAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FMidComp)
  then FMidComp.Attack := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbMidAttackValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlMidAttack.Position <> Log10(Value)
     then DlMidAttack.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMHighAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FHighComp)
  then FHighComp.Attack := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm) do
   begin
    LbHighAttackValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlHighAttack.Position <> Log10(Value)
     then DlHighAttack.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMLowReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLowComp)
  then FLowComp.Release := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm)
   do UpdateLowRelease;
end;

procedure TMBCDataModule.MBCDMMidReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FMidComp)
  then FMidComp.Release := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm)
   do UpdateMidRelease;
end;

procedure TMBCDataModule.MBCDMHighReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FHighComp)
  then FHighComp.Release := Value;

 // update GUI if necessary
 if EditorForm is TFmMBC then
  with TFmMBC(EditorForm)
   do UpdateHighRelease;
end;

function TMBCDataModule.GetHighGainReduction: Single;
begin
 result := 1 - FHighComp.GainReductionFactor;
end;

function TMBCDataModule.GetLowGainReduction: Single;
begin
 result := 1 - FLowComp.GainReductionFactor;
end;

function TMBCDataModule.GetMidGainReduction: Single;
begin
 result := 1 - FMidComp.GainReductionFactor;
end;

procedure TMBCDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if round(Parameter[Index]) = 0
  then PreDefined := 'Off'
  else PreDefined := 'On';
end;

procedure TMBCDataModule.ParameterLimiterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case round(Value) of
  0: begin
      OnProcess := VSTModuleProcess;
      OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacing;
     end;
  1: begin
      OnProcess := VSTModuleProcessLimiter;
      OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacingLimiter;
     end;
 end;
 OnProcessReplacing := OnProcess; 
end;

procedure TMBCDataModule.MBCDCHighOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  i : Integer;
begin
 for i := 0 to Length(FHighSplit) - 1 do
  if assigned(FHighSplit[i])
   then FHighSplit[i].Order := round(Value);
end;

procedure TMBCDataModule.MBCDMHighFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  i : Integer;
begin
 for i := 0 to Length(FHighSplit) - 1 do
  if assigned(FHighSplit[i])
   then FHighSplit[i].Frequency := Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    if Value < 1000
     then LbHighFreqHz.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + 'Hz'
     else LbHighFreqHz.Caption := FloatToStrF(0.001 * Value, ffGeneral, 3, 2) + 'kHz';
    if SbHighFreq.Position <> Round(10000 * FreqLogToLinear(Value))
     then SbHighFreq.Position := Round(10000 * FreqLogToLinear(Value));
   end;
end;

procedure TMBCDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i       : Integer;
  L, M, H : array [0..1] of Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   if abs(Inputs[0, i]) > FInputPeak[0] then FInputPeak[0] := abs(Inputs[0, i]);
   FInputPeak[0] := FMeterRelease * FInputPeak[0];
   if abs(Inputs[1, i]) > FInputPeak[1] then FInputPeak[1] := abs(Inputs[1, i]);
   FInputPeak[1] := FMeterRelease * FInputPeak[1];

   FLowSplit[0].ProcessSample(Inputs[0, i], L[0], M[0]);
   FLowSplit[1].ProcessSample(Inputs[1, i], L[1], M[1]);
   FHighSplit[0].ProcessSample(M[0], M[0], H[0]);
   FHighSplit[1].ProcessSample(M[1], M[1], H[1]);

   if abs(L[0]) > FInputPeak[2] then FInputPeak[2] := abs(L[0]);
   FInputPeak[2] := FMeterRelease * FInputPeak[2];
   if abs(L[1]) > FInputPeak[3] then FInputPeak[3] := abs(L[1]);
   FInputPeak[3] := FMeterRelease * FInputPeak[3];

   if abs(M[0]) > FInputPeak[4] then FInputPeak[4] := abs(M[0]);
   FInputPeak[4] := FMeterRelease * FInputPeak[4];
   if abs(M[1]) > FInputPeak[5] then FInputPeak[5] := abs(M[1]);
   FInputPeak[5] := FMeterRelease * FInputPeak[5];

   if abs(H[0]) > FInputPeak[6] then FInputPeak[6] := abs(H[0]);
   FInputPeak[6] := FMeterRelease * FInputPeak[6];
   if abs(H[1]) > FInputPeak[7] then FInputPeak[7] := abs(H[1]);
   FInputPeak[7] := FMeterRelease * FInputPeak[7];

   FLowComp.InputSample(CHalf32 * (L[0] + L[1]));
   FMidComp.InputSample(CHalf32 * (M[0] + M[1]));
   FHighComp.InputSample(CHalf32 * (H[0] + H[1]));

   case FLowState of
    bsNone : begin
              L[0] := FLowComp.GainSample(L[0]);
              L[1] := FLowComp.GainSample(L[1]);
             end;
    bsMute : begin
              L[0] := 0;
              L[1] := 0;
             end;
   end;
   case FMidState of
    bsNone : begin
              M[0] := FMidComp.GainSample(M[0]);
              M[1] := FMidComp.GainSample(M[1]);
             end;
    bsMute : begin
              M[0] := 0;
              M[1] := 0;
             end;
   end;
   case FHighState of
    bsNone : begin
              H[0] := FHighComp.GainSample(H[0]);
              H[1] := FHighComp.GainSample(H[1]);
             end;
    bsMute : begin
              H[0] := 0;
              H[1] := 0;
             end;
   end;

   if abs(L[0]) > FOutputPeak[2] then FOutputPeak[2] := abs(L[0]);
   FOutputPeak[2] := FMeterRelease * FOutputPeak[2];
   if abs(L[1]) > FOutputPeak[3] then FOutputPeak[3] := abs(L[1]);
   FOutputPeak[3] := FMeterRelease * FOutputPeak[3];

   if abs(M[0]) > FOutputPeak[4] then FOutputPeak[4] := abs(M[0]);
   FOutputPeak[4] := FMeterRelease * FOutputPeak[4];
   if abs(M[1]) > FOutputPeak[5] then FOutputPeak[5] := abs(M[1]);
   FOutputPeak[5] := FMeterRelease * FOutputPeak[5];

   if abs(H[0]) > FOutputPeak[6] then FOutputPeak[6] := abs(H[0]);
   FOutputPeak[6] := FMeterRelease * FOutputPeak[6];
   if abs(H[1]) > FOutputPeak[7] then FOutputPeak[7] := abs(H[1]);
   FOutputPeak[7] := FMeterRelease * FOutputPeak[7];

   Outputs[0, i] := L[0] + M[0] + H[0];
   Outputs[1, i] := L[1] + M[1] + H[1];

   if abs(Outputs[0, i]) > FOutputPeak[0] then FOutputPeak[0] := abs(Outputs[0, i]);
   FOutputPeak[0] := FMeterRelease * FOutputPeak[0];
   if abs(Outputs[1, i]) > FOutputPeak[1] then FOutputPeak[1] := abs(Outputs[1, i]);
   FOutputPeak[1] := FMeterRelease * FOutputPeak[1];
  end;
end;

procedure TMBCDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i       : Integer;
  L, M, H : array [0..1] of Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   if abs(Inputs[0, i]) > FInputPeak[0] then FInputPeak[0] := abs(Inputs[0, i]);
   FInputPeak[0] := FMeterRelease * FInputPeak[0];
   if abs(Inputs[1, i]) > FInputPeak[1] then FInputPeak[1] := abs(Inputs[1, i]);
   FInputPeak[1] := FMeterRelease * FInputPeak[1];

   FLowSplit[0].ProcessSample(Inputs[0, i], L[0], M[0]);
   FLowSplit[1].ProcessSample(Inputs[1, i], L[1], M[1]);
   FHighSplit[0].ProcessSample(M[0], M[0], H[0]);
   FHighSplit[1].ProcessSample(M[1], M[1], H[1]);

   if abs(L[0]) > FInputPeak[2] then FInputPeak[2] := abs(L[0]);
   FInputPeak[2] := FMeterRelease * FInputPeak[2];
   if abs(L[1]) > FInputPeak[3] then FInputPeak[3] := abs(L[1]);
   FInputPeak[3] := FMeterRelease * FInputPeak[3];

   if abs(M[0]) > FInputPeak[4] then FInputPeak[4] := abs(M[0]);
   FInputPeak[4] := FMeterRelease * FInputPeak[4];
   if abs(M[1]) > FInputPeak[5] then FInputPeak[5] := abs(M[1]);
   FInputPeak[5] := FMeterRelease * FInputPeak[5];

   if abs(H[0]) > FInputPeak[6] then FInputPeak[6] := abs(H[0]);
   FInputPeak[6] := FMeterRelease * FInputPeak[6];
   if abs(H[1]) > FInputPeak[7] then FInputPeak[7] := abs(H[1]);
   FInputPeak[7] := FMeterRelease * FInputPeak[7];

   FLowComp.InputSample(CHalf32 * (L[0] + L[1]));
   FMidComp.InputSample(CHalf32 * (M[0] + M[1]));
   FHighComp.InputSample(CHalf32 * (H[0] + H[1]));

   case FLowState of
    bsNone : begin
              L[0] := FLowComp.GainSample(L[0]);
              L[1] := FLowComp.GainSample(L[1]);
             end;
    bsMute : begin
              L[0] := 0;
              L[1] := 0;
             end;
   end;
   case FMidState of
    bsNone : begin
              M[0] := FMidComp.GainSample(M[0]);
              M[1] := FMidComp.GainSample(M[1]);
             end;
    bsMute : begin
              M[0] := 0;
              M[1] := 0;
             end;
   end;
   case FHighState of
    bsNone : begin
              H[0] := FHighComp.GainSample(H[0]);
              H[1] := FHighComp.GainSample(H[1]);
             end;
    bsMute : begin
              H[0] := 0;
              H[1] := 0;
             end;
   end;

   if abs(L[0]) > FOutputPeak[2] then FOutputPeak[2] := abs(L[0]);
   FOutputPeak[2] := FMeterRelease * FOutputPeak[2];
   if abs(L[1]) > FOutputPeak[3] then FOutputPeak[3] := abs(L[1]);
   FOutputPeak[3] := FMeterRelease * FOutputPeak[3];

   if abs(M[0]) > FOutputPeak[4] then FOutputPeak[4] := abs(M[0]);
   FOutputPeak[4] := FMeterRelease * FOutputPeak[4];
   if abs(M[1]) > FOutputPeak[5] then FOutputPeak[5] := abs(M[1]);
   FOutputPeak[5] := FMeterRelease * FOutputPeak[5];

   if abs(H[0]) > FOutputPeak[6] then FOutputPeak[6] := abs(H[0]);
   FOutputPeak[6] := FMeterRelease * FOutputPeak[6];
   if abs(H[1]) > FOutputPeak[7] then FOutputPeak[7] := abs(H[1]);
   FOutputPeak[7] := FMeterRelease * FOutputPeak[7];

   Outputs[0, i] := L[0] + M[0] + H[0];
   Outputs[1, i] := L[1] + M[1] + H[1];

   if abs(Outputs[0, i]) > FOutputPeak[0] then FOutputPeak[0] := abs(Outputs[0, i]);
   FOutputPeak[0] := FMeterRelease * FOutputPeak[0];
   if abs(Outputs[1, i]) > FOutputPeak[1] then FOutputPeak[1] := abs(Outputs[1, i]);
   FOutputPeak[1] := FMeterRelease * FOutputPeak[1];
  end;
end;


procedure TMBCDataModule.VSTModuleProcessLimiter(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i       : Integer;
  L, M, H : array [0..1] of Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   if abs(Inputs[0, i]) > FInputPeak[0] then FInputPeak[0] := abs(Inputs[0, i]);
   FInputPeak[0] := FMeterRelease * FInputPeak[0];
   if abs(Inputs[1, i]) > FInputPeak[1] then FInputPeak[1] := abs(Inputs[1, i]);
   FInputPeak[1] := FMeterRelease * FInputPeak[1];

   FLowSplit[0].ProcessSample(Inputs[0, i], L[0], M[0]);
   FLowSplit[1].ProcessSample(Inputs[1, i], L[1], M[1]);
   FHighSplit[0].ProcessSample(M[0], M[0], H[0]);
   FHighSplit[1].ProcessSample(M[1], M[1], H[1]);

   if abs(L[0]) > FInputPeak[2] then FInputPeak[2] := abs(L[0]);
   FInputPeak[2] := FMeterRelease * FInputPeak[2];
   if abs(L[1]) > FInputPeak[3] then FInputPeak[3] := abs(L[1]);
   FInputPeak[3] := FMeterRelease * FInputPeak[3];

   if abs(M[0]) > FInputPeak[4] then FInputPeak[4] := abs(M[0]);
   FInputPeak[4] := FMeterRelease * FInputPeak[4];
   if abs(M[1]) > FInputPeak[5] then FInputPeak[5] := abs(M[1]);
   FInputPeak[5] := FMeterRelease * FInputPeak[5];

   if abs(H[0]) > FInputPeak[6] then FInputPeak[6] := abs(H[0]);
   FInputPeak[6] := FMeterRelease * FInputPeak[6];
   if abs(H[1]) > FInputPeak[7] then FInputPeak[7] := abs(H[1]);
   FInputPeak[7] := FMeterRelease * FInputPeak[7];

   FLowComp.InputSample(CHalf32 * (L[0] + L[1]));
   FMidComp.InputSample(CHalf32 * (M[0] + M[1]));
   FHighComp.InputSample(CHalf32 * (H[0] + H[1]));

   case FLowState of
    bsNone : begin
              L[0] := FLowComp.GainSample(L[0]);
              L[1] := FLowComp.GainSample(L[1]);
             end;
    bsMute : begin
              L[0] := 0;
              L[1] := 0;
             end;
   end;
   case FMidState of
    bsNone : begin
              M[0] := FMidComp.GainSample(M[0]);
              M[1] := FMidComp.GainSample(M[1]);
             end;
    bsMute : begin
              M[0] := 0;
              M[1] := 0;
             end;
   end;
   case FHighState of
    bsNone : begin
              H[0] := FHighComp.GainSample(H[0]);
              H[1] := FHighComp.GainSample(H[1]);
             end;
    bsMute : begin
              H[0] := 0;
              H[1] := 0;
             end;
   end;

   if abs(L[0]) > FOutputPeak[2] then FOutputPeak[2] := abs(L[0]);
   FOutputPeak[2] := FMeterRelease * FOutputPeak[2];
   if abs(L[1]) > FOutputPeak[3] then FOutputPeak[3] := abs(L[1]);
   FOutputPeak[3] := FMeterRelease * FOutputPeak[3];

   if abs(M[0]) > FOutputPeak[4] then FOutputPeak[4] := abs(M[0]);
   FOutputPeak[4] := FMeterRelease * FOutputPeak[4];
   if abs(M[1]) > FOutputPeak[5] then FOutputPeak[5] := abs(M[1]);
   FOutputPeak[5] := FMeterRelease * FOutputPeak[5];

   if abs(H[0]) > FOutputPeak[6] then FOutputPeak[6] := abs(H[0]);
   FOutputPeak[6] := FMeterRelease * FOutputPeak[6];
   if abs(H[1]) > FOutputPeak[7] then FOutputPeak[7] := abs(H[1]);
   FOutputPeak[7] := FMeterRelease * FOutputPeak[7];

   Outputs[0, i] := FastTanhOpt3Term(L[0] + M[0] + H[0]);
   Outputs[1, i] := FastTanhOpt3Term(L[1] + M[1] + H[1]);

   if abs(Outputs[0, i]) > FOutputPeak[0] then FOutputPeak[0] := abs(Outputs[0, i]);
   FOutputPeak[0] := FMeterRelease * FOutputPeak[0];
   if abs(Outputs[1, i]) > FOutputPeak[1] then FOutputPeak[1] := abs(Outputs[1, i]);
   FOutputPeak[1] := FMeterRelease * FOutputPeak[1];
  end;
end;

procedure TMBCDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLowSplit) - 1 do
  if assigned(FLowSplit[Channel])
   then FLowSplit[Channel].SampleRate := SampleRate;
 for Channel := 0 to Length(FHighSplit) - 1 do
  if assigned(FHighSplit[Channel])
   then FHighSplit[Channel].SampleRate := SampleRate;
end;

procedure TMBCDataModule.VSTModuleProcessDoubleReplacingLimiter(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i       : Integer;
  L, M, H : array [0..1] of Single;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   if abs(Inputs[0, i]) > FInputPeak[0] then FInputPeak[0] := abs(Inputs[0, i]);
   FInputPeak[0] := FMeterRelease * FInputPeak[0];
   if abs(Inputs[1, i]) > FInputPeak[1] then FInputPeak[1] := abs(Inputs[1, i]);
   FInputPeak[1] := FMeterRelease * FInputPeak[1];

   FLowSplit[0].ProcessSample(Inputs[0, i], L[0], M[0]);
   FLowSplit[1].ProcessSample(Inputs[1, i], L[1], M[1]);
   FHighSplit[0].ProcessSample(M[0], M[0], H[0]);
   FHighSplit[1].ProcessSample(M[1], M[1], H[1]);

   if abs(L[0]) > FInputPeak[2] then FInputPeak[2] := abs(L[0]);
   FInputPeak[2] := FMeterRelease * FInputPeak[2];
   if abs(L[1]) > FInputPeak[3] then FInputPeak[3] := abs(L[1]);
   FInputPeak[3] := FMeterRelease * FInputPeak[3];

   if abs(M[0]) > FInputPeak[4] then FInputPeak[4] := abs(M[0]);
   FInputPeak[4] := FMeterRelease * FInputPeak[4];
   if abs(M[1]) > FInputPeak[5] then FInputPeak[5] := abs(M[1]);
   FInputPeak[5] := FMeterRelease * FInputPeak[5];

   if abs(H[0]) > FInputPeak[6] then FInputPeak[6] := abs(H[0]);
   FInputPeak[6] := FMeterRelease * FInputPeak[6];
   if abs(H[1]) > FInputPeak[7] then FInputPeak[7] := abs(H[1]);
   FInputPeak[7] := FMeterRelease * FInputPeak[7];

   FLowComp.InputSample(CHalf32 * (L[0] + L[1]));
   FMidComp.InputSample(CHalf32 * (M[0] + M[1]));
   FHighComp.InputSample(CHalf32 * (H[0] + H[1]));

   case FLowState of
    bsNone : begin
              L[0] := FLowComp.GainSample(L[0]);
              L[1] := FLowComp.GainSample(L[1]);
             end;
    bsMute : begin
              L[0] := 0;
              L[1] := 0;
             end;
   end;
   case FMidState of
    bsNone : begin
              M[0] := FMidComp.GainSample(M[0]);
              M[1] := FMidComp.GainSample(M[1]);
             end;
    bsMute : begin
              M[0] := 0;
              M[1] := 0;
             end;
   end;
   case FHighState of
    bsNone : begin
              H[0] := FHighComp.GainSample(H[0]);
              H[1] := FHighComp.GainSample(H[1]);
             end;
    bsMute : begin
              H[0] := 0;
              H[1] := 0;
             end;
   end;

   if abs(L[0]) > FOutputPeak[2] then FOutputPeak[2] := abs(L[0]);
   FOutputPeak[2] := FMeterRelease * FOutputPeak[2];
   if abs(L[1]) > FOutputPeak[3] then FOutputPeak[3] := abs(L[1]);
   FOutputPeak[3] := FMeterRelease * FOutputPeak[3];

   if abs(M[0]) > FOutputPeak[4] then FOutputPeak[4] := abs(M[0]);
   FOutputPeak[4] := FMeterRelease * FOutputPeak[4];
   if abs(M[1]) > FOutputPeak[5] then FOutputPeak[5] := abs(M[1]);
   FOutputPeak[5] := FMeterRelease * FOutputPeak[5];

   if abs(H[0]) > FOutputPeak[6] then FOutputPeak[6] := abs(H[0]);
   FOutputPeak[6] := FMeterRelease * FOutputPeak[6];
   if abs(H[1]) > FOutputPeak[7] then FOutputPeak[7] := abs(H[1]);
   FOutputPeak[7] := FMeterRelease * FOutputPeak[7];

   Outputs[0, i] := FastTanhOpt3Term(L[0] + M[0] + H[0]);
   Outputs[1, i] := FastTanhOpt3Term(L[1] + M[1] + H[1]);

   if abs(Outputs[0, i]) > FOutputPeak[0] then FOutputPeak[0] := abs(Outputs[0, i]);
   FOutputPeak[0] := FMeterRelease * FOutputPeak[0];
   if abs(Outputs[1, i]) > FOutputPeak[1] then FOutputPeak[1] := abs(Outputs[1, i]);
   FOutputPeak[1] := FMeterRelease * FOutputPeak[1];
  end;
end;

end.
