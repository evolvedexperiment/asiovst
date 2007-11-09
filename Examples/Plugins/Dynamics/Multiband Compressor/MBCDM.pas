unit MBCDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule, DDspFilter, DDspButterworthFilter, DDspDynamics;

type
  TMultiband = record
    Lowpass      : TButterworthLP;
    LowComp      : TSimpleCompressor;
    MidHighpass  : TButterworthHP;
    MidComp      : TSimpleCompressor;
    MidLowpass   : TButterworthLP;
    Highpass     : TButterworthHP;
    HighComp     : TSimpleCompressor;
  end;

  TMBCDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
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
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: Cardinal);
  private
    fMultiband : Array [0..1] of TMultiband;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, MBCGUI;

procedure TMBCDataModule.VSTModuleCreate(Sender: TObject);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass      := TButterworthLP.Create;
    LowComp      := TSimpleCompressor.Create;
    MidHighpass  := TButterworthHP.Create;
    MidComp      := TSimpleCompressor.Create;
    MidLowpass   := TButterworthLP.Create;
    Highpass     := TButterworthHP.Create;
    HighComp     := TSimpleCompressor.Create;
   end;
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
end;

procedure TMBCDataModule.VSTModuleDestroy(Sender: TObject);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass.Free;
    LowComp.Free;
    MidHighpass.Free;
    MidComp.Free;
    MidLowpass.Free;
    Highpass.Free;
    HighComp.Free;
   end;
end;

procedure TMBCDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmMBC.Create(Self);
end;

procedure TMBCDataModule.MBCDMLowFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass.Frequency := Value;
    MidHighpass.Frequency := Value;
   end;
 if Assigned(EditorForm) then
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
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    Lowpass.Order := round(Value);
    MidHighpass.Order := round(Value);
   end;
end;

procedure TMBCDataModule.MBCDMLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i]
   do LowComp.MakeUpGaindB := Value;

 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbLowGaindB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlLowGain.Position <> Value
     then DlLowGain.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMMidGainChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i]
   do MidComp.MakeUpGaindB := Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbMidGaindB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlMidGain.Position <> Value
     then DlMidGain.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMHighGainChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i]
   do HighComp.MakeUpGaindB := Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbHighGaindB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlHighGain.Position <> Value
     then DlHighGain.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMLowThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do LowComp.Threshold := Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbLowThresholddB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlLowThreshold.Position <> Value
     then DlLowThreshold.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMMidThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Threshold := Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbMidThresholddB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlMidThreshold.Position <> Value
     then DlMidThreshold.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMHighThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Threshold := Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbHighThresholddB.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' dB';
    if DlHighThreshold.Position <> Value
     then DlHighThreshold.Position := Value;
   end;
end;

procedure TMBCDataModule.MBCDMLowRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   LowComp.Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbLowRatioValue.Caption := '1:' + FloatToStrF(Value, ffGeneral, 3, 2);
    if DlLowRatio.Position <> Log10(Value)
     then DlLowRatio.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMMidRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbMidRatioValue.Caption := '1:' + FloatToStrF(Value, ffGeneral, 3, 2);
    if DlMidRatio.Position <> Log10(Value)
     then DlMidRatio.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMHighRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbHighRatioValue.Caption := '1:' + FloatToStrF(Value, ffGeneral, 3, 2);
    if DlHighRatio.Position <> Log10(Value)
     then DlHighRatio.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMLowAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   LowComp.Attack := Value;

 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbLowAttackValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlLowAttack.Position <> Log10(Value)
     then DlLowAttack.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMMidAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Attack := Value;

 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbMidAttackValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlMidAttack.Position <> Log10(Value)
     then DlMidAttack.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMHighAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Attack := Value;

 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbHighAttackValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlHighAttack.Position <> Log10(Value)
     then DlHighAttack.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMLowReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   LowComp.Decay := Value;

 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbLowReleaseValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlLowRelease.Position <> Log10(Value)
     then DlLowRelease.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMMidReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   MidComp.Decay := Value;

 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbMidReleaseValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlMidRelease.Position <> Log10(Value)
     then DlMidRelease.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDMHighReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   HighComp.Decay := Value;

 if Assigned(EditorForm) then
  with TFmMBC(EditorForm) do
   begin
    LbHighReleaseValue.Caption := FloatToStrF(Value, ffGeneral, 3, 2) + ' ms';
    if DlHighRelease.Position <> Log10(Value)
     then DlHighRelease.Position := Log10(Value);
   end;
end;

procedure TMBCDataModule.MBCDCHighOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    MidLowpass.Order := round(Value);
    Highpass.Order := round(Value);
   end;
end;

procedure TMBCDataModule.MBCDMHighFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to 1 do
  with fMultiband[i] do
   begin
    MidLowpass.Frequency := Value;
    Highpass.Frequency := Value;
   end;
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
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
(*
   with fMultiband[0] do
    Outputs[0, i] := Lowpass.ProcessSample(Inputs[0, i]) +
                     MidHighpass.ProcessSample(Inputs[0, i]);
   with fMultiband[1] do
    Outputs[1, i] := Lowpass.ProcessSample(Inputs[1, i]) +
                     MidHighpass.ProcessSample(Inputs[1, i]);
*)
   with fMultiband[0] do
    Outputs[0, i] := LowComp.ProcessSample(Lowpass.ProcessSample(Inputs[0, i])) +
                     MidComp.ProcessSample(MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i]))) -
                     HighComp.ProcessSample(Highpass.ProcessSample(Inputs[0, i]));
   with fMultiband[1] do
    Outputs[1, i] := LowComp.ProcessSample(Lowpass.ProcessSample(Inputs[1, i])) +
                     MidComp.ProcessSample(MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i]))) -
                     HighComp.ProcessSample(Highpass.ProcessSample(Inputs[1, i]));
  end;
end;

procedure TMBCDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TArrayOfDoubleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
   with fMultiband[0] do
   Outputs[0, i] := Lowpass.ProcessSample(Inputs[0, i]) +
                    MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i])) +
                    Highpass.ProcessSample(Inputs[0, i]);
   with fMultiband[1] do
   Outputs[1, i] := Lowpass.ProcessSample(Inputs[1, i]) +
                    MidHighpass.ProcessSample(MidLowpass.ProcessSample(Inputs[0, i])) +
                    Highpass.ProcessSample(Inputs[1, i]);
  end;
end;

end.