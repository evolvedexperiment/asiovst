unit BassExtenderDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPDynamics, DAV_DSPFrequencyDivider, DAV_DspButterworthFilter;

type
  TBassExtenderModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcess64(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMS32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMS64(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessLight32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessLight64(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessLightMS32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessLightMS64(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParamAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamBalanceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamCompressionMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDividerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamReleaseLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamSplitOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFreqDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamFreqLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamModeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    fLowpass       : Array [0..1, 0..1] of TButterworthLP;
    fHighpass      : Array [0..1, 0..1] of TButterworthHP;
    FSign          : Single;
    fDivideMix     : Array [0..1] of Single;
    fCompressorMix : Array [0..1] of Single;
    fBalance       : Array [0..1] of Single;
    FCompressor    : Array [0..1] of TSimpleCompressor;
    FOctaveDivider : Array [0..1] of TOcatveDivider;
  public
  end;

implementation

{$R *.DFM}

uses
  BassExtenderGUI, DAV_VSTModuleWithPrograms, DAV_VSTCustomModule;

procedure TBassExtenderModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 fDivideMix[0]     := 0.5;
 fDivideMix[1]     := 0.5;
 fCompressorMix[0] := 0.5;
 fCompressorMix[1] := 0.5;
 fBalance[0]       := 1;
 fBalance[1]       := 1;
 for ch := 0 to numInputs - 1 do
  begin
   fLowpass[ch, 0]  := TButterworthLP.Create;
   fLowpass[ch, 1]  := TButterworthLP.Create;
   fHighpass[ch, 0] := TButterworthHP.Create;
   fHighpass[ch, 1] := TButterworthHP.Create;
   fLowpass[ch, 0].SetFilterValues(80, 0);
   fLowpass[ch, 1].SetFilterValues(80, 0);
   fHighpass[ch, 0].SetFilterValues(80, 0);
   fHighpass[ch, 1].SetFilterValues(80, 0);

   FCompressor[ch]    := TSimpleCompressor.Create;
   FCompressor[ch].AutoMakeUp := True;
   FOctaveDivider[ch] := TOcatveDivider.Create;
  end;
 Parameter[ 0] := 70;    // Split Frequency [Hz]
 Parameter[ 1] := 4;     // Split Order
 Parameter[ 2] := 50;    // Divider [%]
 Parameter[ 3] := 50;    // Shape [%]
 Parameter[ 4] := -20;   // Threshold [dB]
 Parameter[ 5] := 6;     // Ratio (1 : x)
 Parameter[ 6] := 50;    // Attack [탎]
 Parameter[ 7] := 50;    // Release [ms]
 Parameter[ 8] := 50;    // Mix [%]
 Parameter[ 9] := 0;     // Balance [%]
 Parameter[10] := 3;     // Mode (0 = Stereo, 1 = Mid only, 2 = Light, 1 = Light Mid only)

 with Programs[0] do
  begin
   Parameter[ 0] := 70;    // Split Frequency [Hz]
   Parameter[ 1] := 2;     // Split Order
   Parameter[ 2] := 50;    // Divider [%]
   Parameter[ 3] := 50;    // Shape [%]
   Parameter[ 4] := -20;   // Threshold [dB]
   Parameter[ 5] := 6;     // Ratio (1 : x)
   Parameter[ 6] := 50;    // Attack [탎]
   Parameter[ 7] := 50;    // Release [ms]
   Parameter[ 8] := 50;    // Mix [%]
   Parameter[ 9] := 0;     // Balance [%]
   Parameter[10] := 3;     // Mode (0 = Stereo, 1 = Mid only, 2 = Light, 1 = Light Mid only)
  end;

 with Programs[1] do
  begin
   Parameter[ 0] := 1000;  // Split Frequency [Hz]
   Parameter[ 1] := 1;     // Split Order
   Parameter[ 2] := 0;     // Divider [%]
   Parameter[ 3] := 0;     // Shape [%]
   Parameter[ 4] := 0;     // Threshold [dB]
   Parameter[ 5] := 1;     // Ratio (1 : x)
   Parameter[ 6] := 50;    // Attack [탎]
   Parameter[ 7] := 50;    // Release [ms]
   Parameter[ 8] := 0;     // Mix [%]
   Parameter[ 9] := 0;     // Balance [%]
   Parameter[10] := 0;     // Mode (0 = Stereo, 1 = Mid only, 2 = Light, 1 = Light Mid only)
  end;

 with Programs[2] do
  begin
   Parameter[ 0] := 100;   // Split Frequency [Hz]
   Parameter[ 1] := 3;     // Split Order
   Parameter[ 2] := 30;    // Divider [%]
   Parameter[ 3] := 0;     // Shape [%]
   Parameter[ 4] := -10;   // Threshold [dB]
   Parameter[ 5] := 2;     // Ratio (1 : x)
   Parameter[ 6] := 100;   // Attack [탎]
   Parameter[ 7] := 200;   // Release [ms]
   Parameter[ 8] := 50;    // Mix [%]
   Parameter[ 9] := 0;     // Balance [%]
   Parameter[10] := 3;     // Mode (0 = Stereo, 1 = Mid only, 2 = Light, 1 = Light Mid only)
  end;

 with Programs[3] do
  begin
   Parameter[ 0] := 80;    // Split Frequency [Hz]
   Parameter[ 1] := 4;     // Split Order
   Parameter[ 2] := 60;    // Divider [%]
   Parameter[ 3] := 5;     // Shape [%]
   Parameter[ 4] := -15;   // Threshold [dB]
   Parameter[ 5] := 4;     // Ratio (1 : x)
   Parameter[ 6] := 60;    // Attack [탎]
   Parameter[ 7] := 300;   // Release [ms]
   Parameter[ 8] := 70;    // Mix [%]
   Parameter[ 9] := 5;     // Balance [%]
   Parameter[10] := 1;     // Mode (0 = Stereo, 1 = Mid only)
  end;
end;

procedure TBassExtenderModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   FreeAndNil(fLowpass[ch, 0]);
   FreeAndNil(fLowpass[ch, 1]);
   FreeAndNil(fHighpass[ch, 0]);
   FreeAndNil(fHighpass[ch, 1]);
   FreeAndNil(FCompressor[ch]);
   FreeAndNil(FOctaveDivider[ch]);
  end;
end;

procedure TBassExtenderModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmBassExtender.Create(Self);
end;

procedure TBassExtenderModule.ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch, Order : Integer;
begin
 Order := round(Value);
 for ch := 0 to 1 do
  begin
   if assigned(FLowpass[ch, 0]) then FLowpass[ch, 0].Order  := Order;
   if assigned(FLowpass[ch, 1]) then FLowpass[ch, 1].Order  := Order;
   if assigned(FHighpass[ch, 0]) then FHighpass[ch, 0].Order := Order;
   if assigned(FHighpass[ch, 1]) then FHighpass[ch, 1].Order := Order;
  end;

 FSign := 1 - 2 * (Order mod 2);

 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateSplitOrder;
   end;
end;

procedure TBassExtenderModule.ParamReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then FCompressor[0].Decay := Value;
 if assigned(FCompressor[1]) then FCompressor[1].Decay := Value;
 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateRelease;
   end;
end;

procedure TBassExtenderModule.ParamReleaseLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[index] < 1000
  then PreDefined := 'ms'
  else PreDefined := 's'
end;

procedure TBassExtenderModule.ParamAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Attack : Single;
begin
 Attack := Parameter[index];
 if Attack < 1000
  then PreDefined := FloatToStrF(Attack, ffGeneral, 3, 2)
  else PreDefined := FloatToStrF(Attack * 1E-3, ffGeneral, 3, 2);
end;

procedure TBassExtenderModule.ParamReleaseDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Release : Single;
begin
 Release := Parameter[index];
 if Release < 1000
  then PreDefined := FloatToStrF(Release, ffGeneral, 3, 2)
  else PreDefined := FloatToStrF(Release * 1E-3, ffGeneral, 3, 2);
end;

procedure TBassExtenderModule.ParamRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] = 1000
  then PreDefined := '1 : oo'
  else PreDefined := '1 : ' + FloatToStrF(Parameter[Index], ffGeneral, 3, 5);
end;

procedure TBassExtenderModule.ParamThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then FCompressor[0].Threshold := Value;
 if assigned(FCompressor[1]) then FCompressor[1].Threshold := Value;
 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateThreshold;
   end;
end;

procedure TBassExtenderModule.ParamRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then
  begin
   FCompressor[0].Ratio := 1 / Value;
   if assigned(FCompressor[1])
    then FCompressor[1].Ratio := FCompressor[0].Ratio;
  end;
 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateRatio;
   end;
end;

procedure TBassExtenderModule.ParamShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FOctaveDivider[0]) then FOctaveDivider[0].Shape := 0.01 * Value;
 if assigned(FOctaveDivider[1]) then FOctaveDivider[1].Shape := 0.01 * Value;

 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateShape;
   end;
end;

procedure TBassExtenderModule.ParamDividerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDivideMix[0] := f_Limit(0.01 * Value, 0, 1);
 fDivideMix[1] := 1 - fDivideMix[0];

 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateDivider;
   end;
end;

procedure TBassExtenderModule.ParamCompressionMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fCompressorMix[0] := f_Limit(0.01 * Value, 0, 1);
 fCompressorMix[1] := 1 - fCompressorMix[0];

 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateCompressionMix;
   end;
end;

procedure TBassExtenderModule.ParamBalanceChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fBalance[1] := f_Limit(1 + 0.01 * Value, 0, 2);
 fBalance[0] := 2 - fBalance[1];

 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateBalance;
   end;
end;

procedure TBassExtenderModule.ParamSplitOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(round(2 * Parameter[Index]));
end;

procedure TBassExtenderModule.ParamFreqDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq < 1000
  then Predefined := FloatToStrF(Freq, ffGeneral, 4, 4)
  else Predefined := FloatToStrF(Freq * 1E-3, ffGeneral, 4, 4);
end;

procedure TBassExtenderModule.ParamFreqLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1000
  then Predefined := 'Hz'
  else Predefined := 'kHz'
end;

procedure TBassExtenderModule.ParamModeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case round(Parameter[Index]) of
  0: begin
      OnProcess := VSTModuleProcess32;
      OnProcessReplacing := VSTModuleProcess32;
      OnProcessDoubleReplacing := VSTModuleProcess64;
     end;
  1: begin
      OnProcess := VSTModuleProcessMS32;
      OnProcessReplacing := VSTModuleProcessMS32;
      OnProcessDoubleReplacing := VSTModuleProcessMS64;
     end;
  2: begin
      OnProcess := VSTModuleProcessLight32;
      OnProcessReplacing := VSTModuleProcessLight32;
      OnProcessDoubleReplacing := VSTModuleProcessLight64;
     end;
  3: begin
      OnProcess := VSTModuleProcessLightMS32;
      OnProcessReplacing := VSTModuleProcessLightMS32;
      OnProcessDoubleReplacing := VSTModuleProcessLightMS64;
     end;
 end;
end;

procedure TBassExtenderModule.ParamModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0: PreDefined := 'Stereo';
  1: PreDefined := 'Mid only';
  2: PreDefined := 'Light';
  3: PreDefined := 'Light Mid Only';
 end;
end;

procedure TBassExtenderModule.ParamAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FCompressor[0]) then FCompressor[0].Attack := 1E-3 * Value;
 if assigned(FCompressor[1]) then FCompressor[1].Attack := 1E-3 * Value;

 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateAttack;
   end;
end;

procedure TBassExtenderModule.ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[index] < 1000
  then PreDefined := '탎'
  else PreDefined := 'ms'
end;

procedure TBassExtenderModule.ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  begin
   if assigned(FLowpass[ch, 0]) then FLowpass[ch, 0].Frequency  := Value;
   if assigned(FLowpass[ch, 1]) then FLowpass[ch, 1].Frequency  := Value;
   if assigned(FHighpass[ch, 0]) then FHighpass[ch, 0].Frequency := Value;
   if assigned(FHighpass[ch, 1]) then FHighpass[ch, 1].Frequency := Value;
  end;

 if EditorForm is TFmBassExtender then
  with TFmBassExtender(EditorForm) do
   begin
    UpdateSplitFrequency;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcess32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, ch : Integer;
  L, H       : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    L := cDenorm64 + Inputs[ch, Sample];
    H := fHighpass[ch, 0].ProcessSample(cDenorm64 +
         fHighpass[ch, 1].ProcessSample(FSign * L));
    L := fLowpass[ch, 1].ProcessSample(L);
    L := fLowpass[ch, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[ch].ProcessSample(L) +
                                       fDivideMix[1] * L);
    FCompressor[ch].InputSideChain(L);
    Outputs[ch, Sample] := fBalance[0] * (fCompressorMix[0] * FCompressor[ch].ProcessSample(L) +
                           fCompressorMix[1] * L) + fBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcess64(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample, ch : Integer;
  L, H       : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    L := cDenorm64 + Inputs[ch, Sample];
    H := fHighpass[ch, 0].ProcessSample(cDenorm64 +
         fHighpass[ch, 1].ProcessSample(FSign * L));
    L := fLowpass[ch, 1].ProcessSample(L);
    L := fLowpass[ch, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[ch].ProcessSample(L) +
                                       fDivideMix[1] * L);
    FCompressor[ch].InputSideChain(L);
    Outputs[ch, Sample] := fBalance[0] * (fCompressorMix[0] * FCompressor[ch].ProcessSample(L) +
                           fCompressorMix[1] * L) + fBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcessMS32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample     : Integer;
  L, H, M, S : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // Mid
   L := cDenorm64 + Inputs[0, Sample] + Inputs[1, Sample];
   H := fHighpass[0, 0].ProcessSample(cDenorm64 +
        fHighpass[0, 1].ProcessSample(FSign * L));
   L := fLowpass[0, 1].ProcessSample(L);
   L := fLowpass[0, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[0].ProcessSample(L) +
                                      fDivideMix[1] * L);
   FCompressor[0].InputSideChain(L);
   M := fBalance[0] * (fCompressorMix[0] * FCompressor[0].ProcessSample(L) +
                       fCompressorMix[1] * L) + fBalance[1] * H;

   // Side
   L := Inputs[0, Sample] - Inputs[1, Sample];
   H := fHighpass[1, 0].ProcessSample(
        fHighpass[1, 1].ProcessSample(FSign * L));
   L := fLowpass[1, 0].ProcessSample(
        fLowpass[1, 1].ProcessSample(L));
   S := fBalance[0] * L + fBalance[1] * H;

   Outputs[0, Sample] := 0.5 * (M + S);
   Outputs[1, Sample] := 0.5 * (M - S);
  end;
end;

procedure TBassExtenderModule.VSTModuleProcessMS64(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample     : Integer;
  L, H, M, S : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // Mid
   L := cDenorm64 + Inputs[0, Sample] + Inputs[1, Sample];
   H := fHighpass[0, 0].ProcessSample(cDenorm64 + 
        fHighpass[0, 1].ProcessSample(FSign * L));
   L := fLowpass[0, 1].ProcessSample(L);
   L := fLowpass[0, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[0].ProcessSample(L) +
                                      fDivideMix[1] * L);
   FCompressor[0].InputSideChain(L);
   M := fBalance[0] * (fCompressorMix[0] * FCompressor[0].ProcessSample(L) +
                       fCompressorMix[1] * L) + fBalance[1] * H;

   // Side
   L := Inputs[0, Sample] - Inputs[1, Sample];
   H := fHighpass[1, 0].ProcessSample(
        fHighpass[1, 1].ProcessSample(FSign * L));
   L := fLowpass[1, 0].ProcessSample(
        fLowpass[1, 1].ProcessSample(L));
   S := fBalance[0] * L + fBalance[1] * H;

   Outputs[0, Sample] := 0.5 * (M + S);
   Outputs[1, Sample] := 0.5 * (M - S);
  end;
end;

procedure TBassExtenderModule.VSTModuleProcessLight32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, ch : Integer;
  L, H       : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    L := fLowpass[ch, 1].ProcessSample(cDenorm64 + Inputs[ch, Sample]);
    H := fHighpass[ch, 1].ProcessSample(Inputs[ch, Sample] - L);
    L := fLowpass[ch, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[ch].ProcessSample(L) +
                                       fDivideMix[1] * L);
    FCompressor[ch].InputSideChain(L);
    Outputs[ch, Sample] := fBalance[0] * (fCompressorMix[0] * FCompressor[ch].ProcessSample(L) +
                           fCompressorMix[1] * L) + fBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcessLight64(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample, ch : Integer;
  L, H       : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  for ch := 0 to 1 do
   begin
    L := fLowpass[ch, 1].ProcessSample(cDenorm64 + Inputs[ch, Sample]);
    H := fHighpass[ch, 1].ProcessSample(Inputs[ch, Sample] - L);
    L := fLowpass[ch, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[ch].ProcessSample(L) +
                                       fDivideMix[1] * L);
    FCompressor[ch].InputSideChain(L);
    Outputs[ch, Sample] := fBalance[0] * (fCompressorMix[0] * FCompressor[ch].ProcessSample(L) +
                           fCompressorMix[1] * L) + fBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcessLightMS32(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample     : Integer;
  L, H, M, S : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // Mid
   M := cDenorm64 + Inputs[0, Sample] + Inputs[1, Sample];
   L := fLowpass[0, 1].ProcessSample(M);
   H := fHighpass[0, 1].ProcessSample(M - L);
   L := fLowpass[0, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[0].ProcessSample(L) +
                                     fDivideMix[1] * L);

   FCompressor[0].InputSideChain(L);
   M := fBalance[0] * (fCompressorMix[0] * FCompressor[0].ProcessSample(L) +
                       fCompressorMix[1] * L) + fBalance[1] * H;

   // Side
   L := Inputs[0, Sample] - Inputs[1, Sample];
   H := fHighpass[1, 0].ProcessSample(
        fHighpass[1, 1].ProcessSample(FSign * L));
   L := fLowpass[1, 0].ProcessSample(
        fLowpass[1, 1].ProcessSample(L));
   S := fBalance[0] * L + fBalance[1] * H;

   Outputs[0, Sample] := 0.5 * (M + S);
   Outputs[1, Sample] := 0.5 * (M - S);
  end;
end;

procedure TBassExtenderModule.VSTModuleProcessLightMS64(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample     : Integer;
  L, H, M, S : Double;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // Mid
   M := cDenorm64 + Inputs[0, Sample] + Inputs[1, Sample];
   L := fLowpass[0, 1].ProcessSample(M);
   H := fHighpass[0, 1].ProcessSample(M - L);
   L := fLowpass[0, 0].ProcessSample(fDivideMix[0] * FOctaveDivider[0].ProcessSample(L) +
                                     fDivideMix[1] * L);

   FCompressor[0].InputSideChain(L);
   M := fBalance[0] * (fCompressorMix[0] * FCompressor[0].ProcessSample(L) +
                       fCompressorMix[1] * L) + fBalance[1] * H;

   // Side
   L := Inputs[0, Sample] - Inputs[1, Sample];
   H := fHighpass[1, 0].ProcessSample(
        fHighpass[1, 1].ProcessSample(FSign * L));
   L := fLowpass[1, 0].ProcessSample(
        fLowpass[1, 1].ProcessSample(L));
   S := fBalance[0] * L + fBalance[1] * H;

   Outputs[0, Sample] := 0.5 * (M + S);
   Outputs[1, Sample] := 0.5 * (M - S);
  end;
end;

end.
