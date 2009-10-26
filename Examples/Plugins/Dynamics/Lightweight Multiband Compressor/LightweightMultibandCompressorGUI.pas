unit LightweightMultibandCompressorGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGraphXY,
  DAV_GuiLED, ExtCtrls, DAV_GuiPanel;

type
  TGraph = (gLow, gLowMid, gHighMid, gHigh);
  TGraphs = set of TGraph;
  TFmLightweightMultibandCompressor = class(TForm)
    DialLowAttack: TGuiDial;
    DialLowKnee: TGuiDial;
    DialLowMakeUpGain: TGuiDial;
    DialLowRatio: TGuiDial;
    DialLowRelease: TGuiDial;
    DialLowThreshold: TGuiDial;
    GXYLow: TGuiGraphXY;
    LbLowAttack: TGuiLabel;
    LbLowAttackValue: TGuiLabel;
    LbLowAutogain: TGuiLabel;
    LbLowKnee: TGuiLabel;
    LbLowKneeValue: TGuiLabel;
    LbLowMakeUpGain: TGuiLabel;
    LbLowMakeUpGainValue: TGuiLabel;
    LbLowRatio: TGuiLabel;
    LbLowRatioValue: TGuiLabel;
    LbLowRelease: TGuiLabel;
    LbLowReleaseValue: TGuiLabel;
    LbLowThreshold: TGuiLabel;
    LbLowThresholdValue: TGuiLabel;
    PnLowBand: TGuiPanel;
    DialLowMidAttack: TGuiDial;
    DialLowMidKnee: TGuiDial;
    DialLowMidMakeUpGain: TGuiDial;
    DialLowMidRatio: TGuiDial;
    DialLowMidRelease: TGuiDial;
    DialLowMidThreshold: TGuiDial;
    GXYLowMid: TGuiGraphXY;
    LbLowMidAttack: TGuiLabel;
    LbLowMidAttackValue: TGuiLabel;
    LbLowMidAutogain: TGuiLabel;
    LbLowMidKnee: TGuiLabel;
    LbLowMidKneeValue: TGuiLabel;
    LbLowMidMakeUpGain: TGuiLabel;
    LbLowMidMakeUpGainValue: TGuiLabel;
    LbLowMidRatio: TGuiLabel;
    LbLowMidRatioValue: TGuiLabel;
    LbLowMidRelease: TGuiLabel;
    LbLowMidReleaseValue: TGuiLabel;
    LbLowMidThreshold: TGuiLabel;
    LbLowMidThresholdValue: TGuiLabel;
    PnLowMidBand: TGuiPanel;
    DialHighMidAttack: TGuiDial;
    DialHighMidKnee: TGuiDial;
    DialHighMidMakeUpGain: TGuiDial;
    DialHighMidRatio: TGuiDial;
    DialHighMidRelease: TGuiDial;
    DialHighMidThreshold: TGuiDial;
    GXYHighMid: TGuiGraphXY;
    LbHighMidAttack: TGuiLabel;
    LbHighMidAttackValue: TGuiLabel;
    LbHighMidAutogain: TGuiLabel;
    LbHighMidKnee: TGuiLabel;
    LbHighMidKneeValue: TGuiLabel;
    LbHighMidMakeUpGain: TGuiLabel;
    LbHighMidMakeUpGainValue: TGuiLabel;
    LbHighMidRatio: TGuiLabel;
    LbHighMidRatioValue: TGuiLabel;
    LbHighMidRelease: TGuiLabel;
    LbHighMidReleaseValue: TGuiLabel;
    LbHighMidThreshold: TGuiLabel;
    LbHighMidThresholdValue: TGuiLabel;
    PnHighMidBand: TGuiPanel;
    DialHighAttack: TGuiDial;
    DialHighKnee: TGuiDial;
    DialHighMakeUpGain: TGuiDial;
    DialHighRatio: TGuiDial;
    DialHighRelease: TGuiDial;
    DialHighThreshold: TGuiDial;
    GXYHigh: TGuiGraphXY;
    LbHighAttack: TGuiLabel;
    LbHighAttackValue: TGuiLabel;
    LbHighAutogain: TGuiLabel;
    LbHighKnee: TGuiLabel;
    LbHighKneeValue: TGuiLabel;
    LbHighMakeUpGain: TGuiLabel;
    LbHighMakeUpGainValue: TGuiLabel;
    LbHighRatio: TGuiLabel;
    LbHighRatioValue: TGuiLabel;
    LbHighRelease: TGuiLabel;
    LbHighReleaseValue: TGuiLabel;
    LbHighThreshold: TGuiLabel;
    LbHighThresholdValue: TGuiLabel;
    PnHighBand: TGuiPanel;
    GuiDialImageList: TGuiDialImageList;
    LbSoftClip: TGuiLabel;
    LEDSoftClip: TGuiLED;
    LEDLowAutoGain: TGuiLED;
    LEDLowMidAutoGain: TGuiLED;
    LEDHighMidAutoGain: TGuiLED;
    LEDHighAutoGain: TGuiLED;
    LbLow: TGuiLabel;
    LbLowMid: TGuiLabel;
    LbHighMid: TGuiLabel;
    LbHigh: TGuiLabel;
    DialLowFreq: TGuiDial;
    LbLowFreqValue: TGuiLabel;
    DialMidFreq: TGuiDial;
    LbMidFreqValue: TGuiLabel;
    DialHighFreq: TGuiDial;
    LbHighFreqValue: TGuiLabel;
    Timer: TTimer;
    LbLowS: TGuiLabel;
    LbLowMute: TGuiLabel;
    LbLowBypass: TGuiLabel;
    LbLowMidSolo: TGuiLabel;
    LbLowMidMute: TGuiLabel;
    LbLowMidBypass: TGuiLabel;
    LbHighMidSolo: TGuiLabel;
    LbHighMidMute: TGuiLabel;
    LbHighMidBypass: TGuiLabel;
    LbHighSolo: TGuiLabel;
    LbHighMute: TGuiLabel;
    LbHighBypass: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialMakeUpGainChange(Sender: TObject);
    procedure LEDSoftClipClick(Sender: TObject);
    procedure LEDLowAutoGainClick(Sender: TObject);
    procedure DialLowFreqChange(Sender: TObject);
    procedure DialMidFreqChange(Sender: TObject);
    procedure DialHighFreqChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure LEDHighMidAutoGainClick(Sender: TObject);
    procedure LEDHighAutoGainClick(Sender: TObject);
    procedure LEDLowMidAutoGainClick(Sender: TObject);
  private
    function EvaluateHighCharacteristic(Sender: TObject; X: Double): Double;
    function EvaluateLowMidCharacteristic(Sender: TObject; X: Double): Double;
    function EvaluateHighMidCharacteristic(Sender: TObject; X: Double): Double;
    function EvaluateLowCharacteristic(Sender: TObject; X: Double): Double;
  private
    FGraphNeedUpdate : TGraphs;
    procedure SetGraphNeedUpdate(const Value: TGraphs);
  public
    procedure UpdateLimit;
    procedure UpdateLowAttack;
    procedure UpdateLowAutoMakeUpGain;
    procedure UpdateLowFrequency;
    procedure UpdateLowKnee;
    procedure UpdateLowMakeUp;
    procedure UpdateLowRatio;
    procedure UpdateLowRelease;
    procedure UpdateLowThreshold;
    procedure UpdateLowMidAttack;
    procedure UpdateLowMidAutoMakeUpGain;
    procedure UpdateLowMidKnee;
    procedure UpdateLowMidMakeUp;
    procedure UpdateLowMidRatio;
    procedure UpdateLowMidRelease;
    procedure UpdateLowMidThreshold;
    procedure UpdateMidFrequency;
    procedure UpdateHighMidAttack;
    procedure UpdateHighMidAutoMakeUpGain;
    procedure UpdateHighMidKnee;
    procedure UpdateHighMidMakeUp;
    procedure UpdateHighMidRatio;
    procedure UpdateHighMidRelease;
    procedure UpdateHighMidThreshold;
    procedure UpdateHighAttack;
    procedure UpdateHighAutoMakeUpGain;
    procedure UpdateHighFrequency;
    procedure UpdateHighKnee;
    procedure UpdateHighMakeUp;
    procedure UpdateHighRatio;
    procedure UpdateHighRelease;
    procedure UpdateHighThreshold;
    property GraphNeedUpdate: TGraphs read FGraphNeedUpdate write SetGraphNeedUpdate;
  end;

implementation

uses
  PngImage, DAV_Common, DAV_VSTModuleWithPrograms,
  LightweightMultibandCompressorDM;

{$R *.DFM}

procedure TFmLightweightMultibandCompressor.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'MultibandCompressorKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[0].DialBitmap do
    begin
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;

   DialLowFreq.DialImageIndex := 0;
   DialMidFreq.DialImageIndex := 0;
   DialHighFreq.DialImageIndex := 0;

   DialLowThreshold.DialImageIndex := 0;
   DialLowKnee.DialImageIndex := 0;
   DialLowRatio.DialImageIndex := 0;
   DialLowAttack.DialImageIndex := 0;
   DialLowRelease.DialImageIndex := 0;
   DialLowMakeUpGain.DialImageIndex := 0;

   DialLowMidThreshold.DialImageIndex := 0;
   DialLowMidKnee.DialImageIndex := 0;
   DialLowMidRatio.DialImageIndex := 0;
   DialLowMidAttack.DialImageIndex := 0;
   DialLowMidRelease.DialImageIndex := 0;
   DialLowMidMakeUpGain.DialImageIndex := 0;

   DialHighMidThreshold.DialImageIndex := 0;
   DialHighMidKnee.DialImageIndex := 0;
   DialHighMidRatio.DialImageIndex := 0;
   DialHighMidAttack.DialImageIndex := 0;
   DialHighMidRelease.DialImageIndex := 0;
   DialHighMidMakeUpGain.DialImageIndex := 0;

   DialHighThreshold.DialImageIndex := 0;
   DialHighKnee.DialImageIndex := 0;
   DialHighRatio.DialImageIndex := 0;
   DialHighAttack.DialImageIndex := 0;
   DialHighRelease.DialImageIndex := 0;
   DialHighMakeUpGain.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  try
   FreeAndNil(PngBmp);
  except
  end;
 end;
 with TGuiGraphXYFunctionSeries(GXYLow[0].Series) do
  begin
   OnEvaluate := EvaluateLowCharacteristic;
  end;
 with TGuiGraphXYFunctionSeries(GXYLowMid[0].Series) do
  begin
   OnEvaluate := EvaluateLowMidCharacteristic;
  end;
 with TGuiGraphXYFunctionSeries(GXYHighMid[0].Series) do
  begin
   OnEvaluate := EvaluateHighMidCharacteristic;
  end;
 with TGuiGraphXYFunctionSeries(GXYHigh[0].Series) do
  begin
   OnEvaluate := EvaluateHighCharacteristic;
  end;
end;

procedure TFmLightweightMultibandCompressor.FormShow(Sender: TObject);
begin
 UpdateLowAttack;
 UpdateLowAutoMakeUpGain;
 UpdateLowKnee;
 UpdateLowMakeUp;
 UpdateLowRatio;
 UpdateLowRelease;
 UpdateLowThreshold;
 UpdateLowMidAttack;
 UpdateLowMidAutoMakeUpGain;
 UpdateLowMidKnee;
 UpdateLowMidMakeUp;
 UpdateLowMidRatio;
 UpdateLowMidRelease;
 UpdateLowMidThreshold;
 UpdateHighMidAttack;
 UpdateHighMidAutoMakeUpGain;
 UpdateHighMidKnee;
 UpdateHighMidMakeUp;
 UpdateHighMidRatio;
 UpdateHighMidRelease;
 UpdateHighMidThreshold;
 UpdateHighAttack;
 UpdateHighAutoMakeUpGain;
 UpdateHighKnee;
 UpdateHighMakeUp;
 UpdateHighRatio;
 UpdateHighRelease;
 UpdateHighThreshold;
 UpdateLimit;
end;

function TFmLightweightMultibandCompressor.EvaluateLowCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateLowCharacteristic(X);
end;

function TFmLightweightMultibandCompressor.EvaluateLowMidCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateLowMidCharacteristic(X);
end;

function TFmLightweightMultibandCompressor.EvaluateHighMidCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateHighMidCharacteristic(X);
end;

function TFmLightweightMultibandCompressor.EvaluateHighCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightMultibandCompressorDataModule(Owner).EvaluateHighCharacteristic(X);
end;

// Low Frequency

procedure TFmLightweightMultibandCompressor.DialLowFreqChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialLowFreq.Position;
  end;
end;

// Mid Frequency

procedure TFmLightweightMultibandCompressor.DialMidFreqChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[1] := DialMidFreq.Position;
  end;
end;

// High Frequency

procedure TFmLightweightMultibandCompressor.DialHighFreqChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[2] := DialHighFreq.Position;
  end;
end;

// Soft Clip

procedure TFmLightweightMultibandCompressor.LEDSoftClipClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Parameter[3] := Integer(LEDSoftClip.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightMultibandCompressor.SetGraphNeedUpdate(const Value: TGraphs);
begin
 if FGraphNeedUpdate <> Value then
  begin
   FGraphNeedUpdate := Value;
   Timer.Enabled := FGraphNeedUpdate <> [];
  end;
end;

procedure TFmLightweightMultibandCompressor.TimerTimer(Sender: TObject);
begin
 if gLow in GraphNeedUpdate then GXYLow.UpdateGraph;
 if gLowMid in GraphNeedUpdate then GXYLowMid.UpdateGraph;
 if gHighMid in GraphNeedUpdate then GXYHighMid.UpdateGraph;
 if gHigh in GraphNeedUpdate then GXYHigh.UpdateGraph;
 GraphNeedUpdate := [];
end;

procedure TFmLightweightMultibandCompressor.DialAttackChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiDial(Sender) do
  begin
   Parameter[4 + Tag * 7] := Position;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialReleaseChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiDial(Sender) do
  begin
   Parameter[5 + Tag * 7] := Position;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialThresholdChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiDial(Sender) do
  begin
   Parameter[6 + Tag * 7] := Position;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialRatioChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiDial(Sender) do
  begin
   Parameter[7 + Tag * 7] := Position;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialKneeChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiDial(Sender) do
  begin
   Parameter[8 + Tag * 7] := Position;
  end;
end;

procedure TFmLightweightMultibandCompressor.DialMakeUpGainChange(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner), TGuiDial(Sender) do
  begin
   Parameter[9 + Tag * 7] := Position;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDLowAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[0] := LEDLowAutoGain.Brightness_Percent < 50;
   if not DialLowMakeUpGain.Enabled then UpdateLowMakeUp;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDLowMidAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[1] := LEDLowMidAutoGain.Brightness_Percent < 50;
   if not DialLowMidMakeUpGain.Enabled then UpdateLowMidMakeUp;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDHighMidAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[2] := LEDHighMidAutoGain.Brightness_Percent < 50;
   if not DialHighMidMakeUpGain.Enabled then UpdateHighMidMakeUp;
  end;
end;

procedure TFmLightweightMultibandCompressor.LEDHighAutoGainClick(Sender: TObject);
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   AutoGain[3] := LEDHighAutoGain.Brightness_Percent < 50;
   if not DialHighMakeUpGain.Enabled then UpdateHighMakeUp;
  end;
end;

////////////
// Update //
////////////

procedure TFmLightweightMultibandCompressor.UpdateLowFrequency;
var
  Freq : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Freq := Parameter[0];
   if Freq <> DialLowFreq.Position
    then DialLowFreq.Position := Freq;
   DialMidFreq.Min := DialLowFreq.Position;
   LbLowFreqValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateMidFrequency;
var
  Freq : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Freq := Parameter[1];
   if Freq <> DialMidFreq.Position
    then DialMidFreq.Position := Freq;
   DialHighFreq.Min := DialMidFreq.Position;
   DialLowFreq.Max := DialMidFreq.Position;
   LbMidFreqValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighFrequency;
var
  Freq : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Freq := Parameter[2];
   if Freq <> DialHighFreq.Position
    then DialHighFreq.Position := Freq;
   DialMidFreq.Max := DialHighFreq.Position;
   LbHighFreqValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[4];
   if Attack <> DialLowAttack.Position
    then DialLowAttack.Position := Attack;
   LbLowAttackValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[5];
   if Release <> DialLowRelease.Position
    then DialLowRelease.Position := Release;
   LbLowReleaseValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[6];
   if Threshold <> DialLowThreshold.Position
    then DialLowThreshold.Position := Threshold;
   LbLowThresholdValue.Caption := ParameterDisplay[6] + ' ' + ParameterLabel[6];
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[7];
   if Ratio <> DialLowRatio.Position
    then DialLowRatio.Position := Ratio;
   LbLowRatioValue.Caption := '1 : ' + ParameterDisplay[7];
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[8];
   if Knee <> DialLowKnee.Position
    then DialLowKnee.Position := Knee;
   LbLowKneeValue.Caption := ParameterDisplay[8] + ' ' + ParameterLabel[8];
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[0].MakeUpGain_dB;
   if MakeUp <> DialLowMakeUpGain.Position
    then DialLowMakeUpGain.Position := MakeUp;
   LbLowMakeUpGainValue.Caption := ParameterDisplay[9] + ' ' + ParameterLabel[9];
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[0])), 10, 100);
   if Brightness <> LEDLowAutoGain.Brightness_Percent
    then LEDLowAutoGain.Brightness_Percent := Brightness;
   DialLowMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gLow];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[11];
   if Attack <> DialLowMidAttack.Position
    then DialLowMidAttack.Position := Attack;
   LbLowMidAttackValue.Caption := ParameterDisplay[11] + ' ' + ParameterLabel[11];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[12];
   if Release <> DialLowMidRelease.Position
    then DialLowMidRelease.Position := Release;
   LbLowMidReleaseValue.Caption := ParameterDisplay[12] + ' ' + ParameterLabel[12];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[13];
   if Threshold <> DialLowMidThreshold.Position
    then DialLowMidThreshold.Position := Threshold;
   LbLowMidThresholdValue.Caption := ParameterDisplay[13] + ' ' + ParameterLabel[13];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[14];
   if Ratio <> DialLowMidRatio.Position
    then DialLowMidRatio.Position := Ratio;
   LbLowMidRatioValue.Caption := '1 : ' + ParameterDisplay[14];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[15];
   if Knee <> DialLowMidKnee.Position
    then DialLowMidKnee.Position := Knee;
   LbLowMidKneeValue.Caption := ParameterDisplay[15] + ' ' + ParameterLabel[15];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[1].MakeUpGain_dB;
   if MakeUp <> DialLowMidMakeUpGain.Position
    then DialLowMidMakeUpGain.Position := MakeUp;
   LbLowMidMakeUpGainValue.Caption := ParameterDisplay[16] + ' ' + ParameterLabel[16];
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLowMidAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[1])), 10, 100);
   if Brightness <> LEDLowMidAutoGain.Brightness_Percent
    then LEDLowMidAutoGain.Brightness_Percent := Brightness;
   DialLowMidMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gLowMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[18];
   if Attack <> DialHighMidAttack.Position
    then DialHighMidAttack.Position := Attack;
   LbHighMidAttackValue.Caption := ParameterDisplay[18] + ' ' + ParameterLabel[18];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[19];
   if Release <> DialHighMidRelease.Position
    then DialHighMidRelease.Position := Release;
   LbHighMidReleaseValue.Caption := ParameterDisplay[19] + ' ' + ParameterLabel[19];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[20];
   if Threshold <> DialHighMidThreshold.Position
    then DialHighMidThreshold.Position := Threshold;
   LbHighMidThresholdValue.Caption := ParameterDisplay[20] + ' ' + ParameterLabel[20];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[21];
   if Ratio <> DialHighMidRatio.Position
    then DialHighMidRatio.Position := Ratio;
   LbHighMidRatioValue.Caption := '1 : ' + ParameterDisplay[21];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[22];
   if Knee <> DialHighMidKnee.Position
    then DialHighMidKnee.Position := Knee;
   LbHighMidKneeValue.Caption := ParameterDisplay[22] + ' ' + ParameterLabel[22];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[2].MakeUpGain_dB;
   if MakeUp <> DialHighMidMakeUpGain.Position
    then DialHighMidMakeUpGain.Position := MakeUp;
   LbHighMidMakeUpGainValue.Caption := ParameterDisplay[23] + ' ' + ParameterLabel[23];
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMidAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[2])), 10, 100);
   if Brightness <> LEDHighMidAutoGain.Brightness_Percent
    then LEDHighMidAutoGain.Brightness_Percent := Brightness;
   DialHighMidMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gHighMid];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighAttack;
var
  Attack : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Attack := Parameter[25];
   if Attack <> DialHighAttack.Position
    then DialHighAttack.Position := Attack;
   LbHighAttackValue.Caption := ParameterDisplay[25] + ' ' + ParameterLabel[25];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighRelease;
var
  Release : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Release := Parameter[26];
   if Release <> DialHighRelease.Position
    then DialHighRelease.Position := Release;
   LbHighReleaseValue.Caption := ParameterDisplay[26] + ' ' + ParameterLabel[26];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighThreshold;
var
  Threshold : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[27];
   if Threshold <> DialHighThreshold.Position
    then DialHighThreshold.Position := Threshold;
   LbHighThresholdValue.Caption := ParameterDisplay[27] + ' ' + ParameterLabel[27];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighRatio;
var
  Ratio : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[28];
   if Ratio <> DialHighRatio.Position
    then DialHighRatio.Position := Ratio;
   LbHighRatioValue.Caption := '1 : ' + ParameterDisplay[28];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighKnee;
var
  Knee : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Knee := Parameter[29];
   if Knee <> DialHighKnee.Position
    then DialHighKnee.Position := Knee;
   LbHighKneeValue.Caption := ParameterDisplay[29] + ' ' + ParameterLabel[29];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightMultibandCompressor[3].MakeUpGain_dB;
   if MakeUp <> DialHighMakeUpGain.Position
    then DialHighMakeUpGain.Position := MakeUp;
   LbHighMakeUpGainValue.Caption := ParameterDisplay[30] + ' ' + ParameterLabel[30];
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateHighAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := Limit(100 * (0.1 + 0.9 * Integer(AutoGain[3])), 10, 100);
   if Brightness <> LEDHighAutoGain.Brightness_Percent
    then LEDHighAutoGain.Brightness_Percent := Brightness;
   DialHighMakeUpGain.Enabled := Brightness < 50;
   GraphNeedUpdate := GraphNeedUpdate + [gHigh];
  end;
end;

procedure TFmLightweightMultibandCompressor.UpdateLimit;
var
  Brightness : Single;
begin
 with TLightweightMultibandCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[3]);
   if Brightness <> LEDSoftClip.Brightness_Percent
    then LEDSoftClip.Brightness_Percent := Brightness;
  end;
end;

end.
