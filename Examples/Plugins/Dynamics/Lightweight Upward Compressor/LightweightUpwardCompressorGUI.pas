unit LightweightUpwardCompressorGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGraphXY,
  DAV_GuiLED;

type
  TFmLightweightUpwardCompressor = class(TForm)
    DialAttack: TGuiDial;
    DialKnee: TGuiDial;
    DialMakeUpGain: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    GuiDialImageList: TGuiDialImageList;
    GuiGraphXY: TGuiGraphXY;
    GuiLabel2: TGuiLabel;
    GuiLabel3: TGuiLabel;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbMakeUpGain: TGuiLabel;
    LbMakeUpGainValue: TGuiLabel;
    LbOversample: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbStereo: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    LEDAutoGain: TGuiLED;
    LEDLimit: TGuiLED;
    LEDOversample: TGuiLED;
    LEDStereo: TGuiLED;
    procedure FormCreate(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialMakeUpGainChange(Sender: TObject);
    procedure LEDStereoClick(Sender: TObject);
    procedure LEDLimitClick(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMakeUp;
    procedure UpdateStereo;
    procedure UpdateLimit;
    procedure UpdateAutoMakeUpGain;
    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  LightweightUpwardCompressorDM, PngImage, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmLightweightUpwardCompressor.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'CompressorKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[0].DialBitmap do
    begin
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialThreshold.DialImageIndex := 0;
   DialKnee.DialImageIndex := 0;
   DialRatio.DialImageIndex := 0;
   DialAttack.DialImageIndex := 0;
   DialRelease.DialImageIndex := 0;
   DialMakeUpGain.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
 with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
  begin
   OnEvaluate := EvaluateCharacteristic;
  end;
end;

procedure TFmLightweightUpwardCompressor.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRelease;
 UpdateThreshold;
 UpdateRatio;
 UpdateKnee;
 UpdateMakeUp;
 UpdateStereo;
 UpdateLimit;
 UpdateAutoMakeUpGain;
end;

procedure TFmLightweightUpwardCompressor.LEDAutoGainClick(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[8] := Integer(LEDAutoGain.Brightness_Percent < 50);
   if not DialMakeUpGain.Enabled then UpdateMakeUp;
  end;
end;

procedure TFmLightweightUpwardCompressor.LEDLimitClick(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[7] := Integer(LEDLimit.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightUpwardCompressor.LEDStereoClick(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[6] := Integer(LEDStereo.Brightness_Percent < 50);
  end;
end;

procedure TFmLightweightUpwardCompressor.DialAttackChange(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[0] := DialAttack.Position;
  end;
end;

procedure TFmLightweightUpwardCompressor.DialReleaseChange(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[1] := DialRelease.Position;
  end;
end;

procedure TFmLightweightUpwardCompressor.DialThresholdChange(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[2] := DialThreshold.Position;
  end;
end;

function TFmLightweightUpwardCompressor.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TLightweightUpwardCompressorDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmLightweightUpwardCompressor.DialRatioChange(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[3] := DialRatio.Position;
  end;
end;

procedure TFmLightweightUpwardCompressor.DialKneeChange(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[4] := DialKnee.Position;
  end;
end;

procedure TFmLightweightUpwardCompressor.DialMakeUpGainChange(Sender: TObject);
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Parameter[5] := DialMakeUpGain.Position;
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateAttack;
var
  Attack : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Attack := Parameter[0];
   if Attack <> DialAttack.Position
    then DialAttack.Position := Attack;
   LbAttackValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateRelease;
var
  Release : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Release := Parameter[1];
   if Release <> DialRelease.Position
    then DialRelease.Position := Release;
   LbReleaseValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateKnee;
var
  Knee : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Knee := Parameter[4];
   if Knee <> DialKnee.Position
    then DialKnee.Position := Knee;
   LbKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateMakeUp;
var
  MakeUp : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   MakeUp := LightweightUpwardCompressor[0].MakeUpGain_dB;
   if MakeUp <> DialMakeUpGain.Position
    then DialMakeUpGain.Position := MakeUp;
   LbMakeUpGainValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateRatio;
var
  Ratio : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[3];
   if Ratio <> DialRatio.Position
    then DialRatio.Position := Ratio;
   LbRatioValue.Caption := ParameterDisplay[3] + ' : 1';
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateThreshold;
var
  Threshold : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[2];
   if Threshold <> DialThreshold.Position
    then DialThreshold.Position := Threshold;
   LbThresholdValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateStereo;
var
  Brightness : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[6]);
   if Brightness <> LEDStereo.Brightness_Percent
    then LEDStereo.Brightness_Percent := Brightness;
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[8]);
   if Brightness <> LEDAutoGain.Brightness_Percent
    then LEDAutoGain.Brightness_Percent := Brightness;
   DialMakeUpGain.Enabled := Brightness < 50;
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmLightweightUpwardCompressor.UpdateLimit;
var
  Brightness : Single;
begin
 with TLightweightUpwardCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.1 + 0.9 * Parameter[7]);
   if Brightness <> LEDLimit.Brightness_Percent
    then LEDLimit.Brightness_Percent := Brightness;
  end;
end;

end.
