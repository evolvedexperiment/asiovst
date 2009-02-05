unit FastGateGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGraphXY,
  DAV_GuiLED;

type
  TFmFastGate = class(TForm)
    DialAttack: TGuiDial;
    DialKnee: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    GuiDialImageList: TGuiDialImageList;
    GuiGraphXY: TGuiGraphXY;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbRatio: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateThreshold;
    procedure UpdateRatio;
    procedure UpdateKnee;
    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  FastGateDM, PngImage, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmFastGate.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'GateKnob', 'PNG');
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

procedure TFmFastGate.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateRelease;
 UpdateThreshold;
 UpdateRatio;
 UpdateKnee;
end;

procedure TFmFastGate.DialAttackChange(Sender: TObject);
begin
 with TFastGateDataModule(Owner) do
  begin
   Parameter[0] := DialAttack.Position;
  end;
end;

procedure TFmFastGate.DialReleaseChange(Sender: TObject);
begin
 with TFastGateDataModule(Owner) do
  begin
   Parameter[1] := DialRelease.Position;
  end;
end;

procedure TFmFastGate.DialThresholdChange(Sender: TObject);
begin
 with TFastGateDataModule(Owner) do
  begin
   Parameter[2] := DialThreshold.Position;
  end;
end;

function TFmFastGate.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TFastGateDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmFastGate.DialRatioChange(Sender: TObject);
begin
 with TFastGateDataModule(Owner) do
  begin
   Parameter[3] := 1 / DialRatio.Position;
  end;
end;

procedure TFmFastGate.DialKneeChange(Sender: TObject);
begin
 with TFastGateDataModule(Owner) do
  begin
   Parameter[4] := DialKnee.Position;
  end;
end;

procedure TFmFastGate.UpdateAttack;
var
  Attack : Single;
begin
 with TFastGateDataModule(Owner) do
  begin
   Attack := Parameter[0];
   if Attack <> DialAttack.Position
    then DialAttack.Position := Attack;
   LbAttackValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmFastGate.UpdateRelease;
var
  Release : Single;
begin
 with TFastGateDataModule(Owner) do
  begin
   Release := Parameter[1];
   if Release <> DialRelease.Position
    then DialRelease.Position := Release;
   LbReleaseValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmFastGate.UpdateKnee;
var
  Knee : Single;
begin
 with TFastGateDataModule(Owner) do
  begin
   Knee := Parameter[4];
   if Knee <> DialKnee.Position
    then DialKnee.Position := Knee;
   LbKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmFastGate.UpdateRatio;
var
  Ratio : Single;
begin
 with TFastGateDataModule(Owner) do
  begin
   Ratio := 1 / Parameter[3];
   if Ratio <> DialRatio.Position
    then DialRatio.Position := Ratio;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Ratio, ffGeneral, 3, 3);
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmFastGate.UpdateThreshold;
var
  Threshold : Single;
begin
 with TFastGateDataModule(Owner) do
  begin
   Threshold := Parameter[2];
   if Threshold <> DialThreshold.Position
    then DialThreshold.Position := Threshold;
   LbThresholdValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiGraphXY.UpdateGraph;
  end;
end;

end.