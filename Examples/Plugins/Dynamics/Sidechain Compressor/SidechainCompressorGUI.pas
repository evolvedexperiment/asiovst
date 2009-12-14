unit SidechainCompressorGUI;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, Dialogs,
  DAV_Types, DAV_VSTModule, DAV_VstHost, DAV_GuiLabel, DAV_GuiBaseControl,
  DAV_GuiGraphXY, DAV_GuiLED, DAV_GuiSlider;

type
  TFmSidechainCompressor = class(TForm)
    PnTitle: TPanel;
    LbTitle: TGuiLabel;
    LbAttack: TGuiLabel;
    LbRelease: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbKnee: TGuiLabel;
    LbRatio: TGuiLabel;
    SliderAttack: TGuiSlider;
    SliderRelease: TGuiSlider;
    SliderThreshold: TGuiSlider;
    SliderKnee: TGuiSlider;
    SliderRatio: TGuiSlider;
    LbAttackValue: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    LbKneeValue: TGuiLabel;
    LbRatioValue: TGuiLabel;
    LbSidechainVstPlugin: TGuiLabel;
    LbVstPluginValue: TGuiLabel;
    LbMakupGain: TGuiLabel;
    SliderMakeUpGain: TGuiSlider;
    LbMakeUpGainValue: TGuiLabel;
    LbAutoMakeUpGain: TGuiLabel;
    LEDAutoGain: TGuiLED;
    LbSoftClip: TGuiLabel;
    LEDSoftClip: TGuiLED;
    LbStereo: TGuiLabel;
    LEDStereo: TGuiLED;
    GuiGraphXY: TGuiGraphXY;
    procedure SliderAttackChange(Sender: TObject);
    procedure SliderReleaseChange(Sender: TObject);
    procedure SliderThresholdChange(Sender: TObject);
    procedure SliderRatioChange(Sender: TObject);
    procedure SliderKneeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SliderMakeUpGainChange(Sender: TObject);
    procedure LEDStereoClick(Sender: TObject);
    procedure LEDSoftClipClick(Sender: TObject);
    procedure LEDAutoGainClick(Sender: TObject);
    procedure LbVstPluginValueDblClick(Sender: TObject);
    procedure LbVstPluginValueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    procedure UpdateVstPlugin;
    function EvaluateCharacteristic(Sender: TObject; X: Double): Double;
  end;

implementation

uses
  SidechainCompressorDM, PngImage, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmSidechainCompressor.FormCreate(Sender: TObject);
begin
 with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
  begin
   OnEvaluate := EvaluateCharacteristic;
  end;
end;

procedure TFmSidechainCompressor.FormShow(Sender: TObject);
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

procedure TFmSidechainCompressor.LbVstPluginValueClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   if VstPlugIn.Active then
    begin
     if VstPlugIn.EditVisible
      then VstPlugIn.GUIControl.BringToFront
      else VstPlugIn.ShowEdit;
    end;
  end;
end;

procedure TFmSidechainCompressor.LbVstPluginValueDblClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner), TOpenDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   Filter := 'VST plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select a VST Plugin';
   if Execute
    then LoadVSTPlugin(FileName);
  finally
   Free;
  end;
end;

procedure TFmSidechainCompressor.LEDAutoGainClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[8] := Integer(LEDAutoGain.Brightness_Percent < 50);
   if not SliderMakeUpGain.Enabled then UpdateMakeUp;
  end;
end;

procedure TFmSidechainCompressor.LEDSoftClipClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[7] := Integer(LEDSoftClip.Brightness_Percent < 50);
  end;
end;

procedure TFmSidechainCompressor.LEDStereoClick(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[6] := Integer(LEDStereo.Brightness_Percent < 50);
  end;
end;

procedure TFmSidechainCompressor.SliderAttackChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[0] := SliderAttack.Position;
  end;
end;

procedure TFmSidechainCompressor.SliderReleaseChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[1] := SliderRelease.Position;
  end;
end;

procedure TFmSidechainCompressor.SliderThresholdChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[2] := SliderThreshold.Position;
  end;
end;

function TFmSidechainCompressor.EvaluateCharacteristic(Sender: TObject;
  X: Double): Double;
begin
 result := TSidechainCompressorDataModule(Owner).EvaluateCharacteristic(X);
end;

procedure TFmSidechainCompressor.SliderRatioChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[3] := SliderRatio.Position;
  end;
end;

procedure TFmSidechainCompressor.SliderKneeChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[4] := SliderKnee.Position;
  end;
end;

procedure TFmSidechainCompressor.SliderMakeUpGainChange(Sender: TObject);
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Parameter[5] := SliderMakeUpGain.Position;
  end;
end;

procedure TFmSidechainCompressor.UpdateAttack;
var
  Attack : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Attack := Parameter[0];
   if Attack <> SliderAttack.Position
    then SliderAttack.Position := Attack;
   LbAttackValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmSidechainCompressor.UpdateRelease;
var
  Release : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Release := Parameter[1];
   if Release <> SliderRelease.Position
    then SliderRelease.Position := Release;
   LbReleaseValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmSidechainCompressor.UpdateKnee;
var
  Knee : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Knee := Parameter[4];
   if Knee <> SliderKnee.Position
    then SliderKnee.Position := Knee;
   LbKneeValue.Caption := ParameterDisplay[4] + ' ' + ParameterLabel[4];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateMakeUp;
var
  MakeUp : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   MakeUp := SidechainCompressor[0].MakeUpGain_dB;
   if MakeUp <> SliderMakeUpGain.Position
    then SliderMakeUpGain.Position := MakeUp;
   LbMakeUpGainValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateRatio;
var
  Ratio : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Ratio := Parameter[3];
   if Ratio <> SliderRatio.Position
    then SliderRatio.Position := Ratio;
   LbRatioValue.Caption := ParameterDisplay[3] + ' : 1';
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateThreshold;
var
  Threshold : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Threshold := Parameter[2];
   if Threshold <> SliderThreshold.Position
    then SliderThreshold.Position := Threshold;
   LbThresholdValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateStereo;
var
  Brightness : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.3 + 0.7 * Parameter[6]);
   if Brightness <> LEDStereo.Brightness_Percent
    then LEDStereo.Brightness_Percent := Brightness;
  end;
end;

procedure TFmSidechainCompressor.UpdateAutoMakeUpGain;
var
  Brightness : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.3 + 0.7 * Parameter[8]);
   if Brightness <> LEDAutoGain.Brightness_Percent
    then LEDAutoGain.Brightness_Percent := Brightness;
   SliderMakeUpGain.Enabled := Brightness < 50;
   GuiGraphXY.UpdateGraph;
  end;
end;

procedure TFmSidechainCompressor.UpdateLimit;
var
  Brightness : Single;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   Brightness := 100 * (0.3 + 0.7 * Parameter[7]);
   if Brightness <> LEDSoftClip.Brightness_Percent
    then LEDSoftClip.Brightness_Percent := Brightness;
  end;
end;

procedure TFmSidechainCompressor.UpdateVstPlugin;
begin
 with TSidechainCompressorDataModule(Owner) do
  begin
   if VstPlugIn.Active
    then LbVstPluginValue.Caption := VstPlugIn.GetFriendlyNameString(45)
    else
     begin
      if VstPlugIn.numInputs <> 2 then LbVstPluginValue.Caption := 'input channel mismatch' else
      if VstPlugIn.numOutputs <> 2 then LbVstPluginValue.Caption := 'output channel mismatch'
       else LbVstPluginValue.Caption := '(double click to load)';
     end;
  end;
end;

end.
