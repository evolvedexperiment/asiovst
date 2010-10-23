unit PlateReverbGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel,
  DAV_GuiPanel, DAV_GuiSelectBox, DAV_GuiButton;

type

  TFmPlateReverb = class(TForm)
    BtAB: TGuiButton;
    BtAbout: TGuiButton;
    CBFreeze: TCheckBox;
    DialDampingFrequency: TGuiDial;
    DialDry: TGuiDial;
    DialInputDiffusion: TGuiDial;
    DialDecayDiffusion: TGuiDial;
    DialWet: TGuiDial;
    DialPreDelay: TGuiDial;
    LbPreset: TGuiLabel;
    PnLabel: TGuiPanel;
    PnToolbar: TPanel;
    SBPreset: TGuiSelectBox;
    LbDry: TGuiLabel;
    LbWet: TGuiLabel;
    LbPreDelay: TGuiLabel;
    LbInputDiffusion: TGuiLabel;
    LbDecayDiffusion: TGuiLabel;
    LbDampingFrequency: TGuiLabel;
    DIL: TGuiDialImageList;
    DialDecay: TGuiDial;
    LbDecay: TGuiLabel;
    LbMod: TGuiLabel;
    DialModulation: TGuiDial;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtAboutClick(Sender: TObject);
    procedure DialWetChange(Sender: TObject);
    procedure DialDryChange(Sender: TObject);
    procedure DialPreDelayChange(Sender: TObject);
    procedure DialInputDiffusionChange(Sender: TObject);
    procedure DialDecayDiffusionChange(Sender: TObject);
    procedure DialDampingFrequencyChange(Sender: TObject);
    procedure SBPresetChange(Sender: TObject);
    procedure DialDecayChange(Sender: TObject);
  public
    procedure UpdateDry;
    procedure UpdateWet;
    procedure UpdatePreDelay;
    procedure UpdateDecay;
    procedure UpdateDampingFrequency;
    procedure UpdateInputDiffusion;
    procedure UpdateDecayDiffusion;
    procedure UpdateModulation;
  end;

implementation

{$R *.DFM}

uses
  Dialogs, PlateReverbModule;

procedure TFmPlateReverb.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'PlateReverbKnob', 'BMP');
 try
  with DIL.DialImages.Add do
   begin
    Width := 48;
    Height := 48;
    GlyphCount := 64;
    StitchKind := skVertical;
    DialBitmap.LoadFromStream(RS);
   end;
  DialDry.DialImageIndex := 0;
  DialWet.DialImageIndex := 0;     
  DialPreDelay.DialImageIndex := 0;
  DialDecay.DialImageIndex := 0;
  DialDampingFrequency.DialImageIndex := 0;
  DialInputDiffusion.DialImageIndex := 0;
  DialDecayDiffusion.DialImageIndex := 0;
  DialModulation.DialImageIndex := 0;
 finally
  RS.Free;
 end;
end;

procedure TFmPlateReverb.FormShow(Sender: TObject);
var
  i : Integer;
begin
 with TPlateReverbVST(Owner) do
  begin
   SBPreset.Items.Clear;
   for i := 0 to numPrograms - 1
    do SBPreset.Items.Add(Programs[i].DisplayName);
   SBPreset.ItemIndex := CurrentProgram; 
  end;
end;

procedure TFmPlateReverb.SBPresetChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  begin
   CurrentProgram := SBPreset.ItemIndex;
   UpdateDry;
   UpdateWet;
   UpdatePreDelay;
   UpdateDecay;
   UpdateDampingFrequency;
   UpdateInputDiffusion;
   UpdateDecayDiffusion;
   UpdateModulation;
  end;
end;

procedure TFmPlateReverb.UpdateDry;
begin
 with TPlateReverbVST(Owner) do
  if DialDry.Position <> Parameter[0]  then
   begin
    DialDry.Position := Parameter[0];
   end;
end;

procedure TFmPlateReverb.UpdateWet;
begin
 with TPlateReverbVST(Owner) do
  if DialWet.Position <> Parameter[1]  then
   begin
    DialWet.Position := Parameter[1];
   end;
end;

procedure TFmPlateReverb.UpdatePreDelay;
begin
 with TPlateReverbVST(Owner) do
  if DialPreDelay.Position <> Parameter[2]  then
   begin
    DialPreDelay.Position := Parameter[2];
   end;
end;

procedure TFmPlateReverb.UpdateDecay;
begin
 with TPlateReverbVST(Owner) do
  if DialDecay.Position <> Parameter[3]  then
   begin
    DialDecay.Position := Parameter[3];
   end;
end;

procedure TFmPlateReverb.UpdateDampingFrequency;
begin
 with TPlateReverbVST(Owner) do
  if DialDampingFrequency.Position <> Parameter[4] then
   begin
    DialDampingFrequency.Position := Parameter[4];
   end;
end;

procedure TFmPlateReverb.UpdateInputDiffusion;
begin
 with TPlateReverbVST(Owner) do
  if DialInputDiffusion.Position <> Parameter[5] then
   begin
    DialInputDiffusion.Position := Parameter[5];
   end;
end;

procedure TFmPlateReverb.UpdateDecayDiffusion;
begin
 with TPlateReverbVST(Owner) do
  if DialDecayDiffusion.Position <> Parameter[6] then
   begin
    DialDecayDiffusion.Position := Parameter[6];
   end;
end;

procedure TFmPlateReverb.UpdateModulation;
begin
 with TPlateReverbVST(Owner) do
  if DialModulation.Position <> Parameter[7] then
   begin
    DialModulation.Position := Parameter[7];
   end;
end;

procedure TFmPlateReverb.DialDryChange(Sender: TObject);
var
  Value : Single;
begin
 with TPlateReverbVST(Owner) do
  begin
   Value := DialDry.Position;
   if Parameter[0] <> Value
    then Parameter[0] := Value;
  end;
end;

procedure TFmPlateReverb.DialWetChange(Sender: TObject);
var
  Value : Single;
begin
 with TPlateReverbVST(Owner) do
  begin
   Value := DialWet.Position;
   if Parameter[1] <> Value
    then Parameter[1] := Value;
  end;
end;

procedure TFmPlateReverb.DialPreDelayChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[2] <> DialPreDelay.Position then
   begin
    Parameter[2] := DialPreDelay.Position;
   end;
end;

procedure TFmPlateReverb.DialDecayChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[3] <> DialDecay.Position then
   begin
    Parameter[3] := DialDecay.Position;
   end;
end;

procedure TFmPlateReverb.DialDampingFrequencyChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[4] <> DialDampingFrequency.Position then
   begin
    Parameter[4] := DialDampingFrequency.Position;
   end;
end;

procedure TFmPlateReverb.DialInputDiffusionChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[5] <> DialInputDiffusion.Position then
   begin
    Parameter[5] := DialInputDiffusion.Position;
   end;
end;

procedure TFmPlateReverb.DialDecayDiffusionChange(Sender: TObject);
begin
 with TPlateReverbVST(Owner) do
  if Parameter[6] <> DialDecayDiffusion.Position then
   begin
    Parameter[6] := DialDecayDiffusion.Position;
   end;
end;

procedure TFmPlateReverb.BtAboutClick(Sender: TObject);
begin
 MessageDlg('PlateReverb example plugin written by Christian Budde' + #13#10 +
            'based on GUI by thcilnnahoj', mtInformation, [mbOK], 0);
end;

end.
