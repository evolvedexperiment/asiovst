unit fReeverbGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel,
  DAV_GuiPanel, DAV_GuiSelectBox, DAV_GuiButton;

type

  TFmReverb = class(TForm)
    BtAB: TGuiButton;
    BtAbout: TGuiButton;
    CBFreeze: TCheckBox;
    DialDamp: TGuiDial;
    DialDry: TGuiDial;
    DialRoomSize: TGuiDial;
    DialStretch: TGuiDial;
    DialWet: TGuiDial;
    DialWidth: TGuiDial;
    LbPreset: TGuiLabel;
    PnLabel: TGuiPanel;
    PnToolbar: TPanel;
    SBPreset: TGuiSelectBox;
    LbDry: TGuiLabel;
    LbWet: TGuiLabel;
    LbWidth: TGuiLabel;
    LbSize: TGuiLabel;
    LbStretch: TGuiLabel;
    LbDamp: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtAboutClick(Sender: TObject);
    procedure CBFreezeClick(Sender: TObject);
    procedure DialWetChange(Sender: TObject);
    procedure DialDryChange(Sender: TObject);
    procedure DialWidthChange(Sender: TObject);
    procedure DialRoomSizeChange(Sender: TObject);
    procedure DialStretchChange(Sender: TObject);
    procedure DialDampChange(Sender: TObject);
    procedure SBPresetChange(Sender: TObject);
  public
    procedure UpdateDry;
    procedure UpdateWet;
    procedure UpdateWidth;
    procedure UpdateDamp;
    procedure UpdateSize;
    procedure UpdateStretch;
  end;

implementation

{$R *.DFM}

uses
  Dialogs, fReeverbModule;

procedure TFmReverb.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'ReverbKnob', 'BMP');
 try
  DialDry.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
  DialWet.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
  DialWidth.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DialDamp.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DialRoomSize.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialStretch.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TFmReverb.FormShow(Sender: TObject);
var
  i : Integer;
begin
 with TfReeverbVST(Owner) do
  begin
   SBPreset.Items.Clear;
   for i := 0 to numPrograms - 1
    do SBPreset.Items.Add(Programs[i].DisplayName);
   SBPreset.ItemIndex := CurrentProgram; 
  end;
end;

procedure TFmReverb.SBPresetChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  begin
   CurrentProgram := SBPreset.ItemIndex;
   UpdateDry;
   UpdateWet;
   UpdateWidth;
   UpdateDamp;
   UpdateSize;
   UpdateStretch;
  end;
end;

procedure TFmReverb.UpdateDamp;
begin
 with TfReeverbVST(Owner) do
  if DialDamp.Position <> Parameter[6] then
   begin
    DialDamp.Position := Parameter[6];
   end;
end;

procedure TFmReverb.UpdateDry;
begin
 with TfReeverbVST(Owner) do
  if DialDry.Position <> Parameter[0]  then
   begin
    DialDry.Position := Parameter[0];
   end;
end;

procedure TFmReverb.UpdateSize;
begin
 with TfReeverbVST(Owner) do
  if DialRoomSize.Position <> Parameter[3] then
   begin
    DialRoomSize.Position := Parameter[3];
   end;
end;

procedure TFmReverb.UpdateStretch;
begin
 with TfReeverbVST(Owner) do
  if DialStretch.Position <> Parameter[5] then
   begin
    DialStretch.Position := Parameter[5];
   end;
end;

procedure TFmReverb.UpdateWet;
begin
 with TfReeverbVST(Owner) do
  if DialWet.Position <> Parameter[1]  then
   begin
    DialWet.Position := Parameter[1];
   end;
end;

procedure TFmReverb.UpdateWidth;
begin
 with TfReeverbVST(Owner) do
  if DialWidth.Position <> Parameter[2]  then
   begin
    DialWidth.Position := Parameter[2];
   end;
end;

procedure TFmReverb.DialDryChange(Sender: TObject);
var
  Value : Single;
begin
 with TfReeverbVST(Owner) do
  begin
   Value := DialDry.Position;
   if Parameter[0] <> Value
    then Parameter[0] := Value;
  end;
end;

procedure TFmReverb.DialWetChange(Sender: TObject);
var
  Value : Single;
begin
 with TfReeverbVST(Owner) do
  begin
   Value := DialWet.Position;
   if Parameter[1] <> Value
    then Parameter[1] := Value;
  end;
end;

procedure TFmReverb.DialWidthChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[2] <> DialWidth.Position then
   begin
    Parameter[2] := DialWidth.Position;
   end;
end;

procedure TFmReverb.DialRoomSizeChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[3] <> DialRoomSize.Position then
   begin
    Parameter[3] := DialRoomSize.Position;
   end;
end;

procedure TFmReverb.BtAboutClick(Sender: TObject);
begin
 MessageDlg('fReeverb example plugin written by Christian Budde' + #13#10 +
            'based on algorithm by Jezar at Dreampoint' + #13#10 +
            'based on GUI by thcilnnahoj', mtInformation, [mbOK], 0);
end;

procedure TFmReverb.CBFreezeClick(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[4] := Byte(CBFreeze.Checked);
end;

procedure TFmReverb.DialStretchChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[5] <> DialStretch.Position then
   begin
    Parameter[5] := DialStretch.Position;
   end;
end;

procedure TFmReverb.DialDampChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[6] <> DialDamp.Position then
   begin
    Parameter[6] := DialDamp.Position;
   end;
end;

end.
