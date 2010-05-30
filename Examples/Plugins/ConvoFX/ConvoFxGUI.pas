unit ConvoFxGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Types,
  DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmConvoFx = class(TForm)
    DialIR: TGuiDial;
    DialGain: TGuiDial;
    LbIR: TGuiLabel;
    LbIRSelected: TGuiLabel;
    LbGain: TGuiLabel;
    LbGainValue: TGuiLabel;
    DialDamp: TGuiDial;
    LbDamp: TGuiLabel;
    LbDampValue: TGuiLabel;
    DIL: TGuiDialImageList;
    procedure DialIRChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbIRSelectedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DialGainChange(Sender: TObject);
    procedure DialDampChange(Sender: TObject);
  public
    procedure UpdateIRSelect;
    procedure UpdateGain;
    procedure UpdateDamping;
  end;

implementation

{$R *.DFM}

uses
  Dialogs, Math, PngImage, ConvoFxDM, DAV_VSTModuleWithPrograms;

procedure TFmConvoFx.DialDampChange(Sender: TObject);
begin
 with TConvoFxDataModule(Owner) do
  begin
   Parameter[4] := DialDamp.Position;
  end;
end;

procedure TFmConvoFx.DialGainChange(Sender: TObject);
begin
 with TConvoFxDataModule(Owner) do
  begin
   Parameter[3] := DialGain.Position;
  end;
end;

procedure TFmConvoFx.DialIRChange(Sender: TObject);
begin
 with TConvoFxDataModule(Owner) do
  begin
   if round(Parameter[2]) <> round(DialIR.Position)
    then Parameter[2] := round(DialIR.Position);
  end;
end;

procedure TFmConvoFx.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ConvoFxKnob', 'PNG');
  try
   with DIL.DialImages.Add do
    begin
     NumGlyphs := 65;
     PngBmp.LoadFromStream(RS);
     DialBitmap.Assign(PngBmp);
    end;
   DialIR.DialImageIndex := 0;
   DialGain.DialImageIndex := 0;
   DialDamp.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmConvoFx.FormShow(Sender: TObject);
begin
 UpdateIRSelect;
 UpdateGain;
 UpdateDamping;
end;

procedure TFmConvoFx.LbIRSelectedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 case Button of
  mbLeft  : DialIR.Position := round(DialIR.Position) + 1;
  mbRight : DialIR.Position := round(DialIR.Position) - 1;
 end;
end;

procedure TFmConvoFx.UpdateGain;
begin
 with TConvoFxDataModule(Owner) do
  begin
   if DialGain.Position <> Parameter[3]
    then DialGain.Position := Parameter[3];
   LbGainValue.Caption := FloatToStrF(RoundTo(Parameter[3], -1), ffGeneral, 2, 2) + ' dB';
  end;
end;

procedure TFmConvoFx.UpdateDamping;
begin
 with TConvoFxDataModule(Owner) do
  begin
   if DialDamp.Position <> Parameter[4]
    then DialDamp.Position := Parameter[4];
   if Parameter[4] < 1000
    then LbDampValue.Caption := FloatToStrF(RoundTo(Parameter[4], -1), ffGeneral, 3, 3) + ' Hz'
    else LbDampValue.Caption := FloatToStrF(RoundTo(1E-3 * Parameter[4], -1), ffGeneral, 3, 3) + ' kHz';
  end;
end;

procedure TFmConvoFx.UpdateIRSelect;
begin
 with TConvoFxDataModule(Owner) do
  begin
   if round(DialIR.Position) <> round(Parameter[2])
    then DialIR.Position := round(Parameter[2]);
   LbIRSelected.Caption := IntToStr(round(Parameter[2]));
  end;
end;

end.
