unit ASIOMP3VSTSetup;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2019        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TFormSetup = class(TForm)
    LabelPreset: TLabel;
    LabelOutput: TLabel;
    ComboBoxDrivers: TComboBox;
    ComboBoxOutput: TComboBox;
    ButtonControlPanel: TButton;
    procedure ComboBoxDriversChange(Sender: TObject);
    procedure ComboBoxOutputChange(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
  end;

var
  FormSetup: TFormSetup;

implementation

uses
  IniFiles, ASIOMP3VSTGUI;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFormSetup.ButtonControlPanelClick(Sender: TObject);
begin
  FormASIOMP3VST.AsioHost.ControlPanel;
end;

procedure TFormSetup.ComboBoxDriversChange(Sender: TObject);
var
  i : Integer;
begin
  with FormASIOMP3VST.ASIOHost do
    if ComboBoxDrivers.ItemIndex >= 0 then
    begin
      Active := False;
      DriverIndex := ComboBoxDrivers.ItemIndex;
      ComboBoxOutput.Clear;
      for i := 0 to (OutputChannelCount div 2) - 1 do
      begin
        ComboBoxOutput.Items.Add(string(
          OutputChannelInfos[2 * i].name + ' / ' +
          OutputChannelInfos[2 * i + 1].name));
      end;
      ComboBoxOutput.ItemIndex := 0;
      if Assigned(OnReset) then
        OnReset(Self);
      Active := True;
    end;
end;

procedure TFormSetup.ComboBoxOutputChange(Sender: TObject);
begin
  FormASIOMP3VST.OutputChannelOffset := ComboBoxOutput.ItemIndex * 2;
end;

{$IFDEF FPC}
initialization
  {$i EditorSetup.lrs}
{$ENDIF}

end.

