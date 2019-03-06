{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit SmpSetup;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TFormSetup = class(TForm)
    LabelAsioDriver: TLabel;
    LabelOutput: TLabel;
    ComboBoxDrivers: TComboBox;
    ComboBoxOutput: TComboBox;
    ButtonControlPanel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxDriversChange(Sender: TObject);
    procedure ComboBoxOutputChange(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
  end;

var
  FormSetup: TFormSetup;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, Dialogs, SmpMain;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFormSetup.FormCreate(Sender: TObject);
begin
  ComboBoxDrivers.Items := FormSimpleMp3Player.ASIOHost.DriverList;
  if ComboBoxDrivers.Items.Count = 0 then
  begin
    MessageDlg(RCStrNoASIODriverPresent, mtError, [mbOK], 0);
    Application.Terminate;
  end;

  with TIniFile.Create(FormSimpleMp3Player.IniFile) do
    try
      Top := ReadInteger('Layout', 'Setup Top', Top);
      Left := ReadInteger('Layout', 'Setup Left', Left);
      ComboBoxDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver',
        ComboBoxDrivers.ItemIndex);
      ComboBoxDriversChange(Self);
    finally
      Free;
    end;
end;

procedure TFormSetup.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(FormSimpleMp3Player.IniFile) do
    try
      WriteInteger('Layout', 'Setup Top', Top);
      WriteInteger('Layout', 'Setup Left', Left);
      WriteInteger('Setup', 'ASIO Driver', ComboBoxDrivers.ItemIndex);
    finally
      Free;
    end;
end;

procedure TFormSetup.ComboBoxDriversChange(Sender: TObject);
var
  i: Integer;
begin
  with FormSimpleMp3Player.ASIOHost do
    if ComboBoxDrivers.ItemIndex >= 0 then
    begin
      Active := False;
      DriverIndex := ComboBoxDrivers.ItemIndex;
      ComboBoxOutput.Clear;
      for i := 0 to (OutputChannelCount div 2) - 1 do
      begin
        ComboBoxOutput.Items.Add(OutputChannelInfos[2 * i].name + ' / ' +
          OutputChannelInfos[2 * i + 1].name);
      end;
      ComboBoxOutput.ItemIndex := 0;
      if Assigned(OnReset) then
        OnReset(Self);

      with TIniFile.Create(FormSimpleMp3Player.IniFile) do
        try
          WriteInteger('Setup', 'Asio Driver', ComboBoxDrivers.ItemIndex);
        finally
          Free;
        end;

      ButtonControlPanel.Enabled := True;
    end;
end;

procedure TFormSetup.ComboBoxOutputChange(Sender: TObject);
begin
  FormSimpleMp3Player.OutputChannelOffset := ComboBoxOutput.ItemIndex * 2;
end;

procedure TFormSetup.ButtonControlPanelClick(Sender: TObject);
begin
  FormSimpleMp3Player.ASIOHost.ControlPanel;
end;

end.
