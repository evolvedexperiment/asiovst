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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit SHRSetup;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls;

const
  IniFileName = 'Simple HD Recorder.INI';

type
  TFormSetup = class(TForm)
    LabelDriver: TLabel;
    LabelInput: TLabel;
    LabelOutput: TLabel;
    ComboBoxDrivers: TComboBox;
    ComboBoxInput: TComboBox;
    ComboBoxOutput: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxDriversChange(Sender: TObject);
    procedure ComboBoxInputChange(Sender: TObject);
    procedure ComboBoxOutputChange(Sender: TObject);
  end;

var
  FormSetup: TFormSetup;

implementation

uses
  IniFiles, SHRmain;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFormSetup.ComboBoxInputChange(Sender: TObject);
begin
  FormSimpleHDRecorder.InputChannelOffset := ComboBoxInput.ItemIndex;
end;

procedure TFormSetup.ComboBoxOutputChange(Sender: TObject);
begin
  FormSimpleHDRecorder.OutputChannelOffset := ComboBoxOutput.ItemIndex;
end;

procedure TFormSetup.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ComboBoxDrivers.Items := FormSimpleHDRecorder.ASIOHost.DriverList;
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + IniFileName) do
    try
      Top := ReadInteger('Layout', 'Setup Top', Top);
      Left := ReadInteger('Layout', 'Setup Left', Left);

      for i := 0 to ComboBoxDrivers.Items.Count - 1 do
        if Pos('M-Audio', ComboBoxDrivers.Items[i]) > 0 then
        begin
          ComboBoxDrivers.ItemIndex := i;
          Break;
        end;
      ComboBoxDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver',
        ComboBoxDrivers.ItemIndex);
        ComboBoxDriversChange(Self);
    finally
      Free;
    end;
end;

procedure TFormSetup.ComboBoxDriversChange(Sender: TObject);
var
  i: Integer;
begin
  with FormSimpleHDRecorder.ASIOHost do
    if ComboBoxDrivers.ItemIndex >= 0 then
    begin
      Active := False;
      DriverIndex := ComboBoxDrivers.ItemIndex;
      ComboBoxInput.Clear;
      for i := 0 to InputChannelCount - 1 do
        ComboBoxInput.Items.Add(InputChannelInfos[i].Name);
      ComboBoxOutput.Clear;
      for i := 0 to OutputChannelCount - 1 do
        ComboBoxOutput.Items.Add(OutputChannelInfos[i].Name);
      ComboBoxInput.ItemIndex := 0;
      ComboBoxOutput.ItemIndex := 0;
      if Assigned(OnReset) then
        OnReset(Self);
      Active := True;
    end;
end;

procedure TFormSetup.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + IniFileName) do
    try
      WriteInteger('Layout', 'Setup Top', Top);
      WriteInteger('Layout', 'Setup Left', Left);
      WriteInteger('Setup', 'ASIO Driver', ComboBoxDrivers.ItemIndex);
    finally
      Free;
    end;
end;

{$IFDEF FPC}

initialization

{$I EditorSetup.lrs}
{$ENDIF}

end.
