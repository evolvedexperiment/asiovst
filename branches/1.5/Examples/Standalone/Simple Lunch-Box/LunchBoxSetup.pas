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

unit LunchBoxSetup;

interface

{$I DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons,
{$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls, Spin;

type
  TFormSetup = class(TForm)
    LabelPreset: TLabel;
    LabelOutput: TLabel;
    ComboBoxDrivers: TComboBox;
    ComboBoxOutput: TComboBox;
    ButtonControlPanel: TButton;
    LabelPlaybackSampleRate: TLabel;
    SpinEditSampleRate: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxDriversChange(Sender: TObject);
    procedure ComboBoxOutputChange(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure SpinEditSampleRateChange(Sender: TObject);
  end;

var
  FormSetup: TFormSetup;

implementation

uses
  IniFiles, DAV_ASIOHost, LunchBoxMain;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFormSetup.FormCreate(Sender: TObject);
var
  Settings: TInifile;
begin
  ComboBoxDrivers.Items := FormLunchBox.ASIOHost.DriverList;
  Settings := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI');
  Top := Settings.ReadInteger('Layout', 'Setup Top', Top);
  Left := Settings.ReadInteger('Layout', 'Setup Left', Left);
  ComboBoxDrivers.ItemIndex := Settings.ReadInteger('Setup', 'ASIO Driver',
    ComboBoxDrivers.ItemIndex);
  ComboBoxDriversChange(Self);
  Settings.Free;
end;

procedure TFormSetup.ButtonControlPanelClick(Sender: TObject);
begin
  FormLunchBox.ASIOHost.ControlPanel;
end;

procedure TFormSetup.ComboBoxDriversChange(Sender: TObject);
var
  i: Integer;
begin
  if ComboBoxDrivers.ItemIndex >= 0 then
    with FormLunchBox.ASIOHost do
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
      SpinEditSampleRate.Value := Round(Samplerate);
      OnReset(Self);
      Active := True;
    end;
end;

procedure TFormSetup.ComboBoxOutputChange(Sender: TObject);
begin
  // FormVSTEditor.ASIOHost.OutputChannels:=ComboBoxOutput.ItemIndex*2;
end;

procedure TFormSetup.FormDestroy(Sender: TObject);
var
  Settings: TInifile;
begin
  Settings := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI');
  Settings.WriteInteger('Layout', 'Setup Top', Top);
  Settings.WriteInteger('Layout', 'Setup Left', Left);
  Settings.WriteInteger('Setup', 'ASIO Driver', ComboBoxDrivers.ItemIndex);
  Settings.Free;
end;

procedure TFormSetup.SpinEditSampleRateChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FormLunchBox.EventList.Count - 1 do
    with FormLunchBox.EventList[i] do
    begin
      Samplerate := sqr(FormLunchBox.ASIOHost.Samplerate) / SpinEditSampleRate.Value;
      Frequency := Samples[SampleIndex].Samplerate / Samplerate;
    end;
end;

end.
