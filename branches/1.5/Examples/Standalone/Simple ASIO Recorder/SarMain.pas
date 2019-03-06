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

unit SarMain;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, DAV_Types,
  DAV_DspBufferedAudioFileRecorder, DAV_ASIOHost, DAV_AudioFile,
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  TFormRecordAudio = class(TForm)
    ASIOHost: TASIOHost;
    ButtonControlPanel: TButton;
    ButtonSelect: TButton;
    ButtonStartStop: TButton;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    EditFile: TEdit;
    LabelBuffer: TLabel;
    LabelBufferValue: TLabel;
    LabelChannels: TLabel;
    LabelDrivername: TLabel;
    LabelRecordedFile: TLabel;
    Timer: TTimer;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ButtonSelectClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxChannelChange(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure EditFileChange(Sender: TObject);
    procedure LabelBufferClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FIniFile: TFileName;
    FVolumeFactor: Single;
    FChannelOffset: Byte;
    FBufferedRecorder: TBufferedAudioFileRecorder;
  end;

var
  FormRecordAudio: TFormRecordAudio;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  IniFiles;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

{ TFormASIOMP3 }

procedure TFormRecordAudio.FormCreate(Sender: TObject);
begin
  FIniFile := ExtractFilePath(ParamStr(0)) + 'SimpleAsioRecorder.INI';
  ComboBoxDriver.Items := ASIOHost.DriverList;
  if ComboBoxDriver.Items.Count = 0 then
  begin
    MessageDlg(RCStrNoASIODriverPresent, mtError, [mbOK], 0);
    Application.Terminate;
  end;

  FVolumeFactor := 1;
  FChannelOffset := 0;

  FBufferedRecorder := TBufferedAudioFileRecorder.Create;
  with FBufferedRecorder do
  begin
    BufferSize := 65536;
    BlockSize := 4096
  end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFile) do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);

      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);
      ComboBoxChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
      EditFile.Text := ReadString('Audio', 'File', EditFile.Text);
      EditFileChange(Self);
      ButtonStartStop.Enabled := (EditFile.Text <> '') and
        (ComboBoxDriver.ItemIndex >= 0);
    finally
      Free;
    end;
end;

procedure TFormRecordAudio.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(FIniFile) do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
      WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
      WriteString('Audio', 'File', EditFile.Text);
    finally
      Free;
    end;

  ASIOHost.Active := False;
  FreeAndNil(FBufferedRecorder);
end;

procedure TFormRecordAudio.LabelBufferClick(Sender: TObject);
begin
  ASIOHost.SampleRate := 48000;
end;

procedure TFormRecordAudio.TimerTimer(Sender: TObject);
begin
  LabelBufferValue.Caption := IntToStr(Round(FBufferedRecorder.BufferFill)) + ' %';
end;

procedure TFormRecordAudio.ComboBoxDriverChange(Sender: TObject);
var
  i: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for i := 0 to (ASIOHost.InputChannelCount) - 1 do
      ComboBoxChannel.Items.Add(ASIOHost.InputChannelInfos[i].Name);

    with TIniFile.Create(FIniFile) do
      try
        WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
      finally
        Free;
      end;

    ButtonControlPanel.Enabled := True;
    ButtonStartStop.Enabled := EditFile.Text <> '';
    ComboBoxChannel.ItemIndex := 0;
  end;
end;

procedure TFormRecordAudio.ComboBoxChannelChange(Sender: TObject);
begin
  FChannelOffset := ComboBoxChannel.ItemIndex * 2;
end;

procedure TFormRecordAudio.ASIOHostSampleRateChanged(Sender: TObject);
begin
  if Assigned(FBufferedRecorder) then
    FBufferedRecorder.SampleRate := ASIOHost.SampleRate;
end;

procedure TFormRecordAudio.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormRecordAudio.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Caption = '&Record Audio' then
  begin
    ASIOHost.Active := True;
    ButtonStartStop.Caption := '&Stop Audio';
  end
  else
  begin
    ASIOHost.Active := False;
    FBufferedRecorder.Reset;
    ButtonStartStop.Caption := '&Record Audio';
  end;
end;

procedure TFormRecordAudio.ButtonSelectClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    EditFile.Text := SaveDialog.FileName;
end;

procedure TFormRecordAudio.EditFileChange(Sender: TObject);
begin
  DeleteFile(EditFile.Text);
  FBufferedRecorder.FileName := EditFile.Text;
  ButtonStartStop.Enabled := (FBufferedRecorder.FileName <> '') and
    (ComboBoxDriver.ItemIndex >= 0);
end;

procedure TFormRecordAudio.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
  FBufferedRecorder.PutSamples(InBuffer[0], ASIOHost.BufferSize);
end;

end.
