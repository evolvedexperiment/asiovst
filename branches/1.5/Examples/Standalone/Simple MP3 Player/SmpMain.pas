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

unit SmpMain;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, DAV_Types, 
  DAV_DspBufferedMp3Player, DAV_DspBufferedAudioFilePlayer, DAV_ASIOHost;

type
  TFormSimpleMp3Player = class(TForm)
    ASIOHost: TASIOHost;
    ButtonAddFile: TButton;
    ButtonDeleteItem: TButton;
    ButtonForward: TButton;
    ButtonPause: TButton;
    ButtonPlay: TButton;
    ButtonRewind: TButton;
    ButtonSetup: TButton;
    ButtonStop: TButton;
    LabelBitrateInfo: TLabel;
    LabelBuffer: TLabel;
    LabelBufferValue: TLabel;
    LabelInformation: TLabel;
    LabelTimeInfo: TLabel;
    OpenDialog: TOpenDialog;
    PlayList: TListBox;
    PanelInformation: TPanel;
    TrackBarPosition: TTrackBar;
    TrackBarVolume: TTrackBar;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BtControlPanelClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonAddFileClick(Sender: TObject);
    procedure PlayListClick(Sender: TObject);
    procedure ButtonSetupClick(Sender: TObject);
    procedure TrackBarVolumeChange(Sender: TObject);
    procedure ButtonDeleteItemClick(Sender: TObject);
    procedure ButtonRewindClick(Sender: TObject);
    procedure ButtonForwardClick(Sender: TObject);
  private
    FIniFile: TFileName;
    FVolumeFactor: Single;
    FBufferedPlayer: TBufferedMP3FilePlayer;
    FOutputChannelOffset : Integer;
  public
    property IniFile: TFileName read FIniFile;
    property OutputChannelOffset: Integer read FOutputChannelOffset write FOutputChannelOffset;
  end;

var
  FormSimpleMp3Player: TFormSimpleMp3Player;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, SmpSetup, DAV_MpegAudio;

{ TFmSmp }

procedure TFormSimpleMp3Player.FormCreate(Sender: TObject);
var
  Item : Integer;
begin
  FIniFile := ExtractFilePath(ParamStr(0)) + 'Simple MP3 Player.ini';

  FVolumeFactor := 1;
  FOutputChannelOffset := 0;
  FBufferedPlayer := TBufferedMP3FilePlayer.Create;
  with FBufferedPlayer do
  begin
    BufferSize := 65536;
    BlockSize  := 4096
  end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFile) do
  try
    Left := ReadInteger('Layout', 'Main Left', Left);
    Top := ReadInteger('Layout', 'Main Top', Top);

    ReadSection('Playlist', PlayList.Items);
    for Item := 0 to PlayList.Items.Count - 1 do
      PlayList.Items[Item] := ReadString('Playlist', PlayList.Items[Item], PlayList.Items[Item]);
    
    EraseSection('Playlist');
  finally
    Free;
  end;
end;

procedure TFormSimpleMp3Player.FormDestroy(Sender: TObject);
var
  Item : Integer;
begin
  ASIOHost.Active := False;
  FreeAndNil(FBufferedPlayer);

  with TIniFile.Create(FIniFile) do
  try
    WriteInteger('Layout', 'Main Left', Left);
    WriteInteger('Layout', 'Main Top', Top);

    for Item := 0 to PlayList.Count - 1 do
      WriteString('Playlist', 'File ' + IntToStr(Item + 1), PlayList.Items[Item]);
  finally
    Free;
  end;
end;

procedure TFormSimpleMp3Player.PlayListClick(Sender: TObject);
begin
  if (PlayList.ItemIndex >= 0) and FileExists(PlayList.Items[PlayList.ItemIndex]) then
  begin
    FBufferedPlayer.Filename := PlayList.Items[PlayList.ItemIndex];
    if Assigned(FBufferedPlayer.MpegAudio) then
      with FBufferedPlayer.MpegAudio do
      begin
        LabelInformation.Caption := Id3Artist + ' - ' + Id3Album + ' - ' +
          Id3Title + ' (' + Id3Year + ')';
       end;
  end;
end;

procedure TFormSimpleMp3Player.TrackBarVolumeChange(Sender: TObject);
begin
  FVolumeFactor := 0.01 * TrackBarVolume.Position;
end;

procedure TFormSimpleMp3Player.TimerTimer(Sender: TObject);
var
  Sec, Min: Integer;
  TmpStr: string;
begin
  LabelBufferValue.Caption := IntToStr(Round(FBufferedPlayer.BufferFill)) + ' %';
  if Assigned(FBufferedPlayer.MpegAudio) then
    with FBufferedPlayer.MpegAudio do
    begin
      LabelBitrateInfo.Caption := IntToStr(Bitrate div 1000) + 'kB/s';
      Min := Round(CurrentSamplePosition / SampleFrames * TotalLength / 60 - 0.5);
      Sec := Round(CurrentSamplePosition / SampleFrames * TotalLength - 60 * Min);
      if Min < 10 then
        TmpStr := '0' + IntToStr(Min)
      else
        TmpStr := IntToStr(Min);
      TmpStr := TmpStr + ':';
      if Sec < 10 then
        TmpStr := TmpStr + '0' + IntToStr(Sec)
      else
        TmpStr := TmpStr + IntToStr(Sec);
      LabelTimeInfo.Caption := TmpStr;
      TrackBarPosition.Position := Round(1000 * CurrentSamplePosition / SampleFrames);
    end;
end;

procedure TFormSimpleMp3Player.ASIOHostSampleRateChanged(Sender: TObject);
begin
  if Assigned(FBufferedPlayer) then
    FBufferedPlayer.SampleRate := ASIOHost.SampleRate;
end;

procedure TFormSimpleMp3Player.BtControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormSimpleMp3Player.ButtonPauseClick(Sender: TObject);
begin
  ASIOHost.Active := False;
end;

procedure TFormSimpleMp3Player.ButtonPlayClick(Sender: TObject);
begin
  ASIOHost.Active := True;
end;

procedure TFormSimpleMp3Player.ButtonRewindClick(Sender: TObject);
begin
  if PlayList.ItemIndex > 0 then
  begin
    PlayList.ItemIndex := PlayList.ItemIndex - 1;
    FBufferedPlayer.Filename := PlayList.Items[PlayList.ItemIndex];
  end;
end;

procedure TFormSimpleMp3Player.ButtonStopClick(Sender: TObject);
begin
  ASIOHost.Active := False;
  FBufferedPlayer.Reset;
end;

procedure TFormSimpleMp3Player.ButtonAddFileClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    PlayList.Items.Add(OpenDialog.FileName);
end;

procedure TFormSimpleMp3Player.ButtonDeleteItemClick(Sender: TObject);
begin
  if PlayList.ItemIndex >= 0 then
    PlayList.Items.Delete(PlayList.ItemIndex);
end;

procedure TFormSimpleMp3Player.ButtonForwardClick(Sender: TObject);
begin
  if PlayList.ItemIndex < PlayList.Count - 1 then
  begin
    PlayList.ItemIndex := PlayList.ItemIndex + 1;
    FBufferedPlayer.Filename := PlayList.Items[PlayList.ItemIndex];
  end;
end;

procedure TFormSimpleMp3Player.ButtonSetupClick(Sender: TObject);
begin
  FormSetup.ShowModal;
end;

procedure TFormSimpleMp3Player.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample: Integer;
begin
  FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);
  for Sample := 0 to ASIOHost.Buffersize - 1 do
  begin
    OutBuffer[0, Sample] := FVolumeFactor * OutBuffer[0, Sample];
    OutBuffer[1, Sample] := FVolumeFactor * OutBuffer[1, Sample];
  end;
end;

end.
