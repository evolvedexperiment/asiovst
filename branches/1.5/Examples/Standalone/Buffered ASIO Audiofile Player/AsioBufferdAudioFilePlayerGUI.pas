unit AsioBufferdAudioFilePlayerGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2019        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, DAV_Types, 
  DAV_ASIOHost, DAV_DspBufferedAudioFilePlayer, DAV_AudioFile, 
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU;

type
  TFormAsioBufferdAudioFilePlayer = class(TForm)
    ASIOHost: TASIOHost;
    ButtonControlPanel: TButton;
    ButtonStartStop: TButton;
    ButtonSelect: TButton;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    EditFile: TEdit;
    LabelAudioFile: TLabel;
    LabelChannels: TLabel;
    LabelDrivername: TLabel;
    OpenDialog: TOpenDialog;
    LabelBuffer: TLabel;
    LabelBufferValue: TLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
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
    FIniFile        : TFileName;
    FVolumeFactor   : Single;
    FChannelOffset  : Byte;
    FBufferedPlayer : TBufferedAudioFilePlayer;
  end;

var
  FormAsioBufferdAudioFilePlayer: TFormAsioBufferdAudioFilePlayer;

implementation

{$R *.dfm}

uses
  IniFiles;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

{ TFormAsioBufferdAudioFilePlayer }

procedure TFormAsioBufferdAudioFilePlayer.FormCreate(Sender: TObject);
begin
  FIniFile := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';
  ComboBoxDriver.Items := ASIOHost.DriverList;
  if ComboBoxDriver.Items.Count = 0 then
  begin
    MessageDlg(RCStrNoASIODriverPresent,
      mtError, [mbOK], 0);
    Application.Terminate;
  end;

  FVolumeFactor := 1;
  FChannelOffset := 0;
  FBufferedPlayer := TBufferedAudioFilePlayer.Create;
  FBufferedPlayer.Pitch := 0;
  FBufferedPlayer.Interpolation := biBSpline6Point5thOrder;
  with FBufferedPlayer do
  begin
    BufferSize := 65536;
    BlockSize  := 4096
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
    EditFile.Text := ReadString('Audio', 'Audio File', EditFile.Text);
    ButtonStartStop.Enabled := FileExists(EditFile.Text);
  finally
    Free;
  end;
end;

procedure TFormAsioBufferdAudioFilePlayer.FormDestroy(Sender: TObject);
begin
  ASIOHost.Active := False;
  FreeAndNil(FBufferedPlayer);

  with TIniFile.Create(FIniFile) do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
      WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
      WriteString('Audio', 'Audio File', EditFile.Text);
    finally
      Free;
    end;
end;

procedure TFormAsioBufferdAudioFilePlayer.LabelBufferClick(Sender: TObject);
begin
  ASIOHost.SampleRate := 48000;
end;

procedure TFormAsioBufferdAudioFilePlayer.TimerTimer(Sender: TObject);
begin
  LabelBufferValue.Caption := IntToStr(Round(FBufferedPlayer.BufferFill)) + ' %';
end;

procedure TFormAsioBufferdAudioFilePlayer.ComboBoxDriverChange(Sender: TObject);
var
  ChannelPairIndex: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for ChannelPairIndex := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
      ComboBoxChannel.Items.Add(string(
        string(ASIOHost.OutputChannelInfos[2 * ChannelPairIndex].Name) + ' / ' +
        string(ASIOHost.OutputChannelInfos[2 * ChannelPairIndex + 1].Name)));

    with TIniFile.Create(FIniFile) do
    try
      WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;

    ButtonControlPanel.Enabled := True;
    ButtonStartStop.Enabled := FileExists(EditFile.Text);
    ComboBoxChannel.ItemIndex := 0;
  end;
end;

procedure TFormAsioBufferdAudioFilePlayer.ComboBoxChannelChange(Sender: TObject);
begin
  FChannelOffset := ComboBoxChannel.ItemIndex * 2;
end;

procedure TFormAsioBufferdAudioFilePlayer.ASIOHostSampleRateChanged(Sender: TObject);
begin
  if Assigned(FBufferedPlayer) then
    FBufferedPlayer.SampleRate := ASIOHost.SampleRate;
end;

procedure TFormAsioBufferdAudioFilePlayer.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormAsioBufferdAudioFilePlayer.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Caption = '&Start Audio' then
  begin
    ASIOHost.Active := True;
    ButtonStartStop.Caption := '&Stop Audio';
  end
  else
  begin
    ASIOHost.Active := False;
    FBufferedPlayer.Reset;
    ButtonStartStop.Caption := '&Start Audio';
  end;
end;

procedure TFormAsioBufferdAudioFilePlayer.ButtonSelectClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    EditFile.Text := OpenDialog.FileName;
end;

procedure TFormAsioBufferdAudioFilePlayer.EditFileChange(Sender: TObject);
begin
  FBufferedPlayer.Filename := EditFile.Text;
  ButtonStartStop.Enabled := FileExists(EditFile.Text);
end;

procedure TFormAsioBufferdAudioFilePlayer.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
//  FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);
  FBufferedPlayer.GetSamples(OutBuffer, ASIOHost.Buffersize);
end;

end.
