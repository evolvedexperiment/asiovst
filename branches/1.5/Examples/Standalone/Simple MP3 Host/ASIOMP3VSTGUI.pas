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

unit ASIOMP3VSTGUI;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, Menus, ActnList, ToolWin, 
  ActnMan, ActnCtrls, ActnMenus, PlatformDefaultStyleActnCtrls, StdActns, 
  ComCtrls, ImgList, ExtCtrls, DAV_Types, DAV_AsioHost, DAV_MpegAudio, 
  DAV_DspBufferedMp3Player, DAV_VSTHost, System.ImageList, System.Actions;

type
  TFormASIOMP3VST = class(TForm)
    ActionAsioSettings: TAction;
    ActionFileExit: TFileExit;
    ActionFileOpenMP3: TFileOpen;
    ActionFileOpenVST: TFileOpen;
    ActionPlay: TAction;
    ActionMainMenuBar: TActionMainMenuBar;
    ActionManager: TActionManager;
    ActionToolBar: TActionToolBar;
    ASIOHost: TASIOHost;
    CoolBar: TCoolBar;
    ImageList: TImageList;
    PanelVSTPlugin: TPanel;
    VstHost: TVstHost;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionAsioSettingsExecute(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ActionFileOpenVSTAccept(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionFileOpenMP3Accept(Sender: TObject);
    procedure ASIOHostDriverChanged(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
  private
    FBufferedPlayer : TBufferedMP3FilePlayer;
    FChannelOffset  : Integer;
    FVolumeFactor   : Single;
    FVstFileName    : TFileName;
    FVstBuffers     : array [0..1, 0..1] of PDAVSingleFixedArray;
    procedure LoadVSTPlugin(FileName: TFileName);
    procedure LoadMP3File(FileName: TFileName);
  public
    property OutputChannelOffset: Integer read FChannelOffset write FChannelOffset;
    property Volume: Single read FVolumeFactor write FVolumeFactor;
  end;

var
  FormASIOMP3VST: TFormASIOMP3VST;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  IniFiles, DAV_VSTEffect, ASIOMP3VSTSetup;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFormASIOMP3VST.FormCreate(Sender: TObject);
begin
  if ASIOHost.DriverList.Count = 0 then
  try
    raise Exception.Create(RCStrNoASIODriverPresent);
  except
    on E: Exception do
    begin
      MessageDlg(E.Message , mtError, [mbOK], 0);
      Application.Terminate;
    end;
  end;

  FVolumeFactor := 1;
  FChannelOffset := 0;

  FBufferedPlayer := TBufferedMP3FilePlayer.Create;
  with FBufferedPlayer do
  begin
    BufferSize := 65536;
    BlockSize  := 4096;
    Pitch := 0;
    Interpolation := biBSpline6Point5thOrder;
    SampleRate := ASIOHost.SampleRate;
  end;
end;

procedure TFormASIOMP3VST.FormDestroy(Sender: TObject);
begin
  Dispose(FVstBuffers[0, 0]);
  Dispose(FVstBuffers[0, 1]);
  Dispose(FVstBuffers[1, 0]);
  Dispose(FVstBuffers[1, 1]);
  FreeAndNil(FBufferedPlayer);
end;

procedure TFormASIOMP3VST.FormShow(Sender: TObject);
var
  Index : Integer;
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIOMP3VST.INI') do
  try
    // load MP3 file
    FBufferedPlayer.Filename := ReadString('Audio', 'MP3 File', '');
    ActionPlay.Enabled := FileExists(FBufferedPlayer.Filename);

    // load VST plugin
    FVstFileName := ReadString('Audio', 'VST Plugin', FVstFileName);
    if FileExists(FVstFileName) then
      LoadVSTPlugin(FVstFileName);

    with FormSetup do
    begin
      ComboBoxDrivers.Items := FormASIOMP3VST.ASIOHost.DriverList;
      ComboBoxDrivers.ItemIndex := ComboBoxDrivers.Items.IndexOf(ReadString('Audio', 'ASIO Driver', 'ASIO4ALL v2'));
      if ComboBoxDrivers.ItemIndex >= 0 then
      begin
        ASIOHost.DriverIndex := ComboBoxDrivers.ItemIndex;
        ComboBoxOutput.Clear;
        for Index := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
          ComboBoxOutput.Items.Add(
            string(ASIOHost.OutputChannelInfos[2 * Index].Name) + ' / ' +
            string(ASIOHost.OutputChannelInfos[2 * Index + 1].Name));

        ButtonControlPanel.Enabled := True;
      end;
      ComboBoxOutput.ItemIndex := ReadInteger('Audio', 'Output Channel Offset', 0);
    end;
  finally
    Free;
  end;
end;

procedure TFormASIOMP3VST.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIOMP3VST.INI') do
  try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteString('Audio', 'MP3 File', FBufferedPlayer.Filename);
    WriteString('Audio', 'ASIO Driver', ASIOHost.DriverName);
    WriteString('Audio', 'VST Plugin', FVstFileName);
    WriteInteger('Audio', 'Output Channel Offset', FChannelOffset);
  finally
    Free;
  end;

  ASIOHost.Active := False;
  VSTHost[0].Close;
end;

procedure TFormASIOMP3VST.LoadVSTPlugin(FileName: TFileName);
var
  rct : TRect;
begin
  with VstHost[0] do
  begin
    LoadFromFile(FileName);
    Active := True;
    Idle;
    ShowEdit(PanelVSTPlugin);
    Idle;
    EditIdle;
    Caption := 'MP3 ASIO & VST Host' + GetVendorString + ' ' + GetEffectName;
  end;

  if (effFlagsHasEditor in VstHost[0].EffectOptions) then
  begin
    rct := VstHost[0].GetRect;
    ClientWidth := rct.Right - rct.Left;
    ClientHeight := rct.Bottom - rct.Top + ActionToolBar.Height;
  end;
end;


procedure TFormASIOMP3VST.LoadMP3File(FileName: TFileName);
begin
  FBufferedPlayer.Filename := Filename;
  ActionPlay.Enabled := FileExists(FBufferedPlayer.Filename);
end;

procedure TFormASIOMP3VST.ActionAsioSettingsExecute(Sender: TObject);
begin
  FormSetup.ShowModal;
end;

procedure TFormASIOMP3VST.ActionFileOpenMP3Accept(Sender: TObject);
begin
  if FileExists(ActionFileOpenMP3.Dialog.FileName) then
    LoadMP3File(ActionFileOpenMP3.Dialog.FileName);
end;

procedure TFormASIOMP3VST.ActionFileOpenVSTAccept(Sender: TObject);
begin
  FVstFileName := ActionFileOpenVST.Dialog.FileName;
  if FileExists(FVstFileName) then
    LoadVSTPlugin(FVstFileName);
end;

procedure TFormASIOMP3VST.ActionPlayExecute(Sender: TObject);
begin
  if ActionPlay.Caption = '&Play' then
  begin
    ASIOHost.Active := True;
    ActionPlay.Caption := '&Stop';
  end
  else
  begin
    ASIOHost.Active := False;
    FBufferedPlayer.Reset;
    ActionPlay.Caption := '&Play';
  end;
end;

procedure TFormASIOMP3VST.ASIOHostDriverChanged(Sender: TObject);
begin
  ReallocMem(FVstBuffers[0, 0], ASIOHost.BufferSize * SizeOf(Single));
  ReallocMem(FVstBuffers[0, 1], ASIOHost.BufferSize * SizeOf(Single));
  ReallocMem(FVstBuffers[1, 0], ASIOHost.BufferSize * SizeOf(Single));
  ReallocMem(FVstBuffers[1, 1], ASIOHost.BufferSize * SizeOf(Single));
end;

procedure TFormASIOMP3VST.ASIOHostSampleRateChanged(Sender: TObject);
begin
  if Assigned(FBufferedPlayer) then
    FBufferedPlayer.SampleRate := ASIOHost.SampleRate;
end;

procedure TFormASIOMP3VST.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Index : Integer;
begin
  FBufferedPlayer.GetSamples(FVstBuffers[0, 0], FVstBuffers[0, 1], ASIOHost.Buffersize);

  if VSTHost[0].Active then
  begin
    VSTHost[0].Process32Replacing(@FVstBuffers[0, 0], @FVstBuffers[1, 0],
      ASIOHost.BufferSize);

    Move(FVstBuffers[1, 0]^, OutBuffer[0]^, ASIOHost.Buffersize * SizeOf(Single));
    Move(FVstBuffers[1, 1]^, OutBuffer[1]^, ASIOHost.Buffersize * SizeOf(Single));
  end
  else
  begin
    Move(FVstBuffers[0, 0]^, OutBuffer[0]^, ASIOHost.Buffersize * SizeOf(Single));
    Move(FVstBuffers[0, 1]^, OutBuffer[1]^, ASIOHost.Buffersize * SizeOf(Single));
  end;
end;

end.
