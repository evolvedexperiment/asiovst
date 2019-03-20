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

unit MetronomeForm;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ComCtrls,
  ExtCtrls, Spin, Buttons, DAV_Types, DAV_Complex, DAV_AsioHost;

type
  TFormASIO = class(TForm)
    ASIOHost: TASIOHost;
    ButtonControlPanel: TButton;
    ButtonPlay: TButton;
    ComboBoxDriver: TComboBox;
    LabelDrivername: TLabel;
    LabelBPM: TLabel;
    LabelTempo: TLabel;
    LabelVolume: TLabel;
    ScrollBarVolume: TScrollBar;
    SpinEditTempo: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch(Sender: TObject;
      InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure ScrollBarVolumeChange(Sender: TObject);
    procedure SpinEditTempoChange(Sender: TObject);
  private
    FAngle: TComplex64;
    FPosition: TComplex64;
    FVolume: Single;
    FBeatPos: Integer;
    FIniFile: TFileName;
    procedure CalculateSineAngles;
  public
    FSamplesPerBeat: Single;
    FSamplesCount: Single;
    FMetroVolume: Single;
  published
  end;

var
  FormASIO: TFormASIO;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Inifiles, DAV_Common, DAV_Convert, DAV_Math;

procedure TFormASIO.FormCreate(Sender: TObject);
begin
  ComboBoxDriver.Items := ASIOHost.DriverList;
  FSamplesPerBeat := 60 / SpinEditTempo.Value * ASIOHost.SampleRate;
  FSamplesCount := 0;
  FMetroVolume := 1;
  FVolume := 1;
  CalculateSineAngles;
  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create('No ASIO Driver present! Application Terminated!');
    except
      Application.Terminate;
    end;

  FIniFile := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFile) do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);
      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);
    finally
      Free;
    end;
end;

procedure TFormASIO.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(FIniFile) do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;
end;

procedure TFormASIO.ComboBoxDriverChange(Sender: TObject);
begin
  ButtonControlPanel.Enabled := False;
  ButtonPlay.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    with TIniFile.Create(FIniFile) do
      try
        WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
      finally
        Free;
      end;
    ButtonControlPanel.Enabled := True;
    ButtonPlay.Enabled := True;
  end;
end;

procedure TFormASIO.CalculateSineAngles;
begin
  GetSinCos(2 * Pi * 1000 / ASIOHost.SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TFormASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
  FSamplesPerBeat := 60 / SpinEditTempo.Value * ASIOHost.SampleRate;
  CalculateSineAngles;
end;

procedure TFormASIO.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormASIO.ScrollBarVolumeChange(Sender: TObject);
begin
  FVolume := db_to_Amp(ScrollBarVolume.Position);
end;

procedure TFormASIO.SpinEditTempoChange(Sender: TObject);
begin
  FSamplesPerBeat := 60 / SpinEditTempo.Value * ASIOHost.SampleRate;
end;

procedure TFormASIO.ButtonPlayClick(Sender: TObject);
begin
  if ButtonPlay.Caption = 'Start Audio' then
  begin
    ASIOHost.Active := True; // Start Audio
    ButtonPlay.Caption := 'Stop Audio';
    FMetroVolume := 1;
    FSamplesCount := 0;
    FPosition.Re := 1;
    FPosition.Im := 0;
  end
  else
  begin
    ASIOHost.Active := False; // Stop Audio
    ButtonPlay.Caption := 'Start Audio';
    FBeatPos := 0;
  end;
end;

procedure TFormASIO.ASIOHostBufferSwitch(Sender: TObject;
  InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  i, j: Integer;
  s: Single;
begin
  for i := 0 to ASIOHost.BufferSize - 1 do
  begin
    s := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
    FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
    FPosition.Re := s;

    if FBeatPos = 0 then
      s := 2 * FPosition.Re * FPosition.Re - 1;

    s := FVolume * s * FMetroVolume;

    for j := 0 to ASIOHost.OutputChannelCount - 1 do
      OutBuffer[j, i] := s;
    FMetroVolume := 0.995 * FMetroVolume;
    FSamplesCount := FSamplesCount + 1;
    if FSamplesCount > FSamplesPerBeat then
    begin
      FMetroVolume := 1;
      FSamplesCount := FSamplesCount - FSamplesPerBeat;
      FPosition.Re := 1;
      FPosition.Im := 0;
      if FBeatPos < 3 then
        inc(FBeatPos)
      else
        FBeatPos := 0;
    end;
  end;
end;

end.
