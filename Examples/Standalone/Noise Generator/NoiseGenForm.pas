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

unit NoiseGenForm;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Types, DAV_ASIOHost;

type
  TFormASIONoiseGenerator = class(TForm)
    ASIOHost: TASIOHost;
    ButtonStartStop: TButton;
    ComboBoxDriver: TComboBox;
    LabelCopyright: TLabel;
    LabelDrivername: TLabel;
    LabelPanorama: TLabel;
    LabelVolume: TLabel;
    ScrollBarPan: TScrollBar;
    ScrollBarVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostBufferSwitch64(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure Bt_CPClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure ScrollBarPanChange(Sender: TObject);
    procedure ScrollBarVolumeChange(Sender: TObject);
  private
    FVol, FPan: Single;
  end;

var
  FormASIONoiseGenerator: TFormASIONoiseGenerator;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Common;

procedure TFormASIONoiseGenerator.FormCreate(Sender: TObject);
begin
  ComboBoxDriver.Items := ASIOHost.DriverList;
  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create('No ASIO Driver present! Application Terminated!');
    except
      Application.Terminate;
    end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
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

procedure TFormASIONoiseGenerator.ComboBoxDriverChange(Sender: TObject);
begin
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
      try
        WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
      finally
        Free;
      end;
    ButtonStartStop.Enabled := True;
  end;
end;

procedure TFormASIONoiseGenerator.Bt_CPClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormASIONoiseGenerator.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;
end;

procedure TFormASIONoiseGenerator.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Caption = 'Start Audio' then
  begin
    ASIOHost.Active := True; // Start Audio
    ButtonStartStop.Caption := 'Stop Audio';
  end
  else
  begin
    ASIOHost.Active := False; // Stop Audio
    ButtonStartStop.Caption := 'Start Audio';
  end;
end;

procedure TFormASIONoiseGenerator.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel, Sample: integer;
begin
  for Channel := 0 to ASIOHost.BufferSize - 1 do
  begin
    for Sample := 0 to ASIOHost.OutputChannelCount - 1 do
      OutBuffer[Channel, Sample] := (Random - Random) * FVol;
  end;
end;

procedure TFormASIONoiseGenerator.ASIOHostBufferSwitch64(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Channel, Sample: integer;
begin
  for Channel := 0 to ASIOHost.BufferSize - 1 do
  begin
    for Sample := 0 to ASIOHost.OutputChannelCount - 1 do
      OutBuffer[Channel, Sample] := (Random - Random) * FVol;
  end;
end;

procedure TFormASIONoiseGenerator.ScrollBarVolumeChange(Sender: TObject);
begin
  FVol := ScrollBarVolume.Position * 0.00001;
  if FVol = 0 then
    LabelVolume.Caption := 'Volume: 0 equals -oo dB'
  else
    LabelVolume.Caption := 'Volume: ' + FloatToStrF(FVol, ffFixed, 2, 2) +
      ' equals ' + FloatToStrF(Amp_to_dB(FVol), ffGeneral, 2, 2) + ' dB';
end;

procedure TFormASIONoiseGenerator.ScrollBarPanChange(Sender: TObject);
begin
  FPan := ScrollBarPan.Position * 0.01;
  if FPan = 0.5 then
    LabelPanorama.Caption := 'Panorama: C'
  else
    LabelPanorama.Caption := 'Panorama: ' + IntToStr(round(100 * (FPan * 2 - 1)));
end;

{$IFDEF FPC}

initialization

{$I NoiseGenForm.lrs}
{$ENDIF}

end.
