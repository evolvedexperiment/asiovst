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

unit AsioDemoForm;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC} LCLType, Buttons, {$ELSE} Windows, {$ENDIF} Forms, Classes,
  Controls, SysUtils, StdCtrls, DAV_Complex, DAV_Types, DAV_ASIOHost,
  DAV_DspSimpleOscillator;

type
  TFormASIO = class(TForm)
    ASIOHost: TASIOHost;
    ButtonControlPanel: TButton;
    ButtonStartStop: TButton;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    LabelChannels: TLabel;
    LabelCopyright: TLabel;
    LabelDrivername: TLabel;
    LabelFreq: TLabel;
    LabelPanorama: TLabel;
    LabelVolume: TLabel;
    ScrollBarFreq: TScrollBar;
    ScrollBarPan: TScrollBar;
    ScrollBarVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostBufferSwitch64(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxChannelChange(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure ScrollBarFreqChange(Sender: TObject);
    procedure ScrollBarPanChange(Sender: TObject);
    procedure ScrollBarVolumeChange(Sender: TObject);
  private
    procedure SetFrequency(const Value: Double);
    procedure SetAmplitude(const Value: Double);
  protected
    procedure AmplitudeChanged; virtual;
    procedure FrequencyChanged; virtual;
  public
    FIniFileName: TFileName;
    FOscillator: TSimpleOscillator64;
    FPan, FFreq, FAmp: Double;
    FChannelOffset: Byte;
  published
    property Frequency: Double read FFreq write SetFrequency;
    property Amplitude: Double read FAmp write SetAmplitude;
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

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFormASIO.FormCreate(Sender: TObject);
begin
  ComboBoxDriver.Items := ASIOHost.DriverList;

  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create(RCStrNoASIODriverPresent);
    except
      Application.Terminate;
    end;

  FIniFileName := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFileName) do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);
      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);
      ComboBoxChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
      FChannelOffset := ComboBoxChannel.ItemIndex * 2;
    finally
      Free;
    end;

  FAmp := 1;
  FFreq := 1000;
  FPan := 0.5;
  FChannelOffset := 0;

  FOscillator := TSimpleOscillator64.Create;
  with FOscillator do
  begin
    Frequency := FFreq;
    Amplitude := FAmp;
    SampleRate := ASIOHost.SampleRate;
  end;
end;

procedure TFormASIO.ComboBoxDriverChange(Sender: TObject);
var
  Channel: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for Channel := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
    begin
      ComboBoxChannel.Items.Add(string(ASIOHost.OutputChannelInfos[2 * Channel].Name)
        + ' / ' + string(ASIOHost.OutputChannelInfos[2 * Channel + 1].Name));
    end;

    // store current ASIO driver index
    with TIniFile.Create(FIniFileName) do
      try
        WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
      finally
        Free;
      end;
    ButtonControlPanel.Enabled := True;
    ButtonStartStop.Enabled := True;
    ComboBoxChannel.ItemIndex := 0;
  end;
end;

procedure TFormASIO.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.Active := False;
  ASIOHost.ControlPanel;
end;

procedure TFormASIO.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(FIniFileName) do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
      WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
    finally
      Free;
    end;

  ASIOHost.Active := False;
  FreeAndNil(FOscillator);
end;

procedure TFormASIO.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Caption = '&Start Audio' then
  begin
    ASIOHost.Active := True; // Start Audio
    ButtonStartStop.Caption := '&Stop Audio';
  end
  else
  begin
    ASIOHost.Active := False; // Stop Audio
    ButtonStartStop.Caption := '&Start Audio';
  end;
end;

procedure TFormASIO.ComboBoxChannelChange(Sender: TObject);
begin
  FChannelOffset := ComboBoxChannel.ItemIndex * 2;
end;

procedure TFormASIO.ScrollBarFreqChange(Sender: TObject);
begin
  Frequency := FreqLinearToLog(ScrollBarFreq.Position * 0.00001);
end;

procedure TFormASIO.SetAmplitude(const Value: Double);
begin
  if FAmp <> Value then
  begin
    FAmp := Value;
    AmplitudeChanged;
  end;
end;

procedure TFormASIO.SetFrequency(const Value: Double);
begin
  if FFreq <> Value then
  begin
    FFreq := Value;
    FrequencyChanged;
  end;
end;

procedure TFormASIO.AmplitudeChanged;
begin
  FOscillator.Amplitude := FAmp;
  if FAmp = 0 then
    LabelVolume.Caption := 'Volume: 0 equals -oo dB'
  else
    LabelVolume.Caption := 'Volume: ' + FloatToStrF(FAmp, ffFixed, 2, 2) +
      ' equals ' + FloatToStrF(Amp_to_dB(FAmp), ffGeneral, 2, 2) + ' dB';
end;

procedure TFormASIO.FrequencyChanged;
begin
  FOscillator.Frequency := FFreq;
  LabelFreq.Caption := 'Frequency: ' + FloatToStrF(FFreq, ffGeneral, 5, 5) + ' Hz';
end;

procedure TFormASIO.ScrollBarVolumeChange(Sender: TObject);
begin
  Amplitude := ScrollBarVolume.Position * 0.00001;
end;

procedure TFormASIO.ScrollBarPanChange(Sender: TObject);
begin
  FPan := ScrollBarPan.Position * 0.01;
  if FPan = 0.5 then
    LabelPanorama.Caption := 'Panorama: C'
  else
    LabelPanorama.Caption := 'Panorama: ' + IntToStr(Round(100 * (FPan * 2 - 1)));
end;

procedure TFormASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
  if Assigned(FOscillator) then
    FOscillator.SampleRate := ASIOHost.SampleRate;
end;

procedure TFormASIO.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample: Integer;
  L, R: Integer;
begin
  L := FChannelOffset;
  R := L + 1;
  for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
    OutBuffer[L, Sample] := (1 - FPan) * FOscillator.Sine;
    OutBuffer[R, Sample] := FPan * FOscillator.Sine;
    FOscillator.CalculateNextSample;
  end;
end;

procedure TFormASIO.ASIOHostBufferSwitch64(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Sample: Integer;
  L, R: Integer;
begin
  L := FChannelOffset;
  R := L + 1;
  for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
    OutBuffer[L, Sample] := (1 - FPan) * FOscillator.Sine;
    OutBuffer[R, Sample] := FPan * FOscillator.Sine;
    FOscillator.CalculateNextSample;
  end;
end;

procedure TFormASIO.ASIOHostReset(Sender: TObject);
begin
  if ButtonStartStop.Caption = '&Stop Audio' then
    ASIOHost.Active := True;
end;

end.
