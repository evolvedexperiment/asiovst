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

unit SsgMain;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Complex, DAV_Types, DAV_ASIOHost,
  DAV_DspPinkNoiseGenerator, DAV_DspLfo;

type
  TSignalType = (stSine, stWhiteNoise, stPinkNoise);
  TNoiseDistribution = (ndRectangle, ndTriangular, ndFastGauss, ndGauss);

  TFormASIO = class(TForm)
    ASIOHost: TASIOHost;
    ButtonControlPanel: TButton;
    ButtonStartStop: TButton;
    ComboBoxDistribution: TComboBox;
    ComboBoxDriver: TComboBox;
    ComboBoxSignal: TComboBox;
    LabelCopyright: TLabel;
    LabelDistribution: TLabel;
    LabelDrivername: TLabel;
    LabelFreq: TLabel;
    LabelSignal: TLabel;
    LabelVolume: TLabel;
    ScrollBarFreq: TScrollBar;
    ScrollBarVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ASIOHostBufferSwitch64(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure ASIOHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure ComboBoxSignalChange(Sender: TObject);
    procedure ComboBoxDistributionChange(Sender: TObject);
    procedure ScrollBarFreqChange(Sender: TObject);
    procedure ScrollBarVolumeChange(Sender: TObject);
  private
    procedure SetFrequency(const Value: Double);
  public
    FFreq, FVol: Double;
    FSignalType: TSignalType;
    FSineLFO: TLFOSine64;
    FPinkNoise: TPinkNoiseGenerator;
    FNoiseDistribution: TNoiseDistribution;
  published
    property Frequency: Double read FFreq write SetFrequency;
    property SignalType: TSignalType read FSignalType;
    property NoiseDistribution: TNoiseDistribution read FNoiseDistribution;
  end;

var
  FormASIO: TFormASIO;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Common, DAV_Math, DAV_Approximations, DAV_Convert;

procedure TFormASIO.FormCreate(Sender: TObject);
begin
  ComboBoxDriver.Items := ASIOHost.DriverList;
  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create('No ASIO Driver present! Application Terminated!');
    except
      Application.Terminate;
    end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);
      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);
    finally
      Free;
    end;

  FFreq := 1000;
  FVol := 1;

  FSineLFO := TLFOSine64.Create;
  with FSineLFO do
  begin
    Frequency := FFreq;
    SampleRate := ASIOHost.SampleRate;
  end;

  FPinkNoise := TPinkNoiseGenerator.Create;
end;

procedure TFormASIO.FormDestroy(Sender: TObject);
begin
  ASIOHost.Active := False;
  FreeAndNil(FSineLFO);
  FreeAndNil(FPinkNoise);

  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;
end;

procedure TFormASIO.ComboBoxDistributionChange(Sender: TObject);
begin
  FNoiseDistribution := TNoiseDistribution(ComboBoxDistribution.ItemIndex);
end;

procedure TFormASIO.ComboBoxDriverChange(Sender: TObject);
begin
  ButtonControlPanel.Enabled := False;
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
      try
        WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
      finally
        Free;
      end;
    ButtonControlPanel.Enabled := True;
    ButtonStartStop.Enabled := True;
  end;
end;

procedure TFormASIO.ComboBoxSignalChange(Sender: TObject);
begin
  FSignalType := TSignalType(ComboBoxSignal.ItemIndex);

  ScrollBarFreq.Visible := FSignalType = stSine;
  LabelFreq.Visible := FSignalType = stSine;
  LabelDistribution.Visible := FSignalType = stWhiteNoise;
  ComboBoxDistribution.Visible := FSignalType = stWhiteNoise;
  case FSignalType of
    stSine:
      ClientHeight := 173;
    stWhiteNoise:
      ClientHeight := 155;
    stPinkNoise:
      ClientHeight := 132;
  end;
end;

procedure TFormASIO.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormASIO.ButtonStartStopClick(Sender: TObject);
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

procedure TFormASIO.ScrollBarFreqChange(Sender: TObject);
begin
  Frequency := FreqLinearToLog(ScrollBarFreq.Position * 0.00001);
end;

procedure TFormASIO.SetFrequency(const Value: Double);
begin
  if FFreq <> Value then
  begin
    FFreq := Value;
    LabelFreq.Caption := 'Frequency: ' + FloatToStrF(FFreq, ffGeneral, 5,
      5) + ' Hz';
    FSineLFO.Frequency := FFreq;
  end;
end;

procedure TFormASIO.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel: Integer;
  Sample: Integer;
begin
  case FSignalType of
    stSine:
      for Sample := 0 to ASIOHost.BufferSize - 1 do
      begin
        OutBuffer[0, Sample] := FSineLFO.Sine * FVol;
        FSineLFO.CalculateNextSample;
      end;
    stWhiteNoise:
      case FNoiseDistribution of
        ndRectangle:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := (2 * Random - 1) * FVol;
        ndTriangular:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := (Random - Random) * FVol;
        ndFastGauss:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := FastRandomGauss * FVol;
        ndGauss:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := RandomGauss * FVol;
      end;
    stPinkNoise:
      for Sample := 0 to ASIOHost.BufferSize - 1 do
        OutBuffer[0, Sample] := FPinkNoise.ProcessSample64;
  end;

  // copy signal to all channels
  for Channel := 1 to ASIOHost.OutputChannelCount - 1 do
    Move(OutBuffer[0, 0], OutBuffer[Channel, 0], ASIOHost.BufferSize *
      SizeOf(Single));
end;

procedure TFormASIO.ASIOHostBufferSwitch64(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Channel: Integer;
  Sample: Integer;
begin
  case FSignalType of
    stSine:
      for Sample := 0 to ASIOHost.BufferSize - 1 do
      begin
        OutBuffer[0, Sample] := FSineLFO.Sine * FVol;
        FSineLFO.CalculateNextSample;
      end;
    stWhiteNoise:
      case FNoiseDistribution of
        ndRectangle:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := (2 * Random - 1) * FVol;
        ndTriangular:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := (Random - Random) * FVol;
        ndFastGauss:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := FastRandomGauss * FVol;
        ndGauss:
          for Sample := 0 to ASIOHost.BufferSize - 1 do
            OutBuffer[0, Sample] := RandomGauss * FVol;
      end;
    stPinkNoise:
      for Sample := 0 to ASIOHost.BufferSize - 1 do
        OutBuffer[0, Sample] := FPinkNoise.ProcessSample64;
  end;

  // copy signal to all channels
  for Channel := 1 to ASIOHost.OutputChannelCount - 1 do
    Move(OutBuffer[0, 0], OutBuffer[Channel, 0], ASIOHost.BufferSize *
      SizeOf(Double));
end;

procedure TFormASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
  FSineLFO.SampleRate := ASIOHost.SampleRate;
end;

procedure TFormASIO.ScrollBarVolumeChange(Sender: TObject);
begin
  FVol := ScrollBarVolume.Position * 0.00001;
  if FVol = 0 then
    LabelVolume.Caption := 'Volume: 0 equals -oo dB'
  else
    LabelVolume.Caption := 'Volume: ' + FloatToStrF(FVol, ffFixed, 2, 2) +
      ' equals ' + FloatToStrF(Amp_to_dB(FVol), ffGeneral, 2, 2) + ' dB';
end;

end.
