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

unit MultiSineGeneratorMain;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLType, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Sysutils, Controls, StdCtrls, ExtCtrls, DAV_Complex,
  DAV_Types, DAV_ASIOHost, DAV_GuiBaseControl, DAV_GuiLevelMeter,
  DAV_DspSimpleOscillator, DAV_GuiLED;

const
  CDefaultFrequencies: Array [0 .. 30] of Single = (20, 25, 31.5, 40, 50, 63,
    80, 100, 125, 160, 200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600,
    2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000, 12500, 16000, 20000);

type
  TFormMultiSineGenerator = class(TForm)
    ASIOHost: TASIOHost;
    ButtonAllOctaves: TButton;
    ButtonAllThirdOctaves: TButton;
    ButtonControlPanel: TButton;
    ButtonMute: TButton;
    ButtonStartStop: TButton;
    CheckBoxLinkChannels: TCheckBox;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    Label0L: TLabel;
    Label0R: TLabel;
    Label100: TLabel;
    Label10kHz: TLabel;
    Label125: TLabel;
    Label12AL: TLabel;
    Label12AR: TLabel;
    Label12k5Hz: TLabel;
    Label160: TLabel;
    Label16kHz: TLabel;
    Label1k: TLabel;
    Label1k25: TLabel;
    Label1k6: TLabel;
    Label200: TLabel;
    Label20Hz: TLabel;
    Label20kHz: TLabel;
    Label24AL: TLabel;
    Label24AR: TLabel;
    Label250: TLabel;
    Label25Hz: TLabel;
    Label2k: TLabel;
    Label2k5: TLabel;
    Label315: TLabel;
    Label31Hz: TLabel;
    Label3k15: TLabel;
    Label400: TLabel;
    Label40Hz: TLabel;
    Label4k: TLabel;
    Label50: TLabel;
    Label500: TLabel;
    Label5k: TLabel;
    Label63: TLabel;
    Label630: TLabel;
    Label6k3Hz: TLabel;
    Label80: TLabel;
    Label800: TLabel;
    Label8kHz: TLabel;
    LabelChannels: TLabel;
    LabelDrivername: TLabel;
    LabelLeftOrMid: TLabel;
    LabelRightOrSide: TLabel;
    LedClipL: TGuiLED;
    LedClipR: TGuiLED;
    MeterTimer: TTimer;
    MiddleL: TShape;
    MiddleR: TShape;
    PeakMeterLeft: TGuiColorLevelMeter;
    PeakMeterRight: TGuiColorLevelMeter;
    SB100L: TScrollBar;
    SB100R: TScrollBar;
    SB10kL: TScrollBar;
    SB10kR: TScrollBar;
    SB125L: TScrollBar;
    SB125R: TScrollBar;
    SB12k5L: TScrollBar;
    SB12k5R: TScrollBar;
    SB160L: TScrollBar;
    SB160R: TScrollBar;
    SB16kL: TScrollBar;
    SB16kR: TScrollBar;
    SB1k25L: TScrollBar;
    SB1k25R: TScrollBar;
    SB1k6L: TScrollBar;
    SB1k6R: TScrollBar;
    SB1kL: TScrollBar;
    SB1kR: TScrollBar;
    SB200L: TScrollBar;
    SB200R: TScrollBar;
    SB20kL: TScrollBar;
    SB20kR: TScrollBar;
    SB20L: TScrollBar;
    SB20R: TScrollBar;
    SB250L: TScrollBar;
    SB250R: TScrollBar;
    SB25L: TScrollBar;
    SB25R: TScrollBar;
    SB2k5L: TScrollBar;
    SB2k5R: TScrollBar;
    SB2kL: TScrollBar;
    SB2kR: TScrollBar;
    SB315L: TScrollBar;
    SB315R: TScrollBar;
    SB31L: TScrollBar;
    SB31R: TScrollBar;
    SB3k15L: TScrollBar;
    SB3k15R: TScrollBar;
    SB400L: TScrollBar;
    SB400R: TScrollBar;
    SB40L: TScrollBar;
    SB40R: TScrollBar;
    SB4kL: TScrollBar;
    SB4kR: TScrollBar;
    SB500L: TScrollBar;
    SB500R: TScrollBar;
    SB50L: TScrollBar;
    SB50R: TScrollBar;
    SB5kL: TScrollBar;
    SB5kR: TScrollBar;
    SB630L: TScrollBar;
    SB630R: TScrollBar;
    SB63L: TScrollBar;
    SB63R: TScrollBar;
    SB6k3L: TScrollBar;
    SB6k3R: TScrollBar;
    SB800L: TScrollBar;
    SB800R: TScrollBar;
    SB80L: TScrollBar;
    SB80R: TScrollBar;
    SB8kL: TScrollBar;
    SB8kR: TScrollBar;
    ShapeBackText: TShape;
    ButtonExport: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostBufferSwitch64(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonAllOctavesClick(Sender: TObject);
    procedure ButtonAllThirdOctavesClick(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ButtonExportClick(Sender: TObject);
    procedure ButtonMuteClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxChannelChange(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure LabelFrequencyDblClick(Sender: TObject);
    procedure LedClipLClick(Sender: TObject);
    procedure LedClipRClick(Sender: TObject);
    procedure MeterTimerTimer(Sender: TObject);
    procedure SBVolumeChange(Sender: TObject);
  private
    procedure CalculatePeakDecay;
    procedure ExportToFilename(Filename: TFilename);
  public
    FOscillators: array [0 .. 30] of TSimpleOscillator64;
    FVolume: array [0 .. 1, 0 .. 30] of Double;
    FPeak: array [0 .. 1] of Double;
    FPeakDecay: Double;
    FChannelOffset: Byte;
  published
  end;

var
  FormMultiSineGenerator: TFormMultiSineGenerator;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Inifiles, Dialogs, DAV_Approximations, DAV_Common, DAV_Math, DAV_AudioFile,
  DAV_AudioData, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU,
  MultiSineGeneratorFrequency;

procedure TFormMultiSineGenerator.FormCreate(Sender: TObject);
var
  BandIndex: Integer;
begin
  ComboBoxDriver.Items := ASIOHost.DriverList;
  LedClipL.Brightness_Percent := 0;
  LedClipR.Brightness_Percent := 0;

  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create('No ASIO Driver present! Application Terminated!');
    except
      Application.Terminate;
    end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) +
    'MultiSineGenerator.INI') do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);
      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);
      CalculatePeakDecay;
      ComboBoxChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
      FChannelOffset := ComboBoxChannel.ItemIndex * 2;
    finally
      Free;
    end;

  for BandIndex := 0 to Length(CDefaultFrequencies) - 1 do
  begin
    FOscillators[BandIndex] := TSimpleOscillator64.Create;
    with FOscillators[BandIndex] do
    begin
      Frequency := CDefaultFrequencies[BandIndex];
      SampleRate := ASIOHost.SampleRate;
    end;
  end;
end;

procedure TFormMultiSineGenerator.FormDestroy(Sender: TObject);
var
  BandIndex: Integer;
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) +
    'MultiSineGenerator.INI') do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
      WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
    finally
      Free;
    end;

  ASIOHost.Active := False;

  for BandIndex := 0 to Length(FOscillators) - 1 do
    FreeAndNil(FOscillators[BandIndex]);
end;

procedure TFormMultiSineGenerator.LabelFrequencyDblClick(Sender: TObject);
var
  FormatSettings: TFormatSettings;
  ShiftAllFreqs: Boolean;
  NewFrequency: Single;
  Ratio: Single;
  Band: Integer;
begin
  GetLocaleFormatSettings(1033, FormatSettings);
  with TFormSetFrequency.Create(Self) do
    try
      EditFrequency.Text := FloatToStr(FOscillators[TLabel(Sender).Tag].Frequency);
      ShiftAllFreqs := ssShift in KeyboardStateToShiftState;

      if ShowModal = mrOK then
        try
          NewFrequency := StrToFloat(EditFrequency.Text);
          if ShiftAllFreqs then
          begin
            Ratio := NewFrequency / FOscillators[TLabel(Sender).Tag].Frequency;
            for Band := 0 to Length(FOscillators) - 1 do
            begin
              NewFrequency := FOscillators[Band].Frequency * Ratio;
              FOscillators[Band].Frequency := NewFrequency;
              (*
              if NewFrequency > 1000 then
                TLabel(Sender).Caption := FloatToStrF(1E-3 * NewFrequency, ffGeneral, 3, 3, FormatSettings) + ' kHz'
              else
                TLabel(Sender).Caption := FloatToStrF(       NewFrequency, ffGeneral, 3, 3, FormatSettings) + ' Hz';
              *)
            end;
          end
          else
          begin
            FOscillators[TLabel(Sender).Tag].Frequency := NewFrequency;
            if NewFrequency > 1000 then
              TLabel(Sender).Caption := FloatToStrF(1E-3 * NewFrequency,
                ffGeneral, 3, 3, FormatSettings) + ' kHz'
            else
              TLabel(Sender).Caption := FloatToStrF(NewFrequency, ffGeneral, 3,
                3, FormatSettings) + ' Hz';
          end;
        except
          on E: EConvertError do
            MessageDlg(E.Message, mtError, [mbOK], 0);
        end;
    finally
      Free;
    end;
end;

procedure TFormMultiSineGenerator.LedClipLClick(Sender: TObject);
begin
  LedClipL.Brightness_Percent := 0;
end;

procedure TFormMultiSineGenerator.LedClipRClick(Sender: TObject);
begin
  LedClipR.Brightness_Percent := 0;
end;

procedure TFormMultiSineGenerator.MeterTimerTimer(Sender: TObject);
begin
  if FPeak[0] > 1 then
    LedClipL.Brightness_Percent := 100;
  if FPeak[1] > 1 then
    LedClipR.Brightness_Percent := 100;

  PeakMeterLeft.PeakLevel := FPeak[0];
  PeakMeterRight.PeakLevel := FPeak[1];
end;

procedure TFormMultiSineGenerator.ComboBoxDriverChange(Sender: TObject);
var
  ChannelIndex: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for ChannelIndex := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
    begin
      ComboBoxChannel.Items.Add(string(ASIOHost.OutputChannelInfos[2 * ChannelIndex]
        .Name) + ' / ' + string(ASIOHost.OutputChannelInfos[2 * ChannelIndex +
        1].Name));
    end;

    // store current ASIO driver index
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) +
      'MultiSineGenerator.INI') do
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

procedure TFormMultiSineGenerator.ButtonAllOctavesClick(Sender: TObject);
begin
  SB20L.Position := 100;
  SB25L.Position := 100;
  SB31L.Position := 85;
  SB40L.Position := 100;
  SB50L.Position := 100;
  SB63L.Position := 85;
  SB80L.Position := 100;
  SB100L.Position := 100;
  SB125L.Position := 85;
  SB160L.Position := 100;
  SB200L.Position := 100;
  SB250L.Position := 85;
  SB315L.Position := 100;
  SB400L.Position := 100;
  SB500L.Position := 85;
  SB630L.Position := 100;
  SB800L.Position := 100;
  SB1kL.Position := 85;
  SB1k25L.Position := 100;
  SB1k6L.Position := 100;
  SB2kL.Position := 85;
  SB2k5L.Position := 100;
  SB3k15L.Position := 100;
  SB4kL.Position := 85;
  SB5kL.Position := 100;
  SB6k3L.Position := 100;
  SB8kL.Position := 85;
  SB10kL.Position := 100;
  SB12k5L.Position := 100;
  SB16kL.Position := 85;
  SB20kL.Position := 100;

  SB20R.Position := 100;
  SB25R.Position := 100;
  SB31R.Position := 85;
  SB40R.Position := 100;
  SB50R.Position := 100;
  SB63R.Position := 85;
  SB80R.Position := 100;
  SB100R.Position := 100;
  SB125R.Position := 85;
  SB160R.Position := 100;
  SB200R.Position := 100;
  SB250R.Position := 85;
  SB315R.Position := 100;
  SB400R.Position := 100;
  SB500R.Position := 85;
  SB630R.Position := 100;
  SB800R.Position := 100;
  SB1kR.Position := 85;
  SB1k25R.Position := 100;
  SB1k6R.Position := 100;
  SB2kR.Position := 85;
  SB2k5R.Position := 100;
  SB3k15R.Position := 100;
  SB4kR.Position := 85;
  SB5kR.Position := 100;
  SB6k3R.Position := 100;
  SB8kR.Position := 85;
  SB10kR.Position := 100;
  SB12k5R.Position := 100;
  SB16kR.Position := 85;
  SB20kR.Position := 100;
end;

procedure TFormMultiSineGenerator.ButtonAllThirdOctavesClick(Sender: TObject);
begin
  SB20L.Position := 93;
  SB25L.Position := 93;
  SB31L.Position := 93;
  SB40L.Position := 93;
  SB50L.Position := 93;
  SB63L.Position := 93;
  SB80L.Position := 93;
  SB100L.Position := 93;
  SB125L.Position := 93;
  SB160L.Position := 93;
  SB200L.Position := 93;
  SB250L.Position := 93;
  SB315L.Position := 93;
  SB400L.Position := 93;
  SB500L.Position := 93;
  SB630L.Position := 93;
  SB800L.Position := 93;
  SB1kL.Position := 93;
  SB1k25L.Position := 93;
  SB1k6L.Position := 93;
  SB2kL.Position := 93;
  SB2k5L.Position := 93;
  SB3k15L.Position := 93;
  SB4kL.Position := 93;
  SB5kL.Position := 93;
  SB6k3L.Position := 93;
  SB8kL.Position := 93;
  SB10kL.Position := 93;
  SB12k5L.Position := 93;
  SB16kL.Position := 93;
  SB20kL.Position := 93;

  SB20R.Position := 93;
  SB25R.Position := 93;
  SB31R.Position := 93;
  SB40R.Position := 93;
  SB50R.Position := 93;
  SB63R.Position := 93;
  SB80R.Position := 93;
  SB100R.Position := 93;
  SB125R.Position := 93;
  SB160R.Position := 93;
  SB200R.Position := 93;
  SB250R.Position := 93;
  SB315R.Position := 93;
  SB400R.Position := 93;
  SB500R.Position := 93;
  SB630R.Position := 93;
  SB800R.Position := 93;
  SB1kR.Position := 93;
  SB1k25R.Position := 93;
  SB1k6R.Position := 93;
  SB2kR.Position := 93;
  SB2k5R.Position := 93;
  SB3k15R.Position := 93;
  SB4kR.Position := 93;
  SB5kR.Position := 93;
  SB6k3R.Position := 93;
  SB8kR.Position := 93;
  SB10kR.Position := 93;
  SB12k5R.Position := 93;
  SB16kR.Position := 93;
  SB20kR.Position := 93;
end;

procedure TFormMultiSineGenerator.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormMultiSineGenerator.ButtonExportClick(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
    try
      Title := 'Export Audio File...';
      Filter := 'Wave (*.wav)|*.wav|AIFF (*.aiff)|*.aiff|AU (*.au)|*.au';
      DefaultExt := '.wav';
      if Execute then
        ExportToFilename(Filename);
    finally
      Free;
    end;
end;

procedure TFormMultiSineGenerator.ExportToFilename(Filename: TFilename);
var
  SampleIndex: Integer;
  BandIndex: Integer;
  Data: Double;
begin
  with TAudioDataCollection32.Create(nil) do
    try
      ChannelCount := 2;
      SampleRate := ASIOHost.SampleRate;
      SampleFrames := Round(20 * SampleRate);

      for SampleIndex := 0 to SampleFrames - 1 do
      begin
        // left channel
        Data := FVolume[0, 0] * FOscillators[0].Sine;
        for BandIndex := 1 to Length(FOscillators) - 1 do
          Data := Data + FVolume[0, BandIndex] * FOscillators[BandIndex].Sine;
        ChannelDataPointer[0]^[SampleIndex] := Data;

        // right channel
        Data := FVolume[1, 0] * FOscillators[0].Sine;
        FOscillators[0].CalculateNextSample;
        for BandIndex := 1 to Length(FOscillators) - 1 do
        begin
          Data := Data + FVolume[1, BandIndex] * FOscillators[BandIndex].Sine;
          FOscillators[BandIndex].CalculateNextSample;
        end;
        ChannelDataPointer[1]^[SampleIndex] := Data;
      end;

      SaveToFile(Filename);
    finally
      Free;
    end;
end;

procedure TFormMultiSineGenerator.ButtonMuteClick(Sender: TObject);
begin
  SB20L.Position := 100;
  SB25L.Position := 100;
  SB31L.Position := 100;
  SB40L.Position := 100;
  SB50L.Position := 100;
  SB63L.Position := 100;
  SB80L.Position := 100;
  SB100L.Position := 100;
  SB125L.Position := 100;
  SB160L.Position := 100;
  SB200L.Position := 100;
  SB250L.Position := 100;
  SB315L.Position := 100;
  SB400L.Position := 100;
  SB500L.Position := 100;
  SB630L.Position := 100;
  SB800L.Position := 100;
  SB1kL.Position := 100;
  SB1k25L.Position := 100;
  SB1k6L.Position := 100;
  SB2kL.Position := 100;
  SB2k5L.Position := 100;
  SB3k15L.Position := 100;
  SB4kL.Position := 100;
  SB5kL.Position := 100;
  SB6k3L.Position := 100;
  SB8kL.Position := 100;
  SB10kL.Position := 100;
  SB12k5L.Position := 100;
  SB16kL.Position := 100;
  SB20kL.Position := 100;
  SB20R.Position := 100;
  SB25R.Position := 100;
  SB31R.Position := 100;
  SB40R.Position := 100;
  SB50R.Position := 100;
  SB63R.Position := 100;
  SB80R.Position := 100;
  SB100R.Position := 100;
  SB125R.Position := 100;
  SB160R.Position := 100;
  SB200R.Position := 100;
  SB250R.Position := 100;
  SB315R.Position := 100;
  SB400R.Position := 100;
  SB500R.Position := 100;
  SB630R.Position := 100;
  SB800R.Position := 100;
  SB1kR.Position := 100;
  SB1k25R.Position := 100;
  SB1k6R.Position := 100;
  SB2kR.Position := 100;
  SB2k5R.Position := 100;
  SB3k15R.Position := 100;
  SB4kR.Position := 100;
  SB5kR.Position := 100;
  SB6k3R.Position := 100;
  SB8kR.Position := 100;
  SB10kR.Position := 100;
  SB12k5R.Position := 100;
  SB16kR.Position := 100;
  SB20kR.Position := 100;
end;

procedure TFormMultiSineGenerator.SBVolumeChange(Sender: TObject);
var
  Channel: Integer;
begin
  if Sender is TScrollBar then
    with TScrollBar(Sender) do
    begin
      Channel := Tag mod 31;
      FVolume[Channel, Tag - 31 * Channel] := 1 - 0.01 * Position;
    end;
end;

procedure TFormMultiSineGenerator.ButtonStartStopClick(Sender: TObject);
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

procedure TFormMultiSineGenerator.ComboBoxChannelChange(Sender: TObject);
begin
  FChannelOffset := ComboBoxChannel.ItemIndex * 2;
end;

procedure TFormMultiSineGenerator.ASIOHostSampleRateChanged(Sender: TObject);
var
  BandIndex: Integer;
begin
  for BandIndex := 0 to Length(FOscillators) - 1 do
    FOscillators[BandIndex].SampleRate := ASIOHost.SampleRate;
  CalculatePeakDecay
end;

procedure TFormMultiSineGenerator.CalculatePeakDecay;
begin
  // fixed time = 4 second
  FPeakDecay := FastPower2ContinousError3(-ASIOHost.BufferSize /
    (4 * ASIOHost.SampleRate));
end;

procedure TFormMultiSineGenerator.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  SampleIndex: Integer;
  BandIndex: Integer;
  Data: Double;
begin
  for SampleIndex := 0 to ASIOHost.BufferSize - 1 do
  begin
    // left channel
    Data := FVolume[0, 0] * FOscillators[0].Sine;
    for BandIndex := 1 to Length(FOscillators) - 1 do
      Data := Data + FVolume[0, BandIndex] * FOscillators[BandIndex].Sine;
    OutBuffer[FChannelOffset, SampleIndex] := Data;
    if Abs(Data) > FPeak[0] then
      FPeak[0] := Abs(Data);

    // right channel
    Data := FVolume[1, 0] * FOscillators[0].Sine;
    FOscillators[0].CalculateNextSample;
    for BandIndex := 1 to Length(FOscillators) - 1 do
    begin
      Data := Data + FVolume[1, BandIndex] * FOscillators[BandIndex].Sine;
      FOscillators[BandIndex].CalculateNextSample;
    end;
    OutBuffer[FChannelOffset + 1, SampleIndex] := Data;
    if Abs(Data) > FPeak[1] then
      FPeak[1] := Abs(Data);
  end;
  FPeak[0] := FPeakDecay * FPeak[0];
  FPeak[1] := FPeakDecay * FPeak[1];
end;

procedure TFormMultiSineGenerator.ASIOHostBufferSwitch64(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  SampleIndex: Integer;
  BandIndex: Integer;
  Data: Double;
begin
  for SampleIndex := 0 to ASIOHost.BufferSize - 1 do
  begin
    // left channel
    Data := FVolume[0, 0] * FOscillators[0].Sine;
    for BandIndex := 1 to Length(FOscillators) - 1 do
      Data := Data + FVolume[0, BandIndex] * FOscillators[BandIndex].Sine;
    OutBuffer[FChannelOffset, SampleIndex] := Data;
    if Abs(Data) > FPeak[0] then
      FPeak[0] := Abs(Data);

    // right channel
    Data := FVolume[1, 0] * FOscillators[0].Sine;
    FOscillators[0].CalculateNextSample;
    for BandIndex := 1 to Length(FOscillators) - 1 do
    begin
      Data := Data + FVolume[1, BandIndex] * FOscillators[BandIndex].Sine;
      FOscillators[BandIndex].CalculateNextSample;
    end;
    OutBuffer[FChannelOffset + 1, SampleIndex] := Data;
    if Abs(Data) > FPeak[1] then
      FPeak[1] := Abs(Data);
  end;
  FPeak[0] := FPeakDecay * FPeak[0];
  FPeak[1] := FPeakDecay * FPeak[1];
end;

procedure TFormMultiSineGenerator.ASIOHostReset(Sender: TObject);
begin
  ASIOHost.Active := True;
end;

end.
