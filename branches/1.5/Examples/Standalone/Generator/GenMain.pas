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

unit GenMain;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LMessages, {$ELSE}Windows, Messages, {$ENDIF} SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Spin, ComCtrls,
  ExtCtrls, Buttons, SyncObjs, DAV_Types, DAV_Classes, DAV_GuiBaseControl,
  DAV_GuiAudioDataDisplay, DAV_GuiLED, DAV_ASIOHost, DAV_DspPinkNoiseGenerator,
  DAV_DspSimpleOscillator, DAV_DspSweepOscillator, DAV_AudioData, DAV_AudioFile,
  DAV_AudioFileWav, DAV_AudioFileAiff, DAV_AudioFileAu,
  DAV_DspBufferedAudioFilePlayer;

type
  TFadeDirection = (fdNone, fdUp, fdDown);

  TFormGenerator = class(TForm)
    ADC: TAudioDataCollection32;
    ASIOHost: TASIOHost;
    AudioDisplayTimeDomain: TGuiAudioDataDisplay;
    ButtonPause: TSpeedButton;
    ButtonPlay: TSpeedButton;
    ButtonSelectWavFile: TButton;
    ButtonStop: TSpeedButton;
    CheckBoxIdenticalChannelsSine: TCheckBox;
    CheckBoxIdenticalChannelsSweep: TCheckBox;
    CheckBoxTimeLimit: TCheckBox;
    ComboBoxChannelSine: TComboBox;
    ComboBoxChannelSweep: TComboBox;
    ComboBoxDriver: TComboBox;
    ComboBoxSine: TComboBox;
    EditEndFreq: TEdit;
    EditFrequency: TEdit;
    EditGainSweep: TEdit;
    EditInitialPhase: TEdit;
    EditSineGain: TEdit;
    EditStartFreq: TEdit;
    EditSweepModulationTime: TEdit;
    EditWavFile: TEdit;
    GroupBoxParameterSine: TGroupBox;
    GroupBoxSweep: TGroupBox;
    GuiTimer: TTimer;
    LabelChannelCount: TLabel;
    LabelDistribution: TLabel;
    LabelEndFreq: TLabel;
    LabelFrequency: TLabel;
    LabelGain: TLabel;
    LabelGainSweep: TLabel;
    LabelInitialPhase: TLabel;
    LabelM30: TLabel;
    LabelModulationTime: TLabel;
    LabelNoChannelSine: TLabel;
    LabelNoChannelSweep: TLabel;
    LabelP30: TLabel;
    LabelPinkNoiseGain: TLabel;
    LabelSineCount: TLabel;
    LabelSineNo: TLabel;
    LabelStartFreq: TLabel;
    LabelTime: TLabel;
    LabelVolume: TLabel;
    LabelWhiteNoiseGain: TLabel;
    LabelZero: TLabel;
    LED1: TGuiLED;
    LED2: TGuiLED;
    LED3: TGuiLED;
    LED4: TGuiLED;
    LED5: TGuiLED;
    LED6: TGuiLED;
    OpenWavDialog: TOpenDialog;
    PageControlSelect: TPageControl;
    PanelTime: TPanel;
    PanelTimeDomain: TPanel;
    RadioButtonFallingSweep: TRadioButton;
    RadioButtonRectangle: TRadioButton;
    RadioButtonRisingSweep: TRadioButton;
    RadioButtonTriangle: TRadioButton;
    SpinEditChannelCount: TSpinEdit;
    SpinEditPinkNoiseGain: TSpinEdit;
    SpinEditSineCount: TSpinEdit;
    SpinEditWhiteNoiseGain: TSpinEdit;
    TabSheetPinkNoise: TTabSheet;
    TabSheetSine: TTabSheet;
    TabSheetSweep: TTabSheet;
    TabSheetWavFile: TTabSheet;
    TabSheetWhiteNoise: TTabSheet;
    TrackBarVolume: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BufferSwitchWhiteNoise32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchPinkNoise32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchSine32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchSweep32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchWave32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonSelectWavFileClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure CbSineChannelChange(Sender: TObject);
    procedure CheckBoxIdenticalChannelsSineClick(Sender: TObject);
    procedure CheckBoxIdenticalChannelsSweepClick(Sender: TObject);
    procedure ComboBoxChannelSweepChange(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure EditEndFreqChange(Sender: TObject);
    procedure EditFrequencyChange(Sender: TObject);
    procedure EditGainSweepChange(Sender: TObject);
    procedure EditInitialPhaseChange(Sender: TObject);
    procedure EditSineGainChange(Sender: TObject);
    procedure EditStartFreqChange(Sender: TObject);
    procedure EditSweepModulationTimeChange(Sender: TObject);
    procedure EditWavFileChange(Sender: TObject);
    procedure GuiTimerTimer(Sender: TObject);
    procedure PageControlSelectChange(Sender: TObject);
    procedure RadioButtonFallingSweepClick(Sender: TObject);
    procedure RadioButtonRisingSweepClick(Sender: TObject);
    procedure SpinEditChannelCountChange(Sender: TObject);
    procedure SpinEditPinkNoiseGainChange(Sender: TObject);
    procedure SpinEditSineCountChange(Sender: TObject);
    procedure SpinEditWhiteNoiseGainChange(Sender: TObject);
    procedure TrackBarVolumeChange(Sender: TObject);
  private
    FIniFile: TFileName;
    FCriticalSection: TCriticalSection;
    FMainGain: Double;
    FFadeGain: Double;
    FFadeFactor: Double;
    FFadeDirection: TFadeDirection;
    FPinkNoise: array of TPinkNoiseGenerator;
    FSineOsc: array of array of TSimpleOscillator;
    FSweepOsc: array of TRangeSweepOscillator64;
    FAudioFile: TBufferedAudioFilePlayer;
    FChannelCount: Integer;
    FSineCount: Integer;
    FSweepCount: Integer;
    FWhiteNoiseGain: Double;
    FPinkNoiseGain: Double;
    FTime: Double;
    FPeak: Double;
    procedure SetChannelCount(const Value: Integer);
    procedure SetSineCount(const Value: Integer);
    function GetCurrentGain: Single;
  protected
    procedure ChannelCountChanged; virtual;
    procedure SineCountChanged; virtual;
    procedure UpdateTime;
    procedure CalculatePeak(Data: Single);
  public
    property ChannelCount: Integer read FChannelCount write SetChannelCount;
    property SineCount: Integer read FSineCount write SetSineCount;
    property CurrentGain: Single read GetCurrentGain;
  end;

var
  FormGenerator: TFormGenerator;

implementation

uses
  Math, IniFiles, DAV_Common;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFormGenerator.FormCreate(Sender: TObject);
begin
  FIniFile := ExtractFilePath(ParamStr(0)) + 'Generator.ini';
  FCriticalSection := TCriticalSection.Create;
  FSineCount := 1;
  FChannelCount := 1;
  FWhiteNoiseGain := 1;
  FPinkNoiseGain := 1;
  FFadeGain := 1;
  FFadeFactor := 1.001;
  FFadeDirection := fdUp;

  ChannelCountChanged;
  TrackBarVolumeChange(Sender);

  FAudioFile := TBufferedAudioFilePlayer.Create;
  with FAudioFile do
  begin
    SampleRate := ASIOHost.SampleRate;
    Interpolation := biLinear;
  end;

  ComboBoxDriver.Items := ASIOHost.DriverList;
  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create('No ASIO Driver present! Application Terminated!');
    except
      Application.Terminate;
    end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFile) do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);
      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);

      EditWavFile.Text := ReadString('Recent', 'Audio File', EditWavFile.Text);
    finally
      Free;
    end;
end;

procedure TFormGenerator.FormDestroy(Sender: TObject);
var
  Channel: Integer;
  Band: Integer;
begin
  // stop audio processing
  ASIOHost.Active := False;
  GuiTimer.Enabled := False;

  with TIniFile.Create(FIniFile) do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
      WriteString('Recent', 'Audio File', EditWavFile.Text);
    finally
      Free;
    end;

  FreeAndNil(FAudioFile);

  for Channel := 0 to Length(FPinkNoise) - 1 do
    FreeAndNil(FPinkNoise[Channel]);
  for Channel := 0 to Length(FSineOsc) - 1 do
    for Band := 0 to Length(FSineOsc[Channel]) - 1 do
      FreeAndNil(FSineOsc[Channel, Band]);
  for Channel := 0 to Length(FSweepOsc) - 1 do
    FreeAndNil(FSweepOsc[Channel]);
  FreeAndNil(FCriticalSection);
end;

procedure TFormGenerator.ComboBoxDriverChange(Sender: TObject);
begin
  ButtonPlay.Enabled := False;
  ButtonStop.Enabled := False;
  ButtonPause.Enabled := False;
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
    ButtonPlay.Enabled := True;
    ButtonPause.Enabled := True;
    ButtonStop.Enabled := True;
  end;
end;

procedure TFormGenerator.EditEndFreqChange(Sender: TObject);
var
  Channel: Integer;
  Freq: Single;
begin
  try
    Freq := StrToFloat(EditEndFreq.Text);
  except
    Exit;
  end;

  if CheckBoxIdenticalChannelsSweep.Checked then
    for Channel := 0 to FChannelCount - 1 do
      FSweepOsc[Channel].StopFrequency := Freq
  else
  begin
    Channel := ComboBoxChannelSine.ItemIndex;
    FSweepOsc[Channel].StopFrequency := Freq;
  end;
end;

procedure TFormGenerator.EditFrequencyChange(Sender: TObject);
var
  SineNo: Integer;
  Channel: Integer;
  Freq: Single;
begin
  try
    Freq := StrToFloat(EditFrequency.Text);
  except
    Exit;
  end;

  SineNo := ComboBoxSine.ItemIndex;
  if CheckBoxIdenticalChannelsSine.Checked then
    for Channel := 0 to FChannelCount - 1 do
      FSineOsc[Channel, SineNo].Frequency := Freq
  else
  begin
    Channel := ComboBoxChannelSine.ItemIndex;
    FSineOsc[Channel, SineNo].Frequency := Freq;
  end;
end;

procedure TFormGenerator.EditGainSweepChange(Sender: TObject);
var
  Channel: Integer;
  Amp: Single;
begin
  try
    Amp := db_to_Amp(StrToFloat(EditGainSweep.Text));
  except
    Exit;
  end;

  if CheckBoxIdenticalChannelsSweep.Checked then
    for Channel := 0 to FChannelCount - 1 do
      FSweepOsc[Channel].Amplitude := Amp
  else
  begin
    Channel := ComboBoxChannelSine.ItemIndex;
    FSweepOsc[Channel].Amplitude := Amp;
  end;
end;

procedure TFormGenerator.EditInitialPhaseChange(Sender: TObject);
var
  SineNo: Integer;
  Channel: Integer;
  Phase: Single;
begin
  try
    Phase := StrToFloat(EditInitialPhase.Text);
  except
    Exit;
  end;

  SineNo := ComboBoxSine.ItemIndex;
  if CheckBoxIdenticalChannelsSine.Checked then
    for Channel := 0 to FChannelCount - 1 do
      FSineOsc[Channel, SineNo].Phase := Phase
  else
  begin
    Channel := ComboBoxChannelSine.ItemIndex;
    FSineOsc[Channel, SineNo].Phase := Phase;
  end;
end;

procedure TFormGenerator.EditSineGainChange(Sender: TObject);
var
  SineNo: Integer;
  Channel: Integer;
  Amplitude: Single;
begin
  try
    Amplitude := db_to_Amp(StrToFloat(EditSineGain.Text));
  except
    Exit;
  end;

  SineNo := ComboBoxSine.ItemIndex;
  if CheckBoxIdenticalChannelsSine.Checked then
    for Channel := 0 to FChannelCount - 1 do
      FSineOsc[Channel, SineNo].Amplitude := Amplitude
  else
  begin
    Channel := ComboBoxChannelSine.ItemIndex;
    FSineOsc[Channel, SineNo].Amplitude := Amplitude;
  end;
end;

procedure TFormGenerator.EditStartFreqChange(Sender: TObject);
var
  Channel: Integer;
  Freq: Single;
begin
  try
    Freq := StrToFloat(EditStartFreq.Text);
  except
    Exit;
  end;

  if CheckBoxIdenticalChannelsSweep.Checked then
    for Channel := 0 to FChannelCount - 1 do
      FSweepOsc[Channel].StartFrequency := Freq
  else
  begin
    Channel := ComboBoxChannelSine.ItemIndex;
    FSweepOsc[Channel].StartFrequency := Freq;
  end;
end;

procedure TFormGenerator.EditSweepModulationTimeChange(Sender: TObject);
var
  Channel: Integer;
  ModTime: Single;
begin
  try
    ModTime := 0.001 * StrToFloat(EditSweepModulationTime.Text);
  except
    Exit;
  end;

  for Channel := 0 to FChannelCount - 1 do
    FSweepOsc[Channel].ModulationFrequency := 1 / ModTime;
end;

procedure TFormGenerator.EditWavFileChange(Sender: TObject);
begin
  if FileExists(EditWavFile.Text) then
    FAudioFile.Filename := EditWavFile.Text
end;

procedure TFormGenerator.PageControlSelectChange(Sender: TObject);
begin
  case PageControlSelect.ActivePageIndex of
    0:
      ASIOHost.OnBufferSwitch32 := BufferSwitchWhiteNoise32;
    1:
      ASIOHost.OnBufferSwitch32 := BufferSwitchPinkNoise32;
    2:
      ASIOHost.OnBufferSwitch32 := BufferSwitchSine32;
    3:
      ASIOHost.OnBufferSwitch32 := BufferSwitchSweep32;
    4:
      ASIOHost.OnBufferSwitch32 := BufferSwitchWave32;
  end;
end;

procedure TFormGenerator.RadioButtonFallingSweepClick(Sender: TObject);
var
  Channel: Integer;
  Freq: Single;
begin
  (*
    if CheckBoxIdenticalChannelsSweep.Checked then
      for Channel := 0 to FChannelCount - 1 do
        FSweepOsc[Channel].S
    else
    begin
      Channel := ComboBoxChannelSine.ItemIndex;
      FSweepOsc[Channel].StartFrequency := Freq;
    end;
  *)
end;

procedure TFormGenerator.RadioButtonRisingSweepClick(Sender: TObject);
var
  Channel: Integer;
  Freq: Single;
begin
  (*
    try
      Freq := StrToFloat(EditStartFreq.Text);
    except
      Exit;
    end;

    if CheckBoxIdenticalChannelsSweep.Checked then
      for Channel := 0 to FChannelCount - 1 do
        FSweepOsc[Channel].StartFrequency := Freq
    else
    begin
      Channel := ComboBoxChannelSine.ItemIndex;
      FSweepOsc[Channel].StartFrequency := Freq;
    end;
  *)
end;

procedure TFormGenerator.SpinEditChannelCountChange(Sender: TObject);
begin
  ChannelCount := SpinEditChannelCount.Value;
end;

procedure TFormGenerator.SpinEditPinkNoiseGainChange(Sender: TObject);
begin
  FPinkNoiseGain := db_to_Amp(SpinEditPinkNoiseGain.Value);
end;

procedure TFormGenerator.SpinEditSineCountChange(Sender: TObject);
begin
  SineCount := SpinEditSineCount.Value;
  CbSineChannelChange(Sender);
end;

procedure TFormGenerator.SetChannelCount(const Value: Integer);
begin
  if FChannelCount <> Value then
  begin
    FChannelCount := Value;
    ChannelCountChanged;
  end;
end;

procedure TFormGenerator.SetSineCount(const Value: Integer);
begin
  if FSineCount <> Value then
  begin
    FSineCount := Value;
    SineCountChanged;
  end;
end;

procedure TFormGenerator.SineCountChanged;
var
  Channel: Integer;
  Band: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to FChannelCount - 1 do
    begin
      for Band := FSineCount to Length(FSineOsc[Channel]) - 1 do
        FreeAndNil(FSineOsc[Channel, Band]);

      // Band oscillator
      SetLength(FSineOsc[Channel], FSineCount);
      for Band := 1 to FSineCount - 1 do
        if not Assigned(FSineOsc[Channel, Band]) then
        begin
          FSineOsc[Channel, Band] := TSimpleOscillator.Create;
          FSineOsc[Channel, Band].Assign(FSineOsc[Channel, 0]);
        end;
    end;

    ComboBoxSine.Visible := FSineCount > 1;
    LabelSineNo.Visible := ComboBoxSine.Visible;
    if ComboBoxSine.Visible then
    begin
      Band := ComboBoxSine.ItemIndex;
      ComboBoxSine.Clear;
      for Channel := 1 to FSineCount do
        ComboBoxSine.Items.Add(IntToStr(Channel));
      ComboBoxSine.ItemIndex := Band;
    end
    else
      ComboBoxSine.ItemIndex := 0;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TFormGenerator.SpinEditWhiteNoiseGainChange(Sender: TObject);
begin
  FWhiteNoiseGain := db_to_Amp(SpinEditWhiteNoiseGain.Value);
end;

procedure TFormGenerator.ButtonPauseClick(Sender: TObject);
begin
  FFadeDirection := fdDown;
end;

procedure TFormGenerator.ButtonPlayClick(Sender: TObject);
begin
  FFadeGain := db_to_Amp(-90);
  FFadeDirection := fdUp;
  ASIOHost.Active := True;
  GuiTimer.Enabled := True;
end;

procedure TFormGenerator.ButtonStopClick(Sender: TObject);
begin
  ButtonPauseClick(Sender);
  FTime := 0;
  LabelTime.Caption := '0:00:00:000';
end;

procedure TFormGenerator.ButtonSelectWavFileClick(Sender: TObject);
begin
  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFile) do
    try
      OpenWavDialog.InitialDir := ReadString('Recent', 'Audio File Directory',
        OpenWavDialog.InitialDir);
      if OpenWavDialog.Execute then
      begin
        EditWavFile.Text := OpenWavDialog.Filename;
        WriteString('Recent', 'Audio File Directory',
          ExtractFilePath(OpenWavDialog.Filename));
      end;
    finally
      Free;
    end;
end;

function TFormGenerator.GetCurrentGain: Single;
begin
  Result := FMainGain * FFadeGain;
  case FFadeDirection of
    fdUp:
      begin
        FFadeGain := FFadeGain * FFadeFactor;
        if FFadeGain > 1 then
        begin
          FFadeGain := 1;
          FFadeDirection := fdNone;
        end;
      end;
    fdDown:
      begin
        FFadeGain := FFadeGain / FFadeFactor;
        if FFadeGain < 1E-10 then
          FFadeGain := 0;
      end;
  end;
end;

procedure TFormGenerator.GuiTimerTimer(Sender: TObject);
var
  NewTime: Double;
  Mil, Sec: Integer;
  Min, Hr: Integer;
  PeakdB: Double;
begin
  Hr := Trunc(FTime / 3600);
  NewTime := FTime - Hr * 3600;
  Min := Trunc(NewTime / 60);
  NewTime := NewTime - Min * 60;
  Sec := Trunc(NewTime);
  NewTime := NewTime - Sec;
  Mil := Trunc(1000 * NewTime);

  // hour
  LabelTime.Caption := IntToStr(Hr) + ':';

  // minute
  if Min < 10 then
    LabelTime.Caption := LabelTime.Caption + '0';
  LabelTime.Caption := LabelTime.Caption + IntToStr(Min) + ':';

  // seconds
  if Sec < 10 then
    LabelTime.Caption := LabelTime.Caption + '0';
  LabelTime.Caption := LabelTime.Caption + IntToStr(Sec) + ':';

  // milli seconds
  if Mil < 100 then
    LabelTime.Caption := LabelTime.Caption + '0';
  if Mil < 10 then
    LabelTime.Caption := LabelTime.Caption + '0';
  LabelTime.Caption := LabelTime.Caption + IntToStr(Mil);

  // display peak
  PeakdB := Amp_to_dB(1E-10 + FPeak);
  if PeakdB > -25 then
    LED1.Brightness_Percent := 90
  else
    LED1.Brightness_Percent := 10;
  if PeakdB > -15 then
    LED2.Brightness_Percent := 90
  else
    LED2.Brightness_Percent := 10;
  if PeakdB > -5 then
    LED3.Brightness_Percent := 90
  else
    LED3.Brightness_Percent := 10;
  if PeakdB > 5 then
    LED4.Brightness_Percent := 90
  else
    LED4.Brightness_Percent := 10;
  if PeakdB > 15 then
    LED5.Brightness_Percent := 90
  else
    LED5.Brightness_Percent := 10;
  if PeakdB > 25 then
    LED6.Brightness_Percent := 90
  else
    LED6.Brightness_Percent := 10;

  // eventually stop audio processing
  if FFadeGain = 0 then
  begin
    ASIOHost.Active := False;
    GuiTimer.Enabled := False;
    FAudioFile.Reset;
  end;

  AudioDisplayTimeDomain.Refresh;
end;

procedure TFormGenerator.TrackBarVolumeChange(Sender: TObject);
begin
  FMainGain := db_to_Amp(-0.1 * TrackBarVolume.Position);
end;

procedure TFormGenerator.UpdateTime;
begin
  if FFadeDirection = fdNone then
    FTime := FTime + ASIOHost.BufferSize / ASIOHost.SampleRate;
end;

procedure TFormGenerator.CalculatePeak(Data: Single);
begin
  if Abs(Data) > FPeak then
    FPeak := Abs(Data);
  FPeak := 0.9999 * FPeak;
end;

procedure TFormGenerator.ComboBoxChannelSweepChange(Sender: TObject);
var
  Channel: Integer;
begin
  Channel := ComboBoxChannelSine.ItemIndex;

  EditSweepModulationTime.Text :=
    FloatToStrF(FSweepOsc[Channel].ModulationFrequency, ffGeneral, 5, 5);
end;

procedure TFormGenerator.CheckBoxIdenticalChannelsSineClick(Sender: TObject);
begin
  ComboBoxChannelSine.Visible := (not CheckBoxIdenticalChannelsSine.Checked) and
    (SpinEditChannelCount.Value > 0);
  LabelNoChannelSine.Visible := ComboBoxChannelSine.Visible;
end;

procedure TFormGenerator.CheckBoxIdenticalChannelsSweepClick(Sender: TObject);
begin
  ComboBoxChannelSweep.Visible := (not CheckBoxIdenticalChannelsSweep.Checked) and
    (SpinEditChannelCount.Value > 0);
  LabelNoChannelSweep.Visible := ComboBoxChannelSweep.Visible;
end;

procedure TFormGenerator.CbSineChannelChange(Sender: TObject);
var
  SineNo: Integer;
  Channel: Integer;
begin
  SineNo := ComboBoxSine.ItemIndex;
  Channel := ComboBoxChannelSine.ItemIndex;

  EditSineGain.Text := FloatToStrF(Amp_to_dB(FSineOsc[Channel, SineNo].Amplitude),
    ffGeneral, 3, 3);
  EditFrequency.Text := FloatToStrF(FSineOsc[Channel, SineNo].Frequency,
    ffGeneral, 5, 5);
  EditInitialPhase.Text := '0';
end;

procedure TFormGenerator.ChannelCountChanged;
var
  Channel: Integer;
  Band: Integer;
begin
  FCriticalSection.Enter;
  try
    // pink noise
    for Channel := FChannelCount to Length(FPinkNoise) - 1 do
      FreeAndNil(FPinkNoise[Channel]);

    // sine oscillator
    for Channel := FChannelCount to Length(FSineOsc) - 1 do
      for Band := 0 to FSineCount - 1 do
        FreeAndNil(FSineOsc[Channel, Band]);

    // sweep oscillator
    for Channel := FChannelCount to Length(FSweepOsc) - 1 do
      FreeAndNil(FSweepOsc[Channel]);

    // pink noise
    SetLength(FPinkNoise, FChannelCount);
    for Channel := 0 to FChannelCount - 1 do
      if not Assigned(FPinkNoise[Channel]) then
        FPinkNoise[Channel] := TPinkNoiseGenerator.Create;

    // Band oscillator
    SetLength(FSineOsc, FChannelCount, FSineCount);
    for Channel := 0 to FChannelCount - 1 do
      for Band := 0 to FSineCount - 1 do
        if not Assigned(FSineOsc[Channel, Band]) then
        begin
          FSineOsc[Channel, Band] := TSimpleOscillator.Create;
          FSineOsc[Channel, Band].SampleRate := ASIOHost.SampleRate;
          FSineOsc[Channel, Band].Frequency := 1000;
        end;

    // sweep oscillator
    SetLength(FSweepOsc, FChannelCount);
    for Channel := 0 to FChannelCount - 1 do
      if not Assigned(FSweepOsc[Channel]) then
      begin
        FSweepOsc[Channel] := TRangeSweepOscillator64.Create;
        FSweepOsc[Channel].SampleRate := ASIOHost.SampleRate;
        FSweepOsc[Channel].StartFrequency := 20;
        FSweepOsc[Channel].StopFrequency := 20000;
        FSweepOsc[Channel].ModulationFrequency := 1;
      end;
  finally
    FCriticalSection.Leave;
  end;

  // update controls
  CheckBoxIdenticalChannelsSine.Visible := SpinEditChannelCount.Value > 1;
  ComboBoxChannelSine.Visible := CheckBoxIdenticalChannelsSine.Visible and
    (not CheckBoxIdenticalChannelsSine.Checked);
  LabelNoChannelSine.Visible := ComboBoxChannelSine.Visible;

  CheckBoxIdenticalChannelsSweep.Visible := SpinEditChannelCount.Value > 1;
  ComboBoxChannelSweep.Visible := CheckBoxIdenticalChannelsSweep.Visible and
    (not CheckBoxIdenticalChannelsSweep.Checked);
  LabelNoChannelSweep.Visible := ComboBoxChannelSweep.Visible;
end;

procedure TFormGenerator.ASIOHostSampleRateChanged(Sender: TObject);
var
  Channel, Band: Integer;
begin
  FCriticalSection.Enter;
  try
    for Channel := 0 to Length(FSineOsc) - 1 do
      for Band := 0 to FSineCount - 1 do
        FSineOsc[Channel, Band].SampleRate := ASIOHost.SampleRate;
    for Channel := 0 to Length(FSweepOsc) - 1 do
      FSweepOsc[Channel].SampleRate := ASIOHost.SampleRate;
    if Assigned(FAudioFile) then
      FAudioFile.SampleRate := ASIOHost.SampleRate;
  finally
    FCriticalSection.Leave;
  end;
end;

/// /////////////
// processing //
/// /////////////

procedure TFormGenerator.BufferSwitchWhiteNoise32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel: Integer;
  Sample: Integer;
  Gain: Double;
begin
  FCriticalSection.Enter;
  try
    if RadioButtonRectangle.Checked then
      for Sample := 0 to ASIOHost.BufferSize - 1 do
      begin
        Gain := CurrentGain;

        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
        begin
          OutBuffer[Channel, Sample] := Gain * (2 * random - 1);
          CalculatePeak(Gain * OutBuffer[Channel, Sample]);
        end;
      end
    else
      for Sample := 0 to ASIOHost.BufferSize - 1 do
      begin
        Gain := CurrentGain;

        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
        begin
          OutBuffer[Channel, Sample] := Gain * (random - random);
          CalculatePeak(Gain * OutBuffer[Channel, Sample]);
        end;
      end;

    // update waveform
    ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
    ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
    for Channel := 0 to ADC.ChannelCount - 1 do
      Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0],
        ADC.SampleFrames * SizeOf(Single));
  finally
    FCriticalSection.Leave;
  end;

  // update time information
  UpdateTime;
end;

procedure TFormGenerator.BufferSwitchPinkNoise32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel: Integer;
  Sample: Integer;
  Data: array of Double;
  Gain: Double;
begin
  FCriticalSection.Enter;
  try
    SetLength(Data, FChannelCount);
    for Sample := 0 to ASIOHost.BufferSize - 1 do
    begin
      // calculate data
      for Channel := 0 to FChannelCount - 1 do
        Data[Channel] := FPinkNoise[Channel mod FChannelCount].ProcessSample64;

      Gain := CurrentGain;

      // calculate peak
      for Channel := 0 to FChannelCount - 11 do
        CalculatePeak(Gain * Data[Channel]);

      // distribute data
      if CheckBoxIdenticalChannelsSine.Checked then
        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
          OutBuffer[Channel, Sample] := Gain * Data[0]
      else
        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
          OutBuffer[Channel, Sample] := Gain * Data[Channel mod FChannelCount]
    end;

    // update waveform
    ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
    ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
    for Channel := 0 to ADC.ChannelCount - 1 do
      Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0],
        ADC.SampleFrames * SizeOf(Single));
  finally
    FCriticalSection.Leave;
  end;

  // update time information
  UpdateTime;
end;

procedure TFormGenerator.BufferSwitchSine32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel: Integer;
  Sample: Integer;
  Band: Integer;
  Data: array of Double;
  Gain: Double;
begin
  FCriticalSection.Enter;
  try
    SetLength(Data, FChannelCount);
    for Sample := 0 to ASIOHost.BufferSize - 1 do
    begin
      // calculate data
      for Channel := 0 to FChannelCount - 1 do
      begin
        Data[Channel] := 0;
        for Band := 0 to FSineCount - 1 do
        begin
          Data[Channel] := Data[Channel] + FSineOsc[Channel, Band].Sine;
          FSineOsc[Channel, Band].CalculateNextSample;
        end;
      end;

      Gain := CurrentGain;

      // calculate peak
      for Channel := 0 to FChannelCount - 11 do
        CalculatePeak(Gain * Data[Channel]);

      // distribute data
      if CheckBoxIdenticalChannelsSine.Checked then
        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
          OutBuffer[Channel, Sample] := Gain * Data[0]
      else
        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
          OutBuffer[Channel, Sample] := Gain * Data[Channel mod FChannelCount]
    end;

    // update waveform
    ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
    ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
    for Channel := 0 to ADC.ChannelCount - 1 do
      Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0],
        ADC.SampleFrames * SizeOf(Single));
  finally
    FCriticalSection.Leave;
  end;

  // update time information
  UpdateTime;
end;

procedure TFormGenerator.BufferSwitchSweep32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel: Integer;
  Sample: Integer;
  Data: array of Double;
  Gain: Double;
begin
  FCriticalSection.Enter;
  try
    SetLength(Data, FChannelCount);
    for Sample := 0 to ASIOHost.BufferSize - 1 do
    begin
      // calculate data
      for Channel := 0 to FChannelCount - 1 do
      begin
        Data[Channel] := FSweepOsc[Channel].Sine;
        FSweepOsc[Channel].CalculateNextSample;
      end;

      Gain := CurrentGain;

      // calculate peak
      for Channel := 0 to FChannelCount - 11 do
        CalculatePeak(Gain * Data[Channel]);

      // distribute data
      if CheckBoxIdenticalChannelsSine.Checked then
        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
          OutBuffer[Channel, Sample] := Gain * Data[0]
      else
        for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
          OutBuffer[Channel, Sample] := Gain * Data[Channel mod FChannelCount]
    end;

    // update waveform
    ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
    ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
    for Channel := 0 to ADC.ChannelCount - 1 do
      Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0],
        ADC.SampleFrames * SizeOf(Single));
  finally
    FCriticalSection.Leave;
  end;

  // update time information
  UpdateTime;
end;

procedure TFormGenerator.BufferSwitchWave32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel: Integer;
  Sample: Integer;
  Gain: Double;
begin
  FCriticalSection.Enter;
  try
    FAudioFile.GetSamples(OutBuffer, ASIOHost.BufferSize);

    for Sample := 0 to ASIOHost.BufferSize - 1 do
    begin

      Gain := CurrentGain;

      // calculate peak
      for Channel := 0 to 1 do
        CalculatePeak(Gain * OutBuffer[Channel, Sample]);

      for Channel := ASIOHost.OutputChannelCount - 1 downto 0 do
        OutBuffer[Channel, Sample] := Gain * OutBuffer[Channel mod 2, Sample];
    end;

    // update waveform
    ADC.ChannelCount := 2;
    ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
    Move(OutBuffer[0, 0], ADC[0].ChannelDataPointer^[0],
      ADC.SampleFrames * SizeOf(Single));
    Move(OutBuffer[1, 0], ADC[1].ChannelDataPointer^[0],
      ASIOHost.BufferSize * SizeOf(Single));
  finally
    FCriticalSection.Leave;
  end;

  // update time information
  UpdateTime;
end;

end.
