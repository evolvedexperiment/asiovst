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

unit PortAudioDemoForm;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLType, Buttons, {$ELSE} Windows, {$ENDIF} Forms, Classes,
  Controls, StdCtrls, DAV_Complex, DAV_Types, DAV_DspSimpleOscillator,
  DAV_PortAudioHost, DAV_PortAudioTypes;

type
  TFormPortAudio = class(TForm)
    ButtonStartStop: TButton;
    ComboBoxDriver: TComboBox;
    LabelCopyright: TLabel;
    LabelDrivername: TLabel;
    LabelFreq: TLabel;
    LabelVolume: TLabel;
    ScrollBarFreq: TScrollBar;
    ScrollBarVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    function PortAudioCallback(Sender: TObject;
      InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray; FrameCount: NativeUInt;
      TimeInfo: PPaStreamCallbackTimeInfo;
      StatusFlags: TPaStreamCallbackFlags): LongInt;
    procedure PortAudioReset(Sender: TObject);
    procedure PortAudioSampleRateChanged(Sender: TObject);
    procedure ScrollBarFreqChange(Sender: TObject);
    procedure ScrollBarVolumeChange(Sender: TObject);
  private
    procedure SetFrequency(const Value: Double);
    procedure SetAmplitude(const Value: Double);
  protected
    procedure AmplitudeChanged; virtual;
    procedure FrequencyChanged; virtual;
  public
    FPortAudio: TPortAudioHost;
    FOscillator: TSimpleOscillator64;
    FFreq, FAmp: Double;
  published
    property Frequency: Double read FFreq write SetFrequency;
    property Amplitude: Double read FAmp write SetAmplitude;
  end;

var
  FormPortAudio: TFormPortAudio;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Common, DAV_Math;

resourcestring
  RCStrNoPortAudioDriverPresent =
    'No PortAudio Driver present! Application Terminated!';

procedure TFormPortAudio.FormCreate(Sender: TObject);
begin
  FPortAudio := TPortAudioHost.Create;
  FPortAudio.OnSampleRateChanged := PortAudioSampleRateChanged;
  FPortAudio.OnStreamCallback := PortAudioCallback;
  ComboBoxDriver.Items := FPortAudio.OutputDeviceList;

  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create(RCStrNoPortAudioDriverPresent);
    except
      Application.Terminate;
    end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'PortAudioDemo.INI') do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);
      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'PortAudio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);
    finally
      Free;
    end;

  FAmp := 1;
  FFreq := 1000;

  FOscillator := TSimpleOscillator64.Create;
  with FOscillator do
  begin
    Frequency := FFreq;
    Amplitude := FAmp;
    SampleRate := FPortAudio.SampleRate;
  end;
end;

procedure TFormPortAudio.ComboBoxDriverChange(Sender: TObject);
var
  Channel: Integer;
begin
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    FPortAudio.Close;
    FPortAudio.OutputDevice :=
      Integer(ComboBoxDriver.Items.Objects[ComboBoxDriver.ItemIndex]);
    FPortAudio.InputDevice := -1;
    FPortAudio.Open;

    // store current PortAudio driver index
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'PortAudioDemo.INI') do
      try
        WriteInteger('Audio', 'PortAudio Driver', ComboBoxDriver.ItemIndex);
      finally
        Free;
      end;

    ButtonStartStop.Enabled := True;
  end;
end;

procedure TFormPortAudio.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'PortAudioDemo.INI') do
    try
      WriteInteger('Layout', 'Audio Left', Left);
      WriteInteger('Layout', 'Audio Top', Top);
      WriteInteger('Audio', 'PortAudio Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;

  FPortAudio.Active := False;
  FreeAndNil(FOscillator);
  FreeAndNil(FPortAudio);
end;

procedure TFormPortAudio.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Caption = '&Start Audio' then
  begin
    FPortAudio.Start; // Start Audio
    ButtonStartStop.Caption := '&Stop Audio';
  end
  else
  begin
    FPortAudio.Abort; // Stop Audio
    ButtonStartStop.Caption := '&Start Audio';
  end;
end;

procedure TFormPortAudio.ScrollBarFreqChange(Sender: TObject);
begin
  Frequency := FreqLinearToLog(ScrollBarFreq.Position * 0.00001);
end;

procedure TFormPortAudio.SetAmplitude(const Value: Double);
begin
  if FAmp <> Value then
  begin
    FAmp := Value;
    AmplitudeChanged;
  end;
end;

procedure TFormPortAudio.SetFrequency(const Value: Double);
begin
  if FFreq <> Value then
  begin
    FFreq := Value;
    FrequencyChanged;
  end;
end;

procedure TFormPortAudio.AmplitudeChanged;
begin
  FOscillator.Amplitude := FAmp;
  if FAmp = 0 then
    LabelVolume.Caption := 'Volume: 0 equals -oo dB'
  else
    LabelVolume.Caption := 'Volume: ' + FloatToStrF(FAmp, ffFixed, 2, 2) +
      ' equals ' + FloatToStrF(Amp_to_dB(FAmp), ffGeneral, 2, 2) + ' dB';
end;

procedure TFormPortAudio.FrequencyChanged;
begin
  FOscillator.Frequency := FFreq;
  LabelFreq.Caption := 'Frequency: ' + FloatToStrF(FFreq, ffGeneral, 5, 5) + ' Hz';
end;

procedure TFormPortAudio.ScrollBarVolumeChange(Sender: TObject);
begin
  Amplitude := ScrollBarVolume.Position * 0.00001;
end;

procedure TFormPortAudio.PortAudioSampleRateChanged(Sender: TObject);
begin
  if Assigned(FOscillator) then
    FOscillator.SampleRate := FPortAudio.SampleRate;
end;

function TFormPortAudio.PortAudioCallback(Sender: TObject;
  InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray; FrameCount: NativeUInt;
  TimeInfo: PPaStreamCallbackTimeInfo;
  StatusFlags: TPaStreamCallbackFlags): LongInt;
var
  Sample: Integer;
  ChannelIndex: Integer;
begin
  for Sample := 0 to FrameCount - 1 do
  begin
    for ChannelIndex := 0 to Length(OutBuffer) - 1 do
      OutBuffer[ChannelIndex, Sample] := FOscillator.Sine;
    FOscillator.CalculateNextSample;
  end;
  Result := paContinue;
end;

procedure TFormPortAudio.PortAudioReset(Sender: TObject);
begin
  if ButtonStartStop.Caption = '&Stop Audio' then
    FPortAudio.Active := True;
end;

end.
