unit AsioDemoForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Complex, DAV_Types,
  DAV_AudioData, DAV_ASIOHost, DAV_ASIOHostAudioData;

type
  TFormAnalyserGoertzel = class(TForm)
    ASIOHostAudioData: TASIOHostAudioData;
    ButtonControlPanel: TButton;
    ButtonStartStop: TButton;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    LabelChannels: TLabel;
    LabelCopyright: TLabel;
    LabelDrivername: TLabel;
    LabelFrequency: TLabel;
    LabelPanorama: TLabel;
    LabelVolume: TLabel;
    ScrollBarFreq: TScrollBar;
    ScrollBarPan: TScrollBar;
    ScrollBarVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostAudioDataBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TASIOAudioDataCollection32);
    procedure ASIOHostAudioDataBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TASIOAudioDataCollection64);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxChannelChange(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure ScrollBarFreqChange(Sender: TObject);
    procedure ScrollBarPanChange(Sender: TObject);
    procedure ScrollBarVolumeChange(Sender: TObject);
  private
    procedure SetFrequency(const CurrentValue: Double);
  public
    FAngle, FPosition: TComplex64;
    FPan, FFreq, FVol: Double;
    FChannelOffset: Byte;
  published
    property Frequency: Double read FFreq write SetFrequency;
  end;

var
  FormAnalyserGoertzel: TFormAnalyserGoertzel;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Math, DAV_Common;

resourcestring
  RCStrPanorama = 'Panorama';
  RCStrVolume = 'Volume';
  RCStrFrequency = 'Frequency';
  RCStrStartAudio = 'Start Audio';
  RCStrStopAudio = 'Stop Audio';
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFormAnalyserGoertzel.FormCreate(Sender: TObject);
begin
  ComboBoxDriver.Items := ASIOHostAudioData.DriverList;
  if ComboBoxDriver.Items.Count = 0 then
  try
    raise Exception.Create(RCStrNoASIODriverPresent);
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
    ComboBoxChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
  finally
    Free;
  end;

  FPosition.Re   := 0;
  FPosition.Im   := -1;
  FFreq          := 1000;
  FPan           := 0.5;
  FVol           := 1;
  FChannelOffset := 0;
  GetSinCos(2 * Pi * FFreq / ASIOHostAudioData.SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TFormAnalyserGoertzel.ComboBoxDriverChange(Sender: TObject);
var
  i: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHostAudioData.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for i := 0 to (ASIOHostAudioData.OutputChannelCount div 2) - 1 do
    begin
      ComboBoxChannel.Items.Add(
        ASIOHostAudioData.OutputChannelInfos[2 * i].Name + ' / ' +
       ASIOHostAudioData.OutputChannelInfos[2 * i + 1].Name);
    end;
    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
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

procedure TFormAnalyserGoertzel.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHostAudioData.ControlPanel;
end;

procedure TFormAnalyserGoertzel.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
    WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
  finally
    Free;
  end; 
end;

procedure TFormAnalyserGoertzel.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Caption = RCStrStartAudio then
  begin
    ASIOHostAudioData.Active := True; // Start Audio
    ButtonStartStop.Caption := RCStrStopAudio;
  end
  else
  begin
    ASIOHostAudioData.Active := False; // Stop Audio
    ButtonStartStop.Caption := RCStrStartAudio;
  end;
end;

procedure TFormAnalyserGoertzel.ComboBoxChannelChange(Sender: TObject);
begin
  FChannelOffset := ComboBoxChannel.ItemIndex * 2;
end;

procedure TFormAnalyserGoertzel.ScrollBarFreqChange(Sender: TObject);
begin
  Frequency := FreqLinearToLog(ScrollBarFreq.Position * 0.00001);
end;

procedure TFormAnalyserGoertzel.SetFrequency(const CurrentValue: Double);
begin
  if FFreq <> CurrentValue then
  begin
    FFreq := CurrentValue;
    LabelFrequency.Caption := RCStrFrequency + ': ' + FloatTostrF(FFreq, ffGeneral, 5, 5) + ' Hz';
    GetSinCos(2 * Pi * FFreq / ASIOHostAudioData.SampleRate, FAngle.Im, FAngle.Re);
  end;
end;

procedure TFormAnalyserGoertzel.ASIOHostAudioDataBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TASIOAudioDataCollection32);
var
  Sample, Channel: Integer;
  CurrentValue: Double;
begin
  for Sample := 0 to OutBuffer.SampleFrames - 1 do
  begin
    CurrentValue := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
    FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
    FPosition.Re := CurrentValue; CurrentValue := CurrentValue * FVol;
    for Channel := 0 to ASIOHostAudioData.OutputChannelCount - 1 do
      OutBuffer[Channel].ChannelDataPointer[Sample] := CurrentValue;
  end;
end;

procedure TFormAnalyserGoertzel.ASIOHostAudioDataBufferSwitch64(Sender: TObject;
  const InBuffer, OutBuffer: TASIOAudioDataCollection64);
var
  Sample, Channel: Integer;
  CurrentValue: Double;
begin
  for Sample := 0 to ASIOHostAudioData.BufferSize - 1 do
  begin
    CurrentValue := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
    FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
    FPosition.Re := CurrentValue; CurrentValue := CurrentValue * FVol;
    for Channel := 0 to ASIOHostAudioData.OutputChannelCount - 1 do
      OutBuffer[Channel].ChannelDataPointer[Sample] := CurrentValue;
  end;
end;

procedure TFormAnalyserGoertzel.ASIOHostSampleRateChanged(Sender: TObject);
begin
  GetSinCos(2 * Pi * FFreq / ASIOHostAudioData.SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TFormAnalyserGoertzel.ScrollBarVolumeChange(Sender: TObject);
begin
  FVol := ScrollBarVolume.Position * 0.00001;
  if FVol = 0 then
    LabelVolume.Caption := RCStrVolume + ': 0 equals -oo dB'
  else
    LabelVolume.Caption := RCStrVolume + ': ' +
      FloattostrF(FVol, ffFixed, 2, 2) + ' equals ' +
      FloattostrF(Amp_to_dB(FVol), ffGeneral, 2, 2) + ' dB';
end;

procedure TFormAnalyserGoertzel.ScrollBarPanChange(Sender: TObject);
begin
  FPan := ScrollBarPan.Position * 0.01;
  if FPan = 0.5 then
    LabelPanorama.Caption := RCStrPanorama + ': C'
  else
    LabelPanorama.Caption := RCStrPanorama + ': ' + IntToStr(round(100 * (FPan * 2 - 1)));
end;

{$IFDEF FPC}
initialization
  {$i AsioDemoForm.lrs}
{$ENDIF}

end.

