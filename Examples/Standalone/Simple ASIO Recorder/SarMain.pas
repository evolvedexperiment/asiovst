unit SarMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DAV_Common, DAV_DspBufferedAudioFileRecorder,
  DAV_ASIOHost, DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF,
  DAV_AudioFileAU;

type
  TFmRecordAudio = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtSelect: TButton;
    BtStartStop: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    EdFile: TEdit;
    LbBuffer: TLabel;
    LbBufferValue: TLabel;
    LbChannels: TLabel;
    LbDrivername: TLabel;
    LbRecordedFile: TLabel;
    Timer: TTimer;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtSelectClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure EdFileChange(Sender: TObject);
    procedure LbBufferClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FIniFile        : TFileName;
    FVolumeFactor   : Single;
    FChannelOffset  : Byte;
    FBufferedRecorder : TBufferedAudioFileRecorder;
  end;

var
  FmRecordAudio: TFmRecordAudio;

implementation

{$R *.dfm}

uses
  IniFiles;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

{ TFmASIOMP3 }

procedure TFmRecordAudio.FormCreate(Sender: TObject);
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'SimpleAsioRecorder.INI';
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  begin
   MessageDlg(RCStrNoASIODriverPresent, mtError, [mbOK], 0);
   Application.Terminate;
  end;

 FVolumeFactor := 1;
 FChannelOffset := 0;

 FBufferedRecorder := TBufferedAudioFileRecorder.Create;
 with FBufferedRecorder do
  begin
   BufferSize := 65536;
   BlockSize  := 4096
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);

   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then
     DriverComboChange(DriverCombo);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   EdFile.Text := ReadString('Audio', 'File', EdFile.Text);
   EdFileChange(Self);
   BtStartStop.Enabled := (EdFile.Text <> '') and (DriverCombo.ItemIndex >= 0);
  finally
   Free;
  end;
end;

procedure TFmRecordAudio.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(FIniFile) do
   try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
    WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
    WriteString('Audio', 'File', EdFile.Text);
   finally
    Free;
   end;

  ASIOHost.Active := False;
  FreeAndNil(FBufferedRecorder);
end;

procedure TFmRecordAudio.LbBufferClick(Sender: TObject);
begin
 ASIOHost.SampleRate := 48000;
end;

procedure TFmRecordAudio.TimerTimer(Sender: TObject);
begin
 LbBufferValue.Caption := IntToStr(Round(FBufferedRecorder.BufferFill)) + ' %';
end;

procedure TFmRecordAudio.DriverComboChange(Sender: TObject);
var
  i: Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for i := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
     ChannelBox.Items.Add(
       ASIOHost.OutputChannelInfos[2 * i].Name + ' / ' +
       ASIOHost.OutputChannelInfos[2 * i + 1].Name);

   with TIniFile.Create(FIniFile) do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;

   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := EdFile.Text <> '';
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmRecordAudio.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmRecordAudio.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if assigned(FBufferedRecorder)
  then FBufferedRecorder.SampleRate := ASIOHost.SampleRate;
end;

procedure TFmRecordAudio.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmRecordAudio.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = '&Record Audio' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := '&Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False;
   FBufferedRecorder.Reset;
   BtStartStop.Caption := '&Record Audio';
  end;
end;

procedure TFmRecordAudio.BtSelectClick(Sender: TObject);
begin
 if SaveDialog.Execute
  then EdFile.Text := SaveDialog.FileName;
end;

procedure TFmRecordAudio.EdFileChange(Sender: TObject);
begin
 DeleteFile(EdFile.Text);
 FBufferedRecorder.Filename := EdFile.Text;
 BtStartStop.Enabled := (FBufferedRecorder.Filename <> '') and (DriverCombo.ItemIndex >= 0);
end;

procedure TFmRecordAudio.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
 FBufferedRecorder.PutSamples(InBuffer[0], InBuffer[1], ASIOHost.Buffersize);
end;

end.
