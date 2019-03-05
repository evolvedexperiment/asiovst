unit ASIOMP3GUI;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, DAV_Types, 
  DAV_DspBufferedMp3Player, DAV_ASIOHost;

type
  TFormASIOMP3 = class(TForm)
    ASIOHost: TASIOHost;
    ButtonControlPanel: TButton;
    ButtonSelect: TButton;
    ButtonStartStop: TButton;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    EditFile: TEdit;
    LabelBuffer: TLabel;
    LabelBufferValue: TLabel;
    LabelChannels: TLabel;
    LabelDrivername: TLabel;
    LabelMp3File: TLabel;
    OpenDialog: TOpenDialog;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ButtonSelectClick(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ComboBoxChannelChange(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure EditFileChange(Sender: TObject);
    procedure LabelBufferClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FIniFile        : TFileName;
    FVolumeFactor   : Single;
    FChannelOffset  : Byte;
    FBufferedPlayer : TBufferedMP3FilePlayer;
  end;

var
  FormASIOMP3: TFormASIOMP3;

implementation

{$R *.dfm}

uses
  IniFiles;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

{ TFormASIOMP3 }

procedure TFormASIOMP3.FormCreate(Sender: TObject);
begin
  FIniFile := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';
  ComboBoxDriver.Items := ASIOHost.DriverList;
  if ComboBoxDriver.Items.Count = 0 then
  begin
    MessageDlg(RCStrNoASIODriverPresent,
      mtError, [mbOK], 0);
    Application.Terminate;
  end;

  FVolumeFactor := 1;
  FChannelOffset := 0;
  FBufferedPlayer := TBufferedMP3FilePlayer.Create;
  FBufferedPlayer.Pitch := 1;
  FBufferedPlayer.Interpolation := biBSpline6Point5thOrder;
  with FBufferedPlayer do
  begin
    BufferSize := 65536;
    BlockSize  := 4096
  end;

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFile) do
    try
      Left := ReadInteger('Layout', 'Audio Left', Left);
      Top := ReadInteger('Layout', 'Audio Top', Top);

      ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
      if ComboBoxDriver.ItemIndex >= 0 then
        ComboBoxDriverChange(ComboBoxDriver);
      ComboBoxChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
      EditFile.Text := ReadString('Audio', 'MP3 File', EditFile.Text);
      ButtonStartStop.Enabled := FileExists(EditFile.Text);
    finally
      Free;
    end;

  // enable timer
  Timer.Enabled := True;
end;

procedure TFormASIOMP3.FormDestroy(Sender: TObject);
begin
  ASIOHost.Active := False;
  FreeAndNil(FBufferedPlayer);

  with TIniFile.Create(FIniFile) do
  try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
    WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
    WriteString('Audio', 'MP3 File', EditFile.Text);
  finally
    Free;
  end;
end;

procedure TFormASIOMP3.LabelBufferClick(Sender: TObject);
begin
  ASIOHost.SampleRate := 48000;
end;

procedure TFormASIOMP3.TimerTimer(Sender: TObject);
begin
  LabelBufferValue.Caption := IntToStr(Round(FBufferedPlayer.BufferFill)) + ' %';
end;

procedure TFormASIOMP3.ComboBoxDriverChange(Sender: TObject);
var
  i: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonStartStop.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for i := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
      ComboBoxChannel.Items.Add(
        string(ASIOHost.OutputChannelInfos[2 * i].Name) + ' / ' +
        string(ASIOHost.OutputChannelInfos[2 * i + 1].Name));

    with TIniFile.Create(FIniFile) do
    try
      WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;

   ButtonControlPanel.Enabled := True;
   ButtonStartStop.Enabled := FileExists(EditFile.Text);
   ComboBoxChannel.ItemIndex := 0;
  end;
end;

procedure TFormASIOMP3.ComboBoxChannelChange(Sender: TObject);
begin
  FChannelOffset := ComboBoxChannel.ItemIndex * 2;
end;

procedure TFormASIOMP3.ASIOHostSampleRateChanged(Sender: TObject);
begin
  if Assigned(FBufferedPlayer) then
    FBufferedPlayer.SampleRate := ASIOHost.SampleRate;
end;

procedure TFormASIOMP3.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormASIOMP3.ButtonStartStopClick(Sender: TObject);
begin
  if ButtonStartStop.Caption = '&Start Audio' then
  begin
    ASIOHost.Active := True;
    ButtonStartStop.Caption := '&Stop Audio';
  end
  else
  begin
    ASIOHost.Active := False;
    FBufferedPlayer.Reset;
    ButtonStartStop.Caption := '&Start Audio';
  end;
end;

procedure TFormASIOMP3.ButtonSelectClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    EditFile.Text := OpenDialog.FileName;
end;

procedure TFormASIOMP3.EditFileChange(Sender: TObject);
begin
  FBufferedPlayer.Filename := EditFile.Text;
end;

procedure TFormASIOMP3.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
  FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);
end;

end.
