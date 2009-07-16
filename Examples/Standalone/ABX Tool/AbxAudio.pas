unit AbxAudio;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFmAudioSettings = class(TForm)
    LbPreset: TLabel;
    CBDrivers: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBDriversChange(Sender: TObject);
  end;

var
  FmAudioSettings: TFmAudioSettings;

implementation

{$R *.dfm}

uses
  AbxMain, IniFiles;

procedure TFmAudioSettings.FormCreate(Sender: TObject);
begin
 CBDrivers.Items := FmAbxMain.ASIOHost.DriverList;
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   CBDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
   if CBDrivers.ItemIndex = -1
    then CBDrivers.ItemIndex := CBDrivers.Items.IndexOf('ASIO4ALL v2') else
   if CBDrivers.ItemIndex = -1
    then CBDrivers.ItemIndex := CBDrivers.Items.IndexOf('ASIO4ALL');
   CBDriversChange(Self);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   WriteInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   Top := ReadInteger('Layout', 'Audio Settings Top', Top);
   Left := ReadInteger('Layout', 'Audio Settings Left', Left);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TIniFile.Create(FmAbxMain.IniFileName) do
  try
   WriteInteger('Layout', 'Audio Settings Top', Top);
   WriteInteger('Layout', 'Audio Settings Left', Left);
  finally
   Free;
  end;
end;

procedure TFmAudioSettings.CBDriversChange(Sender: TObject);
begin
 with FmAbxMain.ASIOHost do
  if CBDrivers.ItemIndex >= 0 then
   begin
    DriverIndex := CBDrivers.ItemIndex;
    if assigned(OnReset) then OnReset(Self);
   end;
end;

end.
