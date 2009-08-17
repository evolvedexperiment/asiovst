unit SmpSetup;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TFmSetup = class(TForm)
    LbAsioDriver: TLabel;
    LbOutput: TLabel;
    CBDrivers: TComboBox;
    CBOutput: TComboBox;
    BtControlPanel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure CBOutputChange(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
  end;

var
  FmSetup: TFmSetup;

implementation

uses
  IniFiles, Dialogs, SmpMain;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFmSetup.FormCreate(Sender: TObject);
begin
 CBDrivers.Items := FmSimpleMp3Player.ASIOHost.DriverList;
 if CBDrivers.Items.Count = 0 then
  begin
   MessageDlg(RCStrNoASIODriverPresent, mtError, [mbOK], 0);
   Application.Terminate;
  end;

 with TIniFile.Create(FmSimpleMp3Player.IniFile) do
  try
   Top := ReadInteger('Layout', 'Setup Top', Top);
   Left := ReadInteger('Layout', 'Setup Left', Left);
   CBDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
   CBDriversChange(Self);
  finally
   Free;
  end;
end;

procedure TFmSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmSimpleMp3Player.IniFile) do
  try
   WriteInteger('Layout', 'Setup Top', Top);
   WriteInteger('Layout', 'Setup Left', Left);
   WriteInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
  finally
   Free;
  end;
end;

procedure TFmSetup.CBDriversChange(Sender: TObject);
var
  i : Integer;
begin
 with FmSimpleMp3Player.ASIOHost do
  if CBDrivers.ItemIndex >= 0 then
   begin
    Active := False;
    DriverIndex := CBDrivers.ItemIndex;
    CBOutput.Clear;
    for i := 0 to (OutputChannelCount div 2) - 1 do
     begin
      CBOutput.Items.Add(
        OutputChannelInfos[2 * i].name + ' / ' +
        OutputChannelInfos[2 * i + 1].name);
     end;
    CBOutput.ItemIndex := 0;
    if assigned(OnReset)
     then OnReset(Self);

    with TIniFile.Create(FmSimpleMp3Player.IniFile) do
     try
      WriteInteger('Setup', 'Asio Driver', CbDrivers.ItemIndex);
     finally
      Free;
     end;

    BtControlPanel.Enabled := True;
   end;
end;

procedure TFmSetup.CBOutputChange(Sender: TObject);
begin
 FmSimpleMp3Player.OutputChannelOffset := CBOutput.ItemIndex * 2;
end;

procedure TFmSetup.BtControlPanelClick(Sender: TObject);
begin
 FmSimpleMp3Player.AsioHost.ControlPanel;
end;

{$IFDEF FPC}
initialization
  {$i SmpSetup.lrs}
{$ENDIF}

end.

