unit AsioHostDriverControlPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  ToolWin, ComCtrls, StdCtrls, Menus;

type
  TFmAsioDriverControlPanel = class(TForm)
    LbDriver: TLabel;
    CbDriver: TComboBox;
    BtControlPanel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtControlPanelClick(Sender: TObject);
    procedure CbDriverChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRegName : string;
  public
  end;

implementation

uses
  Math, Dialogs, Registry, DAV_AsioHost, AsioHostDriverMain;

{$R *.dfm}

procedure TFmAsioDriverControlPanel.FormCreate(Sender: TObject);
begin
 FRegName := 'SOFTWARE\ASIO\' + CDriverDescription;
 CbDriver.Items := TAsioHost(Owner).DriverList;
 if CbDriver.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

(*
 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   CbDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if CbDriver.ItemIndex >= 0 then CbDriverChange(CbDriver);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
  finally
   Free;
  end;
*)
end;

procedure TFmAsioDriverControlPanel.FormShow(Sender: TObject);
begin
 CbDriver.ItemIndex := TAsioHost(Owner).DriverIndex;
end;

procedure TFmAsioDriverControlPanel.CbDriverChange(Sender: TObject);
var
  Channel : Integer;
begin
 BtControlPanel.Enabled := False;
// BtStartStop.Enabled := False;
 CbDriver.ItemIndex := CbDriver.Items.IndexOf(CbDriver.Text);
 if CbDriver.ItemIndex >= 0 then
  begin
   TAsioHost(Owner).DriverIndex := CbDriver.ItemIndex;
(*
   ChannelBox.Clear;
   for Channel := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
   begin
    ChannelBox.Items.Add(
     ASIOHost.OutputChannelInfos[2 * Channel].name + ' / ' +
     ASIOHost.OutputChannelInfos[2 * Channel + 1].name);
   end;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', CbDriver.ItemIndex);
    finally
     Free;
    end;
*)
   BtControlPanel.Enabled := True;
//   BtStartStop.Enabled := True;
//   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmAsioDriverControlPanel.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := caHide;
end;

procedure TFmAsioDriverControlPanel.BtControlPanelClick(Sender: TObject);
begin
 TAsioHost(Owner).ControlPanel;
end;

end.
