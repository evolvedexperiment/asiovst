unit LunchBoxSetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TFmSetup = class(TForm)
    LbPreset: TLabel;
    Label1: TLabel;
    CBDrivers: TComboBox;
    CBOutput: TComboBox;
    BtControlPanel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure CBInputChange(Sender: TObject);
    procedure CBOutputChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmSetup: TFmSetup;

implementation

uses IniFiles, DASIOHost, LunchBoxMain;

{$R *.dfm}

procedure TFmSetup.FormCreate(Sender: TObject);
var Settings : TInifile;
begin
 CBDrivers.Items:=FmVSTEditor.ASIOHost.DriverList;
 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Top:=Settings.ReadInteger('Layout','Setup Top',Top);
 Left:=Settings.ReadInteger('Layout','Setup Left',Left);
 CBDrivers.ItemIndex:=Settings.ReadInteger('Setup','ASIO Driver',CBDrivers.ItemIndex);
 CBDriversChange(Self);
 Settings.Free;
end;

procedure TFmSetup.BtControlPanelClick(Sender: TObject);
begin
 FmVSTEditor.ASIOHost.ControlPanel;
end;

procedure TFmSetup.CBDriversChange(Sender: TObject);
var i : Integer;
begin
 if CBDrivers.ItemIndex >= 0 then
  begin
   FmVSTEditor.ASIOHost.Active:=False;
   FmVSTEditor.ASIOHost.DriverIndex := CBDrivers.ItemIndex;
   CBOutput.Clear;
   for i := 0 to (FmVSTEditor.ASIOHost.OutputChannels div 2) - 1 do
    begin
     CBOutput.Items.Add(
     FmVSTEditor.ASIOHost.OutputChannelInfos[2 * i].name + ' / ' +
     FmVSTEditor.ASIOHost.OutputChannelInfos[2 * i + 1].name);
    end;
   CBOutput.ItemIndex := 0;
   FmVSTEditor.ASIOHost.OnReset(Self);
   FmVSTEditor.ASIOHost.Active:=True;
  end;
end;

procedure TFmSetup.CBInputChange(Sender: TObject);
begin
// FmVSTEditor.ASIOHost.InputChannels:=CBInput.ItemIndex*2;
end;

procedure TFmSetup.CBOutputChange(Sender: TObject);
begin
// FmVSTEditor.ASIOHost.OutputChannels:=CBOutput.ItemIndex*2;
end;

procedure TFmSetup.FormDestroy(Sender: TObject);
var Settings : TInifile;
begin
 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Settings.WriteInteger('Layout','Setup Top',Top);
 Settings.WriteInteger('Layout','Setup Left',Left);
 Settings.WriteInteger('Setup','ASIO Driver',CBDrivers.ItemIndex);
 Settings.Free;
end;

end.
