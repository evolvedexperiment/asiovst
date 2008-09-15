unit LunchBoxSetup;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LMessages, LResources, Buttons,
  {$ELSE} Windows, Messages,{$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls, Spin;

type
  TFmSetup = class(TForm)
    LbPreset: TLabel;
    Label1: TLabel;
    CBDrivers: TComboBox;
    CBOutput: TComboBox;
    BtControlPanel: TButton;
    Label2: TLabel;
    SESampleRate: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure CBInputChange(Sender: TObject);
    procedure CBOutputChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure SESampleRateChange(Sender: TObject);
  private
  public
  end;

var
  FmSetup: TFmSetup;

implementation

uses
  IniFiles, DAV_ASIOHost, LunchBoxMain;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmSetup.FormCreate(Sender: TObject);
var Settings : TInifile;
begin
 CBDrivers.Items:=FmLunchBox.ASIOHost.DriverList;
 Settings:=TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI');
 Top:=Settings.ReadInteger('Layout','Setup Top',Top);
 Left:=Settings.ReadInteger('Layout','Setup Left',Left);
 CBDrivers.ItemIndex:=Settings.ReadInteger('Setup','ASIO Driver',CBDrivers.ItemIndex);
 CBDriversChange(Self);
 Settings.Free;
end;

procedure TFmSetup.BtControlPanelClick(Sender: TObject);
begin
 FmLunchBox.ASIOHost.ControlPanel;
end;

procedure TFmSetup.CBDriversChange(Sender: TObject);
var i : Integer;
begin
 if CBDrivers.ItemIndex >= 0 then
  with FmLunchBox.ASIOHost do
   begin
    Active:=False;
    DriverIndex := CBDrivers.ItemIndex;
    CBOutput.Clear;
    for i := 0 to (OutputChannelCount div 2) - 1 do
     begin
      CBOutput.Items.Add(
      OutputChannelInfos[2 * i].name + ' / ' +
      OutputChannelInfos[2 * i + 1].name);
     end;
    CBOutput.ItemIndex := 0;
    SESampleRate.Value:=Round(Samplerate);
    OnReset(Self);
    Active:=True;
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

procedure TFmSetup.SESampleRateChange(Sender: TObject);
var i : Integer;
begin
 for i:=0 to FmLunchBox.EventList.Count-1 do
  with FmLunchBox.EventList[i] do
   begin
    SampleRate:=sqr(FmLunchBox.ASIOHost.SampleRate)/SESampleRate.Value;
    Frequency:=Samples[SampleIndex].SampleRate/SampleRate;
   end;
end;

{$IFDEF FPC}
initialization
  {$i LunchBoxSetup.lrs}
{$ENDIF}

end.
