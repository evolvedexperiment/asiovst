unit EditorSetup;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls;

type
  TFmSetup = class(TForm)
    LbPreset: TLabel;
    LbIn: TLabel;
    Label1: TLabel;
    CBDrivers: TComboBox;
    CBInput: TComboBox;
    CBOutput: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure CBInputChange(Sender: TObject);
    procedure CBOutputChange(Sender: TObject);
  end;

var
  FmSetup: TFmSetup;

implementation

uses
  IniFiles, EditorForm;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmSetup.FormCreate(Sender: TObject);
begin
 CBDrivers.Items := FmVSTEditor.ASIOHost.DriverList;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSTEditor.INI') do
  try
   Top := ReadInteger('Layout', 'Setup Top', Top);
   Left := ReadInteger('Layout', 'Setup Left', Left);
   CBDrivers.ItemIndex := ReadInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
   CBDriversChange(Self);
  finally
   Free;
  end;
end;

procedure TFmSetup.CBDriversChange(Sender: TObject);
var
  i : Integer;
begin
 with FmVSTEditor.ASIOHost do
  if CBDrivers.ItemIndex >= 0 then
   begin
    Active := False;
    DriverIndex := CBDrivers.ItemIndex;
    CBInput.Clear;
    for i := 0 to (InputChannelCount div 2) - 1 do
     begin
      CBInput.Items.Add(
        InputChannelInfos[2 * i].name + ' / ' +
        InputChannelInfos[2 * i + 1].name);
     end;
    CBOutput.Clear;
    for i := 0 to (OutputChannelCount div 2) - 1 do
     begin
      CBOutput.Items.Add(
        OutputChannelInfos[2 * i].name + ' / ' +
        OutputChannelInfos[2 * i + 1].name);
     end;
    CBInput.ItemIndex := 0;
    CBOutput.ItemIndex := 0;
    if assigned(OnReset)
     then OnReset(Self);
    Active := True;
   end;
end;

procedure TFmSetup.CBInputChange(Sender: TObject);
begin
 FmVSTEditor.InputChannelOffset := CBInput.ItemIndex * 2;
end;

procedure TFmSetup.CBOutputChange(Sender: TObject);
begin
 FmVSTEditor.OutputChannelOffset := CBOutput.ItemIndex * 2;
end;

procedure TFmSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0))+'VSTEditor.INI') do
  try
   WriteInteger('Layout', 'Setup Top', Top);
   WriteInteger('Layout', 'Setup Left', Left);
   WriteInteger('Setup', 'ASIO Driver', CBDrivers.ItemIndex);
  finally
   Free;
  end;
end;

{$IFDEF FPC}
initialization
  {$i EditorSetup.lrs}
{$ENDIF}

end.

