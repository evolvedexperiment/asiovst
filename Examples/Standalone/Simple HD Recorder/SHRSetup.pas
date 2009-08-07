unit SHRSetup;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls;

const
  IniFileName = 'Simple HD Recorder.INI';
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
  IniFiles, SHRmain;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmSetup.CBInputChange(Sender: TObject);
begin
 FmSimpleHDRecorder.InputChannelOffset := CBInput.ItemIndex;
end;

procedure TFmSetup.CBOutputChange(Sender: TObject);
begin
 FmSimpleHDRecorder.OutputChannelOffset := CBOutput.ItemIndex;
end;

procedure TFmSetup.FormCreate(Sender: TObject);
var
  i : Integer;
begin
 CBDrivers.Items := FmSimpleHDRecorder.ASIOHost.DriverList;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + IniFileName) do
  try
   Top := ReadInteger('Layout', 'Setup Top', Top);
   Left := ReadInteger('Layout', 'Setup Left', Left);

   for i := 0 to CBDrivers.Items.Count - 1 do
    if Pos('M-Audio', CBDrivers.Items[i]) > 0 then
     begin
      CBDrivers.ItemIndex := i;
      Break;
     end;
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
 with FmSimpleHDRecorder.ASIOHost do
  if CBDrivers.ItemIndex >= 0 then
   begin
    Active := False;
    DriverIndex := CBDrivers.ItemIndex;
    CBInput.Clear;
    for i := 0 to InputChannelCount - 1
     do CBInput.Items.Add(InputChannelInfos[i].Name);
    CBOutput.Clear;
    for i := 0 to OutputChannelCount - 1
     do CBOutput.Items.Add(OutputChannelInfos[i].Name);
    CBInput.ItemIndex := 0;
    CBOutput.ItemIndex := 0;
    if assigned(OnReset)
     then OnReset(Self);
    Active := True;
   end;
end;

procedure TFmSetup.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + IniFileName) do
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

