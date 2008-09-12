unit NoiseGenForm;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAVASIOHost, DAVDCommon;

type
  TFmASIO = class(TForm)
    ASIOHost: TASIOHost;
    Bt_Play: TButton;
    DriverCombo: TComboBox;
    Lb_Copyright: TLabel;
    Lb_Drivername: TLabel;
    LbPanorama: TLabel;
    LbVolume: TLabel;
    SbPan: TScrollBar;
    SbVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfSingleDynArray);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfDoubleDynArray);
    procedure Bt_CPClick(Sender: TObject);
    procedure Bt_PlayClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
  private
    fVol, fPan : Single;  
  end;

var
  FmASIO        : TFmASIO;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles;

procedure TFmASIO.FormCreate(Sender: TObject);
begin
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
  finally
   Free;
  end;
end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
begin
 Bt_Play.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   Bt_Play.Enabled := True;
  end;
end;

procedure TFmASIO.Bt_CPClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'NoiseGen.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
  finally
   Free;
  end; 
end;

procedure TFmASIO.Bt_PlayClick(Sender: TObject);
begin
 if Bt_Play.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True; // Start Audio
   Bt_Play.Caption := 'Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   Bt_Play.Caption := 'Start Audio';
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TAVDArrayOfSingleDynArray);
var
  i, j : integer;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   for j := 0 to ASIOHost.OutputChannelCount - 1
    do OutBuffer[j, i] := (2 * random - 1) * fVol;
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TAVDArrayOfDoubleDynArray);
var
  i, j : integer;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   for j := 0 to ASIOHost.OutputChannelCount - 1
    do OutBuffer[j, i] := (2 * random - 1) * fVol;
  end;
end;

procedure TFmASIO.SbVolumeChange(Sender: TObject);
begin
 fVol := SbVolume.position * 0.00001;
 if fVol=0
  then LbVolume.Caption := 'Volume: 0 equals -oo dB'
  else LbVolume.Caption := 'Volume: ' +
                           FloattostrF(fVol, ffFixed, 2, 2) + ' equals ' +
                           FloattostrF(Amp_to_dB(fVol), ffGeneral, 2, 2) + ' dB';
end;

procedure TFmASIO.SbPanChange(Sender: TObject);
begin
 fPan := SbPan.Position * 0.01;
 if fPan = 0.5
  then LbPanorama.Caption := 'Panorama: C'
  else LbPanorama.Caption := 'Panorama: ' + Inttostr(round(100 * (fPan * 2 - 1)));
end;

{$IFDEF FPC}
initialization
  {$i NoiseGenForm.lrs}
{$ENDIF}

end.

