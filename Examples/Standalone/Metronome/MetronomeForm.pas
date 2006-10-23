unit MetronomeForm;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Math, StdCtrls, ComCtrls, DASIOHost, ExtCtrls, DDspBase, Spin;

type
  TComplexDouble = record
                    Re, Im : Double;
                   end;
  TFmASIO = class(TForm)
    Bt_CP: TButton;
    Bt_Play: TButton;
    DriverCombo: TComboBox;
    ASIOHost: TASIOHost;
    Lb_Drivername: TLabel;
    LbTempo: TLabel;
    SETempo: TSpinEdit;
    LbBPM: TLabel;
    LbVolume: TLabel;
    SBVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure Bt_PlayClick(Sender: TObject);
    procedure ASIOHostBufferSwitch(Sender: TObject; InBuffer, OutBuffer: TArrayOfSingleArray);
    procedure SBVolumeChange(Sender: TObject);
    procedure SETempoChange(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
  private
    fAngle     : TComplexDouble;
    fPosition  : TComplexDouble;
    fVolume    : Single;
    fBeatPos   : Integer;
    procedure CalculateSineAngles;
  public
    fSamplesPerBeat : Single;
    fSamplesCount   : Single;
    fMetroVolume    : Single;
  published
  end;

var FmASIO        : TFmASIO;

implementation

{$R *.DFM}

uses inifiles, registry, DASIOConvert;

procedure TFmASIO.FormCreate(Sender: TObject);
begin
 DriverCombo.Items := ASIOHost.DriverList;
 fSamplesPerBeat:=60/SETempo.Value*ASIOHost.SampleRate;
 fSamplesCount := 0;
 fMetroVolume := 1;
 fVolume := 1;
 CalculateSineAngles;
 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
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
var i        : Integer;
begin
 Bt_CP.Enabled := False;
 Bt_Play.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex:=DriverCombo.ItemIndex;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   Bt_CP.Enabled := True;
   Bt_Play.Enabled := True;
  end;
end;

procedure TFmASIO.CalculateSineAngles;
var w0 : Single;
begin
 w0:=2*Pi*1000/ASIOHost.SampleRate;
 fAngle.Re:=cos(w0);
 fAngle.Im:=sin(w0);
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
 fSamplesPerBeat:=60/SETempo.Value*ASIOHost.SampleRate;
 CalculateSineAngles;
end;

procedure TFmASIO.Bt_CPClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
  finally
   Free;
  end;
end;

procedure TFmASIO.SBVolumeChange(Sender: TObject);
begin
 fVolume:=db_to_Amp(SBVolume.Position);
end;

procedure TFmASIO.SETempoChange(Sender: TObject);
begin
 fSamplesPerBeat:=60/SETempo.Value*ASIOHost.SampleRate;
end;

procedure TFmASIO.Bt_PlayClick(Sender: TObject);
begin
 if Bt_Play.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True; // Start Audio
   Bt_Play.Caption := 'Stop Audio';
   fMetroVolume:=1;
   fSamplesCount:=0;
   fPosition.Re:=1;
   fPosition.Im:=0;
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   Bt_Play.Caption := 'Start Audio';
   fBeatPos:=0;
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch(Sender: TObject; InBuffer,
  OutBuffer: TArrayOfSingleArray);
var i,j : Integer;
    s   : Single;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   s:=fPosition.Re*fAngle.Re-fPosition.Im*fAngle.Im;
   fPosition.Im:=fPosition.Im*fAngle.Re+fPosition.Re*fAngle.Im;
   fPosition.Re:=s;

   if fBeatPos=0 then
    begin
     s:=fPosition.Re*fAngle.Re-fPosition.Im*fAngle.Im;
     fPosition.Im:=fPosition.Im*fAngle.Re+fPosition.Re*fAngle.Im;
     fPosition.Re:=s;
    end;

   s:=fVolume*s*fMetroVolume;

   for j := 0 to ASIOHost.OutputChannels - 1 do OutBuffer[j,i] := s;
   fMetroVolume:=0.995*fMetroVolume;
   fSamplesCount:=fSamplesCount+1;
   if fSamplesCount>fSamplesPerBeat then
    begin
     fMetroVolume:=1;
     fSamplesCount:=fSamplesCount-fSamplesPerBeat;
     fPosition.Re:=1;
     fPosition.Im:=0;
     if fBeatPos<3
      then inc(fBeatPos)
      else fBeatPos:=0;
    end;
  end;
end;

end.
