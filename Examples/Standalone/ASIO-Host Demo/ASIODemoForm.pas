unit AsioDemoForm;

{$I ASIOVST.INC}

interface

uses {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
     Forms, Classes, Controls, StdCtrls, DASIOHost, DAVDCommon, DAVDComplex;

type
  TFmASIO = class(TForm)
    Bt_CP: TButton;
    Bt_Play: TButton;
    DriverCombo: TComboBox;
    ChannelBox: TComboBox;
    ASIOHost: TASIOHost;
    SbFreq: TScrollBar;
    SbVolume: TScrollBar;
    SbPan: TScrollBar;
    LbFreq: TLabel;
    LbVolume: TLabel;
    LbPanorama: TLabel;
    Lb_Drivername: TLabel;
    Lb_Channels: TLabel;
    Lb_Copyright: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure Bt_PlayClick(Sender: TObject);
    procedure SbFreqChange(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
      OutBuffer: TArrayOfDoubleDynArray);
  private
    procedure SetFrequency(const Value: Double);
  public
    fAngle, fPosition   : TComplexDouble;
    fPan, fFreq, fVol   : Double;
  published
    property Frequency : Double read fFreq write SetFrequency;
  end;

var
  FmASIO        : TFmASIO;
  VolumeFactor  : Single = 1;
  ChannelOffset : Byte = 0;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses SysUtils, Inifiles;

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
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
  finally
   Free;
  end;

 fPosition.Re:=0;
 fPosition.Im:=-1;
 fFreq := 1000; fPan:=0.5; fVol:=1;
 fAngle.Re:=cos(2*Pi*fFreq/ASIOHost.SampleRate);
 fAngle.Im:=sin(2*Pi*fFreq/ASIOHost.SampleRate);
end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
var i        : Integer;
begin
 Bt_CP.Enabled := False;
 Bt_Play.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for i := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
   begin
    ChannelBox.Items.Add(
     ASIOHost.OutputChannelInfos[2 * i].name + ' / ' +
     ASIOHost.OutputChannelInfos[2 * i + 1].name);
   end;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   Bt_CP.Enabled := True;
   Bt_Play.Enabled := True;
   ChannelBox.ItemIndex := 0;
  end;
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
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
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

procedure TFmASIO.ChannelBoxChange(Sender: TObject);
begin
 ChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmASIO.SbFreqChange(Sender: TObject);
begin
 Frequency:=FreqLinearToLog(SbFreq.Position * 0.00001);
end;

procedure TFmASIO.SetFrequency(const Value: Double);
begin
 if fFreq<>Value then
  begin
   fFreq := Value;
   LbFreq.Caption:='Frequency: '+FloatTostrF(fFreq,ffGeneral,5,5)+' Hz';
   fAngle.Re:=cos(2*Pi*fFreq/ASIOHost.SampleRate);
   fAngle.Im:=sin(2*Pi*fFreq/ASIOHost.SampleRate);
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TArrayOfDoubleDynArray);
var i: integer;
    s: single;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
 begin
  s:=fPosition.Re*fAngle.Re-fPosition.Im*fAngle.Im;
  fPosition.Im:=fPosition.Im*fAngle.Re+fPosition.Re*fAngle.Im;
  fPosition.Re:=s; s:=s * fVol;
  OutBuffer[0,i] := s * (1 - fPan);
  OutBuffer[1,i] := s * fPan;
 end;
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
 fAngle.Re:=cos(2*Pi*fFreq/ASIOHost.SampleRate);
 fAngle.Im:=sin(2*Pi*fFreq/ASIOHost.SampleRate);
end;

procedure TFmASIO.SbVolumeChange(Sender: TObject);
begin
 fVol := SbVolume.position * 0.00001;
 if fVol=0
  then LbVolume.Caption:='Volume: 0 equals -oo dB'
  else LbVolume.Caption:='Volume: '+FloattostrF(fVol,ffFixed,2,2)+' equals '+FloattostrF(Amp_to_dB(fVol),ffGeneral,2,2)+' dB';
end;

procedure TFmASIO.SbPanChange(Sender: TObject);
begin
 fPan := SbPan.Position * 0.01;
 if fPan=0.5
  then LbPanorama.Caption:='Panorama: C'
  else LbPanorama.Caption:='Panorama: '+Inttostr(round(100*(fPan*2-1)));
end;

{$IFDEF FPC}
initialization
  {$i AsioDemoForm.lrs}
{$ENDIF}

end.

