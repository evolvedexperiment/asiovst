unit AnalyserForm;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Math, StdCtrls, ComCtrls, DASIOHost, ExtCtrls, DDspBase, DFilter,
     TeeProcs, TeEngine, Chart, Series;

type
  TComplexDouble = record
                    Re, Im : Double;
                   end;
  TFmAnalyser = class(TForm)
    Bt_CP: TButton;
    Bt_Play: TButton;
    DriverCombo: TComboBox;
    ChannelBox: TComboBox;
    ASIOHost: TASIOHost;
    Lb_Drivername: TLabel;
    Lb_Channels: TLabel;
    AnalyserChart: TChart;
    BarSeries: TBarSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure Bt_PlayClick(Sender: TObject);
    procedure ASIOHostBufferSwitch(Sender: TObject; InBuffer, OutBuffer: TArrayOfSingleArray);
  private
    fFilterArray : Array[0..31] of TSimpleBandpass;
    fFilterRMS   : Array[0..31] of Single;
    fChannelNr   : Integer;
    procedure UpdateBarGraph;
  public
  published
  end;

var
  FmAnalyser    : TFmAnalyser;

implementation

{$R *.DFM}

uses inifiles, registry, DASIOConvert;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var i : Integer;
begin
 fChannelNr:=0;
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
 with BarSeries do
  begin
   Add(0,'16 Hz');
   Add(0,'20 Hz');
   Add(0,'25 Hz');
   Add(0,'31 Hz');
   Add(0,'40 Hz');
   Add(0,'50 Hz');
   Add(0,'63 Hz');
   Add(0,'80 Hz');
   Add(0,'100 Hz');
   Add(0,'125 Hz');
   Add(0,'160 Hz');
   Add(0,'200 Hz');
   Add(0,'250 Hz');
   Add(0,'315 Hz');
   Add(0,'400 Hz');
   Add(0,'500 Hz');
   Add(0,'630 Hz');
   Add(0,'800 Hz');
   Add(0,'1 kHz');
   Add(0,'1.25 kHz');
   Add(0,'1.6 kHz');
   Add(0,'2 kHz');
   Add(0,'2.5 kHz');
   Add(0,'3.15 kHz');
   Add(0,'4 kHz');
   Add(0,'5 kHz');
   Add(0,'6.3 kHz');
   Add(0,'8 kHz');
   Add(0,'10 kHz');
   Add(0,'12.5 kHz');
   Add(0,'16 kHz');
   Add(0,'20 kHz');
  end;
 for i := 0 to Length(fFilterArray)-1 do
  begin
   fFilterArray[i]:=TSimpleBandpass.Create;
   fFilterArray[i].SampleRate:=44100;
   fFilterArray[i].Bandwidth:=1;
  end;

 fFilterArray[ 0].Frequency:=16;
 fFilterArray[ 1].Frequency:=20;
 fFilterArray[ 2].Frequency:=25;
 fFilterArray[ 3].Frequency:=31;
 fFilterArray[ 4].Frequency:=40;
 fFilterArray[ 5].Frequency:=50;
 fFilterArray[ 6].Frequency:=63;
 fFilterArray[ 7].Frequency:=80;
 fFilterArray[ 8].Frequency:=100;
 fFilterArray[ 9].Frequency:=125;
 fFilterArray[10].Frequency:=160;
 fFilterArray[11].Frequency:=200;
 fFilterArray[12].Frequency:=250;
 fFilterArray[13].Frequency:=315;
 fFilterArray[14].Frequency:=400;
 fFilterArray[15].Frequency:=500;
 fFilterArray[16].Frequency:=630;
 fFilterArray[17].Frequency:=800;
 fFilterArray[18].Frequency:=1000;
 fFilterArray[19].Frequency:=1250;
 fFilterArray[20].Frequency:=1600;
 fFilterArray[21].Frequency:=2000;
 fFilterArray[22].Frequency:=2500;
 fFilterArray[23].Frequency:=3150;
 fFilterArray[24].Frequency:=4000;
 fFilterArray[25].Frequency:=5000;
 fFilterArray[26].Frequency:=6300;
 fFilterArray[27].Frequency:=8000;
 fFilterArray[28].Frequency:=10000;
 fFilterArray[29].Frequency:=12500;
 fFilterArray[30].Frequency:=16000;
 fFilterArray[31].Frequency:=20000;
end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
var i : Integer;
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
 for i := 0 to Length(fFilterArray)-1 do fFilterArray[i].Free;
end;

procedure TFmAnalyser.DriverComboChange(Sender: TObject);
var i        : Integer;
begin
 Bt_CP.Enabled := False;
 Bt_Play.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for i := 0 to ASIOHost.InputChannels - 1
    do ChannelBox.Items.Add(ASIOHost.InputChannelInfos[i].name);
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

procedure TFmAnalyser.Bt_CPClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmAnalyser.Bt_PlayClick(Sender: TObject);
begin
 if Bt_Play.Caption = 'Analyse' then
  begin
   ASIOHost.Active := True; // Start Audio
   Bt_Play.Caption := 'Stop';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   Bt_Play.Caption := 'Analyse';
  end;
end;

procedure TFmAnalyser.UpdateBarGraph;
var i,j : Integer;
begin
 for j := 0 to Length(fFilterArray) - 1
  do BarSeries.YValue[j]:=Amp_to_dB(f_abs(fFilterRMS[j]))+120;
 AnalyserChart.Invalidate;
end;

procedure TFmAnalyser.ASIOHostBufferSwitch(Sender: TObject; InBuffer, OutBuffer: TArrayOfSingleArray);
var i,j : Integer;
    s   : Single;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   s:=InBuffer[fChannelNr,i];
   for j := 0 to Length(fFilterArray) - 1
    do fFilterRMS[j]:=0.999*fFilterRMS[j]+0.001*fFilterArray[j].Process(s+1E-24);
  end;
 UpdateBarGraph;
end;

end.
