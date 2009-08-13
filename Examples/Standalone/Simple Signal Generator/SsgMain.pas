unit SsgMain;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Complex, DAV_Common, DAV_ASIOHost,
  DAV_DspPinkNoiseGenerator, DAV_DspLfo;

type
  TSignalType = (stSine, stWhiteNoise, stPinkNoise);
  TNoiseDistribution = (ndRectangle, ndTriangular, ndFastGauss, ndGauss);
  TFmASIO = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtStartStop: TButton;
    CbDriver: TComboBox;
    CbSignal: TComboBox;
    LbDrivername: TLabel;
    LbCopyright: TLabel;
    LbFreq: TLabel;
    LbSignal: TLabel;
    LbVolume: TLabel;
    SbFreq: TScrollBar;
    SbVolume: TScrollBar;
    LbDistribution: TLabel;
    CbDistribution: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure CbDriverChange(Sender: TObject);
    procedure SbFreqChange(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure CbSignalChange(Sender: TObject);
    procedure CbDistributionChange(Sender: TObject);
  private
    procedure SetFrequency(const Value: Double);
  public
    FFreq, FVol        : Double;
    FSignalType        : TSignalType;
    FSineLFO           : TLFOSine64;
    FPinkNoise         : TPinkNoiseGenerator;
    FNoiseDistribution : TNoiseDistribution;
  published
    property Frequency : Double read FFreq write SetFrequency;
    property SignalType: TSignalType read FSignalType;
    property NoiseDistribution: TNoiseDistribution read FNoiseDistribution;
  end;

var
  FmASIO : TFmASIO;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Approximations;

procedure TFmASIO.FormCreate(Sender: TObject);
begin
 CbDriver.Items := ASIOHost.DriverList;
 if CbDriver.Items.Count = 0 then
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
   CbDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if CbDriver.ItemIndex >= 0 then CbDriverChange(CbDriver);
  finally
   Free;
  end;

 FFreq          := 1000;
 FVol           :=    1;

 FSineLFO       := TLFOSine64.Create;
 with FSineLFO do
  begin
   Frequency := FFreq;
   SampleRate := ASIOHost.SampleRate;
  end;

 FPinkNoise     := TPinkNoiseGenerator.Create;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
begin
 ASIOHost.Active := False;
 FreeAndNil(FSineLFO);
 FreeAndNil(FPinkNoise);

 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', CbDriver.ItemIndex);
  finally
   Free;
  end;
end;

procedure TFmASIO.CbDistributionChange(Sender: TObject);
begin
 FNoiseDistribution := TNoiseDistribution(CbDistribution.ItemIndex);
end;

procedure TFmASIO.CbDriverChange(Sender: TObject);
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 CbDriver.ItemIndex := CbDriver.Items.IndexOf(CbDriver.Text);
 if CbDriver.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := CbDriver.ItemIndex;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', CbDriver.ItemIndex);
    finally
     Free;
    end;
   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := True;
  end;
end;

procedure TFmASIO.CbSignalChange(Sender: TObject);
begin
 FSignalType := TSignalType(CbSignal.ItemIndex);

 SbFreq.Visible := FSignalType = stSine;
 LbFreq.Visible := FSignalType = stSine;
 LbDistribution.Visible := FSignalType = stWhiteNoise;
 CbDistribution.Visible := FSignalType = stWhiteNoise;
 case FSignalType of
        stSine : ClientHeight := 173;
  stWhiteNoise : ClientHeight := 155;
   stPinkNoise : ClientHeight := 132;
 end;
end;

procedure TFmASIO.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIO.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True; // Start Audio
   BtStartStop.Caption := 'Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   BtStartStop.Caption := 'Start Audio';
  end;
end;

procedure TFmASIO.SbFreqChange(Sender: TObject);
begin
 Frequency := FreqLinearToLog(SbFreq.Position * 0.00001);
end;

procedure TFmASIO.SetFrequency(const Value: Double);
begin
 if FFreq <> Value then
  begin
   FFreq := Value;
   LbFreq.Caption := 'Frequency: ' + FloatTostrF(FFreq, ffGeneral, 5, 5) + ' Hz';
   FSineLFO.Frequency := FFreq;
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Sample  : Integer;
begin
 case FSignalType of
  stSine :
    for Sample := 0 to ASIOHost.BufferSize - 1 do
     begin
      OutBuffer[0, Sample] := FSineLFO.Sine * FVol;
      FSineLFO.CalculateNextSample;
     end;
  stWhiteNoise :
    case FNoiseDistribution of
      ndRectangle : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := (2 * Random - 1) * FVol;
     ndTriangular : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := (Random - Random) * FVol;
      ndFastGauss : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := FastRandomGauss * FVol;
          ndGauss : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := RandomGauss * FVol;
    end;
  stPinkNoise :
    for Sample := 0 to ASIOHost.BufferSize - 1
     do OutBuffer[0, Sample] := FPinkNoise.ProcessSample;
 end;

 // copy signal to all channels
 for Channel := 1 to ASIOHost.OutputChannelCount - 1
  do Move(OutBuffer[0, 0], OutBuffer[Channel, 0], ASIOHost.BufferSize * SizeOf(Single));
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Channel : Integer;
  Sample  : Integer;
begin
 case FSignalType of
  stSine :
    for Sample := 0 to ASIOHost.BufferSize - 1 do
     begin
      OutBuffer[0, Sample] := FSineLFO.Sine * FVol;
      FSineLFO.CalculateNextSample;
     end;
  stWhiteNoise :
    case FNoiseDistribution of
      ndRectangle : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := (2 * Random - 1) * FVol;
     ndTriangular : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := (Random - Random) * FVol;
      ndFastGauss : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := FastRandomGauss * FVol;
          ndGauss : for Sample := 0 to ASIOHost.BufferSize - 1
                     do OutBuffer[0, Sample] := RandomGauss * FVol;
    end;
  stPinkNoise :
    for Sample := 0 to ASIOHost.BufferSize - 1
     do OutBuffer[0, Sample] := FPinkNoise.ProcessSample;
 end;

 // copy signal to all channels
 for Channel := 1 to ASIOHost.OutputChannelCount - 1
  do Move(OutBuffer[0, 0], OutBuffer[Channel, 0], ASIOHost.BufferSize * SizeOf(Double));
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
 FSineLFO.SampleRate := ASIOHost.SampleRate;
end;

procedure TFmASIO.SbVolumeChange(Sender: TObject);
begin
 FVol := SbVolume.Position * 0.00001;
 if FVol = 0
  then LbVolume.Caption := 'Volume: 0 equals -oo dB'
  else LbVolume.Caption := 'Volume: ' +
                           FloattostrF(FVol, ffFixed, 2, 2) + ' equals ' +
                           FloattostrF(Amp_to_dB(FVol), ffGeneral, 2, 2) + ' dB';
end;

{$IFDEF FPC}
initialization
  {$i AsioDemoForm.lrs}
{$ENDIF}

end.

