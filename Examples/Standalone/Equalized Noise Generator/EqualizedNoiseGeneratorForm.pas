unit EqualizedNoiseGeneratorForm;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, ExtCtrls, DAV_Common, DAV_ASIOHost,
  DAV_DspFilter, DAV_DspFilterBasics;

type
  TFmASIO = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtPlay: TButton;
    DriverCombo: TComboBox;
    Lb_Drivername: TLabel;
    Lb10k: TLabel;
    Lb1200: TLabel;
    Lb160: TLabel;
    Lb20: TLabel;
    Lb20k: TLabel;
    Lb2500: TLabel;
    Lb320: TLabel;
    Lb40: TLabel;
    Lb5k: TLabel;
    Lb640: TLabel;
    Lb80: TLabel;
    LbLM: TLabel;
    LbRS: TLabel;
    MiddleL: TShape;
    MiddleR: TShape;
    SB10kL: TScrollBar;
    SB10kR: TScrollBar;
    SB1200L: TScrollBar;
    SB1200R: TScrollBar;
    SB160L: TScrollBar;
    SB160R: TScrollBar;
    SB20kL: TScrollBar;
    SB20kR: TScrollBar;
    SB20L: TScrollBar;
    SB20R: TScrollBar;
    SB2500L: TScrollBar;
    SB2500R: TScrollBar;
    SB320L: TScrollBar;
    SB320R: TScrollBar;
    SB40L: TScrollBar;
    SB40R: TScrollBar;
    SB5kL: TScrollBar;
    SB5kR: TScrollBar;
    SB640L: TScrollBar;
    SB640R: TScrollBar;
    SB80L: TScrollBar;
    SB80R: TScrollBar;
    ShBackText: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure BtPlayClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure SliderChange(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
  private
    FEQs : Array [0..1, 0..10] of TBasicPeakFilter;
  end;

var
  FmASIO: TFmASIO;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles;

procedure TFmASIO.FormCreate(Sender: TObject);
var
  Channel, Band : Integer;
const
  CDefaultFrequencies : Array [0..10] of Single = (20, 40, 80, 160, 320, 640,
    1250, 2500, 5000, 10000, 20000);   
begin
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'EqualizedNoiseGenerator.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
  finally
   Free;
  end;

 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   begin
    if not Assigned(FEQs[Channel, Band])
     then FEQs[Channel, Band] := TBasicPeakFilter.Create;
    with FEQs[Channel, Band] do
     begin
      SampleRate := ASIOHost.SampleRate;
      Frequency := CDefaultFrequencies[Band];
      Bandwidth := 1;
     end;
   end;

end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
begin
 BtPlay.Enabled := False;
 BtControlPanel.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'EqualizedNoiseGenerator.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   BtPlay.Enabled := True;
   BtControlPanel.Enabled := True;
  end;
end;

procedure TFmASIO.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
var
  Channel, Band : Integer;
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'EqualizedNoiseGenerator.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
  finally
   Free;
  end;

 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   if not Assigned(FEQs[Channel, Band]) then FreeAndNil(FEQs[Channel, Band]);
end;

procedure TFmASIO.SliderChange(Sender: TObject);
begin
 with (Sender As TScrollBar)
  do FEQs[Tag div 11, Tag mod 11].Gain := -Position * 0.1;
end;

procedure TFmASIO.BtPlayClick(Sender: TObject);
begin
 if BtPlay.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True; // Start Audio
   BtPlay.Caption := 'Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   BtPlay.Caption := 'Start Audio';
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample : integer;
begin
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   OutBuffer[0, Sample] := FEQs[0,  0].ProcessSample(FEQs[0, 1].ProcessSample(
                           FEQs[0,  2].ProcessSample(FEQs[0, 3].ProcessSample(
                           FEQs[0,  4].ProcessSample(FEQs[0, 5].ProcessSample(
                           FEQs[0,  6].ProcessSample(FEQs[0, 7].ProcessSample(
                           FEQs[0,  8].ProcessSample(FEQs[0, 9].ProcessSample(
                           FEQs[0, 10].ProcessSample((2 * random - 1))))))))))));
   OutBuffer[1, Sample] := FEQs[1,  0].ProcessSample(FEQs[1, 1].ProcessSample(
                           FEQs[1,  2].ProcessSample(FEQs[1, 3].ProcessSample(
                           FEQs[1,  4].ProcessSample(FEQs[1, 5].ProcessSample(
                           FEQs[1,  6].ProcessSample(FEQs[1, 7].ProcessSample(
                           FEQs[1,  8].ProcessSample(FEQs[1, 9].ProcessSample(
                           FEQs[1, 10].ProcessSample((2 * random - 1))))))))))));
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Sample : integer;
begin
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   OutBuffer[0, Sample] := FEQs[0,  0].ProcessSample(FEQs[0, 1].ProcessSample(
                           FEQs[0,  2].ProcessSample(FEQs[0, 3].ProcessSample(
                           FEQs[0,  4].ProcessSample(FEQs[0, 5].ProcessSample(
                           FEQs[0,  6].ProcessSample(FEQs[0, 7].ProcessSample(
                           FEQs[0,  8].ProcessSample(FEQs[0, 9].ProcessSample(
                           FEQs[0, 10].ProcessSample((2 * random - 1))))))))))));
   OutBuffer[1, Sample] := FEQs[1,  0].ProcessSample(FEQs[1, 1].ProcessSample(
                           FEQs[1,  2].ProcessSample(FEQs[1, 3].ProcessSample(
                           FEQs[1,  4].ProcessSample(FEQs[1, 5].ProcessSample(
                           FEQs[1,  6].ProcessSample(FEQs[1, 7].ProcessSample(
                           FEQs[1,  8].ProcessSample(FEQs[1, 9].ProcessSample(
                           FEQs[1, 10].ProcessSample((2 * random - 1))))))))))));
  end;
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
var
  Channel, Band : Integer;
begin
 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   begin
    if not Assigned(FEQs[Channel, Band])
     then FEQs[Channel, Band] := TBasicPeakFilter.Create;
    FEQs[Channel, Band].SampleRate := ASIOHost.SampleRate;
   end;
end;

{$IFDEF FPC}
initialization
  {$i EqualizedNoiseGeneratorForm.lrs}
{$ENDIF}

end.

