unit AnalyserForm;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2019        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, Spin, Math, TeeProcs, 
  TeEngine, Chart, Series, DAV_Types, DAV_Complex, DAV_ASIOHost,
  VclTee.TeeGDIPlus;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies: array [0 .. CNumFrequencies - 1] of Single = (
    16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
    630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
    10000, 12500, 16000, 20000);

type
  TFormAnalyser = class(TForm)
    ChartAnalyser: TChart;
    ASIOHost: TASIOHost;
    BarSeries: TBarSeries;
    ButtonAnalyse: TButton;
    ButtonControlPanel: TButton;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    LabelChannels: TLabel;
    LabelFullScaleUnit: TLabel;
    LabelDriverName: TLabel;
    LabelFullscale: TLabel;
    LabelSpeed: TLabel;
    RadioButtonFast: TRadioButton;
    RadioButtonMedium: TRadioButton;
    RadioButtonSlow: TRadioButton;
    SpinEditFullscaleGain: TSpinEdit;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ChartAnalyserDblClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ButtonAnalyseClick(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure RadioButtonFastClick(Sender: TObject);
    procedure RadioButtonMediumClick(Sender: TObject);
    procedure RadioButtonSlowClick(Sender: TObject);
    procedure SpinEditFullscaleGainChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FMagnitudes: array [0 .. CNumFrequencies - 1] of Double;
    FThirdOctaveExp: array [0 .. CNumFrequencies - 1] of TComplex32;
    FSpeedConst: array [0 .. 1] of Single;
    FChannelNr: Integer;
    FSampleRateReci: Double;
    FFSGain: Single;
    FIniFile: TFileName;
    FBuffer: PDAVSingleFixedArray;  // the buffer
    FBufferSize: Integer;           // buffer size
    FBufferPosition: Integer;       // position within the Buffer
    FBufferOverlap: Integer;        // overlap in samples
    procedure DoGoertzelMagic;
    procedure CalculateWeight;
    procedure CalculateComplexAngulars;
  end;

var
  FormAnalyser: TFormAnalyser;

implementation

{$R *.DFM}

uses
  Inifiles, Registry, DAV_Common, DAV_Math, DAV_ASIOConvert, DAV_DspDft;

procedure TFormAnalyser.FormCreate(Sender: TObject);
var
  Band: Integer;
begin
  FChannelNr := 0;
  FSpeedConst[0] := 0.99;
  CalculateWeight;

  FBufferPosition := 0;
  FBufferOverlap  := 15 * 1024;
  FBufferSize := 16 * 1024;
  ReallocMem(FBuffer, FBufferSize * SizeOf(Single));

  FFSGain := SpinEditFullscaleGain.Value;
  FSampleRateReci := 1 / ASIOHost.SampleRate;
  ComboBoxDriver.Items := ASIOHost.DriverList;

  if ComboBoxDriver.Items.Count = 0 then
  try
    raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
    Application.Terminate;
  end;

  // set absolute ini file
  FIniFile := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';

  // and make sure all controls are enabled or disabled
  with TIniFile.Create(FIniFile) do
  try
    Left := ReadInteger('Layout', 'Audio Left', Left);
    Top := ReadInteger('Layout', 'Audio Top', Top);
    ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
    if ComboBoxDriver.ItemIndex >= 0 then
      ComboBoxDriverChange(ComboBoxDriver);
    ComboBoxChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
    SpinEditFullscaleGain.Value := ReadInteger('Audio', 'Fullscale Gain', 0);
  finally
    Free;
  end;

  CalculateComplexAngulars;
  for Band := 0 to CNumFrequencies - 1 do
  begin
    {$IFNDEF FPC}
    if CThirdOctaveFrequencies[Band] < 1000 then
      BarSeries.Add(0,FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz')
    else
      BarSeries.Add(0,FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz');
    {$ELSE}
    if Frequency < 1000 then
      ChartAnalyser.AddBar(FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz', 0, $000000FF)
    else
      ChartAnalyser.AddBar(FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz', 0, $000000FF);
    {$ENDIF}
  end;

  ASIOHostSampleRateChanged(Sender);
end;

procedure TFormAnalyser.FormDestroy(Sender: TObject);
begin
  ASIOHost.Active := False;
  with TIniFile.Create(FIniFile) do
  try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
    WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
    WriteInteger('Audio', 'Fullscale Gain', SpinEditFullscaleGain.Value);
  finally
    Free;
  end;

  Dispose(FBuffer);
end;

procedure TFormAnalyser.CalculateComplexAngulars;
var
  Band: Integer;
begin
  for Band := 0 to CNumFrequencies - 1 do
    GetSinCos(CThirdOctaveFrequencies[Band] * FSampleRateReci, FThirdOctaveExp[Band].Im, FThirdOctaveExp[Band].Re);
end;

procedure TFormAnalyser.RadioButtonFastClick(Sender: TObject);
begin
  FSpeedConst[0] := 0.9;
  CalculateWeight;
end;

procedure TFormAnalyser.RadioButtonMediumClick(Sender: TObject);
begin
  FSpeedConst[0] := 0.99;
  CalculateWeight;
end;

procedure TFormAnalyser.RadioButtonSlowClick(Sender: TObject);
begin
  FSpeedConst[0] := 0.999;
  CalculateWeight;
end;

procedure TFormAnalyser.CalculateWeight;
begin
  FSpeedConst[1] := 0.5 * (1 - FSpeedConst[0]);
end;

procedure TFormAnalyser.SpinEditFullscaleGainChange(Sender: TObject);
begin
  FFSGain := SpinEditFullscaleGain.Value;
// ChartAnalyser.LeftAxis.Maximum := FFSGain + 20;
end;

procedure TFormAnalyser.ComboBoxDriverChange(Sender: TObject);
var
  i: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonAnalyse.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for i := 0 to ASIOHost.InputChannelCount - 1 do
      ComboBoxChannel.Items.Add(string(ASIOHost.InputChannelInfos[i].name));
    with TIniFile.Create(FIniFile) do
    try
      WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;
    ButtonControlPanel.Enabled := True;
    ButtonAnalyse.Enabled := True;
    ComboBoxChannel.ItemIndex := 0;
  end;
end;

procedure TFormAnalyser.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFormAnalyser.ButtonAnalyseClick(Sender: TObject);
begin
  if ButtonAnalyse.Caption = 'Analyse' then
  begin
    ASIOHost.Active := True; // Start Audio
    ButtonAnalyse.Caption := 'Stop';
  end
  else
  begin
    ASIOHost.Active := False; // Stop Audio
    ButtonAnalyse.Caption := 'Analyse';
  end;
  Timer.Enabled := ASIOHost.Active;
end;

procedure TFormAnalyser.ASIOHostSampleRateChanged(Sender: TObject);
begin
  FSampleRateReci := 1 / ASIOHost.SampleRate;
  CalculateComplexAngulars;
end;

procedure TFormAnalyser.BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  CurrentPosition: Integer;
begin
  CurrentPosition := 0;
  repeat
    if FBufferPosition + (ASIOHost.BufferSize - CurrentPosition) < FBufferSize then
    begin
      Move(InBuffer[0, CurrentPosition], FBuffer[FBufferPosition],
        (ASIOHost.BufferSize - CurrentPosition) * Sizeof(Single));
      FBufferPosition  := FBufferPosition + (ASIOHost.BufferSize - CurrentPosition);
      CurrentPosition := ASIOHost.BufferSize;
    end
    else
    begin
      Move(InBuffer[0, CurrentPosition], FBuffer[FBufferPosition],
        (FBufferSize - FBufferPosition) * Sizeof(Single));

      DoGoertzelMagic; // do Processing here!

      Move(FBuffer[FBufferSize - FBufferOverlap], FBuffer[0],
        FBufferOverlap * Sizeof(Single));

      CurrentPosition := CurrentPosition + (FBufferSize - FBufferPosition);
      FBufferPosition := FBufferOverlap;
    end;
  until CurrentPosition >= ASIOHost.BufferSize;
end;

procedure TFormAnalyser.DoGoertzelMagic;
var
  i: Integer;
  bs: Integer;
begin
 for i := 0 to CNumFrequencies - 1 do
  begin
   bs := round(sqr(sqr(1 - CThirdOctaveFrequencies[i] * FSampleRateReci)) * FBufferSize);
   with Goertzel(PDAVSingleFixedArray(@FBuffer^[(FBufferSize - bs) div 2]), bs, FThirdOctaveExp[i]) do
     FMagnitudes[i] := FSpeedConst[0] * FMagnitudes[i] + FSpeedConst[1] *
       Amp_to_dB(Sqr(Re) + Sqr(Im));
  end;
end;

procedure TFormAnalyser.ChartAnalyserDblClick(Sender: TObject);
begin
  with ChartAnalyser do
    if Align <> alClient then
      Align := alClient
    else
    begin
      Align := alBottom;
      Top := 88;
      Height := Self.ClientHeight - 88;
    end;
end;

procedure TFormAnalyser.TimerTimer(Sender: TObject);
var
  Band: Integer;
begin
{$IFNDEF FPC}
  for Band := 0 to cNumFrequencies - 1 do
    BarSeries.YValue[Band] := FMagnitudes[Band] + FFSGain;
  ChartAnalyser.Invalidate;
 {$ELSE}
  for j := 0 to cNumFrequencies - 1 do
    TBar(ChartAnalyser.Bars.Items[Band]).Value := round(FMagnitudes[Band] + FFSGain);
  ChartAnalyser.Invalidate;
 {$ENDIF}
end;

initialization
  Set8087CW(Default8087CW or $3F);

end.
