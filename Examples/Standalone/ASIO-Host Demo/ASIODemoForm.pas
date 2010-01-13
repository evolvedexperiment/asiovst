unit AsioDemoForm;

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
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde                                                        //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Complex, DAV_Types, DAV_ASIOHost;

type
  TFmASIO = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtStartStop: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    Lb_Channels: TLabel;
    Lb_Copyright: TLabel;
    Lb_Drivername: TLabel;
    LbFreq: TLabel;
    LbPanorama: TLabel;
    LbVolume: TLabel;
    SbFreq: TScrollBar;
    SbPan: TScrollBar;
    SbVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure SbFreqChange(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure Lb_ChannelsClick(Sender: TObject);
  private
    procedure SetFrequency(const Value: Double);
  public
    FAngle, FPosition   : TComplexDouble;
    FPan, FFreq, FVol   : Double;
    FChannelOffset      : Byte;
  published
    property Frequency : Double read FFreq write SetFrequency;
  end;

var
  FmASIO : TFmASIO;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Common, DAV_Math;

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

 FPosition.Re   :=    0;
 FPosition.Im   :=   -1;
 FFreq          := 1000;
 FPan           :=    0.5;
 FVol           :=    1;
 FChannelOffset :=    0;
 GetSinCos(2 * Pi * FFreq / ASIOHost.SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
var
  Channel : Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for Channel := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
    begin
     ChannelBox.Items.Add(
       ASIOHost.OutputChannelInfos[2 * Channel].Name + ' / ' +
       ASIOHost.OutputChannelInfos[2 * Channel + 1].Name);
    end;

   // store current ASIO driver index
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := True;
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmASIO.BtControlPanelClick(Sender: TObject);
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

procedure TFmASIO.Lb_ChannelsClick(Sender: TObject);
begin
 AsioHost.CanSampleRate(48000);
 AsioHost.SampleRate := 48000;
 AsioHost.CanSampleRate(44100);
 AsioHost.SampleRate := 44100;
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

procedure TFmASIO.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
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
   GetSinCos(2 * Pi * FFreq / ASIOHost.SampleRate, FAngle.Im, FAngle.Re);
  end;
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

procedure TFmASIO.SbPanChange(Sender: TObject);
begin
 FPan := SbPan.Position * 0.01;
 if FPan = 0.5
  then LbPanorama.Caption := 'Panorama: C'
  else LbPanorama.Caption := 'Panorama: ' + Inttostr(round(100 * (FPan * 2 - 1)));
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
 GetSinCos(2 * Pi * FFreq / ASIOHost.SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TFmASIO.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample : Integer;
  Data   : Double;
  L, R   : Integer;
begin
 L := FChannelOffset;
 R := L + 1;
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   Data := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
   FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
   FPosition.Re := Data; Data := Data * FVol;
   OutBuffer[L, Sample] := (1 - FPan) * Data;
   OutBuffer[R, Sample] := FPan * Data;
  end;

  { following line was removed by MyCo
    @C.W.B: When I uncomment this, ASIO doesn't work at all, and the EXE crashs when I close it
            please confirm this change, because I don't know why this line was added}

  //  ASIOHost.Active := False;
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Sample : Integer;
  Data   : Double;
  L, R   : Integer;
begin
 L := FChannelOffset;
 R := L + 1;
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   Data := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
   FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
   FPosition.Re := Data; Data := Data * FVol;
   OutBuffer[L, Sample] := (1 - FPan) * Data;
   OutBuffer[R, Sample] := FPan * Data;
  end;
end;

{$IFDEF FPC}
initialization
  {$i AsioDemoForm.lrs}
{$ENDIF}

end.

