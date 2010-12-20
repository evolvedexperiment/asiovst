unit AaseMain;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, Graphics, Controls, StdCtrls, ExtCtrls, Dialogs, Menus, DAV_Types,
  DAV_DspBufferedMp3Player, DAV_DspSimpleOscillator, DAV_GuiPixelMap,
  DAV_GuiLabel, DAV_GuiCustomControl, DAV_GuiGraphicControl, DAV_AsioHost,
  DAV_GuiSlider, DAV_GuiButton;

type
  TFmAASE = class(TForm)
    ASIOHost: TASIOHost;
    OpenDialog: TOpenDialog;
    MainMenu: TMainMenu;
    MiFile: TMenuItem;
    MiExit: TMenuItem;
    N1: TMenuItem;
    MiOpen: TMenuItem;
    MiSaveAs: TMenuItem;
    MiSave: TMenuItem;
    MiSettings: TMenuItem;
    MiAsio: TMenuItem;
    SlLevelMp3: TGuiSlider;
    LbMp3Level: TGuiLabel;
    GuiLabel1: TGuiLabel;
    SlLevelSynth: TGuiSlider;
    BtStartStop: TGuiButton;
    N2: TMenuItem;
    MiAdvanced: TMenuItem;
    LbMp3File: TGuiLabel;
    LbMp3FileName: TGuiButton;
    GuiLabel2: TGuiLabel;
    SlOsc1Freq: TGuiSlider;
    SlOsc1Gain: TGuiSlider;
    GuiLabel3: TGuiLabel;
    GuiSlider1: TGuiSlider;
    GuiSlider2: TGuiSlider;
    GuiLabel4: TGuiLabel;
    GuiSlider3: TGuiSlider;
    GuiSlider4: TGuiSlider;
    GuiLabel5: TGuiLabel;
    GuiSlider5: TGuiSlider;
    GuiSlider6: TGuiSlider;
    GuiLabel6: TGuiLabel;
    GuiSlider7: TGuiSlider;
    GuiSlider8: TGuiSlider;
    GuiLabel7: TGuiLabel;
    GuiLabel8: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure LbBufferClick(Sender: TObject);
    procedure MiAsioClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure SlLevelMp3Change(Sender: TObject);
    procedure SlLevelGetText(Sender: TObject; var Text: string);
    procedure SlLevelSynthChange(Sender: TObject);
    procedure MiAdvancedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbMp3FileNameClick(Sender: TObject);
    procedure SlFreqGetText(Sender: TObject; var Text: string);
    procedure SlOscFreqChange(Sender: TObject);
    procedure SlOscGainChange(Sender: TObject);
  private
    FIniFile             : TFileName;
    FBufferedPlayer      : TBufferedMP3FilePlayer;
    FSynthFactor         : Single;
    FMp3Factor           : Single;
    FSineGenerators      : array [0..4] of TSimpleOscillator32;
    FOutputChannelOffset : Integer;
    FMp3File             : TFileName;
    procedure SetMp3File(const Value: TFileName);
  protected
    FBackgroundBitmap : TGuiCustomPixelMap;
    procedure Mp3FileChanged; virtual;
  public
    property IniFile: TFileName read FIniFile;
    property MP3File: TFileName read FMp3File write SetMp3File;
    property OutputChannelOffset: Integer read FOutputChannelOffset write FOutputChannelOffset;
  end;

var
  FmAASE: TFmAASE;

implementation

{$R *.dfm}

uses
  DAV_Common, DAV_GuiCommon, DAV_GuiBlend, DAV_BlockProcessing, Math, IniFiles,
  AaseSetup;

{ TFmASIOMP3 }

procedure TFmAASE.FormCreate(Sender: TObject);
var
  GeneratorIndex : Integer;
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'AASE.INI';

 // create background bitmap
 FBackgroundBitmap := TGuiPixelMapMemory.Create;

 // create and setup MP3 player
 FBufferedPlayer := TBufferedMP3FilePlayer.Create;
 FBufferedPlayer.Pitch := 1;
 FBufferedPlayer.Interpolation := biBSpline6Point5thOrder;
 with FBufferedPlayer do
  begin
   BufferSize := 65536;
   BlockSize  := 4096
  end;

 FOutputChannelOffset := 0;
 FSynthFactor := 1;
 FMp3Factor := 1;
 ClientHeight := 68;

 for GeneratorIndex := 0 to Length(FSineGenerators) - 1 do
  begin
   FSineGenerators[GeneratorIndex] := TSimpleOscillator32.Create;
   with FSineGenerators[GeneratorIndex] do
    begin
     Frequency := 100 * Power(2, GeneratorIndex);
     Amplitude := 1 / Length(FSineGenerators);
     SampleRate := ASIOHost.SampleRate;
    end;
  end;
end;

procedure TFmAASE.FormDestroy(Sender: TObject);
var
  GeneratorIndex : Integer;
begin
 // stop ASIO
 ASIOHost.Active := False;

 // free background bitmap
 FreeAndNil(FBackgroundBitmap);

 // free buffered MP3 player
 FreeAndNil(FBufferedPlayer);

 // free sine generators
 for GeneratorIndex := 0 to Length(FSineGenerators) - 1
  do FreeAndNil(FSineGenerators[GeneratorIndex]);
end;

procedure TFmAASE.FormShow(Sender: TObject);
begin
 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);

   MP3File := ReadString('Audio', 'MP3 File', MP3File);
  finally
   Free;
  end;
end;

procedure TFmAASE.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TIniFile.Create(FIniFile) do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteString('Audio', 'MP3 File', MP3File);
  finally
   Free;
  end;
end;

procedure TFmAASE.FormPaint(Sender: TObject);
begin
 if Assigned(FBackgroundBitmap)
  then FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFmAASE.FormResize(Sender: TObject);
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  h, hr : Single;
  ScnLn : PPixel32Array;
begin
 if Assigned(FBackgroundBitmap) then
  with FBackgroundBitmap do
   begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    hr   := 1 / Height;
    for y := 0 to Height - 1 do
     begin
      ScnLn := Scanline[y];
      h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * random;
        s[0] := s[1];

        ScnLn[x].B := Round($70 - $34 * (s[1] - h));
        ScnLn[x].G := Round($84 - $48 * (s[1] - h));
        ScnLn[x].R := Round($8D - $50 * (s[1] - h));
       end;
     end;
   end;
end;

procedure TFmAASE.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := 'Stop Audio';
   BtStartStop.ButtonColor := clWhite;
  end
 else
  begin
   ASIOHost.Active := False;
   FBufferedPlayer.Reset;
   BtStartStop.Caption := 'Start Audio';
   BtStartStop.ButtonColor := $0070848D;
  end;
end;

procedure TFmAASE.LbBufferClick(Sender: TObject);
begin
 ASIOHost.SampleRate := 48000;
end;

procedure TFmAASE.LbMp3FileNameClick(Sender: TObject);
begin
 if OpenDialog.Execute
  then MP3File := OpenDialog.FileName;
end;

procedure TFmAASE.MiAdvancedClick(Sender: TObject);
begin
 MiAdvanced.Checked := not MiAdvanced.Checked;

 if MiAdvanced.Checked
  then ClientHeight := 268
  else ClientHeight := 68;
end;

procedure TFmAASE.MiAsioClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmAASE.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAASE.Mp3FileChanged;
begin
 FBufferedPlayer.Filename := FMp3File;
 LbMp3FileName.Caption := ExtractFileName(FMp3File);
end;

procedure TFmAASE.SetMp3File(const Value: TFileName);
begin
 if FMp3File <> Value then
  begin
   FMp3File := Value;
   Mp3FileChanged;
  end;
end;

procedure TFmAASE.SlLevelMp3Change(Sender: TObject);
begin
 FMp3Factor := dB_to_Amp(SlLevelMp3.Value);
end;

procedure TFmAASE.SlLevelGetText(Sender: TObject; var Text: string);
begin
 if Sender is TGuiSlider then
  with TGuiSlider(Sender)
   do Text := FloatToStrF(Value, ffGeneral, 2, 2) + ' dB';
end;

procedure TFmAASE.SlLevelSynthChange(Sender: TObject);
begin
 FSynthFactor := dB_to_Amp(SlLevelSynth.Value);
end;

procedure TFmAASE.SlOscGainChange(Sender: TObject);
begin
 with TGuiSlider(Sender)
  do FSineGenerators[Tag].Amplitude := dB_to_Amp(Value);
end;

procedure TFmAASE.SlOscFreqChange(Sender: TObject);
begin
 with TGuiSlider(Sender)
  do FSineGenerators[Tag].Frequency := Value;
end;

procedure TFmAASE.SlFreqGetText(Sender: TObject; var Text: string);
begin
 if Sender is TGuiSlider then
  with TGuiSlider(Sender) do
   if Value >= 1000
    then Text := FloatToStrF(1E-3 * Value, ffGeneral, 4, 4) + ' kHz'
    else Text := FloatToStrF(Value, ffGeneral, 4, 4) + ' Hz';
end;

procedure TFmAASE.ASIOHostSampleRateChanged(Sender: TObject);
var
  GeneratorIndex : Integer;
begin
 if Assigned(FBufferedPlayer)
  then FBufferedPlayer.SampleRate := ASIOHost.SampleRate;

 for GeneratorIndex := 0 to Length(FSineGenerators) - 1
  do FSineGenerators[GeneratorIndex].SampleRate := ASIOHost.SampleRate;
end;

procedure TFmAASE.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmAASE.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  GeneratorIndex  : Integer;
  SampleIndex     : Integer;
  GeneratorSignal : Single;
begin
 FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);

 for SampleIndex := 0 to ASIOHost.Buffersize - 1 do
  begin
   GeneratorSignal := FSineGenerators[0].Sine;
   FSineGenerators[0].CalculateNextSample;
   for GeneratorIndex := 1 to Length(FSineGenerators) - 1 do
    begin
     GeneratorSignal := GeneratorSignal + FSineGenerators[GeneratorIndex].Sine;
     FSineGenerators[GeneratorIndex].CalculateNextSample;
    end;

   OutBuffer[0, SampleIndex] := FMp3Factor * OutBuffer[0, SampleIndex] +
     FSynthFactor * GeneratorSignal;
   OutBuffer[1, SampleIndex] := FMp3Factor * OutBuffer[1, SampleIndex] +
     FSynthFactor * GeneratorSignal;
  end;

end;

end.
