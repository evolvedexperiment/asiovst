unit CTCGui;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel;

type
  TFmCTC = class(TForm)
    LbAttenuation: TLabel;
    LbAttenuationValue: TLabel;
    LbListenerDistance: TLabel;
    LbListenerDistanceValue: TLabel;
    LbRecursionSteps: TLabel;
    LbRecursionStepsValue: TLabel;
    LbSpeakerDistance: TLabel;
    LbSpeakerDistanceValue: TLabel;
    SbAttenuation: TScrollBar;
    SbListenerDistance: TScrollBar;
    SbRecursionSteps: TScrollBar;
    SbSpeakerDistance: TScrollBar;
    LbFilterType: TLabel;
    LbFilterFrequencyValue: TLabel;
    SbFilterFrequency: TScrollBar;
    Image1: TImage;
    Image2: TImage;
    LbFilterTypeValue: TLabel;
    LbFilterFrequency: TLabel;
    LbFilterGainValue: TLabel;
    SbFilterGain: TScrollBar;
    LbFilterGain: TLabel;
    LbOutputGain: TLabel;
    LbOutputGainValue: TLabel;
    SbOutputGain: TScrollBar;
    LbTitle: TGuiLabel;
    CBBypass: TCheckBox;
    CBAGC: TCheckBox;
    LbSwitches: TLabel;
    procedure FormShow(Sender: TObject);
    procedure SbRecursionStepsChange(Sender: TObject);
    procedure SbAttenuationChange(Sender: TObject);
    procedure SbListenerDistanceChange(Sender: TObject);
    procedure SbSpeakerDistanceChange(Sender: TObject);
    procedure SbOutputGainChange(Sender: TObject);
    procedure SbFilterGainChange(Sender: TObject);
    procedure SbFilterFrequencyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBBypassClick(Sender: TObject);
  private
    FBackground : TBitmap;
  public
    procedure UpdateSpeakerDistance;
    procedure UpdateListenerDistance;
    procedure UpdateRecursionSteps;
    procedure UpdateAttenuation;
    procedure UpdateFilterType;
    procedure UpdateFilterGain;
    procedure UpdateFilterFrequency;
    procedure UpdateOutputGain;
    procedure UpdateBypass;
    procedure UpdateAGC;
  end;

implementation

{$R *.DFM}

uses
  PngImage, CTCDM, DAV_Common, DAV_GuiCommon, DAV_VSTModuleWithPrograms;

procedure TFmCTC.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PBGR24Array;
  PngBmp : TPngObject;
begin
  // Create Background Image
 FBackground := TBitmap.Create;
 with FBackground do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.3 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round($C8 - $3A * (s[1] - h));
       Line[x].G := round($D0 - $3C * (s[1] - h));
       Line[x].R := round($D4 - $40 * (s[1] - h));
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'Left', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   FBackground.Canvas.Draw(8, 8, PngBmp);
  finally
   RS.Free;
  end;

  RS := TResourceStream.Create(hInstance, 'Right', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   FBackground.Canvas.Draw(Width - PngBmp.Width - 8, 8, PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmCTC.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmCTC.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackground);
end;

procedure TFmCTC.FormShow(Sender: TObject);
begin
 UpdateSpeakerDistance;
 UpdateListenerDistance;
 UpdateAttenuation;
 UpdateRecursionSteps;
 UpdateFilterType;
 UpdateFilterGain;
 UpdateFilterFrequency;
 UpdateOutputGain;
 LbTitle.Height := LbTitle.Height + 1;
end;

procedure TFmCTC.SbSpeakerDistanceChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[0] := 0.1 * SbSpeakerDistance.Position;
  end;
end;

procedure TFmCTC.SbListenerDistanceChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[1] := 0.1 * SbListenerDistance.Position;
  end;
end;

procedure TFmCTC.SbOutputGainChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[7] := 0.1 * SbOutputGain.Position;
  end;
end;

procedure TFmCTC.SbRecursionStepsChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[2] := SbRecursionSteps.Position;
  end;
end;

procedure TFmCTC.SbAttenuationChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[3] := 0.1 * SbAttenuation.Position;
  end;
end;

procedure TFmCTC.SbFilterFrequencyChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[5] := FreqLinearToLog(0.0001 * SbFilterFrequency.Position);
  end;
end;

procedure TFmCTC.SbFilterGainChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[6] := 0.1 * SbFilterGain.Position;
  end;
end;

procedure TFmCTC.CBBypassClick(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[8] := Integer(CBBypass.Checked);
  end;
end;

procedure TFmCTC.UpdateSpeakerDistance;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[0]) <> SbSpeakerDistance.Position
    then SbSpeakerDistance.Position := round(10 * Parameter[0]);
   LbSpeakerDistanceValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmCTC.UpdateListenerDistance;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[1]) <> SbListenerDistance.Position
    then SbListenerDistance.Position := round(10 * Parameter[1]);
   LbListenerDistanceValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmCTC.UpdateRecursionSteps;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(Parameter[2]) <> SbRecursionSteps.Position
    then SbRecursionSteps.Position := round(Parameter[2]);
   LbRecursionStepsValue.Caption := IntToStr(round(Parameter[2]));
  end;
end;

procedure TFmCTC.UpdateAGC;
begin

end;

procedure TFmCTC.UpdateAttenuation;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[3]) <> SbAttenuation.Position
    then SbAttenuation.Position := round(10 * Parameter[3]);
   LbAttenuationValue.Caption := ParameterDisplay[3] + ' dB';
  end;
end;

procedure TFmCTC.UpdateBypass;
begin
 with TCTCDataModule(Owner)
  do CBBypass.Checked := Boolean(Round(Parameter[8]));
end;

procedure TFmCTC.UpdateFilterType;
begin
 with TCTCDataModule(Owner) do
  begin
   LbFilterTypeValue.Caption := 'Simple Highshelf';
  end;
end;

procedure TFmCTC.UpdateFilterFrequency;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10000 * FreqLogToLinear(Parameter[5])) <> SbFilterFrequency.Position
    then SbFilterFrequency.Position := round(10000 * FreqLogToLinear(Parameter[5]));
   LbFilterFrequencyValue.Caption := ParameterDisplay[5] + ' ' + ParameterLabel[5];
  end;
end;

procedure TFmCTC.UpdateFilterGain;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[6]) <> SbFilterGain.Position
    then SbFilterGain.Position := round(10 * Parameter[6]);
   LbFilterGainValue.Caption := ParameterDisplay[6] + ' dB';
  end;
end;

procedure TFmCTC.UpdateOutputGain;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[7]) <> SbOutputGain.Position
    then SbOutputGain.Position := round(10 * Parameter[7]);
   LbOutputGainValue.Caption := ParameterDisplay[7] + ' dB';
  end;
end;

end.
