unit LookaheadLimiterGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, DAV_Types,
  DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGraphXY,
  DAV_GuiLED, DAV_GuiLevelMeter;

type
  TFmLookaheadLimiter = class(TForm)
    DialOutput: TGuiDial;
    DialRelease: TGuiDial;
    DialInput: TGuiDial;
    GuiDialImageList: TGuiDialImageList;
    Lb0dB: TGuiLabel;
    Lb10dB: TGuiLabel;
    Lb30dB: TGuiLabel;
    LbGR: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbInput: TGuiLabel;
    LbInputValue: TGuiLabel;
    LMGainReduction: TGuiColorLevelMeter;
    Timer: TTimer;
    Lb20dB: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  public
    procedure UpdateInput;
    procedure UpdateOutput;
    procedure UpdateRelease;
  end;

implementation

uses
  PngImage, DAV_Approximations, DAV_VSTModuleWithPrograms, LookaheadLimiterDM;

{$R *.DFM}

procedure TFmLookaheadLimiter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'LimiterKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with GuiDialImageList[0].DialBitmap do
    begin
     Canvas.FillRect(Canvas.ClipRect);
     Assign(PngBmp);
    end;
   DialInput.DialImageIndex := 0;
   DialOutput.DialImageIndex := 0;
   DialRelease.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmLookaheadLimiter.FormShow(Sender: TObject);
begin
 UpdateInput;
 UpdateOutput;
 UpdateRelease;
end;

procedure TFmLookaheadLimiter.TimerTimer(Sender: TObject);
var
  GR : Single;
begin
 with TLookaheadLimiterDataModule(Owner), LMGainReduction do
  begin
   GR := FastAmptodBMinError3(Limiter.GainReductionFactor);
   if GR > PeakLevel
    then PeakLevel := 0.2 * PeakLevel + 0.8 * GR
    else PeakLevel := 0.8 * PeakLevel + 0.2 * GR;
  end;
end;

procedure TFmLookaheadLimiter.DialInputChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Parameter[0] := DialInput.Position;
  end;
end;

procedure TFmLookaheadLimiter.DialOutputChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Parameter[1] := DialOutput.Position;
  end;
end;

procedure TFmLookaheadLimiter.DialReleaseChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Parameter[2] := DialRelease.Position;
  end;
end;

procedure TFmLookaheadLimiter.UpdateInput;
var
  Input : Single;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Input := Parameter[0];
   if Input <> DialInput.Position
    then DialInput.Position := Input;
   LbInputValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLookaheadLimiter.UpdateOutput;
var
  Output : Single;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Output := Parameter[1];
   if Output <> DialOutput.Position
    then DialOutput.Position := Output;
   LbOutputValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmLookaheadLimiter.UpdateRelease;
var
  Release : Single;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Release := Parameter[2];
   if Release <> DialRelease.Position
    then DialRelease.Position := Release;
   LbReleaseValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
  end;
end;

end.
