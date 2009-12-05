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
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, StdCtrls,
  Menus, DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLED, DAV_GuiDial,
  DAV_GuiLevelMeter, DAV_GuiSelectBox, DAV_GuiLabel;

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
    PuOutputValues: TPopupMenu;
    Mi001dB: TMenuItem;
    Mi002dB: TMenuItem;
    Mi0dB: TMenuItem;
    Mi003dB: TMenuItem;
    Mi005dB: TMenuItem;
    Mi01dB: TMenuItem;
    Mi02dB: TMenuItem;
    GuiLabel1: TGuiLabel;
    SbProcessingType: TGuiSelectBox;
    PuInputValues: TPopupMenu;
    MiGain0dB: TMenuItem;
    MiGain1dB: TMenuItem;
    MiGain2dB: TMenuItem;
    MiGain3dB: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialInputDblClick(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure Mi001dBClick(Sender: TObject);
    procedure Mi002dBClick(Sender: TObject);
    procedure Mi003dBClick(Sender: TObject);
    procedure Mi005dBClick(Sender: TObject);
    procedure Mi01dBClick(Sender: TObject);
    procedure Mi02dBClick(Sender: TObject);
    procedure Mi0dBClick(Sender: TObject);
    procedure MiGain0dBClick(Sender: TObject);
    procedure MiGain1dBClick(Sender: TObject);
    procedure MiGain2dBClick(Sender: TObject);
    procedure MiGain3dBClick(Sender: TObject);
    procedure SbProcessingTypeChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure DialOutputDblClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure DialReleaseDblClick(Sender: TObject);
  private
    FEdValue: TEdit;
  public
    procedure UpdateInput;
    procedure UpdateOutput;
    procedure UpdateRelease;
    procedure UpdateProcessingMode;
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

procedure TFmLookaheadLimiter.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmLookaheadLimiter.FormShow(Sender: TObject);
begin
 UpdateInput;
 UpdateOutput;
 UpdateRelease;
 UpdateProcessingMode;
end;

procedure TFmLookaheadLimiter.FormClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmLookaheadLimiter.Mi0dBClick(Sender: TObject);
begin
 DialOutput.Position := 0;
end;

procedure TFmLookaheadLimiter.MiGain0dBClick(Sender: TObject);
begin
 DialInput.Position := 0;
end;

procedure TFmLookaheadLimiter.MiGain1dBClick(Sender: TObject);
begin
 DialInput.Position := 1;
end;

procedure TFmLookaheadLimiter.MiGain2dBClick(Sender: TObject);
begin
 DialInput.Position := 2;
end;

procedure TFmLookaheadLimiter.MiGain3dBClick(Sender: TObject);
begin
 DialInput.Position := 3;
end;

procedure TFmLookaheadLimiter.SbProcessingTypeChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Parameter[2] := SbProcessingType.ItemIndex;
  end;
end;

procedure TFmLookaheadLimiter.Mi001dBClick(Sender: TObject);
begin
 DialOutput.Position := -0.01;
end;

procedure TFmLookaheadLimiter.Mi002dBClick(Sender: TObject);
begin
 DialOutput.Position := -0.02;
end;

procedure TFmLookaheadLimiter.Mi003dBClick(Sender: TObject);
begin
 DialOutput.Position := -0.03;
end;

procedure TFmLookaheadLimiter.Mi005dBClick(Sender: TObject);
begin
 DialOutput.Position := -0.05;
end;

procedure TFmLookaheadLimiter.Mi01dBClick(Sender: TObject);
begin
 DialOutput.Position := -0.1;
end;

procedure TFmLookaheadLimiter.Mi02dBClick(Sender: TObject);
begin
 DialOutput.Position := -0.2;
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
   if Parameter[0] <> DialInput.Position
    then Parameter[0] := DialInput.Position;
  end;
end;

procedure TFmLookaheadLimiter.DialInputDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent      := Self;
   Left        := LbInputValue.Left;
   Top         := LbInputValue.Top;
   Width       := LbInputValue.Width;
   Height      := LbInputValue.Height;
   BorderStyle := bsNone;
   Color       := Self.Color;
   Text        := LbInputValue.Caption;
   Tag         := 0;
   TabOrder    := 0;
   OnKeyPress  := EdValueKeyPress;
   Font.Assign(Self.Font);
   SetFocus;
  end;
end;

procedure TFmLookaheadLimiter.DialOutputChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   if Parameter[1] <> DialOutput.Position
    then Parameter[1] := DialOutput.Position;
  end;
end;

procedure TFmLookaheadLimiter.DialOutputDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent      := Self;
   Left        := LbOutputValue.Left;
   Top         := LbOutputValue.Top;
   Width       := LbOutputValue.Width;
   Height      := LbOutputValue.Height;
   BorderStyle := bsNone;
   Color       := Self.Color;
   Text        := LbOutputValue.Caption;
   Tag         := 1;
   TabOrder    := 0;
   OnKeyPress  := EdValueKeyPress;
   Font.Assign(Self.Font);
   SetFocus;
  end;
end;

procedure TFmLookaheadLimiter.DialReleaseChange(Sender: TObject);
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   if Parameter[3] <> DialRelease.Position
    then Parameter[3] := DialRelease.Position;
  end;
end;

procedure TFmLookaheadLimiter.DialReleaseDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent      := Self;
   Left        := LbReleaseValue.Left;
   Top         := LbReleaseValue.Top;
   Width       := LbReleaseValue.Width;
   Height      := LbReleaseValue.Height;
   BorderStyle := bsNone;
   Color       := Self.Color;
   Text        := LbReleaseValue.Caption;
   Tag         := 3;
   TabOrder    := 0;
   OnKeyPress  := EdValueKeyPress;
   Font.Assign(Self.Font);
   SetFocus;
  end;
end;

procedure TFmLookaheadLimiter.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TLookaheadLimiterDataModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, FEdValue.Text);
    FreeAndNil(FEdValue);
   except
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

procedure TFmLookaheadLimiter.UpdateProcessingMode;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   if SbProcessingType.ItemIndex <> Round(Parameter[2])
    then SbProcessingType.ItemIndex := Round(Parameter[2]);
  end;
end;

procedure TFmLookaheadLimiter.UpdateRelease;
var
  Release : Single;
begin
 with TLookaheadLimiterDataModule(Owner) do
  begin
   Release := Parameter[3];
   if Release <> DialRelease.Position
    then DialRelease.Position := Release;
   LbReleaseValue.Caption := ParameterDisplay[3] + ' ' + ParameterLabel[3];
  end;
end;

end.
