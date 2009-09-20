unit AmpSimGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiSelectBox,
  DAV_GuiLED, DAV_GuiPanel;

type
  TFmCombo = class(TForm)
    DialBias: TGuiDial;
    DialDrive: TGuiDial;
    DialFrequency: TGuiDial;
    DialOutput: TGuiDial;
    DialResonance: TGuiDial;
    DIL: TGuiDialImageList;
    GuiLED: TGuiLED;
    GuiPanel1: TGuiPanel;
    LbBias: TGuiLabel;
    LbBiasValue: TLabel;
    LbDrive: TGuiLabel;
    LbDriveValue: TLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TLabel;
    LbModel: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TLabel;
    LbResonance: TGuiLabel;
    LbResonanceValue: TLabel;
    LbStereo: TGuiLabel;
    SBModel: TGuiSelectBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialDriveChange(Sender: TObject);
    procedure DialBiasChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialResoChange(Sender: TObject);
    procedure SBModelChange(Sender: TObject);
    procedure LbStereoClick(Sender: TObject);
  private
    FBackground : TBitmap;
  public
    procedure UpdateBias;
    procedure UpdateDrive;
    procedure UpdateFreq;
    procedure UpdateModel;
    procedure UpdateNoise;
    procedure UpdateOutput;
    procedure UpdateProcess;
    procedure UpdateReso;
  end;

var
  FmCombo: TFmCombo;

implementation

{$R *.dfm}

uses
  DAV_GuiCommon, AmpSimDM;

procedure TFmCombo.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;

begin
 RS := TResourceStream.Create(hInstance, 'AmpKnob', 'BMP');
 try
  with DIL.DialImages.Add do
   begin
    NumGlyphs := 65;
    DialBitmap.LoadFromStream(RS);
   end;
  DialDrive.DialImageIndex := 0;
  DialBias.DialImageIndex := 0;
  DialOutput.DialImageIndex := 0;
  DialFrequency.DialImageIndex := 0;
  DialResonance.DialImageIndex := 0;
 finally
  RS.Free;
 end;

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
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.06 * random - 0.03;
       s[0] := s[1];

       Line[x].B := round($40 - $10 * (s[1] - h));
       Line[x].G := round($80 - $20 * (s[1] - h));
       Line[x].R := round($80 - $20 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmCombo.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackground);
end;

procedure TFmCombo.SBModelChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[0] <> SBModel.ItemIndex
    then Parameter[0] := SBModel.ItemIndex;
  end;
end;

procedure TFmCombo.UpdateDrive;
var
  Drive : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Drive := ParameterByName['Drive'];
   if DialDrive.Position <> Drive
    then DialDrive.Position := Drive;
   LbDriveValue.Caption := FloatToStrF(Drive, ffGeneral, 3, 3) + '%';
  end;
end;

procedure TFmCombo.UpdateBias;
var
  Bias : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Bias := ParameterByName['Bias'];
   if DialBias.Position <> Bias
    then DialBias.Position := Bias;
   LbBiasValue.Caption := FloatToStrF(Bias, ffGeneral, 3, 3) + '%';
  end;
end;

procedure TFmCombo.UpdateFreq;
var
  Frequency : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Frequency := ParameterByName['HPF Frequency'];
   if DialFrequency.Position <> Frequency
    then DialFrequency.Position := Frequency;
   LbFrequencyValue.Caption := FloatToStrF(Frequency, ffGeneral, 5, 5) + 'Hz';
  end;
end;

procedure TFmCombo.UpdateProcess;
begin
 with TComboDataModule(Owner) do
  begin
   if Stereo then
    begin
     LbStereo.Caption := 'Stereo';
     GuiLED.Brightness_Percent := 100;
    end
   else
    begin
     LbStereo.Caption := 'Mono';
     GuiLED.Brightness_Percent := 20;
    end;
  end;
end;

procedure TFmCombo.UpdateModel;
var
  Model : Integer;
begin
 with TComboDataModule(Owner) do
  begin
   Model := Round(ParameterByName['Model']);
   if SBModel.ItemIndex <> Model
    then SBModel.ItemIndex := Model;
  end;
end;

procedure TFmCombo.UpdateNoise;
begin

end;

procedure TFmCombo.UpdateOutput;
var
  Output : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Output := ParameterByName['Output'];
   if DialOutput.Position <> Output
    then DialOutput.Position := Output;
   LbOutputValue.Caption := FloatToStrF(Output, ffGeneral, 3, 3) + 'dB';
  end;
end;

procedure TFmCombo.UpdateReso;
var
  Reso : Single;
begin
 with TComboDataModule(Owner) do
  begin
   Reso := ParameterByName['HPF Resonance'];
   if DialResonance.Position <> Reso
    then DialResonance.Position := Reso;
   LbResonanceValue.Caption := FloatToStrF(Reso, ffGeneral, 3, 3) + '%';
  end;
end;

procedure TFmCombo.DialBiasChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[2] <> DialBias.Position
    then Parameter[2] := DialBias.Position;
  end;
end;

procedure TFmCombo.DialDriveChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[1] <> DialDrive.Position
    then Parameter[1] := DialDrive.Position;
  end;
end;

procedure TFmCombo.DialOutputChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[3] <> DialOutput.Position
    then Parameter[3] := DialOutput.Position;
  end;
end;

procedure TFmCombo.DialFreqChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[5] <> DialFrequency.Position
    then Parameter[5] := DialFrequency.Position;
  end;
end;

procedure TFmCombo.DialResoChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   if Parameter[6] <> DialResonance.Position
    then Parameter[6] := DialResonance.Position;
  end;
end;

procedure TFmCombo.FormShow(Sender: TObject);
begin
 UpdateModel;
 UpdateDrive;
 UpdateBias;
 UpdateOutput;
 UpdateFreq;
 UpdateReso;
 UpdateProcess;
 UpdateNoise;
end;

procedure TFmCombo.LbStereoClick(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   Parameter[4] := 1 - Parameter[4];
  end;
end;

end.
