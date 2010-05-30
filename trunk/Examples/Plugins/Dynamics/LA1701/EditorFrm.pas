unit EditorFrm;

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
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, StdCtrls,
  ExtCtrls, Menus, DAV_Types, DAV_VSTModule, DAV_GuiButton, DAV_GuiVUMeter,
  DAV_GuiLED, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiPanel;

type
  TLevelState = (lsIn, lsGR, lsOut);
  TFmLA1701 = class(TForm)
    BtGR: TGuiButton;
    BtIn: TGuiButton;
    BtOut: TGuiButton;
    DialAttack: TGuiDial;
    DialInput: TGuiDial;
    DialKnee: TGuiDial;
    DialMix: TGuiDial;
    DialOutput: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    LbAttack: TGuiLabel;
    LbFast: TGuiLabel;
    LbInput: TGuiLabel;
    LbInputValue: TLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TLabel;
    LbLevelingAmplifier: TLabel;
    LbMix: TGuiLabel;
    LbMixValue: TLabel;
    LbOnOff: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TLabel;
    LbRatioValue: TLabel;
    LbRatioX: TGuiLabel;
    LbRelease: TGuiLabel;
    LbSlow: TGuiLabel;
    LbTitle: TGuiLabel;
    LbVUMeterDisplay: TLabel;
    LEDOnOff: TGuiLED;
    MIFast: TMenuItem;
    MIMedium: TMenuItem;
    MISlow: TMenuItem;
    PnA: TGuiPanel;
    PnB: TGuiPanel;
    PnInputValue: TGuiPanel;
    PnKnee: TGuiPanel;
    PnMix: TGuiPanel;
    PnOutputValue: TGuiPanel;
    PnRatio: TGuiPanel;
    PopupVUMeterSpeed: TPopupMenu;
    SpDivide1: TShape;
    SpDivide2: TShape;
    Timer1: TTimer;
    VUMeter: TGuiVUMeter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtGRClick(Sender: TObject);
    procedure BtInClick(Sender: TObject);
    procedure BtOutClick(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure LEDOnOffClick(Sender: TObject);
    procedure MIFastClick(Sender: TObject);
    procedure MIMediumClick(Sender: TObject);
    procedure MISlowClick(Sender: TObject);
    procedure PopupVUMeterSpeedPopup(Sender: TObject);
    procedure VUMeterTimerTimer(Sender: TObject);
    procedure LbTitleClick(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
    procedure SetLevelState(const Value: TLevelState);
    function GetLevelState: TLevelState;
    function VUMeterValueToPos(Value: Double): Integer;
  public
    procedure UpdateOnOff;
    procedure UpdateInput;
    procedure UpdateOutput;
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateRatio;
    procedure UpdateKnee;
    procedure UpdateMix;
    procedure UpdateLevelState;
  published
    property LevelState: TLevelState read GetLevelState write SetLevelState;
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, DAV_GUICommon, LA1701DM;

procedure TFmLA1701.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  ClrBt  : Byte;
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB32Array;
  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
 ClrBt := $F + random($40);
 with FBackgrounBitmap do
  begin
   PixelFormat := pf32bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.9 * s[0] + 0.1 * (2 * random - 1);
       b := round($F * s[1]);
       s[0] := s[1];
       Line[x].B := ClrBt + b;
       Line[x].G := ClrBt + b;
       Line[x].R := ClrBt + b;
       Line[x].A := 0;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'PanKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialInput.DialBitmap.Assign(PngBmp);
   DialOutput.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'RatioKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialRatio.DialBitmap.Assign(PngBmp);
   DialKnee.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'AttackKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialAttack.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'ReleaseKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialRelease.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
  RS := TResourceStream.Create(hInstance, 'VUMeter', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   VUMeter.VUMeterBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmLA1701.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmLA1701.FormPaint(Sender: TObject);
begin
 if assigned(FBackgrounBitmap)
  then Self.Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmLA1701.FormShow(Sender: TObject);
begin
 UpdateOnOff;
 UpdateInput;
 UpdateOutput;
 UpdateAttack;
 UpdateRelease;
 UpdateRatio;
 UpdateKnee;
 UpdateMix;
 UpdateLevelState;
end;

function TFmLA1701.GetLevelState: TLevelState;
begin
 with TLA1701DataModule(Owner)
  do result := TLevelState(round(Parameter[8]));
end;

procedure TFmLA1701.LbTitleClick(Sender: TObject);
var
  ProductNumber : Integer;
begin
 repeat
  repeat
   ProductNumber := random(1000);
  until (ProductNumber div 100 = 0) or
        ((ProductNumber - 100 * (ProductNumber div 100)) div 10 = 0);
  ProductNumber := 1000 * round(Power(2, random(4))) + ProductNumber;
 until ProductNumber <> 2094;
 LbTitle.Caption := 'LA-' + IntToStr(ProductNumber);
end;

procedure TFmLA1701.LEDOnOffClick(Sender: TObject);
begin
 with TLA1701DataModule(Owner) do
  begin
   Parameter[0] := 1 - Parameter[0];
   UpdateOnOff;
  end;
end;

procedure TFmLA1701.MIFastClick(Sender: TObject);
begin
 with TLA1701DataModule(Owner)
  do Parameter[9] := 5;
end;

procedure TFmLA1701.MIMediumClick(Sender: TObject);
begin
 with TLA1701DataModule(Owner)
  do Parameter[9] := 50;
end;

procedure TFmLA1701.MISlowClick(Sender: TObject);
begin
 with TLA1701DataModule(Owner)
  do Parameter[9] := 500;
end;

procedure TFmLA1701.PopupVUMeterSpeedPopup(Sender: TObject);
begin
 with TLA1701DataModule(Owner) do
  begin
   MIFast.Checked   := Parameter[9] < 20;
   MISlow.Checked   := Parameter[9] > 200;
   MIMedium.Checked := not (MIFast.Checked or MISlow.Checked);
  end;
end;

procedure TFmLA1701.SetLevelState(const Value: TLevelState);
begin
 with TLA1701DataModule(Owner) do
  if LevelState <> Value then
   begin
    Parameter[8] := Integer(Value);
    UpdateLevelState;
   end;
end;

procedure TFmLA1701.UpdateOnOff;
begin
 with TLA1701DataModule(Owner) do
  begin
   if Parameter[0] < 0.5
    then LEDOnOff.Brightness_Percent := 100
    else LEDOnOff.Brightness_Percent := 10;
  end;
end;

procedure TFmLA1701.UpdateInput;
begin
 with TLA1701DataModule(Owner) do
  begin
   if DialInput.Position <> Parameter[1]
    then DialInput.Position := Parameter[1];
   LbInputValue.Caption := FloatToStrF(DialInput.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TFmLA1701.UpdateKnee;
begin
 with TLA1701DataModule(Owner) do
  begin
   if DialKnee.Position <> Parameter[6]
    then DialKnee.Position := Parameter[6];
   LbKneeValue.Caption := FloatToStrF(DialKnee.Position, ffFixed, 3, 1);
  end;
end;

procedure TFmLA1701.UpdateMix;
begin
 with TLA1701DataModule(Owner) do
  begin
   if DialMix.Position <> Parameter[7]
    then DialMix.Position := Parameter[7];
   LbMixValue.Caption := FloatToStrF(DialMix.Position, ffFixed, 3, 1) + '%';
  end;
end;

procedure TFmLA1701.UpdateLevelState;
begin
 case LevelState of
   lsIn : begin
           BtIn.ButtonColor  := $00202020;
           BtIn.Font.Color   := $00E2E2E2;
           BtIn.LineColor    := clSilver;
           BtGR.ButtonColor  := $00000000;
           BtGR.Font.Color   := clGray;
           BtGR.LineColor    := $00333333;
           BtOut.ButtonColor := $00000000;
           BtOut.Font.Color  := clGray;
           BtOut.LineColor   := $00333333;
           LbVUMeterDisplay.Caption := 'Input';
          end;
   lsGR : begin
           BtIn.ButtonColor  := $00000000;
           BtIn.Font.Color   := clGray;
           BtIn.LineColor    := $00333333;
           BtGR.ButtonColor  := $00202020;
           BtGR.Font.Color   := $00E2E2E2;
           BtGR.LineColor    := clSilver;
           BtOut.ButtonColor := $00000000;
           BtOut.Font.Color  := clGray;
           BtOut.LineColor   := $00333333;
           LbVUMeterDisplay.Caption := 'Gain Reduction';
          end;
  lsOut : begin
           BtIn.ButtonColor  := $00000000;
           BtIn.Font.Color   := clGray;
           BtIn.LineColor    := $00333333;
           BtGR.ButtonColor  := $00000000;
           BtGR.Font.Color   := clGray;
           BtGR.LineColor    := $00333333;
           BtOut.ButtonColor := $00202020;
           BtOut.Font.Color  := $00E2E2E2;
           BtOut.LineColor   := clSilver;
           LbVUMeterDisplay.Caption := 'Output';
          end;
 end;
end;

procedure TFmLA1701.UpdateOutput;
begin
 with TLA1701DataModule(Owner) do
  begin
   if DialOutput.Position <> Parameter[2]
    then DialOutput.Position := Parameter[2];
   LbOutputValue.Caption := FloatToStrF(DialOutput.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TFmLA1701.UpdateAttack;
var
  s : Single;
begin
 with TLA1701DataModule(Owner) do
  begin
   s := Log10(Parameter[3]);
   if DialAttack.Position <> s
    then DialAttack.Position := s;
//   LbAttackValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TFmLA1701.UpdateRelease;
var
  s : Single;
begin
 with TLA1701DataModule(Owner) do
  begin
   s := Log10(Parameter[4]);
   if DialRelease.Position <> s
    then DialRelease.Position := s;
//   LbReleaseValue.Caption := FloatToStrF(Parameter[4], ffGeneral, 4, 5) + ' ms';
  end;
end;

function TFmLA1701.VUMeterValueToPos(Value: Double): Integer;
begin
 // ToDo: Create a true mapping
 if Value < -40 then result := 0 else
 if Value >   3 then result := VUMeter.NumGlyphs - 1 else
 if Value < -10 then result := round(((40 + Value) / 30) * 22) else
 if Value <  -7 then result := round(22 + (10 + Value) * 1.66) else
 if Value <  -5 then result := round(28 + (7 + Value) * 2.5) else
 if Value <  -3 then result := round(33 + (5 + Value) * 2.5) else
 if Value <  -1 then result := round(38 + (3 + Value) * 2.5) else
 if Value <   0 then result := round(43 + (1 + Value) * 5)
  else result := round(48 + Value / 3 * (VUMeter.NumGlyphs - 49));
end;

procedure TFmLA1701.VUMeterTimerTimer(Sender: TObject);
begin
 with TLA1701DataModule(Owner) do
  case LevelState of
    lsIn : VUMeter.GlyphIndex := VUMeterValueToPos(InLevel_dB);
    lsGR : VUMeter.GlyphIndex := VUMeterValueToPos(GRReduction_dB);
   lsOut : VUMeter.GlyphIndex := VUMeterValueToPos(OutLevel_dB);
  end;
end;

procedure TFmLA1701.UpdateRatio;
var
  s : Single;
begin
 with TLA1701DataModule(Owner) do
  begin
   s := Log10(Parameter[5]);
   if DialRatio.Position <> s
    then DialRatio.Position := s;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[5], ffFixed, 3, 1);
  end;
end;

procedure TFmLA1701.DialInputChange(Sender: TObject);
begin
 with TLA1701DataModule(Owner) do
  begin
   if Parameter[1] <> DialInput.Position
    then Parameter[1] := DialInput.Position;
   UpdateInput;
  end;
end;

procedure TFmLA1701.DialOutputChange(Sender: TObject);
begin
 with TLA1701DataModule(Owner) do
  begin
   if Parameter[2] <> DialOutput.Position
    then Parameter[2] := DialOutput.Position;
   UpdateOutput;
  end;
end;

procedure TFmLA1701.BtGRClick(Sender: TObject);
begin
 LevelState := lsGR;
end;

procedure TFmLA1701.BtInClick(Sender: TObject);
begin
 LevelState := lsIn;
end;

procedure TFmLA1701.BtOutClick(Sender: TObject);
begin
 LevelState := lsOut;
end;

procedure TFmLA1701.DialAttackChange(Sender: TObject);
var
  s : Single;
begin
 with TLA1701DataModule(Owner) do
  begin
   s := Power(10, DialAttack.Position);
   Parameter[3] := s;
  end;
end;

procedure TFmLA1701.DialReleaseChange(Sender: TObject);
var
  s : Single;
begin
 with TLA1701DataModule(Owner) do
  begin
   s := Power(10, DialRelease.Position);
   Parameter[4] := s;
  end;
end;

procedure TFmLA1701.DialRatioChange(Sender: TObject);
var
  s : Single;
begin
 with TLA1701DataModule(Owner) do
  begin
   s := Power(10, DialRatio.Position);
   if abs (Parameter[5] - s) > 1E-3
    then Parameter[5] := s;
   UpdateRatio;
  end;
end;

procedure TFmLA1701.DialKneeChange(Sender: TObject);
begin
 with TLA1701DataModule(Owner) do
  begin
   Parameter[6] := DialKnee.Position;
   UpdateKnee;
  end;
end;

procedure TFmLA1701.DialMixChange(Sender: TObject);
begin
 with TLA1701DataModule(Owner) do
  begin
   Parameter[7] := DialMix.Position;
   UpdateMix;
  end;
end;

end.
