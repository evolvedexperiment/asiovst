unit TwoBandDistortionGUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes,
  SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls, DAV_VSTModule,
  DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiPanel;

type
  TFmTwoBandDistortion = class(TForm)
    DialFreq: TGuiDial;
    DialHighDist: TGuiDial;
    DialLowDist: TGuiDial;
    DialOrder: TGuiDial;
    LbFreq: TGuiLabel;
    LbFreqValue: TGuiLabel;
    LbHighDist: TGuiLabel;
    LbHighDistValue: TGuiLabel;
    LbLowDist: TGuiLabel;
    LbLowDistValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    PnControl: TGuiPanel;
    DIL: TGuiDialImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialLowDistChange(Sender: TObject);
    procedure DialHighDistChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
    FEdValue         : TEdit;
  public
    procedure UpdateFrequency;
    procedure UpdateOrder;
    procedure UpdateLowDistortion;
    procedure UpdateHighDistortion;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_GUICommon,
  {$IFDEF FPC}
  LazPNG,
  {$ELSE}
  PNGImage,
  {$ENDIF}
  DAV_GuiPng,
  TwoBandDistortionDM;

procedure TFmTwoBandDistortion.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB24Array;
  {$IFDEF FPC}
  PngBmp : TPNGImage;
  {$ELSE}
  RS     : TResourceStream;
  {$IFDEF DELPHI2010_UP}
  PngBmp : TPngImage;
  {$ELSE}
  PngBmp : TPngObject;
  {$ENDIF}
//  PngBmp : TPngBitmap;
  {$ENDIF}

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;

 {$IFDEF FPC}
 PngBmp := TPNGImage.Create;
 try
  PngBmp.LoadFromLazarusResource('TwoBandDistortion');
  with DIL.DialImages.Add do
   begin
    DialBitmap.Canvas.Brush.Color := $696969;
    DialBitmap.Assign(PngBmp);
    NumGlyphs := 65;
   end;
  DialFreq.DialImageIndex := 0;
  DialOrder.DialImageIndex := 0;
  DialHighDist.DialImageIndex := 0;
  DialLowDist.DialImageIndex := 0;
 finally
  FreeAndNil(PngBmp);
 end;

 {$ELSE}

 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       b := Round($3F + $1A * s[1]);
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;

 {$IFDEF DELPHI2010_UP}
 PngBmp := TPngImage.Create;
 {$ELSE}
 PngBmp := TPngObject.Create;
 {$ENDIF}

// PngBmp := TPngBitmap.Create;
 try
  RS := TResourceStream.Create(hInstance, 'TwoBandKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with DIL.DialImages.Add do
    begin
     DialBitmap.Canvas.Brush.Color := $696969;
     {$IFDEF DELPHI2010_UP}
     DialBitmap.SetSize(PngBmp.Width, PngBmp.Height);
     PngBmp.Draw(DialBitmap.Canvas, Rect(0, 0, DialBitmap.Width,
       DialBitmap.Height));
     {$ELSE}
     DialBitmap.Assign(PngBmp);
     {$ENDIF}
//     PngBmp.AssignTo(DialBitmap);
//     DialBitmap.Assign(PngBmp);
     GlyphCount := 65;
    end;
   DialFreq.DialImageIndex := 0;
   DialOrder.DialImageIndex := 0;
   DialHighDist.DialImageIndex := 0;
   DialLowDist.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
 {$ENDIF}
end;

procedure TFmTwoBandDistortion.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmTwoBandDistortion.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmTwoBandDistortion.DialFreqChange(Sender: TObject);
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   ParameterByName['Frequency'] := DialFreq.Position;
  end;
end;

procedure TFmTwoBandDistortion.DialHighDistChange(Sender: TObject);
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   ParameterByName['High Distortion'] := DialHighDist.Position;
  end; 
end;

procedure TFmTwoBandDistortion.DialLowDistChange(Sender: TObject);
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   ParameterByName['Low Distortion'] := DialLowDist.Position;
  end;
end;

procedure TFmTwoBandDistortion.DialOrderChange(Sender: TObject);
var
  CurrentOrder : Single;
  DesiredOrder : Integer;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   DesiredOrder := round(DialOrder.Position);
   CurrentOrder := ParameterByName['Order'];
   if round(CurrentOrder) = DesiredOrder then
    if DialOrder.Position < CurrentOrder
     then ParameterByName['Order'] := DesiredOrder - 1 else
    if DialOrder.Position > CurrentOrder
     then ParameterByName['Order'] := DesiredOrder + 1 else
  end;
end;

procedure TFmTwoBandDistortion.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateOrder;
 UpdateLowDistortion;
 UpdateHighDistortion;
end;

procedure TFmTwoBandDistortion.UpdateFrequency;
var
  Freq : Single;
const
  CThousand : Single = 1000;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   Freq := ParameterByName['Frequency'];
   if Freq < CThousand
    then LbFreqValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 4) + ' Hz'
    else LbFreqValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + ' kHz';
   if DialFreq.Position <> Freq
    then DialFreq.Position := Freq;
  end;
end;

procedure TFmTwoBandDistortion.UpdateHighDistortion;
var
  HighDist : Single;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   HighDist := ParameterByName['High Distortion'];
   LbHighDistValue.Caption := FloatToStrF(RoundTo(HighDist, -2), ffGeneral, 3, 1) + '%';
   if DialHighDist.Position <> HighDist
    then DialHighDist.Position := HighDist;
  end;
end;

procedure TFmTwoBandDistortion.UpdateLowDistortion;
var
  LowDist : Single;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   LowDist := ParameterByName['Low Distortion'];
   LbLowDistValue.Caption := FloatToStrF(RoundTo(LowDist, -2), ffGeneral, 3, 1) + '%';
   if DialLowDist.Position <> LowDist
    then DialLowDist.Position := LowDist;
  end;
end;

procedure TFmTwoBandDistortion.UpdateOrder;
var
  Order : Integer;
begin
 with Owner as TTwoBandDistortionDataModule do
  begin
   Order := round(ParameterByName['Order']);
   LbOrderValue.Caption := IntToStr(Order);
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
  end;
end;

{$IFDEF FPC}
initialization
  {$i TwoBandDistortionGUI.lrs}
  {$i TwoBandDistortionPNG.lrs}
{$ENDIF}

end.
