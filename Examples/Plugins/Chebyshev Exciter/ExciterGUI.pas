unit ExciterGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, ExtCtrls,
  DAV_Types, DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiPanel;

type
  TFmExciter = class(TForm)
    DialMix: TGuiDial;
    DialOrder: TGuiDial;
    DialShape: TGuiDial;
    DialTune: TGuiDial;
    LbFreq: TGuiLabel;
    LbFreqValue: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbShape: TGuiLabel;
    LbShapeValue: TGuiLabel;
    PnControl: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialTuneChange(Sender: TObject);
    procedure DialShapeChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
  private
    FBackgroundBitmap : TBitmap;
  public
    procedure UpdateTune;
    procedure UpdateOrder;
    procedure UpdateShape;
    procedure UpdateMix;
  end;

implementation

{$R *.DFM}

uses
  PNGImage, DAV_GUICommon, ExciterDM;

procedure TFmExciter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  Line   : PRGB24Array;
  h, hr  : Single;
  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgroundBitmap := TBitmap.Create;
 with FBackgroundBitmap do
  begin
   PixelFormat := pf24bit;
   Width       := Self.Width;
   Height      := Self.Height;
   hr          := 1 / Height;
   s[0]        := 0;
   s[1]        := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.6 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       b := round($3F + $1A * (h + s[1]));
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ExciterKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   PngBmp.AssignTo(DialTune.DialBitmap);
   PngBmp.AssignTo(DialOrder.DialBitmap);
   DialShape.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmExciter.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgroundBitmap);
end;

procedure TFmExciter.DialTuneChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Tune'] := DialTune.Position;
  end;
end;

procedure TFmExciter.DialMixChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Mix'] := DialMix.Position;
  end; 
end;

procedure TFmExciter.DialShapeChange(Sender: TObject);
begin
 with Owner as TExciterDataModule do
  begin
   ParameterByName['Shape'] := DialShape.Position;
  end;
end;

procedure TFmExciter.DialOrderChange(Sender: TObject);
var
  CurrentOrder : Single;
  DesiredOrder : Integer;
begin
 with Owner as TExciterDataModule do
  begin
   DesiredOrder := round(DialOrder.Position);
   CurrentOrder := ParameterByName['Order'];
   if round(CurrentOrder) = DesiredOrder then
    if DialOrder.Position < CurrentOrder
     then ParameterByName['Order'] := DesiredOrder - 1 else
    if DialOrder.Position > CurrentOrder
     then ParameterByName['Order'] := DesiredOrder + 1 else
   else ParameterByName['Order'] := DesiredOrder;
  end;
end;

procedure TFmExciter.FormShow(Sender: TObject);
begin
 UpdateTune;
 UpdateOrder;
 UpdateShape;
 UpdateMix;
end;

procedure TFmExciter.UpdateTune;
var
  Freq : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Freq := ParameterByName['Tune'];
   if Freq < 1000
    then LbFreqValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + 'Hz'
    else LbFreqValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + 'kHz';
   if DialTune.Position <> Freq
    then DialTune.Position := Freq;
  end;
end;

procedure TFmExciter.UpdateMix;
var
  Mix : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Mix := ParameterByName['Mix'];
   LbMixValue.Caption := FloatToStrF(Mix, ffGeneral, 3, 1) + '%';
   if DialMix.Position <> Mix
    then DialMix.Position := Mix;
  end;
end;

procedure TFmExciter.UpdateShape;
var
  Shape : Single;
begin
 with Owner as TExciterDataModule do
  begin
   Shape := ParameterByName['Shape'];
   LbShapeValue.Caption := FloatToStrF(Shape, ffGeneral, 3, 1) + '%';
   if DialShape.Position <> Shape
    then DialShape.Position := Shape;
  end;
end;

procedure TFmExciter.UpdateOrder;
var
  Order : Integer;
begin
 with Owner as TExciterDataModule do
  begin
   Order := round(ParameterByName['Order']);
   LbOrderValue.Caption := IntToStr(Order);
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
  end;
end;

end.