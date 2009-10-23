unit SimpleSonogramGui;

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

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, Graphics,
  DAV_Types, DAV_VSTModule, StdCtrls, Menus;

type
  TFmSonogram = class(TForm)
    Timer: TTimer;
    LbFftOrder: TLabel;
    PuFftOrder: TPopupMenu;
    MiOrder6: TMenuItem;
    MiOrder7: TMenuItem;
    MiOrder8: TMenuItem;
    MiOrder9: TMenuItem;
    MiOrder10: TMenuItem;
    MiOrder11: TMenuItem;
    MiOrder12: TMenuItem;
    MiOrder13: TMenuItem;
    MiOrder14: TMenuItem;
    LbOverlapFactor: TLabel;
    PuOverlapFactor: TPopupMenu;
    MiOverlapOrder1: TMenuItem;
    MiOverlapOrder2: TMenuItem;
    MiOverlapOrder3: TMenuItem;
    MiOverlapOrder4: TMenuItem;
    MiOverlapOrder5: TMenuItem;
    MiOverlapOrder6: TMenuItem;
    MiOverlapOrder7: TMenuItem;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MiOrderClick(Sender: TObject);
    procedure MiOverlapOrderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  end;

implementation

uses
  Math, DAV_GuiCommon, SimpleSonogramDM;

{$R *.DFM}

procedure TFmSonogram.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
 with FBackgrounBitmap do
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
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round($70 - $34 * (s[1] - h));
       Line[x].G := round($84 - $48 * (s[1] - h));
       Line[x].R := round($8D - $50 * (s[1] - h));
      end;
    end;
  end;

 ControlStyle := ControlStyle + [csOpaque];
// Sonogram.ControlStyle := Sonogram.ControlStyle + [csOpaque];
end;

procedure TFmSonogram.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmSonogram.TimerTimer(Sender: TObject);
begin
 Invalidate;
end;

procedure TFmSonogram.FormPaint(Sender: TObject);
begin
 with FBackgrounBitmap.Canvas do
  begin
   Draw(8, 8, TSonogramDataModule(Owner).Sonogram.Bitmap);
   Pen.Color := $0070848D;
   Pen.Width := 1;
   Brush.Style := bsClear;
   RoundRect(7, 7, 265, 265, 2, 2);
  end;
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmSonogram.FormShow(Sender: TObject);
var
  MenuItem : TMenuItem;
begin
 MenuItem := nil;
 with TSonogramDataModule(Owner) do
  begin
   case Round(Parameter[0]) of
     6 : MenuItem := MiOrder6;
     7 : MenuItem := MiOrder7;
     8 : MenuItem := MiOrder8;
     9 : MenuItem := MiOrder9;
    10 : MenuItem := MiOrder10;
    11 : MenuItem := MiOrder11;
   end;
  end;

 if Assigned(MenuItem) then
  with MenuItem do
   begin
    Checked := True;
    LbFftOrder.Caption := 'FFT Order: ' + IntToStr(Tag);
   end;

 with TSonogramDataModule(Owner) do
  begin
   case Round(Log2(Parameter[1])) of
     1 : MenuItem := MiOverlapOrder1;
     2 : MenuItem := MiOverlapOrder2;
     3 : MenuItem := MiOverlapOrder3;
     4 : MenuItem := MiOverlapOrder4;
     5 : MenuItem := MiOverlapOrder5;
     6 : MenuItem := MiOverlapOrder6;
   end;
  end;

 if Assigned(MenuItem) then
  with MenuItem do
   begin
    Checked := True;
    LbOverlapFactor.Caption := 'Overlap Order: ' + IntToStr(Tag);
   end;
end;

procedure TFmSonogram.MiOrderClick(Sender: TObject);
begin
 with TMenuItem(Sender) do
  begin
   TSonogramDataModule(Self.Owner).Parameter[0] := Tag;
   Checked := True;
   LbFftOrder.Caption := 'FFT Order: ' + IntToStr(Tag);
  end;
end;

procedure TFmSonogram.MiOverlapOrderClick(Sender: TObject);
begin
 with TMenuItem(Sender) do
  begin
   TSonogramDataModule(Self.Owner).Parameter[1] := 1 shl Tag;
   Checked := True;
   LbOverlapFactor.Caption := 'Overlap Order: ' + IntToStr(Tag);
  end;
end;

end.
