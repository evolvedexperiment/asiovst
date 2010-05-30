unit LabelTestMain;

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
  {$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, LResources, DAV_GuiLabel;

type
  TFmLabelTest = class(TForm)
    LabelA: TGuiLabel;
    LabelC: TGuiLabel;
    LabelB: TGuiLabel;
    LabelD: TGuiLabel;
    CbTransparent: TCheckBox;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    { Public-Deklarationen }
  end;

var
  FmLabelTest: TFmLabelTest;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmLabelTest.FormCreate(Sender: TObject);
begin
 FBackgrounBitmap := TBitmap.Create;
end;

procedure TFmLabelTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmLabelTest.FormResize(Sender: TObject);
{$IFDEF FPC}
begin

end;
{$ELSE}
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
begin
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
end;
{$ENDIF}

procedure TFmLabelTest.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmLabelTest.CbTransparentClick(Sender: TObject);
begin
 LabelA.Transparent := CbTransparent.Checked;
 LabelB.Transparent := CbTransparent.Checked;
 LabelC.Transparent := CbTransparent.Checked;
 LabelD.Transparent := CbTransparent.Checked;
end;

{$IFDEF FPC}
initialization
  {$i LabelTestMain.lrs}
{$ENDIF}

end.
