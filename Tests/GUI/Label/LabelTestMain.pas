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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  DAV_GuiCommon, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiPixelMap;

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
    FBackground : TGuiCustomPixelMap;
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
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmLabelTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmLabelTest.FormResize(Sender: TObject);
var
  x, y  : Integer;
  s     : array[0..1] of Single;
  h, hr : Single;
  ScnLn : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       ScnLn[x].B := round($70 - $34 * (s[1] - h));
       ScnLn[x].G := round($84 - $48 * (s[1] - h));
       ScnLn[x].R := round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmLabelTest.FormPaint(Sender: TObject);
begin
 FBackground.PaintTo(Canvas);
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
