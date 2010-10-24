unit ModDelay2GUI;

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
  Windows, Messages, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
  DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel;

type
  TFmModDelay2 = class(TForm)
    LbCurrentValue: TGuiLabel;
    LbDelay: TGuiLabel;
    LbDepth: TGuiLabel;
    LbFeedback: TGuiLabel;
    LbGain: TGuiLabel;
    LbLeft: TGuiLabel;
    LbLpf: TGuiLabel;
    LbMix: TGuiLabel;
    LbRate: TGuiLabel;
    LbRight: TGuiLabel;
    SBDelayLeft: TScrollBar;
    SBdelayRight: TScrollBar;
    SbDepthLeft: TScrollBar;
    SBDepthRight: TScrollBar;
    SBFeedbackLeft: TScrollBar;
    SBFeedbackRight: TScrollBar;
    SBGainLeft: TScrollBar;
    SBGainRight: TScrollBar;
    SBLPFLeft: TScrollBar;
    SBLpfRight: TScrollBar;
    SBMixLeft: TScrollBar;
    SBMixRight: TScrollBar;
    SBRateLeft: TScrollBar;
    SBRateRight: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  end;

implementation

uses
  DAV_GuiCommon, ModDelay2DM;

{$R *.DFM}

procedure TFmModDelay2.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
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
end;

procedure TFmModDelay2.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

end.