unit ModDelayGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, Graphics, DAV_Types,
  DAV_VSTModule, Controls, DAV_GuiBaseControl, DAV_GuiLabel, StdCtrls;

type
  TFmModDelay = class(TForm)
    LbGain: TGuiLabel;
    LbMix: TGuiLabel;
    LbLpf: TGuiLabel;
    LbDelay: TGuiLabel;
    LbDepth: TGuiLabel;
    LbRate: TGuiLabel;
    LbFeedback: TGuiLabel;
    SbGain: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    ScrollBar4: TScrollBar;
    ScrollBar5: TScrollBar;
    ScrollBar6: TScrollBar;
    ScrollBar7: TScrollBar;
    LbGainValue: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbLpfValue: TGuiLabel;
    LbDelayValue: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbRateValue: TGuiLabel;
    LbFeedbackValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SbGainChange(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateGain;
    procedure UpdateMix;
    procedure UpdateLowpass;
    procedure UpdateDelay;
    procedure UpdateDepth;
    procedure UpdateRate;
    procedure UpdateFeedback;
  end;

implementation

uses
  DAV_GuiCommon, ModDelayDM;

{$R *.DFM}

procedure TFmModDelay.FormCreate(Sender: TObject);
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
end;

procedure TFmModDelay.FormShow(Sender: TObject);
begin
 UpdateGain;
 UpdateMix;
 UpdateLowpass;
 UpdateDelay;
 UpdateDepth;
 UpdateRate;
 UpdateFeedback;
end;

procedure TFmModDelay.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmModDelay.SbGainChange(Sender: TObject);
begin
 with Owner as TModDelayModule do
  begin
//   if Parameter[0] <> SbGain.Position 
//    then Parameter[0] := SbGain.Position;
  end;
end;

procedure TFmModDelay.UpdateDelay;
begin
 with Owner as TModDelayModule do
  begin

  end;
end;

procedure TFmModDelay.UpdateDepth;
begin
 with Owner as TModDelayModule do
  begin

  end;
end;

procedure TFmModDelay.UpdateFeedback;
begin
 with Owner as TModDelayModule do
  begin

  end;
end;

procedure TFmModDelay.UpdateGain;
begin
 with Owner as TModDelayModule do
  begin
//   if SbGain.Position <> Parameter[0] 
//    then SbGain.Position := Parameter[0];
  end;
end;

procedure TFmModDelay.UpdateLowpass;
begin
 with Owner as TModDelayModule do
  begin

  end;
end;

procedure TFmModDelay.UpdateMix;
begin
 with Owner as TModDelayModule do
  begin

  end;
end;

procedure TFmModDelay.UpdateRate;
begin
 with Owner as TModDelayModule do
  begin

  end;
end;

end.
