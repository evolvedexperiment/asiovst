unit BaxxpanderGui;

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

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, Graphics, DAV_GuiBaseControl, DAV_GuiDial, ExtCtrls, DAV_GuiPanel,
  DAV_GuiLabel, DAV_GuiLED;

type
  TFmBaxxpanderGui = class(TForm)
    PnControls: TGuiPanel;
    DlDryWet: TGuiDial;
    LbDryWet: TGuiLabel;
    DlMixer: TGuiDial;
    LbMixer: TGuiLabel;
    DlShape: TGuiDial;
    DlLimit: TGuiDial;
    LbShape: TGuiLabel;
    LbLimit: TGuiLabel;
    LbSaturation: TGuiLabel;
    LEDSaturation: TGuiLED;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgroundBitmap : TBitmap;
  end;

implementation

uses
  DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmBaxxpanderGui.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array [0..3] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
//  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgroundBitmap := TBitmap.Create;
 with FBackgroundBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   s[2] := 0.5;
   s[3] := 0.5;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.2 * (1 - Sqr(2 * (y - Height div 2) * hr)) + 0.3 * s[3];
     if Random < 0.2
      then s[2] := Random;
     s[3] := 0.5 * (s[3] + s[2]);

     for x := 0 to Width - 1 do
      begin
       s[1] := 0.98 * s[0] + 0.02 * Random;
       s[0] := s[1];

       Line[x].B := Round($52 - $36 * (s[1] - h));
       Line[x].G := Round($94 - $62 * (s[1] - h));
       Line[x].R := Round($CF - $8A * (s[1] - h));
      end;
    end;
  end;

(*
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ClipperKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialInputGain.DialBitmap.Assign(PngBmp);
   DialOutputGain.DialBitmap.Assign(PngBmp);
   DialOSFactor1.DialBitmap.Assign(PngBmp);
   DialOSFactor2.DialBitmap.Assign(PngBmp);
   DialFilterOrder1.DialBitmap.Assign(PngBmp);
   DialFilterOrder2.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
*)
end;

procedure TFmBaxxpanderGui.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgroundBitmap);
end;

procedure TFmBaxxpanderGui.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgroundBitmap);
end;

end.
