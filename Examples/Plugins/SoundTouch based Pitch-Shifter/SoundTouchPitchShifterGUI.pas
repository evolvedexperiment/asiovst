unit SoundTouchPitchShifterGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, DAV_Types,
  DAV_VSTModule, DAV_GuiCommon, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmSoundTouchPitchShifter = class(TForm)
    DialSemitones: TGuiDial;
    LbSemitones: TGuiLabel;
    LbSemitoneValue: TGuiLabel;
    procedure DialSemitonesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateSemitones;
  end;

implementation

uses
  PngImage, SoundTouchPitchShifterDM, DAV_VSTModuleWithPrograms;

{$R *.DFM}

procedure TFmSoundTouchPitchShifter.DialSemitonesChange(Sender: TObject);
begin
 with TSoundTouchPitchShifterModule(Owner) do
  begin
   if Parameter[0] <> DialSemitones.Position
    then Parameter[0] := DialSemitones.Position
  end;
end;

procedure TFmSoundTouchPitchShifter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  Line   : PRGB24Array;
  PngBmp : TPngObject;

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
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       s[0] := s[1];
       Line[x].B := round($0F + $0E * s[1]);;
       Line[x].G := round($12 + $0E * s[1]);;
       Line[x].R := round($13 + $0E * s[1]);;
      end;
    end;
  end;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'SoundTouchKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSemitones.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSoundTouchPitchShifter.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmSoundTouchPitchShifter.UpdateSemitones;
var
  SemiTones : Integer;
begin
 with TSoundTouchPitchShifterModule(Owner) do
  begin
   if DialSemitones.Position <> Parameter[0]
    then DialSemitones.Position := Parameter[0];
   SemiTones := round(Parameter[0]);
   LbSemitoneValue.Caption := IntToStr(SemiTones) + ' : ' +
     IntToStr(round(100 * (Parameter[0] - SemiTones)));
  end;
end;

end.
