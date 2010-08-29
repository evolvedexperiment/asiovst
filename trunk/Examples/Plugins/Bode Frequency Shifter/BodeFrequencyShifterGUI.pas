unit BodeFrequencyShifterGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, StdCtrls,
  Graphics, DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiLabel, DAV_GuiPanel, DAV_GuiGroup;

type
  TFmBodeFrequencyShifter = class(TForm)
    DialFrequency: TGuiDial;
    DialMix: TGuiDial;
    GpFrequency: TGuiGroup;
    GpMix: TGuiGroup;
    LbFrequencyValue: TGuiLabel;
    LbMixValue: TGuiLabel;
    PnDisplay: TGuiPanel;
    PnMix: TGuiPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateFrequency;
    procedure UpdateMix;
  end;

implementation

{$R *.DFM}

uses
  {$IFDEF FPC} LazPNG, {$ELSE} PNGImage, {$ENDIF}
  DAV_GuiCommon, DAV_VSTModuleWithPrograms, BodeFrequencyShifterDM;

{ TFmBodeFrequencyShifter }

procedure TFmBodeFrequencyShifter.FormCreate(Sender: TObject);
var
  {$IFDEF FPC}
  PngBmp : TPNGImage;
  {$ELSE}
  RS     : TResourceStream;
  {$IFDEF DELPHI2010_UP}
  PngBmp : TPngImage;
  {$ELSE}
  PngBmp : TPngObject;
  {$ENDIF}
  {$ENDIF}
begin
 // Create BackgRound Image
 FBackgrounBitmap := TBitmap.Create;
 FBackgrounBitmap.PixelFormat := pf24bit;

 {$IFDEF FPC}
 PngBmp := TPNGImage.Create;
 try
  PngBmp.LoadFromLazarusResource('TwoBandDistortion');

  // yet todo!

 finally
  FreeAndNil(PngBmp);
 end;
 {$ELSE}
 {$IFDEF DELPHI2010_UP}
 PngBmp := TPngImage.Create;
 {$ELSE}
 PngBmp := TPngObject.Create;
 {$ENDIF}
 try
  RS := TResourceStream.Create(hInstance, 'BodeFrequencyShifter', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   {$IFDEF DELPHI2010_UP}
   DialFrequency.DialBitmap.SetSize(PngBmp.Width, PngBmp.Height);
   PngBmp.DrawUsingPixelInformation(DialFrequency.DialBitmap.Canvas, Point(0, 0));
   DialMix.DialBitmap.SetSize(PngBmp.Width, PngBmp.Height);
   PngBmp.DrawUsingPixelInformation(DialMix.DialBitmap.Canvas, Point(0, 0));
   {$ELSE}
   DialFrequency.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
   {$ENDIF}
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
 {$ENDIF}
end;

procedure TFmBodeFrequencyShifter.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PBGR24Array;
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

       Line[x].B := Round($70 - $34 * (s[1] - h));
       Line[x].G := Round($84 - $48 * (s[1] - h));
       Line[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmBodeFrequencyShifter.FormShow(Sender: TObject);
begin
 with TBodeFrequencyShifterDataModule(Owner) do
  begin
   DialFrequency.Max := ParameterProperties[0].Max;
   DialFrequency.Min := ParameterProperties[0].Min;
  end;

 UpdateFrequency;
 UpdateMix;
end;

procedure TFmBodeFrequencyShifter.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmBodeFrequencyShifter.DialFrequencyChange(Sender: TObject);
begin
 with TBodeFrequencyShifterDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmBodeFrequencyShifter.DialMixChange(Sender: TObject);
begin
 with TBodeFrequencyShifterDataModule(Owner) do
  begin
   if Parameter[1] <> DialMix.Position
    then Parameter[1] := DialMix.Position;
  end;
end;

procedure TFmBodeFrequencyShifter.UpdateFrequency;
begin
 with TBodeFrequencyShifterDataModule(Owner) do
  begin
   if DialFrequency.Position <> Parameter[0]
    then DialFrequency.Position := Parameter[0];
   LbFrequencyValue.Caption := ParameterDisplay[0] + 'Hz';
  end;
end;

procedure TFmBodeFrequencyShifter.UpdateMix;
begin
 with TBodeFrequencyShifterDataModule(Owner) do
  begin
   if DialMix.Position <> Parameter[1]
    then DialMix.Position := Parameter[1];
   LbMixValue.Caption := ParameterDisplay[1] + '%';
  end;
end;

end.
