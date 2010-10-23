unit LinkwitzRileySeparatedGui;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, DAV_GuiPanel, DAV_GuiLabel, DAV_GuiBaseControl,
  DAV_GuiDial, DAV_GuiGroup, DAV_GuiEQGraph;

type
  TFmLinkwitzRiley = class(TForm)
    DialFrequency: TGuiDial;
    DialSlope: TGuiDial;
    DialType: TGuiDial;
    GbFrequencyResponse: TGuiGroup;
    GpLinkwitzRiley: TGuiGroup;
    GuiEQGraph: TGuiEQGraph;
    LbDisplay: TGuiLabel;
    LbIFrequency: TGuiLabel;
    LbSlope: TGuiLabel;
    LbType: TGuiLabel;
    PnDisplay: TGuiPanel;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DialTypeChange(Sender: TObject);
    procedure DialSlopeChange(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GuiEQGraphGetFilterGain(Sender: TObject;
      const Frequency: Single): Single;
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateFrequency;
    procedure UpdateSlope;
    procedure UpdateType;
  end;

implementation

{$R *.dfm}

uses
  DAV_GuiCommon, PNGImage, LinkwitzRileySeparatedDM, DAV_VSTModuleWithPrograms;

procedure TFmLinkwitzRiley.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
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

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ClipperKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialFrequency.DialBitmap.Assign(PngBmp);
   DialSlope.DialBitmap.Assign(PngBmp);
   DialType.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmLinkwitzRiley.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmLinkwitzRiley.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateSlope;
 UpdateType;
 LbDisplay.Caption := 'Linkwitz-Riley';
end;

function TFmLinkwitzRiley.GuiEQGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   Result := -5;
  end;
end;

procedure TFmLinkwitzRiley.DialFrequencyChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialSlopeChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[1] <> DialSlope.Position
    then Parameter[1] := DialSlope.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialTypeChange(Sender: TObject);
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if Parameter[2] <> DialType.Position
    then Parameter[2] := DialType.Position;
  end;
end;

procedure TFmLinkwitzRiley.UpdateFrequency;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialFrequency.Position <> Parameter[0]
    then DialFrequency.Position := Parameter[0];
   LbDisplay.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmLinkwitzRiley.UpdateSlope;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialSlope.Position <> Parameter[1]
    then DialSlope.Position := Parameter[1];
   LbDisplay.Caption := 'Slope: ' + ParameterDisplay[1] + 'dB/Oct';
  end;
end;

procedure TFmLinkwitzRiley.UpdateType;
begin
 with Owner as TLinkwitzRileySeparatedModule do
  begin
   if DialType.Position <> Parameter[2]
    then DialType.Position := Parameter[2];
   LbDisplay.Caption := ParameterDisplay[2];
  end;
end;

end.
