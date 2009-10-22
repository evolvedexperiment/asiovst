unit SimpleFlangerGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TFmSimpleFlanger = class(TForm)
    DialDepth: TGuiDial;
    DialMix: TGuiDial;
    DialSpeed: TGuiDial;
    LbDepth: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbSpeed: TGuiLabel;
    LbSpeedValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSpeedChange(Sender: TObject);
    procedure DialDepthChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
  public
    procedure UpdateDepth;
    procedure UpdateMix;
    procedure UpdateSpeed;
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, DAV_VSTModuleWithPrograms, SimpleFlangerDM;

procedure TFmSimpleFlanger.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'FlangerKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSpeed.DialBitmap.Assign(PngBmp);
   DialDepth.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSimpleFlanger.FormShow(Sender: TObject);
begin
 UpdateDepth;
 UpdateMix;
 UpdateSpeed;
end;

procedure TFmSimpleFlanger.DialDepthChange(Sender: TObject);
begin
 with TSimpleFlangerModule(Owner) do
  begin
   if Parameter[0] <> DialDepth.Position
    then Parameter[0] := DialDepth.Position;
  end;
end;

procedure TFmSimpleFlanger.DialSpeedChange(Sender: TObject);
begin
 with TSimpleFlangerModule(Owner) do
  begin
   if Parameter[1] <> DialSpeed.Position
    then Parameter[1] := DialSpeed.Position;
  end;
end;

procedure TFmSimpleFlanger.DialMixChange(Sender: TObject);
begin
 with TSimpleFlangerModule(Owner) do
  begin
   if Parameter[2] <> DialMix.Position
    then Parameter[2] := DialMix.Position;
  end;
end;

procedure TFmSimpleFlanger.UpdateDepth;
var
  Depth : Single;
begin
 with TSimpleFlangerModule(Owner) do
  begin
   Depth := Parameter[0];
   if DialDepth.Position <> Depth
    then DialDepth.Position := Depth;
   LbDepthValue.Caption := FloatToStrF(RoundTo(Depth, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleFlanger.UpdateSpeed;
var
  Speed : Single;
begin
 with TSimpleFlangerModule(Owner) do
  begin
   Speed := Parameter[1];
   if DialSpeed.Position <> Speed
    then DialSpeed.Position := Speed;
   LbSpeedValue.Caption := FloatToStrF(RoundTo(Speed, -2), ffGeneral, 2, 2) + ' Hz';
  end;
end;

procedure TFmSimpleFlanger.UpdateMix;
var
  Mix : Single;
begin
 with TSimpleFlangerModule(Owner) do
  begin
   Mix := Parameter[2];
   if DialMix.Position <> Mix
    then DialMix.Position := Mix;
   LbMixValue.Caption := FloatToStrF(RoundTo(Mix, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

end.
