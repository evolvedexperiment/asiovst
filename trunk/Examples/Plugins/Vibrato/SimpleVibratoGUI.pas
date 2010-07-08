unit SimpleVibratoGUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows,{$ENDIF}
  Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TFmSimpleVibrato = class(TForm)
    DialDepth: TGuiDial;
    DialSpeed: TGuiDial;
    LbDepth: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbSpeed: TGuiLabel;
    LbSpeedValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSpeedChange(Sender: TObject);
    procedure DialDepthChange(Sender: TObject);
  public
    procedure UpdateDepth;
    procedure UpdateSpeed;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, {$IFDEF FPC} Graphics, {$ELSE} PngImage, {$ENDIF}
  DAV_VSTModuleWithPrograms, SimpleVibratoDM;

procedure TFmSimpleVibrato.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  {$IFDEF FPC}
  PngBmp : TPortableNetworkGraphic;
  {$ELSE}
  PngBmp : TPngObject;
  {$ENDIF}
begin
 {$IFDEF FPC}
 PngBmp := TPortableNetworkGraphic.Create;
 {$ELSE}
 PngBmp := TPngObject.Create;
 {$ENDIF}
 try
  RS := TResourceStream.Create(hInstance, 'VibratoKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSpeed.DialBitmap.Assign(PngBmp);
   DialDepth.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSimpleVibrato.FormShow(Sender: TObject);
begin
 UpdateDepth;
 UpdateSpeed;
end;

procedure TFmSimpleVibrato.DialDepthChange(Sender: TObject);
begin
 with TSimpleVibratoModule(Owner) do
  begin
   if Parameter[1] <> DialDepth.Position
    then Parameter[1] := DialDepth.Position;
  end;
end;

procedure TFmSimpleVibrato.DialSpeedChange(Sender: TObject);
begin
 with TSimpleVibratoModule(Owner) do
  begin
   if Parameter[0] <> DialSpeed.Position
    then Parameter[0] := DialSpeed.Position;
  end;
end;

procedure TFmSimpleVibrato.UpdateDepth;
var
  Depth : Single;
begin
 with TSimpleVibratoModule(Owner) do
  begin
   Depth := Parameter[1];
   if DialDepth.Position <> Depth
    then DialDepth.Position := Depth;
   LbDepthValue.Caption := FloatToStrF(RoundTo(Depth, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleVibrato.UpdateSpeed;
var
  Speed : Single;
begin
 with TSimpleVibratoModule(Owner) do
  begin
   Speed := Parameter[0];
   if DialSpeed.Position <> Speed
    then DialSpeed.Position := Speed;
   LbSpeedValue.Caption := FloatToStrF(RoundTo(Speed, -2), ffGeneral, 2, 2) + ' Hz';
  end;
end;

{$IFDEF FPC}
initialization
  {$i SimpleVibratoGUI.lrs}
{$ENDIF}

end.