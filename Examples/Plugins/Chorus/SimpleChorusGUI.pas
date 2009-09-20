unit SimpleChorusGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TFmSimpleChorus = class(TForm)
    DialDepth: TGuiDial;
    DialMix: TGuiDial;
    DialSpeed: TGuiDial;
    DialStages: TGuiDial;
    LbDepth: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbSpeed: TGuiLabel;
    LbSpeedValue: TGuiLabel;
    LbStages: TGuiLabel;
    LbStagesValue: TGuiLabel;
    DialDrift: TGuiDial;
    LbDrift: TGuiLabel;
    LbDriftValue: TGuiLabel;
    DIL: TGuiDialImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSpeedChange(Sender: TObject);
    procedure DialStagesChange(Sender: TObject);
    procedure DialDepthChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialDriftChange(Sender: TObject);
  public
    procedure UpdateDepth;
    procedure UpdateDrift;
    procedure UpdateMix;
    procedure UpdateSpeed;
    procedure UpdateStages;
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, DAV_VSTModuleWithPrograms, SimpleChorusDM;

procedure TFmSimpleChorus.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ChorusKnob', 'PNG');
  try
   with DIL.DialImages.Add do
    begin
     NumGlyphs := 65;
     PngBmp.LoadFromStream(RS);
     DialBitmap.Assign(PngBmp);
    end;
   DialSpeed.DialImageIndex  := 0;
   DialDepth.DialImageIndex  := 0;
   DialStages.DialImageIndex := 0;
   DialMix.DialImageIndex    := 0;
   DialDrift.DialImageIndex  := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSimpleChorus.FormShow(Sender: TObject);
begin
 UpdateDepth;
 UpdateDrift;
 UpdateMix;
 UpdateSpeed;
 UpdateStages;
end;

procedure TFmSimpleChorus.DialDepthChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[2] <> DialDepth.Position
    then Parameter[2] := DialDepth.Position;
  end;
end;

procedure TFmSimpleChorus.DialDriftChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[4] <> DialDrift.Position
    then Parameter[4] := DialDrift.Position;
  end;
end;

procedure TFmSimpleChorus.DialMixChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[3] <> DialMix.Position
    then Parameter[3] := DialMix.Position;
  end;
end;

procedure TFmSimpleChorus.DialSpeedChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[0] <> DialSpeed.Position
    then Parameter[0] := DialSpeed.Position;
  end;
end;

procedure TFmSimpleChorus.DialStagesChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[1] <> DialStages.Position
    then Parameter[1] := DialStages.Position;
  end;
end;

procedure TFmSimpleChorus.UpdateDepth;
var
  Depth : Single;
begin
 with TSimpleChorusModule(Owner) do
  begin
   Depth := Parameter[2];
   if DialDepth.Position <> Depth
    then DialDepth.Position := Depth;
   LbDepthValue.Caption := FloatToStrF(RoundTo(Depth, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleChorus.UpdateDrift;
var
  Drift : Single;
begin
 with TSimpleChorusModule(Owner) do
  begin
   Drift := Parameter[4];
   if DialDrift.Position <> Drift
    then DialDrift.Position := Drift;
   LbDriftValue.Caption := FloatToStrF(RoundTo(Drift, -1), ffGeneral, 3, 2) + ' %';
  end;
end;

procedure TFmSimpleChorus.UpdateMix;
var
  Mix : Single;
begin
 with TSimpleChorusModule(Owner) do
  begin
   Mix := Parameter[3];
   if DialMix.Position <> Mix
    then DialMix.Position := Mix;
   LbMixValue.Caption := FloatToStrF(RoundTo(Mix, -1), ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleChorus.UpdateSpeed;
var
  Speed : Single;
begin
 with TSimpleChorusModule(Owner) do
  begin
   Speed := Parameter[0];
   if DialSpeed.Position <> Speed
    then DialSpeed.Position := Speed;
   LbSpeedValue.Caption := FloatToStrF(RoundTo(Speed, -2), ffGeneral, 2, 2) + ' Hz';
  end;
end;

procedure TFmSimpleChorus.UpdateStages;
begin
 with TSimpleChorusModule(Owner) do
  begin
   if DialStages.Position <> Parameter[1]
    then DialStages.Position := Parameter[1];
   LbStagesValue.Caption := IntToStr(round(Parameter[1]));
  end;
end;

end.
