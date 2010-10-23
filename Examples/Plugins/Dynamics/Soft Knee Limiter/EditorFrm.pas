unit EditorFrm;

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
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, PNGImage,
  DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TEditorForm = class(TForm)
    DialAttack: TGuiDial;
    DialMakeUp: TGuiDial;
    DialRelease: TGuiDial;
    DialSoftKnee: TGuiDial;
    DialThreshold: TGuiDial;
    LbAttack: TGuiLabel;
    LbAttackValue: TGuiLabel;
    LbMakeUp: TGuiLabel;
    LbMakeUpValue: TGuiLabel;
    LbRelease: TGuiLabel;
    LbReleaseValue: TGuiLabel;
    LbSoftKnee: TGuiLabel;
    LbSoftKneeValue: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbThresholdValue: TGuiLabel;
    procedure DialThresholdChange(Sender: TObject);
    procedure DialMakeUpChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialSoftKneeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateAttack;
    procedure UpdateMakeUp;
    procedure UpdateRelease;
    procedure UpdateSoftKnee;
    procedure UpdateThreshold;
  end;

implementation

{$R *.DFM}

uses
  Math, Graphics, SKLDM;

procedure TEditorForm.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
  PNG : TPNGObject;
begin
 RS := TResourceStream.Create(hInstance, 'NuNop', 'PNG');
 try
  PNG := TPNGObject.Create;
  with PNG do
   try
    LoadFromStream(RS);
    with DialThreshold.DialBitmap do
     begin
(*
      PixelFormat := pf32bit;
      Transparent := True;
      TransparentMode := tmFixed;
      TransparentColor := clGray;
      DialThreshold.DialBitmap.Assign(Png);
*)
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialSoftKnee.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialAttack.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialRelease.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

    with DialMakeUp.DialBitmap do
     begin
      SetSize(PNG.Width, PNG.Height);
      Canvas.Brush.Color := Self.Color;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Draw(0, 0, PNG);
     end;

   finally
    FreeAndNil(PNG);
   end;
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateAttack;
 UpdateMakeUp;
 UpdateRelease;
 UpdateSoftKnee;
 UpdateThreshold;
end;

procedure TEditorForm.UpdateSoftKnee;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialSoftKnee.Position <> Parameter[1]
    then DialSoftKnee.Position := Parameter[1];
   LbSoftKneeValue.Caption := FloatToStrF(Parameter[1], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialAttack.Position <> Log10(Parameter[2])
    then DialAttack.Position := Log10(Parameter[2]);
   if Parameter[2] < 1
    then LbAttackValue.Caption := FloatToStrF(Parameter[2] * 1E3, ffGeneral, 4, 2) + ' μs'
    else LbAttackValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialRelease.Position <> Log10(Parameter[3])
    then DialRelease.Position := Log10(Parameter[3]);
   if Parameter[3] >= 1000
    then LbReleaseValue.Caption := FloatToStrF(Parameter[3] * 1E-3, ffGeneral, 3, 2) + ' s'
    else LbReleaseValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 3, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateMakeUp;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialMakeUp.Position <> Parameter[4]
    then DialMakeUp.Position := Parameter[4];
   LbMakeUpValue.Caption := FloatToStrF(Parameter[4], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   if DialThreshold.Position <> Parameter[0]
    then DialThreshold.Position := Parameter[0];
   LbThresholdValue.Caption := FloatToStrF(Parameter[0], ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.DialThresholdChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[0] := DialThreshold.Position;
  end;
end;

procedure TEditorForm.DialSoftKneeChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[1] := DialSoftKnee.Position;
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[2] := Power(10, DialAttack.Position);
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[3] := Power(10, DialRelease.Position);
  end;
end;

procedure TEditorForm.DialMakeUpChange(Sender: TObject);
begin
 with TSoftKneeLimiterDataModule(Owner) do
  begin
   Parameter[4] := DialMakeUp.Position;
  end;
end;

end.
