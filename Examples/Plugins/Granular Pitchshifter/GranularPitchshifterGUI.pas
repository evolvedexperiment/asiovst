unit GranularPitchShifterGUI;

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
  Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel, DAV_GuiSelectBox;

type
  TFmGranularPitchShifter = class(TForm)
    DialStages: TGuiDial;
    LbStages: TGuiLabel;
    LbStagesValue: TGuiLabel;
    DialSemitones: TGuiDial;
    LbSemitones: TGuiLabel;
    LbSemitonesValue: TGuiLabel;
    DialGranularity: TGuiDial;
    LbGranularity: TGuiLabel;
    LbGranularityValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSemitonesChange(Sender: TObject);
    procedure DialStagesChange(Sender: TObject);
    procedure DialGranularityChange(Sender: TObject);
  public
    procedure UpdateSemitones;
    procedure UpdateGranularity;
    procedure UpdateStages;
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, DAV_VSTModuleWithPrograms, GranularPitchShifterDM;

procedure TFmGranularPitchShifter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'GranularPitchShifterKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSemitones.DialBitmap.Assign(PngBmp);
   DialStages.DialBitmap.Assign(PngBmp);
   DialGranularity.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmGranularPitchShifter.FormShow(Sender: TObject);
begin
 UpdateSemitones;
 UpdateGranularity;
 UpdateStages;
end;

procedure TFmGranularPitchShifter.DialSemitonesChange(Sender: TObject);
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if Parameter[0] <> DialSemitones.Position
    then Parameter[0] := DialSemitones.Position;
  end;
end;

procedure TFmGranularPitchShifter.DialGranularityChange(Sender: TObject);
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if Parameter[1] <> DialGranularity.Position
    then Parameter[1] := DialGranularity.Position;
  end;
end;

procedure TFmGranularPitchShifter.DialStagesChange(Sender: TObject);
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if Parameter[2] <> DialStages.Position
    then Parameter[2] := DialStages.Position;
  end;
end;

procedure TFmGranularPitchShifter.UpdateSemitones;
var
  Semitones : Single;
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   Semitones := Parameter[0];
   if DialSemitones.Position <> Semitones
    then DialSemitones.Position := Semitones;
   LbSemitonesValue.Caption := FloatToStrF(RoundTo(Semitones, -2), ffGeneral, 2, 2);
  end;
end;

procedure TFmGranularPitchShifter.UpdateGranularity;
var
  Granularity : Single;
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   Granularity := Parameter[1];
   if DialGranularity.Position <> Granularity
    then DialGranularity.Position := Granularity;
   LbGranularityValue.Caption := FloatToStrF(RoundTo(Granularity, -2), ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmGranularPitchShifter.UpdateStages;
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if DialStages.Position <> Parameter[2]
    then DialStages.Position := Parameter[2];
   LbStagesValue.Caption := IntToStr(round(Parameter[2]));
  end;
end;

end.
