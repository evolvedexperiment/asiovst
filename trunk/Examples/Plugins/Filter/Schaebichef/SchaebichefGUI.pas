unit SchaebichefGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, ExtCtrls, Controls, StdCtrls,
  DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiDial,
  DAV_GuiPanel;

type
  TFmSchaebichef = class(TForm)
    DialOrder: TGuiDial;
    DialRipple: TGuiDial;
    DialFrequency: TGuiDial;
    LbFrequencyValue: TGuiLabel;
    LbRippleValue: TGuiLabel;
    LbOrderValue: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbRipple: TGuiLabel;
    LbOrder: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialRippleChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    procedure DialFrequencyDblClick(Sender: TObject);
    procedure DialRippleDblClick(Sender: TObject);
    procedure DialOrderDblClick(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure PnControlsClick(Sender: TObject);
  private
    FEdValue: TEdit;
  public
    procedure UpdateFrequency;
    procedure UpdateRipple;
    procedure UpdateOrder;
  end;

implementation

{$R *.DFM}

uses
  DAV_VSTModuleWithPrograms, SchaebichefDM;

procedure TFmSchaebichef.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'SchaebichefKnob', 'BMP');
 try
  DialFrequency.DialBitmap.LoadFromStream(RS);
  DialOrder.DialBitmap.Assign(DialFrequency.DialBitmap);
  DialRipple.DialBitmap.Assign(DialFrequency.DialBitmap);
 finally
  RS.Free;
 end;
end;

procedure TFmSchaebichef.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmSchaebichef.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateRipple;
 UpdateOrder;
(*
 with TSchaebichefLPModule(Owner) do
  begin
   Resizer.SetEditorHwnd(Self.Handle);
  end;
*)
end;

procedure TFmSchaebichef.PnControlsClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmSchaebichef.DialFrequencyChange(Sender: TObject);
begin
 with TSchaebichefLPModule(Owner) do
  begin
   if ParameterByName['Frequency'] <> DialFrequency.Position
    then ParameterByName['Frequency'] := DialFrequency.Position;
  end;
end;

procedure TFmSchaebichef.DialFrequencyDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbFrequencyValue.Left;
   Top := LbFrequencyValue.Top;
   Width := LbFrequencyValue.Width;
   Height := LbFrequencyValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbFrequencyValue.Caption;
   Tag := 0;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmSchaebichef.DialOrderChange(Sender: TObject);
begin
 with TSchaebichefLPModule(Owner) do
  begin
   if ParameterByName['Order'] <> DialOrder.Position
    then ParameterByName['Order'] := DialOrder.Position;
  end;
end;

procedure TFmSchaebichef.DialOrderDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbOrderValue.Left;
   Top := LbOrderValue.Top;
   Width := LbOrderValue.Width;
   Height := LbOrderValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbOrderValue.Caption;
   Tag := 2;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmSchaebichef.DialRippleChange(Sender: TObject);
begin
 with TSchaebichefLPModule(Owner) do
  begin
   if ParameterByName['Ripple'] <> DialRipple.Position
    then ParameterByName['Ripple'] := DialRipple.Position;
  end;
end;

procedure TFmSchaebichef.DialRippleDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbRippleValue.Left;
   Top := LbRippleValue.Top;
   Width := LbRippleValue.Width;
   Height := LbRippleValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbRippleValue.Caption;
   Tag := 1;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmSchaebichef.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TSchaebichefLPModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, FEdValue.Text);
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmSchaebichef.UpdateFrequency;
var
  Freq : Single;
begin
 with TSchaebichefLPModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialFrequency.Position <> Freq
    then DialFrequency.Position := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 5, 5) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Freq * 1E-3, ffGeneral, 5, 5) + ' kHz'
  end;
end;

procedure TFmSchaebichef.UpdateOrder;
var
  Order : Single;
begin
 with TSchaebichefLPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
   LbOrderValue.Caption := IntToStr(6 * Round(Order)) + ' dB / Oct';
  end;
end;

procedure TFmSchaebichef.UpdateRipple;
var
  Ripple : Single;
begin
 with TSchaebichefLPModule(Owner) do
  begin
   Ripple := ParameterByName['Ripple'];
   if DialRipple.Position <> Ripple
    then DialRipple.Position := Ripple;
   LbRippleValue.Caption := IntToStr(Round(Ripple * 10)) + '%';
  end;
end;

end.
