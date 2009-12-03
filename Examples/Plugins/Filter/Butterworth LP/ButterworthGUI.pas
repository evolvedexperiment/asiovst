unit ButterworthGUI;

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
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows {$ENDIF}, Messages,
  SysUtils, Classes, Forms, Controls, ExtCtrls, StdCtrls, DAV_Types,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiLabel, DAV_GuiDial, DAV_GuiPanel,
  DAV_GuiEQGraph;

type
  TFmButterworth = class(TForm)
    DialFrequency: TGuiDial;
    DialOrder: TGuiDial;
    LbButterworthFilterDemo: TGuiLabel;
    LbButterworthFilterDemoShaddow: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbOrder: TGuiLabel;
    LbOrderValue: TGuiLabel;
    PnControls: TGuiPanel;
    GuiEQGraph: TGuiEQGraph;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialOrderChange(Sender: TObject);
    function GetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure EQGraphUpdateTimer(Sender: TObject);
    procedure LbFrequencyValueDblClick(Sender: TObject);
    procedure PnControlsClick(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure LbOrderValueDblClick(Sender: TObject);
  private
    FEdValue: TEdit;
    procedure EQGraphUpdate;
  public
    procedure UpdateFrequency;
    procedure UpdateOrder;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  DAV_VSTModuleWithPrograms, ButterworthDM;

procedure TFmButterworth.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'WineKnob', 'BMP');
 try
  DialFrequency.DialBitmap.LoadFromStream(RS);
  DialOrder.DialBitmap.Assign(DialFrequency.DialBitmap);
 finally
  FreeAndNil(RS);
 end;
end;

procedure TFmButterworth.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmButterworth.DialFrequencyChange(Sender: TObject);
begin
 with TButterworthLPModule(Owner) do
  begin
   ParameterByName['Frequency'] := DialFrequency.Position;
  end;
end;

procedure TFmButterworth.DialOrderChange(Sender: TObject);
begin
 with TButterworthLPModule(Owner) do
  begin
   ParameterByName['Order'] := DialOrder.Position;
  end;
end;

procedure TFmButterworth.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateOrder;
end;

function TFmButterworth.GetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 Result := TButterworthLPModule(Owner).Magnitude_dB(Frequency);
end;

procedure TFmButterworth.LbFrequencyValueDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbFrequencyValue.Left;
   Top := LbFrequencyValue.Top;
   Width := LbFrequencyValue.Width;
   Height := LbFrequencyValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbFrequencyValue.Caption;
   Tag := 0;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmButterworth.LbOrderValueDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnControls;
   Left := LbOrderValue.Left;
   Top := LbOrderValue.Top;
   Width := LbOrderValue.Width;
   Height := LbOrderValue.Height;
   BorderStyle := bsNone;
   Color := PnControls.PanelColor;
   Text := LbOrderValue.Caption;
   Tag := 1;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmButterworth.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TButterworthLPModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, FEdValue.Text);
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmButterworth.PnControlsClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmButterworth.UpdateFrequency;
var
  Freq : Single;
begin
 with TButterworthLPModule(Owner) do
  begin
   Freq := ParameterByName['Frequency'];
   if DialFrequency.Position <> Freq
    then DialFrequency.Position := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 4, 4) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(Freq * 1E-3, ffGeneral, 4, 4) + ' kHz';
   EQGraphUpdate;
  end;
end;

procedure TFmButterworth.UpdateOrder;
var
  Order : Single;
begin
 with TButterworthLPModule(Owner) do
  begin
   Order := ParameterByName['Order'];
   if DialOrder.Position <> Order
    then DialOrder.Position := Order;
   LbOrderValue.Caption := IntToStr(round(Order));
   EQGraphUpdate;
  end;
end;

procedure TFmButterworth.EQGraphUpdateTimer(Sender: TObject);
begin
 Timer.Enabled := False;
 GuiEQGraph.Invalidate;
end;

procedure TFmButterworth.EQGraphUpdate;
begin
 Timer.Enabled := True;
end;

{$IFDEF FPC}
initialization
  {$i ButterworthGUI.lrs}
{$ENDIF}

end.
