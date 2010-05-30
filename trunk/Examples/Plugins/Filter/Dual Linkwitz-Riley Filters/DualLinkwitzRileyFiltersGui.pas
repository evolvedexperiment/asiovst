unit DualLinkwitzRileyFiltersGui;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, DAV_GuiPanel, DAV_GuiLabel,
  DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiGroup, DAV_GuiEQGraph, DAV_GuiLED;

type
  TFmLinkwitzRiley = class(TForm)
    DialHighpassFrequency: TGuiDial;
    DialHighpassSlope: TGuiDial;
    DialLowpassFrequency: TGuiDial;
    DialLowpassSlope: TGuiDial;
    EQGraphUpdate: TTimer;
    GpDualLiknwitzRiley: TGuiGroup;
    GuiEQGraph: TGuiEQGraph;
    LbDisplay: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbHighpass: TGuiLabel;
    LbLowpass: TGuiLabel;
    LbShowFrequencyPlot: TGuiLabel;
    LbSlope: TGuiLabel;
    LedHighCut: TGuiLED;
    LedLowCut: TGuiLED;
    Mi100Hz: TMenuItem;
    Mi10kHz: TMenuItem;
    Mi125Hz: TMenuItem;
    Mi12k5Hz: TMenuItem;
    Mi160Hz: TMenuItem;
    Mi16Hz: TMenuItem;
    Mi16kHz: TMenuItem;
    Mi1k25Hz: TMenuItem;
    Mi1k6Hz: TMenuItem;
    Mi1kHz: TMenuItem;
    Mi200Hz: TMenuItem;
    Mi20Hz: TMenuItem;
    Mi20kHz: TMenuItem;
    Mi250Hz: TMenuItem;
    Mi2k5Hz: TMenuItem;
    Mi2kHz: TMenuItem;
    Mi315Hz: TMenuItem;
    Mi31Hz5: TMenuItem;
    Mi31k5Hz: TMenuItem;
    Mi400Hz: TMenuItem;
    Mi40Hz: TMenuItem;
    Mi4kHz: TMenuItem;
    Mi500Hz: TMenuItem;
    Mi50Hz: TMenuItem;
    Mi5kHz: TMenuItem;
    Mi630Hz: TMenuItem;
    Mi63Hz: TMenuItem;
    Mi6k3Hz: TMenuItem;
    Mi800Hz: TMenuItem;
    Mi80Hz: TMenuItem;
    Mi8kHz: TMenuItem;
    MiLoadA: TMenuItem;
    MiLoadB: TMenuItem;
    MiLoadC: TMenuItem;
    MiLoadD: TMenuItem;
    MiLoadE: TMenuItem;
    MiLoadF: TMenuItem;
    MiLoadHigh: TMenuItem;
    MiStoreA: TMenuItem;
    MiStoreB: TMenuItem;
    MiStoreC: TMenuItem;
    MiStoreD: TMenuItem;
    MiStoreE: TMenuItem;
    MiStoreF: TMenuItem;
    MiStoreHigh: TMenuItem;
    N251: TMenuItem;
    PnDisplay: TGuiPanel;
    PuFrequency: TPopupMenu;
    PuPreset: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    function GuiEQGraphGetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure DialDblClick(Sender: TObject);
    procedure DialHighpassFrequencyChange(Sender: TObject);
    procedure DialHighpassFrequencyMouseEnter(Sender: TObject);
    procedure DialHighpassSlopeChange(Sender: TObject);
    procedure DialHighpassSlopeMouseEnter(Sender: TObject);
    procedure DialLowpassFrequencyChange(Sender: TObject);
    procedure DialLowpassFrequencyMouseEnter(Sender: TObject);
    procedure DialLowpassSlopeChange(Sender: TObject);
    procedure DialLowpassSlopeMouseEnter(Sender: TObject);
    procedure DialMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure EQGraphUpdateTimer(Sender: TObject);
    procedure GpDualLiknwitzRileyClick(Sender: TObject);
    procedure GuiEQGraphClick(Sender: TObject);
    procedure LbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LbShowFrequencyPlotClick(Sender: TObject);
    procedure LedHighCutClick(Sender: TObject);
    procedure LedLowCutClick(Sender: TObject);
    procedure Mi31Hz5Click(Sender: TObject);
    procedure MiFrequencyClick(Sender: TObject);
    procedure MiLoadClick(Sender: TObject);
    procedure MiStoreClick(Sender: TObject);
    procedure PuFrequencyPopup(Sender: TObject);
    procedure PuPresetPopup(Sender: TObject);
    procedure LbDisplayClick(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
    FCurrentDial     : TGuiDial;
    FIsLow           : Boolean;
    FDirectUpdate    : Boolean;
    FEdValue         : TEdit;
  public
    procedure UpdateLowpassFrequency;
    procedure UpdateLowpassSlope;
    procedure UpdateHighpassFrequency;
    procedure UpdateHighpassSlope;
    procedure UpdateType;
    procedure UpdateEQGraph;
  end;

implementation

{$R *.dfm}

uses
  Math, Registry, PNGImage, DAV_GuiCommon, DAV_VSTModuleWithPrograms,
  DualLinkwitzRileyFiltersDM;

resourcestring
  RCStrLinkwitzRiley = 'Linkwitz-Riley';

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
   DialLowpassFrequency.DialBitmap.Assign(PngBmp);
   DialLowpassSlope.DialBitmap.Assign(PngBmp);
   DialHighpassFrequency.DialBitmap.Assign(PngBmp);
   DialHighpassSlope.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmLinkwitzRiley.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmLinkwitzRiley.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmLinkwitzRiley.FormShow(Sender: TObject);
begin
 UpdateLowpassFrequency;
 UpdateLowpassSlope;
 UpdateHighpassFrequency;
 UpdateHighpassSlope;
 UpdateType;
 LbDisplay.Caption := RCStrLinkwitzRiley;
end;

procedure TFmLinkwitzRiley.GpDualLiknwitzRileyClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmLinkwitzRiley.GuiEQGraphClick(Sender: TObject);
begin
 LbShowFrequencyPlot.Visible := True;
 GuiEQGraph.Visible := False;

 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

function TFmLinkwitzRiley.GuiEQGraphGetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   Result := Magnitude_dB(Frequency);
  end;
end;

function RoundFrequency(Value: Single): Single;
var
  Base10  : Double;
begin
 Base10 := Log10(Value);
 Result := RoundTo(Value, round(Base10 - 1.5));
end;

procedure TFmLinkwitzRiley.DialLowpassFrequencyChange(Sender: TObject);
var
  NewValue : Single;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if ssAlt in KeyboardStateToShiftState
    then NewValue := RoundFrequency(DialLowpassFrequency.Position)
    else NewValue := DialLowpassFrequency.Position;

   if Parameter[0] <> NewValue
    then Parameter[0] := NewValue;
  end;
end;

procedure TFmLinkwitzRiley.DialDblClick(Sender: TObject);
begin
 if not assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := PnDisplay;
   Left := 4;
   Top := 2;
   Width := 75;
   Height := 14;
   BorderStyle := bsNone;
   Color := LbDisplay.Color;
   TabOrder := 0;
   Tag := TComponent(Sender).Tag;
   Text := LbDisplay.Caption;
   OnKeyPress := EdValueKeyPress;
   SetFocus;
  end;
end;

procedure TFmLinkwitzRiley.EdValueKeyPress(Sender: TObject; var Key: Char);
var
  TextValue : string;
  CharPos   : Integer;
begin
 with TDualLinkwitzRileyFiltersModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    TextValue := FEdValue.Text;
    CharPos := Pos(':', TextValue);
    if CharPos > 0
     then Delete(TextValue, 1, CharPos);
    StringToParameter(FEdValue.Tag, TextValue);
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmLinkwitzRiley.DialMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(FEdValue) then
  begin
   FEdValue.Tag := TComponent(Sender).Tag;
   FEdValue.Text := LbDisplay.Caption;
  end;
end;

procedure TFmLinkwitzRiley.DialLowpassFrequencyMouseEnter(Sender: TObject);
begin
 with TDualLinkwitzRileyFiltersModule(Owner)
  do LbDisplay.Caption := 'Freq.: ' + ParameterDisplay[0] + ' ' + ParameterLabel[0];

 if Sender is TGuiDial
  then FCurrentDial := TGuiDial(Sender)
end;

procedure TFmLinkwitzRiley.DialLowpassSlopeChange(Sender: TObject);
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if Parameter[1] <> DialLowpassSlope.Position
    then Parameter[1] := DialLowpassSlope.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialLowpassSlopeMouseEnter(Sender: TObject);
begin
 with TDualLinkwitzRileyFiltersModule(Owner)
  do LbDisplay.Caption := 'Slope: ' + ParameterDisplay[1] + ' ' + ParameterLabel[1];
end;

procedure TFmLinkwitzRiley.EQGraphUpdateTimer(Sender: TObject);
begin
 EQGraphUpdate.Enabled := False;
 GuiEQGraph.ChartChanged;
end;

procedure TFmLinkwitzRiley.DialHighpassFrequencyChange(Sender: TObject);
var
  NewValue : Single;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if ssAlt in KeyboardStateToShiftState
    then NewValue := RoundFrequency(DialHighpassFrequency.Position)
    else NewValue := DialHighpassFrequency.Position;

   if Parameter[2] <> NewValue
    then Parameter[2] := NewValue;
  end;
end;

procedure TFmLinkwitzRiley.DialHighpassFrequencyMouseEnter(Sender: TObject);
begin
 with TDualLinkwitzRileyFiltersModule(Owner)
  do LbDisplay.Caption := 'Freq.: ' + ParameterDisplay[2] + ' ' + ParameterLabel[2];

 if Sender is TGuiDial
  then FCurrentDial := TGuiDial(Sender)
end;

procedure TFmLinkwitzRiley.DialHighpassSlopeChange(Sender: TObject);
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if Parameter[3] <> DialHighpassSlope.Position
    then Parameter[3] := DialHighpassSlope.Position;
  end;
end;

procedure TFmLinkwitzRiley.DialHighpassSlopeMouseEnter(Sender: TObject);
begin
 with TDualLinkwitzRileyFiltersModule(Owner)
  do LbDisplay.Caption := 'Slope: ' + ParameterDisplay[3] + ' ' + ParameterLabel[3];
end;

procedure TFmLinkwitzRiley.LbDisplayClick(Sender: TObject);
begin
 LbDisplay.Caption := RCStrLinkwitzRiley;
end;

procedure TFmLinkwitzRiley.LbMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FIsLow := Sender = LbLowpass;

 if Button = mbRight
  then PuPreset.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TFmLinkwitzRiley.LbShowFrequencyPlotClick(Sender: TObject);
begin
 LbShowFrequencyPlot.Visible := False;
 GuiEQGraph.Visible := True;
end;

procedure TFmLinkwitzRiley.LedHighCutClick(Sender: TObject);
var
  CurrentBit : Integer;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   CurrentBit := round(Parameter[4]);
   Parameter[4] := (CurrentBit and $2) or ((not CurrentBit) and $1)
  end;

 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmLinkwitzRiley.LedLowCutClick(Sender: TObject);
var
  CurrentBit : Integer;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   CurrentBit := round(Parameter[4]);
   Parameter[4] := (CurrentBit and $1) or ((not CurrentBit) and $2)
  end;

 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmLinkwitzRiley.MiFrequencyClick(Sender: TObject);
begin
 assert(Sender is TMenuItem);
 if assigned(FCurrentDial)
  then FCurrentDial.Position := TMenuItem(Sender).Tag;
end;

procedure TFmLinkwitzRiley.MiLoadClick(Sender: TObject);
begin
 with TDualLinkwitzRileyFiltersModule(Owner), TComponent(Sender) do
  if FIsLow then LoadLow(Tag) else LoadHigh(Tag) 
end;

procedure TFmLinkwitzRiley.MiStoreClick(Sender: TObject);
begin
 with TDualLinkwitzRileyFiltersModule(Owner), TComponent(Sender) do
  if FIsLow then StoreLow(Tag) else StoreHigh(Tag);
end;

procedure TFmLinkwitzRiley.PuFrequencyPopup(Sender: TObject);
begin
 if Sender is TGuiDial
  then FCurrentDial := TGuiDial(Sender)
end;

procedure TFmLinkwitzRiley.PuPresetPopup(Sender: TObject);
var
  Index   : Integer;
  Caption : string;
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   for Index := 0 to 5 do
    if KeyExists(CRegistryKey + IntToStr(Index)) then
     try
      if OpenKey(CRegistryKey + IntToStr(Index), False) then
       try
        Caption := '';
        if ValueExists('Frequency')
         then Caption := Caption + FloatToStrF(ReadFloat('Frequency'), ffGeneral, 5, 5) + ' Hz';
        if ValueExists('Order')
         then Caption := Caption + ', ' + IntToStr(12 * ReadInteger('Order')) + ' dB/Oct';
        if Caption <> '' then
         case Index of
          0 : begin MiLoadA.Caption := '&A (' + Caption + ')'; MiStoreA.Caption := MiLoadA.Caption; end;
          1 : begin MiLoadB.Caption := '&B (' + Caption + ')'; MiStoreB.Caption := MiLoadB.Caption; end;
          2 : begin MiLoadC.Caption := '&C (' + Caption + ')'; MiStoreC.Caption := MiLoadC.Caption; end;
          3 : begin MiLoadD.Caption := '&D (' + Caption + ')'; MiStoreD.Caption := MiLoadD.Caption; end;
          4 : begin MiLoadE.Caption := '&E (' + Caption + ')'; MiStoreE.Caption := MiLoadE.Caption; end;
          5 : begin MiLoadF.Caption := '&F (' + Caption + ')'; MiStoreF.Caption := MiLoadF.Caption; end;
         end;
       finally
        CloseKey;
       end;
     except
      DeleteKey(CRegistryKey + IntToStr(Index));
     end;
  finally
   Free;
  end;
end;

procedure TFmLinkwitzRiley.Mi31Hz5Click(Sender: TObject);
begin
 assert(Sender is TMenuItem);
 if assigned(FCurrentDial)
  then FCurrentDial.Position := 31.5;
end;

procedure TFmLinkwitzRiley.UpdateLowpassFrequency;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if DialLowpassFrequency.Position <> Parameter[0]
    then DialLowpassFrequency.Position := Parameter[0];
   LbDisplay.Caption := 'Freq.: ' + ParameterDisplay[0] + ' ' + ParameterLabel[0];
   UpdateEQGraph;
  end;
end;

procedure TFmLinkwitzRiley.UpdateLowpassSlope;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if DialLowpassSlope.Position <> Parameter[1]
    then DialLowpassSlope.Position := Parameter[1];
   LbDisplay.Caption := 'Slope: ' + ParameterDisplay[1] + ' ' + ParameterLabel[1];
   UpdateEQGraph;
  end;
end;

procedure TFmLinkwitzRiley.UpdateHighpassFrequency;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if DialHighpassFrequency.Position <> Parameter[2]
    then DialHighpassFrequency.Position := Parameter[2];
   LbDisplay.Caption := 'Freq.: ' + ParameterDisplay[2] + ' ' + ParameterLabel[2];
   UpdateEQGraph;
  end;
end;

procedure TFmLinkwitzRiley.UpdateHighpassSlope;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   if DialHighpassSlope.Position <> Parameter[3]
    then DialHighpassSlope.Position := Parameter[3];
   LbDisplay.Caption := 'Slope: ' + ParameterDisplay[3] + ' ' + ParameterLabel[3];
   UpdateEQGraph;
  end;
end;

procedure TFmLinkwitzRiley.UpdateType;
var
  CurrentBit : Integer;
begin
 with Owner as TDualLinkwitzRileyFiltersModule do
  begin
   CurrentBit := round(Parameter[4]);
   if (CurrentBit and $1) > 0
    then LedHighCut.Brightness_Percent := 90
    else LedHighCut.Brightness_Percent := 10;
   if (CurrentBit and $2) > 0
    then LedLowCut.Brightness_Percent := 90
    else LedLowCut.Brightness_Percent := 10;

   LbDisplay.Caption := ParameterDisplay[4];
   UpdateEQGraph;
  end;
end;

procedure TFmLinkwitzRiley.UpdateEQGraph;
begin
 if FDirectUpdate
  then GuiEQGraph.Invalidate
  else EQGraphUpdate.Enabled := True;
end;

end.
