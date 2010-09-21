unit DAV_GuiStitchedSwitch;
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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, Contnrs, DAV_GuiCommon,
  DAV_GuiStitchedControls;

type
  TCustomGuiStitchedSwitch = class(TGuiCustomStitchedControl)
  private
    FGlyphIndex        : Integer;
    FDefaultGlyphIndex : Integer;
    FReadOnly          : Boolean;
    procedure SetGlyphIndex(Value: Integer);
    procedure SetDefaultGlyphIndex(Value: Integer);
    procedure GlyphIndexChanged;
  protected
    function GetGlyphIndex: Integer; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure GlyphCountChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DefaultGlyphIndex: Integer read FDefaultGlyphIndex write SetDefaultGlyphIndex;
    property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
  end;

  TGuiStichedSwitch = class(TCustomGuiStitchedSwitch)
  published
    property DefaultGlyphIndex;
    property GlyphIndex;
    property ReadOnly;
    property AutoSize;
    property StitchedImageIndex;
    property StitchedImageList;
    property OnChange;
  end;

implementation

{ TCustomGuiStitchedSwitch }

constructor TCustomGuiStitchedSwitch.Create(AOwner: TComponent);
begin
 inherited;
 FGlyphIndex := 0;
 FDefaultGlyphIndex := 0;
end;

destructor TCustomGuiStitchedSwitch.Destroy;
begin
 inherited;
end;

function TCustomGuiStitchedSwitch.GetGlyphIndex: Integer;
begin
 Result := FGlyphIndex;
end;

procedure TCustomGuiStitchedSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 if not FReadOnly then
  begin
   if (Button = mbLeft) then
    if FGlyphIndex < FGlyphCount - 1
     then GlyphIndex := FGlyphIndex + 1
     else GlyphIndex := 0 else
   if (Button = mbRight) then
    if FGlyphIndex > 0
     then GlyphIndex := FGlyphIndex - 1
     else GlyphIndex := FGlyphCount - 1;
  end;
 inherited;
end;

procedure TCustomGuiStitchedSwitch.GlyphCountChanged;
begin
 inherited;
 if FDefaultGlyphIndex >= FGlyphCount then DefaultGlyphIndex := FGlyphCount - 1;
 if FGlyphIndex >= FGlyphCount then GlyphIndex := FGlyphCount - 1;
end;

(*
procedure TCustomGuiStitchedSwitch.RenderBitmap(const Bitmap: TBitmap);
var
  txt : string;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;
   Font.Assign(Self.Font);
   Font.Size := Font.Size * OversamplingFactor;
   if FGlyphIndex < FStringList.Count
    then txt := FStringList[FGlyphIndex]
    else txt := IntToStr(FGlyphIndex);
   TextOut((Width - TextWidth(txt)) div 2, 0, txt);
  end;
end;
*)

procedure TCustomGuiStitchedSwitch.SetDefaultGlyphIndex(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value >= FGlyphCount then Value := FGlyphCount - 1;
 if Value <> FDefaultGlyphIndex then
  begin
   FDefaultGlyphIndex := Value;
  end;
end;

procedure TCustomGuiStitchedSwitch.SetGlyphIndex(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value >= FGlyphCount then Value := FGlyphCount - 1;
 if Value <> FGlyphIndex then
  begin
   FGlyphIndex := Value;
   GlyphIndexChanged;
  end;
end;

procedure TCustomGuiStitchedSwitch.GlyphIndexChanged;
begin
 if Assigned(FOnChange) and ([csLoading, csDestroying] * ComponentState = [])
  then FOnChange(Self);
 Invalidate;
end;

end.

