unit DAV_GuiShadow;

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
  {$IFDEF FPC} LCLIntf, LCLType, LResources, LMessages,
  {$IFDEF Windows} Windows, {$ENDIF} {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_MemoryUtils, DAV_GuiCommon,
  DAV_GuiBlend, DAV_GuiPixelMap, DAV_GuiByteMap, DAV_GuiFilters;

type
  TGuiShadow = class(TPersistent)
  private
    FBlur         : Single;
    FColor        : TColor;
    FOffset       : TPoint;
    FTransparency : Byte;
    FVisible      : Boolean;
    FOnChange     : TNotifyEvent;
    function GetOffsetX: Integer;
    function GetOffsetY: Integer;
    procedure SetBlur(const Value: Single);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetTransparency(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetOffset(const Value: TPoint);
    procedure SetColor(const Value: TColor);
  protected
    procedure Changed; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    property Offset: TPoint read FOffset write SetOffset;
  published
    property Blur: Single read FBlur write SetBlur;
    property Color: TColor read FColor write SetColor;
    property OffsetX: Integer read GetOffsetX write SetOffsetX default 1;
    property OffsetY: Integer read GetOffsetY write SetOffsetY default 1;
    property Transparency: Byte read FTransparency write SetTransparency default $FF;
    property Visible: Boolean read FVisible write SetVisible default False;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TGuiShadow }

constructor TGuiShadow.Create;
begin
 inherited;
 FBlur         := 4;
 FOffset.X     := 1;
 FOffset.Y     := 1;
 FTransparency := $FF;
 FVisible      := False;
end;

procedure TGuiShadow.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiShadow then
  with TGuiShadow(Dest) do
   begin
    FBlur         := Self.FBlur;
    FColor        := Self.FColor;
    FOffset.X     := Self.FOffset.X;
    FOffset.Y     := Self.FOffset.Y;
    FTransparency := Self.FTransparency;
    FVisible      := Self.FVisible;
    FOnChange     := Self.FOnChange;
   end
 else inherited;
end;

procedure TGuiShadow.Changed;
begin
 if Assigned(FOnChange)
  then FOnChange(Self);
end;

function TGuiShadow.GetOffsetX: Integer;
begin
 Result := FOffset.X;
end;

function TGuiShadow.GetOffsetY: Integer;
begin
 Result := FOffset.Y;
end;

procedure TGuiShadow.SetBlur(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create('Must be positive!');

 if FBlur <> Value then
  begin
   FBlur := Value;
   Changed;
  end;
end;

procedure TGuiShadow.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   Changed;
  end;
end;

procedure TGuiShadow.SetOffset(const Value: TPoint);
begin
 if (FOffset.X <> Value.X) or (FOffset.Y <> Value.Y) then
  begin
   FOffset := Value;
   Changed;
  end;
end;

procedure TGuiShadow.SetOffsetX(const Value: Integer);
begin
 if FOffset.X <> Value then
  begin
   FOffset.X := Value;
   Changed;
  end;
end;

procedure TGuiShadow.SetOffsetY(const Value: Integer);
begin
 if FOffset.Y <> Value then
  begin
   FOffset.Y := Value;
   Changed;
  end;
end;

procedure TGuiShadow.SetTransparency(const Value: Byte);
begin
 if FTransparency <> Value then
  begin
   FTransparency := Value;
   Changed;
  end;
end;

procedure TGuiShadow.SetVisible(const Value: Boolean);
begin
 if FVisible <> Value then
  begin
   FVisible := Value;
   Changed;
  end;
end;


end.

