unit DAV_GuiVector;

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
  {$IFDEF Windows} Windows, {$ENDIF}
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_GuiCommon;

type
  TIntFloatRecord = record
    Value   : Integer;
    Fractal : Single;
  end;

  TGuiCustomPrimitive = class(TPersistent)
  private
    FOnChange : TNotifyEvent;
  protected
    procedure Changed; virtual;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiCustomSimplePrimitive = class(TGuiCustomPrimitive)
  private
    FColor: TColor;
    FAlpha: Byte;
    procedure SetAlpha(const Value: Byte);
    procedure SetColor(const Value: TColor);
  protected
    procedure AlphaChanged; virtual;
    procedure ColorChanged; virtual;
  public
    property Color: TColor read FColor write SetColor;
    property Alpha: Byte read FAlpha write SetAlpha;
  end;

  TGuiCustomCenteredPrimitive = class(TGuiCustomSimplePrimitive)
  private
    FCenterX : TIntFloatRecord;
    FCenterY : TIntFloatRecord;
    procedure SetCenterX(const Value: TIntFloatRecord);
    procedure SetCenterY(const Value: TIntFloatRecord);
  protected
    procedure CenterXChanged; virtual;
    procedure CenterYChanged; virtual;
  public
    property CenterX: TIntFloatRecord read FCenterX write SetCenterX;
    property CenterY: TIntFloatRecord read FCenterY write SetCenterY;
  end;

  TGuiCustomCircle = class(TGuiCustomCenteredPrimitive)
  private
    FRadius  : TIntFloatRecord;
    procedure SetRadius(const Value: TIntFloatRecord);
  protected
    procedure RadiusChanged; virtual;
  public
    property Radius: TIntFloatRecord read FRadius write SetRadius;
  end;

  TGuiCircle = class(TGuiCustomCircle);

  TGuiCustomEllipse = class(TGuiCustomCenteredPrimitive)
  private
    FRadiusX  : TIntFloatRecord;
    FRadiusY  : TIntFloatRecord;
    procedure SetRadiusX(const Value: TIntFloatRecord);
    procedure SetRadiusY(const Value: TIntFloatRecord);
  protected
    procedure RadiusXChanged; virtual;
    procedure RadiusYChanged; virtual;
  public
    property RadiusX: TIntFloatRecord read FRadiusX write SetRadiusX;
    property RadiusY: TIntFloatRecord read FRadiusY write SetRadiusY;
  end;

  TGuiEllipse = class(TGuiCustomCircle);

(*
  TCustomLine = class(TPersistent)
  public
    property X1: TPoint
//    property
  end;
*)

implementation

{ TGuiCustomPrimitive }

procedure TGuiCustomPrimitive.Changed;
begin
 if Assigned(FOnChange)
  then FOnChange(Self);
end;


{ TGuiCustomSimplePrimitive }

procedure TGuiCustomSimplePrimitive.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TGuiCustomSimplePrimitive.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;

procedure TGuiCustomSimplePrimitive.AlphaChanged;
begin
 Changed;
end;

procedure TGuiCustomSimplePrimitive.ColorChanged;
begin
 Changed;
end;


{ TGuiCustomCenteredPrimitive }

procedure TGuiCustomCenteredPrimitive.SetCenterX(const Value: TIntFloatRecord);
begin
 if (FCenterX.Value <> Value.Value) or
    (FCenterX.Fractal <> Value.Fractal) then
  begin
   FCenterX := Value;
   CenterXChanged;
  end;
end;

procedure TGuiCustomCenteredPrimitive.SetCenterY(const Value: TIntFloatRecord);
begin
 if (FCenterY.Value <> Value.Value) or
    (FCenterY.Fractal <> Value.Fractal) then
  begin
   FCenterY := Value;
   CenterYChanged;
  end;
end;

procedure TGuiCustomCenteredPrimitive.CenterXChanged;
begin
 Changed;
end;

procedure TGuiCustomCenteredPrimitive.CenterYChanged;
begin
 Changed;
end;


{ TGuiCustomCircle }

procedure TGuiCustomCircle.SetRadius(const Value: TIntFloatRecord);
begin
 if (FRadius.Value <> Value.Value) or
    (FRadius.Fractal <> Value.Fractal) then
  begin
   FRadius := Value;
   RadiusChanged;
  end;
end;

procedure TGuiCustomCircle.RadiusChanged;
begin
 Changed;
end;


{ TGuiCustomEllipse }

procedure TGuiCustomEllipse.SetRadiusX(const Value: TIntFloatRecord);
begin
 if (FRadiusX.Value <> Value.Value) or
    (FRadiusX.Fractal <> Value.Fractal) then
  begin
   FRadiusX := Value;
   RadiusXChanged;
  end;
end;

procedure TGuiCustomEllipse.SetRadiusY(const Value: TIntFloatRecord);
begin
 if (FRadiusY.Value <> Value.Value) or
    (FRadiusY.Fractal <> Value.Fractal) then
  begin
   FRadiusY := Value;
   RadiusYChanged;
  end;
end;

procedure TGuiCustomEllipse.RadiusXChanged;
begin
 Changed;
end;

procedure TGuiCustomEllipse.RadiusYChanged;
begin
 Changed;
end;

end.
