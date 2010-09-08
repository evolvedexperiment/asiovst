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
  Graphics, Classes, SysUtils, DAV_Common, DAV_GuiCommon, DAV_GuiFixedPoint;

type
  TGuiCustomGeometricShape = class(TPersistent)
  private
    FOnChange : TNotifyEvent;
  protected
    procedure Changed; virtual;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiCustomCenteredGeometricShape = class(TGuiCustomGeometricShape)
  private
    FCenterX : TFixed24Dot8Point;
    FCenterY : TFixed24Dot8Point;
    procedure SetCenterX(const Value: TFixed24Dot8Point);
    procedure SetCenterY(const Value: TFixed24Dot8Point);
  protected
    procedure CenterXChanged; virtual;
    procedure CenterYChanged; virtual;
  public
    property CenterX: TFixed24Dot8Point read FCenterX write SetCenterX;
    property CenterY: TFixed24Dot8Point read FCenterY write SetCenterY;
  end;

  TGuiCustomCircle = class(TGuiCustomCenteredGeometricShape)
  private
    FRadius  : TFixed24Dot8Point;
    procedure SetRadius(const Value: TFixed24Dot8Point);
  protected
    procedure RadiusChanged; virtual;
  public
    property Radius: TFixed24Dot8Point read FRadius write SetRadius;
  end;
  TGuiCircle = class(TGuiCustomCircle);

  TGuiCustomEllipse = class(TGuiCustomCenteredGeometricShape)
  private
    FRadiusX  : TFixed24Dot8Point;
    FRadiusY  : TFixed24Dot8Point;
    procedure SetRadiusX(const Value: TFixed24Dot8Point);
    procedure SetRadiusY(const Value: TFixed24Dot8Point);
  protected
    procedure RadiusXChanged; virtual;
    procedure RadiusYChanged; virtual;
  public
    property RadiusX: TFixed24Dot8Point read FRadiusX write SetRadiusX;
    property RadiusY: TFixed24Dot8Point read FRadiusY write SetRadiusY;
  end;
  TGuiEllipse = class(TGuiCustomEllipse);

  TGuiCustomRectangle = class(TGuiCustomGeometricShape)
  private
    FRight: TFixed24Dot8Point;
    FBottom: TFixed24Dot8Point;
    FTop: TFixed24Dot8Point;
    FLeft: TFixed24Dot8Point;
    procedure SetBottom(const Value: TFixed24Dot8Point);
    procedure SetLeft(const Value: TFixed24Dot8Point);
    procedure SetRight(const Value: TFixed24Dot8Point);
    procedure SetTop(const Value: TFixed24Dot8Point);
  protected
    procedure BottomChanged; virtual;
    procedure LeftChanged; virtual;
    procedure RightChanged; virtual;
    procedure TopChanged; virtual;
  public
    property Left: TFixed24Dot8Point read FLeft write SetLeft;
    property Right: TFixed24Dot8Point read FRight write SetRight;
    property Top: TFixed24Dot8Point read FTop write SetTop;
    property Bottom: TFixed24Dot8Point read FBottom write SetBottom;
  end;
  TGuiRectangle = TGuiCustomRectangle;

  TGuiCustomRoundedRectangle = class(TGuiCustomRectangle)
  private
    FBorderRadius: TFixed24Dot8Point;
    procedure SetBorderRadius(const Value: TFixed24Dot8Point);
  protected
    procedure BorderRadiusChanged; virtual;
  public
    property BorderRadius: TFixed24Dot8Point read FBorderRadius write SetBorderRadius;
  end;
  TGuiRoundedRectangle = class(TGuiCustomRoundedRectangle);

  TGuiCustomLine = class(TGuiCustomGeometricShape)
  private
    FXB: TFixed24Dot8Point;
    FYB: TFixed24Dot8Point;
    FXA: TFixed24Dot8Point;
    FYA: TFixed24Dot8Point;
    procedure SetXA(const Value: TFixed24Dot8Point);
    procedure SetXB(const Value: TFixed24Dot8Point);
    procedure SetYA(const Value: TFixed24Dot8Point);
    procedure SetYB(const Value: TFixed24Dot8Point);
  protected
    procedure XAChanged; virtual;
    procedure YAChanged; virtual;
    procedure XBChanged; virtual;
    procedure YBChanged; virtual;
  public
    property XA: TFixed24Dot8Point read FXA write SetXA;
    property YA: TFixed24Dot8Point read FYA write SetYA;
    property XB: TFixed24Dot8Point read FXB write SetXB;
    property YB: TFixed24Dot8Point read FYB write SetYB;
  end;
  TGuiLine = class(TGuiCustomLine);

implementation

{ TGuiCustomGeometricShape }

procedure TGuiCustomGeometricShape.Changed;
begin
 if Assigned(FOnChange)
  then FOnChange(Self);
end;


{ TGuiCustomCenteredGeometricShape }

procedure TGuiCustomCenteredGeometricShape.SetCenterX(const Value: TFixed24Dot8Point);
begin
 if FCenterX.Fixed <> Value.Fixed then
  begin
   FCenterX := Value;
   CenterXChanged;
  end;
end;

procedure TGuiCustomCenteredGeometricShape.SetCenterY(const Value: TFixed24Dot8Point);
begin
 if (FCenterY.Fixed <> Value.Fixed) then
  begin
   FCenterY := Value;
   CenterYChanged;
  end;
end;

procedure TGuiCustomCenteredGeometricShape.CenterXChanged;
begin
 Changed;
end;

procedure TGuiCustomCenteredGeometricShape.CenterYChanged;
begin
 Changed;
end;


{ TGuiCustomCircle }

procedure TGuiCustomCircle.SetRadius(const Value: TFixed24Dot8Point);
begin
 if (FRadius.Fixed <> Value.Fixed) then
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

procedure TGuiCustomEllipse.SetRadiusX(const Value: TFixed24Dot8Point);
begin
 if (FRadiusX.Fixed <> Value.Fixed) then
  begin
   FRadiusX := Value;
   RadiusXChanged;
  end;
end;

procedure TGuiCustomEllipse.SetRadiusY(const Value: TFixed24Dot8Point);
begin
 if (FRadiusY.Fixed <> Value.Fixed) then
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


{ TGuiCustomRectangle }

procedure TGuiCustomRectangle.BottomChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.LeftChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.RightChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.TopChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.SetBottom(const Value: TFixed24Dot8Point);
begin
 if FBottom.Fixed <> Value.Fixed then
  begin
   FBottom := Value;
   BottomChanged;
  end;
end;

procedure TGuiCustomRectangle.SetLeft(const Value: TFixed24Dot8Point);
begin
 if FLeft.Fixed <> Value.Fixed then
  begin
   FLeft := Value;
   LeftChanged;
  end;
end;

procedure TGuiCustomRectangle.SetRight(const Value: TFixed24Dot8Point);
begin
 if FRight.Fixed <> Value.Fixed then
  begin
   FRight := Value;
   RightChanged;
  end;
end;

procedure TGuiCustomRectangle.SetTop(const Value: TFixed24Dot8Point);
begin
 if FTop.Fixed <> Value.Fixed then
  begin
   FTop := Value;
   TopChanged;
  end;
end;


{ TGuiCustomLine }

procedure TGuiCustomLine.XAChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.XBChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.YAChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.YBChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.SetXA(const Value: TFixed24Dot8Point);
begin
 if FXA.Fixed <> Value.Fixed then
  begin
   FXA := Value;
   XAChanged;
  end;
end;

procedure TGuiCustomLine.SetXB(const Value: TFixed24Dot8Point);
begin
 if FXB.Fixed <> Value.Fixed then
  begin
   FXB := Value;
   XBChanged;
  end;
end;

procedure TGuiCustomLine.SetYA(const Value: TFixed24Dot8Point);
begin
 if FYA.Fixed <> Value.Fixed then
  begin
   FYA := Value;
   YAChanged;
  end;
end;

procedure TGuiCustomLine.SetYB(const Value: TFixed24Dot8Point);
begin
 if FYB.Fixed <> Value.Fixed then
  begin
   FYB := Value;
   YBChanged;
  end;
end;

{ TGuiCustomRoundedRectangle }

procedure TGuiCustomRoundedRectangle.BorderRadiusChanged;
begin
 Changed;
end;

procedure TGuiCustomRoundedRectangle.SetBorderRadius(
  const Value: TFixed24Dot8Point);
begin
 if FBorderRadius.Fixed <> Value.Fixed then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

end.
