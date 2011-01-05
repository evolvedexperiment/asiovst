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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LResources, LMessages,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF} {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_GuiCommon, DAV_GuiFixedPoint;

type
  TGuiCustomGeometricShape = class(TPersistent)
  private
    FOnChange : TNotifyEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
  public
    constructor Create; virtual; abstract;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiCustomCenteredGeometricShape = class(TGuiCustomGeometricShape)
  private
    FCenterX : TFixed24Dot8Point;
    FCenterY : TFixed24Dot8Point;
    procedure SetCenterX(const Value: TFixed24Dot8Point);
    procedure SetCenterY(const Value: TFixed24Dot8Point);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CenterXChanged; virtual;
    procedure CenterYChanged; virtual;
  public
    constructor Create; override;

    property CenterX: TFixed24Dot8Point read FCenterX write SetCenterX;
    property CenterY: TFixed24Dot8Point read FCenterY write SetCenterY;
  end;

  TGuiCustomCircle = class(TGuiCustomCenteredGeometricShape)
  private
    FRadius  : TFixed24Dot8Point;
    procedure SetRadius(const Value: TFixed24Dot8Point);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure RadiusChanged; virtual;
  public
    constructor Create; override;

    property Radius: TFixed24Dot8Point read FRadius write SetRadius;
  end;
  TGuiCircle = class(TGuiCustomCircle);

  TGuiCustomCircleSector = class(TGuiCustomCircle)
  private
    FAngleStart : TFixed24Dot8Point;
    FAngleEnd   : TFixed24Dot8Point;
    procedure SetAngleEnd(const Value: TFixed24Dot8Point);
    procedure SetAngleStart(const Value: TFixed24Dot8Point);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AngleStartChanged; virtual;
    procedure AngleEndChanged; virtual;
  public
    constructor Create; override;

    property AngleStart: TFixed24Dot8Point read FAngleStart write SetAngleStart;
    property AngleEnd: TFixed24Dot8Point read FAngleEnd write SetAngleEnd;
  end;
  TGuiCircleSector = class(TGuiCustomCircleSector);

  TGuiCustomEllipse = class(TGuiCustomCenteredGeometricShape)
  private
    FRadiusX  : TFixed24Dot8Point;
    FRadiusY  : TFixed24Dot8Point;
    procedure SetRadiusX(const Value: TFixed24Dot8Point);
    procedure SetRadiusY(const Value: TFixed24Dot8Point);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure RadiusXChanged; virtual;
    procedure RadiusYChanged; virtual;
  public
    constructor Create; override;

    property RadiusX: TFixed24Dot8Point read FRadiusX write SetRadiusX;
    property RadiusY: TFixed24Dot8Point read FRadiusY write SetRadiusY;
  end;
  TGuiEllipse = class(TGuiCustomEllipse);

  TGuiCustomRectangle = class(TGuiCustomGeometricShape)
  private
    FRight  : TFixed24Dot8Point;
    FBottom : TFixed24Dot8Point;
    FTop    : TFixed24Dot8Point;
    FLeft   : TFixed24Dot8Point;
    procedure SetBottom(const Value: TFixed24Dot8Point);
    procedure SetLeft(const Value: TFixed24Dot8Point);
    procedure SetRight(const Value: TFixed24Dot8Point);
    procedure SetTop(const Value: TFixed24Dot8Point);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BottomChanged; virtual;
    procedure LeftChanged; virtual;
    procedure RightChanged; virtual;
    procedure TopChanged; virtual;
  public
    constructor Create; override;

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
    procedure AssignTo(Dest: TPersistent); override;
    procedure BorderRadiusChanged; virtual;
  public
    constructor Create; override;

    property BorderRadius: TFixed24Dot8Point read FBorderRadius write SetBorderRadius;
  end;
  TGuiRoundedRectangle = class(TGuiCustomRoundedRectangle);

  TGuiCustomLine = class(TGuiCustomGeometricShape)
  private
    FXA: TFixed24Dot8Point;
    FYA: TFixed24Dot8Point;
    FXB: TFixed24Dot8Point;
    FYB: TFixed24Dot8Point;
    procedure SetXA(const Value: TFixed24Dot8Point);
    procedure SetXB(const Value: TFixed24Dot8Point);
    procedure SetYA(const Value: TFixed24Dot8Point);
    procedure SetYB(const Value: TFixed24Dot8Point);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure XAChanged; virtual;
    procedure YAChanged; virtual;
    procedure XBChanged; virtual;
    procedure YBChanged; virtual;
  public
    constructor Create; override;

    property XA: TFixed24Dot8Point read FXA write SetXA;
    property YA: TFixed24Dot8Point read FYA write SetYA;
    property XB: TFixed24Dot8Point read FXB write SetXB;
    property YB: TFixed24Dot8Point read FYB write SetYB;
  end;
  TGuiLine = class(TGuiCustomLine);

  TFixed24Dot8PointPoint = record
    X, Y : TFixed24Dot8Point;
  end;

  TFixed16Dot16PointPoint = record
    X, Y : TFixed16Dot16Point;
  end;

  TGuiCustomPolygon = class(TGuiCustomGeometricShape)
  private
    FData : array of TFixed24Dot8PointPoint;
    function GetX(Index: Integer): TFixed24Dot8Point;
    function GetY(Index: Integer): TFixed24Dot8Point;
    procedure SetX(Index: Integer; const Value: TFixed24Dot8Point);
    procedure SetY(Index: Integer; const Value: TFixed24Dot8Point);
    function GetCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddPoint(X, Y: TFixed24Dot8Point); overload;
    procedure AddPoint(Point: TFixed24Dot8PointPoint); overload;
(*
    procedure RemovePoint(X, Y: TFixed24Dot8Point); overload;
    procedure RemovePoint(Point: TFixed24Dot8PointPoint); overload;
*)
    procedure RemovePoint(Index: Integer); overload;

    property X[Index: Integer]: TFixed24Dot8Point read GetX write SetX;
    property Y[Index: Integer]: TFixed24Dot8Point read GetY write SetY;
    property Count: Integer read GetCount;
  end;
  TGuiPolygon = class(TGuiCustomPolygon);

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds %d';

{ TGuiCustomGeometricShape }

procedure TGuiCustomGeometricShape.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiCustomGeometricShape then
  with TGuiCustomGeometricShape(Dest) do
   begin
    FOnChange := Self.FOnChange;
   end
 else inherited;
end;

procedure TGuiCustomGeometricShape.Changed;
begin
 if Assigned(FOnChange)
  then FOnChange(Self);
end;


{ TGuiCustomCenteredGeometricShape }

constructor TGuiCustomCenteredGeometricShape.Create;
begin
 inherited;
 FCenterX.Fixed := 0;
 FCenterY.Fixed := 0;
end;

procedure TGuiCustomCenteredGeometricShape.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomCenteredGeometricShape then
  with TGuiCustomCenteredGeometricShape(Dest) do
   begin
    FCenterX := Self.FCenterX;
    FCenterY := Self.FCenterY;
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


{ TGuiCustomCircle }

constructor TGuiCustomCircle.Create;
begin
 inherited;
 FRadius := CFixed24Dot8One;
end;

procedure TGuiCustomCircle.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomCircle then
  with TGuiCustomCircle(Dest) do
   begin
    FRadius := Self.FRadius;
   end;
end;

procedure TGuiCustomCircle.RadiusChanged;
begin
 Changed;
end;

procedure TGuiCustomCircle.SetRadius(const Value: TFixed24Dot8Point);
begin
 if (FRadius.Fixed <> Value.Fixed) then
  begin
   FRadius := Value;
   RadiusChanged;
  end;
end;


{ TGuiCustomCircleSector }

constructor TGuiCustomCircleSector.Create;
begin
 inherited;

 FAngleStart.Fixed := 0;
 FAngleEnd := CFixed24Dot8PI;
end;

procedure TGuiCustomCircleSector.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiCustomCircleSector then
  with TGuiCustomCircleSector(Dest) do
   begin
    FAngleStart := Self.FAngleStart;
    FAngleEnd := Self.FAngleEnd;
   end;
end;

procedure TGuiCustomCircleSector.AngleEndChanged;
begin
 Changed;
end;

procedure TGuiCustomCircleSector.AngleStartChanged;
begin
 Changed;
end;

procedure TGuiCustomCircleSector.SetAngleEnd(const Value: TFixed24Dot8Point);
begin
 if FAngleEnd.Fixed <> Value.Fixed then
  begin
   FAngleEnd := Value;
   AngleEndChanged;
  end;
end;

procedure TGuiCustomCircleSector.SetAngleStart(const Value: TFixed24Dot8Point);
begin
 if FAngleStart.Fixed <> Value.Fixed then
  begin
   FAngleStart := Value;
   AngleStartChanged;
  end;
end;


{ TGuiCustomEllipse }

constructor TGuiCustomEllipse.Create;
begin
 inherited;
 FRadiusX := CFixed24Dot8One;
 FRadiusY := CFixed24Dot8One;
end;

procedure TGuiCustomEllipse.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomEllipse then
  with TGuiCustomEllipse(Dest) do
   begin
    FRadiusX := Self.FRadiusX;
    FRadiusY := Self.FRadiusY;
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


{ TGuiCustomRectangle }

constructor TGuiCustomRectangle.Create;
begin
 inherited;
 FLeft.Fixed := 0;
 FTop.Fixed := 0;
 FRight := CFixed24Dot8One;
 FBottom := CFixed24Dot8One;
end;

procedure TGuiCustomRectangle.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomRectangle then
  with TGuiCustomRectangle(Dest) do
   begin
    FRight  := Self.FRight;
    FBottom := Self.FBottom;
    FTop    := Self.FTop;
    FLeft   := Self.FLeft;
   end;
end;

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


{ TGuiCustomRoundedRectangle }

procedure TGuiCustomRoundedRectangle.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomRoundedRectangle then
  with TGuiCustomRoundedRectangle(Dest) do
   begin
    FBorderRadius := Self.FBorderRadius;
   end;
end;

procedure TGuiCustomRoundedRectangle.BorderRadiusChanged;
begin
 Changed;
end;

constructor TGuiCustomRoundedRectangle.Create;
begin
 inherited;
 FBorderRadius := CFixed24Dot8One;
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


{ TGuiCustomLine }

constructor TGuiCustomLine.Create;
begin
 inherited;
 FXA.Fixed := 0;
 FYA.Fixed := 0;
 FXB.Fixed := 0;
 FYB.Fixed := 0;
end;

procedure TGuiCustomLine.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomLine then
  with TGuiCustomLine(Dest) do
   begin
    FXA := Self.FXA;
    FYA := Self.FYA;
    FXB := Self.FXB;
    FYB := Self.FYB;
   end;
end;

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


{ TGuiCustomPolygon }

constructor TGuiCustomPolygon.Create;
begin
 inherited;
end;

destructor TGuiCustomPolygon.Destroy;
begin
 inherited;
end;

procedure TGuiCustomPolygon.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomPolygon then
  with TGuiCustomPolygon(Dest) do
   begin
    SetLength(FData, Length(Self.FData));
    if Length(Self.FData) > 0
     then Move(Self.FData[0], FData[0], Length(Self.FData) * SizeOf(TFixed24Dot8PointPoint));
   end;
end;

procedure TGuiCustomPolygon.AddPoint(X, Y: TFixed24Dot8Point);
begin
 SetLength(FData, Length(FData) + 1);
 FData[Length(FData) - 1].X := X;
 FData[Length(FData) - 1].Y := Y;
end;

procedure TGuiCustomPolygon.AddPoint(Point: TFixed24Dot8PointPoint);
begin
 SetLength(FData, Length(FData) + 1);
 FData[Length(FData) - 1].X := Point.X;
 FData[Length(FData) - 1].Y := Point.Y;
end;

function TGuiCustomPolygon.GetCount: Integer;
begin
 Result := Length(FData);
end;

function TGuiCustomPolygon.GetX(Index: Integer): TFixed24Dot8Point;
begin
 if Index < Length(FData)
  then Result := FData[Index].X
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TGuiCustomPolygon.GetY(Index: Integer): TFixed24Dot8Point;
begin
 if Index < Length(FData)
  then Result := FData[Index].Y
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGuiCustomPolygon.RemovePoint(Index: Integer);
begin
 if Index >= Length(FData)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
   begin
    if Index + 1 < Length(FData)
     then Move(FData[Index + 1], FData[Index], (Length(FData) - Index - 1) *
       SizeOf(TFixed24Dot8PointPoint));
    SetLength(FData, Length(FData) - 1);
   end;
end;

procedure TGuiCustomPolygon.SetX(Index: Integer;
  const Value: TFixed24Dot8Point);
begin
 if Index < Length(FData)
  then FData[Index].X := Value
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGuiCustomPolygon.SetY(Index: Integer;
  const Value: TFixed24Dot8Point);
begin
 if Index < Length(FData)
  then FData[Index].Y := Value
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

end.
