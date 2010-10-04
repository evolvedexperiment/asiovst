unit DAV_GuiStitchedDial;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, Messages,
  {$ENDIF} Classes, Graphics, Forms, SysUtils, Controls, Contnrs,
  DAV_GuiCommon, DAV_GuiStitchedControls;

type
  TCustomGuiStitchedDial = class(TGuiCustomStitchedControl)
  private
    FLockCursor      : Boolean;
    FMax             : Single;
    FMin             : Single;
    FMouseInControl  : Boolean;
    FMouseIsDown     : Boolean;
    FPosition        : Single;
    FRange           : Single;
    FRangeReciprocal : Single;
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetPosition(Value: Single);

    function PositionToAngle: Single;
  protected
    FOldMousPos      : TPoint;
    FOnMouseEnter    : TNotifyEvent;
    FOnMouseLeave    : TNotifyEvent;

    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure CalculateRange; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure PositionChanged; virtual;
    procedure GlyphIndexChanged; override;

    property MouseInControl: Boolean read FMouseInControl;
    property Range: Single read FRange;
  public
    constructor Create(AOwner: TComponent); override;

    property LockCursor: Boolean read FLockCursor write FLockCursor;
    property Max: Single read FMax write SetMax;
    property Min: Single read FMin write SetMin;
    property Position: Single read FPosition write SetPosition;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  TGuiStichedDial = class(TCustomGuiStitchedDial)
  published
    property AutoSize;
    property DefaultGlyphIndex;
    property LockCursor;
    property Max;
    property Min;
    property OnChange;
    property Position;
    property StitchedImageIndex;
    property StitchedImageList;
    property Transparent;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

implementation

uses
  Consts, Math, DAV_Common, DAV_GuiBlend;


{ TCustomGuiStitchedDial }

constructor TCustomGuiStitchedDial.Create(AOwner: TComponent);
begin
 inherited;
 FMin := 0;
 FMax := 100;
 FPosition := 0;
 CalculateRange;
end;

procedure TCustomGuiStitchedDial.GlyphIndexChanged;
begin
 inherited;

 // calculate position for this glyph index
 if Assigned(FStitchedItem)
  then Position := GlyphIndex * FRange / FStitchedItem.GlyphCount;
end;

procedure TCustomGuiStitchedDial.MaximumChanged;
begin
 if FPosition > FMax then FPosition := FMax;
 CalculateRange;
 BufferChanged;
end;

procedure TCustomGuiStitchedDial.MinimumChanged;
begin
 if FPosition < FMin then FPosition := FMin;
 CalculateRange;
 BufferChanged;
end;

procedure TCustomGuiStitchedDial.CalculateRange;
begin
 FRange := FMax - FMin;
 if FRange <> 0
  then FRangeReciprocal := 1 / FRange
  else FRangeReciprocal := 1;
end;

procedure TCustomGuiStitchedDial.MouseEnter;
begin
 if FMouseIsDown and FLockCursor then exit;
 FMouseInControl := True;
 if Assigned(FOnMouseEnter)
  then FOnMouseEnter(Self);
end;

procedure TCustomGuiStitchedDial.MouseLeave;
begin
 if FMouseIsDown and FLockCursor then exit;
 FMouseInControl := True;
 if Assigned(FOnMouseLeave)
  then FOnMouseLeave(Self);
end;

procedure TCustomGuiStitchedDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if (Button = mbLeft) then
  begin
   SetFocus;
   FMouseIsDown := True;
   FOldMousPos.X := X; FOldMousPos.Y := Y;
   Click;
  end;
 if Enabled then inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGuiStitchedDial.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Pt : TPoint;
begin
 if (Button = mbLeft) then
  begin
   FMouseIsDown := False;
   FOldMousPos.X := X; FOldMousPos.Y := Y;
   if FLockCursor and (Screen.Cursor=crNone) then
    begin
     Pt := Point(Width div 2,Height div 2);
     Pt := ClientToScreen(Pt);
     SetCursorPos(Pt.X,Pt.Y);
    end;
   Screen.Cursor := crDefault;
  end;
 if Enabled then inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomGuiStitchedDial.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Range: Single;
begin
 inherited;
 if FLockCursor and FMouseIsDown then Screen.Cursor := crNone;
// if FMouseIsDown then Position := CircularMouseToPosition(X, Y);

 Range := Max - (Min - 1);

 if FMouseIsDown then
  begin
   if ssShift in Shift then Range := 0.1 * Range;
   if ssCtrl in Shift
    then Range := 0.0005 * Range
    else Range := 0.005 * Range;
   Position := Position + (FOldMousPos.Y - Y) * Range;
  end;

 FOldMousPos.X := X;
 FOldMousPos.Y := Y;
end;

procedure TCustomGuiStitchedDial.PositionChanged;
begin
 // calculate matching glyph index
 if Assigned(FStitchedItem)
  then GlyphIndex := Round(Position * FRangeReciprocal * FStitchedItem.GlyphCount);

 if not (csLoading in ComponentState) then
  if Assigned(FOnChange)
   then FOnChange(Self);

 BufferChanged;
end;

function TCustomGuiStitchedDial.PositionToAngle: Single;
begin
  Result := (FPosition - Min) * 360 * FRangeReciprocal;
end;

procedure TCustomGuiStitchedDial.SetMax(const Value: Single);
begin
 if Value <> FMax then
  begin
   if Value < FMin then
    if not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin + 1, MaxInt]);
   FMax := Value;
   MaximumChanged;
  end;
end;

procedure TCustomGuiStitchedDial.SetMin(const Value: Single);
begin
 if Value <> FMin then
  begin
   if Value > FMax then
    if not (csLoading in ComponentState)
     then raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMax - 1]);
   FMin := Value;
   MinimumChanged;
  end;
end;

procedure TCustomGuiStitchedDial.SetPosition(Value: Single);
begin
 Value := Limit(Value, FMin, FMax);

 if FPosition <> Value then
  begin
   FPosition := Value;
   PositionChanged;
  end;
end;

end.
