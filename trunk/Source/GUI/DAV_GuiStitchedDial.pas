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
  TQuantizeValueEvent = procedure(Sender: TObject; var Value: Double) of object;

  TCustomGuiStitchedDial = class(TGuiCustomStitchedControl)
  private
    FLockCursor          : Boolean;
    FMax                 : Single;
    FMin                 : Single;
    FIgnoreNextMouseMove : Boolean;
    FPosition            : Single;
    FDefaultPosition     : Single;
    FRange               : Single;
    FRangeReciprocal     : Single;

    FCurveMapping        : Single;
    FCurveMappingExp     : Single;
    FScrollRange         : Single;
    FWheelStep           : Single;
    FOnQuantizeValue     : TQuantizeValueEvent;

    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetPosition(Value: Single);
    procedure SetDefaultPosition(const Value: Single);

    function PositionToAngle: Single;
    function GetNormalizedPosition: Single;
    function GetMappedPosition: Single;
    function MapValue(Value: Double): Double;
    function UnmapValue(Value: Double): Double;
    procedure SetCurveMapping(const Value: Single);
    procedure SetNormalizedPosition(const Value: Single);
    procedure ReadMaxProperty(Reader: TReader);
    procedure WriteMaxProperty(Writer: TWriter);
  protected
    FOldMousPos   : TPoint;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure CalculateRange; virtual;
    procedure CalculateExponentialCurveMapping;
    procedure CurveMappingChanged; virtual;
    procedure DefaultPositionChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure PositionChanged; virtual;
    procedure GlyphIndexChanged; override;

    property Range: Single read FRange;
    property NormalizedPosition: Single read GetNormalizedPosition write SetNormalizedPosition;
    property MappedPosition: Single read GetMappedPosition;
  public
    constructor Create(AOwner: TComponent); override;

    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property LockCursor: Boolean read FLockCursor write FLockCursor;
    property Max: Single read FMax write SetMax;
    property Min: Single read FMin write SetMin;
    property Position: Single read FPosition write SetPosition;
    property DefaultPosition: Single read FDefaultPosition write SetDefaultPosition;
    property WheelStep: Single read FWheelStep write FWheelStep;
    property OnQuantizeValue: TQuantizeValueEvent read FOnQuantizeValue write FOnQuantizeValue;
  end;

  TGuiStichedDial = class(TCustomGuiStitchedDial)
  published
    property AutoSize;
    property CurveMapping;
    property DefaultPosition;
    property LockCursor;
    property Max;
    property Min;
    property OnChange;
    property Position;
    property StitchedImageList;
    property StitchedImageIndex;
    property Transparent;
    property WheelStep;
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
 FMin             := 0;
 FMax             := 100;
 FPosition        := 0;
 FCurveMapping    := 0;
 FCurveMappingExp := 1;
 FDefaultPosition := 0;
 FScrollRange     := 400;
 FWheelStep       := 1;
 CalculateRange;
end;

procedure TCustomGuiStitchedDial.CurveMappingChanged;
begin
 CalculateExponentialCurveMapping;
 BufferChanged;
end;

procedure TCustomGuiStitchedDial.CalculateExponentialCurveMapping;
begin
 FCurveMappingExp := Power(2, FCurveMapping);
end;

procedure TCustomGuiStitchedDial.DefaultPositionChanged;
begin
 inherited;

 // calculate matching default glyph index
 if Assigned(FStitchedItem)
  then DefaultGlyphIndex := Round(DefaultPosition * FRangeReciprocal *
    FStitchedItem.GlyphCount);
end;

procedure TCustomGuiStitchedDial.DefineProperties(Filer: TFiler);
begin
 inherited DefineProperties(Filer);
 Filer.DefineProperty('Max', ReadMaxProperty,
   WriteMaxProperty, Max = 0);
end;

function TCustomGuiStitchedDial.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  Difference : Single;
begin
 Difference := FWheelStep * WheelDelta / (120 * FScrollRange);
 NormalizedPosition := UnMapValue(MapValue(NormalizedPosition) + Difference);
 Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
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

(*
function TCustomGuiStitchedDial.CircularMouseToPosition(X, Y: Integer): Single;
var
  Range: Single;
  Angle: Single;
begin
  Range := Max - (Min - 1);
  Angle := SafeAngle(RelativeAngle(Width div 2, Height div 2, X, Y) - PointerAngles.Start);
  Result := Angle * Range / PointerAngles.Range;
  while Result > Max do Result := Result - Range;
  while Result < Min do Result := Result + Range;

  if Result > Max then Result := FPosition;
  if Result < Min then Result := FPosition;
end;
*)

procedure TCustomGuiStitchedDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if (Button = mbLeft) then
  begin
   SetFocus;
   FOldMousPos := Point(X, Y);
   Click;
  end else
 if (Button = mbRight) and (ssCtrl in Shift)
  then Position := DefaultPosition;

 if Enabled then inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGuiStitchedDial.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Pt : TPoint;
begin
 if (Button = mbLeft) then
  begin
   FOldMousPos := Point(X, Y);
   if FLockCursor and (Screen.Cursor = crNone) then
    begin
     Pt := Point(Width div 2, Height div 2);
     Pt := ClientToScreen(Pt);
     SetCursorPos(Pt.X, Pt.Y);
    end;
   Screen.Cursor := crDefault;
  end;
 if Enabled then inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomGuiStitchedDial.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Difference : Double;
  NewValue   : Double;
  Pt         : TPoint;
begin
 inherited;

 if FIgnoreNextMouseMove then
  begin
   FIgnoreNextMouseMove := False;
   Exit;
  end;

 if FLockCursor and (ssLeft in Shift)
  then Screen.Cursor := crNone;

 if (ssLeft in Shift) then
  begin
   Difference := (FOldMousPos.Y - Y) / FScrollRange;

   if ssShift in Shift
    then NewValue := MapValue(NormalizedPosition) + Difference * 0.1
    else NewValue := MapValue(NormalizedPosition) + Difference;

   NormalizedPosition := UnMapValue(NewValue);
  end else;
(*
 if (ssRight in Shift)
  then Position := CircularMouseToPosition(x, y);
*)

 if FLockCursor and (ssLeft in Shift) then
  begin
   FOldMousPos := Point(Width div 2, Height div 2);
   Pt := ClientToScreen(FOldMousPos);
   FIgnoreNextMouseMove := True;
   SetCursorPos(Pt.X, Pt.Y);
  end
 else
  begin
   FOldMousPos.X := X;
   FOldMousPos.Y := Y;
  end;
end;

procedure TCustomGuiStitchedDial.PositionChanged;
begin
 // calculate matching glyph index
 if Assigned(FStitchedItem)
  then GlyphIndex := Round(NormalizedPosition * FStitchedItem.GlyphCount);
 // GlyphIndex := Trunc(MapValue(NormalizedPosition) * FStitchedItem.GlyphCount);


 if not (csLoading in ComponentState) then
  if Assigned(FOnChange)
   then FOnChange(Self);

 BufferChanged;
end;

function TCustomGuiStitchedDial.PositionToAngle: Single;
begin
  Result := (FPosition - Min) * 360 * FRangeReciprocal;
end;

procedure TCustomGuiStitchedDial.ReadMaxProperty(Reader: TReader);
begin
 FMax := Reader.ReadFloat;
end;

procedure TCustomGuiStitchedDial.WriteMaxProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FMax);
end;

function TCustomGuiStitchedDial.GetMappedPosition: Single;
begin
 Result := MapValue(NormalizedPosition) * (Max - Min) + Min;
end;

function TCustomGuiStitchedDial.GetNormalizedPosition: Single;
begin
 if Max = Min
  then Result := Min
  else Result := (FPosition - Min) / (Max - Min);
end;

function TCustomGuiStitchedDial.MapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), FCurveMappingExp)
  else Result :=  Power(Abs(Value), FCurveMappingExp);
end;

function TCustomGuiStitchedDial.UnmapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), 1 / FCurveMappingExp)
  else Result :=  Power(Abs(Value), 1 / FCurveMappingExp)
end;

procedure TCustomGuiStitchedDial.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   CurveMappingChanged;
  end;
end;

procedure TCustomGuiStitchedDial.SetDefaultPosition(const Value: Single);
begin
 if FDefaultPosition <> Value then
  begin
   FDefaultPosition := Value;
   DefaultPositionChanged;
  end;
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

procedure TCustomGuiStitchedDial.SetNormalizedPosition(const Value: Single);
var
  NewValue : Double;
begin
 NewValue := Min + Value * (Max - Min);

 if Assigned(FOnQuantizeValue)
  then FOnQuantizeValue(Self, NewValue);

 Position := NewValue;
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
