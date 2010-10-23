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
    FNormalizedPosition  : Single;
    FIgnoreNextMouseMove : Boolean;
    FValue               : Single;
    FDefaultValue        : Single;
    FRange               : Single;
    FRangeReciprocal     : Single;

    FCurveMapping        : Single;
    FCurveMappingExp     : Single;
    FScrollRange         : Single;
    FWheelStep           : Single;
    FOnQuantizeValue     : TQuantizeValueEvent;

    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetValue(Value: Single);
    procedure SetDefaultValue(const Value: Single);

    function PositionToAngle: Single;
    function GetNormalizedValue: Single;
    function CalculateValueFromNormalizedPosition: Double;
    function MapNormalizedValueToNormalizedPosition(Value: Double): Double;
    function MapNormalizedPositionToNormalizedValue(Value: Double): Double;
    procedure SetCurveMapping(const Value: Single);
    procedure ReadMaxProperty(Reader: TReader);
    procedure WriteMaxProperty(Writer: TWriter);
  protected
    FOldMousePos : TPoint;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure CalculateRange; virtual;
    procedure CalculateExponentialCurveMapping;
    procedure CurveMappingChanged; virtual;
    procedure DefaultValueChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure ValueChanged; virtual;
    procedure ChangeDialPosition(Amount: Single);
    procedure Loaded; override;

    property Range: Single read FRange;
    property NormalizedValue: Single read GetNormalizedValue;
  public
    constructor Create(AOwner: TComponent); override;

    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property LockCursor: Boolean read FLockCursor write FLockCursor default False;
    property Max: Single read FMax write SetMax;
    property Min: Single read FMin write SetMin;
    property Value: Single read FValue write SetValue;
    property DefaultValue: Single read FDefaultValue write SetDefaultValue;
    property WheelStep: Single read FWheelStep write FWheelStep;

    property OnQuantizeValue: TQuantizeValueEvent read FOnQuantizeValue write FOnQuantizeValue;
  end;

  TGuiStitchedDial  = class(TCustomGuiStitchedDial)
  published
    property AutoSize;
    property Color;
    property CurveMapping;
    property DefaultValue;
    property LockCursor;
    property Max;
    property Min;
    property OnChange;
    property ParentColor;
    property StitchedImageIndex;
    property StitchedImageList;
    property Transparent;
    property Value;
    property WheelStep;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    {$IFDEF DELPHI8_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
  end;

implementation

uses
  Math, DAV_Common, DAV_GuiBlend;

resourcestring
  RCStrOutOfRange = 'Value must be between %d and %d';


{ TCustomGuiStitchedDial }

constructor TCustomGuiStitchedDial.Create(AOwner: TComponent);
begin
 inherited;
 FMin                 := 0;
 FMax                 := 100;
 FValue               := 0;
 FDefaultValue        := 0;
 FCurveMapping        := 0;
 FCurveMappingExp     := 1;
 FScrollRange         := 400;
 FWheelStep           := 1;
 FLockCursor          := False;
 FIgnoreNextMouseMove := False;
 FNormalizedPosition  := 0;

 CalculateRange;
end;

procedure TCustomGuiStitchedDial.ReadMaxProperty(Reader: TReader);
begin
 FMax := Reader.ReadFloat;
end;

procedure TCustomGuiStitchedDial.WriteMaxProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FMax);
end;

procedure TCustomGuiStitchedDial.DefineProperties(Filer: TFiler);
begin
 inherited DefineProperties(Filer);
 Filer.DefineProperty('Max', ReadMaxProperty,
   WriteMaxProperty, Max = 0);
end;

procedure TCustomGuiStitchedDial.CalculateExponentialCurveMapping;
begin
 FCurveMappingExp := Power(2, FCurveMapping);
end;

function TCustomGuiStitchedDial.GetNormalizedValue: Single;
begin
 Result := (Value - FMin) * FRangeReciprocal;
end;

procedure TCustomGuiStitchedDial.Loaded;
begin
 inherited;
 if Assigned(FStitchedItem) then
  begin
   FNormalizedPosition := MapNormalizedValueToNormalizedPosition(NormalizedValue);
   GlyphIndex := Round(FNormalizedPosition * (FStitchedItem.GlyphCount - 1));
  end;
end;

procedure TCustomGuiStitchedDial.CalculateRange;
begin
 FRange := FMax - FMin;
 if FRange <> 0
  then FRangeReciprocal := 1 / FRange
  else FRangeReciprocal := 1;
end;

procedure TCustomGuiStitchedDial.ChangeDialPosition(Amount: Single);
begin
 FNormalizedPosition := Limit(FNormalizedPosition + Amount, 0, 1);
 Value := CalculateValueFromNormalizedPosition;
end;

function TCustomGuiStitchedDial.PositionToAngle: Single;
begin
 Result := (FValue - Min) * 360 * FRangeReciprocal;
end;

function TCustomGuiStitchedDial.CalculateValueFromNormalizedPosition: Double;
begin
 Result := FMin +
   MapNormalizedPositionToNormalizedValue(FNormalizedPosition) * FRange;
end;

function TCustomGuiStitchedDial.MapNormalizedValueToNormalizedPosition(
  Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), FCurveMappingExp)
  else Result :=  Power(Abs(Value), FCurveMappingExp);
end;

function TCustomGuiStitchedDial.MapNormalizedPositionToNormalizedValue(
  Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), 1 / FCurveMappingExp)
  else Result :=  Power(Abs(Value), 1 / FCurveMappingExp);
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

  if Result > Max then Result := FValue;
  if Result < Min then Result := FValue;
end;
*)

function TCustomGuiStitchedDial.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
 if ssShift in Shift
  then ChangeDialPosition(0.1 * FWheelStep * WheelDelta / (120 * FScrollRange))
  else ChangeDialPosition(FWheelStep * WheelDelta / (120 * FScrollRange));
 Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TCustomGuiStitchedDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if (Button = mbLeft) then
  begin
   SetFocus;
   FOldMousePos := Point(X, Y);
   Click;
  end else
 if (Button = mbRight) and (ssCtrl in Shift)
  then Value := DefaultValue;

 if Enabled then inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGuiStitchedDial.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Pt : TPoint;
begin
 if (Button = mbLeft) then
  begin
   FOldMousePos := Point(X, Y);
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
  Pt : TPoint;
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
  if ssShift in Shift
   then ChangeDialPosition(0.1 * (FOldMousePos.Y - Y) / FScrollRange)
   else ChangeDialPosition((FOldMousePos.Y - Y) / FScrollRange)
  else
 if (ssRight in Shift)
  then; //Position := CircularMouseToPosition(x, y);

 if FLockCursor and (ssLeft in Shift) then
  begin
   FOldMousePos := Point(Width div 2, Height div 2);
   Pt := ClientToScreen(FOldMousePos);
   FIgnoreNextMouseMove := True;
   SetCursorPos(Pt.X, Pt.Y);
  end
 else
  begin
   FOldMousePos.X := X;
   FOldMousePos.Y := Y;
  end;
end;


procedure TCustomGuiStitchedDial.CurveMappingChanged;
begin
 CalculateExponentialCurveMapping;
 BufferChanged;
end;

procedure TCustomGuiStitchedDial.DefaultValueChanged;
var
  MappedNormalizedValue : Double;
begin
 inherited;

 // calculate matching default glyph index
 if Assigned(FStitchedItem) then
  begin
   MappedNormalizedValue := MapNormalizedValueToNormalizedPosition(
     (DefaultValue - FMin) * FRangeReciprocal);
   DefaultGlyphIndex := Round(MappedNormalizedValue * FStitchedItem.GlyphCount);
  end;
end;

procedure TCustomGuiStitchedDial.MaximumChanged;
begin
 if FValue > FMax then FValue := FMax;
 CalculateRange;
 BufferChanged;
end;

procedure TCustomGuiStitchedDial.MinimumChanged;
begin
 if FValue < FMin then FValue := FMin;
 CalculateRange;
 BufferChanged;
end;

procedure TCustomGuiStitchedDial.ValueChanged;
var
  NewGlyphIndex : Integer;
begin
 // calculate new normalized position
 FNormalizedPosition := MapNormalizedValueToNormalizedPosition(NormalizedValue);

 // calculate matching glyph index
 // NOTE: The glyphs are not spaced equally across the entire range
 if Assigned(FStitchedItem) then
  begin
   NewGlyphIndex := Round(FNormalizedPosition * (FStitchedItem.GlyphCount - 1));
   if NewGlyphIndex <> GlyphIndex
    then GlyphIndex := NewGlyphIndex else
   if not (csLoading in ComponentState) then
    if Assigned(FOnChange)
     then FOnChange(Self);
  end;
 BufferChanged;
end;

procedure TCustomGuiStitchedDial.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   CurveMappingChanged;
  end;
end;

procedure TCustomGuiStitchedDial.SetDefaultValue(const Value: Single);
begin
 if FDefaultValue <> Value then
  begin
   FDefaultValue := Value;
   DefaultValueChanged;
  end;
end;

procedure TCustomGuiStitchedDial.SetMax(const Value: Single);
begin
 if Value <> FMax then
  begin
   if Value < FMin then
    if not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(RCStrOutOfRange, [FMin + 1, MaxInt]);
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
     then raise EInvalidOperation.CreateFmt(RCStrOutOfRange, [-MaxInt, FMax - 1]);
   FMin := Value;
   MinimumChanged;
  end;
end;

procedure TCustomGuiStitchedDial.SetValue(Value: Single);
begin
 Value := Limit(Value, FMin, FMax);

 if FValue <> Value then
  begin
   FValue := Value;
   ValueChanged;
  end;
end;

end.
