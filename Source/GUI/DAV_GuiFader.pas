unit DAV_GuiFader;

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
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiCustomControl, DAV_GuiImageControl;

type
  TFaderOrientation = (foHorizontal, foVertical);

  TGuiCustomFader = class(TGuiCustomImageControl)
  private
    FCurveMapping       : Single;
    FCurveMappingExp    : Single;
    FDefaultValue       : Single;
    FMaximum            : Single;
    FMinimum            : Single;
    FNormalizedPosition : Single;
    FOrientation        : TFaderOrientation;
    FRange              : Single;
    FRangeReciprocal    : Single;
    FValue              : Single;
    procedure SetOrientation(const Value: TFaderOrientation);
    procedure SetMaximum(const Value: Single);
    procedure SetMinimum(const Value: Single);
    procedure SetValue(const Value: Single);
    procedure SetCurveMapping(const Value: Single);
    procedure SetDefaultValue(const Value: Single);

    procedure ReadMaxProperty(Reader: TReader);
    procedure WriteMaxProperty(Writer: TWriter);
    procedure ReadDefaultValueProperty(Reader: TReader);
    procedure WriteDefaultValueProperty(Writer: TWriter);
    procedure ReadValueProperty(Reader: TReader);
    procedure WriteValueProperty(Writer: TWriter);
    function GetNormalizedValue: Single;
  protected
    function MapNormalizedValueToNormalizedPosition(Value: Double): Double;
    function MapNormalizedPositionToNormalizedValue(Value: Double): Double;

    procedure CalculateRange; virtual;
    procedure CalculateExponentialCurveMapping; virtual;
    procedure DefineProperties(Filer: TFiler); override;

    procedure OrientationChanged; virtual;
    procedure CurveMappingChanged; virtual;
    procedure DefaultValueChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure ValueChanged; virtual;

    property Range: Single read FRange;
    property NormalizedValue: Single read GetNormalizedValue;
  public
    constructor Create(AOwner: TComponent); override;

    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property Orientation: TFaderOrientation read FOrientation write SetOrientation;
    property DefaultValue: Single read FDefaultValue write SetDefaultValue;
    property Minimum: Single read FMinimum write SetMinimum;
    property Maximum: Single read FMaximum write SetMaximum;
    property Value: Single read FValue write SetValue;
  end;

  TGuiFader = class(TGuiCustomFader)
  published
    property Orientation;
    property ImageIndex;
    property ImageList;
  end;

implementation

uses
  Math;

{ TGuiCustomFader }

constructor TGuiCustomFader.Create(AOwner: TComponent);
begin
 inherited;
 FMinimum            := 0;
 FMaximum            := 100;
 FValue              := 0;
 FDefaultValue       := 0;
 FCurveMapping       := 0;
 FCurveMappingExp    := 1;
 FNormalizedPosition := 0;
end;

procedure TGuiCustomFader.ReadDefaultValueProperty(Reader: TReader);
begin
 FDefaultValue := Reader.ReadFloat;
end;

procedure TGuiCustomFader.ReadMaxProperty(Reader: TReader);
begin
 FMaximum := Reader.ReadFloat;
end;

procedure TGuiCustomFader.ReadValueProperty(Reader: TReader);
begin
 FValue := Reader.ReadFloat;
end;

procedure TGuiCustomFader.WriteDefaultValueProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FDefaultValue);
end;

procedure TGuiCustomFader.WriteMaxProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FMaximum);
end;

procedure TGuiCustomFader.WriteValueProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FValue);
end;

procedure TGuiCustomFader.DefineProperties(Filer: TFiler);
begin
 inherited DefineProperties(Filer);
 Filer.DefineProperty('Max', ReadMaxProperty,
   WriteMaxProperty, Maximum = 0);
 Filer.DefineProperty('DefaultValue', ReadDefaultValueProperty,
   WriteDefaultValueProperty, DefaultValue = 0);
 Filer.DefineProperty('Value', ReadValueProperty,
   WriteValueProperty, Value = 0);
end;

function TGuiCustomFader.GetNormalizedValue: Single;
begin
 Result := (Value - FMinimum) * FRangeReciprocal;
end;

procedure TGuiCustomFader.CurveMappingChanged;
begin
 CalculateExponentialCurveMapping;
 BufferChanged;
end;

procedure TGuiCustomFader.DefaultValueChanged;
begin
 // nothing in here yet
end;

function TGuiCustomFader.MapNormalizedPositionToNormalizedValue(
  Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), 1 / FCurveMappingExp)
  else Result :=  Power(Abs(Value), 1 / FCurveMappingExp);
end;

function TGuiCustomFader.MapNormalizedValueToNormalizedPosition(
  Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), FCurveMappingExp)
  else Result :=  Power(Abs(Value), FCurveMappingExp);
end;

procedure TGuiCustomFader.MaximumChanged;
begin
 if FValue > FMaximum then FValue := FMaximum;
 CalculateRange;
 BufferChanged;
end;

procedure TGuiCustomFader.MinimumChanged;
begin
 if FValue < FMinimum then FValue := FMinimum;
 CalculateRange;
 BufferChanged;
end;

procedure TGuiCustomFader.OrientationChanged;
begin
 // nothing here yet
end;

procedure TGuiCustomFader.ValueChanged;
begin
 // calculate new normalized position
 FNormalizedPosition := MapNormalizedValueToNormalizedPosition(NormalizedValue);
end;

procedure TGuiCustomFader.CalculateExponentialCurveMapping;
begin
 FCurveMappingExp := Power(2, FCurveMapping);
end;

procedure TGuiCustomFader.CalculateRange;
begin
 FRange := FMaximum - FMinimum;
 if FRange <> 0
  then FRangeReciprocal := 1 / FRange
  else FRangeReciprocal := 1;
end;

procedure TGuiCustomFader.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   CurveMappingChanged;
  end;
end;

procedure TGuiCustomFader.SetDefaultValue(const Value: Single);
begin
 if FDefaultValue <> Value then
  begin
   FDefaultValue := Value;
   DefaultValueChanged;
  end;
end;

procedure TGuiCustomFader.SetMaximum(const Value: Single);
begin
 if FMaximum <> Value then
  begin
   FMaximum := Value;
   MaximumChanged;
  end;
end;

procedure TGuiCustomFader.SetMinimum(const Value: Single);
begin
 if FMinimum <> Value then
  begin
   FMinimum := Value;
   MinimumChanged;
  end;
end;

procedure TGuiCustomFader.SetOrientation(const Value: TFaderOrientation);
begin
 if FOrientation <> Value then
  begin
   FOrientation := Value;
   OrientationChanged;
  end;
end;

procedure TGuiCustomFader.SetValue(const Value: Single);
begin
 if FValue <> Value then
  begin
   FValue := Value;
   ValueChanged;
  end;
end;

end.
