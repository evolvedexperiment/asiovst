unit DAV_GuiSlider;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, DAV_GuiCommon,
  DAV_GuiPixelMap, DAV_GuiBaseControl;

type
  TSliderDirection = (sdLeftToRight);

  TCustomGuiSlider = class(TCustomControl)
  private
    FAutoColor       : Boolean;
    FBorderRadius    : Integer;
    FBorderWidth     : Integer;
    FBorderColor     : TColor;
    FSlideColor      : TColor;
    FMin, FMax       : Single;
    FDefaultPosition : Single;
    FCurveMapping    : Single;
    FCurveMappingExp : Single;
    FPosition        : Single;
    FDirection       : TSliderDirection;
    FOnChange        : TNotifyEvent;
    FOnPaint         : TNotifyEvent;
    FShowText        : Boolean;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetCurveMapping(const Value: Single);
    procedure SetDefaultPosition(Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetPosition(Value: Single);
    procedure SetSlideColor(const Value: TColor);
    procedure SetDirection(const Value: TSliderDirection);
    procedure SetShowText(const Value: Boolean);

    // floating point storage filer
    procedure ReadDefaultPositionProperty(Reader: TReader);
    procedure WriteDefaultPositionProperty(Writer: TWriter);
    procedure ReadMaxProperty(Reader: TReader);
    procedure WriteMaxProperty(Writer: TWriter);
    procedure ReadPositionProperty(Reader: TReader);
    procedure WritePositionProperty(Writer: TWriter);
  protected
    function MapValue(Value: Double): Double;
    function UnmapValue(Value: Double): Double;

    procedure AssignTo(Dest: TPersistent); override;
    procedure AutoColorChanged; virtual;
    procedure CurveMappingChanged; virtual;
    procedure DirectionChanged; virtual;
    procedure PositionChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure ControlChanged; virtual;
    procedure SlideColorChanged; virtual;
    procedure ShowTextChanged; virtual;

    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property DefaultPosition: Single read FDefaultPosition write SetDefaultPosition;
    property Direction: TSliderDirection read FDirection write SetDirection default sdLeftToRight;
    property Max: Single read FMax write SetMax;
    property Min: Single read FMin write SetMin;
    property Position: Single read FPosition write SetPosition;
    property SlideColor: TColor read FSlideColor write SetSlideColor default $303030;
    property ShowText: Boolean read FShowText write SetShowText default False;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomGuiSliderGDI = class(TCustomGuiSlider)
  private
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBuffer     : Boolean;
    FUpdateBackBuffer : Boolean;
    FTransparent      : Boolean;

    procedure SetTransparent(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure ControlChanged; override;
    procedure TransparentChanged; virtual;

    {$IFDEF FPC}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}

    {$IFNDEF FPC}
    procedure CopyParentImage(PixelMap: TGuiCustomPixelMap); virtual;
    {$ENDIF}

    procedure BackBufferChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure RenderRoundedFrameRectangle(PixelMap: TGuiCustomPixelMap);

    // mouse input
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiSliderGDI = class(TCustomGuiSliderGDI)
  published
    property Align;
    property Anchors;
    property AutoColor;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property Color;
    property Constraints;
    property CurveMapping;
    property DefaultPosition;
    property Direction;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Max;
    property Min;
    property OnChange;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property ShowText;
    property SlideColor;
    property Transparent;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
  end;

  TGuiSlider = class(TGuiSliderGDI);

implementation

uses
  Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Common, DAV_GuiBlend,
  DAV_Approximations;


{ TCustomGuiSlider }

constructor TCustomGuiSlider.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop := False; // Ensure we're not a tab-stop
 Color := clBtnFace;

 FAutoColor       := False;
 FSlideColor      := $606060;
 FBorderColor     := $202020;
 FBorderWidth     := 1;
 FDirection       := sdLeftToRight;
 FShowText        := False;

 FMin             :=   0;
 FMax             := 100;
 FDefaultPosition :=  50;
 FCurveMapping    :=   0;
 FCurveMappingExp :=   1;
 FPosition        :=  50;
end;

destructor TCustomGuiSlider.Destroy;
begin
 inherited Destroy;
end;

procedure TCustomGuiSlider.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiSlider then
  with TCustomGuiSlider(Dest) do
   begin
    FAutoColor       := Self.FAutoColor;
    FBorderRadius    := Self.FBorderRadius;
    FBorderWidth     := Self.FBorderWidth;
    FBorderColor     := Self.FBorderColor;
    FSlideColor      := Self.FSlideColor;
    FMin             := Self.FMin;
    FMax             := Self.FMax;
    FDefaultPosition := Self.FDefaultPosition;
    FCurveMapping    := Self.FCurveMapping;
    FCurveMappingExp := Self.FCurveMappingExp;
    FPosition        := Self.FPosition;
    FOnPaint         := Self.FOnPaint;
    FOnChange        := Self.FOnChange;
   end;
end;

procedure TCustomGuiSlider.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Position', ReadPositionProperty,
    WritePositionProperty, Position = 0);
  Filer.DefineProperty('DefaultPosition', ReadDefaultPositionProperty,
    WriteDefaultPositionProperty, DefaultPosition = 0);
  Filer.DefineProperty('Max', ReadMaxProperty,
    WriteMaxProperty, Max = 0);
end;

procedure TCustomGuiSlider.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiSlider.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiSlider.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
  end;
end;

procedure TCustomGuiSlider.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiSlider.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   CurveMappingChanged;
  end;
end;

procedure TCustomGuiSlider.CurveMappingChanged;
begin
 FCurveMappingExp := Power(2, FCurveMapping);
 ControlChanged;
end;

procedure TCustomGuiSlider.SetDefaultPosition(Value: Single);
begin
 if not (csLoading in ComponentState) then
  begin
   if Value < FMin then Value := FMin else
   if Value > FMax then Value := FMax;
  end;

 FDefaultPosition := Value;
end;

procedure TCustomGuiSlider.SetDirection(const Value: TSliderDirection);
begin
 if FDirection <> Value then
  begin
   FDirection := Value;
   DirectionChanged;
  end;
end;

procedure TCustomGuiSlider.DirectionChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.SetMax(const Value: Single);
begin
  if Value <> FMax then
  begin
   {$IFNDEF FPC}
   if (Value < FMin) and not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin + 1, MaxInt]);
   {$ENDIF}

   FMax := Value;
   MaximumChanged;
  end;
end;

procedure TCustomGuiSlider.SetMin(const Value: Single);
begin
  if Value <> FMin then
  begin
   {$IFNDEF FPC}
   if (Value > FMax) and not (csLoading in ComponentState) then
    raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMax - 1]);
   {$ENDIF}

   FMin := Value;
  end;
end;

procedure TCustomGuiSlider.MaximumChanged;
begin
 if FPosition > FMax then FPosition := FMax;
 if FDefaultPosition > FMax then FDefaultPosition := FMax;
 ControlChanged;
end;

procedure TCustomGuiSlider.MinimumChanged;
begin
 if FPosition < FMin then FPosition := FMin;
 if FDefaultPosition < FMin then FDefaultPosition := FMin;
 ControlChanged;
end;

procedure TCustomGuiSlider.SetPosition(Value: Single);
begin
  if Value < FMin then Value := FMin else
  if Value > FMax then Value := FMax;

  if FPosition <> Value then
   begin
    FPosition := Value;
    PositionChanged;
   end;
end;

procedure TCustomGuiSlider.PositionChanged;
begin
 if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
 ControlChanged;
end;

procedure TCustomGuiSlider.ReadDefaultPositionProperty(Reader: TReader);
begin
 FDefaultPosition := Reader.ReadFloat;
end;

procedure TCustomGuiSlider.ReadMaxProperty(Reader: TReader);
begin
 FMax := Reader.ReadFloat;
end;

procedure TCustomGuiSlider.ReadPositionProperty(Reader: TReader);
begin
 FPosition := Reader.ReadFloat;
end;

procedure TCustomGuiSlider.SetShowText(const Value: Boolean);
begin
 if FShowText <> Value then
  begin
   FShowText := Value;
   ShowTextChanged;
  end;
end;

procedure TCustomGuiSlider.ShowTextChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.SetSlideColor(const Value: TColor);
begin
 if FSlideColor <> Value then
  begin
   FSlideColor := Value;
   SlideColorChanged;
  end;
end;

procedure TCustomGuiSlider.AutoColorChanged;
begin
 if FAutoColor then
  begin
(*
   FChartColor32 := Lighten(Color32(Color),60);
   FChartColor := WinColor(FChartColor32);
*)
   ControlChanged;
  end;
end;

procedure TCustomGuiSlider.BorderWidthChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.ControlChanged;
begin
 Invalidate;
end;

procedure TCustomGuiSlider.BorderColorChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.BorderRadiusChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.SlideColorChanged;
begin
 ControlChanged;
end;

function TCustomGuiSlider.MapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(abs(Value), FCurveMappingExp)
  else Result :=  Power(abs(Value), FCurveMappingExp);
end;

function TCustomGuiSlider.UnmapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(abs(Value), 1 / FCurveMappingExp)
  else Result :=  Power(abs(Value), 1 / FCurveMappingExp)
end;

procedure TCustomGuiSlider.WriteDefaultPositionProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FDefaultPosition);
end;

procedure TCustomGuiSlider.WriteMaxProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FMax);
end;

procedure TCustomGuiSlider.WritePositionProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FPosition);
end;


{ TCustomGuiSliderGDI }

constructor TCustomGuiSliderGDI.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FBuffer     := TGuiPixelMapMemory.Create;
 FBackBuffer := TGuiPixelMapMemory.Create;
 FTransparent := False;
end;

destructor TCustomGuiSliderGDI.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited Destroy;
end;

procedure TCustomGuiSliderGDI.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiSliderGDI then
  with TCustomGuiSliderGDI(Dest) do
   begin
    FTransparent     := Self.FTransparent;
    FBuffer.Assign(Self.FBuffer);
    FBackBuffer.Assign(Self.FBackBuffer);
   end;
end;

procedure TCustomGuiSliderGDI.ControlChanged;
begin
 FUpdateBuffer := True;
 inherited;
end;

procedure TCustomGuiSliderGDI.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 ControlChanged;
end;

{$IFNDEF FPC}
procedure TCustomGuiSliderGDI.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TCustomGuiSliderGDI.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 inherited;
 // nothing here yet
end;


procedure TCustomGuiSliderGDI.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NormalizedPosition : Single;
begin
 if Button = mbLeft then
  begin
   NormalizedPosition := X / (Width - 1);
   Position := Limit(FMin + MapValue(NormalizedPosition) * (FMax - FMin), FMin, FMax);
  end;

 inherited;
end;

procedure TCustomGuiSliderGDI.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NormalizedPosition : Single;
begin
 if ssLeft in Shift then
  begin
   NormalizedPosition := X / (Width - 1);
   Position := Limit(FMin + MapValue(NormalizedPosition) * (FMax - FMin), FMin, FMax);
  end;

 inherited;
end;


// Drawing stuff

{$IFNDEF FPC}
type
  TParentControl = class(TWinControl);

procedure TCustomGuiSliderGDI.CopyParentImage(PixelMap: TGuiCustomPixelMap);
var
  I         : Integer;
  SubCount  : Integer;
  SaveIndex : Integer;
  Pnt       : TPoint;
  R, SelfR  : TRect;
  CtlR      : TRect;
  Bmp       : TBitmap;
begin
 if (Parent = nil) then Exit;
 SubCount := Parent.ControlCount;
 // set
 {$IFDEF WIN32}
 with Parent
  do ControlState := ControlState + [csPaintCopy];
 try
 {$ENDIF}

  SelfR := Bounds(Left, Top, Width, Height);
  Pnt.X := -Left;
  Pnt.Y := -Top;

  Bmp := TBitmap.Create;
  with Bmp do
   try
    Width := Self.Width;
    Height := Self.Height;
    PixelFormat := pf32bit;

    // Copy parent control image
    SaveIndex := SaveDC(Canvas.Handle);
    try
     SetViewportOrgEx(Canvas.Handle, Pnt.X, Pnt.Y, nil);
     IntersectClipRect(Canvas.Handle, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
     with TParentControl(Parent) do
      begin
       Perform(WM_ERASEBKGND, Canvas.Handle, 0);
       PaintWindow(Canvas.Handle);
      end;
    finally
     RestoreDC(Canvas.Handle, SaveIndex);
    end;

    // Copy images of graphic controls
    for I := 0 to SubCount - 1 do
     begin
      if Parent.Controls[I] = Self then Break else
       if (Parent.Controls[I] <> nil) and
          (Parent.Controls[I] is TGraphicControl)
        then
         with TGraphicControl(Parent.Controls[I]) do
          begin
           CtlR := Bounds(Left, Top, Width, Height);
           if Boolean(IntersectRect(R, SelfR, CtlR)) and Visible then
            begin
             {$IFDEF WIN32}
             ControlState := ControlState + [csPaintCopy];
             {$ENDIF}
             SaveIndex := SaveDC(Canvas.Handle);
             try
              SaveIndex := SaveDC(Canvas.Handle);
              SetViewportOrgEx(Canvas.Handle, Left + Pnt.X, Top + Pnt.Y, nil);
              IntersectClipRect(Canvas.Handle, 0, 0, Width, Height);
              Perform(WM_PAINT, Canvas.Handle, 0);
             finally
              RestoreDC(Handle, SaveIndex);
              {$IFDEF WIN32}
              ControlState := ControlState - [csPaintCopy];
              {$ENDIF}
             end;
            end;
          end;
     end;
    PixelMap.Draw(Bmp);
   finally
    Free;
   end;

 {$IFDEF WIN32}
 finally
   with Parent do ControlState := ControlState - [csPaintCopy];
 end;
 {$ENDIF}
end;
{$ENDIF}

procedure TCustomGuiSliderGDI.Paint;
begin
 if FUpdateBackBuffer
  then UpdateBackBuffer;

 if FUpdateBuffer
  then UpdateBuffer;

 if Assigned(FOnPaint)
  then FOnPaint(Self);

 if Assigned(FBuffer)
  then FBuffer.PaintTo(Canvas);

 inherited;
end;

procedure TCustomGuiSliderGDI.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 if FTransparent then CopyParentImage(FBackBuffer) else
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TCustomGuiSliderGDI.UpdateBuffer;
var
  DataPointer : PPixel32Array;
  LineIndex   : Integer;
begin
 FUpdateBuffer := False;

 inherited;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));

 RenderRoundedFrameRectangle(FBuffer);
end;

procedure TCustomGuiSliderGDI.RenderRoundedFrameRectangle(
  PixelMap: TGuiCustomPixelMap);
var
  X, Y, XPos        : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  SliderColor       : TPixel32;
  BackColor         : TPixel32;
  BorderColor       : TPixel32;
  CombColor         : TPixel32;
  Radius            : Single;
  XStart            : Single;
  BorderWidth       : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  SqrDist, SqrYDist : Single;
  SqrRadMinusOne    : Single;
  Temp              : Single;
begin
 with PixelMap do
  begin
   SliderColor := ConvertColor(FSlideColor);
   BackColor := ConvertColor(Color);
   if FBorderWidth > 0
    then BorderColor := ConvertColor(FBorderColor)
    else BorderColor := SliderColor;

   // draw circle
   Radius := FBorderRadius;
   if 0.5 * Width < Radius then Radius := 0.5 * Width;
   if 0.5 * Height < Radius then Radius := 0.5 * Height;
   BorderWidth := Math.Max(FBorderWidth, 1);

   XPos := Round(Width * UnmapValue((FPosition - FMin) / (FMax - FMin)));

   RadMinusBorderOne := BranchlessClipPositive(Radius - BorderWidth);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - BorderWidth - 1));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   for Y := 0 to Round(Radius) - 1  do
    begin
     SqrYDist := Sqr(Y - (Radius - 1));
     XStart := Sqr(Radius) - SqrYDist;
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;
     ScnLne[0] := Scanline[Y];
     ScnLne[1] := Scanline[Height - 1 - Y];

     for X := Round((Radius - 1) - XStart) to Round((Width - 1) - (Radius - 1) + XStart) do
      begin
       // calculate squared distance
       if X < (Radius - 1)
        then SqrDist := Sqr(X - (Radius - 1)) + SqrYDist else

       if X > (Width - 1) - (Radius - 1)
        then SqrDist := Sqr(X - (Width - 1) + (Radius - 1)) + SqrYDist
        else SqrDist := SqrYDist;

       if SqrDist < SqrRadMinusBorder then
        if X < XPos
         then CombColor := SliderColor
         else CombColor := BackColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         if X < XPos
          then CombColor := CombinePixel(BorderColor, SliderColor, Round($FF - Temp * $FF))
          else CombColor := CombinePixel(BorderColor, BackColor, Round($FF - Temp * $FF));
        end else
       if SqrDist < SqrRadMinusOne
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][X]);
       EMMS;
      end;
    end;

   for Y := Round(Radius) to Height - 1 - Round(Radius) do
    begin
     ScnLne[0] := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       // check whether position is a border
       if (Y < BorderWidth - 1) or (Y > Height - 1 - BorderWidth + 1)
        then CombColor := BorderColor else

       // check whether position is an upper half border
       if (Y < BorderWidth) then
        begin
         Temp := BorderWidth - Y;
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           if X < XPos
            then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
            else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           if X < XPos
            then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
            else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
          end else
         if X < XPos
          then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
          else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end else

       // check whether position is a lower half border
       if (Y > Height - 1 - BorderWidth) then
        begin
         Temp := Y - (Height - 1 - BorderWidth);
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           if X < XPos
            then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
            else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF))
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           if X < XPos
            then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
            else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
          end else
         if X < XPos
          then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
          else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         if X < XPos
          then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
          else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         if X < XPos
          then CombColor := CombinePixel(BorderColor, SliderColor, Round(Temp * $FF))
          else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end
       else
        begin
         if X < XPos
          then CombColor := SliderColor
          else CombColor := BackColor
        end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;
  end;
end;

procedure TCustomGuiSliderGDI.Resize;
begin
 inherited;

 if Assigned(FBuffer)
  then FBuffer.SetSize(Self.Width, Self.Height);

 if Assigned(FBackBuffer)
  then FBackBuffer.SetSize(Self.Width, Self.Height);

 BackBufferChanged;
end;

procedure TCustomGuiSliderGDI.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiSliderGDI.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiSliderGDI.TransparentChanged;
begin
 ControlChanged;
end;

end.
