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
  DAV_GuiBaseControl;

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
    FBuffer       : TBitmap;
    FAntiAlias    : TGuiAntiAlias;
    FOSFactor     : Integer;
    FTransparent  : Boolean;
    FChartChanged : Boolean;

    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure AntiAliasChanged; virtual;
    procedure ControlChanged; override;
    procedure RenderBuffer; virtual;
    procedure TransparentChanged; virtual;
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);

    procedure RenderToBitmap(Bitmap: TBitmap); virtual;
    {$IFDEF FPC}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}

    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}

    // mouse input
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiSliderGDI = class(TCustomGuiSliderGDI)
  published
    property Align;
    property Anchors;
    property AntiAlias;
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
  Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Common, DAV_Approximations;


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


{ TCustomGuiSliderGDI }

constructor TCustomGuiSliderGDI.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FBuffer := TBitmap.Create;
 with FBuffer do
  begin
   Canvas.Brush.Color := Self.Color;
   Width := Self.Width;
   Height := Self.Height;
  end;

 FOSFactor := 1;
 FTransparent := False;
end;

destructor TCustomGuiSliderGDI.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited Destroy;
end;

procedure TCustomGuiSliderGDI.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiSliderGDI then
  with TCustomGuiSliderGDI(Dest) do
   begin
    FAntiAlias       := Self.FAntiAlias;
    FOSFactor        := Self.FOSFactor;
    FTransparent     := Self.FTransparent;

    FBuffer.Assign(Self.FBuffer);
   end;
end;

procedure TCustomGuiSliderGDI.ControlChanged;
begin
 FChartChanged := True;
 inherited;
end;

{$IFNDEF FPC}
procedure TCustomGuiSliderGDI.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TCustomGuiSliderGDI.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 inherited;
 FBuffer.Canvas.Font.Assign(Font);
end;


procedure TCustomGuiSliderGDI.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NormalizedPosition : Single;
begin
 if Button = mbLeft then
  begin
   NormalizedPosition := X / (Width - (FOSFactor + 1) div 2);
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
   NormalizedPosition := X / (Width - (FOSFactor + 1) div 2);
   Position := Limit(FMin + MapValue(NormalizedPosition) * (FMax - FMin), FMin, FMax);
  end;

 inherited;
end;


// Drawing stuff

procedure TCustomGuiSliderGDI.UpsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Upsample2xBitmap32(Bitmap);
   gaaLinear3x: Upsample3xBitmap32(Bitmap);
   gaaLinear4x: Upsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TCustomGuiSliderGDI.DownsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Downsample2xBitmap32(Bitmap);
   gaaLinear3x: Downsample3xBitmap32(Bitmap);
   gaaLinear4x: Downsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

{$IFNDEF FPC}
procedure TCustomGuiSliderGDI.DrawParentImage(Dest: TCanvas);
var
  SaveIndex : Integer;
  DC        : THandle;
  Position  : TPoint;
begin
  if Parent = nil then Exit;
  DC := Dest.Handle;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, Position);
  SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
  IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
  Parent.Perform(WM_ERASEBKGND, Longint(DC), 0);
  Parent.Perform(WM_PAINT, Longint(DC), 0);
  RestoreDC(DC, SaveIndex);
end;
{$ENDIF}

procedure TCustomGuiSliderGDI.Paint;
begin
 if Assigned(FBuffer) then
  begin
   if FChartChanged then
    begin
     FChartChanged := False;
     RenderBuffer;
    end;
   Canvas.Draw(0, 0, FBuffer);
  end;

 inherited;

 if Assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TCustomGuiSliderGDI.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TCustomGuiSliderGDI.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 ControlChanged;
end;

procedure TCustomGuiSliderGDI.RenderBuffer;
var
  Bmp: TBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    Brush.Assign(Canvas.Brush);

    case FAntiAlias of
     gaaNone:
      begin
       // draw background
       {$IFNDEF FPC}
       if FTransparent
        then DrawParentImage(FBuffer.Canvas)
        else
       {$ENDIF}
        begin
         Brush.Color := Self.Color;
         FillRect(ClipRect);
        end;
       RenderToBitmap(FBuffer);
      end;
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSFactor * FBuffer.Width;
         Height      := FOSFactor * FBuffer.Height;
         Canvas.Font.Assign(Font);
         Canvas.Font.Size := FOSFactor * Font.Size;
         {$IFNDEF FPC}
         if FTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
//           DrawParentImage(Bmp.Canvas);
           UpsampleBitmap(Bmp);
          end
         else
         {$ENDIF}
          with Canvas do
           begin
            Brush.Color := Self.Color;
            FillRect(ClipRect);
           end;
         RenderToBitmap(Bmp);
         DownsampleBitmap(Bmp);
         FBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;
    end;
    Unlock;
   end;
end;

procedure TCustomGuiSliderGDI.RenderToBitmap(Bitmap: TBitmap);
var
  Offset   : Integer;
  Scale    : Single;
  Text     : string;
  TextSize : TSize;
begin
 with Bitmap, Canvas do
  begin
   Lock;
   Offset := FOSFactor * FBorderWidth;

   Pen.Color := FSlideColor;
   Brush.Color := FSlideColor;
   Scale := (FPosition - FMin) / (FMax - FMin);
   Scale := UnmapValue(Scale);
   Rectangle(Offset, Offset, Round(Offset + Scale * (Width - 2 * Offset)), Height - Offset);

   if ShowText then
    begin
     Text := FloatToStrF(FPosition, ffGeneral, 5, 5);
     TextSize := TextExtent(Text);
     Brush.Style := bsClear;
     TextOut((Width - TextSize.cx) div 2, (Height - TextSize.cy) div 2, Text);
    end;

   if FBorderWidth > 0 then
    begin
     Pen.Color := FBorderColor;
     Pen.Width := FOSFactor * FBorderWidth;
     Brush.Style := bsClear;
     RoundRect((FOSFactor * FBorderWidth) div 2,
       (FOSFactor * FBorderWidth) div 2,
       Width - (FOSFactor * FBorderWidth) div 2,
       Height - (FOSFactor * FBorderWidth) div 2,
       FOSFactor * FBorderRadius, FOSFactor * FBorderRadius);
    end;

   Unlock;
  end;
end;

procedure TCustomGuiSliderGDI.Resize;
begin
 inherited;
 if Assigned(FBuffer) then
  with FBuffer do
   begin
    Canvas.Brush.Color := Self.Color;
    Width := Self.Width;
    Height := Self.Height;
   end;
 ControlChanged;
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
