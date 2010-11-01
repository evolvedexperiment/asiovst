unit DAV_GuiEQSlide;

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
  DAV_GuiBaseControl, DAV_GuiCustomControl;

type
  TGuiEQSlide = class;

  TGetColorEvent = function(Sender: TObject; const Frequency: Single): TColor of object;

  TCustomGuiEQSlide = class;

  TCustomGuiEQSlideAxis = class(TPersistent)
  protected
    FOwner        : TCustomGuiEQSlide;
    FUpper        : Single;
    FLower        : Single;
    FRange        : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
    procedure CalculateRange;
    procedure RangeChanged; virtual;
  public
    constructor Create(AOwner: TCustomGuiEQSlide); virtual;
    property Range: Single read FRange;
  end;

  // X-Axis

  TCustomGuiEQSlideXAxis = class(TCustomGuiEQSlideAxis)
  private
    FInvUpper       : Single;
    FInvLower       : Single;
    FLog2Ratio      : Single;
    FInvLog2Ratio   : Single;
    procedure SetLowerFrequency(const Value: Single);
    procedure SetUpperFrequency(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure LowerFrequencyChanged; virtual;
    procedure UpperFrequencyChanged; virtual;

    procedure CalculateLowerFrequencyReciprocal;
    procedure CalculateUpperFrequencyReciprocal;
    procedure CalculateFrequencyRangeRatios;
  public
    constructor Create(AOwner: TCustomGuiEQSlide); override;

    // conversion between logarithmic frequency and linear
    function LinearToLogarithmicFrequency(Value: Double): Double;
    function LogarithmicFrequencyToLinear(Value: Double): Double;

    // conversion between linear and logarithmic frequency
    function FastLinearToLogarithmicFrequency(Value: Single): Single;
    function FastLogarithmicFrequencyToLinear(Value: Single): Single;

    property UpperFrequency: Single read FUpper write SetUpperFrequency;
    property LowerFrequency: Single read FLower write SetLowerFrequency;
  end;

  TGuiEQSlideXAxis = class(TCustomGuiEQSlideXAxis)
  published
    property UpperFrequency;
    property LowerFrequency;
  end;



  // EQ-Slide

  TCustomGuiEQSlide = class(TCustomControl)
  private
    FAutoColor    : Boolean;
    FBorderRadius : Integer;
    FBorderWidth  : Integer;
    FBorderColor  : TColor;
    FSlideColor   : TColor;
    FXAxis        : TGuiEQSlideXAxis;
    FOnPaint      : TNotifyEvent;
    FOnGetColor   : TGetColorEvent;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetSlideColor(const Value: TColor);
    procedure SetXAxis(const Value: TGuiEQSlideXAxis);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AutoColorChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure SlideColorChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ChartChanged; virtual;

    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property SlideColor: TColor read FSlideColor write SetSlideColor default $303030;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;

    property XAxis: TGuiEQSlideXAxis read FXAxis write SetXAxis;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnGetColor: TGetColorEvent read FOnGetColor write FOnGetColor;
  end;

  TGuiEQSlide = class(TCustomGuiEQSlide)
  private
    FBuffer           : TBitmap;
    FAntiAlias        : TGuiAntiAlias;
    FOSFactor         : Integer;
    FTransparent      : Boolean;
    FChartChanged     : Boolean;

    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure AntiAliasChanged; virtual;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ChartChanged; override;
  published
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property AutoColor;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property SlideColor;
    property XAxis;

    property OnGetColor;
    property OnPaint;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
  end;

implementation

uses
  Math, DAV_Common, DAV_Approximations;

{ TCustomGuiEQSlideAxis }

constructor TCustomGuiEQSlideAxis.Create(AOwner: TCustomGuiEQSlide);
begin
 FOwner := AOwner;
end;

procedure TCustomGuiEQSlideAxis.Changed;
begin
 FOwner.ChartChanged;
end;

procedure TCustomGuiEQSlideAxis.RangeChanged;
begin
 CalculateRange;
end;

procedure TCustomGuiEQSlideAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQSlideAxis then
  with TCustomGuiEQSlideAxis(Dest) do
   begin
    FOwner        := Self.FOwner;
    FUpper        := Self.FUpper;
    FLower        := Self.FLower;
    FRange        := Self.FRange;
   end
 else inherited;
end;

procedure TCustomGuiEQSlideAxis.CalculateRange;
begin
 FRange := FUpper - FLower;
end;


{ TCustomGuiEQSlideXAxis }

constructor TCustomGuiEQSlideXAxis.Create(AOwner: TCustomGuiEQSlide);
begin
 inherited;
 FLower := 20;
 FUpper := 20000;
 CalculateUpperFrequencyReciprocal;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
end;

procedure TCustomGuiEQSlideXAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQSlideXAxis then
  with TCustomGuiEQSlideXAxis(Dest) do
   begin
    inherited;
    FInvUpper     := Self.FInvUpper;
    FInvLower     := Self.FInvLower;
    FLog2Ratio    := Self.FLog2Ratio;
    FInvLog2Ratio := Self.FInvLog2Ratio;
   end
 else inherited;
end;

function TCustomGuiEQSlideXAxis.LogarithmicFrequencyToLinear(Value: Double): Double;
begin
 Result := Log2(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQSlideXAxis.LinearToLogarithmicFrequency(Value: Double): Double;
begin
 Result := Power(2, Value * FLog2Ratio) * FLower;
end;

function TCustomGuiEQSlideXAxis.FastLogarithmicFrequencyToLinear(Value: Single): Single;
begin
 Result := FastLog2MinError3(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQSlideXAxis.FastLinearToLogarithmicFrequency(Value: Single): Single;
begin
 Result := FastPower2MinError3(Value * FLog2Ratio) * FLower;
end;

procedure TCustomGuiEQSlideXAxis.SetLowerFrequency(const Value: Single);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerFrequencyChanged;
  end;
end;

procedure TCustomGuiEQSlideXAxis.SetUpperFrequency(const Value: Single);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperFrequencyChanged;
  end;
end;

procedure TCustomGuiEQSlideXAxis.UpperFrequencyChanged;
begin
 RangeChanged;
 CalculateUpperFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQSlideXAxis.LowerFrequencyChanged;
begin
 RangeChanged;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQSlideXAxis.CalculateUpperFrequencyReciprocal;
begin
 Assert(FUpper <> 0);

 // calculate reciprocal of upper frequency
 FInvUpper := 1 / FUpper;
end;

procedure TCustomGuiEQSlideXAxis.CalculateLowerFrequencyReciprocal;
begin
 Assert(FLower <> 0);

 // calculate reciprocal of lower frequency
 FInvLower := 1 / FLower;
end;

procedure TCustomGuiEQSlideXAxis.CalculateFrequencyRangeRatios;
begin
 Assert(FUpper <> 0);
 Assert(FInvLower <> 0);

 // calculate lograithmic frequency ratio (as new logarithm base)
 FLog2Ratio := Log2(FUpper * FInvLower);
 FInvLog2Ratio := 1 / FLog2Ratio;
end;


{ TCustomGuiEQSlide }

constructor TCustomGuiEQSlide.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop := False; // Ensure we're not a tab-stop
 Color := clBtnFace;

 FXAxis           := TGuiEQSlideXAxis.Create(Self);

 FAutoColor       := False;
 FSlideColor      := $606060;
 FBorderColor     := $202020;
 FBorderWidth     := 1;
end;

destructor TCustomGuiEQSlide.Destroy;
begin
 FreeAndNil(FXAxis);
 inherited Destroy;
end;

procedure TCustomGuiEQSlide.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiEQSlide then
  with TCustomGuiEQSlide(Dest) do
   begin
    FAutoColor    := Self.FAutoColor;
    FBorderRadius := Self.FBorderRadius;
    FBorderWidth  := Self.FBorderWidth;
    FBorderColor  := Self.FBorderColor;
    FSlideColor   := Self.FSlideColor;
    FOnPaint      := Self.FOnPaint;
    FOnGetColor   := Self.FOnGetColor;

    FXAxis.Assign(Self.FXAxis);
   end;
end;

procedure TCustomGuiEQSlide.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiEQSlide.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiEQSlide.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
  end;
end;

procedure TCustomGuiEQSlide.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiEQSlide.SetSlideColor(const Value: TColor);
begin
 if FSlideColor <> Value then
  begin
   FSlideColor := Value;
   SlideColorChanged;
  end;
end;

procedure TCustomGuiEQSlide.AutoColorChanged;
begin
 if FAutoColor then
  begin
(*
   FChartColor32 := Lighten(Color32(Color),60);
   FChartColor := WinColor(FChartColor32);
*)
   ChartChanged;
  end;
end;

procedure TCustomGuiEQSlide.BorderWidthChanged;
begin
 ChartChanged;
end;

procedure TCustomGuiEQSlide.SetXAxis(const Value: TGuiEQSlideXAxis);
begin
 FXAxis.Assign(Value);
end;

procedure TCustomGuiEQSlide.ChartChanged;
begin
 Invalidate;
end;

procedure TCustomGuiEQSlide.BorderColorChanged;
begin
 ChartChanged;
end;

procedure TCustomGuiEQSlide.BorderRadiusChanged;
begin
 ChartChanged;
end;

procedure TCustomGuiEQSlide.SlideColorChanged;
begin
 ChartChanged;
end;


{ TGuiEQSlide }

constructor TGuiEQSlide.Create(AOwner: TComponent);
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

destructor TGuiEQSlide.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited Destroy;
end;

procedure TGuiEQSlide.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiEQSlide then
  with TGuiEQSlide(Dest) do
   begin
    FAntiAlias       := Self.FAntiAlias;
    FOSFactor        := Self.FOSFactor;
    FTransparent     := Self.FTransparent;

    FBuffer.Assign(Self.FBuffer);
   end;
end;

procedure TGuiEQSlide.ChartChanged;
begin
 FChartChanged := True;
 inherited;
end;

{$IFNDEF FPC}
procedure TGuiEQSlide.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TGuiEQSlide.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 inherited;
 FBuffer.Canvas.Font.Assign(Font);
end;


// Drawing stuff

procedure TGuiEQSlide.UpsampleBitmap(Bitmap: TBitmap);
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

procedure TGuiEQSlide.DownsampleBitmap(Bitmap: TBitmap);
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
procedure TGuiEQSlide.DrawParentImage(Dest: TCanvas);
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

procedure TGuiEQSlide.Paint;
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

procedure TGuiEQSlide.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TGuiEQSlide.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 ChartChanged;
end;

procedure TGuiEQSlide.RenderBuffer;
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

procedure TGuiEQSlide.RenderToBitmap(Bitmap: TBitmap);
var
  PixelIndex  : Integer;
  Offset      : Integer;
  Temp        : Single;
begin
 with Bitmap, Canvas do
  begin
   Lock;
   Offset := FOSFactor * FBorderWidth;

   if Assigned(FOnGetColor) then
    begin
     Pen.Color := FOnGetColor(Self, FXAxis.LowerFrequency);
     MoveTo(FOSFactor,          Offset);
     LineTo(FOSFactor, Height - Offset);
     Temp := 1 / (Width - 2 * FOSFactor);
     for PixelIndex := FOSFactor + 1 to Width - FOSFactor - 1 do
      begin
       Pen.Color := FOnGetColor(Self, FXAxis.LinearToLogarithmicFrequency((PixelIndex - FOSFactor) * Temp));
       MoveTo(PixelIndex,          Offset);
       LineTo(PixelIndex, Height - Offset);
      end;
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

procedure TGuiEQSlide.Resize;
begin
 inherited;
 if Assigned(FBuffer) then
  with FBuffer do
   begin
    Canvas.Brush.Color := Self.Color;
    Width := Self.Width;
    Height := Self.Height;
   end;
 ChartChanged;
end;

procedure TGuiEQSlide.Loaded;
begin
 inherited;
 Resize;
end;

procedure TGuiEQSlide.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiEQSlide.TransparentChanged;
begin
 ChartChanged;
end;

end.
