unit DAV_GuiMediaButton;

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
  DAV_GuiBaseControl, DAV_GuiPixelMap;

type
  TMediaButtonState = (mbsPlay, mbsPause, mbsStop, mbsRecord, mbsFastBackward,
    mbsFastForward, mbsPrevious, mbsNext);

  TCustomGuiMediaButton = class(TCustomControl)
  private
    FBorderRadius : Integer;
    FBorderWidth  : Integer;
    FBorderColor  : TColor;
    FButtonColor  : TColor;
    FButtonState  : TMediaButtonState;
    FGlyphOffset  : Integer;
    FOnPaint      : TNotifyEvent;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonState(const Value: TMediaButtonState);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure ButtonColorChanged; virtual;
    procedure ButtonStateChanged; virtual;
    procedure ControlChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default $303030;
    property ButtonState: TMediaButtonState read FButtonState write SetButtonState default mbsPlay;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiMediaButton = class(TCustomGuiMediaButton)
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

    procedure BackBufferChanged; virtual;
    procedure BufferChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure ControlChanged; override;
    procedure RenderButton(PixelMap: TGuiCustomPixelMap);
    procedure RenderGlyph(PixelMap: TGuiCustomPixelMap);
    procedure TransparentChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property ButtonColor;
    property ButtonState;

    property OnPaint;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
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
  Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Common, DAV_GuiBlend,
  DAV_Approximations;


{ TCustomGuiMediaButton }

constructor TCustomGuiMediaButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop := False; // Ensure we're not a tab-stop
 Color := clBtnFace;

 FButtonColor     := $606060;
 FBorderColor     := $202020;
 FBorderWidth     := 1;
 FGlyphOffset     := 3;
 FButtonState     := mbsPlay;
 FBorderRadius    := 0;
end;

destructor TCustomGuiMediaButton.Destroy;
begin
 inherited Destroy;
end;

procedure TCustomGuiMediaButton.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiMediaButton then
  with TCustomGuiMediaButton(Dest) do
   begin
    FBorderRadius := Self.FBorderRadius;
    FBorderWidth  := Self.FBorderWidth;
    FBorderColor  := Self.FBorderColor;
    FButtonColor  := Self.FButtonColor;
    FButtonState  := Self.FButtonState;
    FOnPaint      := Self.FOnPaint;
   end;
end;

procedure TCustomGuiMediaButton.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetButtonState(const Value: TMediaButtonState);
begin
 if FButtonState <> Value then
  begin
   FButtonState := Value;
   ButtonStateChanged;
  end;
end;

procedure TCustomGuiMediaButton.ButtonStateChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   ButtonColorChanged;
  end;
end;

procedure TCustomGuiMediaButton.BorderWidthChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.ControlChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMediaButton.BorderColorChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.BorderRadiusChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.ButtonColorChanged;
begin
 ControlChanged;
end;


{ TGuiMediaButton }

constructor TGuiMediaButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FBuffer := TGuiPixelMapMemory.Create;
 FBackBuffer := TGuiPixelMapMemory.Create;
 FTransparent := False;
end;

destructor TGuiMediaButton.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited Destroy;
end;

procedure TGuiMediaButton.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiMediaButton then
  with TGuiMediaButton(Dest) do
   begin
    FTransparent := Self.FTransparent;
    FBuffer.Assign(Self.FBuffer);
    FBackBuffer.Assign(Self.FBackBuffer);
   end;
end;

procedure TGuiMediaButton.ControlChanged;
begin
 inherited;
end;

{$IFNDEF FPC}
type
  TParentControl = class(TWinControl);

procedure TGuiMediaButton.CopyParentImage(PixelMap: TGuiCustomPixelMap);
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

procedure TGuiMediaButton.Paint;
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

procedure TGuiMediaButton.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 ControlChanged;
end;

procedure TGuiMediaButton.BufferChanged;
begin
 FUpdateBuffer := True;
 ControlChanged;
end;

procedure TGuiMediaButton.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 if FTransparent then FBackBuffer.CopyParentImage(Self) else
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TGuiMediaButton.UpdateBuffer;
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

 RenderButton(FBuffer);
 RenderGlyph(FBuffer);
end;

procedure TGuiMediaButton.RenderButton(PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  ButtonColor        : TPixel32;
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
   ButtonColor := ConvertColor(FButtonColor);
   if FBorderWidth > 0
    then BorderColor := ConvertColor(FBorderColor)
    else BorderColor := ButtonColor;

   // draw circle
   Radius := Min(Min(FBorderRadius, 0.5 * Width), 0.5 * Height) + 1;
   BorderWidth := Max(FBorderWidth, 1);

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

       if SqrDist < SqrRadMinusBorder
        then CombColor := ButtonColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombinePixel(BorderColor, ButtonColor, Round($FF - Temp * $FF));
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
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
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
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end
       else CombColor := ButtonColor;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;
  end;
end;

procedure TGuiMediaButton.RenderGlyph(PixelMap: TGuiCustomPixelMap);
begin
 with PixelMap do
  begin
   case FButtonState of
    mbsPlay :
     begin
(*
      Brush.Color := clLime;
      Polygon([Point((FBorderWidth + FGlyphOffset), (FBorderWidth + FGlyphOffset)),
               Point(Width - (FBorderWidth + FGlyphOffset), Height div 2),
               Point((FBorderWidth + FGlyphOffset), Height - (FBorderWidth + FGlyphOffset))]);
*)
     end;
    mbsPause :
     begin
      FillRect(FBorderWidth + FGlyphOffset, FBorderWidth + FGlyphOffset,
        Width div 2 - ((FGlyphOffset + FBorderWidth) div 2),
        Height - (FBorderWidth + FGlyphOffset), pxGray32);

      FillRect(Width div 2 + ((FGlyphOffset + FBorderWidth) div 2),
        FBorderWidth + FGlyphOffset, Width - FBorderWidth - FGlyphOffset,
        Height - FBorderWidth - FGlyphOffset, pxGray32);
     end;
    mbsStop :
     begin
      FillRect(FBorderWidth + FGlyphOffset, FBorderWidth + FGlyphOffset,
        Width  - (FBorderWidth + FGlyphOffset),
        Height - (FBorderWidth + FGlyphOffset), pxBlack32);
     end;

   end;

  end;
end;

procedure TGuiMediaButton.Resize;
begin
 inherited;

 if Assigned(FBuffer)
  then FBuffer.SetSize(Self.Width, Self.Height);

 if Assigned(FBackBuffer)
  then FBackBuffer.SetSize(Self.Width, Self.Height);

 BackBufferChanged;
end;

procedure TGuiMediaButton.Loaded;
begin
 inherited;
 Resize;
end;

procedure TGuiMediaButton.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiMediaButton.TransparentChanged;
begin
 BufferChanged;
end;

end.
