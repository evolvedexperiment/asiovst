unit DAV_GuiRadioButton;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, StdCtrls, ExtCtrls,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiVectorPixelCircle,
  DAV_GuiFixedPoint, DAV_GuiFont, DAV_GuiShadow;

type
  TGuiControlsRadioButton = class(TRadioButton)
  private
    FBuffer            : TGuiCustomPixelMap;
    FBackBuffer        : TGuiCustomPixelMap;
    FRadioButtonRadius : Integer;
    FCanvas            : TCanvas;
    FUpdateBuffer      : Boolean;
    FUpdateBackBuffer  : Boolean;
    FTransparent       : Boolean;
    FFocused           : Boolean;
    FFlat              : Boolean;
    FGuiFont           : TGuiOversampledGDIFont;
    FMouseIsDown       : Boolean;
    FMouseInControl    : Boolean;
    FFlatChecked       : Boolean;
    FGroupIndex        : Integer;
    FFocusedColor      : TColor;
    FDownColor         : TColor;
    FDotColor          : TColor;
    FBorderColor       : TColor;
    FBackgroundColor   : TColor;
    FDisabledColor     : TColor;
    FOnPaint           : TNotifyEvent;
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetShadow(const Value: TGUIShadow);
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);

  protected
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TWmNoParams); message CM_TEXTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var Message: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF}); message CN_COMMAND;
    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMDesignHitTest(var Message: {$IFNDEF FPC}TCMDesignHitTest{$ELSE}TLMMouse{$ENDIF}); message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: {$IFDEF FPC}TLMMove{$ELSE}TWMMove{$ENDIF}); message WM_MOVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure DoEnter; override;
    procedure DoExit; override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetBiDiMode(Value: TBiDiMode); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure BufferChanged; virtual;
    procedure BackBufferChanged; virtual;
    procedure CalculateRadioButtonRadius; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure RenderControl(Buffer: TGuiCustomPixelMap); virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;

    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseEnter;
    procedure MouseLeave;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Color default $00E1EAEB;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default clBtnHighlight;
    property ColorDown: TColor index 1 read FDownColor write SetColors default clBtnHighlight;
    property ColorDot: TColor index 2 read FDotColor write SetColors default clWindowText;
    property ColorBorder: TColor index 3 read FBorderColor write SetColors default clWindowText;
    property ColorBackground: TColor index 4 read FBackgroundColor write SetColors default clBtnFace;
    property ColorDisabled: TColor index 5 read FDisabledColor write SetColors default clBtnShadow;
    property Flat: Boolean read FFlat write SetFlat default True;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    property Action;
    property Anchors;
    property AutoSize;
    property Caption;
    property Checked;
    property Enabled;
    property Font;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEndDock;
    property OnStartDock;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
   {$IFDEF MFC_COMPILER_4_UP}
    property BiDiMode write SetBidiMode;
   {$ENDIF}
  end;

implementation

uses
  ActnList, Math, DAV_Common, DAV_Complex, DAV_GuiBlend, DAV_Approximations;

constructor TGuiControlsRadioButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 ControlStyle  := ControlStyle + [csOpaque, csReplicatable];

 // create control canvas
 FCanvas := TControlCanvas.Create;
 TControlCanvas(FCanvas).Control := Self;

 // create buffers (& set size)
 FBuffer     := TGuiPixelMapMemory.Create;
 FBackBuffer := TGuiPixelMapMemory.Create;
 FBuffer.SetSize(Width, Height);
 FBackBuffer.SetSize(Width, Height);

 // create font
 FGuiFont          := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 ParentColor       := True;
 ParentFont        := True;
 FFocusedColor     := clBtnHighlight;
 FDownColor        := clBtnHighlight;
 FDotColor         := clWindowText;
 FBorderColor      := clWindowText;
 FBackgroundColor  := clBtnShadow;
 FDisabledColor    := clBtnHighlight;
 FFlat             := True;
 FFlatChecked      := False;
 FGroupIndex       := 0;
 Enabled           := True;
 Visible           := True;

 CalculateRadioButtonRadius;
end;

procedure TGuiControlsRadioButton.CreateParams(var Params: TCreateParams);
begin
 inherited;
 if FFlat then
  with Params do Style := (Style and not $1F) or BS_OWNERDRAW;
end;

destructor TGuiControlsRadioButton.Destroy;
begin
 FreeAndNil(FCanvas);

 // create buffers
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);

 FreeAndNil(FGuiFont);

 inherited;
end;

procedure TGuiControlsRadioButton.Loaded;
begin
 inherited;
 Resize;
end;

procedure TGuiControlsRadioButton.Resize;
begin
 inherited;
 CalculateRadioButtonRadius;
 if Assigned(FBuffer) then FBuffer.SetSize(Width, Height);
 if Assigned(FBackBuffer) then
  begin
   FBackBuffer.SetSize(Width, Height);
   BackBufferChanged;
  end;
end;

procedure TGuiControlsRadioButton.CalculateRadioButtonRadius;
begin
 FRadioButtonRadius := Round(0.5 * Min(Height,
   Abs(Font.Height * Font.PixelsPerInch / 72)));
end;

procedure TGuiControlsRadioButton.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TGuiControlsRadioButton.BufferChanged;
begin
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TGuiControlsRadioButton.CMColorchanged(var Message: TMessage);
begin
 inherited;

 if not FTransparent
  then BackBufferChanged;
end;

procedure TGuiControlsRadioButton.CMDesignHitTest(var Message: {$IFNDEF FPC}TCMDesignHitTest{$ELSE}TLMMouse{$ENDIF});
var
  LazRect : TRect;
begin
 if Alignment = taRightJustify then LazRect := Rect(ClientRect.Left  +  1, ClientRect.Top + 3, ClientRect.Left + 11, ClientRect.Top + 13);
 if Alignment = taLeftJustify  then LazRect := Rect(ClientRect.Right - 11, ClientRect.Top + 3, ClientRect.Right - 1, ClientRect.Top + 13);

 if PtInRect(LazRect, Point(message.XPos, message.YPos))
  then Message.Result := 1
  else Message.Result := 0;
end;

procedure TGuiControlsRadioButton.CMMouseEnter(var Message: TMessage);
begin
 inherited;
 MouseEnter;
end;

procedure TGuiControlsRadioButton.CMMouseLeave(var Message: TMessage);
begin
 inherited;
 MouseLeave;
end;

procedure TGuiControlsRadioButton.CMEnabledChanged(var Message: TMessage);
begin
 inherited;

 if not Enabled and FMouseInControl
  then FMouseIsDown := False;

 BufferChanged;
end;

procedure TGuiControlsRadioButton.CMFontChanged(var Message: TMessage);
begin
 FGuiFont.Font.Assign(Font);
end;

procedure TGuiControlsRadioButton.CMTextChanged(var Message: TWmNoParams);
begin
 inherited;

 if not (csLoading in ComponentState)
   then BufferChanged;
end;

procedure TGuiControlsRadioButton.MouseEnter;
begin
 if Enabled and not FMouseInControl then
  begin
   FMouseInControl := True;
   BufferChanged;
  end;
end;

procedure TGuiControlsRadioButton.MouseLeave;
begin
 if Enabled and FMouseInControl and not FMouseIsDown then
  begin
   FMouseInControl := False;
   BufferChanged;
  end;
end;

procedure TGuiControlsRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin

 with Message do
  if (CharCode = VK_SPACE) and FFocused then SetChecked(not Checked) else
  if IsAccel(CharCode, Caption) and CanFocus
   then
    begin
     SetFocus;
     Result := 1;
     BufferChanged;
    end
   else inherited;
end;

procedure TGuiControlsRadioButton.CNCommand(var Message: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF});
begin
 if Message.NotifyCode = BN_CLICKED then Click;
end;

procedure TGuiControlsRadioButton.WMSetFocus(var Message: TWMSetFocus);
begin
 inherited;
 if Enabled then FFocused := True;
end;

procedure TGuiControlsRadioButton.WMKillFocus(var Message: TWMKillFocus);
begin
 inherited;

 if Enabled then
  begin
   FMouseInControl := False;
   FFocused        := False;
  end;
end;

procedure TGuiControlsRadioButton.CMParentColorChanged(var Message: TWMNoParams);
begin
 inherited;

 if ParentColor and not FTransparent
  then UpdateBackBuffer;
end;

procedure TGuiControlsRadioButton.DoEnter;
begin
 inherited DoEnter;

 if FMouseIsDown and FMouseInControl
  then Checked := True;
 FFocused := True;
 BufferChanged;
end;

procedure TGuiControlsRadioButton.DoExit;
begin
 inherited DoExit;

 FFocused := False;
 BufferChanged;
end;

procedure TGuiControlsRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
   SetFocus;
   FMouseIsDown := True;
   inherited MouseDown(Button, Shift, X, Y);
   BufferChanged;
  end;
end;

procedure TGuiControlsRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
   FMouseIsDown := false;
   if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) and not Checked
    then Checked := True;
   inherited MouseUp(Button, Shift, X, Y);
   BufferChanged;
  end;
end;

procedure TGuiControlsRadioButton.Paint;
begin
 inherited;

 if FUpdateBackBuffer
  then UpdateBackBuffer;

 if FUpdateBuffer
  then UpdateBuffer;

 if Assigned(FOnPaint)
  then FOnPaint(Self);

 if Assigned(FBuffer)
  then FBuffer.PaintTo(Canvas);
end;

procedure TGuiControlsRadioButton.PaintWindow(DC: HDC);
var
  CC : TControlCanvas;
begin
 FCanvas.Lock;
 try
  FCanvas.Handle := DC;
  try
   {$IFNDEF FPC}
   TControlCanvas(FCanvas).UpdateTextFlags;
   {$ENDIF}
   Paint;
  finally
   FCanvas.Handle := 0;
  end;
 finally
  FCanvas.Unlock;
 end;
end;

(*
procedure TGuiControlsRadioButton.DrawRadio;
begin
 if Focused or FMouseInControl then
  if not FMouseIsDown then
   begin
    Brush.Color   := FFocusedColor;
    Pen.Color     := FBorderColor;
   end
  else
   begin
    Brush.Color   := FDownColor;
    Pen.Color     := FBorderColor;
   end
  else
   begin
    Brush.Color   := Color;
    Pen.Color     := FBorderColor;
   end;

  format := DT_WORDBREAK;
  if Alignment=taRightJustify then
   begin
    TextBounds := Rect(ClientRect.Left + 18, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
    format := format or DT_LEFT;
   end
  else if Alignment=taLeftJustify then
   begin
    TextBounds := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 16, ClientRect.Bottom - 1);
    format := format or DT_RIGHT;
   end;

  Brush.Style := bsClear;
  Font := Self.Font;
  if not Enabled then
   begin
    OffsetRect(TextBounds, 1, 1);
    Font.Color := FDisabledColor;
    DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, format);
    OffsetRect(TextBounds, -1, -1);
    Font.Color := FBackgroundColor;
    DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, format);
   end else
end;
*)

procedure TGuiControlsRadioButton.FontChangedHandler(Sender: TObject);
begin
 CalculateRadioButtonRadius;
 BufferChanged;
end;

function TGuiControlsRadioButton.GetChecked: Boolean;
begin
 if FFlat
  then Result := FFlatChecked
  else Result := inherited GetChecked;
end;

procedure TGuiControlsRadioButton.WMSize(var Message: TWMSize);
begin
 inherited;
 if FTransparent and (not (csLoading in ComponentState)) then Invalidate;
end;

procedure TGuiControlsRadioButton.WMMove(var Message: {$IFDEF FPC}TLMMove{$ELSE}TWMMove{$ENDIF});
begin
 inherited;
 if FTransparent and (not (csLoading in ComponentState)) then Invalidate;
end;

procedure TGuiControlsRadioButton.WMPaint(var Message: TWMPaint);
begin
 ControlState := ControlState + [csCustomPaint];
 inherited;
 ControlState := ControlState - [csCustomPaint];
end;

procedure TGuiControlsRadioButton.RenderControl(Buffer: TGuiCustomPixelMap);
var
  Y, X1, X2         : Integer;
  ScnLne            : PPixel32Array;
  CombColor         : TPixel32;
  BorderColor       : TPixel32;
  DotColor          : TPixel32;
  BackColor         : TPixel32;
  DrawDot           : Boolean;
  Radius            : Integer;
  InnerOffset       : Single;
  BorderWidth       : Single;
  XStart            : Single;
  Scale             : Single;
  OffsetX           : Single;
  OffsetY           : Single;
  SqrYDist          : Single;
  SqrDist           : Single;
  SqrRadMinusOne    : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  SqrRadMinusInner  : Single;
  RadMinusInnerOne  : Single;
  ReciSqrRad        : Single;
  Temp              : Single;
  CombAlpha         : Integer;
begin
 with Buffer do
  begin
   // assign local colors
   BorderColor := ConvertColor(FBorderColor);
   DotColor := ConvertColor(FDotColor);
   DrawDot := Checked or (FMouseIsDown and Enabled);
   if not Enabled then
    begin
     DotColor.A := $7F;
     BackColor := ConvertColor(FDisabledColor);
    end else
   if FMouseIsDown then
    begin
     BackColor := ConvertColor(FDownColor);
     if not Checked
      then DotColor.A := $7F;
    end else
   if FMouseInControl
    then BackColor := ConvertColor(FFocusedColor)
    else BackColor := ConvertColor(FBackgroundColor);

   BorderWidth := Max(2.5, 1 + 0.15 * FRadioButtonRadius);

   // draw circle
   Radius := FRadioButtonRadius;
   if Radius <= 0 then Exit;

   ReciSqrRad := 1 / Sqr(Radius);
   InnerOffset := 2 * BorderWidth;
   RadMinusInnerOne := BranchlessClipPositive(Radius - InnerOffset + 1);
   SqrRadMinusInner := Sqr(BranchlessClipPositive(Radius - InnerOffset));
   RadMinusBorderOne := BranchlessClipPositive(Radius - BorderWidth + 1);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - BorderWidth));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   case Alignment of
    taLeftJustify  : OffsetX := Width - Radius - 0.5;
    taRightJustify : OffsetX := Radius + 0.5;
   end;
   OffsetY := 0.5 * (Height - 1);

   for Y := Round(OffsetY - Radius) to Round(OffsetY + Radius) do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - OffsetY);

     XStart := Sqr(Radius) - SqrYDist;
     if XStart < 0
      then Continue
      else XStart := Sqrt(XStart) - 0.4999999;

     ScnLne := Scanline[Y];
     X1 := Round(OffsetX - XStart);
     X2 := Round(OffsetX + XStart);
     while X1 < X2 do
      begin
       // calculate squared distance
       SqrDist := Sqr(X1 - OffsetX) + SqrYDist;

       if SqrDist <= SqrRadMinusBorder then
        begin
         if DrawDot then
          begin
           if (SqrDist <= SqrRadMinusInner) then
            begin
             CombColor := BlendPixel(DotColor, BackColor);
             BlendPixelLine(CombColor, @ScnLne[X1], X2 - X1 + 1);
             EMMS;
             Break;
            end else
           if (SqrDist <= Sqr(RadMinusInnerOne)) then
            begin
             Scale := RadMinusInnerOne - FastSqrtBab2(SqrDist);
             CombColor := BlendPixel(DotColor, BackColor);
             CombColor := CombinePixel(CombColor, BackColor, Round(Scale * $FF));

             BlendPixelInplace(CombColor, ScnLne[X1]);
             BlendPixelInplace(CombColor, ScnLne[X2]);
             EMMS;
             Inc(X1);
             Dec(X2);
            end
           else
            begin
             BlendPixelInplace(BackColor, ScnLne[X1]);
             BlendPixelInplace(BackColor, ScnLne[X2]);
             EMMS;
             Inc(X1);
             Dec(X2);
            end;
          end
         else
          begin
           CombColor := BackColor;
           BlendPixelLine(CombColor, @ScnLne[X1], X2 - X1 + 1);
           EMMS;
           Break;
          end;
        end
       else
        begin
         if SqrDist <= Sqr(RadMinusBorderOne) then
          begin
           Scale := RadMinusBorderOne - FastSqrtBab2(SqrDist);
           CombColor := CombinePixel(BackColor, BorderColor, Round(Scale * $FF));
          end else
         if SqrDist < SqrRadMinusOne
          then CombColor := BorderColor
          else
           begin
            CombColor := BorderColor;
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            CombinePixelInplace(BackColor, CombColor, 0);
            CombColor.A := CombAlpha;
           end;

         BlendPixelInplace(CombColor, ScnLne[X1]);
         BlendPixelInplace(CombColor, ScnLne[X2]);
         EMMS;
         Inc(X1);
         Dec(X2);
        end;
      end;
    end;
  end;
end;

procedure TGuiControlsRadioButton.SetColors(Index: Integer; Value: TColor);
begin
 case Index of
  0: FFocusedColor    := Value;
  1: FDownColor       := Value;
  2: FDotColor        := Value;
  3: FBorderColor     := Value;
  4: FBackgroundColor := Value;
  5: FDisabledColor   := Value;
 end;
 Invalidate;
end;

procedure TGuiControlsRadioButton.SetFlat(const Value: Boolean);
var oldMIC : Boolean;
begin
 if FFlat <> Value then
  begin
   OldMIC          := FMouseInControl;
   FFlat           := Value;
   RecreateWnd;
   FMouseInControl := oldMIC;
  end;
end;

procedure TGuiControlsRadioButton.SetOversampling(
  const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TGuiControlsRadioButton.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TGuiControlsRadioButton.SetBiDiMode(Value: TBiDiMode);
begin
 inherited;

 if BidiMode = bdRightToLeft
  then Alignment := taLeftJustify
  else Alignment := taRightJustify;
end;

procedure TGuiControlsRadioButton.SetChecked(Value: Boolean);

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
   if Parent <> nil then
    with Parent do
     for I := 0 to ControlCount - 1 do
      begin
       Sibling := Controls[I];
       if (Sibling <> Self) and (Sibling is TGuiControlsRadioButton) then
        with TGuiControlsRadioButton(Sibling) do
         if GroupIndex = Self.GroupIndex then
          begin
           if Assigned(Action) and (Action is TCustomAction) and TCustomAction(Action).AutoCheck
            then TCustomAction(Action).Checked := False;
           SetChecked(False);
          end;
      end;
  end;

begin
 if not FFlat then
  begin
   inherited SetChecked(Value);
   Invalidate;
  end else
 if FFlatChecked <> Value then
  begin
   FFlatChecked := Value;
   TabStop := Value;
   if HandleAllocated
    then SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);

   if Value then
    begin
     TurnSiblingsOff;
     inherited Changed;
     if not ClicksDisabled then Click;
    end;
   BufferChanged;
  end;
end;

procedure TGuiControlsRadioButton.SetTransparent(const Value: Boolean);
begin
 FTransparent := Value;
 if not (csLoading in ComponentState)
  then Invalidate;
end;

function TGuiControlsRadioButton.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TGuiControlsRadioButton.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TGuiControlsRadioButton.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 FUpdateBackBuffer := False;

 {$IFNDEF FPC}
 if FTransparent then FBackBuffer.CopyParentImage(Self) else
 {$ENDIF}
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TGuiControlsRadioButton.UpdateBuffer;
var
  DataPointer : PPixel32Array;
  LineIndex   : Integer;
  TextSize    : TSize;
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

 RenderControl(FBuffer);

 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtend(Caption);
   case Alignment of
    taLeftJustify  : TextSize.cx := Width - TextSize.cx - 2 * FRadioButtonRadius - 3;
    taRightJustify : TextSize.cx := 2 * FRadioButtonRadius + 3;
   end;
   TextSize.cy := (Height - TextSize.cy) div 2;
   FGuiFont.TextOut(Caption, FBuffer, TextSize.cx, TextSize.cy);
  end;
end;

end.
