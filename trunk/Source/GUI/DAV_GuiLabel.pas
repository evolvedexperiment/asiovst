unit DAV_GuiLabel;

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
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls, StdCtrls,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiFont, DAV_GuiShadow;

type
  TCustomGuiLabel = class(TGraphicControl)
  private
    FGuiFont     : TGuiOversampledGDIFont;
    FAlignment   : TAlignment;
    FCaption     : string;
    FTransparent : Boolean;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetShadow(const Value: TGUIShadow);
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
  protected
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBackBuffer : Boolean;
    FUpdateBuffer     : Boolean;
    FOnPaint          : TNotifyEvent;

    {$IFNDEF FPC}
    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave     : TNotifyEvent;
    FOnMouseEnter     : TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ENDIF}

    procedure BufferChanged;
    procedure BackBufferChanged;
    procedure CopyParentImage(PixelMap: TGuiCustomPixelMap); virtual;
    procedure UpdateBackBuffer;
    procedure UpdateBuffer;

    procedure AlignmentChanged; virtual;
    procedure CaptionChanged; virtual;
    procedure TransparentChanged; virtual;

    procedure Loaded; override;
    procedure Resize; override;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Oversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    {$IFNDEF COMPILER10_UP}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
  end;

  TGuiLabel = class(TCustomGuiLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Oversampling;
    property ParentFont;
    property PopupMenu;
    property Shadow;
    property ShowHint;
    property Visible;
    property Transparent;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
  end;

implementation

{ TCustomGuiLabel }

constructor TCustomGuiLabel.Create(AOwner: TComponent);
begin
 inherited;
 FGuiFont      := TGuiOversampledGDIFont.Create;
 FAlignment    := taLeftJustify;
 FBuffer       := TGuiPixelMapMemory.Create;
 FBackBuffer   := TGuiPixelMapMemory.Create;
 FUpdateBuffer := False;
 ControlStyle  := ControlStyle + [csOpaque];
end;

destructor TCustomGuiLabel.Destroy;
begin
 FreeAndNil(FGuiFont);
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited;
end;

type
  TParentControl = class(TWinControl);

procedure TCustomGuiLabel.CopyParentImage(PixelMap: TGuiCustomPixelMap);
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

  {$IFNDEF FPC}
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
    PixelMap.MakeOpaque;
   finally
    Free;
   end;
  {$ENDIF}

 {$IFDEF WIN32}
 finally
   with Parent do ControlState := ControlState - [csPaintCopy];
 end;
 {$ENDIF}
end;

procedure TCustomGuiLabel.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiLabel.Resize;
begin
 if Assigned(FBuffer)
  then FBuffer.SetSize(Width, Height);

 if Assigned(FBackBuffer) then
  begin
   FBackBuffer.SetSize(Width, Height);
   UpdateBackBuffer;
  end;

 inherited;
end;

procedure TCustomGuiLabel.Paint;
begin
 inherited;

 if FUpdateBackBuffer
  then UpdateBackBuffer;

 if FUpdateBuffer
  then UpdateBuffer;

 if Assigned(FBuffer)
  then FBuffer.PaintTo(Canvas);
end;

procedure TCustomGuiLabel.BufferChanged;
begin
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TCustomGuiLabel.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TCustomGuiLabel.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 FUpdateBackBuffer := False;
 if FTransparent then CopyParentImage(FBackBuffer) else
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TCustomGuiLabel.UpdateBuffer;
var
  TextSize : TSize;
begin
 FUpdateBuffer := False;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));

 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtend(FCaption);
   case FAlignment of
    taLeftJustify  : TextSize.cx := 0;
    taRightJustify : TextSize.cx := Width - TextSize.cx;
    taCenter       : TextSize.cx := (Width - TextSize.cx) div 2;
   end;

   TextSize.cy := 0;
   FGuiFont.TextOut(FCaption, FBuffer, TextSize.cx, TextSize.cy);
  end;
end;

procedure TCustomGuiLabel.AlignmentChanged;
begin
 Invalidate;
end;

function TCustomGuiLabel.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TCustomGuiLabel.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TCustomGuiLabel.CaptionChanged;
begin
 BufferChanged;
end;

{$IFNDEF FPC}
{$IFNDEF COMPILER10_UP}
procedure TCustomGuiLabel.CMMouseEnter(var Message: TMessage);
begin
 if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomGuiLabel.CMMouseLeave(var Message: TMessage);
begin
 if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;
{$ENDIF}
{$ENDIF}

procedure TCustomGuiLabel.CMColorChanged(var Message: TMessage);
begin
 if not FTransparent
  then BackBufferChanged;
end;

procedure TCustomGuiLabel.CMFontChanged(var Message: TMessage);
begin
 FGuiFont.Font.Assign(Font);
 BufferChanged;
end;

procedure TCustomGuiLabel.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiLabel.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   CaptionChanged;
  end;
end;

procedure TCustomGuiLabel.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
 BufferChanged;
end;

procedure TCustomGuiLabel.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
 BufferChanged;
end;

procedure TCustomGuiLabel.SetTransparent(Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiLabel.TransparentChanged;
begin
 BackBufferChanged;
end;

end.
