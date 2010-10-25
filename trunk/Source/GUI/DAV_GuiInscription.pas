unit DAV_GuiInscription;

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
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiFont, DAV_GuiShadow;

type
  TCustomGuiInscription = class(TGraphicControl)
  private
    FAlignment        : TAlignment;
    FCaption          : string;
    FTransparent      : Boolean;
    FFont             : TGuiCustomFont;
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBackBuffer : Boolean;
    FUpdateBuffer     : Boolean;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TGuiCustomFont);
  protected
    procedure AlignmentChanged; virtual;
    procedure ColorChanged;
    procedure CaptionChanged; virtual;
    procedure FontChanged; virtual;
    procedure TransparentChanged; virtual;

    procedure BufferChanged; virtual;
    procedure BackBufferChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure CopyParentImage(PixelMap: TGuiCustomPixelMap); virtual;

    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Font: TGuiCustomFont read FFont write SetFont;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiInscription = class(TCustomGuiInscription)
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
    property ParentFont;
    property PopupMenu;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
(*
    property OnPaint;
    property OnMouseEnter;
    property OnMouseLeave;
*)
  end;

implementation

{ TCustomGuiInscription }

constructor TCustomGuiInscription.Create(AOwner: TComponent);
begin
 inherited;
 FBuffer       := TGuiPixelMapMemory.Create;
 FBackBuffer   := TGuiPixelMapMemory.Create;
 FUpdateBuffer := False;
 ControlStyle := ControlStyle + [csOpaque];
end;

destructor TCustomGuiInscription.Destroy;
begin
 FreeAndNil(FBackBuffer);
 FreeAndNil(FBuffer);
 inherited;
end;

procedure TCustomGuiInscription.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiInscription.Resize;
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

type
  TParentControl = class(TWinControl);

procedure TCustomGuiInscription.CopyParentImage(
  PixelMap: TGuiCustomPixelMap);
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

procedure TCustomGuiInscription.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TCustomGuiInscription.UpdateBackBuffer;
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

procedure TCustomGuiInscription.UpdateBuffer;
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

 if Assigned(FFont) then
  begin
   TextSize := FFont.TextExtend(FCaption);
   case FAlignment of
    taLeftJustify  : TextSize.cx := 0;
    taRightJustify : TextSize.cx := Width - TextSize.cx;
    taCenter       : TextSize.cx := (Width - TextSize.cx) div 2;
   end;
  end;
 TextSize.cy := 0;
 FFont.TextOut(FCaption, FBuffer, TextSize.cx, TextSize.cy);
end;

procedure TCustomGuiInscription.Paint;
begin
 inherited;

 if FUpdateBackBuffer
  then UpdateBackBuffer;

 if FUpdateBuffer
  then UpdateBuffer;

 if Assigned(FBuffer)
  then FBuffer.PaintTo(Canvas);
end;

procedure TCustomGuiInscription.FontChanged;
begin
 inherited;
 if Assigned(FFont)
  then FFont.OnChange := FontChangedHandler;

 BufferChanged;
end;

procedure TCustomGuiInscription.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

procedure TCustomGuiInscription.AlignmentChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiInscription.BufferChanged;
begin
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TCustomGuiInscription.CaptionChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiInscription.ColorChanged;
begin
 if not FTransparent
  then BackBufferChanged;
end;

procedure TCustomGuiInscription.TransparentChanged;
begin
 BackBufferChanged;
end;

procedure TCustomGuiInscription.CMColorchanged(var Message: TMessage);
begin
 ColorChanged;
end;

procedure TCustomGuiInscription.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiInscription.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   CaptionChanged;
  end;
end;

procedure TCustomGuiInscription.SetFont(const Value: TGuiCustomFont);
begin
 if FFont <> Value then
  begin
   FFont := Value;
   FontChanged;
  end;
end;

procedure TCustomGuiInscription.SetTransparent(Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

end.
