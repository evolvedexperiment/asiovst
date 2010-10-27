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
  {$IFDEF FPC} LCLIntf, LMessages, Types, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiFont, DAV_GuiShadow, DAV_GuiFontList;

type
  TCustomGuiInscription = class(TGraphicControl)
  private
    FAlignment        : TAlignment;
    FCaption          : string;
    FTransparent      : Boolean;
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBackBuffer : Boolean;
    FUpdateBuffer     : Boolean;
    FFontList         : TGuiCustomFontList;
    function GetFontIndex: Integer;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetFontIndex(Value: Integer);
    procedure SetFontList(const Value: TGuiCustomFontList);
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetFontItem(Value: TGuiCustomFontCollectionItem);
  protected
    FFontItem      : TGuiCustomFontCollectionItem;
    FFontItemIndex : Integer;
    procedure AlignmentChanged; virtual;
    procedure ColorChanged;
    procedure CaptionChanged; virtual;
    procedure FontIndexChanged; virtual;
    procedure FontListChanged; virtual;
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

    {$IFDEF FPC}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    {$ELSE}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    {$ENDIF}
    procedure GMFontChanged(var Message: TMessage); message GM_FontChanged;
    procedure GMFontListChanged(var Message: TMessage); message GM_FontListChanged;

    property FontItem: TGuiCustomFontCollectionItem read FFontItem write SetFontItem;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property FontIndex: Integer read GetFontIndex write SetFontIndex;
    property FontList: TGuiCustomFontList read FFontList write SetFontList;
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
    property FontList;
    property FontIndex;
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
 ControlStyle  := ControlStyle + [csOpaque];
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

 if Assigned(FFontList) then
  begin
   if FFontItemIndex >= FFontList.Count then
    begin
     FontIndex := -1;
     FFontItem := nil;
     Exit;
    end;

   if FFontItemIndex >= 0
    then FontItem := FFontList[FFontItemIndex];
   FontIndexChanged;
  end;
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

 if Assigned(FFontItem) and Assigned(FFontItem.Font) then
  begin
   TextSize := FFontItem.Font.TextExtend(FCaption);
   case FAlignment of
    taLeftJustify  : TextSize.cx := 0;
    taRightJustify : TextSize.cx := Width - TextSize.cx;
    taCenter       : TextSize.cx := (Width - TextSize.cx) div 2;
   end;

   TextSize.cy := 0;
   FFontItem.Font.TextOut(FCaption, FBuffer, TextSize.cx, TextSize.cy);
  end;
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

procedure TCustomGuiInscription.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

function TCustomGuiInscription.GetFontIndex: Integer;
begin
 if Assigned(FFontItem)
  then Result := FFontItem.Index
  else Result := -1;
end;

procedure TCustomGuiInscription.GMFontChanged(var Message: TMessage);
begin
 case Message.WParam of
  0 : if Message.LParam = 0
       then FontIndex := -1
 end;
 BufferChanged;
end;

procedure TCustomGuiInscription.GMFontListChanged(var Message: TMessage);
begin
 case Message.WParam of
  0 : if Message.LParam = 0
       then FontList := nil;
  1 : BufferChanged;
 end;
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

procedure TCustomGuiInscription.FontIndexChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiInscription.FontListChanged;
begin
 FontItem := nil;
 BufferChanged;
end;

procedure TCustomGuiInscription.TransparentChanged;
begin
 BackBufferChanged;
end;

procedure TCustomGuiInscription.CMColorchanged(var Message: {$IFDEF FPC}TLMessage {$ELSE}TMessage {$ENDIF});
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

procedure TCustomGuiInscription.SetFontIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FFontItemIndex := Value;
   Exit;
  end;

 // check if Font image list is available
 if Assigned(FFontList) then
  begin
   // limit range to existing Font images (or -1 for nothing)
   if Value < 0 then Value := -1 else
   if Value >= FFontList.Count then Value := FFontList.Count - 1;

   if FontIndex <> Value then
    begin
     FFontItemIndex := Value;

     if Value > -1
      then FontItem := FFontList[Value]
      else FontItem := nil;

     FontIndexChanged;
    end;
  end;
end;

procedure TCustomGuiInscription.SetFontItem(
  Value: TGuiCustomFontCollectionItem);
begin
 if FFontItem <> Value then
  begin
   if not Assigned(Value) then
    begin
     Value := FFontItem;
     FFontItem := nil;
     Value.UnlinkControl(Self);
    end
   else
    begin
     if Assigned(FFontItem)
      then FFontItem.UnLinkControl(Self);
     FFontItem := Value;
     FFontItem.LinkControl(Self);
    end;
  end;
end;

procedure TCustomGuiInscription.SetFontList(const Value: TGuiCustomFontList);
begin
 if FFontList <> Value then
  begin
   // check whether a list is linked at all
   if not Assigned(Value) then
    begin
     Assert(Assigned(FFontList));
     FontItem := nil;
     FFontList.UnLinkControl(Self);
     FFontList := nil;
    end
   else
    begin
     Assert(Assigned(Value));
     FFontList := Value;
     FFontList.LinkControl(Self);
    end;
   FontListChanged;
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
