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
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiBaseControl, DAV_GuiPixelMap, DAV_GuiFont;

type
  TCustomGuiLabel = class(TBufferedGraphicControl)
  private
    FAntiAlias   : TGuiAntiAlias;
    FAlignment   : TAlignment;
    FCaption     : string;
    FOSFactor    : Integer;
    FTransparent : Boolean;
    FShadow      : TGUIShadow;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure ShadowChangedHandler(Sender: TObject);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetShadow(const Value: TGUIShadow);
  protected
    procedure RenderLabelToBitmap(const Bitmap: TBitmap); virtual;
    procedure UpdateBuffer; override;
    procedure AlignmentChanged; virtual;
    procedure AntiAliasChanged; virtual;
    procedure CaptionChanged; virtual;
    procedure ShadowChanged; virtual;
    procedure TransparentChanged; virtual;
    procedure FontChanged; override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Shadow: TGUIShadow read FShadow write SetShadow;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiLabel = class(TCustomGuiLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AntiAlias;
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
    property Shadow;
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
    property OnPaint;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

{ TCustomGuiLabel }

constructor TCustomGuiLabel.Create(AOwner: TComponent);
begin
 inherited;
 FAntiAlias   := gaaNone;
 FOSFactor    := 1;
 FAlignment   := taLeftJustify;
 FTransparent := False;
 FShadow      := TGuiShadow.Create;
 FShadow.OnChange := ShadowChangedHandler;
end;

destructor TCustomGuiLabel.Destroy;
begin
 FreeAndNil(FShadow);
 inherited;
end;

procedure TCustomGuiLabel.FontChanged;
begin
 inherited;
 FBuffer.Canvas.Font.Assign(Self.Font);
end;

procedure TCustomGuiLabel.ShadowChanged;
begin
 Invalidate;
end;

procedure TCustomGuiLabel.ShadowChangedHandler(Sender: TObject);
begin
 ShadowChanged;
end;

procedure TCustomGuiLabel.UpdateBuffer;
var
  Bmp : TBitmap;
begin
 if [csReadingState] * ControlState <> [] then exit;

 // clear buffer
 with FBuffer.Canvas do
  begin
   Brush.Color := Self.Color;
   Font.Assign(Self.Font);
   Font.Size := FOSFactor * Self.Font.Size;
  end;

 case FAntiAlias of
  gaaNone     :
   begin
    if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else
    FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);
    RenderLabelToBitmap(FBuffer);
   end;
  gaaLinear2x:
   begin
    Bmp := TBitmap.Create;
    with Bmp, Canvas do
     try
      PixelFormat := pf32bit;
      Bmp.Width   := FOSFactor * FBuffer.Width;
      Bmp.Height  := FOSFactor * FBuffer.Height;
      Font.Assign(FBuffer.Canvas.Font);
      Brush.Assign(FBuffer.Canvas.Brush);
      Pen.Assign(FBuffer.Canvas.Pen);
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
        Upsample2xBitmap(Bmp);
       end else
      Canvas.FillRect(ClipRect);
{
      if FShadow.Visible then
       begin
        RenderLabelToBitmap(Bmp);
       end
      else
}
      RenderLabelToBitmap(Bmp);
      Downsample2xBitmap(Bmp);
      FBuffer.Canvas.Draw(0, 0, Bmp);
     finally
       FreeAndNil(Bmp);
     end;
   end;
  gaaLinear3x:
   begin
    Bmp := TBitmap.Create;
    with Bmp, Canvas do
     try
      PixelFormat := pf32bit;
      Bmp.Width   := FOSFactor * FBuffer.Width;
      Bmp.Height  := FOSFactor * FBuffer.Height;
      Font.Assign(FBuffer.Canvas.Font);
      Brush.Assign(FBuffer.Canvas.Brush);
      Pen.Assign(FBuffer.Canvas.Pen);
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
        Upsample3xBitmap(Bmp);
       end else
      Canvas.FillRect(ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample3xBitmap(Bmp);
      FBuffer.Canvas.Draw(0, 0, Bmp);
     finally
       FreeAndNil(Bmp);
     end;
   end;
  gaaLinear4x :
   begin
    Bmp := TBitmap.Create;
    with Bmp, Canvas do
     try
      PixelFormat := pf32bit;
      Bmp.Width   := FOSFactor * FBuffer.Width;
      Bmp.Height  := FOSFactor * FBuffer.Height;
      Font.Assign(FBuffer.Canvas.Font);
      Brush.Assign(FBuffer.Canvas.Brush);
      Pen.Assign(FBuffer.Canvas.Pen);
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
       end else
      FillRect(ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      FBuffer.Canvas.Draw(0, 0, Bmp);
     finally
      FreeAndNil(Bmp);
     end;
   end;
  gaaLinear8x :
   begin
    Bmp := TBitmap.Create;
    with Bmp do
     try
      PixelFormat := pf32bit;
      Width       := FOSFactor * FBuffer.Width;
      Height      := FOSFactor * FBuffer.Height;
      Canvas.Font.Assign(FBuffer.Canvas.Font);
      Canvas.Brush.Assign(FBuffer.Canvas.Brush);
      Canvas.Pen.Assign(FBuffer.Canvas.Pen);
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
        Upsample2xBitmap(Bmp);
       end else
      Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      Downsample2xBitmap(Bmp);
      FBuffer.Canvas.Draw(0, 0, Bmp);
     finally
      FreeAndNil(Bmp);
     end;
   end;
  gaaLinear16x :
   begin
    Bmp := TBitmap.Create;
    with Bmp do
     try
      PixelFormat := pf32bit;
      Width       := FOSFactor * FBuffer.Width;
      Height      := FOSFactor * FBuffer.Height;
      Canvas.Font.Assign(FBuffer.Canvas.Font);
      Canvas.Brush.Assign(FBuffer.Canvas.Brush);
      Canvas.Pen.Assign(FBuffer.Canvas.Pen);
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
        Upsample4xBitmap(Bmp);
       end else
      Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      FBuffer.Canvas.Draw(0, 0, Bmp);
     finally
      FreeAndNil(Bmp);
     end;
   end;
 end;

 inherited;
end;

procedure TCustomGuiLabel.RenderLabelToBitmap(const Bitmap: TBitmap);
var
  TextSize : TSize;
begin
 with Bitmap.Canvas do
  begin
   TextSize := TextExtent(FCaption);
   Brush.Style := bsClear;

   if FShadow.Visible then
    begin
     Font.Color := FShadow.Color;
     case FAlignment of
       taLeftJustify : TextOut(FOSFactor * FShadow.Offset.X, FOSFactor * FShadow.Offset.Y, FCaption);
      taRightJustify : TextOut(FOSFactor * FShadow.Offset.X + Bitmap.Width - TextSize.cx, FOSFactor * FShadow.Offset.Y, FCaption);
            taCenter : TextOut(FOSFactor * FShadow.Offset.X + (Bitmap.Width - TextSize.cx) div 2, FOSFactor * FShadow.Offset.Y, FCaption);
     end;
     Font.Color := Self.Font.Color;
    end;

   case FAlignment of
     taLeftJustify : TextOut(0, 0, FCaption);
    taRightJustify : TextOut(Bitmap.Width - TextSize.cx, 0, FCaption);
          taCenter : TextOut((Bitmap.Width - TextSize.cx) div 2, 0, FCaption);
   end;
  end;
end;

procedure TCustomGuiLabel.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiLabel.AlignmentChanged;
begin
 Invalidate;
end;

procedure TCustomGuiLabel.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TCustomGuiLabel.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 Invalidate;
end;

procedure TCustomGuiLabel.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   CaptionChanged;
  end;
end;

procedure TCustomGuiLabel.SetShadow(const Value: TGUIShadow);
begin
 FShadow.Assign(Value);
end;

procedure TCustomGuiLabel.CaptionChanged;
begin
 Invalidate;
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
 Invalidate;
end;

end.
