unit DGuiLabel;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls, Consts,
  DGuiBaseControl;

type
  TCustomGuiLabel = class(TBufferedGraphicControl)
  private
    fAntiAlias     : TGuiAntiAlias;
    fAlignment     : TAlignment;
    fCaption       : string;
    fOSFactor      : Integer;
    {$IFNDEF FPC}
    fTransparent   : Boolean;
    procedure SetTransparent(Value: Boolean); virtual;
    {$ENDIF}
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
  protected
    procedure RenderLabelToBitmap(Bitmap: TBitmap); virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    property AntiAlias: TGuiAntiAlias read fAntiAlias write SetAntiAlias default gaaNone;
    property Alignment: TAlignment read fAlignment write SetAlignment default taLeftJustify;
    property Caption: string read fCaption write SetCaption;
    {$IFNDEF FPC}
    property Transparent: Boolean read fTransparent write SetTransparent default False;
    {$ENDIF}
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
    property PopupMenu;
    property ShowHint;
    property Transparent;
    property Visible;

    property OnCanResize;
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
 fAntiAlias   := gaaNone;
 fOSFactor    := 1;
 fTransparent := False;
 fAlignment   := taLeftJustify;
end;

procedure TCustomGuiLabel.RedrawBuffer(doBufferFlip: Boolean);
var
  Bmp : TBitmap;
begin
 // clear buffer
 with fBuffer.Canvas do
  begin
   Brush.Color := Self.Color;
   Font.Assign(Self.Font);
   Font.Size := fOSFactor * Self.Font.Size;
  end;

 case fAntiAlias of
  gaaNone     :
   begin
    RenderLabelToBitmap(fBuffer);
    {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
    fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);
   end;
  gaaLinear2x :
   begin
    Bmp := TBitmap.Create;
    with Bmp do
     try
      PixelFormat := pf32bit;
      Width       := fOSFactor * fBuffer.Width;
      Height      := fOSFactor * fBuffer.Height;
      Canvas.Font.Assign(fBuffer.Canvas.Font);
      Canvas.Brush.Assign(fBuffer.Canvas.Brush);
      Canvas.Pen.Assign(fBuffer.Canvas.Pen);
      {$IFNDEF FPC}
      if fTransparent then
       begin
        DrawParentImage(Bmp.Canvas);
        Upsample2xBitmap(Bmp);
       end else
      {$ENDIF}
      Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample2xBitmap(Bmp);
      fBuffer.Canvas.Draw(0, 0, Bmp);
     finally
      Free;
     end;
   end;
  gaaLinear4x :
   begin
    Bmp := TBitmap.Create;
    with Bmp do
     try
      PixelFormat := pf32bit;
      Width       := fOSFactor * fBuffer.Width;
      Height      := fOSFactor * fBuffer.Height;
      Canvas.Font.Assign(fBuffer.Canvas.Font);
      Canvas.Brush.Assign(fBuffer.Canvas.Brush);
      Canvas.Pen.Assign(fBuffer.Canvas.Pen);
      {$IFNDEF FPC}
      if fTransparent then
       begin
        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
       end else
      {$ENDIF}
      Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      fBuffer.Canvas.Draw(0, 0, Bmp);
     finally
      Free;
     end;
   end;
  gaaLinear8x :
   begin
    Bmp := TBitmap.Create;
    with Bmp do
     try
      PixelFormat := pf32bit;
      Width       := fOSFactor * fBuffer.Width;
      Height      := fOSFactor * fBuffer.Height;
      Canvas.Font.Assign(fBuffer.Canvas.Font);
      Canvas.Brush.Assign(fBuffer.Canvas.Brush);
      Canvas.Pen.Assign(fBuffer.Canvas.Pen);
      {$IFNDEF FPC}
      if fTransparent then
       begin
        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
        Upsample2xBitmap(Bmp);
       end else
      {$ENDIF}
      Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      Downsample2xBitmap(Bmp);
      fBuffer.Canvas.Draw(0, 0, Bmp);
     finally
      Free;
     end;
   end;
  gaaLinear16x :
   begin
    Bmp := TBitmap.Create;
    with Bmp do
     try
      PixelFormat := pf32bit;
      Width       := fOSFactor * fBuffer.Width;
      Height      := fOSFactor * fBuffer.Height;
      Canvas.Font.Assign(fBuffer.Canvas.Font);
      Canvas.Brush.Assign(fBuffer.Canvas.Brush);
      Canvas.Pen.Assign(fBuffer.Canvas.Pen);
      {$IFNDEF FPC}
      if fTransparent then
       begin
        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
        Upsample4xBitmap(Bmp);
       end else
      {$ENDIF}
      Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
      RenderLabelToBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      Downsample4xBitmap(Bmp);
      fBuffer.Canvas.Draw(0, 0, Bmp);
     finally
      Free;
     end;
   end;
 end;
 if doBufferFlip then Invalidate;
 inherited;
end;

procedure TCustomGuiLabel.RenderLabelToBitmap(Bitmap: TBitmap);
var
  TextSize : TSize;
begin
 with Bitmap.Canvas do
  begin
   TextSize := TextExtent(fCaption);
   case fAlignment of
     taLeftJustify : TextOut(0, 0, fCaption);
    taRightJustify : TextOut(Bitmap.Width - TextSize.cx, 0, fCaption);
          taCenter : TextOut((Bitmap.Width - TextSize.cx) div 2, 0, fCaption);
   end;
  end;
end;

procedure TCustomGuiLabel.SetAlignment(const Value: TAlignment);
begin
 if fAlignment <> Value then
  begin
   fAlignment := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLabel.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if fAntiAlias <> Value then
  begin
   fAntiAlias := Value;
   case fAntiAlias of
         gaaNone : fOSFactor :=  1;
     gaaLinear2x : fOSFactor :=  2;
     gaaLinear4x : fOSFactor :=  4;
     gaaLinear8x : fOSFactor :=  8;
    gaaLinear16x : fOSFactor := 16;
   end;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLabel.SetCaption(const Value: string);
begin
 if fCaption <> Value then
  begin
   fCaption := Value;
   RedrawBuffer(True);
  end;
end;

{$IFNDEF FPC}
procedure TCustomGuiLabel.SetTransparent(Value: Boolean);
begin
 if fTransparent <> Value then
  begin
   fTransparent := Value;
   RedrawBuffer(True);
  end;
end;
{$ENDIF}

end.
