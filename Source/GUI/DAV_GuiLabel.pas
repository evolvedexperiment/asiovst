unit DAV_GuiLabel;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiBaseControl;

type
  TCustomGuiLabel = class(TBufferedGraphicControl)
  private
    FAntiAlias     : TGuiAntiAlias;
    FAlignment     : TAlignment;
    FCaption       : string;
    FOSFactor      : Integer;
    {$IFNDEF FPC}
    FTransparent   : Boolean;
    FShadow        : TGUIShadow;
    procedure SetTransparent(Value: Boolean); virtual;
    {$ENDIF}
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure ShadowChangedHandler(Sender: TObject);
    procedure ShadowChanged;
  protected
    procedure RenderLabelToBitmap(const Bitmap: TBitmap); virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    {$IFNDEF FPC}
    property Shadow: TGUIShadow read FShadow write FShadow;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
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
//    property Shadow;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property Transparent;
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
 {$IFNDEF FPC}
 FTransparent := False;
 FShadow      := TGuiShadow.Create;
 FShadow.OnChange := ShadowChangedHandler;
 {$ENDIF}
end;

destructor TCustomGuiLabel.Destroy;
begin
 {$IFNDEF FPC}
 FreeAndNil(FShadow);
 {$ENDIF}
 inherited;
end;

procedure TCustomGuiLabel.ShadowChanged;
begin
 RedrawBuffer(True);
end;

procedure TCustomGuiLabel.ShadowChangedHandler(Sender: TObject);
begin
 ShadowChanged;
end;

procedure TCustomGuiLabel.RedrawBuffer(doBufferFlip: Boolean);
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
    {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
    FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);
    RenderLabelToBitmap(FBuffer);
   end;
  gaaLinear2x :
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
      {$IFNDEF FPC}
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample2xBitmap(Bmp);
       end else
      {$ENDIF}
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
  gaaLinear3x :
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
      {$IFNDEF FPC}
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample3xBitmap(Bmp);
       end else
      {$ENDIF}
      FillRect(ClipRect);
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
      {$IFNDEF FPC}
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
       end else
      {$ENDIF}
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
      {$IFNDEF FPC}
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
        Upsample2xBitmap(Bmp);
       end else
      {$ENDIF}
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
      {$IFNDEF FPC}
      if FTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
        Upsample4xBitmap(Bmp);
       end else
      {$ENDIF}
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
 if doBufferFlip then Invalidate;
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
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLabel.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   case FAntiAlias of
         gaaNone : FOSFactor :=  1;
     gaaLinear2x : FOSFactor :=  2;
     gaaLinear3x : FOSFactor :=  3;
     gaaLinear4x : FOSFactor :=  4;
     gaaLinear8x : FOSFactor :=  8;
    gaaLinear16x : FOSFactor := 16;
   end;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLabel.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   RedrawBuffer(True);
  end;
end;

{$IFNDEF FPC}
procedure TCustomGuiLabel.SetTransparent(Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   RedrawBuffer(True);
  end;
end;
{$ENDIF}

end.
