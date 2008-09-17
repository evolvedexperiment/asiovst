unit DAV_GuiLabel;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls, Consts,
  DAV_GuiBaseControl;

type
  TCustomGuiLabel = class(TBufferedGraphicControl)
  private
    fAntiAlias     : TGuiAntiAlias;
    fAlignment     : TAlignment;
    fCaption       : string;
    fOSFactor      : Integer;
    {$IFNDEF FPC}
    fTransparent   : Boolean;
    fShadow        : TGUIShadow;
    procedure SetTransparent(Value: Boolean); virtual;
    {$ENDIF}
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure ShadowChangedHandler(Sender: TObject);
    procedure ShadowChanged;
  protected
    procedure RenderLabelToBitmap(Bitmap: TBitmap); virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    property AntiAlias: TGuiAntiAlias read fAntiAlias write SetAntiAlias default gaaNone;
    property Alignment: TAlignment read fAlignment write SetAlignment default taLeftJustify;
    property Caption: string read fCaption write SetCaption;
    property Shadow: TGUIShadow read fShadow write fShadow;
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
//    property Shadow;
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
 fShadow      := TGuiShadow.Create;
 fShadow.OnChange := ShadowChangedHandler;
end;

destructor TCustomGuiLabel.Destroy;
begin
 FreeAndNil(fShadow);
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

type
  TParentControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count,
  SaveIndex  : Integer;
  DC         : HDC;
  Pnt        : TPoint;
  R, SelfR,
  CtlR       : TRect;
begin
 if (Control = nil) or (Control.Parent = nil) then Exit;
 Count := Control.Parent.ControlCount;
 DC := Dest.Handle;
{$IFDEF WIN32}
 with Control.Parent do ControlState := ControlState + [csPaintCopy];
 try
{$ENDIF}
   with Control do
    begin
     SelfR := Bounds(Left, Top, Width, Height);
     Pnt.X := -Left; Pnt.Y := -Top;
    end;
   { Copy parent control image }
   SaveIndex := SaveDC(DC);
   try
    SetViewportOrgEx(DC, Pnt.X, Pnt.Y, nil);
    IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth,
       Control.Parent.ClientHeight);
    with TParentControl(Control.Parent) do
     begin
      Perform(WM_ERASEBKGND, DC, 0);
      PaintWindow(DC);
     end;
   finally
    RestoreDC(DC, SaveIndex);
   end;
   { Copy images of graphic controls }
   for I := 0 to Count - 1 do
    begin
     if Control.Parent.Controls[I] = Control then Break else
      if (Control.Parent.Controls[I] <> nil) and
         (Control.Parent.Controls[I] is TGraphicControl)
       then
        with TGraphicControl(Control.Parent.Controls[I]) do
         begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
           begin
            {$IFDEF WIN32}
            ControlState := ControlState + [csPaintCopy];
            {$ENDIF}
            SaveIndex := SaveDC(DC);
            try
             SaveIndex := SaveDC(DC);
             SetViewportOrgEx(DC, Left + Pnt.X, Top + Pnt.Y, nil);
             IntersectClipRect(DC, 0, 0, Width, Height);
             Perform(WM_PAINT, DC, 0);
            finally
             RestoreDC(DC, SaveIndex);
             {$IFDEF WIN32}
             ControlState := ControlState - [csPaintCopy];
             {$ENDIF}
            end;
           end;
         end;
    end;
{$IFDEF WIN32}
 finally
   with Control.Parent do ControlState := ControlState - [csPaintCopy];
 end;
{$ENDIF}
end;

procedure TCustomGuiLabel.RedrawBuffer(doBufferFlip: Boolean);
var
  Bmp : TBitmap;
begin
 if [csReadingState] * ControlState <> [] then exit;

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
    {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
    fBuffer.Canvas.FillRect(fBuffer.Canvas.ClipRect);
    RenderLabelToBitmap(fBuffer);
   end;
  gaaLinear2x :
   begin
    Bmp := TBitmap.Create;
    with Bmp, Canvas do
     try
      PixelFormat := pf32bit;
      Width       := fOSFactor * fBuffer.Width;
      Height      := fOSFactor * fBuffer.Height;
      Font.Assign(fBuffer.Canvas.Font);
      Brush.Assign(fBuffer.Canvas.Brush);
      Pen.Assign(fBuffer.Canvas.Pen);
      {$IFNDEF FPC}
      if fTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample2xBitmap(Bmp);
       end else
      {$ENDIF}
      Canvas.FillRect(ClipRect);
{
      if fShadow.Visible then
       begin
        RenderLabelToBitmap(Bmp);
       end
      else
}
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
    with Bmp, Canvas do
     try
      PixelFormat := pf32bit;
      Width       := fOSFactor * fBuffer.Width;
      Height      := fOSFactor * fBuffer.Height;
      Font.Assign(fBuffer.Canvas.Font);
      Brush.Assign(fBuffer.Canvas.Brush);
      Pen.Assign(fBuffer.Canvas.Pen);
      {$IFNDEF FPC}
      if fTransparent then
       begin
        CopyParentImage(Self, Bmp.Canvas);
//        DrawParentImage(Bmp.Canvas);
        Upsample4xBitmap(Bmp);
       end else
      {$ENDIF}
      FillRect(ClipRect);
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
   Brush.Style := bsClear;
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
