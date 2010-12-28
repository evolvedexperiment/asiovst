unit DAV_GuiGroup;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Forms, Classes, Graphics, Controls, StdCtrls,
  DAV_GuiCommon, DAV_GuiFont, DAV_GuiPixelMap, DAV_GuiByteMap, DAV_GuiShadow;

type
  TCustomGuiGroup = class(TCustomGroupBox)
  private
    FAutoFocus   : Boolean;
    FBorderColor : TColor;
    FBorderWidth : Single;
    FGroupColor  : TColor;
    FRoundRadius : Single;
    FNative      : Boolean;
    FCanvas      : TCanvas;
    FTransparent : Boolean;
    FOnPaint     : TNotifyEvent;
    FAlpha       : Byte;
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
    procedure SetAlpha(const Value: Byte);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetGroupColor(const Value: TColor);
    procedure SetNative(const Value: Boolean);
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetRoundRadius(Value: Single);
    procedure SetShadow(const Value: TGUIShadow);
    procedure SetTransparent(const Value: Boolean);
  protected
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBuffer     : Boolean;
    FUpdateBackBuffer : Boolean;
    FGuiFont          : TGuiOversampledGDIFont;

    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TWmNoParams{$ENDIF}); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;

    procedure AlphaChanged; virtual;
    procedure Click; override;
    procedure GroupColorChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure RoundRadiusChanged; virtual;
    procedure TransparentChanged; virtual;
    procedure TextChanged; virtual;

    procedure BufferChanged; virtual;
    procedure BackBufferChanged; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;

    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); virtual; abstract;

    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Alpha: Byte read FAlpha write SetAlpha default $FF;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnShadow;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property Caption;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property GroupColor: TColor read FGroupColor write SetGroupColor default clBtnFace;
    property Native: Boolean read FNative write SetNative default True;
    property Radius: Single read FRoundRadius write SetRoundRadius;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiGroup = class(TCustomGuiGroup)
  private
    FHeaderWidth  : Integer;
    FHeaderHeight : Integer;
    procedure CalculateHeaderSize;
  protected
    procedure FontChangedHandler(Sender: TObject); override;
    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); override;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); override;
    procedure TextChanged; override;
  published
    property Align;
    property Alpha;
    property Anchors;
    property BorderColor;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property GroupColor;
    property HelpContext;
    property Hint;
    property Native;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Radius;
    property Shadow;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TGuiGroupSide = class(TCustomGuiGroup)
  private
    FHeaderWidth  : Integer;
    procedure CalculateHeaderSize;
  protected
    procedure FontChangedHandler(Sender: TObject); override;
    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); override;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); override;
    procedure TextChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Alpha;
    property Anchors;
    property BorderColor;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property GroupColor;
    property HelpContext;
    property Hint;
    property Native;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Radius;
    property Shadow;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TGuiGroupTop = class(TCustomGuiGroup)
  private
    FHeaderHeight : Integer;
    procedure CalculateHeaderSize;
  protected
    procedure FontChangedHandler(Sender: TObject); override;
    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); override;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); override;
    procedure TextChanged; override;
  published
    property Align;
    property Alpha;
    property Anchors;
    property BorderColor;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property GroupColor;
    property HelpContext;
    property Hint;
    property Native;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Radius;
    property Shadow;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  Math, DAV_Math, DAV_Complex, DAV_Approximations, DAV_GuiBlend,
  DAV_GuiFixedPoint;

{ TCustomGuiGroup }

constructor TCustomGuiGroup.Create(AOwner: TComponent);
begin
 inherited;

 ControlStyle   := ControlStyle + [csAcceptsControls, csOpaque];

 {$IFDEF FPC}
 DoubleBuffered := True;
 {$ENDIF}

 FAlpha            := $FF;
 FRoundRadius      := 2;
 FUpdateBuffer     := True;
 FUpdateBackBuffer := True;
 FTransparent      := False;
 FBorderColor      := clBtnShadow;
 FGroupColor       := clBtnFace;
 FBorderWidth      := 1;
 FNative           := False;

 // create control canvas
 FCanvas := TControlCanvas.Create;
 TControlCanvas(FCanvas).Control := Self;

 // create buffers (& set size)
 FBuffer           := TGuiPixelMapMemory.Create;
 FBackBuffer       := TGuiPixelMapMemory.Create;
 FBuffer.SetSize(Width, Height);
 FBackBuffer.SetSize(Width, Height);

 // create font
 FGuiFont          := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 // set initial bounds
 SetBounds(0, 0, 128, 64);
end;

destructor TCustomGuiGroup.Destroy;
begin
 // free buffers
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);

 inherited;
end;

procedure TCustomGuiGroup.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.SetGroupColor(const Value: TColor);
begin
 if FGroupColor <> Value then
  begin
   FGroupColor := Value;
   GroupColorChanged;
  end;
end;

function TCustomGuiGroup.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TCustomGuiGroup.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TCustomGuiGroup.GroupColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.AlphaChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.BackBufferChanged;
begin
 if not FNative
  then FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TCustomGuiGroup.BufferChanged;
begin
 if not FNative
  then FUpdateBuffer := True;
 Invalidate;
end;

procedure TCustomGuiGroup.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TCustomGuiGroup.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiGroup.BorderColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiGroup.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiGroup.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TCustomGuiGroup.BorderWidthChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.SetRoundRadius(Value: Single);
begin
 if Value < 0 then Value := 0;
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   RoundRadiusChanged;
  end;
end;

procedure TCustomGuiGroup.Resize;
begin
 inherited;

 // resize and update back buffer
 if Assigned(FBackBuffer) then
  begin
   FBackBuffer.SetSize(Width, Height);
   BackBufferChanged;
  end;

 // resize and update back buffer
 if Assigned(FBuffer) then
  begin
   FBuffer.SetSize(Width, Height);
   BufferChanged;
  end;
end;

procedure TCustomGuiGroup.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TCustomGuiGroup.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiGroup.TextChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.TransparentChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.UpdateBackBuffer;
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

procedure TCustomGuiGroup.UpdateBuffer;
begin
 FUpdateBuffer := False;

 inherited;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy entire back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));

 RenderGroupBox(FBuffer);
 RenderCaption(FBuffer);
end;

procedure TCustomGuiGroup.WMMove(var Message: TWMMove);
begin
 inherited;

 if FTransparent
  then BackBufferChanged;
end;

procedure TCustomGuiGroup.RoundRadiusChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.Click;
begin
 if FAutoFocus then SetFocus;
 inherited;
end;

procedure TCustomGuiGroup.CMColorChanged(var Message: TMessage);
begin
 inherited;

 if not FNative
  then BackBufferChanged;
end;

procedure TCustomGuiGroup.CMDialogChar(var Message: TCMDialogChar);
begin
 with Message do
  if IsAccel(Message.CharCode, Caption) and CanFocus then
   begin
    SetFocus;
    Result := 1;
   end;
end;

procedure TCustomGuiGroup.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 BufferChanged;
end;

procedure TCustomGuiGroup.CMFontChanged(var Message: TMessage);
begin
 FGuiFont.Font.Assign(Font);
end;

procedure TCustomGuiGroup.CMSysColorChange(var Message: TMessage);
begin
 inherited;
 BufferChanged;
end;

procedure TCustomGuiGroup.CMTextChanged(var Message: TWmNoParams);
begin
 TextChanged;
end;

procedure TCustomGuiGroup.SetNative(const Value: Boolean);
begin
 if FNative <> Value then
  begin
   FNative := Value;
   RecreateWnd;
   if not FNative
    then FUpdateBackBuffer := True;
   Invalidate;
  end;
end;

procedure TCustomGuiGroup.Paint;
begin
 if FNative
  then inherited
  else
   begin
    if FUpdateBackBuffer
     then UpdateBackBuffer;

    if FUpdateBuffer
     then UpdateBuffer;

    if Assigned(FOnPaint)
     then FOnPaint(Self);

    if Assigned(FBuffer)
     then FBuffer.PaintTo(Canvas);
   end;
end;


{ TCustomGuiGroupA }

{
procedure TCustomGuiGroupA.RenderGroupBoxToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexSingle;
  Steps, i : Integer;
  LineOffs : array[0..1] of Integer;
  PntArray : array of TPoint;
//  rct      : TRect;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;

   Brush.Style := bsSolid;
   Brush.Color := FGroupColor;
   Pen.Width   := FOSFactor * FBorderWidth;
   Pen.Color   := FBorderColor;
   Font.Assign(Self.Font);
   Font.Size := FOSFactor * Font.Size;
   TextSize := TextExtent(FCaption);

   case FRoundRadius of
    0, 1 : begin
            FrameRect(ClipRect);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
           end;
       2 : begin
            LineOffs[0] := Round(BorderWidth div 2);
            LineOffs[1] := Round((BorderWidth + 1) div 2);
            with ClipRect do
             begin
              PntArray[ 0] := Point(Left  + 1 + LineOffs[0], Bottom - 1 - LineOffs[1]);
              PntArray[ 1] := Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1]);
              PntArray[ 2] := Point(Left      + LineOffs[0], Top    + 2 + LineOffs[0]);
              PntArray[ 3] := Point(Left  + 2 + LineOffs[0], Top        + LineOffs[0]);
              PntArray[ 4] := Point(Right - 2 - LineOffs[1], Top        + LineOffs[0]);
              PntArray[ 5] := Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]);
              PntArray[ 6] := Point(Right - 1 - LineOffs[1], Top    + 1 + LineOffs[0]);
              PntArray[ 7] := Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]);
              PntArray[ 8] := Point(Right     - LineOffs[1], Bottom - 2 - LineOffs[1]);
              PntArray[ 9] := Point(Right - 2 - LineOffs[1], Bottom     - LineOffs[1]);
              PntArray[10] := Point(Left  + 2 + LineOffs[0], Bottom     - LineOffs[1]);
              PntArray[11] := Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1]);
             end;
            PolyLine(PntArray);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
            // MoveTo(TextSize.cx + 12, 1);
            // LineTo(TextSize.cx + 12, TextSize.cy + 3);
           end;
    else
     begin
      rad := FOSFactor * FRoundRadius;
      Steps := Round(2 / Arcsin(1 / rad)) + 1;
      if Steps > 1 then
      begin
        SetLength(PtsArray, Steps + 4);
        Val.Im := 0; Val.Re := -1;
        Val.Re := Val.Re * rad; Val.Im := Val.Im * rad;

        GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
        PtsArray[0] := Point(Round(BorderWidth div 2), Round(BorderWidth div 2 + rad));

        // upper left corner
        for i := 1 to Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 4] := Point(BorderWidth div 2 + rad, BorderWidth div 2 + 0);

        // upper right corner
        for i := Steps div 4 to Steps div 2 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 1] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 2 + 1] := Point(ClipRect.Right - (BorderWidth + 1) div 2, BorderWidth div 2 + rad);

        // lower right corner
        for i := Steps div 2 to 3 * Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 2] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[3 * Steps div 4 + 2] := Point(ClipRect.Right - rad - (BorderWidth + 1) div 2, ClipRect.Bottom - (BorderWidth + 1) div 2);

        // lower left corner
        for i := 3 * Steps div 4 to Steps - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[Steps + 3] := Point(BorderWidth div 2, rad + BorderWidth div 2);

        PolyGon(PtsArray);

(*
        // Draw inner text
        //////////////////

        Brush.Color   := FBorderColor;
        SetLength(PtsArray, Steps div 2 + 5);
        Val.Re := -rad; Val.Im := 0;

        GetSinCos(2 * Pi / (Steps div 2 - 1), Off.Im, Off.Re);
        rct := Rect(BorderWidth div 2, BorderWidth div 2, max(TextSize.cx + 10, FOSFactor * FHeaderMinWidth) - (BorderWidth + 1) div 2, TextSize.cy + 5 - (BorderWidth + 1) div 2);
        PtsArray[0] := Point(Round(rct.Left), Round(rct.Top + rad));

        // upper left corner
        for i := 1 to (Steps div 4) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(rct.Left + rad + Val.Re), Round(rct.Top + rad + Val.Im));
         end;
        PtsArray[Steps div 4    ] := Point(rct.Left + rad, rct.Top);
        PtsArray[Steps div 4 + 1] := Point(rct.Right, rct.Top);
        PtsArray[Steps div 4 + 2] := Point(rct.Right, rct.Bottom - rad);

        Val.Re := rad; Val.Im := 0;

        // lower right corner
        for i := (Steps div 4) to (Steps div 2) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(rct.Right - rad + Val.Re), Round(rct.Bottom - rad + Val.Im));
         end;

        PtsArray[Steps div 2 + 3] := Point(rct.Right - rad, rct.Bottom);
        PtsArray[Steps div 2 + 4] := Point(rct.Left, rct.Bottom);

        Polygon(PtsArray);
*)
      end;
     end;
   end;

   Brush.Style := bsClear;
   TextOut(6, 2, FCaption);
   Unlock;
  end;
end;
}


{ TGuiGroup }

procedure TGuiGroup.FontChangedHandler(Sender: TObject);
begin
 CalculateHeaderSize;
 inherited;
end;

procedure TGuiGroup.CalculateHeaderSize;
var
  TextSize : TSize;
begin
 TextSize := FGuiFont.TextExtend(Caption);
 FHeaderWidth := TextSize.cx;
 FHeaderHeight := TextSize.cy;
end;

procedure TGuiGroup.RenderCaption(PixelMap: TGuiCustomPixelMap);
var
  TextSize : TSize;
begin
 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtend(Caption);
   FGuiFont.TextOut(Caption, PixelMap, Round(2 * FBorderWidth), Round(FBorderWidth));
  end;
end;

procedure TGuiGroup.RenderGroupBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y, Offset         : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  IsHeader             : Boolean;
  HeaderHeight         : TFixed24Dot8Point;
  HeaderWidth          : TFixed24Dot8Point;
  RadiusFixed          : TFixed24Dot8Point;
  XStart               : TFixed24Dot8Point;
  BorderWidthFixed     : TFixed24Dot8Point;
  RadMinusOne          : TFixed24Dot8Point;
  RadMinusBorder       : TFixed24Dot8Point;
  SqrRadMinusBorderOne : TFixed24Dot8Point;
  SqrRadMinusBorder    : TFixed24Dot8Point;
  SqrDist, SqrYDist    : TFixed24Dot8Point;
  SqrRadMinusOne       : TFixed24Dot8Point;
  XFixed, YFixed       : TFixed24Dot8Point;
  WidthMinusOne        : TFixed24Dot8Point;
  YBorderDistance      : TFixed24Dot8Point;
  Temp                 : TFixed24Dot8Point;
begin
 with PixelMap do
  begin
   // set local colors
   PanelColor := ConvertColor(FGroupColor);
   PanelColor.A := Alpha;
   BorderColor := ConvertColor(FBorderColor);

   // set other local variables
   RadiusFixed := ConvertToFixed24Dot8Point(Min(FRoundRadius, 0.5 * Min(Width, Height)) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8Point(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8Point(Width - 1);
   HeaderHeight.Fixed := ConvertToFixed24Dot8Point(FHeaderHeight).Fixed +
     2 * BorderWidthFixed.Fixed;
   HeaderWidth.Fixed := ConvertToFixed24Dot8Point(FHeaderWidth).Fixed +
     3 * BorderWidthFixed.Fixed;

   // precalculate radius variables
   RadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := RadiusFixed.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);

   // draw top rounded borders
   for Y := 0 to FixedRound(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8Point(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Y];

     // check whether the scan line contains the header
     IsHeader := YFixed.Fixed <= HeaderHeight.Fixed;

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8Point(Width - 1), Temp));
     X := XRange[0];
     while X <= XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8Point(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8Point(Width - 1).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed <= SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));

(*
         Offset := FixedRound(FixedSub(HeaderWidth, BorderWidthFixed));
         if X <= Offset then
          begin
           BlendPixelLine(BorderColor, @ScnLne[X], Offset - 1);
           X := X + Offset - 1;
          end;
*)
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X);
      end;
    end;

   // draw bottom rounded borders
   for Y := 0 to FixedRound(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8Point(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Height - 1 - Y];

     // check whether the scan line contains the header
     IsHeader := YFixed.Fixed <= HeaderHeight.Fixed;

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8Point(Width - 1), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8Point(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8Point(Width - 1).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         if Temp.Fixed > $FF
          then CombColor := PanelColor
          else CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;

   for Y := FixedRound(RadiusFixed) to Height - 1 - FixedRound(RadiusFixed) do
    begin
     ScnLne := Scanline[Y];
     YFixed := ConvertToFixed24Dot8Point(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
        (YFixed.Fixed > ConvertToFixed24Dot8Point(Height).Fixed - BorderWidthFixed.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0], Width);
       Continue;
      end;

     // check whether the scan line contains the header
     IsHeader := YFixed.Fixed <= HeaderHeight.Fixed;

     // check upper/lower half and eventually precalculate y-border distance
     Temp := ConvertToFixed24Dot8Point(Height - 1);
     IsUpperLowerHalf := (YFixed.Fixed < BorderWidthFixed.Fixed) or
       (YFixed.Fixed > Temp.Fixed - BorderWidthFixed.Fixed);
     if IsUpperLowerHalf then
      if Y < Height div 2
       then YBorderDistance.Fixed := BorderWidthFixed.Fixed - YFixed.Fixed
       else YBorderDistance.Fixed := YFixed.Fixed - Temp.Fixed + BorderWidthFixed.Fixed
      else YBorderDistance.Fixed := 0;

     X := 0;
     while X < Width do
      begin
       // convert
       XFixed := ConvertToFixed24Dot8Point(X);

       // check whether position is an upper/lower half border
       if IsUpperLowerHalf then
        begin
         if (XFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed > ConvertToFixed24Dot8Point(Width).Fixed - BorderWidthFixed.Fixed) or
            (IsHeader and (XFixed.Fixed < HeaderWidth.Fixed - CFixed24Dot8One.Fixed))
          then CombColor := BorderColor else
         if (XFixed.Fixed < BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end else
         if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := XFixed.Fixed + BorderWidthFixed.Fixed - WidthMinusOne.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end
         else
          begin
           Assert(YBorderDistance.Fixed >= 0);
           Assert(YBorderDistance.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, YBorderDistance.Fixed);
           BlendPixelLine(CombColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end;
        end else
       if (XFixed.Fixed <= BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
          (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed + CFixed24Dot8One.Fixed) or
          (IsHeader and (XFixed.Fixed < HeaderWidth.Fixed - CFixed24Dot8One.Fixed))
        then CombColor := BorderColor else
       if (XFixed.Fixed < BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end else
       if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := XFixed.Fixed - WidthMinusOne.Fixed + BorderWidthFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end
       else
        begin
         if Width - X <= 0 then
          begin
           BlendPixelLine(PanelColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end else CombColor := PanelColor;
        end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X)
      end;
    end;
   EMMS;
  end;
end;

procedure TGuiGroup.TextChanged;
begin
 CalculateHeaderSize;
 inherited;
end;

(*
procedure TCustomGuiGroupB.RenderGroupBoxToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplexSingle;
  Steps, i : Integer;
  rct      : TRect;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;

   Brush.Style := bsSolid;
   Brush.Color := FGroupColor;
   Pen.Width   := FOSFactor * FBorderWidth;
   Pen.Color   := FBorderColor;
   Font.Assign(Self.Font);
   Font.Size   := FOSFactor * Font.Size;
   TextSize    := TextExtent(FCaption);
   TextSize.cx := TextSize.cx + 2 * FOSFactor * (FBorderWidth + FOffset);
   if TextSize.cx < FHeaderMinWidth
    then TextSize.cx := TextSize.cx + FOSFactor * FHeaderMinWidth;
   TextSize.cy := TextSize.cy + 3 * FOSFactor * FBorderWidth div 2;

   rct := ClipRect;
   InflateRect(rct, -FOSFactor * (BorderWidth + 1) div 2, -FOSFactor * (BorderWidth + 1) div 2);

   case FRoundRadius of
    0, 1 : begin
            Rectangle(rct.Left, rct.Top, rct.Right + 1, rct.Bottom + 1);
            Brush.Color := FGroupColor;
            FillRect(Rect(rct.Left + FOSFactor * BorderWidth div 2, rct.Top + FOSFactor * BorderWidth div 2, TextSize.cx, TextSize.cy));
            MoveTo(FOSFactor * FBorderWidth, TextSize.cy);
            LineTo(TextSize.cx, TextSize.cy);
            LineTo(TextSize.cx, rct.Top);
           end;
       2 : begin
            with rct do
             PolyLine([Point(Left  + 1 * FOSFactor, Bottom - 1 * FOSFactor),
                       Point(Left                 , Bottom - 2 * FOSFactor),
                       Point(Left                 , Top    + 2 * FOSFactor),
                       Point(Left  + 2 * FOSFactor, Top                   ),
                       Point(Right - 2 * FOSFactor, Top                   ),
                       Point(Right                , Top    + 2 * FOSFactor),
                       Point(Right - 1 * FOSFactor, Top    + 1 * FOSFactor),
                       Point(Right                , Top    + 2 * FOSFactor),
                       Point(Right                , Bottom - 2 * FOSFactor),
                       Point(Right - 2 * FOSFactor, Bottom                ),
                       Point(Left  + 2 * FOSFactor, Bottom                ),
                       Point(Left                 , Bottom - 2 * FOSFactor)]);
            Brush.Color := FGroupColor;
            FillRect(Rect(rct.Left + FOSFactor * BorderWidth div 2, rct.Top + FOSFactor * BorderWidth div 2, TextSize.cx, TextSize.cy));
            MoveTo(FOSFactor * FBorderWidth, TextSize.cy);
            LineTo(TextSize.cx, TextSize.cy);
            LineTo(TextSize.cx, rct.Top);
           end;
    else
     begin
      rct := ClipRect;
      Brush.Color := FGroupColor;
      InflateRect(rct, -FOSFactor * (BorderWidth + 1) div 2, -FOSFactor * (BorderWidth + 1) div 2);

      rad := FOSFactor * FRoundRadius;
      Steps := Round(2 / arcsin(1 / rad)) + 1;
      if Steps > 1 then
       begin
        SetLength(PtsArray, Steps + 4);
        Val.Im := 0; Val.Re := -1;
        Val.Re := Val.Re * rad; Val.Im := Val.Im * rad;

        GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
        PtsArray[0] := Point(Round(BorderWidth div 2), Round(BorderWidth div 2 + rad));

        // upper left corner
        for i := 1 to Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 4] := Point(BorderWidth div 2 + rad, BorderWidth div 2 + 0);

        // upper right corner
        for i := Steps div 4 to Steps div 2 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 1] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 2 + 1] := Point(ClipRect.Right - (BorderWidth + 1) div 2, BorderWidth div 2 + rad);

        // lower right corner
        for i := Steps div 2 to 3 * Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 2] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[3 * Steps div 4 + 2] := Point(ClipRect.Right - rad - (BorderWidth + 1) div 2, ClipRect.Bottom - (BorderWidth + 1) div 2);

        // lower left corner
        for i := 3 * Steps div 4 to Steps - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[Steps + 3] := Point(BorderWidth div 2, rad + BorderWidth div 2);

        PolyGon(PtsArray);


        // Draw inner text
        //////////////////

        Brush.Color := FGroupColor;
        SetLength(PtsArray, Steps div 2 + 5);
        Val.Re := -rad; Val.Im := 0;

        GetSinCos(Pi / (Steps div 2 - 1), Off.Im, Off.Re);
        rct := Rect(BorderWidth div 2, BorderWidth div 2, TextSize.cx + 10 - (BorderWidth + 1) div 2, TextSize.cy + 5 - (BorderWidth + 1) div 2);
        PtsArray[0] := Point(Round(rct.Left), Round(rct.Top + rad));

        // upper left corner
        for i := 1 to (Steps div 4) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(rct.Left + rad + Val.Re), Round(rct.Top + rad + Val.Im));
         end;
        PtsArray[Steps div 4    ] := Point(rct.Left + rad, rct.Top);
        PtsArray[Steps div 4 + 1] := Point(rct.Right, rct.Top);
        PtsArray[Steps div 4 + 2] := Point(rct.Right, rct.Bottom - rad);

        Val.Re := rad; Val.Im := 0;

        // lower right corner
        for i := (Steps div 4) to (Steps div 2) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(rct.Right - rad + Val.Re), Round(rct.Bottom - rad + Val.Im));
         end;

        PtsArray[Steps div 2 + 3] := Point(rct.Right - rad, rct.Bottom);
        PtsArray[Steps div 2 + 4] := Point(rct.Left, rct.Bottom);

        Polygon(PtsArray);
      end;
     end;
   end;

   Brush.Style := bsClear;
   TextOut(FOSFactor * (FBorderWidth + FOffset), FOSFactor * FBorderWidth, FCaption);
   Unlock;
  end;
end;
*)

{ TGuiGroupSide }

procedure TGuiGroupSide.CalculateHeaderSize;
var
  TextSize : TSize;
begin
 TextSize := FGuiFont.TextExtend(Caption);
 FHeaderWidth := TextSize.cy;
end;

constructor TGuiGroupSide.Create(AOwner: TComponent);
begin
 inherited;
 FGuiFont.FontTurn := ftClockwise;
end;

procedure TGuiGroupSide.FontChangedHandler(Sender: TObject);
begin
 CalculateHeaderSize;
 inherited;
end;

procedure TGuiGroupSide.RenderGroupBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y, Offset         : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  HeaderHeight         : TFixed24Dot8Point;
  HeaderWidth          : TFixed24Dot8Point;
  RadiusFixed          : TFixed24Dot8Point;
  XStart               : TFixed24Dot8Point;
  BorderWidthFixed     : TFixed24Dot8Point;
  RadMinusOne          : TFixed24Dot8Point;
  RadMinusBorder       : TFixed24Dot8Point;
  SqrRadMinusBorderOne : TFixed24Dot8Point;
  SqrRadMinusBorder    : TFixed24Dot8Point;
  SqrDist, SqrYDist    : TFixed24Dot8Point;
  SqrRadMinusOne       : TFixed24Dot8Point;
  XFixed, YFixed       : TFixed24Dot8Point;
  WidthMinusOne        : TFixed24Dot8Point;
  YBorderDistance      : TFixed24Dot8Point;
  Temp                 : TFixed24Dot8Point;
begin
 with PixelMap do
  begin
   // set local colors
   PanelColor := ConvertColor(FGroupColor);
   PanelColor.A := Alpha;
   BorderColor := ConvertColor(FBorderColor);

   // set other local variables
   RadiusFixed := ConvertToFixed24Dot8Point(Min(FRoundRadius, 0.5 * Min(Width, Height)) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8Point(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8Point(Width - 1);
   HeaderWidth.Fixed := ConvertToFixed24Dot8Point(FHeaderWidth).Fixed +
     3 * BorderWidthFixed.Fixed;

   // precalculate radius variables
   RadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := RadiusFixed.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);

   // draw top rounded borders
   for Y := 0 to FixedRound(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8Point(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8Point(Width - 1), Temp));
     X := XRange[0];
     while X <= XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8Point(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8Point(Width - 1).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed <= SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X);
      end;
    end;

   // draw bottom rounded borders
   for Y := 0 to FixedRound(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8Point(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Height - 1 - Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8Point(Width - 1), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8Point(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8Point(Width - 1).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         if Temp.Fixed > $FF
          then CombColor := PanelColor
          else CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;

   for Y := FixedRound(RadiusFixed) to Height - 1 - FixedRound(RadiusFixed) do
    begin
     ScnLne := Scanline[Y];
     YFixed := ConvertToFixed24Dot8Point(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
        (YFixed.Fixed > ConvertToFixed24Dot8Point(Height).Fixed - BorderWidthFixed.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0], Width);
       Continue;
      end;

     // check upper/lower half and eventually precalculate y-border distance
     Temp := ConvertToFixed24Dot8Point(Height - 1);
     IsUpperLowerHalf := (YFixed.Fixed < BorderWidthFixed.Fixed) or
       (YFixed.Fixed > Temp.Fixed - BorderWidthFixed.Fixed);
     if IsUpperLowerHalf then
      if Y < Height div 2
       then YBorderDistance.Fixed := BorderWidthFixed.Fixed - YFixed.Fixed
       else YBorderDistance.Fixed := YFixed.Fixed - Temp.Fixed + BorderWidthFixed.Fixed
      else YBorderDistance.Fixed := 0;

     X := 0;
     while X < Width do
      begin
       // convert
       XFixed := ConvertToFixed24Dot8Point(X);

       // check whether position is an upper/lower half border
       if IsUpperLowerHalf then
        begin
         if (XFixed.Fixed < BorderWidthFixed.Fixed + HeaderWidth.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed > ConvertToFixed24Dot8Point(Width).Fixed - BorderWidthFixed.Fixed)
          then CombColor := BorderColor else
         if (XFixed.Fixed < BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end else
         if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := XFixed.Fixed + BorderWidthFixed.Fixed - WidthMinusOne.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end
         else
          begin
           Assert(YBorderDistance.Fixed >= 0);
           Assert(YBorderDistance.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, YBorderDistance.Fixed);
           BlendPixelLine(CombColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end;
        end else
       if (XFixed.Fixed <= BorderWidthFixed.Fixed + HeaderWidth.Fixed - CFixed24Dot8One.Fixed) or
          (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed + CFixed24Dot8One.Fixed)
        then CombColor := BorderColor else
       if (XFixed.Fixed < BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end else
       if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := XFixed.Fixed - WidthMinusOne.Fixed + BorderWidthFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end
       else
        begin
         if Width - X <= 0 then
          begin
           BlendPixelLine(PanelColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end else CombColor := PanelColor;
        end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X)
      end;
    end;
   EMMS;
  end;
end;

procedure TGuiGroupSide.RenderCaption(PixelMap: TGuiCustomPixelMap);
var
  TextSize : TSize;
begin
 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtend(Caption);
   FGuiFont.TextOut(Caption, PixelMap, Round(2 * FBorderWidth), (Height - TextSize.cx) div 2);
  end;
end;

procedure TGuiGroupSide.TextChanged;
begin
 CalculateHeaderSize;
 inherited;
end;


{ TGuiGroupTop }

procedure TGuiGroupTop.CalculateHeaderSize;
var
  TextSize : TSize;
begin
 TextSize := FGuiFont.TextExtend(Caption);
 FHeaderHeight := TextSize.cy;
end;

procedure TGuiGroupTop.FontChangedHandler(Sender: TObject);
begin
 CalculateHeaderSize;
 inherited;
end;

procedure TGuiGroupTop.RenderGroupBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y, Offset         : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  HeaderHeight         : TFixed24Dot8Point;
  HeaderWidth          : TFixed24Dot8Point;
  RadiusFixed          : TFixed24Dot8Point;
  XStart               : TFixed24Dot8Point;
  BorderWidthFixed     : TFixed24Dot8Point;
  RadMinusOne          : TFixed24Dot8Point;
  RadMinusBorder       : TFixed24Dot8Point;
  SqrRadMinusBorderOne : TFixed24Dot8Point;
  SqrRadMinusBorder    : TFixed24Dot8Point;
  SqrDist, SqrYDist    : TFixed24Dot8Point;
  SqrRadMinusOne       : TFixed24Dot8Point;
  XFixed, YFixed       : TFixed24Dot8Point;
  WidthMinusOne        : TFixed24Dot8Point;
  YBorderDistance      : TFixed24Dot8Point;
  Temp                 : TFixed24Dot8Point;
begin
 with PixelMap do
  begin
   // set local colors
   PanelColor := ConvertColor(FGroupColor);
   PanelColor.A := Alpha;
   BorderColor := ConvertColor(FBorderColor);

   // set other local variables
   RadiusFixed := ConvertToFixed24Dot8Point(Min(FRoundRadius, Min(FHeaderHeight, 0.5 * Min(Width, Height))) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8Point(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8Point(Width - 1);
   HeaderHeight.Fixed := ConvertToFixed24Dot8Point(FHeaderHeight).Fixed +
     2 * BorderWidthFixed.Fixed;

   // precalculate radius variables
   RadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := RadiusFixed.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);

   // draw top rounded borders
   for Y := 0 to FixedRound(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8Point(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8Point(Width - 1), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8Point(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8Point(Width - 1).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed
        then CombColor := BorderColor
        else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;

   // draw bottom rounded borders
   for Y := 0 to FixedRound(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8Point(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Height - 1 - Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8Point(Width - 1), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8Point(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8Point(Width - 1).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         if Temp.Fixed > $FF
          then CombColor := PanelColor
          else CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;

   for Y := FixedRound(RadiusFixed) to Height - 1 - FixedRound(RadiusFixed) do
    begin
     ScnLne := Scanline[Y];
     YFixed := ConvertToFixed24Dot8Point(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
        (YFixed.Fixed > ConvertToFixed24Dot8Point(Height).Fixed - BorderWidthFixed.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0], Width);
       Continue;
      end;

     // check upper/lower half and eventually precalculate y-border distance
     Temp := ConvertToFixed24Dot8Point(Height - 1);
     IsUpperLowerHalf := (YFixed.Fixed < BorderWidthFixed.Fixed) or
       (YFixed.Fixed > Temp.Fixed - BorderWidthFixed.Fixed);
     if IsUpperLowerHalf then
      if Y < Height div 2
       then YBorderDistance.Fixed := BorderWidthFixed.Fixed - YFixed.Fixed
       else YBorderDistance.Fixed := YFixed.Fixed - Temp.Fixed + BorderWidthFixed.Fixed
      else YBorderDistance.Fixed := 0;

     X := 0;
     while X < Width do
      begin
       // convert
       XFixed := ConvertToFixed24Dot8Point(X);

       // check whether position is an upper/lower half border
       if IsUpperLowerHalf then
        begin
         if (XFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed > ConvertToFixed24Dot8Point(Width).Fixed - BorderWidthFixed.Fixed) or
            (YFixed.Fixed <= HeaderHeight.Fixed)
          then CombColor := BorderColor else
         if (XFixed.Fixed < BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end else
         if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := XFixed.Fixed + BorderWidthFixed.Fixed - WidthMinusOne.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end
         else
          begin
           Assert(YBorderDistance.Fixed >= 0);
           Assert(YBorderDistance.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, YBorderDistance.Fixed);
           BlendPixelLine(CombColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end;
        end else
       if (XFixed.Fixed <= BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
          (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed + CFixed24Dot8One.Fixed) or
          (YFixed.Fixed <= HeaderHeight.Fixed)
        then CombColor := BorderColor else
       if (XFixed.Fixed < BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end else
       if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := XFixed.Fixed - WidthMinusOne.Fixed + BorderWidthFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end
       else
        begin
         if Width - X <= 0 then
          begin
           BlendPixelLine(PanelColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end else CombColor := PanelColor;
        end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X)
      end;
    end;
   EMMS;
  end;
end;

procedure TGuiGroupTop.RenderCaption(PixelMap: TGuiCustomPixelMap);
var
  TextSize : TSize;
begin
 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtend(Caption);
   FGuiFont.TextOut(Caption, PixelMap, (Width - TextSize.cx) div 2 , Round(FBorderWidth));
  end;
end;

procedure TGuiGroupTop.TextChanged;
begin
 CalculateHeaderSize;
 inherited;
end;

end.
