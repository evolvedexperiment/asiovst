unit DAV_GuiBaseControl;

interface

{$I ASIOVST.INC}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Messages, Graphics, Classes, Controls, ExtCtrls;

type
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  TRGB32Array = packed array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  TGuiOnDragMouseMove = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;

  TGuiMouseButtonState = record
    ButtonDown     : Boolean;
    EventX, EventY : Integer;
    ShiftState     : TShiftState;
  end;

  TGuiMouseState = class
    LeftBtn,
    MiddleBtn,
    RightBtn     : TGuiMouseButtonState;
    LastEventX,
    LastEventY   : Integer;
  end;

  TGuiMouseStateClass = class of TGuiMouseState;

  TGuiAntiAlias = (gaaNone, gaaLinear2x, gaaLinear4x, gaaLinear8x, gaaLinear16x);

  TGuiStitchKind = (skHorizontal, skVertical);

  TGUIShadow = class(TPersistent)
  private
    fBlur         : Byte;
    fOffset       : TPoint;
    fTransparency : Byte;
    fVisible      : Boolean;
    fOnChange     : TNotifyEvent;
    function GetOffsetX: Integer;
    function GetOffsetY: Integer;
    procedure SetBlur(const Value: Byte);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetTransparency(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetOffset(const Value: TPoint);
    procedure Changed;
  public
    constructor Create; virtual;
    property Offset : TPoint read fOffset write SetOffset;
  published
    property Blur : Byte read fBlur write SetBlur default 4;
    property OffsetX : Integer read GetOffsetX write SetOffsetX default 1;
    property OffsetY : Integer read GetOffsetY write SetOffsetY default 1;
    property Transparency : Byte read fTransparency write SetTransparency default $FF;
    property Visible : Boolean read fVisible write SetVisible default False;
    property OnChange : TNotifyEvent read fOnChange write fOnChange;
  end;

  TBufferedGraphicControl = class(TGraphicControl)
  protected
    fBuffer   : TBitmap;
    fOnPaint  : TNotifyEvent;

    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}
    
    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}

    procedure Downsample2xBitmap(var Bitmap: TBitmap);
    procedure Downsample4xBitmap(var Bitmap: TBitmap);
    procedure Upsample2xBitmap(var Bitmap: TBitmap);
    procedure Upsample4xBitmap(var Bitmap: TBitmap);
    procedure Resize; override;
    procedure ResizeBuffer; dynamic;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); dynamic; abstract;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;

    {$IFNDEF COMPILER10_UP}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
  end;

(*
  TBufferedWinControl = class(TWinControl)
  protected
    fBuffer   : TBitmap;
    fOnPaint  : TNotifyEvent;

    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}
    
    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}

    procedure Downsample2xBitmap(var Bitmap: TBitmap);
    procedure Downsample4xBitmap(var Bitmap: TBitmap);
    procedure Upsample2xBitmap(var Bitmap: TBitmap);
    procedure Upsample4xBitmap(var Bitmap: TBitmap);
    procedure Resize; override;
    procedure ResizeBuffer; dynamic;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); dynamic; abstract;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;

    {$IFNDEF COMPILER10_UP}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
  end;
*)

  TCustomGuiBaseControl = class(TBufferedGraphicControl)
  protected
    fLineColor              : TColor;
    fLineWidth              : Integer;
    {$IFNDEF FPC}
    fTransparent            : Boolean;
    procedure SetTransparent(Value: Boolean); virtual;
    {$ENDIF}

    procedure SetLineWidth(Value: Integer); virtual;
    procedure SetLineColor(Value: TColor); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    property LineWidth: Integer read fLineWidth write SetLineWidth default 1;
    property LineColor: TColor read fLineColor write SetLineColor default clBlack;
    {$IFNDEF FPC}
    property Transparent: Boolean read fTransparent write SetTransparent default False;
    {$ENDIF}
  end;

  TCustomGuiBaseMouseControl = class(TCustomGuiBaseControl)
  protected
    fRedrawTimer            : TTimer;
    fTimerMustRedraw        : Boolean;
    fReleaseMouseBtnOnLeave : Boolean;
    fOnMouseLeave           : TNotifyEvent;
    fOnMouseEnter           : TNotifyEvent;
    fOnDragMouseMove        : TGuiOnDragMouseMove;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure CreateMouseClass(MouseStateClass: TGuiMouseStateClass); dynamic;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure DragMouseMoveMiddle(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure DragMouseMoveRight(Shift: TShiftState; X, Y: Integer); dynamic;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure SetRedrawInterval(Value: Integer); virtual;
    function  GetRedrawInterval: Integer; virtual;
  public
    MouseState: TGuiMouseState;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; MouseStateClass: TGuiMouseStateClass); reintroduce; overload;
    destructor Destroy; override;

    procedure UpdateGuiTimer(Sender: TObject); virtual;

    property RedrawInterval: Integer read GetRedrawInterval write SetRedrawInterval default 0;
    property ReleaseMouseBtnOnLeave: Boolean read fReleaseMouseBtnOnLeave write fReleaseMouseBtnOnLeave default False;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
    property OnDragMouseMove: TGuiOnDragMouseMove read fOnDragMouseMove write fOnDragMouseMove;
  end;

  TGuiBaseControl = class(TCustomGuiBaseMouseControl)
  published
    property Enabled;
    property Align;
    property Anchors;
    property Constraints;
    property ShowHint;
    property Visible;
    property PopupMenu;
    property DragKind;
    property DragCursor;
    property DragMode;

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
    property OnDragMouseMove;
  end;

procedure Downsample2xBitmap32(var Bitmap: TBitmap);
procedure Downsample4xBitmap32(var Bitmap: TBitmap);
procedure Upsample2xBitmap32(var Bitmap: TBitmap);
procedure Upsample4xBitmap32(var Bitmap: TBitmap);

implementation

uses
  SysUtils;

procedure Downsample2xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  Line : Array [0..2] of PRGB32Array;
begin
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 2) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 2];
     Line[2] := Scanline[y * 2 + 1];
     for x := 0 to (Width  div 2) - 1 do
      begin
       Line[0, x].B := (Line[1, 2 * x].B + Line[2, 2 * x].B + Line[1, 2 * x + 1].B + Line[2, 2 * x + 1].B) div 4;
       Line[0, x].G := (Line[1, 2 * x].G + Line[2, 2 * x].G + Line[1, 2 * x + 1].G + Line[2, 2 * x + 1].G) div 4;
       Line[0, x].R := (Line[1, 2 * x].R + Line[2, 2 * x].R + Line[1, 2 * x + 1].R + Line[2, 2 * x + 1].R) div 4;
       Line[0, x].A := (Line[1, 2 * x].A + Line[2, 2 * x].A + Line[1, 2 * x + 1].A + Line[2, 2 * x + 1].A) div 4;
      end;
    end;
  end;
end;

procedure Downsample4xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  Line : Array [0..4] of PRGB32Array;
begin
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 4) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 4];
     Line[2] := Scanline[y * 4 + 1];
     Line[3] := Scanline[y * 4 + 2];
     Line[4] := Scanline[y * 4 + 3];
     for x := 0 to (Width  div 4) - 1 do
      begin
       Line[0, x].B := (Line[1, 4 * x].B + Line[1, 4 * x + 1].B + Line[1, 4 * x + 2].B + Line[1, 4 * x + 3].B +
                        Line[2, 4 * x].B + Line[2, 4 * x + 1].B + Line[2, 4 * x + 2].B + Line[2, 4 * x + 3].B +
                        Line[3, 4 * x].B + Line[3, 4 * x + 1].B + Line[3, 4 * x + 2].B + Line[3, 4 * x + 3].B +
                        Line[4, 4 * x].B + Line[4, 4 * x + 1].B + Line[4, 4 * x + 2].B + Line[4, 4 * x + 3].B) div 16;
       Line[0, x].G := (Line[1, 4 * x].G + Line[1, 4 * x + 1].G + Line[1, 4 * x + 2].G + Line[1, 4 * x + 3].G +
                        Line[2, 4 * x].G + Line[2, 4 * x + 1].G + Line[2, 4 * x + 2].G + Line[2, 4 * x + 3].G +
                        Line[3, 4 * x].G + Line[3, 4 * x + 1].G + Line[3, 4 * x + 2].G + Line[3, 4 * x + 3].G +
                        Line[4, 4 * x].G + Line[4, 4 * x + 1].G + Line[4, 4 * x + 2].G + Line[4, 4 * x + 3].G) div 16;
       Line[0, x].R := (Line[1, 4 * x].R + Line[1, 4 * x + 1].R + Line[1, 4 * x + 2].R + Line[1, 4 * x + 3].G +
                        Line[2, 4 * x].R + Line[2, 4 * x + 1].R + Line[2, 4 * x + 2].R + Line[2, 4 * x + 3].G +
                        Line[3, 4 * x].R + Line[3, 4 * x + 1].R + Line[3, 4 * x + 2].R + Line[3, 4 * x + 3].G +
                        Line[4, 4 * x].R + Line[4, 4 * x + 1].R + Line[4, 4 * x + 2].R + Line[4, 4 * x + 3].G) div 16;
       Line[0, x].A := (Line[1, 4 * x].A + Line[1, 4 * x + 1].A + Line[1, 4 * x + 2].A + Line[1, 4 * x + 3].A +
                        Line[2, 4 * x].A + Line[2, 4 * x + 1].A + Line[2, 4 * x + 2].A + Line[2, 4 * x + 3].A +
                        Line[3, 4 * x].A + Line[3, 4 * x + 1].A + Line[3, 4 * x + 2].A + Line[3, 4 * x + 3].A +
                        Line[4, 4 * x].A + Line[4, 4 * x + 1].A + Line[4, 4 * x + 2].A + Line[4, 4 * x + 3].A) div 16;
      end;
    end;
  end;
end;

procedure Upsample2xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  Line : Array [0..2] of PRGB32Array;
begin
 with Bitmap do
  begin
   assert(PixelFormat = pf32bit);

   // first stage
   for y := (Height div 2) - 1 downto 0 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 2];
     Line[2] := Scanline[y * 2 + 1];
     for x := (Width  div 2) - 1 downto 0 do
      begin
       Line[1, 2 * x].B     := Line[0, x].B;
       Line[2, 2 * x].B     := Line[0, x].B;
       Line[1, 2 * x + 1].B := Line[0, x].B;
       Line[2, 2 * x + 1].B := Line[0, x].B;
       Line[1, 2 * x].G     := Line[0, x].G;
       Line[2, 2 * x].G     := Line[0, x].G;
       Line[1, 2 * x + 1].G := Line[0, x].G;
       Line[2, 2 * x + 1].G := Line[0, x].G;
       Line[1, 2 * x].R     := Line[0, x].R;
       Line[2, 2 * x].R     := Line[0, x].R;
       Line[1, 2 * x + 1].R := Line[0, x].R;
       Line[2, 2 * x + 1].R := Line[0, x].R;
       Line[1, 2 * x].A     := Line[0, x].A;
       Line[2, 2 * x].A     := Line[0, x].A;
       Line[1, 2 * x + 1].A := Line[0, x].A;
       Line[2, 2 * x + 1].A := Line[0, x].A;
      end;
    end;
  end;
end;

procedure Upsample4xBitmap32(var Bitmap: TBitmap);
var
  x, y : Integer;
  i, j : Integer;
  Line : Array [0..4] of PRGB32Array;
begin
 with Bitmap do
  for y := (Height div 4) - 1 downto 0 do
   begin
    assert(PixelFormat = pf32bit);

    Line[0] := Scanline[y];
    Line[1] := Scanline[y * 4];
    Line[2] := Scanline[y * 4 + 1];
    Line[3] := Scanline[y * 4 + 2];
    Line[4] := Scanline[y * 4 + 3];
    for x := (Width  div 4) - 1 downto 0 do
     for i := 1 to 4 do
      for j := 0 to 3 do
       begin
        Line[i, 4 * x + j].B := Line[0, x].B;
        Line[i, 4 * x + j].G := Line[0, x].G;
        Line[i, 4 * x + j].R := Line[0, x].R;
        Line[i, 4 * x + j].A := Line[0, x].A;
       end;
   end;
end;

{ TCustomGuiBaseControl }

constructor TBufferedGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  fBuffer      := TBitmap.Create;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csReplicatable, csOpaque];
end;

destructor TBufferedGraphicControl.Destroy;
begin
 FreeAndNil(fBuffer);
 inherited;
end;

procedure TBufferedGraphicControl.DrawParentImage(Dest: TCanvas);
var
  SaveIndex : Integer;
  DC        : THandle;
  Position  : TPoint;
begin
  if Parent = nil then Exit;
  DC := Dest.Handle;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, Position);
  SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
  IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
  Parent.Perform(WM_ERASEBKGND, Longint(DC), 0);
  Parent.Perform(WM_PAINT, Longint(DC), 0);
  RestoreDC(DC, SaveIndex);
end;

procedure TBufferedGraphicControl.Downsample2xBitmap(var Bitmap: TBitmap);
begin
 Downsample2xBitmap32(Bitmap);
end;

procedure TBufferedGraphicControl.Downsample4xBitmap(var Bitmap: TBitmap);
begin
 Downsample4xBitmap32(Bitmap);
end;

procedure TBufferedGraphicControl.Upsample2xBitmap(var Bitmap: TBitmap);
begin
 Upsample2xBitmap32(Bitmap);
end;

procedure TBufferedGraphicControl.Upsample4xBitmap(var Bitmap: TBitmap);
begin
 Upsample4xBitmap32(Bitmap);
end;

procedure TBufferedGraphicControl.Loaded;
begin
 inherited;
 ResizeBuffer;
end;

procedure TBufferedGraphicControl.Paint;
begin
  with Canvas do
   begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, fBuffer);
   end;
  if Assigned(fOnPaint) then fOnPaint(Self);
end;

procedure TBufferedGraphicControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;

{$IFNDEF COMPILER10_UP}
procedure TBufferedGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TBufferedGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;
{$ENDIF}

procedure TBufferedGraphicControl.ResizeBuffer;
begin
  if (Width > 0) and (Height > 0) then
   begin
    fBuffer.Width := Width;
    fBuffer.Height := Height;
    RedrawBuffer(True);
   end;
end;

procedure TBufferedGraphicControl.Resize;
begin
  inherited Resize;
  ResizeBuffer;
end;

procedure TBufferedGraphicControl.CMColorchanged(var Message: TMessage);
begin
  RedrawBuffer(True);
end;

procedure TBufferedGraphicControl.CMEnabledChanged(var Message: TMessage);
begin
  RedrawBuffer(True);
end;

procedure TBufferedGraphicControl.CMFontChanged(var Message: TMessage);
begin
  RedrawBuffer(True);
end;

{ TCustomGuiBaseControl }

constructor TCustomGuiBaseControl.Create(AOwner: TComponent);
begin
  inherited;
  fLineWidth   := 1;
  fLineColor   := clBlack;
  fTransparent := False;
end;

procedure TCustomGuiBaseControl.SetLineColor(Value: TColor);
begin
  if fLineColor <> Value then
   begin
    fLineColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiBaseControl.SetLinewidth(Value: Integer);
begin
  if (Value > 0) and (Value < 200) and (fLineWidth <> Value) then
  begin
    fLineWidth := Value;
    RedrawBuffer(True);
  end;
end;

{$IFNDEF FPC}
procedure TCustomGuiBaseControl.SetTransparent(Value: Boolean);
begin
 if fTransparent <> Value then
  begin
   fTransparent := Value;
   RedrawBuffer(True);
  end;
end;
{$ENDIF}

{ TCustomGuiBaseMouseControl }

constructor TCustomGuiBaseMouseControl.Create(AOwner: TComponent);
begin
  inherited;
  fReleaseMouseBtnOnLeave := False;
  fRedrawTimer            := TTimer.Create(self);
  fRedrawTimer.Interval   := 0;
  fRedrawTimer.OnTimer    := UpdateGuiTimer;
  fTimerMustRedraw        := False;

  CreateMouseClass(TGuiMouseState);
end;

constructor TCustomGuiBaseMouseControl.Create(AOwner: TComponent;
  MouseStateClass: TGuiMouseStateClass);
begin
  Create(AOwner);

  CreateMouseClass(MouseStateClass);
end;

destructor TCustomGuiBaseMouseControl.Destroy;
begin
  FreeAndNil(fRedrawTimer);
  if assigned(MouseState) then FreeAndNil(MouseState);
  inherited;
end;

procedure TCustomGuiBaseMouseControl.CreateMouseClass(MouseStateClass: TGuiMouseStateClass);
begin
 MouseState := MouseStateClass.Create;
 with MouseState do
  begin
   LeftBtn.ButtonDown   := False;
   MiddleBtn.ButtonDown := False;
   RightBtn.ButtonDown  := False;
   LastEventX := 0;
   LastEventY := 0;
  end;
end;

procedure TCustomGuiBaseMouseControl.CMMouseEnter(var Message: TMessage);
begin
  MouseEnter;
end;

procedure TCustomGuiBaseMouseControl.CMMouseLeave(var Message: TMessage);
begin
  MouseLeave;
end;

procedure TCustomGuiBaseMouseControl.DragMouseMoveLeft(Shift: TShiftState; X,
  Y: Integer);
begin
  if assigned(fOnDragMouseMove) then fOnDragMouseMove(self, mbLeft, Shift, X, Y);
end;

procedure TCustomGuiBaseMouseControl.DragMouseMoveMiddle(Shift: TShiftState; X,
  Y: Integer);
begin
  if assigned(fOnDragMouseMove) then fOnDragMouseMove(self, mbMiddle, Shift, X, Y);
end;

procedure TCustomGuiBaseMouseControl.DragMouseMoveRight(Shift: TShiftState; X,
  Y: Integer);
begin
  if assigned(fOnDragMouseMove) then fOnDragMouseMove(self, mbRight, Shift, X, Y);
end;

function TCustomGuiBaseMouseControl.GetRedrawInterval: Integer;
begin
  Result := fRedrawTimer.Interval;
end;

procedure TCustomGuiBaseMouseControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    inherited;
    MouseCapture := True;
//    Click;

    case Button of
      mbLeft:   with MouseState.LeftBtn do begin
                  ButtonDown := True;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbMiddle: with MouseState.MiddleBtn do begin
                  ButtonDown := True;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbRight:  with MouseState.RightBtn do begin
                  ButtonDown := True;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
    end;

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;
  end;
end;

procedure TCustomGuiBaseMouseControl.MouseEnter;
begin
 if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomGuiBaseMouseControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  if fReleaseMouseBtnOnLeave then
  begin
    with MouseState.LeftBtn   do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
    with MouseState.MiddleBtn do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
    with MouseState.RightBtn  do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
  end;
end;

procedure TCustomGuiBaseMouseControl.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  if Enabled then
  begin
    inherited;

    with MouseState.LeftBtn   do if ButtonDown then DragMouseMoveLeft(Shift, X, Y);
    with MouseState.MiddleBtn do if ButtonDown then DragMouseMoveMiddle(Shift, X, Y);
    with MouseState.RightBtn  do if ButtonDown then DragMouseMoveRight(Shift, X, Y);

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;
  end;
end;

procedure TCustomGuiBaseMouseControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    inherited;

    case Button of
      mbLeft:   with MouseState.LeftBtn do begin
                  ButtonDown := False;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbMiddle: with MouseState.MiddleBtn do begin
                  ButtonDown := False;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbRight:  with MouseState.RightBtn do begin
                  ButtonDown := False;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
    end;

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;

    MouseCapture := MouseState.LeftBtn.ButtonDown or MouseState.MiddleBtn.ButtonDown or MouseState.RightBtn.ButtonDown;
  end;
end;

procedure TCustomGuiBaseMouseControl.SetRedrawInterval(Value: Integer);
begin
  fRedrawTimer.Interval := Value;
end;

procedure TCustomGuiBaseMouseControl.UpdateGuiTimer(Sender: TObject);
begin
  if not fTimerMustRedraw then exit;
  
  fRedrawTimer.Enabled := False;
  RedrawBuffer(True);
  fRedrawTimer.Enabled := True;

  fTimerMustRedraw := False;
end;

{ TGUIShadow }

constructor TGUIShadow.Create;
begin
 fBlur         := 4;
 fOffset.X     := 1;
 fOffset.Y     := 1;
 fTransparency := $FF;
 fVisible      := False;
end;

procedure TGUIShadow.Changed;
begin
 if assigned(fOnChange)
  then fOnChange(Self);
end;

function TGUIShadow.GetOffsetX: Integer;
begin
 result := fOffset.X;
end;

function TGUIShadow.GetOffsetY: Integer;
begin
 result := fOffset.Y;
end;

procedure TGUIShadow.SetBlur(const Value: Byte);
begin
 if fBlur <> Value then
  begin
   fBlur := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetOffset(const Value: TPoint);
begin
 if (fOffset.X <> Value.X) or (fOffset.Y <> Value.Y) then
  begin
   fOffset := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetOffsetX(const Value: Integer);
begin
 if fOffset.X <> Value then
  begin
   fOffset.X := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetOffsetY(const Value: Integer);
begin
 if fOffset.Y <> Value then
  begin
   fOffset.Y := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetTransparency(const Value: Byte);
begin
 if fTransparency <> Value then
  begin
   fTransparency := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetVisible(const Value: Boolean);
begin
 if fVisible <> Value then
  begin
   fVisible := Value;
   Changed;
  end;
end;

end.
