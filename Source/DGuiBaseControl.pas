unit DGuiBaseControl;

interface

{$I ASIOVST.INC}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Messages, Graphics, Classes, Controls, ExtCtrls;

type
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

  TBufferedGraphicControl = class(TGraphicControl)
  protected
    fBuffer   : TBitmap;
    fOnPaint  : TNotifyEvent;

    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}

    procedure Resize; override;
    procedure ResizeBuffer; dynamic;
    procedure RedrawBuffer(doBufferFlip: Boolean = False); dynamic; abstract;

    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
  end;

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

implementation

uses
  SysUtils;

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
  SaveIndex: Integer;
  DC: THandle;
  Position: TPoint;
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

procedure TBufferedGraphicControl.WMEraseBkgnd(var m: TWMEraseBkgnd);
begin
  m.Result := 0;
end;

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
    Click;

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

end.
