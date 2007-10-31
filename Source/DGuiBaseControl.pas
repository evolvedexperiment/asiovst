unit DGuiBaseControl;

interface

uses {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Messages, Graphics, Classes, Controls, ExtCtrls;

type
  TGuiOnDragMouseMove = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TGuiMouseButtonState = record
    ButtonDown: boolean;
    EventX, EventY: Integer;
    ShiftState:  TShiftState;
  end;

  TGuiMouseState = class
    LeftBtn,
    MiddleBtn,
    RightBtn: TGuiMouseButtonState;
    LastEventX, LastEventY: Integer;
  end;

  TGuiMouseStateClass = class of TGuiMouseState;

  TGuiBaseControl = class(TGraphicControl)
  protected
    fBuffer:          TBitmap;
    fOnPaint:         TNotifyEvent;
    fLineColor:       TColor;
    fLineWidth:       Integer;
    fRedrawTimer:     TTimer;
    fTimerMustRedraw: Boolean;
    fReleaseMouseBtnOnLeave: Boolean;
    fOnMouseLeave:    TNotifyEvent;
    fOnMouseEnter:    TNotifyEvent;
    fOnDragMouseMove: TGuiOnDragMouseMove;
    {$IFNDEF FPC}
    fTransparent: Boolean;
    procedure DrawParentImage(Dest: TCanvas); virtual;
    procedure SetTransparent(Value: Boolean); virtual;
    {$ENDIF}

    procedure SetLineWidth(Value: Integer); virtual;
    procedure SetLineColor(Value: TColor); virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean = false); dynamic; abstract;
    procedure ResizeBuffer; dynamic;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure CreateMouseClass(MouseStateClass: TGuiMouseStateClass); dynamic;
    
    procedure Resize; override;
    procedure ReadState(Reader: TReader); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure DragMouseMoveMiddle(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure DragMouseMoveRight(Shift: TShiftState; X, Y: Integer); dynamic;

    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    
    procedure SetRedrawInterval(Value: Integer); virtual;
    function  GetRedrawInterval: Integer; virtual;
  public
    MouseState: TGuiMouseState;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; MouseStateClass: TGuiMouseStateClass); reintroduce; overload;
    destructor Destroy; override;
    procedure Paint; override;

    procedure UpdateGuiTimer(Sender: TObject); virtual;

    property LineWidth: Integer read fLineWidth write SetLineWidth default 1;
    property LineColor: TColor read fLineColor write SetLineColor default clBlack;
    {$IFNDEF FPC}
    property Transparent: Boolean read fTransparent write SetTransparent default False;
    {$ENDIF}   
    property RedrawInterval: Integer read GetRedrawInterval write SetRedrawInterval default 0;
    property ReleaseMouseBtnOnLeave: Boolean read fReleaseMouseBtnOnLeave write fReleaseMouseBtnOnLeave default false;
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
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;  
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
    property OnDragMouseMove: TGuiOnDragMouseMove read fOnDragMouseMove write fOnDragMouseMove;
  end;

implementation

constructor TGuiBaseControl.Create(AOwner: TComponent);
begin
  inherited;

  fLineWidth := 1;
  fLineColor := clBlack;
  fTransparent := False;
  fBuffer := TBitmap.Create;

  fReleaseMouseBtnOnLeave := false;
  fRedrawTimer          := TTimer.Create(self);
  fRedrawTimer.Interval := 0;
  fRedrawTimer.OnTimer  := UpdateGuiTimer;
  fTimerMustRedraw      := false;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csReplicatable, csOpaque];

  CreateMouseClass(TGuiMouseState);
end;


constructor TGuiBaseControl.Create(AOwner: TComponent; MouseStateClass: TGuiMouseStateClass);
begin
  Create(AOwner);

  CreateMouseClass(MouseStateClass);
end;

destructor TGuiBaseControl.Destroy;
begin
  fRedrawTimer.Free;
  fBuffer.Free;
  inherited;
end;

procedure TGuiBaseControl.CreateMouseClass(MouseStateClass: TGuiMouseStateClass);
begin
  MouseState:=MouseStateClass.Create;
  MouseState.LeftBtn.ButtonDown := false;
  MouseState.MiddleBtn.ButtonDown := false;
  MouseState.RightBtn.ButtonDown := false;
  MouseState.LastEventX:=0;
  MouseState.LastEventY:=0;
end;

procedure TGuiBaseControl.Paint;
begin
  with Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, fBuffer);
  end;   
   if Assigned(fOnPaint) then fOnPaint(Self);
end;

procedure TGuiBaseControl.ResizeBuffer;
begin
  if (Width>0) and (Height>0) then
  begin
    fBuffer.Width := Width;
    fBuffer.Height := Height;
    RedrawBuffer(true);
  end;
end;

procedure TGuiBaseControl.Resize;
begin
  inherited Resize;
  ResizeBuffer;
end;

procedure TGuiBaseControl.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  ResizeBuffer;
end;

procedure TGuiBaseControl.SetLineColor(Value: TColor);
begin
  if fLineColor<>Value then
  begin
    fLineColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiBaseControl.SetLinewidth(Value: Integer);
begin
  if (Value>0) and (Value<200) and (fLineWidth<>Value) then
  begin
    fLineWidth := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiBaseControl.SetRedrawInterval(Value: Integer);
begin
  fRedrawTimer.Interval := Value;
end;

function TGuiBaseControl.GetRedrawInterval: Integer;
begin
  Result:=fRedrawTimer.Interval;
end;

procedure TGuiBaseControl.UpdateGuiTimer(Sender: TObject);
begin
  if not fTimerMustRedraw then exit;
  
  fRedrawTimer.Enabled:=false;
  RedrawBuffer(true);
  fRedrawTimer.Enabled:=true;

  fTimerMustRedraw:=false;
end;

{$IFNDEF FPC}  
procedure TGuiBaseControl.SetTransparent(Value: Boolean);
begin
  if fTransparent<>Value then
  begin
    fTransparent := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiBaseControl.DrawParentImage(Dest: TCanvas);
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
{$ENDIF}

procedure TGuiBaseControl.WMEraseBkgnd(var m: TWMEraseBkgnd);
begin
  m.Result := 0;
end;

procedure TGuiBaseControl.CMColorchanged(var Message: TMessage);
begin
  RedrawBuffer(True);
end;

procedure TGuiBaseControl.CMEnabledChanged(var Message: TMessage);
begin
  RedrawBuffer(True);
end;

procedure TGuiBaseControl.CMFontChanged(var Message: TMessage);
begin
  RedrawBuffer(True);
end;

procedure TGuiBaseControl.CMMouseEnter(var Message: TMessage);
begin
  MouseEnter;
end;

procedure TGuiBaseControl.CMMouseLeave(var Message: TMessage);
begin
  MouseLeave;
end;

procedure TGuiBaseControl.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TGuiBaseControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  if fReleaseMouseBtnOnLeave then
  begin
    with MouseState.LeftBtn   do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
    with MouseState.MiddleBtn do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
    with MouseState.RightBtn  do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
  end;
end;

procedure TGuiBaseControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    inherited;
    MouseCapture := True;
    Click;

    case Button of
      mbLeft:   with MouseState.LeftBtn do begin
                  ButtonDown := true;
                  EventX:=X;
                  EventY:=Y;
                  ShiftState:=Shift;
                end;
      mbMiddle: with MouseState.MiddleBtn do begin
                  ButtonDown := true;
                  EventX:=X;
                  EventY:=Y;
                  ShiftState:=Shift;
                end;
      mbRight:  with MouseState.RightBtn do begin
                  ButtonDown := true;
                  EventX:=X;
                  EventY:=Y;
                  ShiftState:=Shift;
                end;
    end;

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;
  end;
end;


procedure TGuiBaseControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    inherited;

    case Button of
      mbLeft:   with MouseState.LeftBtn do begin
                  ButtonDown := false;
                  EventX:=X;
                  EventY:=Y;
                  ShiftState:=Shift;
                end;
      mbMiddle: with MouseState.MiddleBtn do begin
                  ButtonDown := false;
                  EventX:=X;
                  EventY:=Y;
                  ShiftState:=Shift;
                end;
      mbRight:  with MouseState.RightBtn do begin
                  ButtonDown := false;
                  EventX:=X;
                  EventY:=Y;
                  ShiftState:=Shift;
                end;
    end;

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;

    MouseCapture := MouseState.LeftBtn.ButtonDown or MouseState.MiddleBtn.ButtonDown or MouseState.RightBtn.ButtonDown;
  end;
end;

procedure TGuiBaseControl.DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer);
begin
  if assigned(fOnDragMouseMove) then fOnDragMouseMove(self, mbLeft, Shift, X, Y);
end;

procedure TGuiBaseControl.DragMouseMoveMiddle(Shift: TShiftState; X, Y: Integer);
begin
  if assigned(fOnDragMouseMove) then fOnDragMouseMove(self, mbMiddle, Shift, X, Y);
end;

procedure TGuiBaseControl.DragMouseMoveRight(Shift: TShiftState; X, Y: Integer);
begin
  if assigned(fOnDragMouseMove) then fOnDragMouseMove(self, mbRight, Shift, X, Y);
end;


procedure TGuiBaseControl.MouseMove(Shift: TShiftState; X, Y: Integer);
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

end.
