unit DGuiBaseControl;

interface

uses {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Messages, Graphics, Classes, Controls;

type
  TGuiBaseControl = class(TGraphicControl)
  protected
    fBuffer: TBitmap;
    
    fLineColor      : TColor;
    fLineWidth      : Integer;
    fMouseButtonDown: Boolean;

    {$IFNDEF FPC}
    fTransparent: Boolean;
    procedure DrawParentImage(Dest: TCanvas);
    procedure SetTransparent(const Value: Boolean);
    {$ENDIF}

    procedure SetLinewidth(const Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure RedrawBuffer(doBufferFlip: Boolean = false); dynamic; abstract;
    procedure ResizeBuffer; dynamic;
    procedure Resize; override;
    procedure ReadState(Reader: TReader); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
  public    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;  
    procedure Paint; override;
    
    property LineWidth: Integer read fLineWidth write SetLineWidth default 1;
    property LineColor: TColor read fLineColor write SetLineColor;
    {$IFNDEF FPC}
    property Transparent: Boolean read fTransparent write SetTransparent default False;
    {$ENDIF}
  end;

implementation


constructor TGuiBaseControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLineWidth := 1;
  fLineColor := clBlack;
  fTransparent := False; 
  fBuffer := TBitmap.Create;
  fMouseButtonDown := false;
  ControlStyle := ControlStyle+[csOpaque];
end;

destructor TGuiBaseControl.Destroy;
begin   
  fBuffer.Free;
  inherited;
end;

procedure TGuiBaseControl.Paint;
begin
  with Canvas do
  begin
    CopyMode := cmSrcCopy;
    Draw(0, 0, fBuffer);
  end;
end;

procedure TGuiBaseControl.ResizeBuffer;
begin
  fBuffer.Width := Width;
  fBuffer.Height := Height;
  RedrawBuffer(true);
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

procedure TGuiBaseControl.SetLineColor(const Value: TColor);
begin
  if fLineColor<>Value then
  begin
    fLineColor := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiBaseControl.SetLinewidth(const Value: Integer);
begin
  if (Value>0) and (Value<200) and (fLineWidth<>Value) then
  begin
    fLineWidth := Value;
    RedrawBuffer(true);
  end;
end;

{$IFNDEF FPC}  
procedure TGuiBaseControl.SetTransparent(const Value: Boolean);
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

procedure TGuiBaseControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 MouseCapture := True;
 fMouseButtonDown := True;
 inherited MouseDown(Button, Shift, X, Y);
end;


procedure TGuiBaseControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 MouseCapture := False;
 fMouseButtonDown:= False;
 inherited MouseUp(Button, Shift, X, Y);
end;

end.
