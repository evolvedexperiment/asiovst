unit DDial;

interface

uses Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
     Consts;

type
  TStitchKind = (skHorizontal, skVertical);

  TDialSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
  private
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected
    procedure Changed;
  public
    constructor Create; virtual;
  end;

  TDialPointerAngles = class(TDialSettings)
  private
    FResolution: Extended;
    FStart: Integer;
    FRange: Integer;
    procedure SetRange(const Value: Integer);
    procedure SetResolution(const Value: Extended);
    procedure SetStart(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
  published
    property Start: Integer read FStart write SetStart default 0;
    property Range: Integer read FRange write SetRange default 360;
    property Resolution: Extended read FResolution write SetResolution;
  end;

  TDial = class(TGraphicControl)
  private
    fOnPaint     : TNotifyEvent;
    fAutoSize    : Boolean;
    fTransparent : Boolean;
    fDialBitmap  : TBitmap;
    fStitchKind  : TStitchKind;
    fMin, fMax   : Single;
    fPosition    : Single;
    fNumGlyphs   : Integer;
    fPnterAngles : TDialPointerAngles;
    fOnChange    : TNotifyEvent;
    fMouseIsDown : Boolean;
    fOldMousPos  : TPoint;
    fColorCircle : TColor;
    fColorLine   : TColor;
    fColorAuto   : Boolean;
    fLineWidth   : Integer;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetColorAuto(const Value: Boolean);
    procedure SetLineWidth(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetColorCircle(const Value: TColor);
    procedure SetColorLine(const Value: TColor);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetPosition(Value: Single);
    procedure SetDialBitmap(const Value: TBitmap);
    procedure SetStitchKind(const Value: TStitchKind);
    function CircularMouseToPosition(X, Y: Integer): Single;
    procedure MouseTimerHandler(Sender: TObject);
    procedure SetPointerAngles(const Value: TDialPointerAngles);
    function PositionToAngle: Single;
  protected
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure CalcColorCircle;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Font;
    property BiDiMode;
    property Constraints;
    property Color;
    property ShowHint;
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
    property PopupMenu;
    property DragKind;
    property DragCursor;
    property DragMode;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
    property AutoSize: Boolean read fAutoSize write SetAutoSize default false;
    property ColorAuto: Boolean read fColorAuto write SetColorAuto default false;
    property Transparent: Boolean read fTransparent write SetTransparent default false;
    property Position: Single read FPosition write SetPosition;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property ColorCircle : TColor read fColorCircle write SetColorCircle default clBlack;
    property ColorLine : TColor read fColorLine write SetColorLine default clRed;
    property LineWidth : Integer read fLineWidth write SetLineWidth default 2;
    property NumGlyphs: Integer read fNumGlyphs write SetNumGlyphs default 1;
    property DialBitmap: TBitmap read fDialBitmap write SetDialBitmap;
    property StitchKind: TStitchKind read fStitchKind write SetStitchKind;
    property PointerAngles: TDialPointerAngles read FPnterAngles write SetPointerAngles;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

var MouseInDialControl: TDial = nil;

procedure Register;

implementation

uses ExtCtrls, Math;

var
  MouseTimer: TTimer = nil;
  ControlCounter: Integer = 0;

procedure Register;
begin
 RegisterComponents('Audio', [TDial]);
end;

function RadToDeg(const Radians: Extended): Extended;  { Degrees := Radians * 180 / PI }
const DegPi : Double = (180 / PI);
begin
 Result := Radians * DegPi;
end;

function ArcTan2(const Y, X: Extended): Extended;
asm
 fld y
 fld x
 fpatan
end;

function RelativeAngle(X1, Y1, X2, Y2: Integer): Single;
const MulFak=180/pi;
begin
 Result:=arctan2(X2-X1,Y1-Y2)*MulFak;
end;

function SafeAngle(Angle: Single): Single;
begin
 while Angle < 0 do Angle:=Angle+360;
 while Angle >= 360 do Angle:=Angle-360;
 Result := Angle;
end;

{ This function solves for x in the equation "x is y% of z". }
function SolveForX(Y, Z: Longint): Longint;
begin
 Result := round( Z * (Y * 0.01) );//tt
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
 if Z = 0
  then Result := 0
  else Result := round( (X * 100.0) / Z ); //t
end;

{ TDialSettings }

procedure TDialSettings.Changed;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TDialSettings.Create;
begin
 inherited;
end;

{ TDialPointerAngles }

procedure TDialPointerAngles.AssignTo(Dest: TPersistent);
begin
 if Dest is TDialPointerAngles then with TDialPointerAngles(Dest) do
  begin
   FRange := Self.Range;
   FStart := Self.Start;
   FResolution := Self.Resolution;
   Changed;
  end else inherited;
end;

constructor TDialPointerAngles.Create;
begin
 inherited;
 FStart := 0;
 FRange := 360;
 FResolution := 0;
end;

procedure TDialPointerAngles.SetRange(const Value: Integer);
begin
 if (Range < 1) or (Range > 360)
  then raise Exception.Create('Range must be 1..360');
 FRange := Value;
 if Range > Resolution then Resolution := Range;
 Changed;
end;

procedure TDialPointerAngles.SetResolution(const Value: Extended);
begin
 if (Value < 0) or (Value > Range)
  then raise Exception.Create('Resolution must be above 0 and less than ' + IntToStr(Range + 1));
 FResolution := Value;
 Changed;
end;

procedure TDialPointerAngles.SetStart(const Value: Integer);
begin
 if (Value < 0) or (Value > 359)
  then raise Exception.Create('Start must be 0..359');
 FStart := Value;
 Changed;
end;

{ TDial }

constructor TDial.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 if MouseTimer = nil then
  begin
   MouseTimer := TTimer.Create(nil);
   MouseTimer.Enabled := False;
   MouseTimer.Interval := 100;
  end;
 Inc(ControlCounter);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                  csDoubleClicks, csReplicatable, csOpaque];
 FPnterAngles := TDialPointerAngles.Create;
 FPnterAngles.OnChange := SettingsChanged;
 fColorCircle := clBlack;
 fColorLine := clRed;
 fLineWidth := 2;
 fMin := 0;
 fMax := 100;
 fPosition := 0;
 fNumGlyphs := 1;
 fStitchKind:=skHorizontal;
 fDialBitmap := TBitmap.Create;
end;

destructor TDial.Destroy;
begin
 FreeAndNil(fDialBitmap);
 FreeAndNil(fPnterAngles);
 Dec(ControlCounter);
 if ControlCounter = 0 then
  begin
   MouseTimer.Free;
   MouseTimer := nil;
  end;
 inherited Destroy;
end;

procedure TDial.SettingsChanged(Sender: TObject);
begin
 Invalidate;
end;

function TDial.PositionToAngle: Single;
var
  Percircle: Single;
  Range: Single;
const Pi180 : Double = PI/180;
begin
 Range := Max - Min;
 Percircle := (fPosition - Min) * 360 / Range;
 Result := SafeAngle(PointerAngles.Start + (PointerAngles.Range * Percircle / 360))* Pi180;
end;

procedure DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
 with Control do
  begin
   if Parent = nil then Exit;
   DC := Dest.Handle;
   SaveIndex := SaveDC(DC);
   GetViewportOrgEx(DC, Position);
   SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
   IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
   Parent.Perform(WM_ERASEBKGND, DC, 0);
   Parent.Perform(WM_PAINT, DC, 0);
   RestoreDC(DC, SaveIndex);
  end;
end;

procedure TDial.Paint;
type TComplex = record Re,Im : Single; end;
var theRect    : TRect;
    GlyphNr,i  : Integer;
    Val,Off    : TComplex;
    Rad,tmp    : Single;
    PtsArray   : Array of TPoint;
    DblBuffer  : TBitmap;


  procedure GetSinCos(Frequency: Single; var SinValue, CosValue : Single);
  asm
   fld Frequency.Single;
   fsincos
   fstp [CosValue].Single;
   fstp [SinValue].Single;
  end;

begin
 inherited;
 with Canvas do
  begin
   if fDialBitmap.Empty then
    begin
     Lock;
     DblBuffer := TBitmap.Create;
     DblBuffer.SetSize(Width, Height);
     with DblBuffer.Canvas do
      begin
       Brush.Color := Self.Color;
       if fTransparent
        then DrawParentImage(Self, Canvas)
        else FillRect(ClientRect);
       FillRect(ClientRect);
       Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
       GlyphNr:=Round(2 / arcsin(1 / Rad)) + 1;
       if GlyphNr > 1 then
        begin
         SetLength(PtsArray, GlyphNr);
         GetSinCos(PositionToAngle - (PI * 0.5), Val.Im, Val.Re);
         Val.Re := Val.Re * Rad; Val.Im := Val.Im * Rad;
         GetSinCos(2 * Pi / (GlyphNr - 1), Off.Im, Off.Re);
         PtsArray[0] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
         for i:=1 to GlyphNr - 1 do
          begin
           tmp := Val.Re * Off.Re - Val.Im * Off.Im;
           Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
           Val.Re := tmp;
           PtsArray[i] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
          end;
         Pen.Width := fLineWidth;
         Pen.Color := fColorLine;
         Brush.Color := fColorCircle;
         Polygon(PtsArray);
        end;
       MoveTo(PtsArray[0].X, PtsArray[0].Y);
       LineTo(Round(0.5 * Width), Round(0.5 * Height));
      end;
     Draw(0,0, DblBuffer);
     DblBuffer.Free;
     Unlock;
    end
   else
    begin
     GlyphNr:=Trunc((fPosition - fMin) / ((fMax + 1) - fMin) * fNumGlyphs);
     theRect:=ClientRect;
     if fStitchKind=skVertical then
      begin
       theRect.Top := fDialBitmap.Height * GlyphNr div fNumGlyphs;
       theRect.Bottom := fDialBitmap.Height * (GlyphNr+1) div fNumGlyphs;
      end
     else
      begin
       theRect.Left := fDialBitmap.Width * GlyphNr div fNumGlyphs;
       theRect.Right := fDialBitmap.Width * (GlyphNr+1) div fNumGlyphs;
      end;
     Draw(0,0,fDialBitmap);
    end;
   if Assigned(fOnPaint) then fOnPaint(Self);
  end;
end;

procedure TDial.Resize;
begin
 try
  Width:=ClientRect.Right-ClientRect.Left;
  Height:=ClientRect.Bottom-ClientRect.Top;
 except
 end;
end;

procedure TDial.CMFontChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TDial.CMColorChanged(var Message: TMessage);
begin
 inherited;
end;

procedure TDial.DoAutoSize;
begin
 if fNumGlyphs=0 then Exit;
 if fStitchKind=skVertical then
  begin
   Width:=fDialBitmap.Width;
   Height:=fDialBitmap.Height div fNumGlyphs;
  end
 else
  begin
   Width:=fDialBitmap.Width div fNumGlyphs;
   Height:=fDialBitmap.Height;
  end;
end;

procedure TDial.SetAutoSize(const Value: boolean);
begin
 if fAutoSize<>Value then
  begin
   fAutoSize := Value;
   if Autosize then DoAutoSize;
   Invalidate;
  end;
end;

procedure TDial.SetTransparent(const Value: Boolean);
begin
 if fTransparent<>Value then
  begin
   fTransparent := Value;
   Invalidate;
  end;
end;

procedure TDial.SetMax(const Value: Single);
begin
 if Value <> FMax then
  begin
   if Value < FMin then
    if not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin + 1, MaxInt]);
   FMax := Value;
   if fPosition > Value then fPosition := Value;
   Invalidate;
  end;
end;

procedure TDial.SetMin(const Value: Single);
begin
 if Value <> FMin then
  begin
   if Value > FMax then
    if not (csLoading in ComponentState)
     then raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMax - 1]);
   FMin := Value;
   if fPosition < Value then fPosition := Value;
   Invalidate;
  end;
end;

procedure TDial.SetNumGlyphs(const Value: Integer);
begin
 if fNumGlyphs <> Value then
  begin
   fNumGlyphs := Value;
   DoAutoSize;
   Invalidate;
  end;
end;

procedure TDial.SetPosition(Value: Single);
begin
 if Value < FMin then Value := FMin else
 if Value > FMax then Value := FMax;
 if fPosition <> Value then
  begin
   fPosition := Value;
   if not (csLoading in ComponentState) then
    if Assigned(FOnChange)
     then FOnChange(Self);
   Invalidate;
  end;
end;

procedure TDial.SetDialBitmap(const Value: TBitmap);
begin
 fDialBitmap.Assign(Value);
 DoAutoSize;
 Invalidate;
end;

procedure TDial.SetStitchKind(const Value: TStitchKind);
begin
 if fStitchKind <> Value then
  begin
   fStitchKind := Value;
   DoAutoSize;
   Invalidate;
  end;
end;

function TDial.CircularMouseToPosition(X, Y: Integer): Single;
var
  Range: Single;
  Angle: Single;
begin
  Range := Max - (Min - 1);
  Angle := SafeAngle(RelativeAngle(Width div 2, Height div 2, X, Y) - PointerAngles.Start);
  Result := Angle * Range / PointerAngles.Range;
  while Result > Max do Result:=Result-Range;
  while Result < Min do Result:=Result+Range;
  if Result > Max
   then Result := fPosition;
  if Result < Min
   then Result := fPosition;
end;

procedure TDial.MouseTimerHandler (Sender: TObject);
var
  P: TPoint;
begin
 GetCursorPos (P);
// if FindDragTarget(P, True) <> Self
//  then MouseLeave;
end;

procedure TDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
//   SetFocus;
   fMouseIsDown := true;
   fOldMousPos.X:=X; fOldMousPos.Y:=Y;
   Click;
   inherited MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TDial.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
   fMouseIsDown := false;
   fOldMousPos.X:=X; fOldMousPos.Y:=Y;
   inherited MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TDial.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Range: Single;
begin
 inherited;
// if fMouseIsDown then Position := CircularMouseToPosition(X, Y);

 Range := Max - (Min - 1);
 if fMouseIsDown then
 if ssCtrl in Shift
  then Position := Position + (fOldMousPos.Y - Y)*0.001*Range
  else Position := Position + (fOldMousPos.Y - Y)*0.005*Range;
 fOldMousPos.X:=X; fOldMousPos.Y:=Y;

 P := ClientToScreen(Point(X, Y));
 if (MouseInDialControl <> Self) and (FindDragTarget(P, True) = Self) then
  begin
//   if Assigned(MouseInDialControl) then MouseInDialControl.MouseLeave;
   if (GetActiveWindow <> 0) then
    begin
     if MouseTimer.Enabled then MouseTimer.Enabled := False;
     MouseInDialControl := Self;
     MouseTimer.OnTimer := MouseTimerHandler;
     MouseTimer.Enabled := True;
//     MouseEnter;
    end;
  end;
end;

procedure TDial.SetPointerAngles(const Value: TDialPointerAngles);
begin
 FPnterAngles.Assign(Value);
end;

procedure TDial.CalcColorCircle;
begin
 if (Color and $000000FF)<$80 then
  if (((Color and $0000FF00) shr  8)<$80) or
     (((Color and $00FF0000) shr 16)<$80) then fColorCircle:=$FFFFFF
  else
 if (((Color and $0000FF00) shr  8)<$80) and
    (((Color and $00FF0000) shr 16)<$80) then fColorCircle:=$FFFFFF;
 Invalidate;
end;

procedure TDial.SetColorAuto(const Value: Boolean);
begin
 CalcColorCircle;
end;

procedure TDial.SetColorCircle(const Value: TColor);
begin
 if not fColorAuto and (Value<>fColorCircle) then
  begin
   fColorCircle:=Value;
   Invalidate;
  end;
end;

procedure TDial.SetColorLine(const Value: TColor);
begin
 if not fColorAuto and (Value<>fColorLine) then
  begin
   fColorLine:=Value;
   Invalidate;
  end;
end;

procedure TDial.SetLineWidth(const Value: Integer);
begin
 if (Value<>fLineWidth) then
  begin
   fLineWidth:=Value;
   Invalidate;
  end;
end;

end.
