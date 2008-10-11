unit DAV_CorrelationMeter;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics;

type
  TCorrelationMeterEvent = procedure(Sender: TObject; const Canvas: TCanvas) of object;
  TCorrelationMeterDirection = (cmdHorizontal, cmdVertical);

  TGuiCorrelationMeter = class(TCustomControl)
  private
    fMargin    : Integer;
    fOnPaint   : TCorrelationMeterEvent;
    fCenter    : TPoint;
    fStart     : TRect;
    fWidth22   : Single;
    fHeight22  : Single;
    fDirection : TCorrelationMeterDirection;
    procedure SetMargin(const Value: Integer);
    procedure SetDirection(const Value: TCorrelationMeterDirection);
  protected
    procedure Paint; override;
    procedure ResetPositions;
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
    property Margin: Integer read fMargin write SetMargin default 4;
    property OnPaint: TCorrelationMeterEvent read fOnPaint write fOnPaint;
    property Direction: TCorrelationMeterDirection read fDirection write SetDirection default cmdHorizontal;
  end;

implementation

{ TGuiCorrelationMeter }

constructor TGuiCorrelationMeter.Create(AOwner: TComponent);
begin
  inherited;
  fMargin            := 0;
  fDirection         := cmdHorizontal;
  Color              := clBlack;
  ControlStyle       := ControlStyle + [csOpaque, csReplicatable];
  Canvas.Brush.Color := Color;
  Canvas.Pen.Style   := psDash;
  Canvas.Pen.Color   := clSilver;
  Font.Color         := clSilver;
  Canvas.Font.Color  := Font.Color;
  ResetPositions;
  DoubleBuffered     := True;
end;

destructor TGuiCorrelationMeter.Destroy;
begin
 inherited;
end;

procedure TGuiCorrelationMeter.Paint;
var
  i : Integer;
begin
  inherited;
  if fDirection = cmdHorizontal then
    with Canvas do
     begin
      Brush.Color := Color;
      FillRect(rect(0, 0, Width, Height));
      for i := 1 to 21 do
       begin
        MoveTo(round(i * fWidth22), fStart.Top + 16);
        LineTo(round(i * fWidth22), fStart.Top + 22);
        MoveTo(round(i * fWidth22), fStart.Bottom - 18);
        LineTo(round(i * fWidth22), fStart.Bottom - 24);
       end;
      Canvas.Font := Self.Font;
      TextOut(round(fWidth22 - 5), fStart.Top, '-1');
      TextOut(round(6 * fWidth22 - 6.8), fStart.Top, '-.5');
      TextOut(round(11 * fWidth22 - 2.8), fStart.Top, '0');
      TextOut(round(16 * fWidth22 - 6.8), fStart.Top, '+.5');
      TextOut(round(21 * fWidth22 - 5), fStart.Top, '+1');

      TextOut(round(fWidth22 - 9.2), fStart.Bottom - 14, '180');
      TextOut(round(6 * fWidth22 - 7.9), fStart.Bottom - 14, '135');
      TextOut(round(11 * fWidth22 - 5.7), fStart.Bottom - 14, '90');
      TextOut(round(16 * fWidth22 - 5.8), fStart.Bottom - 14, '45');
      TextOut(round(21 * fWidth22 - 2.8), fStart.Bottom - 14, '0');
     end
  else
    with Canvas do
     begin
      Brush.Color := Color;
      FillRect(rect(0, 0, Width, Height));
      for i := 1 to 21 do
       begin
        MoveTo(fStart.Left + 16, round(i * fHeight22));
        LineTo(fStart.Left + 22, round(i * fHeight22), );
        MoveTo(fStart.Right - 22, round(i * fHeight22));
        LineTo(fStart.Right - 28, round(i * fHeight22), );
       end;
      Canvas.Font := Self.Font;
      TextOut(fStart.Left + 2, round(fHeight22 - 7), '-1');
      TextOut(fStart.Left, round(6 * fHeight22 - 7), '-.5');
      TextOut(fStart.Left + 4, round(11 * fHeight22 - 7), '0');
      TextOut(fStart.Left, round(16 * fHeight22 - 7), '+.5');
      TextOut(fStart.Left + 2, round(21 * fHeight22 - 7), '+1');

      TextOut(fStart.Right - 19, round(fHeight22 - 7), '180');
      TextOut(fStart.Right - 19, round(6 * fHeight22 - 7), '135');
      TextOut(fStart.Right - 16, round(11 * fHeight22 - 7), '90');
      TextOut(fStart.Right - 16, round(16 * fHeight22 - 7), '45');
      TextOut(fStart.Right - 14, round(21 * fHeight22 - 7), '0');
     end
end;

procedure TGuiCorrelationMeter.ResetPositions;
begin
  fCenter.X := Width div 2;
  fCenter.Y := Height div 2;
  fStart.Left := fMargin;
  fStart.Top := fMargin;
  fStart.Right := Width - fMargin;
  fStart.Bottom := Height - fMargin;
  fWidth22 := Width / 22;
  fHeight22 := Height / 22;
  Invalidate;
end;

procedure TGuiCorrelationMeter.Resize;
begin
 inherited;
 ResetPositions;
end;

procedure TGuiCorrelationMeter.SetDirection(
  const Value: TCorrelationMeterDirection);
begin
  fDirection := Value;
  Invalidate;
end;

procedure TGuiCorrelationMeter.SetMargin(const Value: Integer);
begin
  fMargin := Value;
  ResetPositions;
end;

end.
