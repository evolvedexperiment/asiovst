unit DAV_CorrelationMeter;

interface

{$I ..\ASIOVST.INC}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, Graphics;

type
  TCorrelationMeterDirection = (cmdHorizontal, cmdVertical);
  TGuiCorrelationMeter = class(TCustomControl)
  private
    FMargin      : Integer;
    FCenter      : TPoint;
    FStart       : TRect;
    FWidth22     : Single;
    FHeight22    : Single;
    FDirection   : TCorrelationMeterDirection;
    FCorrelation : Single;
    procedure SetMargin(const Value: Integer);
    procedure SetDirection(const Value: TCorrelationMeterDirection);
    procedure SetCorrelation(Value: Single);
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
    {$IFNDEF FPC}
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
    property PopupMenu;
    property Correlation: Single read FCorrelation write SetCorrelation;
    property Margin: Integer read FMargin write SetMargin default 4;
    property Direction: TCorrelationMeterDirection read FDirection write SetDirection default cmdHorizontal;
  end;

implementation

uses
  DAV_Common;

{ TGuiCorrelationMeter }

constructor TGuiCorrelationMeter.Create(AOwner: TComponent);
begin
  inherited;
  FMargin            := 0;
  FDirection         := cmdHorizontal;
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
  if FDirection = cmdHorizontal then
   with Canvas do
    begin
     Brush.Color := Color;
     FillRect(rect(0, 0, Width, Height));
     for i := 1 to 21 do
      begin
       MoveTo(round(i * FWidth22), FStart.Top + 16);
       LineTo(round(i * FWidth22), FStart.Top + 22);
       MoveTo(round(i * FWidth22), FStart.Bottom - 18);
       LineTo(round(i * FWidth22), FStart.Bottom - 24);
      end;
     Canvas.Font := Self.Font;
     TextOut(round(FWidth22 - 5), FStart.Top, '-1');
     TextOut(round(6 * FWidth22 - 6.8), FStart.Top, '-.5');
     TextOut(round(11 * FWidth22 - 2.8), FStart.Top, '0');
     TextOut(round(16 * FWidth22 - 6.8), FStart.Top, '+.5');
     TextOut(round(21 * FWidth22 - 5), FStart.Top, '+1');

     TextOut(round(FWidth22 - 9.2), FStart.Bottom - 14, '180');
     TextOut(round(6 * FWidth22 - 7.9), FStart.Bottom - 14, '135');
     TextOut(round(11 * FWidth22 - 5.7), FStart.Bottom - 14, '90');
     TextOut(round(16 * FWidth22 - 5.8), FStart.Bottom - 14, '45');
     TextOut(round(21 * FWidth22 - 2.8), FStart.Bottom - 14, '0');
     if FCorrelation > 0 then
      begin
       Brush.Color := clLime;
       FillRect(Rect(FCenter.X, FStart.Top + 22,
         round(FCenter.X + FCorrelation * (Width - FCenter.X)), FStart.Bottom - 24));
      end else
     if FCorrelation < 0 then
      begin
       Brush.Color := clRed;
       FillRect(Rect(round(FCenter.X + FCorrelation * FCenter.X), FStart.Top + 22,
         FCenter.X, FStart.Bottom - 24));
      end;
    end
  else
   with Canvas do
    begin
     Brush.Color := Color;
     FillRect(rect(0, 0, Width, Height));
     for i := 1 to 21 do
      begin
       MoveTo(FStart.Left + 16, round(i * FHeight22));
       LineTo(FStart.Left + 22, round(i * FHeight22));
       MoveTo(FStart.Right - 22, round(i * FHeight22));
       LineTo(FStart.Right - 28, round(i * FHeight22));
      end;
     Canvas.Font := Self.Font;
     TextOut(FStart.Left + 2, round(FHeight22 - 7), '-1');
     TextOut(FStart.Left, round(6 * FHeight22 - 7), '-.5');
     TextOut(FStart.Left + 4, round(11 * FHeight22 - 7), '0');
     TextOut(FStart.Left, round(16 * FHeight22 - 7), '+.5');
     TextOut(FStart.Left + 2, round(21 * FHeight22 - 7), '+1');

     TextOut(FStart.Right - 19, round(FHeight22 - 7), '180');
     TextOut(FStart.Right - 19, round(6 * FHeight22 - 7), '135');
     TextOut(FStart.Right - 16, round(11 * FHeight22 - 7), '90');
     TextOut(FStart.Right - 16, round(16 * FHeight22 - 7), '45');
     TextOut(FStart.Right - 14, round(21 * FHeight22 - 7), '0');
    end
end;

procedure TGuiCorrelationMeter.ResetPositions;
begin
 FCenter.X := Width div 2;
 FCenter.Y := Height div 2;
 FStart.Left := FMargin;
 FStart.Top := FMargin;
 FStart.Right := Width - FMargin;
 FStart.Bottom := Height - FMargin;
 FWidth22 := Width / 22;
 FHeight22 := Height / 22;
 Invalidate;
end;

procedure TGuiCorrelationMeter.Resize;
begin
 inherited;
 ResetPositions;
end;

procedure TGuiCorrelationMeter.SetCorrelation(Value: Single);
begin
 Value := f_Limit(Value, -1, 1);
 if FCorrelation <> Value then
  begin
   FCorrelation := Value;
   Invalidate;
  end;
end;

procedure TGuiCorrelationMeter.SetDirection(
  const Value: TCorrelationMeterDirection);
begin
  FDirection := Value;
  Invalidate;
end;

procedure TGuiCorrelationMeter.SetMargin(const Value: Integer);
begin
  FMargin := Value;
  ResetPositions;
end;

end.
