unit DGuiDial;

interface

uses Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
     Consts, DGuiBaseControl;

type
  TGuiDialStitchKind = (skHorizontal, skVertical);
  TGuiDialRMBFunc = (rmbfReset,rmbfCircular);

  TGuiDialSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected
    procedure Changed;
  public
    constructor Create; virtual;
  end;

  TGuiDialPointerAngles = class(TGuiDialSettings)
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

  TGuiDial = class(TGuiBaseControl)
  private
    fAutoSize    : Boolean;
    fDialBitmap  : TBitmap;
    fStitchKind  : TGuiDialStitchKind;
    fMin, fMax   : Single;
    fPosition    : Single;
    fNumGlyphs   : Integer;
    fOnChange    : TNotifyEvent;
    fOldMousPos  : TPoint;
    fColorCircle : TColor;
    fColorLine   : TColor;
    fColorAuto   : Boolean;
    fLineWidth   : Integer;
    fPointerAngles : TGuiDialPointerAngles;
    fDefaultPosition    : Single;
    fRightMouseButton: TGuiDialRMBFunc;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetColorAuto(const Value: Boolean);
    procedure SetLineWidth(const Value: Integer);
    procedure SetColorCircle(const Value: TColor);
    procedure SetColorLine(const Value: TColor);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetPosition(Value: Single);
    procedure SetDefaultPosition(Value: Single);
    procedure SetDialBitmap(const Value: TBitmap);
    procedure SetStitchKind(const Value: TGuiDialStitchKind);
    function CircularMouseToPosition(X, Y: Integer): Single;
    procedure SetPointerAngles(const Value: TGuiDialPointerAngles);
    function PositionToAngle: Single;
  protected
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure CalcColorCircle;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoSize: Boolean read fAutoSize write SetAutoSize default false;
    property ColorAuto: Boolean read fColorAuto write SetColorAuto default false;
    property Position: Single read FPosition write SetPosition;
    property DefaultPosition: Single read FDefaultPosition write SetDefaultPosition;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property ColorCircle : TColor read fColorCircle write SetColorCircle default clBlack;
    property ColorLine : TColor read fColorLine write SetColorLine default clRed;
    property LineWidth : Integer read fLineWidth write SetLineWidth default 2;
    property RightMouseButton: TGuiDialRMBFunc read fRightMouseButton write fRightMouseButton default rmbfCircular;
    property NumGlyphs: Integer read fNumGlyphs write SetNumGlyphs default 1;
    property DialBitmap: TBitmap read fDialBitmap write SetDialBitmap;
    property StitchKind: TGuiDialStitchKind read fStitchKind write SetStitchKind;
    property PointerAngles: TGuiDialPointerAngles read fPointerAngles write SetPointerAngles;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

implementation

uses ExtCtrls, Math, DAVDCommon;

function RadToDeg(const Radians: Extended): Extended;  { Degrees := Radians * 180 / PI }
const DegPi : Double = (180 / PI);
begin
  Result := Radians * DegPi;
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
  if Z = 0 then Result := 0 else Result := round( (X * 100.0) / Z ); //t
end;



procedure TGuiDialSettings.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TGuiDialSettings.Create;
begin
  inherited;
end;




procedure TGuiDialPointerAngles.AssignTo(Dest: TPersistent);
begin
  if Dest is TGuiDialPointerAngles then with TGuiDialPointerAngles(Dest) do
  begin
    FRange := Self.Range;
    FStart := Self.Start;
    FResolution := Self.Resolution;
    Changed;
  end else inherited;
end;

constructor TGuiDialPointerAngles.Create;
begin
  inherited;
  FStart := 0;
  FRange := 360;
  FResolution := 0;
end;

procedure TGuiDialPointerAngles.SetRange(const Value: Integer);
begin
  if (Value < 1) or (Value > 360) then
    raise Exception.Create('Range must be 1..360');

  FRange := Value;
  if FRange > Resolution then Resolution := FRange;
  Changed;
end;

procedure TGuiDialPointerAngles.SetResolution(const Value: Extended);
begin
  if (Value < 0) or (Value > Range) then
    raise Exception.Create('Resolution must be above 0 and less than ' + IntToStr(Range + 1));

  FResolution := Value;
  Changed;
end;

procedure TGuiDialPointerAngles.SetStart(const Value: Integer);
begin
  if (Value < 0) or (Value > 359) then
    raise Exception.Create('Start must be 0..359');

  FStart := Value;
  Changed;
end;




constructor TGuiDial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPointerAngles := TGuiDialPointerAngles.Create;
  fPointerAngles.OnChange := SettingsChanged;
  fColorCircle := clBlack;
  fColorLine := clRed;
  fLineWidth := 2;
  fRightMouseButton := rmbfCircular;
  fMin := 0;
  fMax := 100;
  fPosition := 0;
  fDefaultPosition := 0;
  fNumGlyphs := 1;
  fStitchKind:=skHorizontal;
  fDialBitmap := TBitmap.Create;
end;

destructor TGuiDial.Destroy;
begin
  FreeAndNil(fDialBitmap);
  FreeAndNil(fPointerAngles);
  inherited Destroy;
end;

procedure TGuiDial.SettingsChanged(Sender: TObject);
begin
  RedrawBuffer(true);
end;

function TGuiDial.PositionToAngle: Single;
var
  Percircle: Single;
  Range: Single;
const Pi180 : Double = PI/180;
begin
  Range := Max - Min;
  Percircle := (fPosition - Min) * 360 / Range;
  Result := SafeAngle(PointerAngles.Start + (PointerAngles.Range * Percircle / 360))* Pi180;
end;

procedure TGuiDial.RedrawBuffer(doBufferFlip: Boolean);
type TComplex = record Re,Im : Single; end;
var theRect    : TRect;
    GlyphNr,i  : Integer;
    Val,Off    : TComplex;
    Rad,tmp    : Single;
    PtsArray   : Array of TPoint;
begin
  with fBuffer.Canvas do
  begin
    Lock;

    if fDialBitmap.Empty then
    begin
      Brush.Color := Self.Color;

      {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);

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

    end else begin
      GlyphNr:=Trunc((fPosition - fMin) / ((fMax + 1) - fMin) * fNumGlyphs);
      theRect:=ClientRect;
      if fStitchKind=skVertical then
      begin
        theRect.Top := fDialBitmap.Height * GlyphNr div fNumGlyphs;
        theRect.Bottom := fDialBitmap.Height * (GlyphNr+1) div fNumGlyphs;
      end else begin
        theRect.Left := fDialBitmap.Width * GlyphNr div fNumGlyphs;
        theRect.Right := fDialBitmap.Width * (GlyphNr+1) div fNumGlyphs;
      end;

      CopyRect(clientrect,fDialBitmap.Canvas,theRect);
    end;
    Unlock;
  end;

  if doBufferFlip then Invalidate;
end;

procedure TGuiDial.DoAutoSize;
begin
  if fDialBitmap.Empty or (fNumGlyphs=0) then Exit;

  if fStitchKind=skVertical then
  begin
    Width:=fDialBitmap.Width;
    Height:=fDialBitmap.Height div fNumGlyphs;
  end else begin
    Width:=fDialBitmap.Width div fNumGlyphs;
    Height:=fDialBitmap.Height;
  end;
end;

procedure TGuiDial.SetAutoSize(const Value: boolean);
begin
  if fAutoSize<>Value then
  begin
    fAutoSize := Value;
    if Autosize then DoAutoSize;
  end;
end;

procedure TGuiDial.SetMax(const Value: Single);
begin
  if Value <> FMax then
  begin
    if (Value < FMin) and not (csLoading in ComponentState) then
      raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin + 1, MaxInt]);

   FMax := Value;
   if fPosition > Value then fPosition := Value;
   if fDefaultPosition > Value then fDefaultPosition := Value;
   RedrawBuffer(true);
  end;
end;

procedure TGuiDial.SetMin(const Value: Single);
begin
  if Value <> FMin then
  begin
    if (Value > FMax) and not (csLoading in ComponentState) then
      raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMax - 1]);

    FMin := Value;
    if fPosition < Value then fPosition := Value;
    if fDefaultPosition < Value then fDefaultPosition := Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiDial.SetNumGlyphs(const Value: Integer);
begin
  if fNumGlyphs <> Value then
  begin
    fNumGlyphs := Value;
    DoAutoSize;
  end;
end;

procedure TGuiDial.SetPosition(Value: Single);
begin
  if Value < FMin then Value := FMin else
  if Value > FMax then Value := FMax;

  if fPosition <> Value then
  begin
    fPosition := Value;
    if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
    RedrawBuffer(true);
  end;
end;

procedure TGuiDial.SetDefaultPosition(Value: Single);
begin
  if Value < FMin then Value := FMin else
  if Value > FMax then Value := FMax;

  fDefaultPosition := Value;
end;

procedure TGuiDial.SetDialBitmap(const Value: TBitmap);
begin
  fDialBitmap.Assign(Value);
  DoAutoSize;
end;

procedure TGuiDial.SetStitchKind(const Value: TGuiDialStitchKind);
begin
  if fStitchKind <> Value then
  begin
    fStitchKind := Value;
    DoAutoSize; 
  end;
end;

function TGuiDial.CircularMouseToPosition(X, Y: Integer): Single;
var
  Range: Single;
  Angle: Single;
begin
  Range := Max - (Min - 1);
  Angle := SafeAngle(RelativeAngle(Width div 2, Height div 2, X, Y) - PointerAngles.Start);
  Result := Angle * Range / PointerAngles.Range;
  while Result > Max do Result:=Result-Range;
  while Result < Min do Result:=Result+Range;

  if Result > Max then Result := fPosition;
  if Result < Min then Result := fPosition;
end;

procedure TGuiDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Enabled then
  begin
    Click;
    inherited MouseDown(Button, Shift, X, Y);

    if ssCtrl in Shift then position := fDefaultPosition;
    if (Button = mbRight) and (fRightMouseButton=rmbfReset) then position := fDefaultPosition;

    fOldMousPos.X:=X;
    fOldMousPos.Y:=Y;
  end;
end;

procedure TGuiDial.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Enabled then
  begin
    inherited MouseUp(Button, Shift, X, Y);
    fOldMousPos.X:=X;
    fOldMousPos.Y:=Y;
  end;
end;

procedure TGuiDial.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Range: Single;
begin
  if enabled then inherited;

  if fMouseButtonDown then
  begin
    Range := Max - (Min - 1);
    if ssShift in Shift then Position := Position + (fOldMousPos.Y - Y)*0.001*Range
    else if (ssRight in Shift) and (fRightMouseButton=rmbfCircular) then position := CircularMouseToPosition(x,y)
    else Position := Position + (fOldMousPos.Y - Y)*0.005*Range;
    fOldMousPos.X:=X;
    fOldMousPos.Y:=Y;
  end;

end;

procedure TGuiDial.SetPointerAngles(const Value: TGuiDialPointerAngles);
begin
  fPointerAngles.Assign(Value);
end;

procedure TGuiDial.CalcColorCircle;
begin
  if (Color and $000000FF)<$80 then
    if (((Color and $0000FF00) shr  8)<$80) or (((Color and $00FF0000) shr 16)<$80) then fColorCircle:=$FFFFFF
  else
    if (((Color and $0000FF00) shr  8)<$80) and (((Color and $00FF0000) shr 16)<$80) then fColorCircle:=$FFFFFF;

  RedrawBuffer(true);
end;

procedure TGuiDial.SetColorAuto(const Value: Boolean);
begin
  CalcColorCircle;
end;

procedure TGuiDial.SetColorCircle(const Value: TColor);
begin
  if not fColorAuto and (Value<>fColorCircle) then
  begin
    fColorCircle:=Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiDial.SetColorLine(const Value: TColor);
begin
  if not fColorAuto and (Value<>fColorLine) then
  begin
    fColorLine:=Value;
    RedrawBuffer(true);
  end;
end;

procedure TGuiDial.SetLineWidth(const Value: Integer);
begin
  if (Value<>fLineWidth) then
  begin
    fLineWidth:=Value;
    RedrawBuffer(true);
  end;
end; 

end.
