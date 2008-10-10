unit DAV_GuiDial;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls, Consts,
  DAV_GuiBaseControl;

type
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

  TCustomGuiDial = class(TGuiBaseControl)
  private
    FAntiAlias        : TGuiAntiAlias;
    FAutoColor        : Boolean;
    FAutoSize         : Boolean;
    FCircleColor      : TColor;
    FDefaultPosition  : Single;
    FDialBitmap       : TBitmap;
    FInertia          : Single;
    FInertiaExp       : Single;
    FInertiaScale     : Single;
    FImageList        : TImageList;
    FImageIndex       : Integer;
    FMin, FMax        : Single;
    FNumGlyphs        : Integer;
    FOnChange         : TNotifyEvent;
    FOSValue          : Integer;
    FPointerAngles    : TGuiDialPointerAngles;
    FPosition         : Single;
    FRightMouseButton : TGuiDialRMBFunc;
    FStitchKind       : TGuiStitchKind;
    FCurveMapping     : Single;
    FCurveMappingExp  : Single;
    FScrollRange      : Single;
    function CircularMouseToPosition(X, Y: Integer): Single;
    function GetNormalizedPosition: Single;
    function PositionToAngle: Single;
    function GetMappedPosition: Single;
    function MapValue(Value: Single): Single;
    function UnmapValue(Value: Single): Single;
    procedure DoAutoSize;
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetAutoColor(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetCircleColor(const Value: TColor);
    procedure SetDefaultPosition(Value: Single);
    procedure SetDialBitmap(const Value: TBitmap);
    procedure SetInertia(Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetPointerAngles(const Value: TGuiDialPointerAngles);
    procedure SetPosition(Value: Single);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetCurveMapping(const Value: Single);
    procedure SetNormalizedPosition(const Value: Single);
    procedure SetImageList(const Value: TImageList);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure CalcColorCircle;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
    procedure RenderKnobToBitmap(const Bitmap: TBitmap); virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); override;
    procedure DragMouseMoveRight(Shift: TShiftState; X, Y: Integer); override;
    property NormalizedPosition: Single read GetNormalizedPosition write SetNormalizedPosition;
    property MappedPosition: Single read GetMappedPosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;
    property LineWidth default 2;
    property LineColor default clRed;
    property CircleColor : TColor read FCircleColor write SetCircleColor default clBlack;

    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property DefaultPosition: Single read FDefaultPosition write SetDefaultPosition;
    property DialBitmap: TBitmap read FDialBitmap write SetDialBitmap;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageList: TImageList read FImageList write SetImageList;
    property Inertia: Single read fInertia write SetInertia;
    property Max: Single read FMax write SetMax;
    property Min: Single read FMin write SetMin;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 1;
    property PointerAngles: TGuiDialPointerAngles read FPointerAngles write SetPointerAngles;
    property Position: Single read FPosition write SetPosition;
    property RightMouseButton: TGuiDialRMBFunc read FRightMouseButton write FRightMouseButton default rmbfCircular;
    property ScrollRange_Pixel: Single read fScrollRange write fScrollRange;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TGuiDial = class(TCustomGuiDial)
  published
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
    property Inertia;
    property ImageList;
    property ImageIndex;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property NumGlyphs;
    property OnChange;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    property Transparent;
  end;

  TCustomGuiDialMetal = class(TCustomGuiDial)
  protected
    procedure RenderKnobToBitmap(const Bitmap: TBitmap); override;
  end;

  TGuiDialMetal = class(TCustomGuiDialMetal)
  published
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property NumGlyphs;
    property OnChange;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    property Transparent;
  end;

  TCustomGuiDialEx = class(TCustomGuiDial)
  private
    fIndLineLength : Single;
    procedure SetIndLineLength(const Value: Single);
  protected
    procedure RenderKnobToBitmap(const Bitmap: TBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    property IndicatorLineLength_Percent: Single read fIndLineLength write SetIndLineLength;
  end;

  TGuiDialEx = class(TCustomGuiDialEx)
  published
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
    property IndicatorLineLength_Percent;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property NumGlyphs;
    property OnChange;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    property Transparent;
  end;

implementation

uses
  ExtCtrls, Math, DAV_Common, DAV_Complex;

function RadToDeg(const Radians: Extended): Extended;  { Degrees := Radians * 180 / PI }
const
  DegPi : Double = (180 / PI);
begin
  Result := Radians * DegPi;
end;

function RelativeAngle(X1, Y1, X2, Y2: Integer): Single;
const
  MulFak = 180 / Pi;
begin
  Result := arctan2(X2 - X1, Y1 - Y2) * MulFak;
end;

function SafeAngle(Angle: Single): Single;
begin
  while Angle < 0 do Angle := Angle + 360;
  while Angle >= 360 do Angle := Angle - 360;
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




constructor TCustomGuiDial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPointerAngles          := TGuiDialPointerAngles.Create;
  FPointerAngles.OnChange := SettingsChanged;
  FCircleColor            := clBlack;
  FLineColor              := clRed;
  FLineWidth              := 2;
  FImageIndex             := -1;
  FAntiAlias              := gaaNone;
  FOSValue                := 1;
  FRightMouseButton       := rmbfCircular;
  FMin                    := 0;
  FMax                    := 100;
  FCurveMapping           := 0;
  FCurveMappingExp        := 1;
  FPosition               := 0;
  FDefaultPosition        := 0;
  FNumGlyphs              := 1;
  FScrollRange            := 400;
  FInertia                := 0;
  FInertiaExp             := 1;
  FInertiaScale           := 1;
  FStitchKind             := skHorizontal;
  FDialBitmap             := TBitmap.Create;
  FDialBitmap.OnChange    := SettingsChanged;
end;

destructor TCustomGuiDial.Destroy;
begin
  FreeAndNil(FDialBitmap);
  FreeAndNil(FPointerAngles);
  inherited Destroy;
end;

procedure TCustomGuiDial.SettingsChanged(Sender: TObject);
begin
  FDialBitmap.Canvas.Brush.Color := Self.Color;
  RedrawBuffer(True);
end;

function TCustomGuiDial.PositionToAngle: Single;
const
  Pi180 : Double = PI / 180;
begin
 Result := SafeAngle(PointerAngles.Start + (PointerAngles.Range * MapValue(NormalizedPosition))) * Pi180;
end;

procedure TCustomGuiDial.RenderKnobToBitmap(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Val, Off : TComplexDouble;
  Rad, tmp : Single;
  PtsArray : Array of TPoint;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
   if Rad <= 0 then exit;
   Steps := Round(2 / arcsin(1 / Rad)) + 1;
   if Steps > 1 then
    begin
     SetLength(PtsArray, Steps);
     GetSinCos(PositionToAngle - (PI * 0.5), Val.Im, Val.Re);
     Val.Re := Val.Re * Rad; Val.Im := Val.Im * Rad;
     GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
     PtsArray[0] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));

     for i := 1 to Steps - 1 do
      begin
       tmp := Val.Re * Off.Re - Val.Im * Off.Im;
       Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
       Val.Re := tmp;
       PtsArray[i] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
      end;

     Pen.Width := FOSValue * fLineWidth;
     Pen.Color := fLineColor;
     Brush.Color := FCircleColor;
     Polygon(PtsArray);
    end;

   // draw position line
   MoveTo(PtsArray[0].X, PtsArray[0].Y);
   LineTo(Round(0.5 * Width), Round(0.5 * Height));
  end;
end;

procedure TCustomGuiDial.RedrawBuffer(doBufferFlip: Boolean);
var
  theRect    : TRect;
  GlyphNr    : Integer;
  Bmp        : TBitmap;
begin
 if [csLoading..csDestroying] * ComponentState <> [] then exit;

 if (Width > 0) and (Height > 0) then with fBuffer.Canvas do
  begin
   Lock;
   Brush.Color := Self.Color;
   if FDialBitmap.Empty and (FImageIndex < 0) then
    case FAntiAlias of
     gaaNone     :
      begin
       // draw background
       {$IFNDEF FPC}if fTransparent then CopyParentImage(Self, fBuffer.Canvas) else{$ENDIF}
       FillRect(ClipRect);

       RenderKnobToBitmap(fBuffer);
      end;
     gaaLinear2x :
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSValue * fBuffer.Width;
         Height      := FOSValue * fBuffer.Height;
         Canvas.Brush.Style := bsSolid;
         Canvas.Brush.Color := Self.Color;
         {$IFNDEF FPC}
         if fTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
           Upsample4xBitmap(Bmp);
          end else
         {$ENDIF}
         Canvas.FillRect(Canvas.ClipRect);
         RenderKnobToBitmap(Bmp);
         Downsample2xBitmap(Bmp);
         fBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         FreeAndNil(Bmp);
        end;
      end;
     gaaLinear4x :
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSValue * fBuffer.Width;
         Height      := FOSValue * fBuffer.Height;
         Canvas.Brush.Style := bsSolid;
         Canvas.Brush.Color := Self.Color;
         {$IFNDEF FPC}
         if fTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
           Upsample4xBitmap(Bmp);
          end else
         {$ENDIF}
         Canvas.FillRect(Canvas.ClipRect);
         RenderKnobToBitmap(Bmp);
         Downsample4xBitmap(Bmp);
         fBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         FreeAndNil(Bmp);
        end;
      end;
     gaaLinear8x :
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSValue * fBuffer.Width;
         Height      := FOSValue * fBuffer.Height;
         Canvas.Brush.Style := bsSolid;
         Canvas.Brush.Color := Self.Color;
         {$IFNDEF FPC}
         if fTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
           Upsample4xBitmap(Bmp);
           Upsample2xBitmap(Bmp);
          end else
         {$ENDIF}
         Canvas.FillRect(Canvas.ClipRect);
         RenderKnobToBitmap(Bmp);
         Downsample4xBitmap(Bmp);
         Downsample2xBitmap(Bmp);
         fBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         FreeAndNil(Bmp);
        end;
      end;
     gaaLinear16x :
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSValue * fBuffer.Width;
         Height      := FOSValue * fBuffer.Height;
         Canvas.Brush.Style := bsSolid;
         Canvas.Brush.Color := Self.Color;
         {$IFNDEF FPC}
         if fTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
           Upsample4xBitmap(Bmp);
           Upsample4xBitmap(Bmp);
          end else
         {$ENDIF}
         Canvas.FillRect(Canvas.ClipRect);
         RenderKnobToBitmap(Bmp);
         Downsample4xBitmap(Bmp);
         Downsample4xBitmap(Bmp);
         fBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         FreeAndNil(Bmp);
        end;
      end;
    end
   else
    begin
     // draw background
     Brush.Color := Self.Color;
     {$IFNDEF FPC}if fTransparent then CopyParentImage(Self, fBuffer.Canvas) else{$ENDIF}
     FillRect(ClipRect);

     GlyphNr := Trunc(MapValue(NormalizedPosition) * FNumGlyphs);
     if (GlyphNr >= FNumGlyphs) then GlyphNr := FNumGlyphs - 1 else
     if (GlyphNr < 0) then GlyphNr := 0;
     theRect := ClientRect;

     if not DialBitmap.Empty
      then Bmp := DialBitmap
      else raise Exception.Create('Yet todo!');

     if FStitchKind = skVertical then
      begin
       theRect.Top    := Bmp.Height * GlyphNr div FNumGlyphs;
       theRect.Bottom := Bmp.Height * (GlyphNr + 1) div FNumGlyphs;
      end
     else
      begin
       theRect.Left  := Bmp.Width * GlyphNr div FNumGlyphs;
       theRect.Right := Bmp.Width * (GlyphNr + 1) div FNumGlyphs;
      end;

//     CopyRect(Clientrect, Bmp.Canvas, theRect);

     with ClientRect do
      begin
       BitBlt(Handle, Left, Top, Right - Left, Bottom - Top,
         Bmp.Canvas.Handle, theRect.Left, theRect.Top, CopyMode);
      end;

    end;
   Unlock;
  end;

 if doBufferFlip then Invalidate;
end;

function TCustomGuiDial.GetNormalizedPosition: Single;
begin
 result := (FPosition - Min) / (Max - Min);
end;

function TCustomGuiDial.GetMappedPosition: Single;
begin
 result := MapValue(NormalizedPosition) * (Max - Min) + Min;
end;

function TCustomGuiDial.UnmapValue(Value: Single): Single;
begin
 result := sign(Value) * Power(abs(Value), 1 / FCurveMappingExp);
end;

procedure TCustomGuiDial.DoAutoSize;
begin
 if FDialBitmap.Empty or (FNumGlyphs = 0) then Exit;

 if FStitchKind = skVertical then
  begin
   Width  := FDialBitmap.Width;
   Height := FDialBitmap.Height div FNumGlyphs;
  end
 else
  begin
   Width  := FDialBitmap.Width div FNumGlyphs;
   Height := FDialBitmap.Height;
  end;
end;

procedure TCustomGuiDial.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Autosize then DoAutoSize;
  end;
end;

procedure TCustomGuiDial.SetMax(const Value: Single);
begin
  if Value <> FMax then
  begin
    if (Value < FMin) and not (csLoading in ComponentState) then
      raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin + 1, MaxInt]);

   FMax := Value;
   if FPosition > Value then FPosition := Value;
   if FDefaultPosition > Value then FDefaultPosition := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetMin(const Value: Single);
begin
  if Value <> FMin then
  begin
    if (Value > FMax) and not (csLoading in ComponentState) then
      raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMax - 1]);

    FMin := Value;
    if FPosition < Value then FPosition := Value;
    if FDefaultPosition < Value then FDefaultPosition := Value;
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetNormalizedPosition(const Value: Single);
begin
 Position := Min + Value * (Max - Min);
end;

procedure TCustomGuiDial.SetNumGlyphs(const Value: Integer);
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    DoAutoSize;
  end;
end;

procedure TCustomGuiDial.SetPosition(Value: Single);
begin
  if Value < FMin then Value := FMin else
  if Value > FMax then Value := FMax;

  if FPosition <> Value then
  begin
    FPosition := Value;
    if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetDefaultPosition(Value: Single);
begin
  if Value < FMin then Value := FMin else
  if Value > FMax then Value := FMax;

  FDefaultPosition := Value;
end;

procedure TCustomGuiDial.SetDialBitmap(const Value: TBitmap);
begin
  FDialBitmap.Assign(Value);
  DoAutoSize;
end;

procedure TCustomGuiDial.SetImageIndex(Value: Integer);
begin
 if not assigned(FImageList)
  then Value := -1;
 if FImageIndex <> Value then
  begin
   FImageIndex := Value;
   if (FImageIndex < 0) or FDialBitmap.Empty
    then RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetImageList(const Value: TImageList);
begin
 if FImageList <> Value then
  begin
   FImageList := Value;
   if not assigned(FImageList)
    then ImageIndex := -1;
  end;
end;

procedure TCustomGuiDial.SetInertia(Value: Single);
begin
 if Value < 0 then Value := 0;
 if FInertia <> Value then
  begin
   FInertia      := Value;
   FInertiaExp   := Power(2, -Value);
   FInertiaScale := 0.01 * Power(0.01, -FInertiaExp);
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetStitchKind(const Value: TGuiStitchKind);
begin
  if FStitchKind <> Value then
  begin
    FStitchKind := Value;
    DoAutoSize; 
  end;
end;

function TCustomGuiDial.CircularMouseToPosition(X, Y: Integer): Single;
var
  Range: Single;
  Angle: Single;
begin
  Range := Max - (Min - 1);
  Angle := SafeAngle(RelativeAngle(Width div 2, Height div 2, X, Y) - PointerAngles.Start);
  Result := Angle * Range / PointerAngles.Range;
  while Result > Max do Result := Result - Range;
  while Result < Min do Result := Result + Range;

  if Result > Max then Result := FPosition;
  if Result < Min then Result := FPosition;
end;

function TCustomGuiDial.MapValue(Value: Single): Single;
begin
 result := Sign(Value) * Power(abs(Value), FCurveMappingExp);
end;

procedure TCustomGuiDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Enabled then
  begin
    if ssCtrl in Shift then Position := FDefaultPosition;
    if (Button = mbRight) and
       (FRightMouseButton = rmbfReset)
     then position := FDefaultPosition;
  end;

  inherited;
end;

procedure TCustomGuiDial.DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer);
var
  Difference : Single;
begin
  Difference := (MouseState.LastEventY - Y) / fScrollRange;

  // apply inertia function
  Difference := sign(Difference) * Power(abs(Difference), fInertiaExp) * FInertiaScale;

  if ssShift in Shift
   then NormalizedPosition := UnmapValue(MapValue(NormalizedPosition) + Difference * 0.1)
   else NormalizedPosition := UnMapValue(MapValue(NormalizedPosition) + Difference);
  inherited;
end;

procedure TCustomGuiDial.DragMouseMoveRight(Shift: TShiftState; X, Y: Integer);
begin
  if FRightMouseButton = rmbfCircular
   then Position := CircularMouseToPosition(x,y);
  inherited;
end;

procedure TCustomGuiDial.SetPointerAngles(const Value: TGuiDialPointerAngles);
begin
  FPointerAngles.Assign(Value);
end;

procedure TCustomGuiDial.CalcColorCircle;
begin
  if (Color and $000000FF) < $80
   then if (((Color and $0000FF00) shr 8) <$80) or (((Color and $00FF0000) shr 16)<$80) then FCircleColor:=$FFFFFF
   else if (((Color and $0000FF00) shr 8) <$80) and (((Color and $00FF0000) shr 16)<$80) then FCircleColor:=$FFFFFF;

  RedrawBuffer(True);
end;

procedure TCustomGuiDial.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   case FAntiAlias of
         gaaNone : FOSValue :=  1;
     gaaLinear2x : FOSValue :=  2;
     gaaLinear4x : FOSValue :=  4;
     gaaLinear8x : FOSValue :=  8;
    gaaLinear16x : FOSValue := 16;
   end;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetAutoColor(const Value: Boolean);
begin
  CalcColorCircle;
end;

procedure TCustomGuiDial.SetCircleColor(const Value: TColor);
begin
  if not FAutoColor and (Value <> FCircleColor) then
  begin
    FCircleColor:=Value;
    RedrawBuffer(True);
  end;
end;

procedure TCustomGuiDial.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   FCurveMappingExp := Power(2, Value);
   RedrawBuffer(True);
  end;
end;

{ TCustomGuiDialMetal }

procedure TCustomGuiDialMetal.RenderKnobToBitmap(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Val      : Single;
  Rad      : Single;
  Cmplx    : TComplexSingle;
  Pnt      : TPoint;
  XStart   : Single;
  LineFrac : Single;
  BW       : Single;
  Center   : record
              x, y : Single;
             end;
  Line     : PRGB32Array;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   // draw background
   {$IFNDEF FPC}if fTransparent then CopyParentImage(Self, fBuffer.Canvas) else{$ENDIF}
   FillRect(ClipRect);

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
   BW := 1 - FOSValue / Rad; // border width = 1 pixel
   if Rad < 0 then exit;

   Center.x := 0.5 * Width;
   Center.y := 0.5 * Height;
   Pen.Color := fLineColor;
   Brush.Color := FCircleColor;

   for i := 0 to round(2 * Rad) do
    begin
     XStart := sqrt(abs(sqr(rad) - sqr(Rad - i)));
     Line := Scanline[round(Center.y - (Rad - i))];
     for steps := round(Center.x - XStart) to round(Center.x + XStart) do
      begin
       val := 2.9999 * abs(ArcTan2(steps - Center.x, (Rad - i)) / Pi);
       if round(1.5 + val) mod 3 = 0
        then val := val * 50 - 99.5
        else val := -Round(99.5 + val * 50) mod 100;
       if sqr(steps - Center.x) + sqr(Rad - i) > sqr(BW * Rad) then val := -$90;

       Line[steps].B := round(Line[steps].B + val);
       Line[steps].G := round(Line[steps].G + val);
       Line[steps].R := round(Line[steps].R + val);
      end;

     GetSinCos(PositionToAngle - (PI * 0.5), Cmplx.Im, Cmplx.Re);
     Pnt := Point(Round(Center.x + Cmplx.Re * BW * Rad), Round(Center.y + Cmplx.Im * BW * Rad));

     //LineFrac := 0.01 * fIndLineLength;
     LineFrac := 0.5;
     Pen.Width := 3 * FOSValue;
     MoveTo(Pnt.X, Pnt.Y);
     LineTo(Round((1 - LineFrac) * Pnt.X + LineFrac * Center.x),
            Round((1 - LineFrac) * Pnt.Y + LineFrac * Center.y));
    end;
  end;
end;

{ TCustomGuiDialEx }

constructor TCustomGuiDialEx.Create(AOwner: TComponent);
begin
 inherited;
 fIndLineLength := 100;
end;

procedure TCustomGuiDialEx.RenderKnobToBitmap(const Bitmap: TBitmap);
var
  Steps, i  : Integer;
  Val, Off  : TComplexDouble;
  Rad, tmp  : Single;
  PtsArray  : Array of TPoint;
  LineFrac  : Single;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   {$IFNDEF FPC}if fTransparent then CopyParentImage(Self, fBuffer.Canvas) else{$ENDIF}
   FillRect(ClipRect);

   Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
   if Rad < 0 then exit;
   Steps := Round(2 / arcsin(1 / Rad)) + 1;
   if Steps > 1 then
    begin
     SetLength(PtsArray, Steps);
     GetSinCos(PositionToAngle - (PI * 0.5), Val.Im, Val.Re);
     Val.Re := Val.Re * Rad; Val.Im := Val.Im * Rad;
     GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
     PtsArray[0] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));

     for i:=1 to Steps - 1 do
      begin
       tmp := Val.Re * Off.Re - Val.Im * Off.Im;
       Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
       Val.Re := tmp;
       PtsArray[i] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
      end;

     Pen.Width := fLineWidth;
     Pen.Color := fLineColor;
     Brush.Color := FCircleColor;
     Polygon(PtsArray);
    end;

   LineFrac := 0.01 * fIndLineLength;
   MoveTo(PtsArray[0].X, PtsArray[0].Y);
   LineTo(Round((1 - LineFrac) * PtsArray[0].X + LineFrac * 0.5 * Width),
          Round((1 - LineFrac) * PtsArray[0].Y + LineFrac * 0.5 * Height));
  end;
end;

procedure TCustomGuiDialEx.SetIndLineLength(const Value: Single);
begin
 if fIndLineLength <> Value then
  begin
   fIndLineLength := Value;
   RedrawBuffer(True);
  end;
end;

end.
