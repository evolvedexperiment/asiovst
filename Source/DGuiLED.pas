unit DGuiLED;

interface

{$I ASIOVST.INC}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls, Consts,
  DGuiBaseControl;

type
  TCustomGuiLED = class(TGuiBaseControl)
  private
    FAntiAlias  : TGuiAntiAlias;
    FLEDColor   : TColor;
    FOnChange   : TNotifyEvent;
    FOSValue    : Integer;
    fBrightness : Single;
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetLEDColor(const Value: TColor);
    procedure SetBrightness(const Value: Single);
  protected
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
    procedure RenderLEDToBitmap(Bitmap: TBitmap); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Brightness_Percent: Single read fBrightness write SetBrightness;
    property Color;
    property LineWidth;
    property LEDColor: TColor read FLEDColor write SetLEDColor default clBlack;
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiLED = class(TCustomGuiLED)
  published
    property AntiAlias;
    property LEDColor;
    property Color;
    property LineColor;
    property LineWidth;
    property OnChange;
  end;

implementation

uses
  ExtCtrls, Math, DAVDCommon, DAVDComplex;

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
  Result := ArcTan2(X2 - X1, Y1 - Y2) * MulFak;
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


constructor TCustomGuiLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLEDColor    := clBlack;
  FLineColor   := clRed;
  FLineWidth   := 2;
  FAntiAlias   := gaaNone;
  FOSValue     := 1;
  fBrightness  := 100;
end;

destructor TCustomGuiLED.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomGuiLED.SettingsChanged(Sender: TObject);
begin
  RedrawBuffer(True);
end;

procedure TCustomGuiLED.RenderLEDToBitmap(Bitmap: TBitmap);
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
  LEDColor : TRGB32;
  Scale    : Single;
  Bright   : Single;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   LEDColor.A := $FF;
   LEDColor.R := $FF and fLEDColor;
   LEDColor.G := $FF and (fLEDColor shr 8);
   LEDColor.B := $FF and (fLEDColor shr 16);
   Bright := 0.3 + 0.007 * fBrightness;

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - fLineWidth div 2;
   BW := 1 - fLineWidth * FOSValue / Rad;
   if Rad < 0 then exit;

   Center.x := 0.5 * Width;
   Center.y := 0.5 * Height;
   Pen.Color := fLineColor;
   Brush.Color := FLEDColor;

   for i := 0 to round(2 * Rad) do
    begin
     XStart := sqrt(abs(sqr(rad) - sqr(Rad - i)));
     Line := Scanline[round(Center.y - (Rad - i))];
     for steps := round(Center.x - XStart) to round(Center.x + XStart) do
      begin
       Scale := Bright * (1 - 0.8 * Math.Max(0, ((sqr(steps - Center.x) + sqr(Rad - i)) / sqr(rad))));

       if sqr(steps - Center.x) + sqr(Rad - i) > sqr(BW * Rad)
        then Scale := 0.4 * Scale;
       Line[steps].B := round(Scale * LEDColor.B);
       Line[steps].G := round(Scale * LEDColor.G);
       Line[steps].R := round(Scale * LEDColor.R);
      end;
    end;
  end;
end;

procedure TCustomGuiLED.SetBrightness(const Value: Single);
begin
 if fBrightness <> Value then
  begin
   fBrightness := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLED.RedrawBuffer(doBufferFlip: Boolean);
var
  theRect    : TRect;
  GlyphNr    : Integer;
  Bmp        : TBitmap;
begin
 if (Width > 0) and (Height > 0) then with fBuffer.Canvas do
  begin
   Lock;
   case FAntiAlias of
    gaaNone     :
     begin
      // draw background
      {$IFNDEF FPC}
      if fTransparent
       then DrawParentImage(fBuffer.Canvas)
       else
      {$ENDIF}
       begin
        Brush.Color := Self.Color;
        FillRect(ClipRect);
       end;
      RenderLEDToBitmap(fBuffer);
     end;
    gaaLinear2x :
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width       := 2 * fBuffer.Width;
        Height      := 2 * fBuffer.Height;
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample2xBitmap(Bmp);
         end
        else
        {$ENDIF}
         with Bmp.Canvas do
          begin
           Brush.Color := Self.Color;
           FillRect(ClipRect);
          end;
        Bmp.Canvas.FillRect(ClipRect);
        RenderLEDToBitmap(Bmp);
        Downsample2xBitmap(Bmp);
        fBuffer.Canvas.Draw(0, 0, Bmp);
       finally
        Free;
       end;
     end;
    gaaLinear4x :
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width       := 4 * fBuffer.Width;
        Height      := 4 * fBuffer.Height;
        {$IFNDEF FPC}
        if fTransparent then
         begin
          DrawParentImage(Bmp.Canvas);
          Upsample2xBitmap(Bmp);
         end
        else
        {$ENDIF}
         with Bmp.Canvas do
          begin
           Brush.Color := Self.Color;
           FillRect(ClipRect);
          end;
        RenderLEDToBitmap(Bmp);
        Downsample2xBitmap(Bmp);
        Downsample2xBitmap(Bmp);
        fBuffer.Canvas.Draw(0, 0, Bmp);
       finally
        Free;
       end;
     end;
   end;
   Unlock;
  end;

 if doBufferFlip then Invalidate;
end;

procedure TCustomGuiLED.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TCustomGuiLED.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   case FAntiAlias of
        gaaNone : FOSValue := 1;
    gaaLinear2x : FOSValue := 2;
    gaaLinear4x : FOSValue := 4;
   end;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLED.SetLEDColor(const Value: TColor);
begin
  if (Value <> FLEDColor) then
  begin
    FLEDColor := Value;
    RedrawBuffer(True);
  end;
end;

end.
