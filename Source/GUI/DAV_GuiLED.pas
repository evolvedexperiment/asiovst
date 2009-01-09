unit DAV_GuiLED;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiBaseControl;

type
  TCustomGuiLED = class(TCustomGuiBaseAntialiasedControl)
  private
    FLEDColor   : TColor;
    FOnChange   : TNotifyEvent;
    FBrightness : Single;
    procedure SetLEDColor(const Value: TColor);
    procedure SetBrightness(const Value: Single);
  protected
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure RedrawBuffer(doBufferFlip: Boolean); override;
    procedure RenderLEDToBitmap(const Bitmap: TBitmap); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Brightness_Percent: Single read FBrightness write SetBrightness;
    property Color;
    property LineWidth;
    property LEDColor: TColor read FLEDColor write SetLEDColor default clBlack;
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
  FBrightness  := 100;
end;

destructor TCustomGuiLED.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomGuiLED.SettingsChanged(Sender: TObject);
begin
  RedrawBuffer(True);
end;

procedure TCustomGuiLED.RenderLEDToBitmap(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Rad      : Single;
  XStart   : Single;
  BW       : Single;
  Center   : TPointFloat;
  Line     : PRGB32Array;
  LEDColor : TRGB32;
  Scale    : Single;
  Bright   : Single;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   LEDColor.A := $FF;
   LEDColor.R := $FF and FLEDColor;
   LEDColor.G := $FF and (FLEDColor shr 8);
   LEDColor.B := $FF and (FLEDColor shr 16);
   Bright := 0.3 + 0.007 * FBrightness;

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - FLineWidth div 2;
   BW := 1 - FLineWidth * OversamplingFactor / Rad;
   if Rad < 0 then exit;

   Center.x := 0.5 * Width;
   Center.y := 0.5 * Height;
   Pen.Color := FLineColor;
   Brush.Color := FLEDColor;

   {$IFNDEF FPC}
   for i := 0 to round(2 * Rad) do
    begin
     XStart := sqrt(abs(sqr(rad) - sqr(Rad - i)));
     Line := Scanline[round(Center.y - (Rad - i))];
     for steps := round(Center.x - XStart) to round(Center.x + XStart) do
      begin
       Scale := Bright * (1 - 0.8 * Math.Max(0, (sqr(steps - Center.x) + sqr(Rad - i)) / sqr(rad)) / OversamplingFactor);

       if sqr(steps - Center.x) + sqr(Rad - i) > sqr(BW * Rad)
        then Scale := 0.4 * Scale;
       Line[steps].B := round(Scale * LEDColor.B);
       Line[steps].G := round(Scale * LEDColor.G);
       Line[steps].R := round(Scale * LEDColor.R);
      end;
    end;
   {$ENDIF}
  end;
end;

procedure TCustomGuiLED.SetBrightness(const Value: Single);
begin
 if FBrightness <> Value then
  begin
   FBrightness := Value;
   RedrawBuffer(True);
  end;
end;

procedure TCustomGuiLED.RedrawBuffer(doBufferFlip: Boolean);
var
  Bmp        : TBitmap;
begin
 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   if AntiAlias = gaaNone then
    begin
     // draw background
     {$IFNDEF FPC}
     if fTransparent
      then DrawParentImage(FBuffer.Canvas)
      else
     {$ENDIF}
      begin
       Brush.Color := Self.Color;
       FillRect(ClipRect);
      end;
     RenderLEDToBitmap(FBuffer);
    end
   else
    begin
     Bmp := TBitmap.Create;
     with Bmp do
      try
       PixelFormat := pf32bit;
       Width       := OversamplingFactor * FBuffer.Width;
       Height      := OversamplingFactor * FBuffer.Height;
       {$IFNDEF FPC}
       if fTransparent then
        begin
         DrawParentImage(Bmp.Canvas);
         UpsampleBitmap(Bmp);
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
       DownsampleBitmap(Bmp);
       FBuffer.Canvas.Draw(0, 0, Bmp);
      finally
       Free;
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

procedure TCustomGuiLED.SetLEDColor(const Value: TColor);
begin
  if (Value <> FLEDColor) then
  begin
    FLEDColor := Value;
    RedrawBuffer(True);
  end;
end;

end.
