{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{   SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/       }
{                                                                              }
{******************************************************************************}

unit SEGFXGUI;

interface

uses
  Windows, Classes, Graphics, DAV_SEModule, DAV_SEGUI, SEGFXModule;

type
  TSEGFXBaseGuiClass = class of TSEGFXBaseGui;
  PSEGFXColor32 = ^TSEGFXColor32;
  TSEGFXColor32 = type Longword;
  TSEGFXCombineReg = function(X, Y, W: TSEGFXColor32): TSEGFXColor32;

  TSEGFXBaseGui = class(TSEGUIBase)
  protected
    FBitmap: TBitmap;
    FLock: Boolean;
    procedure PerformBitmapOperation; virtual; abstract;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
    procedure GuiPaint(hDC: hDC; wi: PSEWndInfo); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback;
      AHostPtr: Pointer); override;
    destructor Destroy; override;
  end;

  TSEGFXAmountGui = class(TSEGFXBaseGui)
  protected
    FAmount: Byte;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
    procedure GuiWindowOpen(wi: PSEWndInfo); override;

    property Amount: Byte read FAmount;
  end;

  TSEGFXAddColorNoiseGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXAddMonoNoiseGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXContrastGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXEmbossGui = class(TSEGFXBaseGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXGrayScaleGui = class(TSEGFXBaseGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXHueGui = class(TSEGFXAmountGui)
  protected
    FDither: Boolean;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXInvertGui = class(TSEGFXBaseGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXFishEyeGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXLightnessGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXDarknessGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXSaturationGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXSplitBlurGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXSplitBlur2Gui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXGaussianBlurGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXMosaicGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXTwistGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXSplitlightGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXTileGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXSpotlightGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXTraceGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXSolarizeGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXPosterizeGui = class(TSEGFXAmountGui)
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXBrushedMetalGui = class(TSEGFXAmountGui)
  protected
    FGradient: Single;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
  public
    procedure PerformBitmapOperation; override;
  end;

  TSEGFXBrushedMetal2Gui = class(TSEGFXBrushedMetalGui)
  public
    procedure PerformBitmapOperation; override;
  end;

implementation

uses
  SysUtils, Math, DAV_Math, DAV_Types;

(*
  var
  AlphaTable: Pointer;
  BiasPtr: Pointer;
  AlphaPtr: Pointer;

  procedure GenAlphaTable;
  var
  I: Integer;
  L: Longword;
  P: ^Longword;
  begin
  GetMem(AlphaTable, 257 * 8);
  AlphaPtr := Pointer(Integer(AlphaTable) and $FFFFFFF8);
  if Integer(AlphaPtr) < Integer(AlphaTable) then
  AlphaPtr := Pointer(Integer(AlphaPtr) + 8);
  P := AlphaPtr;
  for I := 0 to 255 do
  begin
  L := I + I shl 16;
  P^ := L;
  Inc(P);
  P^ := L;
  Inc(P);
  end;
  BiasPtr := Pointer(Integer(AlphaPtr) + $80 * 8);
  end;

  function CombineRegPascal(X, Y, W: TSEGFXColor32): TSEGFXColor32; assembler;
  const
  Bias = $00800080;
  begin
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)

  if W = 0 then Result := Y else // maybe if W <= 0 ???
  if W = $FF then Result := X else // maybe if W >= $FF ??? or if W > $FF ???
  begin
  Result := (((((X shr 8) and $00FF00FF) * W) + Bias) and $FF00FF00) or
  (((((X and $00FF00FF) * W) + Bias) and $FF00FF00) shr 8);
  W := W xor $FF; // W := 1 - W;

  Result := Result + (
  (((((Y shr 8) and $00FF00FF) * W) + Bias) and $FF00FF00) or
  (((((Y and $00FF00FF) * W) + Bias) and $FF00FF00) shr 8));
  end;
  end;

  function CombineRegMMX(X, Y, W: TSEGFXColor32): TSEGFXColor32; assembler;
  asm
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

  db $0F, $EF, $C0           // PXOR      MM0, MM0
  db $0F, $6E, $C8           // MOVD      MM1, EAX
  SHL       ECX, 3
  db $0F, $6E, $D2           // MOVD      MM2, EDX
  db $0F, $60, $C8           // PUNPCKLBW MM1, MM0
  db $0F, $60, $D0           // PUNPCKLBW MM2, MM0
  ADD       ECX, AlphaPtr
  db $0F, $F9, $CA           // PSUBW     MM1, MM2
  db $0F, $D5, $09           // PMULLW    MM1, [ECX]
  db $0F, $71, $F2,$08       // PSLLW     MM2, 8
  MOV       ECX, BiasPtr
  db $0F, $FD, $11           // PADDW     MM2, [ECX]
  db $0F, $FD, $CA           // PADDW     MM1, MM2
  db $0F, $71, $D1, $08      // PSRLW     MM1, 8
  db $0F, $67, $C8           // PACKUSWB  MM1, MM0
  db $0F, $7E, $C8           // MOVD      EAX, MM1
  end;
*)

constructor TSEGFXBaseGui.Create(SEGuiCallback: TSEGuiCallback;
  AHostPtr: Pointer);
begin
  inherited;
  // FGuiDial.Parent
  CallHost(seGuiHostSetWindowSize, 64, 64);
  CallHost(seGuiHostSetWindowType, 0);
  // 0 = Draw on SE's window (default), 1 = HWND based

  // CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE or HWF_NO_CUSTOM_GFX_ON_STRUCTURE));
  CallHost(seGuiHostSetWindowFlags, Integer(hwfResizable));
  FLock := False;
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf24bit;
  FBitmap.Canvas.Font.Height := 4 * 24;
end;

destructor TSEGFXBaseGui.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TSEGFXBaseGui.GuiPaint(hDC: hDC; wi: PSEWndInfo);
begin
  if not FLock then
  begin
    if FBitmap.Width <> wi.Width then
      FBitmap.Width := wi.Width;
    if FBitmap.Height <> wi.Height then
      FBitmap.Height := wi.Height;
  end;
  with TCanvas.Create do
    try
      Handle := hDC;
      if not FLock then
      begin
        BitBlt(FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height,
          Handle, 0, 0, SRCCOPY);
        PerformBitmapOperation;
      end;
      Draw(0, 0, FBitmap);
    finally
      Free;
    end;
end;

procedure TSEGFXBaseGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
var
  Value: Boolean;
begin
  case CurrentPin.PinIndex of
    0:
      begin
        Value := CurrentPin.ValueAsBoolean;
        if Value <> FLock then
        begin
          FLock := Value;
          if not FLock then
            CallHost(seGuiHostRequestRepaint);
        end;
      end;
  end;
  inherited;
end;

{ TSEGFXAmountGui }

procedure TSEGFXAmountGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
var
  Value: Integer;
begin
  case CurrentPin.PinIndex of
    1:
      begin
        Value := CurrentPin.ValueAsInteger;
        if Value < 0 then
          Value := 0;
        if Value > 255 then
          Value := 255;
        if Value <> FAmount then
        begin
          FAmount := Value;
          CallHost(seGuiHostRequestRepaint);
        end;
      end;
  end;
  inherited;
end;

function IntToByte(i: Integer): Byte;
begin
  if i > 255 then
    Result := 255
  else if i < 0 then
    Result := 0
  else
    Result := i;
end;

procedure TSEGFXAmountGui.GuiWindowOpen(wi: PSEWndInfo);
begin
  inherited;
  FAmount := Pin[0].ValueAsInteger;
end;

{ TSEGFXAddColorNoiseGui }

procedure TSEGFXAddColorNoiseGui.PerformBitmapOperation;
var
  p0: PByteArray;
  X, Y, r, g, b: Integer;

begin
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p0 := FBitmap.Scanline[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      r := p0[X * 3] + (Random(Amount) - (Amount shr 1));
      g := p0[X * 3 + 1] + (Random(Amount) - (Amount shr 1));
      b := p0[X * 3 + 2] + (Random(Amount) - (Amount shr 1));
      p0[X * 3] := IntToByte(r);
      p0[X * 3 + 1] := IntToByte(g);
      p0[X * 3 + 2] := IntToByte(b);
    end;
  end;
end;

{ TSEGFXAddMonoNoiseGui }

procedure TSEGFXAddMonoNoiseGui.PerformBitmapOperation;
var
  p0: PByteArray;
  X, Y, a: Integer;
begin
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p0 := FBitmap.Scanline[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      a := Random(Amount) - (Amount shr 1);
      p0[X * 3] := IntToByte(p0[X * 3] + a);
      p0[X * 3 + 1] := IntToByte(p0[X * 3 + 1] + a);
      p0[X * 3 + 2] := IntToByte(p0[X * 3 + 2] + a);
    end;
  end;
end;

{ TSEGFXContrastGui }

procedure TSEGFXContrastGui.PerformBitmapOperation;
var
  p0: PByteArray;
  rg, gg, bg, r, g, b, X, Y: Integer;
begin
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p0 := FBitmap.Scanline[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      r := p0[X * 3];
      g := p0[X * 3 + 1];
      b := p0[X * 3 + 2];
      rg := (Abs(127 - r) * Amount) div 255;
      gg := (Abs(127 - g) * Amount) div 255;
      bg := (Abs(127 - b) * Amount) div 255;
      if r > 127 then
        r := r + rg
      else
        r := r - rg;
      if g > 127 then
        g := g + gg
      else
        g := g - gg;
      if b > 127 then
        b := b + bg
      else
        b := b - bg;
      p0[X * 3] := IntToByte(r);
      p0[X * 3 + 1] := IntToByte(g);
      p0[X * 3 + 2] := IntToByte(b);
    end;
  end;
end;

{ TSEGFXEmbossGui }

procedure TSEGFXEmbossGui.PerformBitmapOperation;
var
  X, Y: Integer;
  p1, p2: PByteArray;
begin
  for Y := 0 to FBitmap.Height - 2 do
  begin
    p1 := FBitmap.Scanline[Y];
    p2 := FBitmap.Scanline[Y + 1];
    for X := 0 to FBitmap.Width - 2 do
    begin
      p1[X * 3] := (p1[X * 3] + (p2[(X + 1) * 3] xor $FF)) shr 1;
      p1[X * 3 + 1] := (p1[X * 3 + 1] + (p2[(X + 1) * 3 + 1] xor $FF)) shr 1;
      p1[X * 3 + 2] := (p1[X * 3 + 2] + (p2[(X + 1) * 3 + 2] xor $FF)) shr 1;
    end;
  end;
end;

{ TSEGFXInvertGui }

procedure TSEGFXInvertGui.PerformBitmapOperation;
var
  W, h, X, Y: Integer;
  p: PByteArray;
begin
  W := FBitmap.Width;
  h := FBitmap.Height;
  for Y := 0 to h - 1 do
  begin
    p := FBitmap.Scanline[Y];
    for X := 0 to W - 1 do
    begin
      p[X * 3] := not p[X * 3];
      p[X * 3 + 1] := not p[X * 3 + 1];
      p[X * 3 + 2] := not p[X * 3 + 2];
    end;
  end;
end;

{ TSEGFXGrayScaleGui }

procedure TSEGFXGrayScaleGui.PerformBitmapOperation;
var
  p0: PByteArray;
  Gray, X, Y: Integer;
begin
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p0 := FBitmap.Scanline[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      Gray := Round(p0[X * 3] * 0.3 + p0[X * 3 + 1] * 0.59 +
        p0[X * 3 + 2] * 0.11);
      p0[X * 3] := Gray;
      p0[X * 3 + 1] := Gray;
      p0[X * 3 + 2] := Gray;
    end;
  end;
end;

procedure HSLToRGB(const h, S, L: Single; out r, g, b: Single);
var
  M1, M2: Single;
const
  OneThird: Single = 1 / 3;

  function HueToColorValue(Hue: Single): Single;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then
      Result := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      Result := M2
    else if 3 * Hue < 2 then
      Result := M1 + (M2 - M1) * (2 * OneThird - Hue) * 6
    else
      Result := M1;
  end;

begin
  if S = 0 then
  begin
    r := L;
    g := L;
    b := L;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    r := HueToColorValue(h - OneThird);
    g := HueToColorValue(h);
    b := HueToColorValue(h + OneThird)
  end;
end;

procedure RGBToHSL(const r, g, b: Single; out h, S, L: Single);
var
  D, Cmax, Cmin: Single;
const
  OneSixth: Single = 1 / 6;
begin
  Cmax := Max(r, Max(g, b));
  Cmin := Min(r, Min(g, b));
  L := (Cmax + Cmin) * 0.5;

  if Cmax = Cmin then
  begin
    h := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if r = Cmax then
      h := (g - b) / D
    else if g = Cmax then
      h := 2 + (b - r) / D
    else
      h := 4 + (r - g) / D;
    h := h * OneSixth;
    if h < 0 then
      h := h + 1;
  end;
end;

procedure RGBToHue(const r, g, b: Single; out h: Single);
var
  D, Cmax, Cmin: Single;
const
  OneSixth: Single = 1 / 6;
begin
  Cmax := Max(r, Max(g, b));
  Cmin := Min(r, Min(g, b));
  if Cmax = Cmin then
    h := 0
  else
  begin
    D := Cmax - Cmin;
    if r = Cmax then
      h := (g - b) / D
    else if g = Cmax then
      h := 2 + (b - r) / D
    else
      h := 4 + (r - g) / D;
    h := h * OneSixth;
    if h < 0 then
      h := h + 1;
  end;
end;

function GetHueC(Color1, Color2: TColor): Byte;
var
  h1, h2: Single;
begin
  RGBToHue(GetRValue(Color1), GetGValue(Color1), GetBValue(Color1), h1);
  RGBToHue(GetRValue(Color2), GetGValue(Color2), GetBValue(Color2), h2);
  Result := Round((h2 - h1) * 255);
end;

function PixelHue(Color: TColor; Amount: Byte; Dither: Boolean): TColor;
var
  r, g, b, D, Cmax, Cmin, h, S, L, M1, M2: Double;
const
  OneThird: Single = 1 / 3;
  OneSixth: Single = 1 / 6;
  One255th: Single = 1 / 255;

  function HueToColorValue(Hue: Double): Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then
      Result := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      Result := M2
    else if 3 * Hue < 2 then
      Result := M1 + (M2 - M1) * (4 - 6 * Hue)
    else
      Result := M1;
  end;

begin
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  Cmax := Max(r, Max(g, b));
  Cmin := Min(r, Min(g, b));
  L := (Cmax + Cmin) * 0.5;

  if Cmax = Cmin then
  begin
    h := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else if 2 - Cmax - Cmin = 0 then
      S := Infinity
    else
      S := D / (2 - Cmax - Cmin);
    if r = Cmax then
      h := (g - b) / D
    else if g = Cmax then
      h := 2 + (b - r) / D
    else
      h := 4 + (r - g) / D;
    h := h * OneSixth;
    if h < 0 then
      h := h + 1;
  end;

  if Dither then
    h := h + (Amount + Random - Random + Random - Random) * One255th
  else
    h := h + Amount * One255th;
  if S = 0 then
  begin
    r := L;
    g := L;
    b := L;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    r := HueToColorValue(h + OneThird);
    g := HueToColorValue(h);
    b := HueToColorValue(h - OneThird);
  end;
  Result := RGB(trunc(r), trunc(g), trunc(b));
end;

{ TSEGFXHueGui }

procedure TSEGFXHueGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
var
  Value: Boolean;
begin
  case CurrentPin.PinIndex of
    2:
      begin
        Value := CurrentPin.ValueAsBoolean;
        if Value <> FDither then
        begin
          FDither := Value;
          CallHost(seGuiHostRequestRepaint);
        end;
      end;
  end;
  inherited;
end;

procedure TSEGFXHueGui.PerformBitmapOperation;
var
  p0: PByteArray;
  X, Y: Integer;
  Color: TColor;
begin
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p0 := FBitmap.Scanline[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      Color := PixelHue(RGB(p0[X * 3], p0[X * 3 + 1], p0[X * 3 + 2]),
        Amount, True);
      p0[X * 3] := GetRValue(Color);
      p0[X * 3 + 1] := GetGValue(Color);
      p0[X * 3 + 2] := GetBValue(Color);
    end;
  end;
end;

{ TSEGFXFishEyeGui }

procedure FishEye(var Bmp, Dst: TBitmap; Amount: Extended);
var
  xmid, ymid: Single;
  fx, fy: Single;
  r1, r2: Single;
  ifx, ify: Integer;
  dx, dy, tmp: Single;
  rmax, reciprocal: Single;
  ty, tx: Integer;
  WeightX, WeightY: array [0 .. 1] of Single;
  Weight: Single;
  NewRed, NewGreen: Integer;
  NewBlue: Integer;
  TotalRed, TotalGreen: Single;
  TotalBlue: Single;
  ix, iy: Integer;
  sli, slo: PByteArray;
begin
  if Amount = 0 then
    exit;
  xmid := Bmp.Width * 0.5;
  ymid := Bmp.Height * 0.5;
  rmax := Dst.Width * Amount;
  reciprocal := 1 / rmax;

  for ty := 0 to Dst.Height - 1 do
  begin
    for tx := 0 to Dst.Width - 1 do
    begin
      dx := tx - xmid;
      dy := ty - ymid;
      r1 := Sqrt(dx * dx + dy * dy);
      if r1 = 0 then
      begin
        fx := xmid;
        fy := ymid;
      end
      else
      begin
        r2 := rmax * 0.5 * (1 / (1 - r1 * reciprocal) - 1);
        tmp := 1 / r1;
        fx := dx * r2 * tmp + xmid;
        fy := dy * r2 * tmp + ymid;
      end;
      ify := trunc(fy);
      ifx := trunc(fx);

      // Calculate the weights.
      if fy >= 0 then
      begin
        WeightY[1] := fy - ify;
        WeightY[0] := 1 - WeightY[1];
      end
      else
      begin
        WeightY[0] := -(fy - ify);
        WeightY[1] := 1 - WeightY[0];
      end;
      if fx >= 0 then
      begin
        WeightX[1] := fx - ifx;
        WeightX[0] := 1 - WeightX[1];
      end
      else
      begin
        WeightX[0] := -(fx - ifx);
        WeightX[1] := 1 - WeightX[0];
      end;

      if ifx < 0 then
        ifx := Bmp.Width - 1 - (-ifx mod Bmp.Width)
      else if ifx > Bmp.Width - 1 then
        ifx := ifx mod Bmp.Width;
      if ify < 0 then
        ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
      else if ify > Bmp.Height - 1 then
        ify := ify mod Bmp.Height;

      TotalRed := 0.0;
      TotalGreen := 0.0;
      TotalBlue := 0.0;
      for ix := 0 to 1 do
      begin
        for iy := 0 to 1 do
        begin
          if ify + iy < Bmp.Height then
            sli := Bmp.Scanline[ify + iy]
          else
            sli := Bmp.Scanline[Bmp.Height - ify - iy];
          if ifx + ix < Bmp.Width then
          begin
            NewRed := sli[(ifx + ix) * 3];
            NewGreen := sli[(ifx + ix) * 3 + 1];
            NewBlue := sli[(ifx + ix) * 3 + 2];
          end
          else
          begin
            NewRed := sli[(Bmp.Width - ifx - ix) * 3];
            NewGreen := sli[(Bmp.Width - ifx - ix) * 3 + 1];
            NewBlue := sli[(Bmp.Width - ifx - ix) * 3 + 2];
          end;
          Weight := WeightX[ix] * WeightY[iy];
          TotalRed := TotalRed + NewRed * Weight;
          TotalGreen := TotalGreen + NewGreen * Weight;
          TotalBlue := TotalBlue + NewBlue * Weight;
        end;
      end;
      slo := Dst.Scanline[ty];
      slo[tx * 3] := Round(TotalRed);
      slo[tx * 3 + 1] := Round(TotalGreen);
      slo[tx * 3 + 2] := Round(TotalBlue);
    end;
  end;
end;

procedure TSEGFXFishEyeGui.PerformBitmapOperation;
var
  Temp: TBitmap;
begin
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Assign(FBitmap);
    FishEye(Temp, FBitmap, Amount * 0.015625);
  finally
    FreeAndNil(Temp);
  end;
end;

{ TSEGFXLightnessGui }

procedure TSEGFXLightnessGui.PerformBitmapOperation;
var
  p0: PByteArray;
  r, g, b, X, Y: Integer;
begin
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p0 := FBitmap.Scanline[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      r := p0[X * 3];
      g := p0[X * 3 + 1];
      b := p0[X * 3 + 2];
      p0[X * 3] := IntToByte(r + ((255 - r) * Amount) div 255);
      p0[X * 3 + 1] := IntToByte(g + ((255 - g) * Amount) div 255);
      p0[X * 3 + 2] := IntToByte(b + ((255 - b) * Amount) div 255);
    end;
  end;
end;

{ TSEGFXDarknessGui }

procedure Darkness(Bitmap: TBitmap; Amount: Byte);
var
  p0: PByteArray;
  r, g, b, X, Y: Integer;
begin
  for Y := 0 to Bitmap.Height - 1 do
  begin
    p0 := Bitmap.Scanline[Y];
    for X := 0 to Bitmap.Width - 1 do
    begin
      r := p0[X * 3];
      g := p0[X * 3 + 1];
      b := p0[X * 3 + 2];
      p0[X * 3] := IntToByte(r - (r * Amount) div 255);
      p0[X * 3 + 1] := IntToByte(g - (g * Amount) div 255);
      p0[X * 3 + 2] := IntToByte(b - (b * Amount) div 255);
    end;
  end;
end;

procedure TSEGFXDarknessGui.PerformBitmapOperation;
begin
  Darkness(FBitmap, FAmount);
end;

{ TSEGFXSaturationGui }

procedure TSEGFXSaturationGui.PerformBitmapOperation;
var
  p0: PByteArray;
  Gray, r, g, b, X, Y: Integer;
begin
  for Y := 0 to FBitmap.Height - 1 do
  begin
    p0 := FBitmap.Scanline[Y];
    for X := 0 to FBitmap.Width - 1 do
    begin
      r := p0[X * 3];
      g := p0[X * 3 + 1];
      b := p0[X * 3 + 2];
      Gray := (r + g + b) div 3;
      p0[X * 3] := IntToByte(Gray + (((r - Gray) * Amount) div 255));
      p0[X * 3 + 1] := IntToByte(Gray + (((g - Gray) * Amount) div 255));
      p0[X * 3 + 2] := IntToByte(Gray + (((b - Gray) * Amount) div 255));
    end;
  end;
end;

{ TSEGFXSplitBlurGui }

procedure SplitBlur(var Bitmap: TBitmap; Amount: Byte);
var
  p0, p1, p2: PByteArray;
  cx, X, Y: Integer;
  Buf: array [0 .. 3, 0 .. 2] of Byte;
begin
  for Y := 0 to Bitmap.Height - 1 do
  begin
    p0 := Bitmap.Scanline[Y];
    if Y - Amount < 0 then
      p1 := Bitmap.Scanline[Y]
    else { y - Amount > 0 }
      p1 := Bitmap.Scanline[Y - Amount];
    if Y + Amount < Bitmap.Height then
      p2 := Bitmap.Scanline[Y + Amount]
    else { y + Amount >= Height }
      p2 := Bitmap.Scanline[Bitmap.Height - Y];

    for X := 0 to Bitmap.Width - 1 do
    begin
      if X - Amount < 0 then
        cx := X
      else { x - Amount > 0 }
        cx := X - Amount;
      Buf[0, 0] := p1[cx * 3];
      Buf[0, 1] := p1[cx * 3 + 1];
      Buf[0, 2] := p1[cx * 3 + 2];
      Buf[1, 0] := p2[cx * 3];
      Buf[1, 1] := p2[cx * 3 + 1];
      Buf[1, 2] := p2[cx * 3 + 2];
      if X + Amount < Bitmap.Width then
        cx := X + Amount
      else { x + Amount >= Width }
        cx := Bitmap.Width - X;
      Buf[2, 0] := p1[cx * 3];
      Buf[2, 1] := p1[cx * 3 + 1];
      Buf[2, 2] := p1[cx * 3 + 2];
      Buf[3, 0] := p2[cx * 3];
      Buf[3, 1] := p2[cx * 3 + 1];
      Buf[3, 2] := p2[cx * 3 + 2];
      p0[X * 3] := (Buf[0, 0] + Buf[1, 0] + Buf[2, 0] + Buf[3, 0]) shr 2;
      p0[X * 3 + 1] := (Buf[0, 1] + Buf[1, 1] + Buf[2, 1] + Buf[3, 1]) shr 2;
      p0[X * 3 + 2] := (Buf[0, 2] + Buf[1, 2] + Buf[2, 2] + Buf[3, 2]) shr 2;
    end;
  end;
end;

procedure TSEGFXSplitBlurGui.PerformBitmapOperation;
begin
  if Amount = 0 then
    exit;
  SplitBlur(FBitmap, Amount);
end;

{ TSEGFXSplitBlur2Gui }

procedure Downsample2xBitmap24(var Bitmap: TBitmap);
var
  X, Y: Integer;
  Line: Array [0 .. 2] of PByteArray;
begin
  with Bitmap do
  begin
    // first stage
    for Y := 0 to (Height div 2) - 1 do
    begin
      Line[0] := Scanline[Y];
      Line[1] := Scanline[Y * 2];
      Line[2] := Scanline[Y * 2 + 1];
      for X := 0 to (Width div 2) - 1 do
      begin
        Line[0, 3 * X] := (Line[1, 6 * X] + Line[2, 6 * X] + Line[1, 6 * X + 3]
          + Line[2, 6 * X + 3]) div 4;
        Line[0, 3 * X + 1] := (Line[1, 6 * X + 1] + Line[2, 6 * X + 1] + Line[1,
          6 * X + 4] + Line[2, 6 * X + 4]) div 4;
        Line[0, 3 * X + 2] := (Line[1, 6 * X + 2] + Line[2, 6 * X + 2] + Line[1,
          6 * X + 5] + Line[2, 6 * X + 5]) div 4;
      end;
    end;
  end;
end;

procedure Upsample2xBitmap24(var Bitmap: TBitmap);
var
  X, Y: Integer;
  Line: Array [0 .. 2] of PByteArray;
begin
  with Bitmap do
  begin
    // first stage
    for Y := (Height div 2) - 1 downto 0 do
    begin
      Line[0] := Scanline[Y];
      Line[1] := Scanline[Y * 2];
      Line[2] := Scanline[Y * 2 + 1];
      for X := (Width div 2) - 1 downto 0 do
      begin
        Line[1, 6 * X] := Line[0, 3 * X];
        Line[2, 6 * X] := Line[0, 3 * X];
        Line[1, 6 * X + 3] := Line[0, 3 * X];
        Line[2, 6 * X + 3] := Line[0, 3 * X];

        Line[1, 6 * X + 1] := Line[0, 3 * X + 1];
        Line[2, 6 * X + 1] := Line[0, 3 * X + 1];
        Line[1, 6 * X + 4] := Line[0, 3 * X + 1];
        Line[2, 6 * X + 4] := Line[0, 3 * X + 1];

        Line[1, 6 * X + 2] := Line[0, 3 * X + 2];
        Line[2, 6 * X + 2] := Line[0, 3 * X + 2];
        Line[1, 6 * X + 5] := Line[0, 3 * X + 2];
        Line[2, 6 * X + 5] := Line[0, 3 * X + 2];
      end;
    end;
  end;
end;

procedure TSEGFXSplitBlur2Gui.PerformBitmapOperation;
var
  Temp: TBitmap;
begin
  if Amount = 0 then
    exit;
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Width := 2 * FBitmap.Width;
    Temp.Height := 2 * FBitmap.Height;
    Temp.Canvas.Draw(0, 0, FBitmap);
    Upsample2xBitmap24(Temp);
    SplitBlur(Temp, Amount);
    Downsample2xBitmap24(Temp);
    FBitmap.Canvas.Draw(0, 0, Temp);
  finally
    FreeAndNil(Temp);
  end;
end;

{ TSEGFXGaussianBlurGui }

procedure TSEGFXGaussianBlurGui.PerformBitmapOperation;
var
  i: Integer;
begin
  for i := Amount downto 0 do
    SplitBlur(FBitmap, 1);
end;

{ TSEGFXMosaicGui }

procedure TSEGFXMosaicGui.PerformBitmapOperation;
var
  X, Y, i, j: Integer;
  p1, p2: PByteArray;
  r, g, b: Byte;
begin
  Y := 0;
  repeat
    p1 := FBitmap.Scanline[Y];
    repeat
      j := 1;
      repeat
        p2 := FBitmap.Scanline[Y];
        X := 0;
        repeat
          r := p1[X * 3];
          g := p1[X * 3 + 1];
          b := p1[X * 3 + 2];
          i := 1;
          repeat
            p2[X * 3] := r;
            p2[X * 3 + 1] := g;
            p2[X * 3 + 2] := b;
            inc(X);
            inc(i);
          until (X >= FBitmap.Width) or (i > FAmount);
        until X >= FBitmap.Width;
        inc(j);
        inc(Y);
      until (Y >= FBitmap.Height) or (j > FAmount);
    until (Y >= FBitmap.Height) or (X >= FBitmap.Width);
  until Y >= FBitmap.Height;
end;

{ TSEGFXTwistGui }

procedure Twist(var Bmp, Dst: TBitmap; Amount: Integer);
var
  fxmid, fymid: Single;
  txmid, tymid: Single;
  fx, fy: Single;
  tx2, ty2: Single;
  r, tmp: Single;
  Theta: Single;
  ifx, ify: Integer;
  dx, dy: Single;
  Offset: Single;
  ty, tx: Integer;
  WeightX, WeightY: array [0 .. 1] of Single;
  Weight: Single;
  NewRed, NewGreen: Integer;
  NewBlue: Integer;
  TotalRed, TotalGreen: Single;
  TotalBlue: Single;
  ix, iy: Integer;
  sli, slo: PByteArray;

begin
  Offset := -(Pi * 0.5);
  dx := Bmp.Width - 1;
  dy := Bmp.Height - 1;
  r := Sqrt(dx * dx + dy * dy);
  tmp := 1 / Amount;
  tx2 := r;
  ty2 := r;
  txmid := (Bmp.Width - 1) * 0.5; // Adjust these to move center of rotation
  tymid := (Bmp.Height - 1) * 0.5; // Adjust these to move ......
  fxmid := (Bmp.Width - 1) * 0.5;
  fymid := (Bmp.Height - 1) * 0.5;
  if tx2 >= Bmp.Width then
    tx2 := Bmp.Width - 1;
  if ty2 >= Bmp.Height then
    ty2 := Bmp.Height - 1;

  for ty := 0 to Round(ty2) do
  begin
    for tx := 0 to Round(tx2) do
    begin
      dx := tx - txmid;
      dy := ty - tymid;
      r := Sqrt(dx * dx + dy * dy);
      if r = 0 then
      begin
        fx := 0;
        fy := 0;
      end
      else
      begin
        Theta := ArcTan2(dx, dy) - r * tmp - Offset;
        GetSinCos(Theta, fy, fx);
        fx := r * fx;
        fy := r * fy;
      end;
      fx := fx + fxmid;
      fy := fy + fymid;

      ify := trunc(fy);
      ifx := trunc(fx);

      // Calculate the weights.
      if fy >= 0 then
      begin
        WeightY[1] := fy - ify;
        WeightY[0] := 1 - WeightY[1];
      end
      else
      begin
        WeightY[0] := -(fy - ify);
        WeightY[1] := 1 - WeightY[0];
      end;
      if fx >= 0 then
      begin
        WeightX[1] := fx - ifx;
        WeightX[0] := 1 - WeightX[1];
      end
      else
      begin
        WeightX[0] := -(fx - ifx);
        WeightX[1] := 1 - WeightX[0];
      end;

      if ifx < 0 then
        ifx := Bmp.Width - 1 - (-ifx mod Bmp.Width)
      else if ifx > Bmp.Width - 1 then
        ifx := ifx mod Bmp.Width;

      if ify < 0 then
        ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
      else if ify > Bmp.Height - 1 then
        ify := ify mod Bmp.Height;

      TotalRed := 0;
      TotalGreen := 0;
      TotalBlue := 0;
      for ix := 0 to 1 do
      begin
        for iy := 0 to 1 do
        begin
          if ify + iy < Bmp.Height then
            sli := Bmp.Scanline[ify + iy]
          else
            sli := Bmp.Scanline[Bmp.Height - ify - iy];
          if ifx + ix < Bmp.Width then
          begin
            NewRed := sli[(ifx + ix) * 3];
            NewGreen := sli[(ifx + ix) * 3 + 1];
            NewBlue := sli[(ifx + ix) * 3 + 2];
          end
          else
          begin
            NewRed := sli[(Bmp.Width - ifx - ix) * 3];
            NewGreen := sli[(Bmp.Width - ifx - ix) * 3 + 1];
            NewBlue := sli[(Bmp.Width - ifx - ix) * 3 + 2];
          end;
          Weight := WeightX[ix] * WeightY[iy];
          TotalRed := TotalRed + NewRed * Weight;
          TotalGreen := TotalGreen + NewGreen * Weight;
          TotalBlue := TotalBlue + NewBlue * Weight;
        end;
      end;
      slo := Dst.Scanline[ty];
      slo[tx * 3] := Round(TotalRed);
      slo[tx * 3 + 1] := Round(TotalGreen);
      slo[tx * 3 + 2] := Round(TotalBlue);
    end;
  end;
end;

procedure TSEGFXTwistGui.PerformBitmapOperation;
var
  Temp: TBitmap;
begin
  if Amount = 0 then
    exit;
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Assign(FBitmap);
    Twist(Temp, FBitmap, Amount);
  finally
    FreeAndNil(Temp);
  end;
end;

{ TSEGFXSplitlightGui }

procedure TSEGFXSplitlightGui.PerformBitmapOperation;
var
  X, Y, i: Integer;
  p1: PByteArray;

  function SinPixs(a: Integer): Byte;
  const
    Pia: Single = Pi / 510;
  begin
    Result := Round(sin(a * Pia) * 255);
  end;

begin
  for i := 1 to Amount do
    for Y := 0 to FBitmap.Height - 1 do
    begin
      p1 := FBitmap.Scanline[Y];
      for X := 0 to FBitmap.Width - 1 do
      begin
        p1[X * 3] := SinPixs(p1[X * 3]);
        p1[X * 3 + 1] := SinPixs(p1[X * 3 + 1]);
        p1[X * 3 + 2] := SinPixs(p1[X * 3 + 2]);
      end;
    end;
end;

{ TSEGFXTraceGui }

procedure TSEGFXTraceGui.PerformBitmapOperation;
var
  X, Y, i: Integer;
  p1, p2, P3, P4: PByteArray;
  tb, TraceB: Byte;
  Hasb: Boolean;
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf8bit;
    Bitmap.Assign(FBitmap);
    FBitmap.PixelFormat := pf24bit;
    Hasb := False;
    tb := 0;
    TraceB := $00;
    for i := 1 to Amount do
    begin
      for Y := 0 to Bitmap.Height - 2 do
      begin
        p1 := Bitmap.Scanline[Y];
        p2 := Bitmap.Scanline[Y + 1];
        P3 := FBitmap.Scanline[Y];
        P4 := FBitmap.Scanline[Y + 1];
        X := 0;
        repeat
          if p1[X] <> p1[X + 1] then
          begin
            if not Hasb then
            begin
              tb := p1[X + 1];
              Hasb := True;
              P3[X * 3] := TraceB;
              P3[X * 3 + 1] := TraceB;
              P3[X * 3 + 2] := TraceB;
            end
            else
            begin
              if p1[X] <> tb then
              begin
                P3[X * 3] := TraceB;
                P3[X * 3 + 1] := TraceB;
                P3[X * 3 + 2] := TraceB;
              end
              else
              begin
                P3[(X + 1) * 3] := TraceB;
                P3[(X + 1) * 3 + 1] := TraceB;
                P3[(X + 1) * 3 + 2] := TraceB;
              end;
            end;
          end;
          if p1[X] <> p2[X] then
          begin
            if not Hasb then
            begin
              tb := p2[X];
              Hasb := True;
              P3[X * 3] := TraceB;
              P3[X * 3 + 1] := TraceB;
              P3[X * 3 + 2] := TraceB;
            end
            else
            begin
              if p1[X] <> tb then
              begin
                P3[X * 3] := TraceB;
                P3[X * 3 + 1] := TraceB;
                P3[X * 3 + 2] := TraceB;
              end
              else
              begin
                P4[X * 3] := TraceB;
                P4[X * 3 + 1] := TraceB;
                P4[X * 3 + 2] := TraceB;
              end;
            end;
          end;
          inc(X);
        until X >= (Bitmap.Width - 2);
      end;
      if i > 1 then
        for Y := Bitmap.Height - 1 downto 1 do
        begin
          p1 := Bitmap.Scanline[Y];
          p2 := Bitmap.Scanline[Y - 1];
          P3 := FBitmap.Scanline[Y];
          P4 := FBitmap.Scanline[Y - 1];
          X := Bitmap.Width - 1;
          repeat
            if p1[X] <> p1[X - 1] then
            begin
              if not Hasb then
              begin
                tb := p1[X - 1];
                Hasb := True;
                P3[X * 3] := TraceB;
                P3[X * 3 + 1] := TraceB;
                P3[X * 3 + 2] := TraceB;
              end
              else
              begin
                if p1[X] <> tb then
                begin
                  P3[X * 3] := TraceB;
                  P3[X * 3 + 1] := TraceB;
                  P3[X * 3 + 2] := TraceB;
                end
                else
                begin
                  P3[(X - 1) * 3] := TraceB;
                  P3[(X - 1) * 3 + 1] := TraceB;
                  P3[(X - 1) * 3 + 2] := TraceB;
                end;
              end;
            end;
            if p1[X] <> p2[X] then
            begin
              if not Hasb then
              begin
                tb := p2[X];
                Hasb := True;
                P3[X * 3] := TraceB;
                P3[X * 3 + 1] := TraceB;
                P3[X * 3 + 2] := TraceB;
              end
              else
              begin
                if p1[X] <> tb then
                begin
                  P3[X * 3] := TraceB;
                  P3[X * 3 + 1] := TraceB;
                  P3[X * 3 + 2] := TraceB;
                end
                else
                begin
                  P4[X * 3] := TraceB;
                  P4[X * 3 + 1] := TraceB;
                  P4[X * 3 + 2] := TraceB;
                end;
              end;
            end;
            dec(X);
          until X <= 1;
        end;
    end;
  finally
    FreeAndNil(Bitmap);
  end;
end;

{ TSEGFXSolarizeGui }

procedure Solarize(Src, Dst: TBitmap; Amount: Byte);
var
  W, h, X, Y: Integer;
  ps, pd: PByteArray;
  c: Integer;
begin
  W := Src.Width;
  h := Src.Height;
  for Y := 0 to h - 1 do
  begin
    ps := Src.Scanline[Y];
    pd := Dst.Scanline[Y];
    for X := 0 to W - 1 do
    begin
      c := (ps[X * 3] + ps[X * 3 + 1] + ps[X * 3 + 2]) div 3;
      if c > Amount then
      begin
        pd[X * 3] := 255 - ps[X * 3];
        pd[X * 3 + 1] := 255 - ps[X * 3 + 1];
        pd[X * 3 + 2] := 255 - ps[X * 3 + 2];
      end
      else
      begin
        pd[X * 3] := ps[X * 3];
        pd[X * 3 + 1] := ps[X * 3 + 1];
        pd[X * 3 + 2] := ps[X * 3 + 2];
      end;
    end;
  end;
end;

procedure TSEGFXSolarizeGui.PerformBitmapOperation;
var
  Temp: TBitmap;
begin
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Assign(FBitmap);
    Solarize(Temp, FBitmap, Amount);
  finally
    FreeAndNil(Temp);
  end;
end;

{ TSEGFXPosterizeGui }

procedure Posterize(Src, Dst: TBitmap; Amount: Integer);
var
  W, h, X, Y: Integer;
  ps, pd: PByteArray;
  reciprocal: Single;
begin
  W := Src.Width;
  h := Src.Height;
  reciprocal := 1 / Amount;
  for Y := 0 to h - 1 do
  begin
    ps := Src.Scanline[Y];
    pd := Dst.Scanline[Y];
    for X := 0 to W - 1 do
    begin
      pd[X * 3] := Round(ps[X * 3] * reciprocal) * Amount;
      pd[X * 3 + 1] := Round(ps[X * 3 + 1] * reciprocal) * Amount;
      pd[X * 3 + 2] := Round(ps[X * 3 + 2] * reciprocal) * Amount;
    end;
  end;
end;

procedure TSEGFXPosterizeGui.PerformBitmapOperation;
var
  Temp: TBitmap;
begin
  if Amount = 0 then
    exit;
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Assign(FBitmap);
    Posterize(Temp, FBitmap, Amount);
  finally
    FreeAndNil(Temp);
  end;
end;

{ TSEGFXTileGui }

procedure SmoothResize(var Src, Dst: TBitmap);
var
  X, Y, xP, yP, yP2, xP2: Integer;
  Read: array [0 .. 1] of PByteArray;
  t, z, z2, iz2: Integer;
  pc: PByteArray;
  w1, w2, w3, w4: Integer;
  Col1r, col1g, col1b, Col2r, col2g, col2b: Byte;
begin
  xP2 := ((Src.Width - 1) shl 15) div Dst.Width;
  yP2 := ((Src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for Y := 0 to Dst.Height - 1 do
  begin
    xP := 0;
    Read[0] := Src.Scanline[yP shr 15];
    if yP shr 16 < Src.Height - 1 then
      Read[1] := Src.Scanline[yP shr 15 + 1]
    else
      Read[1] := Src.Scanline[yP shr 15];
    pc := Dst.Scanline[Y];
    z2 := yP and $7FFF;
    iz2 := $8000 - z2;
    for X := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      Col1r := Read[0, t * 3];
      col1g := Read[0, t * 3 + 1];
      col1b := Read[0, t * 3 + 2];
      Col2r := Read[1, t * 3];
      col2g := Read[1, t * 3 + 1];
      col2b := Read[1, t * 3 + 2];
      z := xP and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      pc[X * 3 + 2] := (col1b * w1 + Read[0, (t + 1) * 3 + 2] * w2 + col2b * w3
        + Read[1, (t + 1) * 3 + 2] * w4) shr 15;
      pc[X * 3 + 1] := (col1g * w1 + Read[0, (t + 1) * 3 + 1] * w2 + col2g * w3
        + Read[1, (t + 1) * 3 + 1] * w4) shr 15;
      pc[X * 3] := (Col1r * w1 + Read[0, (t + 1) * 3] * w2 + Col2r * w3 +
        Read[1, (t + 1) * 3] * w4) shr 15;
      inc(xP, xP2);
    end;
    inc(yP, yP2);
  end;
end;

procedure Tile(Src, Dst: TBitmap; Amount: Integer);
var
  W, h, i, j, w2, h2: Integer;
  Temp: TBitmap;
begin
  W := Src.Width;
  h := Src.Height;
  Dst.Width := W;
  Dst.Height := h;
  Dst.Canvas.Draw(0, 0, Src);
  if (Amount <= 0) or ((W div Amount) < 5) or ((h div Amount) < 5) then
    exit;
  h2 := h div Amount;
  w2 := W div Amount;
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Width := w2;
    Temp.Height := h2;
    SmoothResize(Src, Temp);
    for j := 0 to Amount - 1 do
      for i := 0 to Amount - 1 do
        Dst.Canvas.Draw(i * w2, j * h2, Temp);
  finally
    FreeAndNil(Temp);
  end;
end;

procedure TSEGFXTileGui.PerformBitmapOperation;
var
  Temp: TBitmap;
begin
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Assign(FBitmap);
    Tile(Temp, FBitmap, Amount);
  finally
    FreeAndNil(Temp);
  end;
end;

{ TSEGFXSpotlightGui }

procedure TSEGFXSpotlightGui.PerformBitmapOperation;
var
  Temp: TBitmap;
  W, h: Integer;
begin
  Darkness(FBitmap, 255 - FAmount);
  W := FBitmap.Width;
  h := FBitmap.Height;
  Temp := TBitmap.Create;
  try
    Temp.PixelFormat := pf24bit;
    Temp.Width := W;
    Temp.Height := h;
    Temp.Canvas.Brush.Style := bsSolid;
    Temp.Canvas.Brush.Color := clBlack;
    Temp.Canvas.FillRect(Rect(0, 0, W, h));
    Temp.Canvas.Brush.Color := clWhite;
    with FBitmap.Canvas.ClipRect do
      Temp.Canvas.Ellipse(Left, Top, Right, Bottom);
    Temp.Transparent := True;
    Temp.TransparentColor := clWhite;
    FBitmap.Canvas.Draw(0, 0, Temp);
  finally
    FreeAndNil(Temp);
  end;
end;

{ TSEGFXBrushedMetalGui }

procedure TSEGFXBrushedMetalGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
var
  Value: Single;
begin
  case CurrentPin.PinIndex of
    2:
      begin
        Value := CurrentPin.ValueAsSingle;
        if Value <> FGradient then
        begin
          FGradient := Value;
          CallHost(seGuiHostRequestRepaint);
        end;
      end;
  end;
  inherited;
end;

procedure TSEGFXBrushedMetalGui.PerformBitmapOperation;
var
  X, Y, v: Integer;
  hght: Integer;
  S: array [0 .. 1] of Single;
  h, hr: Single;
  Line: PByteArray;
begin
  S[0] := 0;
  S[1] := 0;
  hght := FBitmap.Height;
  hr := 1 / hght;
  for Y := 0 to hght - 1 do
  begin
    Line := FBitmap.Scanline[Y];
    h := FGradient * (1 - sqr(2 * (Y - hght div 2) * hr));
    for X := 0 to FBitmap.Width - 1 do
    begin
      S[1] := 0.97 * S[0] + 0.03 * (2 * Random - 1);
      S[0] := S[1];

      // blue
      v := Round(Line[X * 3] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3] := 0
      else if v > 255 then
        Line[X * 3] := 255
      else
        Line[X * 3] := v;

      // green
      v := Round(Line[X * 3 + 1] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3 + 1] := 0
      else if v > 255 then
        Line[X * 3 + 1] := 255
      else
        Line[X * 3 + 1] := v;

      // red
      v := Round(Line[X * 3 + 2] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3 + 2] := 0
      else if v > 255 then
        Line[X * 3 + 2] := 255
      else
        Line[X * 3 + 2] := v;
    end;
  end;
end;

{ TSEGFXBrushedMetal2Gui }

procedure TSEGFXBrushedMetal2Gui.PerformBitmapOperation;
var
  X, Y, v: Integer;
  hght: Integer;
  S: array [0 .. 1] of Single;
  h, hr: Single;
  Line: PByteArray;
begin
  hght := FBitmap.Height;
  hr := 1 / hght;
  for Y := 0 to hght - 1 do
  begin
    Line := FBitmap.Scanline[Y];
    h := FGradient * (1 - sqr(2 * (Y - hght div 2) * hr));
    S[0] := 0;
    S[1] := 0;
    for X := 0 to FBitmap.Width - 1 do
    begin
      S[1] := 0.97 * S[0] + 0.03 * (2 * Random - 1);
      S[0] := S[1];

      // blue
      v := Round(Line[X * 3] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3] := 0
      else if v > 255 then
        Line[X * 3] := 255
      else
        Line[X * 3] := v;

      // green
      v := Round(Line[X * 3 + 1] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3 + 1] := 0
      else if v > 255 then
        Line[X * 3 + 1] := 255
      else
        Line[X * 3 + 1] := v;

      // red
      v := Round(Line[X * 3 + 2] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3 + 2] := 0
      else if v > 255 then
        Line[X * 3 + 2] := 255
      else
        Line[X * 3 + 2] := v;
    end;

    S[0] := 0;
    S[1] := 0;

    for X := FBitmap.Width - 1 downto 0 do
    begin
      S[1] := 0.97 * S[0] + 0.03 * (2 * Random - 1);
      S[0] := S[1];

      // blue
      v := Round(Line[X * 3] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3] := 0
      else if v > 255 then
        Line[X * 3] := 255
      else
        Line[X * 3] := v;

      // green
      v := Round(Line[X * 3 + 1] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3 + 1] := 0
      else if v > 255 then
        Line[X * 3 + 1] := 255
      else
        Line[X * 3 + 1] := v;

      // red
      v := Round(Line[X * 3 + 2] + Amount * (h + S[1]));
      if v < 0 then
        Line[X * 3 + 2] := 0
      else if v > 255 then
        Line[X * 3 + 2] := 255
      else
        Line[X * 3 + 2] := v;
    end;
  end;
end;

(*
  procedure SetupFunctions;
  var
  CpuInfo: TCpuInfo;
  begin
  //WIMDC
  CpuInfo := CPUID;
  MMX_ACTIVE := (CpuInfo.Features and MMX_FLAG) = MMX_FLAG;
  end;

  initialization
  SetupFunctions;
  if MMX_ACTIVE then
  GenAlphaTable;

  finalization
  if MMX_ACTIVE then
  FreeAlphaTable;
*)

end.
