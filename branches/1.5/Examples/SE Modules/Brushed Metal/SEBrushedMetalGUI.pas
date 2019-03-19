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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{   SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/       }
{                                                                              }
{******************************************************************************}

unit SEBrushedMetalGUI;

interface

uses
  Windows, Classes, DAV_SEModule, DAV_SEGUI, Graphics,
  SEBrushedMetalModule;

type
  TRGB24 = packed record
    B, G, R: Byte;
  end;

  TRGB32 = packed record
    R, G, B, A: Byte;
  end;

  TRGB24Array = packed array [0 .. MaxInt div SizeOf(TRGB24) - 1] of TRGB24;
  PRGB24Array = ^TRGB24Array;

  TSEBrushedMetalGui = class(TSEGUIBase)
  private
    FColor: TColor;
    FBitmap: TBitmap;
    FGradient: Single;
    function InvalidateControl: Integer;
    procedure BitmapChanged(Sender: TObject);
  protected
    procedure GuiPaint(hDC: hDC; wi: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer;
      AData: Pointer); override;
    procedure GuiPinValueChange(CurrentPin: TSeGuiPin); override;
    procedure GuiWindowOpen(wi: PSEWndInfo); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback;
      AHostPtr: Pointer); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, DAV_Types;

constructor TSEBrushedMetalGui.Create(SEGuiCallback: TSEGuiCallback;
  AHostPtr: Pointer);
begin
  inherited;
  FGradient := 0.6;
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf24bit;
  FBitmap.OnChange := BitmapChanged;
  CallHost(seGuiHostSetWindowSize, 64, 64);
  CallHost(seGuiHostSetWindowType, 0);
  // 0 = Draw on SE's window (default), 1 = HWND based

  // CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE or HWF_NO_CUSTOM_GFX_ON_STRUCTURE));
  CallHost(seGuiHostSetWindowFlags, Integer(hwfResizable));
end;

destructor TSEBrushedMetalGui.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TSEBrushedMetalGui.GuiPaint(hDC: hDC; wi: PSEWndInfo);
begin
  with TCanvas.Create do
    try
      CallHost(seGuiHostSetWindowSize, 64, 64);
      Handle := hDC;
      with wi^ do
      begin
        FBitmap.Canvas.Lock;
        if FBitmap.Width <> Width then
          FBitmap.Width := Width;
        if FBitmap.Height <> Height then
          FBitmap.Height := Height;
      end;
      Draw(0, 0, FBitmap);
    finally
      Free;
    end;
end;

var // set these variables to your needs, e.g. 360, 255, 255
  MaxHue: Integer = 239;
  MaxSat: Integer = 240;
  MaxLum: Integer = 240;

function HSLtoRGB(H, S, L: Single): TColor;
var
  M1, M2: double;

  function HueToColorValue(Hue: Single): Byte;
  var
    V: Single;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else if Hue > 1 then
      Hue := Hue - 1;
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      V := M2
    else if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (4 - 6 * Hue)
    else
      V := M1;
    Result := Round(255 * V)
  end;

var
  R, G, B: Byte;
begin
  if S = 0 then
  begin
    R := Round(MaxLum * L);
    G := R;
    B := R
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColorValue(H + 1 / 3);
    G := HueToColorValue(H);
    B := HueToColorValue(H - 1 / 3)
  end;
  Result := RGB(R, G, B)
end;

procedure TSEBrushedMetalGui.GuiPinValueChange(CurrentPin: TSeGuiPin);
var
  NewColor: TColor;
begin
  NewColor := FColor;
  case CurrentPin.PinIndex of
    0 .. 2:
      begin
        NewColor := (FColor and $FF000000) or
          (HSLtoRGB(Pin[0].ValueAsSingle, Pin[1].ValueAsSingle,
          Pin[2].ValueAsSingle) and $FFFFFF);
      end;
    3:
      NewColor := (FColor and $FFFFFF) or
        ((Round(255 * CurrentPin.ValueAsSingle) shl 24) and $FF000000);
    4:
      begin
        FGradient := CurrentPin.ValueAsSingle;
        BitmapChanged(Self);
        CallHost(seGuiHostRequestRepaint);
      end;
  end;
  if NewColor <> FColor then
  begin
    FColor := NewColor;
    BitmapChanged(Self);
    CallHost(seGuiHostRequestRepaint);
  end;
  inherited;
end;

procedure TSEBrushedMetalGui.GuiWindowOpen(wi: PSEWndInfo);
begin
  inherited;
  FColor := (Round(255 * Pin[3].ValueAsSingle) shl 24 and $FF000000) or
    (HSLtoRGB(Pin[0].ValueAsSingle, Pin[1].ValueAsSingle, Pin[2].ValueAsSingle)
    and $FFFFFF);
  FGradient := Pin[4].ValueAsSingle;
end;

procedure TSEBrushedMetalGui.BitmapChanged(Sender: TObject);
var
  x, y, V: Integer;
  hght: Integer;
  S: array [0 .. 1] of Single;
  H, hr: Single;
  Line: PRGB24Array;
begin
  S[0] := 0;
  S[1] := 0;
  hght := FBitmap.Height;
  hr := 1 / hght;
  for y := 0 to hght - 1 do
  begin
    Line := FBitmap.Scanline[y];
    H := FGradient * (1 - sqr(2 * (y - hght div 2) * hr));
    for x := 0 to FBitmap.Width - 1 do
    begin
      S[1] := 0.97 * S[0] + 0.03 * (2 * random - 1);
      S[0] := S[1];

      // blue
      V := Round(TRGB32(FColor).B + TRGB32(FColor).A * (H + S[1]));
      if V < 0 then
        Line[x].B := 0
      else if V > 255 then
        Line[x].B := 255
      else
        Line[x].B := V;

      // green
      V := Round(TRGB32(FColor).G + TRGB32(FColor).A * (H + S[1]));
      if V < 0 then
        Line[x].G := 0
      else if V > 255 then
        Line[x].G := 255
      else
        Line[x].G := V;

      // red
      V := Round(TRGB32(FColor).R + TRGB32(FColor).A * (H + S[1]));
      if V < 0 then
        Line[x].R := 0
      else if V > 255 then
        Line[x].R := 255
      else
        Line[x].R := V;
    end;
  end;
end;

procedure TSEBrushedMetalGui.GuiModuleMsg(AUserMsgID, ALength: Integer;
  AData: Pointer);
begin
  InvalidateControl;
end;

function TSEBrushedMetalGui.InvalidateControl: Integer;
begin
  Result := CallHost(seGuiHostRequestRepaint);
end;

end.
