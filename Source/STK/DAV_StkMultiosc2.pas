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
{******************************************************************************}

unit DAV_StkMultiOsc2;

interface

{$I ..\DAV_Compiler.inc}

uses
  Math;

type
  TStkMultiOsc2 = class(TOsc)
  private
    procedure SetMorph(const morph: Single);
    function GetMorph: Single;
    procedure SetActiveWave(i: Integer);
    function GetActiveWave: Integer;
  protected
    FWave: Integer;
    FPwm: Single;
  public
    constructor Create(const SampleRate: Integer); override;
    function Process: Single; override;

    property morph: Single read GetMorph write SetMorph;
    property ActiveWave: Integer read GetActiveWave write SetActiveWave;
  end;

implementation

function FloatMod(const x: Single): Single;
begin
  Result := x - Floor(x);
end;

function FloatPulse(x, a: Single): Single;
begin
  if (FloatMod(x) < a) then
    Result := -1
  else
    Result := 1;
end;

function FloatSaw(x, a: Single): Single;
begin
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  x := FloatMod(x);
  if (x < a) then
    Result := x / a * 2 - 1
  else
    Result := (1 - x) / (1 - a) * 2 - 1;
end;

function FloatTri(x, a: Single): Single;
begin
  x := FloatMod(x + 0.25);
  a := 1 - a;
  if (a < 0.00001) then
    a := 0.00001;
  if (x < 0.5) then
    x := x * 4 - 1
  else
    x := (1 - x) * 4 - 1;
  x := x / (-a);
  if (x < -1) then
    x := -1
  else if (x > 1) then
    x := 1;
  Result := x;
end;

function FloatPower(x, a: Single): Single;
begin
  x := FloatMod(x);
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  Result := Power(x, Exp((a - 0.5) * 10)) * 2 - 1;
end;

function FloatGauss(x, a: Single): Single;
begin
  x := FloatMod(x) * 2 - 1;
  if (a < 0.00001) then
    a := 0.00001;
  Result := Exp(-x * x * (Exp(a * 8) + 5)) * 2 - 1;
end;

function FloatDiode(x, a: Single): Single;
begin
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  a := a * 2 - 1;
  x := cos((x + 0.5) * 2 * pi) - a;
  if (x < 0) then
    x := 0;
  Result := x / (1 - a) * 2 - 1;
end;

function FloatSine(x, a: Single): Single;
var
  y: Single;
begin
  x := FloatMod(x);
  if (x < 0.5) then
    y := 1
  else
    y := -1;
  Result := sin(x * 2 * pi) * (1 - a) + a * y;
end;

function FloatAbsSine(x, a: Single): Single;
begin
  x := FloatMod(x);
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  Result := sin(Power(x, Exp((a - 0.5) * 5)) * pi) * 2 - 1;
end;

function FloatPulseSine(x, a: Single): Single;
begin
  if (a < 0.00001) then
    a := 0.00001;
  x := (FloatMod(x) - 0.5) * Exp((a - 0.5) * log10(128));
  if (x < -0.5) then
    x := -0.5
  else if (x > 0.5) then
    x := 0.5;
  x := sin(x * pi * 2);
  Result := x;
end;

function FloatStretchSine(x, a: Single): Single;
var
  b: Single;
begin
  x := FloatMod(x + 0.5) * 2 - 1;
  a := (a - 0.5) * 4;
  if (a > 0) then
    a := a * 2;
  a := Power(3, a);
  b := Power(abs(x), a);
  if (x < 0) then
    b := -b;
  Result := -sin(b * pi);
end;

function FloatChirp(x, a: Single): Single;
begin
  x := FloatMod(x) * 2 * pi;
  a := (a - 0.5) * 4;
  if (a < 0) then
    a := a * 2;
  a := Power(3, a);
  Result := sin(x * 0.5) * Sin(a * x * x);
end;

function FloatSinEx(x, a: Single): Single;
begin
  x := FloatMod(x);
  Result := 0.5 * (Sin(x * 2 * pi) + Sin(x * 2 * pi * (1 + a * 10)));
end;

function FloatAbsStretchSine(x, a: Single): Single;
var
  b: Single;
begin
  x := FloatMod(x + 0.5) * 2 - 1;
  a := (a - 0.5) * 9;
  a := Power(3, a);
  b := Power(Abs(x), a);
  if (x < 0) then
    b := -b;
  Result := -2 * Power(Sin(b * pi), 2) + 1;
end;

constructor TStkMultiOsc2.Create(const SampleRate: Integer);
begin
  Wave := 0;
  inherited Create(SampleRate);
end;

function TStkMultiOsc2.GetActiveWave: Integer;
begin
  Result := Wave;
end;

function TStkMultiOsc2.GetMorph: Single;
begin
  Result := Pwm;
end;

function TStkMultiOsc2.Process: Single;
var
  y, j: Single;
begin
  j := srate / freq;
  phase := cnt / j;
  case Wave of
    0:
      y := FloatSine(phase, Pwm);
    1:
      y := FloatSaw(phase, Pwm);
    2:
      y := FloatPulse(phase, Pwm);
    3:
      y := FloatTri(phase, Pwm);
    4:
      y := tmp;
    5:
      y := Random * 2 - 1;
    6:
      y := FloatPower(phase, Pwm);
    7:
      y := FloatGauss(phase, Pwm);
    8:
      y := FloatDiode(phase, Pwm);
    9:
      y := FloatStretchSine(phase, Pwm);
    10:
      y := FloatPulseSine(phase, Pwm);
    11:
      y := FloatAbsSine(phase, Pwm);
    12:
      y := FloatAbsStretchSine(phase, Pwm);
    13:
      y := FloatChirp(phase, Pwm);

    14:
      if ((Pwm = 1) or (phase < Pwm)) then
        y := sin(pi * phase / Pwm)
      else
        y := -sin(pi * (phase - Pwm) / (1 - Pwm));
    15:
      if ((Pwm = 1) or (phase < Pwm)) then
        y := phase / Pwm
      else
        y := ((phase - Pwm) / (1 - Pwm)) - 1;
    16:
      if (Pwm = 0) then
      begin
        if (phase < 0.5 * (Pwm + 1)) then
          y := -2 * (phase - Pwm) / (1 - Pwm)
        else
          y := -1 + (2 * phase - (Pwm + 1)) / (1 - Pwm);
      end
      else if (Pwm = 1) then
      begin
        if (phase < 0.5 * Pwm) then
          y := 2 * phase / Pwm
        else
          y := 1 - (2 * phase - Pwm) / Pwm;
      end
      else if (phase < Pwm * 0.5) then
        y := 2 * phase / Pwm
      else if ((phase >= Pwm * 0.5) and (phase < Pwm)) then
        y := 1 - (2 * phase - Pwm) / Pwm
      else if ((phase >= Pwm) and (phase < 0.5 * (Pwm + 1))) then
        y := -2 * (phase - Pwm) / (1 - Pwm)
      else
        y := -1 + (2 * phase - (Pwm + 1)) / (1 - Pwm);
    17:
      y := FloatSine(phase * (Pwm), 0) * ((1 - Pwm) + 1) - (1 - Pwm);
    18:
      y := FloatSinEx(phase, Pwm);
  else
    y := 0;
  end;
  cnt := cnt + 1;
  while (cnt > j) do
  begin
    cnt := cnt - j;
    tmp := Random * 2 - 1;
  end;
  Result := y;
end;

procedure TStkMultiOsc2.SetActiveWave(i: Integer);
begin
  Wave := i;
end;

procedure TStkMultiOsc2.SetMorph(morph: Single);
begin
  if morph > 1 then
    morph := 1
  else if morph < 0 then
    morph := 0;
  Pwm := morph;
end;

end.
