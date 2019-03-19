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
{******************************************************************************}
unit DAV_StkTwoZero;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK two-zero filter class.

  This protected filter subclass implements a two-zero digital filter. A method
  is provided for creating a "notch" in the frequency response while
  maintaining a constant filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_StkCommon, DAV_StkFilter;

type
  TStkTwoZero = class(TStkFilter)
  public
    // Default constructor creates a second-order pass-through filter.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal states of the filter.
    procedure Clear; override;

    // Set the b[0] coefficient value.
    procedure SetB0(const Value: Single);

    // Set the a[1] coefficient value.
    procedure SetB1(const Value: Single);

    // Set the a[2] coefficient value.
    procedure SetB2(const Value: Single);

    // Sets the filter coefficients for a "notch" at \e frequency (in Hz).
    {
      This method determines the filter coefficients corresponding to
      two complex-conjugate zeros with the given \e frequency (in Hz)
      and \e radius from the z-plane origin.  The coefficients are then
      normalized to produce a maximum filter gain of one (independent of
      the filter \e gain parameter).  The resulting filter frequency
      response has a "notch" or anti-resonance at the given \e
      frequency.  The closer the zeros are to the unit-circle (\e radius
      close to or equal to one), the narrower the resulting notch width.
    }
    procedure SetNotch(const Frequency, Radius: Single);

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload; override;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    procedure Tick(const Data: PDAVSingleFixedArray;
      const SampleFrames: Integer); overload;
  end;

implementation

uses
  DAV_Common;

constructor TStkTwoZero.Create(const SampleRate: Single);
var
  a: Single;
  b: array [0 .. 2] of Single;
begin
  inherited Create(SampleRate);
  a := 1.0;
  b[0] := 1;
  b[1] := 0;
  b[2] := 0;
  inherited setCoefficients(3, @b, 1, @a);
end;

destructor TStkTwoZero.Destroy;
begin
  inherited Destroy;
end;

procedure TStkTwoZero.Clear;
begin
  inherited Clear;
end;

procedure TStkTwoZero.SetB0(const Value: Single);
begin
  FB^[0] := Value;
end;

procedure TStkTwoZero.SetB1(const Value: Single);
begin
  PDAV2SingleArray(FB)^[1] := Value;
end;

procedure TStkTwoZero.SetB2(const Value: Single);
begin
  PDAV4SingleArray(FB)^[2] := Value;
end;

procedure TStkTwoZero.SetNotch(const Frequency, Radius: Single);
begin
  PDAV4SingleArray(FB)^[2] := Sqr(Radius);
  PDAV4SingleArray(FB)^[1] := -2.0 * Radius *
    Cos(CTwoPI32 * Frequency * FSampleRateInv);

  // Normalize the filter gain.
  if PDAV4SingleArray(FB)^[1] > 0.0 then
    PDAV4SingleArray(FB)^[0] := 1.0 /
      (1.0 + PDAV4SingleArray(FB)^[1] + PDAV4SingleArray(FB)^[2])
    // Maximum at z = 0.
  else
    PDAV4SingleArray(FB)^[0] := 1.0 /
      (1.0 - PDAV4SingleArray(FB)^[1] + PDAV4SingleArray(FB)^[2]);
  // Maximum at z = -1.
  PDAV4SingleArray(FB)^[1] := PDAV4SingleArray(FB)^[1] *
    PDAV4SingleArray(FB)^[0];
  PDAV4SingleArray(FB)^[2] := PDAV4SingleArray(FB)^[2] *
    PDAV4SingleArray(FB)^[0];
end;

function TStkTwoZero.Tick(const Sample: Single): Single;
begin
  FInputs^[0] := FGain * Sample;

  FOutputs^[0] := PDAV4SingleArray(FB)^[2] * PDAV4SingleArray(FInputs)^[2] +
    PDAV4SingleArray(FB)^[1] * PDAV4SingleArray(FInputs)^[1] +
    PDAV4SingleArray(FB)^[0] * PDAV4SingleArray(FInputs)^[0];

  Move(PDAV4SingleArray(FInputs)^[0], PDAV4SingleArray(FInputs)^[1],
    2 * SizeOf(Single));

  Result := FOutputs^[0];
end;

procedure TStkTwoZero.Tick(const Data: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Data^[Sample] := Tick(Data^[Sample]);
end;

end.
