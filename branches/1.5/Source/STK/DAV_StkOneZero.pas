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

unit DAV_StkOneZero;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK one-zero filter class.

  This protected filter subclass implements a one-zero digital filter. A method
  is provided for setting the zero position along the real axis of the z-plane
  while maintaining a constant filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkOneZero = class(TStkFilter)
  public
    // Default constructor creates a first-order low-pass filter.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which sets the pole position during instantiation.
    constructor Create(const SampleRate, theZero: Single); overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal state of the filter.
    procedure Clear; override;

    // Set the b[0] coefficient value.
    procedure setB0(const Value: Single);

    // Set the b[1] coefficient value.
    procedure setB1(const Value: Single);

    // Set the zero position in the z-plane.
    {
      This method sets the zero position along the real-axis of the
      z-plane and normalizes the coefficients for a maximum gain of one.
      A positive zero value produces a high-pass filter, while a
      negative zero value produces a low-pass filter.  This method does
      not affect the filter \e gain value.
    }
    procedure SetZero(const Value: Single);

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload; override;

    // Processes 'SampleFrames' samples in-place
    procedure Tick(const Data: PDAVSingleFixedArray;
      const SampleFrames: Integer); overload;
  end;

implementation

constructor TStkOneZero.Create(const SampleRate: Single);
var
  b: array [0 .. 1] of Single;
  a: Single;
begin
  inherited Create(SampleRate);
  a := 1.0;
  b[0] := 0.5;
  b[1] := 0.5;
  inherited setCoefficients(2, @b, 1, @a);
end;

constructor TStkOneZero.Create(const SampleRate, theZero: Single);
var
  b: array [0 .. 1] of Single;
  a: Single;
begin
  inherited Create(SampleRate);
  a := 1.0;

  // Normalize coefficients for unity gain.
  b[0] := 1.0 / (1.0 + Abs(theZero));
  b[1] := -theZero * b[0];
  inherited setCoefficients(2, @b, 1, @a);
end;

destructor TStkOneZero.Destroy;
begin
  inherited Destroy;
end;

procedure TStkOneZero.Clear;
begin
  inherited Clear;
end;

procedure TStkOneZero.setB0(const Value: Single);
begin
  FB^[0] := Value;
end;

procedure TStkOneZero.setB1(const Value: Single);
begin
  PDAV4SingleArray(FB)^[1] := Value;
end;

procedure TStkOneZero.SetZero(const Value: Single);
begin
  // Normalize coefficients for unity gain.
  FB^[0] := 1.0 / (1.0 + Abs(Value));
  PDAV4SingleArray(FB)^[1] := -Value * FB^[0];
end;

function TStkOneZero.Tick(const Sample: Single): Single;
begin
  FInputs^[0] := Gain * Sample;

  FOutputs^[0] := PDAV4SingleArray(FB)^[1] * PDAV4SingleArray(FInputs)^[1] +
    FB^[0] * FInputs^[0];
  PDAV4SingleArray(FInputs)^[1] := FInputs^[0];

  Result := FOutputs^[0];
end;

procedure TStkOneZero.Tick(const Data: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Data^[Sample] := Tick(Data^[Sample]);
end;

end.
