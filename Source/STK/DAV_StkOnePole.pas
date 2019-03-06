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

unit DAV_StkOnePole;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ Stk one-pole filter class.

  This protected filter subclass implements a one-pole digital filter. A method
  is provided for setting the pole position along the real axis of the z-plane
  while maintaining a constant peak filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkOnePole = class(TStkFilter)
  public
    // Default constructor creates a first-order low-pass filter.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which sets the pole position during instantiation.
    constructor Create(const SampleRate, thePole: Single); reintroduce;
      overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal state of the filter.
    procedure Clear; override;

    // Set the b[0] coefficient value.
    procedure SetB0(const Value: Single);

    // Set the a[1] coefficient value.
    procedure SetA1(const Value: Single);

    // Set the pole position in the z-plane.
    {
      This method sets the pole position along the real-axis of the
      z-plane and normalizes the coefficients for a maximum gain of one.
      A positive pole value produces a low-pass filter, while a negative
      pole value produces a high-pass filter.  This method does not
      affect the filter \e gain value.
    }
    procedure SetPole(const Value: Single);

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload; override;

    // Processes 'SampleFrames' samples inplace
    procedure Tick(const Data: PDAVSingleFixedArray;
      const SampleFrames: Integer); overload;
  end;

implementation

constructor TStkOnePole.Create(const SampleRate: Single);
var
  a: array [0 .. 1] of Single;
  b: Single;
begin
  inherited Create(SampleRate);
  b := 0.1;
  a[0] := 1.0;
  a[1] := -0.9;
  inherited setCoefficients(1, @b, 2, @a);
end;

constructor TStkOnePole.Create(const SampleRate, thePole: Single);
var
  a: array [0 .. 1] of Single;
  b: Single;
begin
  inherited Create(SampleRate);
  a[0] := 1.0;
  a[1] := -0.9;
  // Normalize coefficients for peak unity gain.
  b := (1.0 - abs(thePole));
  a[1] := -thePole;
  inherited setCoefficients(1, @b, 2, @a);
end;

destructor TStkOnePole.Destroy;
begin
  inherited Destroy;
end;

procedure TStkOnePole.Clear;
begin
  inherited Clear;
end;

procedure TStkOnePole.SetB0(const Value: Single);
begin
  FB^[0] := Value;
end;

procedure TStkOnePole.SetA1(const Value: Single);
begin
  PDAV4SingleArray(FA)^[1] := Value;
end;

procedure TStkOnePole.SetPole(const Value: Single);
begin
  // Normalize coefficients for peak unity gain.
  FB^[0] := (1.0 - abs(Value));
  PDAV4SingleArray(FA)^[1] := -Value;
end;

function TStkOnePole.Tick(const Sample: Single): Single;
begin
  FInputs^[0] := Gain * Sample;
  FOutputs^[0] := FB^[0] * FInputs^[0] - PDAV4SingleArray(FA)^[1] *
    PDAV4SingleArray(FOutputs)^[1];
  PDAV4SingleArray(FOutputs)^[1] := FOutputs^[0];
  Result := FOutputs^[0];
end;

procedure TStkOnePole.Tick(const Data: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Data^[Sample] := Tick(Data^[Sample]);
end;

end.
