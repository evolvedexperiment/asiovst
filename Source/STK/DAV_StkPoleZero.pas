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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_StkPoleZero;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK one-pole, one-zero filter class.

  This protected filter subclass implements a one-pole, one-zero digital
  filter. A method is provided for creating an allpass filter with a given
  coefficient. Another method is provided to create a DC blocking filter.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkPoleZero = class(TStkFilter)
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Clears the internal states of the filter.
    procedure Clear; override;

    // Set the b[0] coefficient value.
    procedure SetB0(const Value: Single);

    // Set the b[1] coefficient value.
    procedure SetB1(const Value: Single);

    // Set the a[1] coefficient value.
    procedure SetA1(const Value: Single);

    // Set the filter for allpass behavior using \e coefficient.
    {
      This method uses \e coefficient to create an allpass filter,
      which has unity gain at all frequencies.  Note that the \e
      coefficient magnitude must be less than one to maintain stability.
    }
    procedure SetAllpass(const Coefficient: Single);

    // Create a DC blocking filter with the given pole position in the z-plane.
    {
      This method sets the given pole position, together with a zero
      at z=1, to create a DC blocking filter.  \e thePole should be
      close to one to minimize low-frequency attenuation.
    }
    procedure SetBlockZero(const Value: Single = 0.99);

    // Set the filter gain.
    {
      The gain is applied at the filter input and does not affect the
      coefficient values.  The default gain value is 1.0.
    }
    // Input one sample to the filter and return one output.
    function Tick(const Input: Single): Single; overload; override;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    procedure Tick(const Data: PDAVSingleFixedArray;
      const SampleFrames: Integer); overload;
  end;

implementation

constructor TStkPoleZero.Create(const SampleRate: Single);
var
  a, b: array [0 .. 1] of Single;
begin
  inherited Create(SampleRate);
  // Default setting for pass-through.
  b[0] := 1;
  b[1] := 0;
  a[0] := 1;
  a[1] := 0;
  inherited setCoefficients(2, @b, 2, @a);
end;

destructor TStkPoleZero.Destroy;
begin
  inherited Destroy;
end;

procedure TStkPoleZero.Clear;
begin
  inherited Clear;
end;

procedure TStkPoleZero.SetB0(const Value: Single);
begin
  FB^[0] := Value;
end;

procedure TStkPoleZero.SetB1(const Value: Single);
begin
  PDav4SingleArray(FB)^[1] := Value;
end;

procedure TStkPoleZero.SetA1(const Value: Single);
begin
  PDav4SingleArray(FA)^[1] := Value;
end;

procedure TStkPoleZero.SetAllpass(const Coefficient: Single);
begin
  FB^[0] := Coefficient;
  PDav4SingleArray(FB)^[1] := 1.0;
  FA^[0] := 1.0; // just in case
  PDav4SingleArray(FA)^[1] := Coefficient;
end;

procedure TStkPoleZero.SetBlockZero(const Value: Single = 0.99); // 0.99
begin
  FB^[0] := 1.0;
  PDav4SingleArray(FB)^[1] := -1.0;
  FA^[0] := 1.0; // just in case
  PDav4SingleArray(FA)^[1] := -Value;
end;

function TStkPoleZero.Tick(const Input: Single): Single;
begin
  FInputs^[0] := FGain * Input;
  FOutputs^[0] := FB^[0] * FInputs^[0] + PDav4SingleArray(FB)^[1] *
    PDav4SingleArray(FInputs)^[1] - PDav4SingleArray(FA)^[1] *
    PDav4SingleArray(FOutputs)^[1];
  PDav4SingleArray(FInputs)^[1] := FInputs^[0];
  PDav4SingleArray(FOutputs)^[1] := FOutputs^[0];
  Result := FOutputs^[0];
end;

procedure TStkPoleZero.Tick(const Data: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1 do
    Data^[Sample] := Tick(Data^[Sample]);
end;

end.
