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

unit DAV_StkDelayL;

interface

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK linear interpolating Delay line class.

  This delay subclass implements a fractional-length digital delay-line using
  first-order linear interpolation.  A fixed maximum length of 4095 and a
  delay of zero is set using the default constructor. Alternatively, the delay
  and maximum length can be set during instantiation with an overloaded
  constructor.

  Linear interpolation is an efficient technique for achieving fractional
  Delay lengths, though it does introduce high-frequency signal attenuation
  to varying degrees depending on the fractional delay setting. The use of
  higher order Lagrange interpolators can typically improve (minimize) this
  attenuation characteristic.
}

uses
  DAV_StkCommon, DAV_StkDelay; // dialogs,sysutils;

{$I ..\DAV_Compiler.inc}

type
  TStkDelayL = class(TStkDelay)
  private
    procedure SetDelay(const ADelay: Single);
  protected
    FAlpha: Single;
    FOmAlpha: Single;
    FNextOutput: Single;
    FDoNextOut: Boolean;
    FDelay: Single;
  public
    // Default constructor creates a delay-line with maximum length of 4095 samples and zero delay.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which specifies the current and maximum delay-line lengths.

    constructor Create(const SampleRate, ADelay: Single;
      const AMaxDelay: Integer); overload; override;

    // Class destructor.
    destructor Destroy; override;

    // Return the value which will be output by the next call to Tick.
    // This method is valid only for delay settings greater than zero!
    function NextOut: Single;

    // Input one Sample to the delay-line and return one output.
    function Tick(const Sample: Single): Single; override;

    property Delay: Single read FDelay write SetDelay;
  end;

implementation

uses
  DAV_Common;

{ TStkDelayL }

constructor TStkDelayL.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FDoNextOut := True;
end;

constructor TStkDelayL.Create(const SampleRate, ADelay: Single;
  const AMaxDelay: Integer);
begin
  inherited Create(SampleRate, round(ADelay + CHalf32), AMaxDelay);
  FLength := AMaxDelay + 1;
  (*
    if (FLength > 4096) then
    begin
    // We need to delete the previously allocated inputs.
    ReallocMem(FInputs, SizeOf(Single) * FLength);
    Clear;
    end;
  *)
  Delay := ADelay;
  FDoNextOut := True;
end;

destructor TStkDelayL.Destroy;
begin
  inherited Destroy;
end;

function TStkDelayL.NextOut: Single;
begin
  if (FDoNextOut) then
  begin
    // First 1/2 of interpolation
    FNextOutput := FInputs^[FOutPoint] * FOmAlpha;
    // Second 1/2 of interpolation
    if (FOutPoint + 1 < length) then
      FNextOutput := FNextOutput + FInputs^[FOutPoint + 1] * FAlpha
    else
      FNextOutput := FNextOutput + FInputs^[0] * FAlpha;
    FDoNextOut := False;
  end;
  Result := FNextOutput;
end;

procedure TStkDelayL.SetDelay(const ADelay: Single);
var
  OutPointer: Single;
begin
  if (ADelay > length - 1) then
  begin
    // Force delay to maxLength
    OutPointer := FInPoint + 1;
    FDelay := FLength - 1;
  end
  else if (ADelay < 0) then
  begin
    OutPointer := FInPoint;
    FDelay := 0;
  end
  else
  begin
    OutPointer := FInPoint - ADelay; // read chases write
    FDelay := ADelay;
  end;

  while (OutPointer < 0) do
    OutPointer := OutPointer + length; // modulo maximum length

  FOutPoint := round(OutPointer); // integer part
  FAlpha := OutPointer - FOutPoint; // fractional part
  FOmAlpha := 1 - FAlpha;
end;

function TStkDelayL.Tick(const Sample: Single): Single;
begin
  FInputs^[FInPoint] := Sample;
  Inc(FInPoint);
  // Increment input pointer modulo length.
  if FInPoint = FLength then
    FInPoint := FInPoint - FLength;

  FOutputs^[0] := NextOut;
  FDoNextOut := True;

  // Increment output pointer modulo length.
  Inc(FOutPoint);
  if (FOutPoint >= FLength) then
    FOutPoint := FOutPoint - FLength;
  Result := FOutputs[0];
end;

end.
