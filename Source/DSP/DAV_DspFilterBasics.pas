unit DAV_DspFilterBasics;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon, DAV_DspFilter;

type
  TBasicGainFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  public
    function ProcessSample64(Input: Double): Double; override;
    function ProcessSampleASM: Double; override;
  end;

  TBasicPeakFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicAllpassFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicLowShelfFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicLowShelfAFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicLowShelfBFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighShelfFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighShelfAFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighShelfBFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighcutFilter = class(TBiquadIIRFilter)
  private
  protected
    procedure CalculateCoefficients; override;
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double;
      out Imaginary: Double); override;
  end;

  TBasicLowcutFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  public
    procedure Complex(const Frequency: Double; out Real: Double;
      out Imaginary: Double); override;
  end;

  TBasicLowpassFilter = class(TBasicHighcutFilter);
  TBasicHighpassFilter = class(TBasicLowcutFilter);

  TBasicBandpassFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicNotchFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

implementation

uses
  Math;

{ TBasicGainFilter }

procedure TBasicGainFilter.CalculateCoefficients;
begin
 FNominator[0] := FGainFactorSquared;
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[1] := 0;
 FDenominator[2] := 0;
end;

function TBasicGainFilter.ProcessSample64(Input: Double): Double;
begin
 result := Input * sqr(FGainFactor);
end;

function TBasicGainFilter.ProcessSampleASM: Double;
asm
 fmul [eax.FGainFactor].Double
end;

{ TBasicPeakFilter }

procedure TBasicPeakFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := FGainFactor / (FGainFactor + FAlpha);
 FDenominator[2] := (FGainFactor - FAlpha) / (FGainFactor + FAlpha);
 FDenominator[1] := -2 * ExpW0.Re * t;
 FNominator[1] := FDenominator[1];
 FNominator[0] := (1 + FAlpha * FGainFactor) * t;
 FNominator[2] := (1 - FAlpha * FGainFactor) * t;
 CalcPolesZeros;
end;

{ TBasicAllpassFilter }

procedure TBasicAllpassFilter.CalculateCoefficients;
var
  t, a : Double;
begin
 t               := 1 / (1 + FAlpha);
 a               := FGainFactorSquared;
 FDenominator[1] := -2 * ExpW0.Re * t;
 FDenominator[2] := (1 - FAlpha) * t;
 FNominator[1]   := FDenominator[1] * a;
 FNominator[0]   := FDenominator[2] * a;
 FNominator[2]   := a;
end;

{ TBasicLowShelfFilter }

procedure TBasicLowShelfFilter.CalculateCoefficients;
var
  t, A1, A2 : Double;
  cn, sA    : Double;
begin
 sA := 2 * sqrt(FGainFactor) * FAlpha;
 cn := ExpW0.Re;
 A1 := FGainFactor + 1;
 A2 := FGainFactor - 1;
 t  := 1 / (A1 + A2 * cn + sA);
 FDenominator[1] := -2 * (A2 + A1 * cn) * t;
 FDenominator[2] := (A1 + A2 * cn - sA) * t;
 FNominator[0] := FGainFactor * t * (A1 - A2 * cn + sA);
 FNominator[1] := FGainFactor * t * (A2 - A1 * cn) * 2;
 FNominator[2] := FGainFactor * t * (A1 - A2 * cn - sA);
 CalcPolesZeros;
end;

{ TBasicLowShelfAFilter }

procedure TBasicLowShelfAFilter.CalculateCoefficients;
var
  K, t1, t2, t3: Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K  := FExpW0.Im / (1 + FExpW0.Re);
 t1 := FGainFactor * CSqrt2 * K;
 t2 := FGainFactorSquared * sqr(K);
 t3 := 1 / (1 + K * FBandWidth + sqr(K));
 FNominator[0] := (1 + t1 + t2) * t3;
 FNominator[1] := 2 * (t2 - 1) * t3;
 FNominator[2] := (1 - t1 + t2) * t3;
 FDenominator[1] := 2 * (sqr(K) - 1) * t3;
 FDenominator[2] := (1 - K * FBandWidth + sqr(K)) * t3;
end;

{ TBasicLowShelfBFilter }

procedure TBasicLowShelfBFilter.CalculateCoefficients;
var
  K, t1, t2, t3: Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K  := FExpW0.Im / (1 + FExpW0.Re);
 t1 := K * FBandWidth;
 t2 := 1 / FGainFactorSquared;
 t3 := FGainFactor / (CSqrt2 * K + FGainFactor * (1 + t2 * sqr(K)));
 FNominator[0] := (1 + t1 + sqr(K)) * t3;
 FNominator[1] := 2 * (sqr(K) - 1) * t3;
 FNominator[2] := (1 - t1 + sqr(K)) * t3;
 FDenominator[1] := (2 * (t2 * sqr(K) - 1)) * t3;
 FDenominator[2] := (1 - CSqrt2 / FGainFactor * K + t2 * sqr(K)) * t3;
end;

{ TBasicHighShelfFilter }

procedure TBasicHighShelfFilter.CalculateCoefficients;
var
  t, A1, A2 : Double;
  cn, sA    : Double;
begin
 cn := ExpW0.Re;
 sA := 2 * sqrt(FGainFactor) * FAlpha;
 A1 := FGainFactor + 1;
 A2 := FGainFactor - 1;
 t  := 1 / (A1 - (A2 * cn) + sA);
 FDenominator[1] := 2 * (A2 -A1 * cn) * t;
 FDenominator[2] := (A1 - A2 * cn - sA) * t;
 FNominator[0] := FGainFactor * (A1 + A2 * cn + sA) * t;
 FNominator[1] := FGainFactor * (A2 + A1 * cn) * -2 * t;
 FNominator[2] := FGainFactor * (A1 + A2 * cn - sA) * t;
 CalcPolesZeros;
end;

{ TBasicHighShelfAFilter }

procedure TBasicHighShelfAFilter.CalculateCoefficients;
var
  K : Double;
  t : array [0..4] of Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K    :=  FExpW0.Im / (1 + FExpW0.Re);
 t[1] := K * K;
 t[2] := K * FBandWidth;
 t[4] := Sqr(FGainFactor);
 t[3] := CSqrt2 * FGainFactor * K;
 t[0] := 1 / (1 + t[2] + t[1]);
 FNominator[0] := (t[4] + t[3] + t[1]) * t[0];
 FNominator[1] := 2 * (t[1] - t[4]) * t[0];
 FNominator[2] := (t[4] - t[3] + t[1]) * t[0];
 FDenominator[1] := 2 * (t[1] - 1) * t[0];
 FDenominator[2] := (1 - t[2] + t[1]) * t[0];
end;

{ TBasicHighShelfBFilter }

procedure TBasicHighShelfBFilter.CalculateCoefficients;
var
  K : Double;
  t : array [0..4] of Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K    := FExpW0.Im / (1 + FExpW0.Re);
 t[0] := K * K;
 t[1] := K * FBandWidth;
 t[2] := sqr(FGainFactor);
 t[3] := CSqrt2 * FGainFactor * K;
 t[4] := 1 / (1 + t[3] + t[2] * t[0]);
 FNominator[0] := (1 + t[1] + t[0]) * t[4] * t[2];
 FNominator[1] := 2 * (t[0] - 1) * t[4] * t[2];
 FNominator[2] := (1 - t[1] + t[0]) * t[4] * t[2];
 FDenominator[1] := (2 * (t[2] * t[0] - 1)) * t[4];
 FDenominator[2] := (1 - t[3] + t[2] * t[0]) * t[4];
end;

{ TBasicHighcut }

procedure TBasicHighcutFilter.CalculateCoefficients;
var
  cn, t : Double;
begin
 t := 1 / (1 + FAlpha);
 cn := ExpW0.Re;
 FNominator[0]   := sqr(FGainFactor) * (1 - cn) * 0.5 * t;
 FNominator[1]   := 2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
 CalcPolesZeros;
end;

function TBasicHighcutFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * FSRR);
 Result := (sqr(FNominator[0]) * sqr(cw + 2))
         / (sqr(1 - FDenominator[2]) + sqr(FDenominator[1]) +
           (FDenominator[1] * (FDenominator[2] + 1) + cw * FDenominator[2]) * cw);
end;

procedure TBasicHighcutFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
var
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Divider   := FNominator[0] / (sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (1 + (2 * FDenominator[1] + FDenominator[2])
              + 2 * cw * (FDenominator[2] + FDenominator[1] + 1)
              + (2 * sqr(cw)-1) * (FDenominator[2] + 1)) * Divider;
 Imaginary := (2 * (1 - FDenominator[2])
              + 2 * cw * (1 - FDenominator[2])) * sqrt(1 - sqr(cw)) * Divider;
end;

{ TBasicLowcutFilter }

procedure TBasicLowcutFilter.CalculateCoefficients;
var
  cn, t : Double;
begin
 t := 1 / (1 + FAlpha);
 cn := ExpW0.Re;
 FNominator[0]   := sqr(FGainFactor) * (1 + cn) * 0.5 * t;
 FNominator[1]   := -2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
 CalcPolesZeros;
end;

procedure TBasicLowcutFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Divider   := FNominator[0] / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
                + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := ((1 - 2 * FDenominator[1] + FDenominator[2])
              + cw * 2 * (FDenominator[1] + FDenominator[2] - 1)
              + (2 * sqr(cw) - 1) * (FDenominator[2] + 1)) * Divider;
 Imaginary := ( 2 * (FDenominator[2] - 1)
              + 2 * cw * (1 - FDenominator[2])) * sqrt(1 - sqr(cw)) * Divider;
end;

{ TBasicBandpassFilter }

procedure TBasicBandpassFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := 1 / (1 + FAlpha);
 FNominator[0]   := sqr(FGainFactor) * FAlpha * t;
 FNominator[2]   := -FNominator[0];
 FDenominator[1] := -2 * ExpW0.Re * t;
 FDenominator[2] := (1 - FAlpha) * t;
 FNominator[1]   := 0;
end;

{ TBasicNotchFilter }

procedure TBasicNotchFilter.CalculateCoefficients;
var
  t, a : Double;
begin
  t := 1 / (1 + FAlpha);
  a := sqr(FGainFactor);
  FDenominator[1] := -2 * ExpW0.Re * t;
  FDenominator[2] := (1 - FAlpha) * t;

  FNominator[0] := a * t;
  FNominator[1] := FDenominator[1] * a;
  FNominator[2] := FNominator[0];
end;

end.
