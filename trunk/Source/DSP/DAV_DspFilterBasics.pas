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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Classes, DAV_Complex, DAV_DspFilter;

type
  TBasicGainFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  public
    function ProcessSample64(Input: Double): Double; override;
    {$IFNDEF PUREPASCAL}
    function ProcessSampleASM: Double; override;
    {$ENDIF}
  end;

  TBasicPeakFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicPeakAFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicOrfanidisPeakFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicShapeFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  private
    FShape: Double;
    procedure SetShape(const Value: Double);
  protected
    procedure CalculateCoefficients; override;
    procedure CalculateAlpha; override;
    procedure ShapeChanged; virtual;
  public
    property Shape : Double read FShape write SetShape;
  end;

  TBasicAllpassFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicLowShelfFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicLowShelfAFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicLowShelfBFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighShelfFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighShelfAFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighShelfBFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicHighcutFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  private
  protected
    procedure CalculateCoefficients; override;
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double;
      out Imaginary: Double); override;
  end;

  TBasicLowcutFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  public
    procedure Complex(const Frequency: Double; out Real: Double;
      out Imaginary: Double); override;
  end;

  TBasicLowpassFilter = class(TBasicHighcutFilter);
  TBasicHighpassFilter = class(TBasicLowcutFilter);

  TBasicBandpassFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TBasicNotchFilter = class(TBiquadIIRFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

implementation

uses
  Math, DAV_Math, DAV_Common;

{ TBasicGainFilter }

procedure TBasicGainFilter.CalculateCoefficients;
begin
 FNominator[0] := FGainFactorSquared;
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[1] := 0;
 FDenominator[2] := 0;
 inherited;
end;

function TBasicGainFilter.ProcessSample64(Input: Double): Double;
begin
 Result := Input * Sqr(FGainFactor);
end;

{$IFNDEF PUREPASCAL}
function TBasicGainFilter.ProcessSampleASM: Double;
asm
 fmul [eax.FGainFactor].Double
end;
{$ENDIF}


{ TBasicPeakFilter }

procedure TBasicPeakFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := 1 / (FGainFactor + FAlpha);
 FDenominator[2] := (FGainFactor - FAlpha) * t;
 FDenominator[1] := -2 * ExpW0.Re * FGainFactor * t;
 FNominator[1] := FDenominator[1];
 FNominator[0] := (FGainFactor + FAlpha * Sqr(FGainFactor)) * t;
 FNominator[2] := (FGainFactor - FAlpha * Sqr(FGainFactor)) * t;
end;


{ TBasicPeakAFilter }

procedure TBasicPeakAFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := 1 / (1 + FAlpha);
 FDenominator[2] := (1 - FAlpha) * t;
 FDenominator[1] := -2 * ExpW0.Re * t;
 FNominator[1] := FDenominator[1];
 FNominator[0] := (1 + FAlpha * Sqr(FGainFactor)) * t;
 FNominator[2] := (1 - FAlpha * Sqr(FGainFactor)) * t;
end;


{ TBasicOrfanidisPeakFilter }

procedure TBasicOrfanidisPeakFilter.CalculateCoefficients;
var
  Divider           : Double;
  GainRelation      : Double;
  OctaveBandwidth   : Double;
  G0, G1, Gb, DW    : Double;
  K, A, B, C, D, Wq : Double;
begin
 K := ExpW0.Im / (1 + ExpW0.Re);
 G0 := 1;
 GB := Power(G0, 0.5) * Power(FGainFactor, 0.5);
 if FGainFactorSquared = Sqr(G0)
  then GainRelation := 1
  else GainRelation := (Sqr(Gb) - Sqr(G0)) / (FGainFactorSquared - Sqr(G0));
 OctaveBandwidth := 2 * FW0 * sinh(ln22 * FBandwidth);
 G1 := Sqr(G0) * Sqr(Sqr(FW0) - Sqr(Pi)) + FGainFactorSquared * Sqr(Pi * OctaveBandwidth) * GainRelation /
   (Sqr(Sqr(FW0) - Sqr(Pi)) + Sqr(Pi * OctaveBandwidth) * GainRelation);
 G1 := Sqrt(Abs(G1));
 DW := 2 * K * sinh(FW0 / FExpW0.Im * ln22 * FBandWidth);
 Wq := Sqrt((FGainFactorSquared - Sqr(G1)) / (FGainFactorSquared - Sqr(G0))) * Sqr(K);

 C := Sqr(DW) * Abs(Sqr(Gb) - Sqr(G1)) - 2 * Wq * (Abs(Sqr(Gb) - G0 * G1) -
   Sqrt((Sqr(Gb) - Sqr(G0)) * (Sqr(Gb) - Sqr(G1)))); 
 D := 2 * Wq * (abs(FGainFactorSquared - G0 * G1) - Sqrt((FGainFactorSquared - Sqr(G0)) * (FGainFactorSquared - Sqr(G1))));
 A := Sqrt((C + D) / abs(FGainFactorSquared - Sqr(Gb)));
 B := Sqrt((FGainFactor * C + Gb * D) / abs(FGainFactorSquared - Sqr(Gb)));

 Divider := 1 / (1 + Wq + A);
 FNominator[0] := (G1 + G0 * Wq + B) * Divider;
 FNominator[1] := 2 * (G1 - G0 * Wq) * Divider;
 FNominator[2] := (G1 + G0 * Wq - B) * Divider;
 FDenominator[2] := (1 + Wq - A) * Divider;
 FDenominator[1] := -2 * (1 - Wq) * Divider;
end;


{ TBasicShapeFilter }

procedure TBasicShapeFilter.CalculateCoefficients;
var
  t, K, G, V, A  : Double;
begin
 K := ExpW0.Im / (1 + ExpW0.Re);
 A := Power(FGainFactor, (abs(Sqr(FShape) + 0.5 * FShape) - abs(Sqr(FShape) + 0.5 * FShape - 2)) * 0.5);

 if FShape < -1 then
  begin
   G := FGainFactor * (2 + FShape);
   V := Power(FGainFactor, (2 + FShape));

   t               := 1 / (Sqr(K) / V + 1 + FAlpha * A);
   FDenominator[1] := 2 * (Sqr(K) / V - 1) * t;
   FDenominator[2] := t * (Sqr(K) / V + 1 - FAlpha * A);

   FNominator[0]   :=     (Sqr(K) * G + FAlpha / A + 1) * t;
   FNominator[1]   := 2 * (Sqr(K) * G              - 1) * t;
   FNominator[2]   :=     (Sqr(K) * G - FAlpha / A + 1) * t;
  end else
 if FShape > 1 then
  begin
   G := FGainFactor * (2 - FShape);
   V := Power(FGainFactor, (2 - FShape));

   t               := 1 / (Sqr(K) * V + 1 + FAlpha * A);
   FDenominator[1] := 2 * (Sqr(K) * V - 1) * t;
   FDenominator[2] := t * (Sqr(K) * V + 1 - FAlpha * A);

   FNominator[0]   :=     V * (Sqr(K) + FAlpha * A + G) * t;
   FNominator[1]   := 2 * V * (Sqr(K)              - G) * t;
   FNominator[2]   :=     V * (Sqr(K) - FAlpha * A + G) * t;
  end
 else
  begin
   if FShape < 0
    then G := 1
    else G := Power(FGainFactor, 2 * FShape);

   V := Power(FGainFactor, FShape);

   t               := 1 / (Sqr(K) * V + FAlpha * A + 1);
   FDenominator[1] := 2 * (Sqr(K) * V              - 1) * t;
   FDenominator[2] := t * (Sqr(K) * V - FAlpha * A + 1);

   FNominator[0]   :=     G * (Sqr(K) / V + FAlpha / A + 1) * t;
   FNominator[1]   := 2 * G * (Sqr(K) / V              - 1) * t;
   FNominator[2]   :=     G * (Sqr(K) / V - FAlpha / A + 1) * t;
  end;
end;

procedure TBasicShapeFilter.CalculateAlpha;
var
  d : Double;
begin
 if Abs(FShape) > 1
  then d := ln(1 + Power(FBandWidth, Abs(FShape)))
  else d := ln(1 + FBandWidth);
 if Abs(FShape) > 1
  then FAlpha := (ExpW0.Im / (1 + ExpW0.Re)) * d / (sqrt(0.5 * (1 + ExpW0.Re))) * 2
  else FAlpha := (ExpW0.Im / (1 + ExpW0.Re)) * d / (sqrt(0.5 * (1 + ExpW0.Re))) * Power(2, abs(FShape));
end;

procedure TBasicShapeFilter.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   ShapeChanged;
  end;
end;

procedure TBasicShapeFilter.ShapeChanged;
begin
 BandwidthChanged;
 CalculateCoefficients;
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
 t2 := FGainFactorSquared * Sqr(K);
 t3 := 1 / (1 + K * FBandWidth + Sqr(K));
 FNominator[0] := (1 + t1 + t2) * t3;
 FNominator[1] := 2 * (t2 - 1) * t3;
 FNominator[2] := (1 - t1 + t2) * t3;
 FDenominator[1] := 2 * (Sqr(K) - 1) * t3;
 FDenominator[2] := (1 - K * FBandWidth + Sqr(K)) * t3;
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
 t3 := FGainFactor / (CSqrt2 * K + FGainFactor * (1 + t2 * Sqr(K)));
 FNominator[0] := (1 + t1 + Sqr(K)) * t3;
 FNominator[1] := 2 * (Sqr(K) - 1) * t3;
 FNominator[2] := (1 - t1 + Sqr(K)) * t3;
 FDenominator[1] := (2 * (t2 * Sqr(K) - 1)) * t3;
 FDenominator[2] := (1 - CSqrt2 / FGainFactor * K + t2 * Sqr(K)) * t3;
end;


{ TBasicHighShelfFilter }

procedure TBasicHighShelfFilter.CalculateCoefficients;
var
  t, A1, A2 : Double;
  cn, sA    : Double;
begin
 cn := ExpW0.Re;
 sA := 2 * Sqrt(FGainFactor) * FAlpha;
 A1 := FGainFactor + 1;
 A2 := FGainFactor - 1;
 t  := 1 / (A1 - (A2 * cn) + sA);
 FDenominator[1] := 2 * (A2 -A1 * cn) * t;
 FDenominator[2] := (A1 - A2 * cn - sA) * t;
 FNominator[0] := FGainFactor * (A1 + A2 * cn + sA) * t;
 FNominator[1] := FGainFactor * (A2 + A1 * cn) * -2 * t;
 FNominator[2] := FGainFactor * (A1 + A2 * cn - sA) * t;
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
 t[2] := Sqr(FGainFactor);
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
 FNominator[0]   := Sqr(FGainFactor) * (1 - cn) * 0.5 * t;
 FNominator[1]   := 2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
end;

function TBasicHighcutFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * FSRR);
 Result := (Sqr(FNominator[0]) * Sqr(cw + 2))
         / (Sqr(1 - FDenominator[2]) + Sqr(FDenominator[1]) +
           (FDenominator[1] * (FDenominator[2] + 1) + cw * FDenominator[2]) * cw);
end;

procedure TBasicHighcutFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
var
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Divider   := FNominator[0] / (Sqr(FDenominator[2]) - 2 * FDenominator[2] + Sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (1 + (2 * FDenominator[1] + FDenominator[2])
              + 2 * cw * (FDenominator[2] + FDenominator[1] + 1)
              + (2 * Sqr(cw)-1) * (FDenominator[2] + 1)) * Divider;
 Imaginary := (2 * (1 - FDenominator[2])
              + 2 * cw * (1 - FDenominator[2])) * sqrt(1 - Sqr(cw)) * Divider;
end;


{ TBasicLowcutFilter }

procedure TBasicLowcutFilter.CalculateCoefficients;
var
  cn, t : Double;
begin
 t := 1 / (1 + FAlpha);
 cn := ExpW0.Re;
 FNominator[0]   := Sqr(FGainFactor) * (1 + cn) * 0.5 * t;
 FNominator[1]   := -2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
end;

procedure TBasicLowcutFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Divider   := FNominator[0] / ( Sqr(FDenominator[2]) - 2 * FDenominator[2] + Sqr(FDenominator[1]) + 1
                + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := ((1 - 2 * FDenominator[1] + FDenominator[2])
              + cw * 2 * (FDenominator[1] + FDenominator[2] - 1)
              + (2 * Sqr(cw) - 1) * (FDenominator[2] + 1)) * Divider;
 Imaginary := ( 2 * (FDenominator[2] - 1)
              + 2 * cw * (1 - FDenominator[2])) * sqrt(1 - Sqr(cw)) * Divider;
end;


{ TBasicBandpassFilter }

procedure TBasicBandpassFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := 1 / (1 + FAlpha);
 FNominator[0]   := Sqr(FGainFactor) * FAlpha * t;
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
  a := Sqr(FGainFactor);
  FDenominator[1] := -2 * ExpW0.Re * t;
  FDenominator[2] := (1 - FAlpha) * t;

  FNominator[0] := a * t;
  FNominator[1] := FDenominator[1] * a;
  FNominator[2] := FNominator[0];
end;

initialization
  RegisterDspProcessors32([TBasicGainFilter, TBasicPeakFilter,
    TBasicAllpassFilter, TBasicLowShelfFilter, TBasicLowShelfAFilter,
    TBasicLowShelfBFilter, TBasicHighShelfAFilter, TBasicHighShelfBFilter,
    TBasicHighcutFilter, TBasicLowcutFilter, TBasicBandpassFilter,
    TBasicNotchFilter]);
  RegisterDspProcessors64([TBasicGainFilter, TBasicPeakFilter,
    TBasicAllpassFilter, TBasicLowShelfFilter, TBasicLowShelfAFilter,
    TBasicLowShelfBFilter, TBasicHighShelfAFilter, TBasicHighShelfBFilter,
    TBasicHighcutFilter, TBasicLowcutFilter, TBasicBandpassFilter,
    TBasicNotchFilter]);

end.
