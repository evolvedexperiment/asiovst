unit DAV_DspFilterChebyshevType1;

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
  Classes, DAV_Complex, DAV_DspFilter, DAV_DspFilterChebyshev;

type
  TCustomChebyshev1Filter = class(TCustomChebyshevFilter)
  private
    procedure SetRipple(const Value: Double);
  protected
    FRipple     : Double;
    FRippleGain : Double;
    FCoeffs     : array [0..63] of Double;
    FState      : array [0..63] of Double;
    FStateStack : array of array [0..63] of Double;
    procedure AssignTo(Dest: TPersistent); override;
    procedure RippleChanged; virtual;
    procedure CalculateHypFactors; override;
    procedure CalculateRippleGain; virtual;
    class function GetMaxOrder: Cardinal; override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure SetFilterValues(const AFrequency, AGain, ARipple : Single); virtual;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure ResetStates; override;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure Reset; override;

    property Ripple : Double read FRipple write SetRipple;
  end;

  TCustomChebyshev1LowpassFilter = class(TCustomChebyshev1Filter)
  public
    function ProcessSample64(Input: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
  end;

  TChebyshev1LowpassFilter = class(TCustomChebyshev1LowpassFilter)
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev1HighCutFilter = TChebyshev1LowpassFilter;

  TChebyshev1LowpassFilterAutomatable = class(TCustomChebyshev1LowpassFilter)
  protected
    procedure CalculateW0; override;
    procedure CalculateExpOrdPiHalf; override;
    procedure CalculateHypFactors; override;
    procedure CalculateRippleGain; override;
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  end;
  TChebyshev1HighCutFilterAutomatable = TChebyshev1LowpassFilterAutomatable;

  TCustomChebyshev1HighpassFilter = class(TCustomChebyshev1Filter)
  public
    function ProcessSample64(Input: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
  end;

  TChebyshev1HighpassFilter = class(TCustomChebyshev1HighpassFilter)
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TChebyshev1LowCutFilter = TChebyshev1HighpassFilter;

  TChebyshev1HighpassFilterAutomatable = class(TCustomChebyshev1HighpassFilter)
  protected
    procedure CalculateW0; override;
    procedure CalculateHypFactors; override;
    procedure OrderChanged; override;
  public
    procedure CalculateCoefficients; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
  end;
  TChebyshev1LowCutFilterAutomatable = TChebyshev1HighpassFilterAutomatable;

implementation

uses
  Math, SysUtils, DAV_Classes, DAV_Common, DAV_Math, DAV_Approximations;

const
  CHalf32 : Single = 0.5;
  CHalf64 : Double = 0.5;

{$IFDEF HandleDenormals}
var
  DenormRandom   : Single;
const
  CDenorm32      : Single = 1E-24;
  CDenorm64      : Double = 1E-34;
{$ENDIF}

{ TCustomChebyshev1Filter }

constructor TCustomChebyshev1Filter.Create(const Order: Integer = 0);
begin
 FRipple := 1;
 FFilterGain := 1;
 CalculateRippleGain;
 inherited Create(Order);
 {$IFDEF HandleDenormals}
 DenormRandom := Random;
 {$ENDIF}
end;

class function TCustomChebyshev1Filter.GetMaxOrder: Cardinal;
begin
 result := 32;
end;

procedure TCustomChebyshev1Filter.CalculateRippleGain;
begin
 assert(FRipple > 0);
 FRippleGain := dB_to_Amp(FRipple);
end;

procedure TCustomChebyshev1Filter.RippleChanged;
begin
 CalculateRippleGain;
 CalculateHypFactors;
 CalculateCoefficients;
end;

procedure TCustomChebyshev1Filter.SetRipple(const Value: Double);
begin
 if Value <> FRipple then
  begin
   FRipple := Value;
   RippleChanged;
  end;
end;

procedure TCustomChebyshev1Filter.Reset;
begin
 Gain := 0;
end;

procedure TCustomChebyshev1Filter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomChebyshev1Filter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChebyshev1Filter then
  with TCustomChebyshev1Filter(Dest) do
   begin
    inherited;
    FRipple     := Self.FRipple;
    FRippleGain := Self.FRippleGain;
    FCoeffs     := Self.FCoeffs;
    FState      := Self.FState;
    FStateStack := Self.FStateStack;
   end
 else inherited;
end;

procedure TCustomChebyshev1Filter.CalculateHypFactors;
var
  t : array [0..1] of Double;
begin
 t[0] := 1 / sqrt(sqr(FRippleGain) - 1);
 t[0] := Ln(t[0] + Sqrt(sqr(t[0]) + 1));
 t[1] := Exp(-t[0] * FOrderInv) * CHalf64;
 t[0] := CQuarter64 / t[1];
 FHypFactors[1] := t[0] - t[1];
 FHypFactors[0] := sqr(t[0] + t[1]);
end;

procedure TCustomChebyshev1Filter.SetFilterValues(const AFrequency, AGain, ARipple : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency  := AFrequency;
 FGain_dB    := AGain;
 FRipple     := ARipple;
 CalculateW0;
 CalculateRippleGain;
 CalculateGainFactor;
 CalculateHypFactors;
 CalculateCoefficients;
end;

procedure TCustomChebyshev1Filter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0],FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TCustomChebyshev1Filter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0], FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

function TCustomChebyshev1Filter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 10 * Log10(MagnitudeSquared(Frequency));
end;


{ TCustomChebyshev1LowpassFilter }

procedure TCustomChebyshev1LowpassFilter.Complex(const Frequency: Double;
  out Real, Imaginary: Double);
var
  i       : Cardinal;
  Cmplx   : TComplexDouble;
  A, B, R : TComplexSingle;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  2 * Cmplx.Re * (Cmplx.Re + 1);
   A.Im := -2 * Cmplx.Im * (Cmplx.Re + 1);
   B.Re :=  1 - FCoeffs[2 * i] * Cmplx.Re - FCoeffs[2 * i + 1] * (2 * sqr(Cmplx.Re) - 1);
   B.Im :=  Cmplx.Im * (FCoeffs[2 * i] + 2 * Cmplx.Re * FCoeffs[2 * i + 1]);
   R := ComplexMultiply(R, ComplexDivide(A, B));
   inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re + 1;
   A.Im := -Cmplx.Im;
   B.Re := -Cmplx.Re * FCoeffs[2 * i] + 1;
   B.Im :=  Cmplx.Im * FCoeffs[2 * i];
   R := ComplexMultiply(R, ComplexDivide(A, B));
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TCustomChebyshev1LowpassFilter.Phase(const Frequency: Double): Double;
var
  Cmplx : array [0..1] of TComplexDouble;
  i     : Integer;
begin
(*
  Complex(Frequency, Cmplx[1].Re, Cmplx[1].Im);
*)
 GetSinCos(2 * Frequency * Pi * SampleRateReciprocal, Cmplx[0].Im, Cmplx[0].Re);
 Cmplx[1].Im := 0; Cmplx[1].Re := 1;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   ComplexMultiplyInplace(Cmplx[1].Re, Cmplx[1].Im,
     (Cmplx[0].Re * (1 - FCoeffs[2 * i + 1] - FCoeffs[2 * i] + Cmplx[0].Re * (1 - FCoeffs[2 * i + 1])) - FCoeffs[2 * i]),
     (Cmplx[0].Im * (1 + FCoeffs[2 * i + 1]) * (Cmplx[0].Re + 1)));
  end;
 Result := ArcTan2(Cmplx[1].Im, Cmplx[1].Re);
end;

function TCustomChebyshev1LowpassFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=     x + FState[2 * i];
   FState[2 * i    ] := 2 * x + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=     x + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        := x + FState[2 * i];
   FState[2 * i] := x + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
 fld   Input.Double;
 {$IFDEF HandleDenormals}
 fadd  CDenorm32
 {$ENDIF}
 fmul  [eax.FFilterGain].Double

 mov   ecx, [eax.FOrder]
 test  ecx, 1
 jz    @BiquadStageCheck

@SingleStage:
 sub   ecx, 1
 fld   st(0)
 fadd  [eax.FState + ecx * 8].Double
 fld   st(0)
 fmul  [eax.FCoeffs + ecx * 8].Double
 faddp st(2), st(0)
 fxch
 fstp  [eax.FState + ecx * 8].Double

@BiquadStageCheck:
 test  ecx, ecx
 jz    @End

@FilterLoop:
 sub   ecx, 2
 fld   st(0)
 fadd  [eax.FState + ecx * 8].Double
 fld   st(0)
 fld   st(0)
 fmul  [eax.FCoeffs + ecx * 8].Double
 fadd  [eax.FState + ecx * 8 + 8].Double
 fld   st(3)
 fadd  st(0), st(0)
 faddp
 fstp  [eax.FState + ecx * 8].Double
 fmul  [eax.FCoeffs + ecx * 8 + 8].Double
 fxch
 fxch  st(2)
 faddp
 fstp  [eax.FState + ecx * 8 + 8].Double
 ja    @FilterLoop

 @End:
 {$ENDIF}
end;


{ TChebyshev1LowpassFilter }

procedure TChebyshev1LowpassFilter.CalculateCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2 : Double;
  t     : array [0..2] of Double;
  i     : Integer;
  Cmplx : TComplexDouble;
begin
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t[0] := K * FHypFactors[1];
   t[1] := 1 / (1 + t[0]);
   FFilterGain := FRippleGain * FFilterGain * t[1] * t[0];
   FCoeffs[FOrder - 1] := (1 - t[0]) * t[1];
   ComplexMultiplyInplace(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t[0] := 1 / (FHypFactors[0] - sqr(Cmplx.Re));
   t[1] := 2 * K * Cmplx.Re * FHypFactors[1] * t[0];
   t[2] := 1 / (t[0] + t[1] + K2);
   FFilterGain := FFilterGain * K2 * t[2];

   FCoeffs[2 * i    ] := 2 * (       t[0] - K2) * t[2];
   FCoeffs[2 * i + 1] :=     (t[1] - t[0] - K2) * t[2];

   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
  end;
{$ELSE}
asm
 mov  ecx, [eax.FOrder]                     // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz   @done                                 // exit if filter order = 0

 fld  [eax.FGainFactorSquared]              // FFilterGain
 fld  [eax.FTanW0Half]                      // K, FFilterGain
 fld  [eax.FExpOrdPiHalf.Im]                // Im(E') := A.Im, K, FFilterGain
 fld  [eax.FExpOrdPiHalf.Re]                // Re(E') := A.Re, A.Im, K, FFilterGain

 mov  ecx, [eax.FOrder]                     // ecx = order

 test ecx, 1
 jz @OrderLoop

  fld  [eax.FExpOrdPiHalf.Re]               // B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fld  [eax.FExpOrdPiHalf.Im]               // B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B.Re - A.Im * B.Im := New A.Re, A.Re, A.Im, K, FFilterGain

  fxch st(2)                                // A.Im, A.Re, New A.Re, K, FFilterGain
  fmul [eax.FExpOrdPiHalf.Re]               // A.Im * B.Re, A.Re, New A.Re, K, FFilterGain
  fxch st(1)                                // A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  fmul  [eax.FExpOrdPiHalf.Im]              // B.Im * A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  faddp                                     // B.Im * A.Re, A.Im * B.Re := New A.Im, New A.Re, K, FFilterGain
  fxch                                      // New A.Re, New A.Im, K, FFilterGain

  fld  [eax.FHypFactors + 8].Double         // FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // K * FHypFactors[1], A.Re, A.Im, K, FFilterGain

  fld1                                      // 1, K * FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // 1 + K * FHypFactors[1], K * FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, 1 + K * FHypFactors[1], K * FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fdivrp                                    // 1 / (1 + K * FHypFactors[1]) := t[0], K * FHypFactors[1], A.Re, A.Im, K, FFilterGain

  fmul st(5), st(0)                         // t[0], K * FHypFactors[1], A.Re, A.Im, K, FFilterGain * t[0]
  fxch st(1)                                // K * FHypFactors[1], t, A.Re, A.Im, K, FFilterGain * t[0]
  fmul st(5), st(0)                         // K * FHypFactors[1], t, A.Re, A.Im, K, FFilterGain * t[0] * K * FHypFactors[1]

  fld1                                      // 1, K * FHypFactors[1], t, A.Re, A.Im, K, FFilterGain * t[0] * K * FHypFactors[1]
  fsubrp                                    // 1 - K * FHypFactors[1], t, A.Re, A.Im, K, FFilterGain * t[0] * K * FHypFactors[1]
  fmulp                                     // (1 - K * FHypFactors[1]) * t, A.Re, A.Im, K, FFilterGain * t[0] * K * FHypFactors[1]

  fstp [eax.FCoeffs + 8 * ecx - 8].Double   // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t[0] * K * FHypFactors[1]

  fxch st(3)                                // FFilterGain * t * K², A.Im, K, A.Re
  fmul  [eax.FRippleGain].Double            // FRippleGain * FFilterGain * t * K², A.Im, K, A.Re
  fxch st(3)                                // A.Im, K, A.Re, FRippleGain * FFilterGain * t * K²


 dec ecx
 jz @clean

 @OrderLoop:
  // calculate t1 = 1 / (FHypFactors[0] - sqr(A.Re));
  fld  st(0)                                // A.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re², A.Re, A.Im, K, FFilterGain
  fld  [eax.FHypFactors].Double             // FHypFactors[0], A.Re², A.Re, A.Im, K, FFilterGain
  fsubrp                                    // FHypFactors[0] - A.Re², A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, FHypFactors[0] - A.Re², A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t1 = 1 / (FHypFactors[0] - A.Re²), A.Re, A.Im, K, FFilterGain

  // calculate t2 = 2 * A.Re * t1 * K * FHypFactors[1];
  fld st(1)                                 // A.Re, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * A.Re, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // 2 * A.Re * t1, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(4)                         // 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain
  fmul [eax.FHypFactors + 8].Double         // t2 = FHypFactors[1]* 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain

  // calculate t = 1 / (t2 + K² + t1);
  fld  st(4)                                // K, t2, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // K², t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // K² + t2, t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(2)                         // K² + t2 + t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, K² + t2 + t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t = 1 / (K² + t2 + t1), t2, t1, A.Re, A.Im, K, FFilterGain

  // FFilterGain = FFilterGain * t;
  fmul st(6), st(0)                         // t, t2, t1, A.Re, A.Im, K, FFilterGain * t
  fxch st(5)                                // K, t2, t1, A.Re, A.Im, t, FFilterGain * t
  fmul st(6), st(0)                         // K, t2, t1, A.Re, A.Im, t, FFilterGain * t * K
  fmul st(6), st(0)                         // K, t2, t1, A.Re, A.Im, t, FFilterGain * t * K²
  fxch st(5)                                // t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K²

  // calculate Coeff[0] = 2 * (t1 - K2) * t
  fld  st(5)                                // K, t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K²
  fmul st(0), st(0)                         // K², t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K²
  fsubr st(0), st(3)                        // t1 - K², t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K²
  fmul st(0), st(1)                         // t * (t1 - K²), t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K²
  fadd st(0), st(0)                         // 2 * t * (t1 - K²), t, t2, t1, A.Re, A.Im, K, FFilterGain * t * K²
  fstp [eax.FCoeffs + 8 * ecx - 16].Double  // store to FCoeffs[2 * i]

  // calculate Coeff[1] = (t2 - t1 - K2) * t;
  fxch st(2)                                // t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K²
  fld  st(5)                                // K, t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K²
  fmul st(0), st(0)                         // K², t1, t2, t, A.Re, A.Im, K, FFilterGain * t * K²
  faddp                                     // t1 + K², t2, t, A.Re, A.Im, K, FFilterGain * t * K²
  fsubp                                     // t2 - t1 - K², t, A.Re, A.Im, K, FFilterGain * t * K²
  fmulp                                     // (t2 - t1 - K²) * t, A.Re, A.Im, K, FFilterGain * t * K²
  fstp [eax.FCoeffs + 8 * ecx - 8].Double   // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t * K²

  // advance complex
  fld  [eax.FExpOrdPiHalf.Re]               // B.Re, A.Re, A.Im, K, FFilterGain
  fld  [eax.FExpOrdPiHalf.Im]               // B.Im, B.Re, A.Re, A.Im, K, FFilterGain
  fmulp                                     // B.Im * B.Re, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * B.Im * B.Re = B'', A.Re, A.Im, K, FFilterGain
  fld  [eax.FExpOrdPiHalf.Re]               // B.Re, B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Re², B'', A.Re, A.Im, K, FFilterGain
  fld  [eax.FExpOrdPiHalf.Im]               // B.Im, B.Re², B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Im², B.Re², B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // B.Im² + B.Re² = B', B'', A.Re, A.Im, K, FFilterGain
  fld st(2)                                 // A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fld st(4)                                 // A.Im, A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B'', A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B' - A.Im * B'' := New A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fxch st(4)                                // A.Im, B', B'', A.Re, New A.Re, K, FFilterGain
  fmulp                                     // A.Im * B', B'', A.Re, New A.Re, K, FFilterGain
  fxch st(2)                                // A.Re, B'', A.Im * B', New A.Re, K, FFilterGain
  fmulp                                     // A.Re * B'', A.Im * B', New A.Re, K, FFilterGain
  faddp                                     // A.Re * B'' + A.Im * B' := New A.Im, New A.Re, K, FFilterGain
  fxch st(1)                                // New A.Re, New A.Im, K, FFilterGain

  // advance
  sub  ecx, 2
 ja  @OrderLoop

@clean:
 fstp st(0)                                 // New A.Im, K, FFilterGain * t * K²
 fstp st(0)                                 // K, FFilterGain  * t * K²
 fstp st(0)                                 // FFilterGain * t * K²
 fstp [eax.FFilterGain].Double             // stack free!

@done:
{$ENDIF}
end;

function TChebyshev1LowpassFilter.MagnitudeSquared(const Frequency: Double): Double;
var                                    
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * cos(2 * Frequency * Pi * SampleRateReciprocal);
 a      := sqr(cw + 2);
 Result := sqr(FFilterGain);
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + sqr(FCoeffs[2 * i]) +
       sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
       cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) / (1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := CDenorm64 + Abs(Result);
end;


{ TCustomChebyshev1HighpassFilter }

procedure TCustomChebyshev1HighpassFilter.Complex(const Frequency: Double;
  out Real, Imaginary: Double);
var
  i       : Cardinal;
  Cmplx   : TComplexDouble;
  A, B, R : TComplexSingle;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 i := 0;
 while i < (FOrder div 2) do
  begin
   A.Re :=  2 * Cmplx.Re * (Cmplx.Re - 1);
   A.Im := -2 * Cmplx.Im * (Cmplx.Re - 1);
   B.Re :=  1 - FCoeffs[2 * i] * Cmplx.Re - FCoeffs[2 * i + 1] * (2 * sqr(Cmplx.Re) - 1);
   B.Im :=  Cmplx.Im * (FCoeffs[2 * i] + 2 * Cmplx.Re * FCoeffs[2 * i + 1]);
   R := ComplexMultiply(R, ComplexDivide(A, B));
   inc(i);
  end;

 if FOrder mod 2 = 1 then
  begin
   A.Re :=  Cmplx.Re - 1;
   A.Im := -Cmplx.Im;
   B.Re := -Cmplx.Re * FCoeffs[2 * i] + 1;
   B.Im :=  Cmplx.Im * FCoeffs[2 * i];
   R := ComplexMultiply(R, ComplexDivide(A, B));
  end;

 Real := R.Re;
 Imaginary := R.Im;
end;

function TCustomChebyshev1HighpassFilter.Phase(const Frequency: Double): Double;
var
  Cmplx    : TComplexDouble;
  Nom, Den : Double;
  i        : Integer;
begin
 GetSinCos(2 * Frequency * Pi * SampleRateReciprocal, Cmplx.Im, Cmplx.Re);
 Nom := 0; Den := 1;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   ComplexMultiplyInplace(Den, Nom,
     (Cmplx.Re * (FCoeffs[2 * i + 1] - FCoeffs[2 * i] - 1 + Cmplx.Re * (1 - FCoeffs[2 * i + 1])) + FCoeffs[2 * i]),
     (Cmplx.Im * (FCoeffs[2 * i + 1] + 1) * (Cmplx.Re - 1)));
  end;
 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   ComplexMultiplyInplace(Den, Nom, (1 + FCoeffs[2 * i]) * (1 - Cmplx.Re),
     Cmplx.Im * (FCoeffs[2 * i] - 1));
  end;
 Result := ArcTan2(Nom, Den);
end;

function TCustomChebyshev1HighpassFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 Result := FFilterGain * Input;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result            :=      x + FState[2 * i];
   FState[2 * i    ] := -2 * x + FCoeffs[2 * i] * Result + FState[2 * i + 1];
   FState[2 * i + 1] :=      x + FCoeffs[2 * i + 1] * Result;
  end;
 if (FOrder mod 2) = 1 then
  begin
   i             := ((FOrder + 1) div 2) - 1;
   x             := Result;
   Result        :=  x + FState[2 * i];
   FState[2 * i] := -x + FCoeffs[2 * i] * Result;
  end;
{$ELSE}
asm
 fld  Input.Double

 // eventually add denormal
 {$IFDEF HandleDenormals}
 mov  edx, DenormRandom
 imul edx, DenormRandom, $08088405
 inc  edx
 shr  edx, 23
 or   edx, $20000000
 mov  DenormRandom, edx
 fadd DenormRandom
 {$ENDIF}

 fmul  [eax.FFilterGain].Double
 mov   ecx, [eax.FOrder]
 test  ecx, ecx
 jz    @End
 shr   ecx, 1
 shl   ecx, 2
 push  ecx
 jz @SingleStage
 @FilterLoop:
  sub  ecx, 4
  fld  st(0)
  fadd [eax.FState + ecx * 4].Double
  fld  st(0)
  fld  st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fadd [eax.FState + ecx * 4 + 8].Double
  fld  st(3)
  fadd st(0), st(0)
  fsubp
  fstp [eax.FState + ecx * 4].Double
  fmul [eax.FCoeffs + ecx * 4 + 8].Double
  fxch
  fxch st(2)
  faddp
  fstp [eax.FState + ecx * 4 + 8].Double
 ja @FilterLoop

 @SingleStage:
 pop ecx
 shr ecx, 1
 sub ecx, [eax.FOrder]
 jz @End
  mov ecx, [eax.FOrder]
  dec ecx
  shl ecx, 1
  fld st(0)
  fadd [eax.FState + ecx * 4].Double
  fld st(0)
  fmul [eax.FCoeffs + ecx * 4].Double
  fsubrp st(2), st(0)
  fxch
  fstp [eax.FState + ecx * 4].Double
 @End:
 {$ENDIF}
end;


{ TChebyshev1LowpassFilterAutomatable }

procedure TChebyshev1LowpassFilterAutomatable.CalculateCoefficients;
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
  Cmplx     : TComplexDouble;
begin
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t1 := 1 / FHypFactors[1];
   t  := 1 / (t1 + K);
   FFilterGain := FRippleGain * FFilterGain * t * K;
   FCoeffs[FOrder - 1] := (t1 - K) * t;
   ComplexMultiplyInplace(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t1 := 1 / (FHypFactors[0] - sqr(Cmplx.Re));
   t2 := 2 * Cmplx.Re * t1 * K * FHypFactors[1];
   t  := 1 / (t2 + t1 + K2);
   FFilterGain := FFilterGain * K2 * t;
   FCoeffs[2 * i    ] := 2 * (     t1 - K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - t1 - K2) * t;

   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
  end;
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateHypFactors;
var
  t : array [0..1] of Single;
begin
 t[0] := 1 / FastSqrtBab1(sqr(FRippleGain) - 1);
 t[0] := FastLog2MinError3(t[0] + FastSqrtBab1(sqr(t[0]) + 1)) * FOrderInv;
 t[1] := FastPower2MinError3(t[0]) * CHalf32;
 t[0] := CQuarter32 / t[1];
 FHypFactors[1] := t[1] - t[0];
 FHypFactors[0] := sqr(t[1] + t[0]);
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateRippleGain;
begin
 FRippleGain := FastdBtoAmpMinError3(FRipple);
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateW0;
begin
 FTanW0Half := FastTan2Term(Pi * FSRR * FFrequency);
end;

function TChebyshev1LowpassFilterAutomatable.MagnitudeLog10(
  const Frequency: Double): Double;
const
  CLogScale : Double = 3.0102999566398119521373889472449;
begin
 result := CLogScale * FastLog2ContinousError4(MagnitudeSquared(Frequency));
end;

function TChebyshev1LowpassFilterAutomatable.MagnitudeSquared(
  const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * FastCosInBounds4Term(2 * Frequency * Pi * SampleRateReciprocal);
 a      := sqr(cw + 2);
 Result := sqr(FFilterGain);
 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a / (1 + sqr(FCoeffs[2 * i]) +
       sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
       cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw + 2) / (1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := CDenorm64 + Abs(Result);
end;

procedure TChebyshev1LowpassFilterAutomatable.CalculateExpOrdPiHalf;
begin
 GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
end;


{ TChebyshev1HighpassFilter }

procedure TChebyshev1HighpassFilter.CalculateCoefficients;
{$DEFINE PUREPASCAL}
{$IFDEF PUREPASCAL}
var
  K, K2 : Double;
  t     : array [0..2] of Double;
  i     : Integer;
  Cmplx : TComplexDouble;
begin
 K  := FTanW0Half;
 K2 := sqr(K);
 FFilterGain := FGainFactorSquared;
 Cmplx := FExpOrdPiHalf;

 if (FOrder mod 2) = 1 then
  begin
   t[0] := K / FHypFactors[1];
   t[1] := 1 / (1 + t[0]);
   FFilterGain := FRippleGain * FFilterGain * t[1];
   FCoeffs[FOrder - 1] := (1 - t[0]) * t[1];
   ComplexMultiplyInplace(Cmplx, FExpOrdPiHalf);
  end;

 for i := 0 to (FOrder div 2) - 1 do
  begin
   t[0] := 1 / (FHypFactors[0] - sqr(Cmplx.Re));
   t[1] := 2 * K * Cmplx.Re * FHypFactors[1] * t[0];
   t[2] := 1 / (t[1] + 1 + t[0] * K2);
   FFilterGain := FFilterGain * t[2];

   FCoeffs[2 * i    ] := 2 * (       1 - t[0] * K2) * t[2];
   FCoeffs[2 * i + 1] :=     (t[1] - 1 - t[0] * K2) * t[2];

   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
  end;
{$ELSE}
asm
 mov  ecx, [self.FOrder]                    // ecx = order
 test ecx, ecx                              // set flags according to ecx
 jz   @done                                 // exit if filter order = 0

 fld  [self.FGainFactorSquared]             // FFilterGain
 fld  [self.FTanW0Half]                     // K, FFilterGain
 fld  [self.FExpOrdPiHalf.Im]               // Im(E') := A.Im, K, FFilterGain
 fld  [self.FExpOrdPiHalf.Re]               // Re(E') := A.Re, A.Im, K, FFilterGain

 mov  ecx, [self.FOrder]                    // ecx = order

 test  ecx, 1
 jz @OrderLoop

  fld  [self.FExpOrdPiHalf.Re]              // B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B.Im, A.Re * B.Re, A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B.Re - A.Im * B.Im := New A.Re, A.Re, A.Im, K, FFilterGain

  fxch st(2)                                // A.Im, A.Re, New A.Re, K, FFilterGain
  fmul [self.FExpOrdPiHalf.Re]              // A.Im * B.Re, A.Re, New A.Re, K, FFilterGain
  fxch st(1)                                // A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  fmul  [self.FExpOrdPiHalf.Im]             // B.Im * A.Re, A.Im * B.Re, New A.Re, K, FFilterGain
  faddp                                     // B.Im * A.Re, A.Im * B.Re := New A.Im, New A.Re, K, FFilterGain
  fxch                                      // New A.Re, New A.Im, K, FFilterGain

  fld  [self.FHypFactors + 8].Double        // FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fdivr st(0), st(3)                        // K / FHypFactors[1], A.Re, A.Im, K, FFilterGain

  fld1                                      // 1, K * FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // 1 + K * FHypFactors[1], K * FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, 1 + K * FHypFactors[1], K * FHypFactors[1], A.Re, A.Im, K, FFilterGain
  fdivrp                                    // 1 / (1 + K * FHypFactors[1]) := t, K * FHypFactors[1], A.Re, A.Im, K, FFilterGain

  fmul st(5), st(0)                         // t, K * FHypFactors[1], A.Re, A.Im, K, FFilterGain * t[0]
  fxch st(1)                                // K * FHypFactors[1], t, A.Re, A.Im, K, FFilterGain * t[0]

  fld1                                      // 1, K * FHypFactors[1], t, A.Re, A.Im, K, FFilterGain * t[0]
  fsubrp                                    // 1 - K * FHypFactors[1], t, A.Re, A.Im, K, FFilterGain * t[0]
  fmulp                                     // (1 - K * FHypFactors[1]) * t, A.Re, A.Im, K, FFilterGain * t[0]

  fstp [self.FCoeffs + 8 * ecx - 8].Double  // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t[0] * K²

  fxch st(3)                                // FFilterGain * t * K², A.Im, K, A.Re
  fmul  [self.FRippleGain].Double           // FRippleGain * FFilterGain * t * K², A.Im, K, A.Re
  fxch st(3)                                // A.Im, K, A.Re, FRippleGain * FFilterGain * t * K²


 dec ecx
 jz @clean

 @OrderLoop:
  // calculate t1 = 1 / (FHypFactors[0] - sqr(A.Re));
  fld  st(0)                                // A.Re, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // A.Re², A.Re, A.Im, K, FFilterGain
  fld  [self.FHypFactors].Double            // FHypFactors[0], A.Re², A.Re, A.Im, K, FFilterGain
  fsubrp                                    // FHypFactors[0] - A.Re², A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, FHypFactors[0] - A.Re², A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t1 = 1 / (FHypFactors[0] - A.Re²), A.Re, A.Im, K, FFilterGain

  // calculate t2 = 2 * A.Re * t1 * K * FHypFactors[1];
  fld st(1)                                 // A.Re, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * A.Re, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // 2 * A.Re * t1, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(4)                         // 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain
  fmul [self.FHypFactors + 8].Double        // t2 = FHypFactors[1]* 2 * A.Re * t1 * K, t1, A.Re, A.Im, K, FFilterGain

  // calculate t = 1 / (t2 + 1 + t1 * K²)
  fld st(1)                                 // t1, t2, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(5)                         // t1 * K, t2, t1, A.Re, A.Im, K, FFilterGain
  fmul st(0), st(5)                         // t1 * K², t2, t1, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, t1 * K², t2, t1, A.Re, A.Im, K, FFilterGain
  faddp                                     // 1 + t1 * K², t2, t1, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(1)                         // 1 + t1 * K² + t2, t2, t1, A.Re, A.Im, K, FFilterGain
  fld1                                      // 1, 1 + t1 * K² + t2, t2, t1, A.Re, A.Im, K, FFilterGain
  fdivrp                                    // t = 1 / (1 + t1 * K² + t2), t2, t1, A.Re, A.Im, K, FFilterGain

  // FFilterGain = FFilterGain * t;
  fmul st(6), st(0)                         // t = 1 / (1 + t1 * K² + t2), t2, t1, A.Re, A.Im, K, FFilterGain * t

  // calculate t1 * K2
  fxch st(2)                                // t1, t2, t, A.Re, A.Im, K, FFilterGain * t
  fmul st(0), st(5)                         // t1 * K, t2, t, A.Re, A.Im, K, FFilterGain * t
  fmul st(0), st(5)                         // t1 * K², t2, t, A.Re, A.Im, K, FFilterGain * t
  fxch st(2)                                // t, t2, t1 * K², A.Re, A.Im, K, FFilterGain * t

  // calculate Coeff[0] := 2 * (1 - t1 * K²) * t
  fld1                                      // 1, t, t2, t1 * K², A.Re, A.Im, K, FFilterGain * t
  fsub st(0), st(3)                         // 1 - t1 * K², t, t2, t1 * K², A.Re, A.Im, K, FFilterGain * t
  fadd st(0), st(0)                         // 2 * (1 - t1 * K²), t, t2, t1 * K², A.Re, A.Im, K, FFilterGain * t
  fmul st(0), st(1)                         // 2 * (1 - t1 * K²) * t, t, t2, t1 * K², A.Re, A.Im, K, FFilterGain * t
  fstp [self.FCoeffs + 8 * ecx - 16].Double // store to FCoeffs[2 * i]

  // calculate Coeff[1] = (t2 - 1 - t1 * K²) * t;
  fxch st(2)                                // t1 * K², t2, t, A.Re, A.Im, K, FFilterGain * t
  fld1                                      // 1, t1 * K², t2, t, A.Re, A.Im, K, FFilterGain * t
  faddp                                     // 1 + t1 * K², t2, t, A.Re, A.Im, K, FFilterGain * t
  fsubp                                     // t2 - (1 + t1 * K²), t, A.Re, A.Im, K, FFilterGain * t
  fmulp                                     // (t2 - (1 + t1 * K²)) * t, A.Re, A.Im, K, FFilterGain * t
  fstp [self.FCoeffs + 8 * ecx - 8].Double  // store to FCoeffs[2 * i + 1], A.Re, A.Im, K, FFilterGain * t

  // advance complex
  fld  [self.FExpOrdPiHalf.Re]              // B.Re, A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, B.Re, A.Re, A.Im, K, FFilterGain
  fmulp                                     // B.Im * B.Re, A.Re, A.Im, K, FFilterGain
  fadd st(0), st(0)                         // 2 * B.Im * B.Re = B'', A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Re]              // B.Re, B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Re², B'', A.Re, A.Im, K, FFilterGain
  fld  [self.FExpOrdPiHalf.Im]              // B.Im, B.Re², B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(0)                         // B.Im², B.Re², B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // B.Im² + B.Re² = B', B'', A.Re, A.Im, K, FFilterGain
  fld st(2)                                 // A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(1)                         // A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fld st(4)                                 // A.Im, A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fmul st(0), st(3)                         // A.Im * B'', A.Re * B', B', B'', A.Re, A.Im, K, FFilterGain
  fsubp                                     // A.Re * B' - A.Im * B'' := New A.Re, B', B'', A.Re, A.Im, K, FFilterGain
  fxch st(4)                                // A.Im, B', B'', A.Re, New A.Re, K, FFilterGain
  fmulp                                     // A.Im * B', B'', A.Re, New A.Re, K, FFilterGain
  fxch st(2)                                // A.Re, B'', A.Im * B', New A.Re, K, FFilterGain
  fmulp                                     // A.Re * B'', A.Im * B', New A.Re, K, FFilterGain
  faddp                                     // A.Re * B'' + A.Im * B' := New A.Im, New A.Re, K, FFilterGain
  fxch st(1)                                // New A.Re, New A.Im, K, FFilterGain

  // advance
  sub  ecx, 2
 jnz @OrderLoop

@clean:
 fstp st(0)                                 // New A.Im, K, FFilterGain * t * K²
 fstp st(0)                                 // K, FFilterGain  * t * K²
 fstp st(0)                                 // FFilterGain * t * K²
 fstp [self.FFilterGain].Double             // stack free!

@done:
{$ENDIF}
end;

function TChebyshev1HighpassFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * cos(2 * Frequency * pi * fSRR);
 a      := sqr(cw - 2);
 Result := sqr(FFilterGain);

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a /
  (1 + sqr(FCoeffs[2 * i]) + sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
   cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));

 if (FOrder mod 2) = 1 then
  begin
   i := ((FOrder + 1) div 2) - 1;
   Result := Result * (cw - 2) / (1 + sqr(FCoeffs[2 * i]) - cw * FCoeffs[2 * i]);
  end;
 Result := CDenorm64 + Abs(Result);
end;

{ TChebyshev1HighpassFilterAutomatable }

procedure TChebyshev1HighpassFilterAutomatable.CalculateCoefficients;
var
  K, K2      : Double;
  t, t1, t2  : Double;
  i          : Integer;
  Cmplx      : TComplexDouble;
begin
 if FOrder = 0 then exit;
 K  := FTanW0Half;
 K2 := sqr(K);
 Cmplx := FExpOrdPiHalf;
 FFilterGain := FGainFactorSquared;
 for i := (FOrder div 2) - 1 downto 0 do
  begin
   t  := Cmplx.Re;
   ComplexMultiply2Inplace(Cmplx, FExpOrdPiHalf);
   t1 := 1 / (FHypFactors[0] - sqr(t));
   t2 := 2 * t * t1 * K * FHypFactors[1];
   t  := 1 / (t2 + 1 + t1 * K2);
   FFilterGain := FFilterGain * t;
   FCoeffs[2 * i    ] := 2 * (     1 - t1 * K2) * t;
   FCoeffs[2 * i + 1] :=     (t2 - 1 - t1 * K2) * t;
  end;
end;

procedure TChebyshev1HighpassFilterAutomatable.CalculateHypFactors;
var
  t : array [0..1] of Single;
const
  CExp1: Single = 0.69314718055994530941723212145818;
begin
 t[0] := 1 / FastSqrtBab1(sqr(FRippleGain) - 1);
 t[0] := FastLog2MinError3(t[0] + FastSqrtBab1(sqr(t[0]) + 1)) * FOrderInv;
 t[1] := FastPower2MinError3(t[0]);
 t[0] := 1 / t[1];
 FHypFactors[1] := (t[1] - t[0]) * CHalf32;
 FHypFactors[0] := sqr((t[1] + t[0]) * CHalf32);
end;

procedure TChebyshev1HighpassFilterAutomatable.CalculateW0;
begin
 FTanW0Half := FastTan2Term(Pi * FSRR * FFrequency);
end;

function TChebyshev1HighpassFilterAutomatable.MagnitudeLog10(
  const Frequency: Double): Double;
const
  CLogScale : Double = 3.0102999566398119521373889472449;
begin
 result := CLogScale * FastLog2ContinousError4(MagnitudeSquared(Frequency));
end;

function TChebyshev1HighpassFilterAutomatable.MagnitudeSquared(const Frequency: Double): Double;
var
  i     : Integer;
  a, cw : Double;
begin
 cw     := 2 * FastCosInBounds4Term(2 * Frequency * pi * fSRR);
 a      := sqr(cw - 2);
 Result := sqr(FFilterGain);

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * a /
  (1 + sqr(FCoeffs[2 * i]) + sqr(FCoeffs[2 * i + 1]) + 2 * FCoeffs[2 * i + 1] +
   cw * ((FCoeffs[2 * i] - cw) * FCoeffs[2 * i + 1] - FCoeffs[2 * i]));
end;

procedure TChebyshev1HighpassFilterAutomatable.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
   CalculateHypFactors;
   ResetStates;
   CalculateCoefficients;
  end else FOrderInv := 1;
end;

initialization
  RegisterDspProcessors32([TChebyshev1LowpassFilter,
    TChebyshev1HighpassFilter, TChebyshev1LowpassFilterAutomatable,
    TChebyshev1HighpassFilterAutomatable]);
  RegisterDspProcessors64([TChebyshev1LowpassFilter,
    TChebyshev1HighpassFilter, TChebyshev1LowpassFilterAutomatable,
    TChebyshev1HighpassFilterAutomatable]);

end.
