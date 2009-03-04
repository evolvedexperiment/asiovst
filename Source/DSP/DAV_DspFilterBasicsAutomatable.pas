unit DAV_DspFilterBasicsAutomatable;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon, DAV_DspFilter;

type
  TCustomAutomatable = class(TBiquadIIRFilter)
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double):Double; override;
  end;

  TAutomatableGainFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  public
    function ProcessSample(const Input: Double): Double; override;
    function ProcessSampleASM: Double; override;
  end;

  TAutomatablePeakFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableAllpassFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableLowShelfFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableLowShelfAFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableLowShelfBFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableHighShelfFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableHighShelfAFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableHighShelfBFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableHighcutFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableLowcutFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableLowpassFilter = class(TAutomatableHighcutFilter);
  TAutomatableHighpassFilter = class(TAutomatableLowcutFilter);

  TAutomatableBandpassFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAutomatableNotchFilter = class(TCustomAutomatable)
  protected
    procedure CalculateCoefficients; override;
  end;

implementation

uses
  Math, DAV_Approximations;

{ TCustomAutomatable }

function TCustomAutomatable.MagnitudeLog10(const Frequency: Double): Double;
const
  C3dB : Double = 3.0102999566398119521373889472449;
begin
 Result := C3dB * FastLog2ContinousError3(MagnitudeSquared(Frequency));
end;

function TCustomAutomatable.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * FastCosInBounds3Term(2 * Frequency * Pi * FSRR);
 Result := (sqr(FNominator[0] - FNominator[2]) + sqr(FNominator[1]) + (FNominator[1] * (FNominator[0] + FNominator[2]) + FNominator[0] * FNominator[2] * cw) * cw)
         / (sqr(1 - FDenominator[2]) + sqr(FDenominator[1]) + (FDenominator[1] * (FDenominator[2] + 1) + cw * FDenominator[2]) * cw );
end;

{ TAutomatableGainFilter }

procedure TAutomatableGainFilter.CalculateCoefficients;
begin
 FNominator[0] := FGainFactorSquared;
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[1] := 0;
 FDenominator[2] := 0;
end;

function TAutomatableGainFilter.ProcessSample(const Input: Double): Double;
begin
 result := Input * sqr(FGainFactorSquared);
end;

function TAutomatableGainFilter.ProcessSampleASM: Double;
asm
 fmul [eax.FGainFactorSquared].Double
end;

{ TAutomatablePeakFilter }

procedure TAutomatablePeakFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := FGainFactor / (FGainFactor + FAlpha);
 FDenominator[2] := (FGainFactor - FAlpha) / (FGainFactor + FAlpha);
 FDenominator[1] := -2 * FastCosInBounds3Term(FW0) * t;
 FNominator[1] := FDenominator[1];
 FNominator[0] := (1 + FAlpha * FGainFactor) * t;
 FNominator[2] := (1 - FAlpha * FGainFactor) * t;
 CalcPolesZeros;
end;

{ TAutomatableAllpassFilter }

procedure TAutomatableAllpassFilter.CalculateCoefficients;
var
  t, a : Double;
begin
 t               := 1 / (1 + FAlpha);
 a               := FGainFactorSquared;
 FDenominator[1] := -2 * FastCosInBounds3Term(FW0) * t;
 FDenominator[2] := (1 - FAlpha) * t;
 FNominator[1]   := FDenominator[1] * a;
 FNominator[0]   := FDenominator[2] * a;
 FNominator[2]   := a;
end;

{ TAutomatableLowShelfFilter }

procedure TAutomatableLowShelfFilter.CalculateCoefficients;
var
  t, A1, A2 : Double;
  cn, sA    : Double;
begin
 sA := 2 * FastSqrtBab1(FGainFactor) * FAlpha;
 cn := FastCosInBounds3Term(FW0);
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

{ TAutomatableLowShelfAFilter }

procedure TAutomatableLowShelfAFilter.CalculateCoefficients;
var
  K, t1, t2, t3: Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K  := Tan(FW0 * 0.5);
 t1 := FGainFactor * CSqrt2 * K;
 t2 := FGainFactorSquared * sqr(K);
 t3 := 1 / (1 + K * FBandWidth + sqr(K));
 FNominator[0] := (1 + t1 + t2) * t3;
 FNominator[1] := 2 * (t2 - 1) * t3;
 FNominator[2] := (1 - t1 + t2) * t3;
 FDenominator[1] := 2 * (sqr(K) - 1) * t3;
 FDenominator[2] := (1 - K * FBandWidth + sqr(K)) * t3;
end;

{ TAutomatableLowShelfBFilter }

procedure TAutomatableLowShelfBFilter.CalculateCoefficients;
var
  K, t1, t2, t3: Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K  := Tan(FW0 * 0.5);
 t1 := K * FBandWidth;
 t2 := 1 / FGainFactorSquared;
 t3 := FGainFactor / (CSqrt2 * K + FGainFactor * (1 + t2 * sqr(K)));
 FNominator[0] := (1 + t1 + sqr(K)) * t3;
 FNominator[1] := 2 * (sqr(K) - 1) * t3;
 FNominator[2] := (1 - t1 + sqr(K)) * t3;
 FDenominator[1] := (2 * (t2 * sqr(K) - 1)) * t3;
 FDenominator[2] := (1 - CSqrt2 / FGainFactor * K + t2 * sqr(K)) * t3;
end;

{ TAutomatableHighShelfFilter }

procedure TAutomatableHighShelfFilter.CalculateCoefficients;
var
  t, A1, A2 : Double;
  cn, sA    : Double;
begin
 cn := FastCosInBounds3Term(FW0);
 sA := 2 * FastSqrtBab1(FGainFactor) * FAlpha;
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

{ TAutomatableHighShelfAFilter }

procedure TAutomatableHighShelfAFilter.CalculateCoefficients;
var
 K, t1, t2, t3, t5, t6: Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K     := Tan(FW0 * 0.5);
 t2    := K * K;
 t3    := K * FBandWidth;
 t6    := Sqr(FGainFactor);
 t5    := CSqrt2 * FGainFactor * K;
 t1    := 1 / (1 + t3 + t2);
 FNominator[0] := (t6 + t5 + t2) * t1;
 FNominator[1] := 2 * (t2 - t6) * t1;
 FNominator[2] := (t6 - t5 + t2) * t1;
 FDenominator[1] := 2 * (t2 - 1) * t1;
 FDenominator[2] := (1 - t3 + t2) * t1;
end;

{ TAutomatableHighShelfBFilter }

procedure TAutomatableHighShelfBFilter.CalculateCoefficients;
var
 K, t1, t2, t3, t4, t5: Double;
const
  CSqrt2: Double = 1.4142135623730950488016887242097;
begin
 K     := Tan(FW0 * 0.5);
 t2    := K * K;
 t3    := K * FBandWidth;
 t4    := sqr(FGainFactor);
 t5    := CSqrt2 * FGainFactor * K;
 t1    := 1 / (1 + t5 + t4 * t2);
 FNominator[0] := (1 + t3 + t2) * t1 * t4;
 FNominator[1] := 2 * (t2 - 1) * t1 * t4;
 FNominator[2] := (1 - t3 + t2) * t1 * t4;
 FDenominator[1] := (2 * (t4 * t2 - 1)) * t1;
 FDenominator[2] := (1 - t5 + t4 * t2) * t1;
end;

{ TAutomatableHighcut }

procedure TAutomatableHighcutFilter.CalculateCoefficients;
var
  cn, t : Double;
begin
 t := 1 / (1 + FAlpha);
 cn := FastCosInBounds3Term(FW0);
 FNominator[0]   := sqr(FGainFactor) * (1 - cn) * 0.5 * t;
 FNominator[1]   := 2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
 CalcPolesZeros;
end;

{ TAutomatableLowcutFilter }

procedure TAutomatableLowcutFilter.CalculateCoefficients;
var
  cn, t : Double;
begin
 t := 1 / (1 + FAlpha);
 cn := FastCosInBounds3Term(FW0);
 FNominator[0]   := sqr(FGainFactor) * (1 + cn) * 0.5 * t;
 FNominator[1]   := -2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
 CalcPolesZeros;
end;

{ TAutomatableBandpassFilter }

procedure TAutomatableBandpassFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := 1 / (1 + FAlpha);
 FNominator[0]   := sqr(FGainFactor) * FAlpha * t;
 FNominator[2]   := -FNominator[0];
 FDenominator[1] := -2 * FastCosInBounds3Term(FW0) * t;
 FDenominator[2] := (1 - FAlpha) * t;
 FNominator[1]   := 0;
end;

{ TAutomatableNotchFilter }

procedure TAutomatableNotchFilter.CalculateCoefficients;
var
  t, a : Double;
begin
  t := 1 / (1 + FAlpha);
  a := sqr(FGainFactor);
  FDenominator[1] := -2 * FastCosInBounds3Term(FW0) * t;
  FDenominator[2] := (1 - FAlpha) * t;

  FNominator[0] := a * t;
  FNominator[1] := FDenominator[1] * a;
  FNominator[2] := FNominator[0];
end;

end.