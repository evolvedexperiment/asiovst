unit DAV_DspAnalogueFilterPrototypes;

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
  Classes, SysUtils, DAV_Common, DAV_Complex;

type
  TCustomAnalogueFilterPrototype = class(TObject)
  protected
    function GetOrder: Integer; virtual; abstract;
    procedure SetOrder(Value: Integer); virtual; abstract;
  public
    function Complex64(Frequency: Double): TComplexDouble; overload; virtual; abstract;
    function Complex32(Frequency: Single): TComplexSingle; overload; virtual;
    function Magnitude(Frequency: Double): Double; virtual;
    function Magnitude_dB(Frequency: Double): Double; virtual;
    function Phase(Frequency: Double): Double; virtual;

    property Order: Integer read GetOrder write SetOrder;
  end;
  TCustomAnalogueFilterPrototypeClass = class of TCustomAnalogueFilterPrototype;

  TCustomBiquadAnalogueFilterPrototype = class(TCustomAnalogueFilterPrototype)
  private
    FNominator   : array [0..2] of Double;
    FDenominator : array [0..2] of Double;
    FGain        : Double;
    FBandwidth   : Double;
    FFrequency   : Double;
    procedure SetBandwidth(const Value: Double);
    procedure SetFrequency(const Value: Double);
    procedure SetGain(const Value: Double);
    procedure CalculateAlpha;
  protected
    FAlpha      : Double;
    FGainFactor : Double;
    procedure CalculateCoefficients; virtual; abstract;
    function GetOrder: Integer; override;
    procedure SetOrder(Value: Integer); override;

    procedure BandwidthChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure GainChanged; virtual;
  public
    constructor Create; virtual;
    function Complex64(Frequency: Double): TComplexDouble; override;

    property Frequency: Double read FFrequency write SetFrequency;
    property Bandwidth: Double read FBandwidth write SetBandwidth;
    property Gain: Double read FGain write SetGain;
  end;
  TCustomBiquadAnalogueFilterPrototypeClass = class of TCustomBiquadAnalogueFilterPrototype;

  TAnalogueLowpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueBandpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueAllpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueNotchFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnaloguePeakFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueLowshelfFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighshelfFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueLowshelfAFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighshelfAFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueLowshelfBFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighshelfBFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TCustomAnalogueShapeFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  private
    FShape : Double;
    procedure SetShape(const Value: Double);
  protected
    procedure ShapeChanged; virtual;
  public
    constructor Create; override;

    property Shape: Double read FShape write SetShape;
  end;

  TAnalogueSimpleShapeFilterPrototype = class(TCustomAnalogueShapeFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueShapeFilterPrototype = class(TCustomAnalogueShapeFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

implementation

uses
  Math, DAV_Math;

resourcestring
  RCStrFixedOrder = 'The order of a biquad filter can not be changed';

{ TCustomAnalogueFilterPrototype }

function TCustomAnalogueFilterPrototype.Magnitude(Frequency: Double): Double;
var
  Cmplx : TComplexDouble;
begin
 Cmplx := Complex64(Frequency);
 Result := Sqrt(Sqr(Cmplx.Re) + Sqr(Cmplx.Im));
end;

function TCustomAnalogueFilterPrototype.Magnitude_dB(Frequency: Double): Double;
var
  Cmplx : TComplexDouble;
begin
 Cmplx := Complex64(Frequency);
 Result := 10 * Log10(Sqr(Cmplx.Re) + Sqr(Cmplx.Im));
end;

function TCustomAnalogueFilterPrototype.Phase(Frequency: Double): Double;
var
  Cmplx : TComplexDouble;
begin
 Cmplx := Complex64(Frequency);
 Result := ArcTan2(Cmplx.Im, Cmplx.Re);
end;

function TCustomAnalogueFilterPrototype.Complex32(Frequency: Single): TComplexSingle;
var
  Cmplx : TComplexDouble;
begin
 Cmplx := Complex64(Frequency);
 Result.Re := Cmplx.Re;
 Result.Im := Cmplx.Im;
end;


{ TCustomBiquadAnalogueFilterPrototype }

constructor TCustomBiquadAnalogueFilterPrototype.Create;
begin
 FGainFactor      := 1;
 FBandwidth := 1;
 FFrequency := 1000;
 CalculateAlpha;
 
 FNominator[0] := 1;
 FDenominator[0] := 1;
end;

function TCustomBiquadAnalogueFilterPrototype.Complex64(
  Frequency: Double): TComplexDouble;
var
  Omega   : Double;
  Divisor : Double;
begin
 Omega    := Frequency / FFrequency;

 Divisor  := 1 / (Sqr(FDenominator[0] - FDenominator[2] * Sqr(Omega)) +
   Sqr(FDenominator[1] * Omega));

 Result.Re := ((FNominator[0] - FNominator[2] * Sqr(Omega)) *
              (FDenominator[0] - FDenominator[2] * Sqr(Omega)) +
              (FNominator[1] * FDenominator[1] * Sqr(Omega))) * Divisor;

 Result.Im := Omega * (FNominator[1]   * (FDenominator[0] - FDenominator[2] * Sqr(Omega)) -
                     (FDenominator[1] * (FNominator[0] - FNominator[2] * Sqr(Omega)))) * Divisor;
end;

function TCustomBiquadAnalogueFilterPrototype.GetOrder: Integer;
begin
 Result := 2;
end;

procedure TCustomBiquadAnalogueFilterPrototype.CalculateAlpha;
begin
 FAlpha := (2 * Sinh(ln22 * FBandwidth));
end;

procedure TCustomBiquadAnalogueFilterPrototype.BandwidthChanged;
begin
 CalculateAlpha;
 CalculateCoefficients;
end;

procedure TCustomBiquadAnalogueFilterPrototype.FrequencyChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomBiquadAnalogueFilterPrototype.GainChanged;
begin
 FGainFactor := dB_to_Amp(0.5 * FGain);
 CalculateCoefficients;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetBandwidth(
  const Value: Double);
begin
 if FBandwidth <> Value then
  begin
   FBandwidth := Value;
   BandwidthChanged;
  end;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetFrequency(
  const Value: Double);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetGain(const Value: Double);
begin
 if FGain <> Value then
  begin
   FGain := Value;
   GainChanged;
  end;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetOrder(Value: Integer);
begin
 raise Exception.Create(RCStrFixedOrder); 
end;


{ TAnalogueLowpassFilterPrototype }

procedure TAnalogueLowpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueHighpassFilterPrototype }

procedure TAnalogueHighpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 0;
 FNominator[1] := 0;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueBandpassFilterPrototype }

procedure TAnalogueBandpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 0;
 FNominator[1] := Sqr(FGainFactor) * FAlpha;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueAllpassFilterPrototype }

procedure TAnalogueAllpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := -Sqr(FGainFactor) * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueNotchFilterPrototype }

procedure TAnalogueNotchFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 1;
 FNominator[1] := 0;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnaloguePeakFilterPrototype }

procedure TAnaloguePeakFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 1;
 FNominator[1] := FAlpha * FGainFactor;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha / FGainFactor;
 FDenominator[2] := 1;
end;


{ TAnalogueLowshelfFilterPrototype }

procedure TAnalogueLowshelfFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) * FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := Sqrt(FGainFactor) * FAlpha;
 FDenominator[2] := FGainFactor;
end;


{ TAnalogueHighshelfFilterPrototype }

procedure TAnalogueHighshelfFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := Sqrt(FGainFactor) * FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueLowshelfAFilterPrototype }

procedure TAnalogueLowshelfAFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := Sqr(FGainFactor) * FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := FGainFactor;
end;


{ TAnalogueHighshelfAFilterPrototype }

procedure TAnalogueHighshelfAFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := FGainFactor;
 FNominator[1] := Sqr(FGainFactor) * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueLowshelfBFilterPrototype }

procedure TAnalogueLowshelfBFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor * FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := FGainFactor * FAlpha;
 FDenominator[2] := FGainFactor;
end;


{ TAnalogueHighshelfBFilterPrototype }

procedure TAnalogueHighshelfBFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := FGainFactor * FAlpha;
 FDenominator[2] := 1;
end;


{ TCustomAnalogueShapeFilterPrototype }

constructor TCustomAnalogueShapeFilterPrototype.Create;
begin
 inherited;
 FShape := 0;
end;

procedure TCustomAnalogueShapeFilterPrototype.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   ShapeChanged;
  end;
end;

procedure TCustomAnalogueShapeFilterPrototype.ShapeChanged;
begin
 CalculateCoefficients;
end;


{ TAnalogueSimpleShapeFilterPrototype }

procedure TAnalogueSimpleShapeFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape)) * Power(FGainFactor, Abs(FShape));
 FNominator[1] := FGainFactor * Power(FGainFactor, 0.5 * Abs(FShape)) / FBandwidth;
 FNominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape)) * Power(FGainFactor, Abs(FShape));
 FDenominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape));
 FDenominator[1] := Power(FGainFactor, 0.5 * Abs(FShape)) / (Power(FGainFactor, 1 - Abs(FShape)) * FBandwidth);
 FDenominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape));
end;


{ TAnalogueShapeFilterPrototype }

procedure TAnalogueShapeFilterPrototype.CalculateCoefficients;
var
  InterShape : Double;
begin
(*

 //////////////
 // COMPLETE //
 //////////////

 // Lowpass
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;

 // OR Highpass if Gain < 1
 FNominator[0] := 0;
 FNominator[1] := 0;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;


 // Lowshelf Type B !!!

 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor / FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := FGainFactor / FAlpha;
 FDenominator[2] := FGainFactor;


 // Normal Lowshelf !!!

 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) / FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := Sqrt(FGainFactor) / FAlpha;
 FDenominator[2] := FGainFactor;


 // Lowshelf Type A !!!

 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := Sqr(FGainFactor) / FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := FGainFactor;


 // Peak

 FNominator[0] := 1;
 FNominator[1] := FGainFactor / FAlpha;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / (FGainFactor * FAlpha);
 FDenominator[2] := 1;


 // Highshelf Type A !!!

 FNominator[0] := FGainFactor;
 FNominator[1] := Sqr(FGainFactor) / FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;


 // Normal Highshelf !!!

 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) / FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := Sqrt(FGainFactor) / FAlpha;
 FDenominator[2] := 1;


 // Highshelf Type B !!!
 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor / FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := FGainFactor / FAlpha;
 FDenominator[2] := 1;


 // Lowpass
 FNominator[0] := 1;
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;

 // OR Highpass if Gain < 1
 FNominator[0] := 0;
 FNominator[1] := 0;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;

*)


 //////////////
 // COMPLETE //
 //////////////

 // pass filters
 if FShape < -3 then
  begin
   InterShape := 4 + FShape;
   if FGainFactor > 1 then
    begin
     FNominator[0] := Sqr(FGainFactor);
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := Power(FGainFactor + 1, InterShape) - 1;
     FDenominator[0] := 1;
     FDenominator[1] := Power(FGainFactor, InterShape) * FAlpha;
     FDenominator[2] := Power(FGainFactor, InterShape);
    end else
   if FGainFactor < 1 then
    begin
     FNominator[0] := (Power(FGainFactor + 1, InterShape) - 1) * FGainFactor;
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := Power(FGainFactor, InterShape);
     FDenominator[0] := 1;
     FDenominator[1] := Power(FGainFactor, InterShape) * FAlpha;
     FDenominator[2] := Power(FGainFactor, InterShape);
    end
   else
    begin
     FNominator[0] := 1;
     FNominator[1] := FAlpha;
     FNominator[2] := 1;
     FDenominator[0] := 1;
     FDenominator[1] := FAlpha;
     FDenominator[2] := 1;
    end;
  end else
 if FShape > 3 then
  begin
   InterShape := 4 - FShape;
   if FGainFactor < 1 then
    begin
     FNominator[0] := Power(FGainFactor, Intershape);
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := (Power(FGainFactor + 1, InterShape) - 1) * FGainFactor;
     FDenominator[0] := Power(FGainFactor, Intershape);
     FDenominator[1] := Power(FGainFactor, Intershape) * FAlpha;
     FDenominator[2] := 1;
    end else
   if FGainFactor > 1 then
    begin
     FNominator[0] := Power(FGainFactor + 1, InterShape) - 1;
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := Sqr(FGainFactor);
     FDenominator[0] := Power(FGainFactor, Intershape);
     FDenominator[1] := Power(FGainFactor, Intershape) * FAlpha;
     FDenominator[2] := 1;
    end
   else
    begin
     FNominator[0] := 1;
     FNominator[1] := FAlpha;
     FNominator[2] := 1;
     FDenominator[0] := 1;
     FDenominator[1] := FAlpha;
     FDenominator[2] := 1;
    end;
  end else
 // shape filters
 if FShape < -2 then
  begin
   InterShape := 3 + FShape;
   FNominator[0] := Sqr(FGainFactor);
   FNominator[1] := FGainFactor * Power(Sqrt(FGainFactor), InterShape) * FAlpha;
   FNominator[2] := FGainFactor;
   FDenominator[0] := 1;
   FDenominator[1] := FGainFactor * Power(Sqrt(FGainFactor), -InterShape) * FAlpha;
   FDenominator[2] := FGainFactor;
  end else
 if FShape < -1 then
  begin
   InterShape := 2 + FShape;
   FNominator[0] := Sqr(FGainFactor);
   FNominator[1] := Power(Sqrt(FGainFactor), InterShape) * FGainFactor * Sqrt(FGainFactor) * FAlpha;
   FNominator[2] := FGainFactor;
   FDenominator[0] := 1;
   FDenominator[1] := Power(Sqrt(FGainFactor), 1 - InterShape) * FAlpha;
   FDenominator[2] := FGainFactor;
  end else
 if FShape > 2 then
  begin
   InterShape := FShape - 2;
   FNominator[0] := FGainFactor;
   FNominator[1] := FGainFactor * Power(Sqrt(FGainFactor), 1 - InterShape) * FAlpha;
   FNominator[2] := Sqr(FGainFactor);
   FDenominator[0] := FGainFactor;
   FDenominator[1] := Sqrt(FGainFactor) * Power(Sqrt(FGainFactor), InterShape) * FAlpha;
   FDenominator[2] := 1;
  end else
 if FShape > 1 then
  begin
   InterShape := FShape - 1;
   FNominator[0] := FGainFactor;
   FNominator[1] := Sqr(FGainFactor) * Power(Sqrt(FGainFactor), -InterShape) * FAlpha;
   FNominator[2] := Sqr(FGainFactor);
   FDenominator[0] := FGainFactor;
   FDenominator[1] := Power(Sqrt(FGainFactor), InterShape) * FAlpha;
   FDenominator[2] := 1;
  end
 else
  begin
   FNominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape)) * Power(FGainFactor, Abs(FShape));
   FNominator[1] := Power(FGainFactor, Abs(FShape)) * FGainFactor * FAlpha;
   FNominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape)) * Power(FGainFactor, Abs(FShape));
   FDenominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape));
   FDenominator[1] := Power(FGainFactor, Abs(FShape) - 1) * FAlpha;
   FDenominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape));
  end;
end;

end.
