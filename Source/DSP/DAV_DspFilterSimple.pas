unit DAV_DspFilterSimple;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Classes, DAV_Types, DAV_Complex, DAV_DspFilter;

type
  TCustomFirstOrderFilter = class(TCustomIIRFilter)
  protected
    FState      : Double;
    FCoeff      : Double;
    FFilterGain : Double;
    FStates     : TDAVDoubleDynArray;
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    procedure Reset; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure PushStates; override;
    procedure PopStates; override;
  end;

  TFirstOrderGainFilter = class(TCustomFirstOrderFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  end;

  TFirstOrderAllpassFilter = class(TCustomFilter, IDspProcessor32,
    IDspProcessor64)
  private
    FFractionalDelay: Double;
    procedure SetFractionalDelay(const Value: Double);
    procedure FractionalDelayChanged;
  protected
    FCoefficient : Double;
    FState       : Double;
    FStates      : TDAVDoubleDynArray;
    procedure AssignTo(Dest: TPersistent); override;
  published
  public
    constructor Create; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Reset; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure PushStates; override;
    procedure PopStates; override;

    property FractionalDelay: Double read FFractionalDelay write SetFractionalDelay;
  end;

  TFirstOrderLowShelfFilter = class(TCustomFirstOrderFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    FAddCoeff : Double;
    procedure CalculateCoefficients; override;
  public
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  end;

  TFirstOrderHighShelfFilter = class(TCustomFirstOrderFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    FAddCoeff : Double;
    procedure CalculateCoefficients; override;
  public
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  end;

  TFirstOrderHighcutFilter = class(TCustomFirstOrderFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;

    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  end;

  TFirstOrderLowcutFilter = class(TCustomFirstOrderFilter, IDspProcessor32,
    IDspProcessor64)
  protected
    procedure CalculateCoefficients; override;
  public
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;

    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  end;

  TFirstOrderLowpassFilter = class(TFirstOrderHighcutFilter);
  TFirstOrderHighpassFilter = class(TFirstOrderLowcutFilter);

implementation

uses
  SysUtils, DAV_Math;

{$IFDEF HandleDenormals}
var
  DenormRandom : Single;
const
  CDenorm32    : Single = 1E-24;
  CDenorm64    : Double = 1E-34;
{$ENDIF}

{ TCustomFirstOrderFilter }

constructor TCustomFirstOrderFilter.Create;
begin
 inherited;
 FState := 0;
end;

procedure TCustomFirstOrderFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomFirstOrderFilter then
  with TCustomFirstOrderFilter(Dest) do
   begin
    inherited;
    FState  := Self.FState;
    FStates := Self.FStates;
   end
  else inherited;
end;

function TCustomFirstOrderFilter.GetOrder: Cardinal;
begin
 Result := 1;
end;

function TCustomFirstOrderFilter.MagnitudeLog10(
  const Frequency: Double): Double;
begin
 Result := FGain_dB;
end;

function TCustomFirstOrderFilter.MagnitudeSquared(
  const Frequency: Double): Double;
begin
 Result := FGainFactor;
end;

procedure TCustomFirstOrderFilter.PopStates;
begin
 FState := FStates[Length(FStates) - 1];
 SetLength(FStates, Length(FStates) - 1);
end;

procedure TCustomFirstOrderFilter.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample32(Data[SampleIndex]);
end;

procedure TCustomFirstOrderFilter.ProcessBlock64(
  const Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample64(Data[SampleIndex]);
end;

procedure TCustomFirstOrderFilter.PushStates;
begin
 SetLength(FStates, Length(FStates) + 1);
 FStates[Length(FStates) - 1] := FState;
end;

function TCustomFirstOrderFilter.Real(const Frequency: Double): Double;
var
  Dummy : Double;
begin
 Complex(Frequency, Result, Dummy);
end;

function TCustomFirstOrderFilter.Imaginary(const Frequency: Double): Double;
var
  Dummy : Double;
begin
 Complex(Frequency, Dummy, Result);
end;

procedure TCustomFirstOrderFilter.Reset;
begin
 FFrequency := 0;
end;

procedure TCustomFirstOrderFilter.ResetStates;
begin
 FState := 0;
end;

procedure TCustomFirstOrderFilter.ResetStatesInt64;
begin
 FState := 0;
end;

procedure TCustomFirstOrderFilter.SetOrder(const Value: Cardinal);
begin
 raise Exception.Create('Order is fixed!');
end;


{ TFirstOrderGainFilter }

procedure TFirstOrderGainFilter.CalculateCoefficients;
begin
 inherited;
end;


{ TFirstOrderAllpassFilter }

constructor TFirstOrderAllpassFilter.Create;
begin
 inherited;
 FState := 0;
end;

procedure TFirstOrderAllpassFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TFirstOrderAllpassFilter then
  with TFirstOrderAllpassFilter(Dest) do
   begin
    inherited;
    FState  := Self.FState;
    FStates := Self.FStates;
   end
  else inherited;
end;

(*
procedure TFirstOrderAllpassFilter.FrequencyChanged;
begin
 Assert(FFrequency >= -0.5);
 Assert(FFrequency <= 1);
// inherited;
end;
*)

function TFirstOrderAllpassFilter.MagnitudeLog10(
  const Frequency: Double): Double;
begin
 Result := 0;
end;

function TFirstOrderAllpassFilter.MagnitudeSquared(
  const Frequency: Double): Double;
begin
 Result := 1;
end;

procedure TFirstOrderAllpassFilter.PopStates;
begin
 FState := FStates[Length(FStates) - 1];
 SetLength(FStates, Length(FStates) - 1);
end;

function TFirstOrderAllpassFilter.ProcessSample32(Input: Single): Single;
begin
 Result := FState + FCoefficient * Input;
 FState := Input - FCoefficient * Result;
end;

function TFirstOrderAllpassFilter.ProcessSample64(Input: Double): Double;
begin
 Result := FState + FCoefficient * Input;
 FState := Input - FCoefficient * Result;
end;

procedure TFirstOrderAllpassFilter.PushStates;
begin
 SetLength(FStates, Length(FStates) + 1);
 FStates[Length(FStates) - 1] := FState;
end;

procedure TFirstOrderAllpassFilter.Reset;
begin
 FCoefficient := 0;
end;

procedure TFirstOrderAllpassFilter.ResetStates;
begin
 FState := 0;
end;

procedure TFirstOrderAllpassFilter.ResetStatesInt64;
begin
 FState := 0;
end;


procedure TFirstOrderAllpassFilter.SetFractionalDelay(const Value: Double);
begin
 if FFractionalDelay <> Value then
  begin
   FFractionalDelay := Value;
   FractionalDelayChanged;
  end;
end;

procedure TFirstOrderAllpassFilter.FractionalDelayChanged;
begin
 FCoefficient := 0.5 * FFractionalDelay;
end;


{ TFirstOrderLowShelfFilter }

procedure TFirstOrderLowShelfFilter.CalculateCoefficients;
var
  K : Double;
begin
 K := FExpW0.Im / (1 + FExpW0.Re);
 FFilterGain := (K * FGainFactor + 1) / (K / FGainFactor + 1);
 FCoeff := (FGainFactor * K - 1) / (FGainFactor * K + 1);
 FAddCoeff := (K - FGainFactor) / (K + FGainFactor);
end;

function TFirstOrderLowShelfFilter.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Input := FFilterGain * Input;
 Result := Input + FState;
 FState := Input * FCoeff - Result * FAddCoeff;
{$ELSE}
asm
 FLD Input.Single
 {$IFDEF HandleDenormals}
 FADD CDenorm32
 {$ENDIF}
 FMUL  [EAX.FFilterGain].Double
 JZ @End
  FLD   ST(0)
  FADD  [EAX.FState].Double
  FLD   ST(0)
  FMUL  [EAX.FAddCoeff].Double
  FXCH  ST(2)
  FMUL  [EAX.FCoeff].Double
  fsubrp ST(2), ST(0)
  FXCH
  FSTP  [EAX.FState].Double
 @End:
 {$ENDIF}
end;

function TFirstOrderLowShelfFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Input := FFilterGain * Input;
 Result := Input + FState;
 FState := Input * FCoeff - Result * FAddCoeff;
{$ELSE}
asm
 FLD Input.Double
 {$IFDEF HandleDenormals}
 FADD CDenorm64
 {$ENDIF}
 FMUL  [EAX.FFilterGain].Double
 JZ @End
  FLD   ST(0)
  FADD  [EAX.FState].Double
  FLD   ST(0)
  FMUL  [EAX.FAddCoeff].Double
  FXCH  ST(2)
  FMUL  [EAX.FCoeff].Double
  fsubrp ST(2), ST(0)
  FXCH
  FSTP  [EAX.FState].Double
 @End:
 {$ENDIF}
end;


{ TFirstOrderHighShelfFilter }

procedure TFirstOrderHighShelfFilter.CalculateCoefficients;
var
  K : Double;
begin
 K  := FExpW0.Im / (1 + FExpW0.Re);
 FFilterGain := ((K / FGainFactor + 1) / (K * FGainFactor + 1)) * FGainFactorSquared;
 FCoeff := (K - FGainFactor) / (K + FGainFactor);
 FAddCoeff := (K * FGainFactor - 1) / (K * FGainFactor + 1);
end;

function TFirstOrderHighShelfFilter.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Input := FFilterGain * Input;
 Result := Input + FState;
 FState := Input * FCoeff - Result * FAddCoeff;
{$ELSE}
asm
 FLD Input.Single
 {$IFDEF HandleDenormals}
 FADD CDenorm32
 {$ENDIF}
 FMUL  [EAX.FFilterGain].Double
 JZ @End
  FLD   ST(0)
  FADD  [EAX.FState].Double
  FLD   ST(0)
  FMUL  [EAX.FAddCoeff].Double
  FXCH  ST(2)
  FMUL  [EAX.FCoeff].Double
  fsubrp ST(2), ST(0)
  FXCH
  FSTP  [EAX.FState].Double
 @End:
 {$ENDIF}
end;

function TFirstOrderHighShelfFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Input := FFilterGain * Input;
 Result := Input + FState;
 FState := Input * FCoeff - Result * FAddCoeff;
{$ELSE}
asm
 FLD Input.Double
 {$IFDEF HandleDenormals}
 FADD CDenorm64
 {$ENDIF}
 FMUL  [EAX.FFilterGain].Double
 JZ @End
  FLD   ST(0)
  FADD  [EAX.FState].Double
  FLD   ST(0)
  FMUL  [EAX.FAddCoeff].Double
  FXCH  ST(2)
  FMUL  [EAX.FCoeff].Double
  fsubrp ST(2), ST(0)
  FXCH
  FSTP  [EAX.FState].Double
 @End:
 {$ENDIF}
end;


{ TFirstOrderHighcut }

procedure TFirstOrderHighcutFilter.CalculateCoefficients;
var
  K, t : Double;
begin
 FFilterGain := FGainFactorSquared;
 K := FExpW0.Im / (1 + FExpW0.Re);

 t := 1 / (1 + K);
 FFilterGain := FFilterGain * t * K;
 FCoeff := (1 - K) * t;
end;

function TFirstOrderHighcutFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * Cos(2 * Frequency * Pi * SampleRateReciprocal);
 Result := Sqr(FFilterGain) * (cw + 2) / (1 + Sqr(FCoeff) - cw * FCoeff);
 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(Result);
end;

function TFirstOrderHighcutFilter.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Input  := FFilterGain * Input;
 Result := Input + FState;
 FState := Input + FCoeff * Result;
{$ELSE}
asm
 FLD     Input.Single
 {$IFDEF HandleDenormals}
 FADD    CDenorm32
 {$ENDIF}
 FMUL    [EAX.FFilterGain].Double
 FLD     ST(0)
 FADD    [EAX.FState].Double
 FLD     ST(0)
 FMUL    [EAX.FCoeff].Double
 FADDP   ST(2), ST(0)
 FXCH
 FSTP    [EAX.FState].Double
 {$ENDIF}
end;

function TFirstOrderHighcutFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Input  := FFilterGain * Input;
 Result := Input + FState;
 FState := Input + FCoeff * Result;
{$ELSE}
asm
 FLD     Input.Double
 {$IFDEF HandleDenormals}
 FADD    CDenorm64
 {$ENDIF}
 FMUL    [EAX.FFilterGain].Double
 FLD     ST(0)
 FADD    [EAX.FState].Double
 FLD     ST(0)
 FMUL    [EAX.FCoeff].Double
 FADDP   ST(2), ST(0)
 FXCH
 FSTP    [EAX.FState].Double
 {$ENDIF}
end;

procedure TFirstOrderHighcutFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
var
  Cmplx   : TComplexDouble;
  A, B, R : TComplexDouble;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 A.Re :=  Cmplx.Re + 1;
 A.Im := -Cmplx.Im;
 B.Re := -Cmplx.Re * FCoeff + 1;
 B.Im :=  Cmplx.Im * FCoeff;
 R := ComplexMultiply64(R, ComplexDivide64(A, B));

 Real := R.Re;
 Imaginary := R.Im;
end;


{ TFirstOrderLowcutFilter }

procedure TFirstOrderLowcutFilter.CalculateCoefficients;
var
  K, t  : Double;
begin
 FFilterGain := Sqr(FGainFactor);
 K := FExpW0.Im / (1 + FExpW0.Re);

 t := 1 / (K + 1);
 FFilterGain := FFilterGain * t;
 FCoeff := (1 - K) * t;
end;

function TFirstOrderLowcutFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw     := 2 * Cos(2 * Frequency * pi * FSRR);
 Result := Sqr(FFilterGain) * (cw - 2) / (1 + Sqr(FCoeff) - cw * FCoeff);
 Result := {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF} Abs(Result);
end;

procedure TFirstOrderLowcutFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  Cmplx : TComplexDouble;
  A, R  : TComplexDouble;
begin
 GetSinCos(2 * Pi * Frequency * FSRR, Cmplx.Im, Cmplx.Re);

 R.Re := FFilterGain;
 R.Im := 0;

 A.Re :=  Cmplx.Re - 1;
 A.Im := -Cmplx.Im;
 R := ComplexMultiply64(R, A);

 A.Re := -Cmplx.Re * FCoeff + 1;
 A.Im :=  Cmplx.Im * FCoeff;
 R := ComplexDivide64(R, A);

 Real := R.Re;
 Imaginary := R.Im;
end;


function TFirstOrderLowcutFilter.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Input := FFilterGain * Input;
 Result :=  Input + FState;
 FState := -Input + FCoeff * Result;
{$ELSE}
asm
 FLD     Input.Single

 // eventually add denormal
 {$IFDEF HandleDenormals}
 MOV     EDX, DenormRandom
 IMUL    EDX, DenormRandom, $08088405
 INC     EDX
 SHR     EDX, 23
 OR      EDX, $20000000
 MOV     DenormRandom, EDX
 FADD    DenormRandom
 {$ENDIF}

 FMUL    [EAX.FFilterGain].Double
 FLD     ST(0)
 FADD    [EAX.FState].Double
 FLD     ST(0)
 FMUL    [EAX.FCoeff].Double
 FSUBRP  ST(2), ST(0)
 FXCH
 FSTP    [EAX.FState].Double
 {$ENDIF}
end;

function TFirstOrderLowcutFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Input := FFilterGain * Input;
 Result :=  Input + FState;
 FState := -Input + FCoeff * Result;
{$ELSE}
asm
 FLD     Input.Double

 // eventually add denormal
 {$IFDEF HandleDenormals}
 MOV     EDX, DenormRandom
 IMUL    EDX, DenormRandom, $08088405
 INC     EDX
 SHR     EDX, 23
 OR      EDX, $20000000
 MOV     DenormRandom, EDX
 FADD    DenormRandom
 {$ENDIF}

 FMUL    [EAX.FFilterGain].Double
 FLD     ST(0)
 FADD    [EAX.FState].Double
 FLD     ST(0)
 FMUL    [EAX.FCoeff].Double
 FSUBRP  ST(2), ST(0)
 FXCH
 FSTP    [EAX.FState].Double
 {$ENDIF}
end;

initialization
  RegisterDspProcessors32([TFirstOrderGainFilter, TFirstOrderAllpassFilter,
    TFirstOrderLowShelfFilter, TFirstOrderHighShelfFilter,
    TFirstOrderHighcutFilter, TFirstOrderLowcutFilter]);
  RegisterDspProcessors64([TFirstOrderGainFilter, TFirstOrderAllpassFilter,
    TFirstOrderLowShelfFilter, TFirstOrderHighcutFilter,
    TFirstOrderLowcutFilter]);

end.
