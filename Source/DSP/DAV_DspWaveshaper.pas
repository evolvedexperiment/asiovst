unit DAV_DspWaveshaper;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2004-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TChebyshevWaveshaper = class(TDspPersistent, IDspProcessor64)
  private
    function GetGain(Harmonic: Integer): Double;
    function GetInverted(Harmonic: Integer): Boolean;
    function GetLevel(Harmonic: Integer): Double;
    function GetOrder: Integer;
    procedure SetGain(Harmonic: Integer; const Value: Double);
    procedure SetLevel(Harmonic: Integer; const Value: Double);
    procedure SetOrder(Value: Integer);
    procedure SetInverted(Harmonic: Integer; const Value: Boolean);
    function GetCoefficients(Index: Integer): Double;
  protected
    FChebyshevCoeffs : TDAVDoubleDynArray;
    FGains           : TDAVDoubleDynArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure RecalculateHarmonics; virtual;
    procedure OrderChanged; virtual;
  public
    constructor Create;
    
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

    property Gain[Harmonic: Integer]: Double read GetGain write SetGain;
    property Level[Harmonic: Integer]: Double read GetLevel write SetLevel;
    property Inverted[Harmonic: Integer]: Boolean read GetInverted write SetInverted;
    property Coefficients[Harmonic: Integer]: Double read GetCoefficients;
  published
    property Order: Integer read GetOrder write SetOrder;
  end;

  TChebyshevWaveshaperSquare = class(TChebyshevWaveshaper)
  protected
    procedure OrderChanged; override;
  end;

  TChebyshevWaveshaperSquarelShape = class(TChebyshevWaveshaper)
  private
    FShape: Double;
    procedure SetShape(const Value: Double);
  published
  protected
    procedure OrderChanged; override;
  published
    property Shape: Double read FShape write SetShape;
  end;

function Waveshaper1(Input, Parameter: Single): Single; overload;
function Waveshaper1(Input, Parameter: Double): Double; overload;
function Waveshaper2(Input, Parameter: Single): Single; overload;
function Waveshaper2(Input, Parameter: Double): Double; overload;
function Waveshaper3(Input, Parameter: Single): Single; overload;
function Waveshaper3(Input, Parameter: Double): Double; overload;
function Waveshaper4(Input, Parameter: Single): Single; overload;
function Waveshaper4(Input, Parameter: Double): Double; overload;
function Waveshaper5(Input, Parameter: Single): Single; overload;
function Waveshaper5(Input, Parameter: Double): Double; overload;
function Waveshaper6(Input: Single): Single; overload;
function Waveshaper6(Input: Double): Double; overload;
function Waveshaper7(Input, Parameter: Single): Single; overload;
function Waveshaper7(Input, Parameter: Double): Double; overload;
function Waveshaper8(Input, Parameter: Single): Single; overload;
function Waveshaper8(Input, Parameter: Double): Double; overload;
function Saturate(Input, Limit: Single): Single; overload;
function Saturate(Input, Limit: Double): Double; overload;
function Saturate2(Input, Limit: Single): Single; overload;
function Saturate2(Input, Limit: Double): Double; overload;
function SoftSat(Input, Parameter: Single): Single; overload;
function SoftSat(Input, Parameter: Double): Double; overload;

implementation

uses
  SysUtils, Math, DAV_Common, DAV_Math;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

function Waveshaper1(Input, Parameter :Single): Single;
begin
 if abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * DAV_Math.Tanh(( Input - Parameter) / (1 - Parameter))
     else Result := -(Parameter + (1 - Parameter) * DAV_Math.Tanh((-Input - Parameter) / (1 - Parameter)));
   end;
end;

function Waveshaper1(Input, Parameter :Double): Double;
begin
 if abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * DAV_Math.Tanh(( Input - Parameter) / (1 - Parameter))
     else Result := -(Parameter + (1 - Parameter) * DAV_Math.Tanh((-Input - Parameter) / (1 - Parameter)));
   end;
end;

function Waveshaper2(Input, Parameter: Single): Single;
begin
 if abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * Sigmoid( (Input - Parameter) / ((1 - Parameter) * 1.5))
     else Result := -(Parameter + (1 - Parameter) * Sigmoid((-Input - Parameter) / ((1 - Parameter) * 1.5)));
   end;
end;

function Waveshaper2(Input, Parameter: Double): Double;
begin
 if abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * Sigmoid( (Input - Parameter) / ((1 - Parameter) * 1.5))
     else Result := -(Parameter + (1 - Parameter) * Sigmoid((-Input - Parameter) / ((1 - Parameter) * 1.5)));
   end;
end;

function Waveshaper3(Input, Parameter: Single): Single;
begin
 Result := Input * (Abs(Input) + Parameter) / (Input * Input + (Parameter - 1) * Abs(Input) + 1);
end;

function Waveshaper3(Input, Parameter: Double): Double;
begin
 Result := Input * (Abs(Input) + Parameter) / (Input * Input + (Parameter - 1) * Abs(Input) + 1);
end;

function Waveshaper4(Input, Parameter: Single): Single;
begin
 Result := sign(Input) * power(ArcTan(Power(Abs(Input), Parameter)), (1 / Parameter));
end;

function Waveshaper4(Input, Parameter: Double): Double;
begin
 Result := sign(Input) * power(arctan(power(Abs(Input), Parameter)), (1 / Parameter));
end;

function Waveshaper5(Input, Parameter: Single): Single;
begin
 Parameter := 2 * Parameter / (1 - Parameter);
 Result := (1 + Parameter) * Input / (1 + Parameter * Abs(Input));
end;

function Waveshaper5(Input, Parameter: Double): Double;
begin
 Parameter := 2 * Parameter / (1 - Parameter);
 Result := (1 + Parameter) * Input / (1 + Parameter * Abs(Input));
end;

function Waveshaper6(Input: Single): Single;
var
  Parameter, b : Single;
begin
 Input := Input * 0.686306;
 Parameter := 1 + Exp(sqrt(Abs(Input)) * -0.75);
 b := Exp(Input);
 Result := (b - Exp(-Input * Parameter)) * b / (b * b + 1);
end;

function Waveshaper6(Input: Double): Double;
var
  Parameter, b : Double;
begin
 Input := Input * 0.686306;
 Parameter := 1 + Exp(sqrt(Abs(Input)) * -0.75);
 b := Exp(Input);
 Result := (b - Exp(-Input * Parameter)) * b / (b * b + 1);
end;

function Waveshaper7(Input, Parameter: Single): Single;
begin
 Result := sign(Input) * Exp(ln(Abs(Input)) * Parameter);
end;

function Waveshaper7(Input, Parameter: Double): Double;
begin
 Result := sign(Input) * Exp(ln(Abs(Input)) * Parameter);
end;

function Waveshaper8(Input, Parameter: Single): Single;
begin
 Result := sign(Input) * Exp(ln(Parameter) * Abs(Input));
end;

function Waveshaper8(Input, Parameter: Double): Double;
begin
 Result := sign(Input) * Exp(ln(Parameter) * Abs(Input));
end;

function Saturate(Input, Limit: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := 0.5 * (Abs(Input + Limit) - Abs(Input - Limit));
{$ELSE}
const
  CGrdDiv : Double = 0.5;
asm
 FLD     Input.Single
 FADD    Limit
 FABS
 FLD     Input.Single
 FSUB    Limit
 FABS
 FSUBP
 FMUL    CGrdDiv;
// Result := CGrdDiv * (Abs(Input + Limit) - Abs(Input - Limit));
{$ENDIF}
end;

function Saturate(Input, Limit: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result := 0.5 * (Abs(Input + Limit) - Abs(Input - Limit));
{$ELSE}
const
  CGrdDiv : Double = 0.5;
asm
 FLD     Input.Double
 FADD    Limit.Double
 FABS
 FLD     Input.Double
 FSUB    Limit.Double
 FABS
 FSUBP
 FMUL    CGrdDiv;
{$ENDIF}
end;

function Saturate2(Input, Limit: Single): Single;
begin
 if Input > Limit
  then Result := Limit
  else
   if Input < -Limit
    then Result := -Limit
    else Result := Input;
end;

function Saturate2(Input, Limit: Double): Double;
begin
 if Input > Limit
  then Result := Limit
  else
   if Input < -Limit
    then Result := -Limit
    else Result := Input;
end; 

function SoftSat(Input, Parameter: Single): Single;
var
  b, c : Single;
begin
 b := Abs(Input);
 if b < Parameter then Result := Input else
 if b > 1 then Result := sign(Input) * (Parameter + 1) * 0.5 else
  begin
   c := ((Input - Parameter) / (1 - Parameter));
   Result := Parameter + (Input - Parameter) / (1 + c * c);
  end;
end;

function SoftSat(Input, Parameter: Double): Double;
var
  b, c : Double;
begin
 b := Abs(Input);
 if b < Parameter then Result := Input else
 if b > 1 then Result := sign(Input) * (Parameter + 1) * 0.5 else
  begin
   c := ((Input - Parameter) / (1 - Parameter));
   Result := Parameter + (Input - Parameter) / (1 + c * c);
  end;
end;


{ TChebyshevWaveshaper }

constructor TChebyshevWaveshaper.Create;
begin
 Order := 1;
 Gain[0] := 1;
end;

procedure TChebyshevWaveshaper.AssignTo(Dest: TPersistent);
begin
 if Dest is TChebyshevWaveshaper then
  with TChebyshevWaveshaper(Dest) do
   begin
    FChebyshevCoeffs := Self.FChebyshevCoeffs;
    FGains           := Self.FGains;
   end
 else inherited;
end;

function TChebyshevWaveshaper.GetCoefficients(Index: Integer): Double;
begin
 if (Index < 0) or (Index >= Length(FChebyshevCoeffs))
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else Result := FChebyshevCoeffs[Index];
end;

function TChebyshevWaveshaper.GetGain(Harmonic: Integer): Double;
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Harmonic])
  else Result := FGains[Harmonic];
end;

function TChebyshevWaveshaper.GetInverted(Harmonic: Integer): Boolean;
begin
 Result := Gain[Harmonic] < 0;
end;

function TChebyshevWaveshaper.GetLevel(Harmonic: Integer): Double;
begin
 Result := Amp_to_dB(abs(FGains[Harmonic]));
end;

function TChebyshevWaveshaper.GetOrder: Integer;
begin
 Result := Length(FGains);
end;

procedure TChebyshevWaveshaper.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
  Term   : Integer;
  Temp   : Double;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   Temp := FChebyshevCoeffs[Order];
   for Term := Order - 1 downto 0
    do Temp := Temp * Data[Sample] + FChebyshevCoeffs[Term];
   Data[Sample] := Temp;
  end;
end;

function TChebyshevWaveshaper.ProcessSample64(Input: Double): Double;
var
  Term : Integer;
begin
 Result := FChebyshevCoeffs[Order];
 for Term := Order - 1 downto 0
  do Result := Result * Input + FChebyshevCoeffs[Term];
end;

function ChebyPolynome(Order, Power: Integer): Integer;
begin
 if (Power < 0) or (Order < Power) then Result := 0 else
  case Order of
    0 : if (Power = 0) then Result := 1 else Result := 0;
    1 : if (Power = 1) then Result := 1 else Result := 0;
    2 : case Power of
         0 : Result := -1;
         2 : Result :=  2;
         else Result := 0;
        end;
    3 : case Power of
         1 : Result := -3;
         3 : Result :=  4;
         else Result := 0;
        end;
    4 : case Power of
         0 : Result :=  1;
         2 : Result := -8;
         4 : Result :=  8;
         else Result := 0;
        end;
    5 : case Power of
         1 : Result :=   5;
         3 : Result := -20;
         5 : Result :=  16;
         else Result := 0;
        end;
    6 : case Power of
         0 : Result :=  -1;
         2 : Result :=  18;
         4 : Result := -48;
         6 : Result :=  32;
         else Result := 0;
        end;
    7 : case Power of
         1 : Result :=  -7;
         3 : Result :=  56;
         5 : Result := -112;
         7 : Result :=  64;
         else Result := 0;
        end;
    8 : case Power of
         0 : Result :=    1;
         2 : Result :=  -32;
         4 : Result :=  160;
         6 : Result := -256;
         8 : Result :=  128;
         else Result := 0;
        end;
   else Result := 2 * ChebyPolynome(Order - 1, Power - 1) - ChebyPolynome(Order - 2, Power);
  end;
end;

procedure TChebyshevWaveshaper.OrderChanged;
begin
 RecalculateHarmonics;
 Changed;
end;

procedure TChebyshevWaveshaper.RecalculateHarmonics;
var
  Input, y  : Integer;
begin
 for y := 0 to Order do
  begin
   FChebyshevCoeffs[y] := FGains[0] * ChebyPolynome(1, y);
   for Input := 1 to Order - 1 do
    if FGains[Input] <> 0
     then FChebyshevCoeffs[y] := FChebyshevCoeffs[y] + FGains[Input] * ChebyPolynome(1 + Input, y);
  end;
end;

procedure TChebyshevWaveshaper.SetGain(Harmonic: Integer; const Value: Double);
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Harmonic])
  else
   begin
    FGains[Harmonic] := Value;
    RecalculateHarmonics;
   end;
end;

procedure TChebyshevWaveshaper.SetInverted(Harmonic: Integer;
  const Value: Boolean);
begin
 if Value
  then Gain[Harmonic] := -abs(Gain[Harmonic])
  else Gain[Harmonic] :=  abs(Gain[Harmonic]);
end;

procedure TChebyshevWaveshaper.SetLevel(Harmonic: Integer; const Value: Double);
begin
 if FGains[Harmonic] < 0
  then Gain[Harmonic] := -dB_to_Amp(Value)
  else Gain[Harmonic] :=  dB_to_Amp(Value);
end;

procedure TChebyshevWaveshaper.SetOrder(Value: Integer);
begin
 if Value < 1 then Value := 1 else
 if Value > 24 then Value := 24;
 if Value <> Order then
  begin
   SetLength(FGains, Value);
   SetLength(FChebyshevCoeffs, Value + 1);
   FGains[0] := 1;
   OrderChanged;
   RecalculateHarmonics;
  end;
end;

{ TChebyshevWaveshaperSquare }

procedure TChebyshevWaveshaperSquare.OrderChanged;
var
  i : Integer;
begin
 for i := 0 to Order - 1 do
  case i mod 4 of
   0: FGains[i] := -1 / (i + 1);
   2: FGains[i] :=  1 / (i + 1);
   else FGains[i] := 0;
  end;
 inherited;
end;


{ TChebyshevWaveshaperSquarelShape }

procedure TChebyshevWaveshaperSquarelShape.OrderChanged;
var
  i : Integer;
begin
 for i := 0 to Order - 1 do
  case i mod 4 of
   0: FGains[i] := -1 / Power(i + 1, FShape);
   2: FGains[i] :=  1 / Power(i + 1, FShape);
   else FGains[i] := 0;
  end;
 inherited;
end;

procedure TChebyshevWaveshaperSquarelShape.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   OrderChanged;
  end;
end;

initialization
  RegisterDspProcessor64(TChebyshevWaveshaper);

end.
