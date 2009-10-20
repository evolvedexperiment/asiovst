unit DAV_DspParametricEQ;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Classes, DAV_DspFilter, DAV_DspFilterBasics;

type
  TCustomParametricEQ = class(TCustomFilter)
  private
    FBandCount : Integer;
    procedure SetSampleRate(const Value: Double);
    procedure SetBands(const Value: Integer);
    function GetFilter(Index: Integer): TBiquadIIRFilter;
    procedure BandCountChanged;
  protected
    FSampleRate  : Double;
    FFilterArray : array of TBiquadIIRFilter;
    procedure SampleRateChanged; virtual;
    procedure OnChangeEventHandler(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ProcessSample64(Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Double); overload; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure Reset; override;

    property Bands: Integer read FBandCount write SetBands;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Filter[Index: Integer]: TBiquadIIRFilter read GetFilter;
  end;

implementation

uses
  SysUtils, Math, DAV_Complex;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TCustomParametricEQ }

constructor TCustomParametricEQ.Create;
begin
 FSampleRate := 44100;
 FBandCount := 3;
 BandCountChanged;
end;

destructor TCustomParametricEQ.Destroy;
var
  Band : Integer;
begin
 for Band := 0 to Length(FFilterArray) - 1
  do FreeAndNil(FFilterArray[Band]);

 inherited;
end;

procedure TCustomParametricEQ.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  i   : Integer;
  Tmp : TComplexDouble;
begin
 if Length(FFilterArray) = 0 then exit;
 assert(assigned(FFilterArray[0]));
 FFilterArray[0].Complex(Frequency, Real, Imaginary);
 for i := 1 to Length(FFilterArray) - 1 do
  begin
   assert(assigned(FFilterArray[i]));
   FFilterArray[i].Complex(Frequency, Tmp.Re, Tmp.Im);
   ComplexMultiply(Real, Imaginary, Tmp.Re, Tmp.Im);
  end;
end;

function TCustomParametricEQ.GetFilter(Index: Integer): TBiquadIIRFilter;
begin
 if (Index >= 0) and (Index < Length(FFilterArray))
  then Result := FFilterArray[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomParametricEQ.Imaginary(const Frequency: Double): Double;
var
  Real : Double;
begin
 Complex(Frequency, Real, Result);
end;

function TCustomParametricEQ.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := 10 * Log10(MagnitudeSquared(Frequency));
end;

function TCustomParametricEQ.MagnitudeSquared(const Frequency: Double): Double;
var
  Band : Integer;
begin
 if Length(FFilterArray) = 0 then
  begin
   Result := 1;
   Exit;
  end;
 assert(assigned(FFilterArray[0]));
 Result := FFilterArray[0].MagnitudeSquared(Frequency);
 for Band := 1 to Length(FFilterArray) - 1 do
  begin
   assert(assigned(FFilterArray[Band]));
   Result := Result * FFilterArray[Band].MagnitudeSquared(Frequency);
  end;
end;

procedure TCustomParametricEQ.OnChangeEventHandler(Sender: TObject);
begin
 Changed;
end;

procedure TCustomParametricEQ.SetBands(const Value: Integer);
begin
 if FBandCount <> Value then
  begin
   FBandCount := Value;
   BandCountChanged;
  end;
end;

procedure TCustomParametricEQ.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomParametricEQ.SampleRateChanged;
var
  Band : Integer;
begin
 for Band := 0 to Length(FFilterArray) - 1
  do FFilterArray[Band].SampleRate := SampleRate
end;

procedure TCustomParametricEQ.BandCountChanged;
var
  Band : Integer;
begin
 for Band := FBandCount to Length(FFilterArray) - 1
  do FreeAndNil(FFilterArray[Band]);

 SetLength(FFilterArray, FBandCount);

 for Band := 0 to Length(FFilterArray) - 1 do
  if not assigned(FFilterArray[Band]) then
   begin
    FFilterArray[Band] := TBasicPeakFilter.Create;
    FFilterArray[Band].OnChange := OnChangeEventHandler;
   end;
end;

function TCustomParametricEQ.ProcessSample64(Input: Double): Double;
var
  Band : Integer;
begin
 Result := Input;
 for Band := 0 to Length(FFilterArray) - 1
  do Result := FFilterArray[Band].ProcessSample64(Result);
end;

function TCustomParametricEQ.Real(const Frequency: Double): Double;
var
  Imag : Double;
begin
 Complex(Frequency, Result, Imag);
end;

procedure TCustomParametricEQ.Reset;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].Reset;
end;

procedure TCustomParametricEQ.ResetStates;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].ResetStates;
end;

procedure TCustomParametricEQ.ResetStatesInt64;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].ResetStatesInt64;
end;

end.
