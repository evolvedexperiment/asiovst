unit DAV_DspDFT;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  DAV_Types, DAV_Complex;

procedure DFT(realTime, imagTime, realFreq, imagFreq : TDAVSingleDynArray); overload;
procedure DFT(realTime, imagTime, realFreq, imagFreq : TDAVDoubleDynArray); overload;
procedure InverseDFT(realTime, imagTime, realFreq, imagFreq : TDAVSingleDynArray); overload;
procedure InverseDFT(realTime, imagTime, realFreq, imagFreq : TDAVDoubleDynArray); overload;

procedure DFT(realTime, realFreq, imagFreq : TDAVSingleDynArray); overload;
procedure DFT(realTime, realFreq, imagFreq : TDAVDoubleDynArray); overload;
procedure InverseDFT(realTime, realFreq, imagFreq : TDAVSingleDynArray); overload;
procedure InverseDFT(realTime, realFreq, imagFreq : TDAVDoubleDynArray); overload;

function Goertzel(TimeSignal: TDAVSingleDynArray; const NormFrequency: Double): TComplexSingle; overload;
function Goertzel(TimeSignal: TDAVDoubleDynArray; const NormFrequency: Double): TComplexDouble; overload;
function Goertzel(TimeSignal: PDAVSingleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexSingle; overload;
function Goertzel(TimeSignal: PDAVDoubleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexDouble; overload;

function Goertzel(TimeSignal: TDAVSingleDynArray; const Angular: TComplexSingle): TComplexSingle; overload;
function Goertzel(TimeSignal: TDAVDoubleDynArray; const Angular: TComplexDouble): TComplexDouble; overload;
function Goertzel(TimeSignal: PDAVSingleFixedArray; const Length: Integer; const Angular: TComplexSingle): TComplexSingle; overload;
function Goertzel(TimeSignal: PDAVDoubleFixedArray; const Length: Integer; const Angular: TComplexDouble): TComplexDouble; overload;

implementation

uses
  DAV_Math;

procedure DFT(realTime, imagTime, realFreq, imagFreq : TDAVSingleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(imagTime));
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1 / sz;
 FillChar(realFreq[0], sz * SizeOf(Single), 0);
 FillChar(imagFreq[0], sz * SizeOf(Single), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz - 1 do
    begin
     GetSinCos(kc * i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr) + (imagTime[i] * si);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si) + (imagTime[i] * sr);
    end;
  end;
end;

procedure InverseDFT(realTime, imagTime, realFreq, imagFreq : TDAVSingleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(imagTime));
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1 / sz;
 FillChar(realTime[0], sz * SizeOf(Single), 0);
 FillChar(imagTime[0], sz * SizeOf(Single), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz - 1 do
    begin
     GetSinCos(kc * i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k] * sd;
   imagTime[k] := imagTime[k] * sd;
  end;
end;

procedure DFT(realTime, imagTime, realFreq, imagFreq : TDAVDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(imagTime));
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1 / sz;
 FillChar(realFreq[0], sz * SizeOf(Double), 0);
 FillChar(imagFreq[0], sz * SizeOf(Double), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz - 1 do
    begin
     GetSinCos(kc * i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr) + (imagTime[i] * si);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si) + (imagTime[i] * sr);
    end;
  end;
end;

procedure InverseDFT(realTime,imagTime,realFreq,imagFreq : TDAVDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(imagTime));
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1/sz;
 FillChar(realTime[0], sz * SizeOf(Double), 0);
 FillChar(imagTime[0], sz * SizeOf(Double), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz - 1 do
    begin
     GetSinCos(kc*i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k] * sd;
   imagTime[k] := imagTime[k] * sd;
  end;
end;




procedure DFT(realTime,realFreq,imagFreq : TDAVSingleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1/sz;
 FillChar(realFreq[0],sz*SizeOf(Single),0);
 FillChar(imagFreq[0], sz * SizeOf(Single), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz-1 do
    begin
     GetSinCos(kc * i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si);
    end;
  end;
end;

procedure InverseDFT(realTime,realFreq,imagFreq : TDAVSingleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1 / sz;
 FillChar(realTime[0], sz * SizeOf(Single), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz - 1 do
    begin
     GetSinCos(kc * i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k] * sd;
  end;
end;

procedure DFT(realTime,realFreq,imagFreq : TDAVDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1 / sz;
 FillChar(realFreq[0], sz * SizeOf(Double), 0);
 FillChar(imagFreq[0], sz * SizeOf(Double), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz - 1 do
    begin
     GetSinCos(kc * i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si);
    end;
  end;
end;

procedure InverseDFT(realTime,realFreq,imagFreq : TDAVDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Double;
begin
 sz := Length(realTime);
 Assert(sz = Length(realFreq));
 Assert(sz = Length(imagFreq));

 sd := 1 / sz;
 FillChar(realTime[0], sz * SizeOf(Double), 0);

 for k := 0 to sz - 1 do
  begin
   kc := 2 * PI * k * sd;
   for i := 0 to sz - 1 do
    begin
     GetSinCos(kc * i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k] * sd;
  end;
end;

function Goertzel(TimeSignal: TDAVSingleDynArray; const NormFrequency: Double): TComplexSingle;
{$IFDEF PUREPASCAL}
var
  Pos, Angle : TComplexDouble;
  i          : Integer;
begin
 Pos.Re   := 0;
 Pos.Im   := 1;
 GetSinCos(NormFrequency, Angle.Im, Angle.Re);
 result.Re := 0;
 result.Im := TimeSignal[0]; // -0,00001
 for i := 0 to Length(TimeSignal) - 1 do
  begin
   ComplexMultiplyInplace64(Pos, Angle);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;

end;
{$ELSE}
asm
 fld NormFrequency.Double          // NormFrequency
 fsincos                           // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Single           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 mov ecx, [TimeSignal - 4].Integer // ecx = Length(TimeSignal)
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 4                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 fstp Result.Im.Single             // Result.Im.Single := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Single             // Result.Re.Single := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 finit                             // (cleared)
end;
{$ENDIF}

function Goertzel(TimeSignal: TDAVDoubleDynArray; const NormFrequency: Double): TComplexDouble;
{$IFDEF PUREPASCAL}
var
  Pos, Angle : TComplexDouble;
  i          : Integer;
begin
 Pos.Re := 0;
 Pos.Im := 1;
 GetSinCos(NormFrequency, Angle.Im, Angle.Re);
 result.Re := 0;
 result.Im := TimeSignal[0];
 for i := 1 to Length(TimeSignal) - 1 do
  begin
   ComplexMultiplyInplace64(Pos, Angle);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;
end;
{$ELSE}
asm
 fld NormFrequency.Double          // NormFrequency
 fsincos                           // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Double           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 mov ecx, [TimeSignal - 4].Integer // ecx = Length(TimeSignal)
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 8                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 fstp Result.Im.Double             // Result.Im.Double := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Double             // Result.Re.Double := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 finit                             // (cleared)
end;
{$ENDIF}

function Goertzel(TimeSignal: PDAVSingleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexSingle; overload;
{$IFDEF PUREPASCAL}
var
  Pos, Angle : TComplexDouble;
  i          : Integer;
begin
 Pos.Re   := 0;
 Pos.Im   := 1;
 GetSinCos(NormFrequency, Angle.Im, Angle.Re);
 Result.Re := 0;
 Result.Im := TimeSignal[0]; // -0,00001
 for i := 1 to Length - 1 do
  begin
   ComplexMultiplyInplace64(Pos, Angle);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;
end;
{$ELSE}
asm
 fld NormFrequency.Double          // NormFrequency
 fsincos                           // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Single           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 push ecx
 mov ecx, edx                      // ecx = Length
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 4                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 pop ecx
 fstp Result.Im.Single             // Result.Im.Single := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Single             // Result.Re.Single := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
end;
{$ENDIF}

function Goertzel(TimeSignal: PDAVDoubleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexDouble; overload;
{$IFDEF PUREPASCAL}
var
  Pos, Angle : TComplexDouble;
  i          : Integer;
begin
 Pos.Re := 0;
 Pos.Im := 1;
 GetSinCos(NormFrequency, Angle.Im, Angle.Re);
 Result.Re := 0;
 Result.Im := TimeSignal[0];
 for i := 1 to Length - 1 do
  begin
   ComplexMultiplyInplace64(Pos, Angle);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;
end;
{$ELSE}
asm
 fld NormFrequency.Double          // NormFrequency
 fsincos                           // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Double           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 push ecx
 mov ecx, edx                      // ecx = Length(TimeSignal)
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 8                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 pop ecx
 fstp Result.Im.Double             // Result.Im.Single := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Double             // Result.Re.Single := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
end;
{$ENDIF}

function Goertzel(TimeSignal: TDAVSingleDynArray; const Angular: TComplexSingle): TComplexSingle;
{$IFDEF PUREPASCAL}
var
  Pos : TComplexSingle;
  i   : Integer;
begin
 Pos.Re   := 0;
 Pos.Im   := 1;
 result.Re := 0;
 Result.Im := TimeSignal[0]; // -0,00001
 for i := 0 to Length(TimeSignal) - 1 do
  begin
   ComplexMultiplyInplace32(Pos, Angular);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;

end;
{$ELSE}
asm
 fld Angular.Im.Single             // Angle.Im
 fld Angular.Re.Single             // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Single           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 mov ecx, [TimeSignal - 4].Integer // ecx = Length(TimeSignal)
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 4                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 fstp Result.Im.Single             // Result.Im.Single := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Single             // Result.Re.Single := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 finit                             // (cleared)
end;
{$ENDIF}

function Goertzel(TimeSignal: TDAVDoubleDynArray; const Angular: TComplexDouble): TComplexDouble;
{$IFDEF PUREPASCAL}
var
  Pos : TComplexDouble;
  i   : Integer;
begin
 Pos.Re := 0;
 Pos.Im := 1;
 Result.Re := 0;
 Result.Im := TimeSignal[0];
 for i := 1 to Length(TimeSignal) - 1 do
  begin
   ComplexMultiplyInplace64(Pos, Angular);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;
end;
{$ELSE}
asm
 fld Angular.Im.Double             // Angle.Im
 fld Angular.Re.Double             // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Double           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 mov ecx, [TimeSignal - 4].Integer // ecx = Length(TimeSignal)
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 8                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 fstp Result.Im.Double             // Result.Im.Double := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Double             // Result.Re.Double := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 finit                             // (cleared)
end;
{$ENDIF}

function Goertzel(TimeSignal: PDAVSingleFixedArray; const Length: Integer; const Angular: TComplexSingle): TComplexSingle; overload;
{$IFDEF PUREPASCAL}
var
  Pos : TComplexSingle;
  i   : Integer;
begin
 Pos.Re   := 0;
 Pos.Im   := 1;
 Result.Re := 0;
 Result.Im := TimeSignal[0]; // -0,00001
 for i := 1 to Length - 1 do
  begin
   ComplexMultiplyInplace32(Pos, Angular);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;
end;
{$ELSE}
asm
 fld Angular.Im.Single             // Angle.Im
 fld Angular.Re.Single             // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Single           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 push ecx
 mov ecx, edx                      // ecx = Length
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 4                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Single                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 pop ecx
 fstp Result.Im.Single             // Result.Im.Single := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Single             // Result.Re.Single := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
end;
{$ENDIF}

function Goertzel(TimeSignal: PDAVDoubleFixedArray; const Length: Integer; const Angular: TComplexDouble): TComplexDouble; overload;
{$IFDEF PUREPASCAL}
var
  Pos : TComplexDouble;
  i   : Integer;
begin
 Pos.Re := 0;
 Pos.Im := 1;
 Result.Re := 0;
 Result.Im := TimeSignal[0];
 for i := 1 to Length - 1 do
  begin
   ComplexMultiplyInplace64(Pos, Angular);
   Result.Re := Result.Re + Pos.Re * TimeSignal[i];
   Result.Im := Result.Im + Pos.Im * TimeSignal[i];
  end;
end;
{$ELSE}
asm
 fld Angular.Im.Double             // Angle.Im
 fld Angular.Re.Double             // Angle.Re, Angle.Im
 fld1                              // Pos.Im, Angle.Re, Angle.Im
 fldz                              // Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fld [TimeSignal].Double           // Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fldz                              // Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 mov eax, TimeSignal               // eax = TimeSignal
 push ecx
 mov ecx, edx                      // ecx = Length(TimeSignal)
 dec ecx                           // ecx = Length(TimeSignal) - 1

@calcloop:
  add eax, 8                       // next timesignal
  fld  st(2)                       // Pos.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(5)                // Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(7)                // Pos.Im * Angle.Im, Pos.Re * Angle.Re, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fsubp                            // newPos.Re := Pos.Re * Angle.Re - Pos.Im * Angle.Im, Result.Re, Result.Im, Pos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(3)                       // Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fld  st(4)                       // Pos.Im, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fmul st(0), st(6)                // Pos.Im * Angle.Re, Angle.Im * Pos.Re, Result.Re, Result.Im, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  faddp                            // newPos.Im := Pos.Im * Angle.Re + Angle.Im * Pos.Re, Result.Im, Result.Re, newPos.Re, Pos.Im, Angle.Re, Angle.Im
  fxch st(4)                       // Pos.Im, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fstp st(0)                       // Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(3)                // TimeSignal * newPos.Re, Result.Re, Result.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Re + TimeSignal * newPos.Re, Result.Im, NewPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch st(1)                       // Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fld [eax].Double                 // TimeSignal, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fmul st(0), st(4)                // TimeSignal * newPos.Im, Result.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  faddp                            // Result.Im + TimeSignal * newPos.Im, NewResult.Re, newPos.Re, newPos.Im, Angle.Re, Angle.Im
  fxch                             // NewResult.Re, NewResult.Im, newPos.Re, newPos.Im, Angle.Re, Angle.Im

 loop @calcloop

 pop ecx
 fstp Result.Im.Double             // Result.Im.Single := Result.Im, Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp Result.Re.Double             // Result.Re.Single := Result.Re, Pos.Re, Pos.Im, Angle.Re, Angle.Im
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
 fstp st(0)                        // (cleared)
end;
{$ENDIF}

end.