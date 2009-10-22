unit DAV_DspWindowing;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_Classes, DAV_Complex;

const
  CHamming : array [0..1] of Double = (0.54, -0.46);
  CHanning : Double =  - 0.5;

  CLawrey5T : array [0..4] of Double = (0.1649521, -0.4063172, 0.3300816,
    -0.093714, 0.0049992);
  CLawrey6T : array [0..5] of Double = (0.0990904, -0.3134582, 0.3639074,
    -0.1849256, 0.0370024, -0.001616);
  CNutallCTD : array [0..3] of Double = (0.177892, -0.427892, 0.322108,
    -0.072108);
  CNutallCFD : array [0..3] of Double = (0.211536, -0.449584, 0.288464,
    -0.050416);
  CBlackmanNutall : array [0..3] of Double = (0.2269824, -0.4572542, 0.273199,
    -0.0425644);
  CBlackman : array [0..2] of Double = (0.34, -0.5, 0.16);
  CExactBlackman : array [0..2] of Double = (0.349742046, -0.496560619,
    0.153697334);
  CFlatTop : array [0..4] of Double = (-5.5603448275862e-2, -1.6508620689655e-1,
    5.0086206896552e-1, -3.3448275862069e-1, 5.5172413793103e-2);
  CAlbrecht2 : array [0..1] of Double = (5.383553946707251E-1,
    -4.616446053292749E-1);
  CAlbrecht3 : array [0..2] of Double = (3.46100822018625E-1,
    -4.97340635096738E-1, 1.56558542884637E-1);
  CAlbrecht4 : array [0..3] of Double = (2.26982412792069E-1,
    -4.57254070828427E-1, 2.73199027957384E-1, -4.25644884221201E-2);
  CAlbrecht5 : array [0..4] of Double = (1.4894260601583E-1,
    -3.86001173639176E-1, 3.40977403214053E-1, -1.139879604246E-1,
     1.00908567063414E-2);
  CAlbrecht6 : array [0..5] of Double = (9.71676200107429E-2,
    -3.08845222524055E-1,  3.62623371437917E-1, -1.88953325525116E-1,
     4.02095714148751E-2, -2.2008890872942E-3);
  CAlbrecht7 : array [0..6] of Double = (6.39644241143904E-2,
    -2.39938645993528E-1, 3.50159563238205E-1, -2.47741118970808E-1,
    8.5438256055858E-2, -1.23202033692932E-2, 4.37788257917735E-4);
  CAlbrecht8 : array [0..7] of Double = (4.21072107042137E-2,
    -1.82076226633776E-1,  3.17713781059942E-1, -2.84438001373442E-1,
     1.36762237777383E-1, -3.34038053504025E-2,  3.41677216705768E-3,
    -8.19649337831348E-5);
  CAlbrecht9 : array [0..8] of Double = (+2.76143731612611E-2,
    -1.35382228758844E-1,  2.75287234472237E-1, -2.98843335317801E-1,
     1.85319330279284E-1, -6.48884482549063E-2,  1.17641910285655E-2,
    -8.85987580106899E-4,  1.48711469943406E-5);
  CAlbrecht10 : array [0..9] of Double = (1.79908225352538E-2,
    -9.8795958606521E-2, 2.29883817001211E-1, -2.94113019095183E-1,
     2.24338977814325E-1, -1.03248806248099E-1, 2.75674109448523E-2,
    -3.83958622947123E-3, 2.18971708430106E-4, -2.62981665347889E-6);
  CAlbrecht11 : array [0..10] of Double = (1.18717127796602E-2,
    -7.19533651951142E-2,  1.87887160922585E-1, -2.75808174097291E-1,
     2.48904243244464E-1, -1.41729867200712E-1,  5.02002976228256E-2,
    -1.04589649084984E-2,  1.1361511274166E-3,  -4.96285981703436E-5,
     4.3430326268572E-7);
  CBlackmanHarris3T : array [0..2] of Double = (0.34401, -0.49755, 0.15844);
  CBlackmanHarris4T : array [0..3] of Double = (0.21747, -0.45325, 0.28256,
    -0.04672);
  CBlackmanHarris7T : array [0..6] of Double = (6.3726256031333E-2,
    -0.239404524196522,   0.350002032211588,  -0.248170089047604,
     8.5827350354072E-2, -1.2425357203616E-2,  4.44390955264E-4);
  CBurgessOpt59 : array [0..2] of Double = (0.414643, -0.491728, 0.093654);
  CBurgessOpt71 : array [0..2] of Double = (0.346098, -0.49734, 0.156562);

type
  TParameterRecord = record
    ComplexPosition          : TComplexDouble;
    ComplexAngle             : TComplexDouble;
    SpectrumCorrectionFactor : Double;
    SpuaredCorrectionFactor  : Double;
    CoefficientPointer       : PDAVDoubleFixedArray;
  end;

procedure ApplyTriangleWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHanningWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHammingWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanHarrisWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyGaussianWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyKaiserBesselWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer; const Alpha: Single); overload;

procedure ApplyTriangleWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyHanningWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyHammingWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanHarrisWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyGaussianWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyKaiserBesselWindow(var Data: TDAVSingleDynArray; const Alpha: Single); overload;

procedure DoWinLoopCos2T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos2T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos2T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos2T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos3T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos3T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos3T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos3T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos4T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos4T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos4T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos4T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos5T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos5T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos5T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos5T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos6T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos6T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos6T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos6T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos7T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos7T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos7T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos7T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos8T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos8T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos8T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos8T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos9T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos9T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos9T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos9T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos10T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos10T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos10T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos10T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCos11T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos11T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCos11T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCos11T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopTriangle32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopTriangle64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopTriangle32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopTriangle64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopCosine32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCosine64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopCosine32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopCosine64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopLanczos32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopLanczos64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopLanczos32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopLanczos64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
procedure DoWinLoopHanning32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopHanning64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
procedure DoWinLoopHanning32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
procedure DoWinLoopHanning64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);

implementation

uses
  DAV_Common;

// Generate window function (Triangle)
procedure ApplyTriangleWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to (SampleFrames div 2) - 1
  do Data^[i] := i / (SampleFrames div 2) * Data^[i];
 for i := (SampleFrames div 2) to SampleFrames - 1
  do Data^[i] := (SampleFrames - i) / (SampleFrames div 2) * Data^[i];
end;

procedure ApplyTriangleWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHanningWindow(@Data[0], Length(Data));
end;


// Generate window function (Hanning)
procedure ApplyHanningWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.5 * (1.0 - cos(2 * PI * i * k)));
end;

procedure ApplyHanningWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHanningWindow(@Data[0], Length(Data));
end;


// Generate window function (Hamming)
procedure ApplyHammingWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.54 - (0.46 * cos(2 * PI * i * k)));
end;

procedure ApplyHammingWindow(var Data: TDAVSingleDynArray);
begin
 ApplyHammingWindow(@Data[0], Length(Data));
end;


// Generate window function (Gaussian)
procedure ApplyGaussianWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 j := SampleFrames - 1;
 for i := 0 to j
  do Data^[i] := Data^[i] * (exp(-5.0 / (sqr(j)) * (2 * i - j) * (2 * i - j)));
end;

procedure ApplyGaussianWindow(var Data: TDAVSingleDynArray);
begin
 ApplyGaussianWindow(@Data[0], Length(Data));
end;


// Generate window function (Blackman)
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Phase  : TComplexDouble;
  Value  : TComplexDouble;
const
  CBlackman : array [0..2] of Double = (0.34, -0.5, 0.16);
begin
 Value.Re := 1;
 Value.Im := 0;
 GetSinCos(2 * PI / (SampleFrames - 1), Phase.Im, Phase.Re);
 for Sample := 0 to SampleFrames - 1 do
  begin
   // using the chebyshev polynom identity to get rid of the cos(2*x)
   Data^[Sample] := Data^[Sample] * (CBlackman[0] + Value.Re * (CBlackman[1] + CBlackman[2] * Value.Re));
   ComplexMultiplyInplace(Value, Phase);
  end;
end;

procedure ApplyBlackmanWindow(var Data: TDAVSingleDynArray);
begin
 ApplyBlackmanWindow(@Data[0], Length(Data));
end;


// Generate window function (Blackman-Harris)
procedure ApplyBlackmanHarrisWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.35875 - 0.48829 * cos(2 * PI * (i + 0.5) * k)
                           + 0.14128 * cos(4 * PI * (i + 0.5) * k)
                           - 0.01168 * cos(6 * PI * (i + 0.5) * k));
end;

procedure ApplyBlackmanHarrisWindow(var Data: TDAVSingleDynArray);
begin
 ApplyBlackmanHarrisWindow(@Data[0], Length(Data));
end;


function Io(const Value: Double): Double;
var
  y, de : Double;
  i     : Integer;
  sde   : Double;
const
  CEpsilon: Double = 1E-08;
begin
 y := 0.5 * Value;
 de := 1.0;
 Result := 1;
 for i := 1 to 25 do
  begin
   de := de * y / i;
   sde := sqr(de);
   Result := Result + sde;
   if (Result * CEpsilon - sde) > 0
    then break;
  end;
end;

// Generate window function (Kaiser-Bessel)
procedure ApplyKaiserBesselWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer; const Alpha: Single); overload;
var
  i    : Integer;
  bes  : Double;
  odd  : Integer;
  xi   : Double;
  xind : Double;
begin
 bes := 1.0 / Io(Alpha);
 odd := SampleFrames mod 2;
 xind := sqr(SampleFrames - 1);
 for i := 0 to SampleFrames - 1 do
  begin
   if (odd = 1)
    then xi := i + 0.5
    else xi := i;
   xi  := 4 * sqr(xi);
   Data^[i] := Io(Alpha * sqrt(1 - xi/xind)) * bes;
  end;
end;

procedure ApplyKaiserBesselWindow(var Data: TDAVSingleDynArray; const Alpha: Single);
begin
 ApplyKaiserBesselWindow(@Data[0], Length(Data), Alpha);
end;

procedure DoWinLoopCos2T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var
  r  : Integer;
  cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw           := (CoefficientPointer[0] - PDAVCoefficientsArray(CoefficientPointer)^[1] * xPosition.Im);
    SpkCrFk      := SpkCrFk + cw;
    SpkCrFkSq    := SpkCrFk + cw * cw;
    StartAdr[r]  := StartAdr[r] * cw;
    cw           := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos2T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] -  PDAVCoefficientsArray(CoefficientPointer)^[1] * xPosition.Im);
    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos2T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw                := CoefficientPointer[0] - PDAVCoefficientsArray(CoefficientPointer)^[1] * xPosition.Im;
    SpkCrFk           := SpkCrFk + cw;
    SpkCrFkSq         := SpkCrFk + cw * cw;
    StartAdr[r]       := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw                := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im      := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re      := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos2T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul [ebx + 8].Double
   fadd [ebx].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
@exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw                    := CoefficientPointer[0] - PDAVCoefficientsArray(CoefficientPointer)^[1] * xPosition.Im;
    SpkCrFk               := SpkCrFk + cw;
    SpkCrFkSq             := SpkCrFk + cw * cw;
    StartAdr[r]           := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw                    := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im          := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re          := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var
  r  : Integer;
  cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw           := CoefficientPointer[0] + xPosition.Im  * 
                    (PDAVCoefficientsArray(CoefficientPointer)^[1] + xPosition.Im  *
                     PDAVCoefficientsArray(CoefficientPointer)^[2]);
    SpkCrFk      := SpkCrFk + cw;
    SpkCrFkSq    := SpkCrFk + cw * cw;
    StartAdr[r]  := StartAdr[r] * cw;
    cw           := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
         CoefficientPointer[ci[2]]));
    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
         CoefficientPointer[ci[2]]));
    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos3T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
         CoefficientPointer[ci[2]]));
    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos4T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ 0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
         CoefficientPointer[ci[3]])));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos4T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
         CoefficientPointer[ci[3]])));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos4T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
         CoefficientPointer[ci[3]])));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos4T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
         CoefficientPointer[ci[3]])));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos5T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[0] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
         CoefficientPointer[ci[4]]))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos5T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
         CoefficientPointer[ci[4]]))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos5T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..4] of Integer = (0, 1, 2, 3, 4);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
         CoefficientPointer[ci[4]]))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos5T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..4] of Integer = (0, 1, 2, 3, 4);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
         CoefficientPointer[ci[4]]))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos6T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..5] of Integer = (0, 1, 2, 3, 4, 5);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
         CoefficientPointer[ci[5]])))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos6T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..5] of Integer = (0, 1, 2, 3, 4, 5);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
         CoefficientPointer[ci[5]])))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos6T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..5] of Integer = (0, 1, 2, 3, 4, 5);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
         CoefficientPointer[ci[5]])))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos6T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..5] of Integer = (0, 1, 2, 3, 4, 5);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
         CoefficientPointer[ci[5]])))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos7T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..6] of Integer = (0, 1, 2, 3, 4, 5, 6);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
         CoefficientPointer[ci[6]]))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos7T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..6] of Integer = (0, 1, 2, 3, 4, 5, 6);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  *
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
         CoefficientPointer[ci[6]]))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos7T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
const
  ci : Array [0..6] of Integer = (0, 1, 2, 3, 4, 5, 6);
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
         CoefficientPointer[ci[6]]))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos7T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
         CoefficientPointer[ci[6]]))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos8T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
         CoefficientPointer[ci[7]])))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos8T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  *
         CoefficientPointer[ci[7]])))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos8T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
         CoefficientPointer[ci[7]])))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos8T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
         CoefficientPointer[ci[7]])))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos9T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
         CoefficientPointer[ci[8]]))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos9T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  *
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
         CoefficientPointer[ci[8]]))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos9T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  *
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
         CoefficientPointer[ci[8]]))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos9T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  *
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
         CoefficientPointer[ci[8]]))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos10T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
         CoefficientPointer[ci[9]])))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos10T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
         CoefficientPointer[ci[9]])))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos10T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  *
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
         CoefficientPointer[ci[9]])))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos10T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
         CoefficientPointer[ci[9]])))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos11T32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
        (CoefficientPointer[ci[9]] + xPosition.Im  * 
         CoefficientPointer[ci[10]]))))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos11T64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
        (CoefficientPointer[ci[9]] + xPosition.Im  * 
         CoefficientPointer[ci[10]]))))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos11T32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
        (CoefficientPointer[ci[9]] + xPosition.Im  * 
         CoefficientPointer[ci[10]]))))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCos11T64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld [ebx + 80].Double
   fmul st(0), st(2)
   fadd [ebx + 72].Double
   fmul st(0), st(2)
   fadd [ebx + 64].Double
   fmul st(0), st(2)
   fadd [ebx + 56].Double
   fmul st(0), st(2)
   fadd [ebx + 48].Double
   fmul st(0), st(2)
   fadd [ebx + 40].Double
   fmul st(0), st(2)
   fadd [ebx + 32].Double
   fmul st(0), st(2)
   fadd [ebx + 24].Double
   fmul st(0), st(2)
   fadd [ebx + 16].Double
   fmul st(0), st(2)
   fadd [ebx +  8].Double
   fmul st(0), st(2)
   fadd [ebx  ].Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := (CoefficientPointer[ci[0]] + xPosition.Im  * 
        (CoefficientPointer[ci[1]] + xPosition.Im  * 
        (CoefficientPointer[ci[2]] + xPosition.Im  * 
        (CoefficientPointer[ci[3]] + xPosition.Im  * 
        (CoefficientPointer[ci[4]] + xPosition.Im  * 
        (CoefficientPointer[ci[5]] + xPosition.Im  * 
        (CoefficientPointer[ci[6]] + xPosition.Im  * 
        (CoefficientPointer[ci[7]] + xPosition.Im  * 
        (CoefficientPointer[ci[8]] + xPosition.Im  * 
        (CoefficientPointer[ci[9]] + xPosition.Im  * 
         CoefficientPointer[ci[10]]))))))))));

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
   end
end;
{$ENDIF}

procedure DoWinLoopTriangle32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @triloop:
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)
   add eax, 4
 loop @triloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double   // SpkCrFkSq

 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpkCrFk := SpkCrFk + xPosition.Re;
    SpkCrFkSq := SpkCrFkSq + Sqr(xPosition.Re);
    StartAdr[r] := StartAdr[r] * xPosition.Re;
    xPosition.Re := xPosition.Re + xAngle.Re;
   end
end;
{$ENDIF}

procedure DoWinLoopTriangle32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @cosloop:
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single
   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(2), st(0)
   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)                 // Cnt + Ofs, Ofs, fSpkCorFak, fSpkCorFakSq
   add eax, 4
   sub edi, 4
 loop @cosloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double   // SpkCrFkSq

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  r  : Integer;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpkCrFk      := SpkCrFk + 2 * xPosition.Re;
    SpkCrFkSq    := SpkCrFkSq + 2 * Sqr(xPosition.Re);
    StartAdr[r]  := StartAdr[r] * xPosition.Re;
    EndAdr[-r]   := EndAdr[-r] * xPosition.Re;
    xPosition.Re := xPosition.Re + xAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopTriangle64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @triloop:
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)
   add eax, 8
 loop @triloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double   // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var
  r  : Integer;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpkCrFk := SpkCrFk + xPosition.Re;
    SpkCrFkSq := SpkCrFkSq + Sqr(xPosition.Re);
    StartAdr[r] := StartAdr[r] * xPosition.Re;
    xPosition.Re := xPosition.Re + xAngle.Re;
   end
end;
{$ENDIF}

procedure DoWinLoopTriangle64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fld [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq

 @cosloop:
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double
   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(2), st(0)
   fadd st(2), st(0)
   fld st(0)
   fmul st(0), st(0)
   faddp st(4), st(0)

   fadd st(0), st(1)                 // Cnt + Ofs, Ofs, fSpkCorFak, fSpkCorFakSq
   add eax, 8
   sub edi, 8
 loop @cosloop

 fstp [edx     ].Double   // Counter, Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 16].Double   // Offset, fSpkCorFak, fSpkCorFakSq
 fstp [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double   // SpkCrFkSq

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var
  r  : Integer;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    SpkCrFk      := SpkCrFk + 2 * xPosition.Re;
    SpkCrFkSq    := SpkCrFkSq + 2 * Sqr(xPosition.Re);
    StartAdr[r]  := StartAdr[r] * xPosition.Re;
    EndAdr[-r]   := EndAdr[-r] * xPosition.Re;
    xPosition.Re := xPosition.Re + xAngle.Re;
   end;
end;
{$ENDIF}

procedure DoWinLoopCosine32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im
   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 4
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im
   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopCosine64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double
   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im
   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
   sub edi, 8
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im
   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 4
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq

 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im
   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopLanczos64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(0)
   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double
   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im
   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im
   add eax, 8
   sub edi, 8
 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning32Forward(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning32Symmetric(StartAdr: PDAVSingleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVSingleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Single
   fmul st(0), st(1)
   fstp [eax].Single

   fld [edi].Single
   fmul st(0), st(1)
   fstp [edi].Single

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 4
   sub edi, 4

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning64Forward(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 mov ebx, [edx + 48]
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re := cw;
    cw := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk := SpkCrFk + cw;
    SpkCrFkSq := SpkCrFk + cw * cw;
    StartAdr[r] := StartAdr[r] * cw;
   end
end;
{$ENDIF}

procedure DoWinLoopHanning64Symmetric(StartAdr: PDAVDoubleFixedArray; var ParameterRecord: TParameterRecord; SampleCount: Integer; EndAdr: PDAVDoubleFixedArray);
{$IFNDEF PUREPASCAL}
asm
 or ecx, ecx
 jng @exit
 push ebx
 push edi
 mov ebx, [edx + 48]
 mov edi, EndAdr
 fld [edx + 40].Double   // SpkCrFkSq
 fld [edx + 32].Double   // SpkCrFk, SpkCrFkSq
 fld [edx + 24].Double   // xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx + 16].Double   // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx +  8].Double   // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fld [edx     ].Double   // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq

 @cosloop:
   fld st(1)
   fmul CHanning.Double
   fsub CHanning.Double

   fld [eax].Double
   fmul st(0), st(1)
   fstp [eax].Double

   fld [edi].Double
   fmul st(0), st(1)
   fstp [edi].Double

   fadd st(5), st(0)
   fadd st(5), st(0)
   fmul st(0), st(0)
   faddp st(6), st(0)

   fld st(3)             // Im, r, i, Re, Im
   fmul st(0), st(2)     // Im * i, r, i, Re, Im
   fld st(3)             // Re, Im * i, r, i, Re, Im
   fmul st(0), st(2)     // Re * r, Im * i, r, i, Re, Im
   fsubrp                // newRe, r, i, Re, Im

   fld st(4)             // Im, newRe, r, i, Re, Im
   fmulp st(2), st(0)    // newRe, Im * r, i, Re, Im
   fld st(3)             // Re, newRe, Im * r, i, Re, Im
   fmulp st(3), st(0)    // newRe, Im * r, Re * i, Re, Im
   fxch                  // Im * r, newRe, Re * i, Re, Im
   faddp st(2), st(0)    // newRe, newIm, Re, Im

   add eax, 8
   sub edi, 8

 loop @cosloop

 fstp [edx     ].Double  // xPosition.Re, xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp [edx +  8].Double  // xPosition.Im, xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Re, xAngle.Im, SpkCrFk, SpkCrFkSq
 fstp st(0)              // xAngle.Im, SpkCrFk, SpkCrFkSq

 fstp [edx + 32].Double  // SpkCrFk, SpkCrFkSq
 fstp [edx + 40].Double  // SpkCrFkSq
 pop edi
 pop ebx
 @Exit:
end;
{$ELSE}
var r  : Integer;
    cw : Double;
begin
 for r := 0 to SampleCount - 1 do
  with ParameterRecord do
   begin
    cw                    := xPosition.Re * xAngle.Re-xPosition.Im * xAngle.Im;
    xPosition.Im          := xPosition.Im * xAngle.Re + xPosition.Re * xAngle.Im;
    xPosition.Re          := cw;
    cw                    := (0.5 -  0.5 * xPosition.Im);

    SpkCrFk               := SpkCrFk + cw;
    SpkCrFkSq             := SpkCrFk + cw * cw;
    StartAdr[r]           := StartAdr[r] * cw;
    EndAdr[SampleCount - 1 - r] := EndAdr[SampleCount - 1 - r] * cw;
   end
end;
{$ENDIF}

end.
