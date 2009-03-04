unit DAV_DspWindowing;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common;

procedure ApplyHanningWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHammingWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanHarrisWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyGaussianWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyKaiserBesselWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer; const Alpha: Single); overload;

procedure ApplyHanningWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyHammingWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyBlackmanHarrisWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyGaussianWindow(var Data: TDAVSingleDynArray); overload;
procedure ApplyKaiserBesselWindow(var Data: TDAVSingleDynArray; const Alpha: Single); overload;

implementation

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


// Generate window function (Blackman
procedure ApplyBlackmanWindow(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  l, i  : Integer;
  f, fm : Double;
const
  CBlackman : array [0..2] of Double = (0.34, -0.5, 0.16);
begin
 l  := SampleFrames - 1;
 fm := 1 / l;
 for i:=0 to l do
  begin
   // using the chebyshev polynom identity to get rid of the cos(2*x)
   f := cos((2 * PI * i) * fm);
   Data^[i]:= Data^[i] * (CBlackman[0] + f * (CBlackman[1] + CBlackman[2] * f));
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


function Io(const x: Double): Double;
var
  y, de : Double;
  i     : Integer;
  sde   : Double;
const
  CEpsilon: Double = 1E-08;
begin
 y := 0.5 * x;
 de := 1.0;
 result := 1;
 for i := 1 to 25 do
  begin
   de := de * y / i;
   sde := sqr(de);
   result := result + sde;
   if (result * CEpsilon - sde) > 0 
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

end.
