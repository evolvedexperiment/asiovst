unit DAVDspWindowing;

interface

{$I ASIOVST.INC}

uses
  DAV_Common;

procedure ApplyBlackmanWindow(var Data : TAVDSingleDynArray);
procedure ApplyGaussianWindow(var Data : TAVDSingleDynArray);
procedure ApplyBlackmanHarrisWindow(var Data : TAVDSingleDynArray);
procedure ApplyHanningWindow(var Data : TAVDSingleDynArray);
procedure ApplyHammingWindow(var Data : TAVDSingleDynArray);

implementation

procedure ApplyBlackmanWindow(var Data : TAVDSingleDynArray);
var
  l, i  : Integer;
  f, fm : Double;
const cBlackman : array [0..2] of Double = ( 0.34, -0.5, 0.16);
begin
 l  := Length(Data) - 1;
 fm := 1 / l;
 for i:=0 to l do
  begin
   // using the chebyshev polynom identity to get rid of the cos(2*x)
   f := cos((2 * PI * i) * fm);
   Data[i]:= Data[i] * (cBlackman[0] + f * (cBlackman[1] + cBlackman[2] * f));
  end;
end;

// Generate window function (Gaussian)
procedure ApplyGaussianWindow(var Data : TAVDSingleDynArray);
var
  i, j : Integer;
begin
 j := Length(Data) - 1;
 for i := 0 to j
  do Data[i] := Data[i] * (exp(-5.0 / (sqr(j)) * (2 * i - j) * (2 * i - j)));
end;

// Generate window function (Blackman-Harris)
procedure ApplyBlackmanHarrisWindow(var Data : TAVDSingleDynArray);
var
  i, j : Integer;
  k    : Double;
begin
 j := Length(Data) - 1;
 k := 1 / j;
 for i := 0 to j
  do Data[i] := Data[i] * (0.35875 - 0.48829 * cos(2 * PI * (i + 0.5) * k)
                         + 0.14128 * cos(4 * PI * (i + 0.5) * k)
                         - 0.01168 * cos(6 * PI * (i + 0.5) * k));
end;

// Generate window function (Hanning)
procedure ApplyHanningWindow(var Data : TAVDSingleDynArray);
var
  i, j : Integer;
  k    : Double;
begin
 j := Length(Data) - 1;
 k := 1 / j;
 for i := 0 to j
  do Data[i] := Data[i] * (0.5 * (1.0 - cos(2 * PI * i * k)));
end;

// Generate window function (Hamming)
procedure ApplyHammingWindow(var Data : TAVDSingleDynArray);
var
  i, j : Integer;
  k    : Double;
begin
 j := Length(Data) - 1;
 k := 1 / j;
 for i := 0 to j
  do Data[i] := Data[i] * (0.54 - (0.46 * cos(2 * PI * i * k)));
end;

end.
