unit DAV_DspWindowing;

interface

{$I ..\ASIOVST.INC}

uses
  DAV_Common;

procedure ApplyGaussianWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanHarrisWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyBlackmanWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHanningWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer); overload;
procedure ApplyHammingWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer); overload;

procedure ApplyGaussianWindow(var Data : TDAVSingleDynArray); overload;
procedure ApplyBlackmanWindow(var Data : TDAVSingleDynArray); overload;
procedure ApplyBlackmanHarrisWindow(var Data : TDAVSingleDynArray); overload;
procedure ApplyHanningWindow(var Data : TDAVSingleDynArray); overload;
procedure ApplyHammingWindow(var Data : TDAVSingleDynArray); overload;

implementation

// Generate window function (Gaussian)
procedure ApplyGaussianWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 j := SampleFrames - 1;
 for i := 0 to j
  do Data^[i] := Data^[i] * (exp(-5.0 / (sqr(j)) * (2 * i - j) * (2 * i - j)));
end;

procedure ApplyGaussianWindow(var Data : TDAVSingleDynArray);
begin
 ApplyGaussianWindow(@Data[0], Length(Data));
end;


procedure ApplyBlackmanWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer);
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

procedure ApplyBlackmanWindow(var Data : TDAVSingleDynArray);
begin
 ApplyBlackmanWindow(@Data[0], Length(Data));
end;


// Generate window function (Blackman-Harris)
procedure ApplyBlackmanHarrisWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer);
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

procedure ApplyBlackmanHarrisWindow(var Data : TDAVSingleDynArray);
begin
 ApplyBlackmanHarrisWindow(@Data[0], Length(Data));
end;


// Generate window function (Hanning)
procedure ApplyHanningWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.5 * (1.0 - cos(2 * PI * i * k)));
end;

procedure ApplyHanningWindow(var Data : TDAVSingleDynArray);
begin
 ApplyHanningWindow(@Data[0], Length(Data));
end;


// Generate window function (Hamming)
procedure ApplyHammingWindow(const Data : PDAVSingleFixedArray; const SampleFrames: Integer);
var
  i, j : Integer;
  k    : Double;
begin
 j := SampleFrames - 1;
 k := 1 / j;
 for i := 0 to j
  do Data^[i] := Data^[i] * (0.54 - (0.46 * cos(2 * PI * i * k)));
end;

procedure ApplyHammingWindow(var Data : TDAVSingleDynArray);
begin
 ApplyHammingWindow(@Data[0], Length(Data));
end;

end.
