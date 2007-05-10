unit DMinBlep;

// MinBLEP Generation Code
// By Daniel Werner
// This Code Is Public Domain
//
// translated to pascal by Christian-W. Budde

interface

uses DDSPBase;

procedure RealCepstrum(signal, realCepstrum : TSingleDynArray);
procedure MinimumPhase(realCepstrum, minimumPhase : TSingleDynArray);
function GenerateMinBLEP(zeroCrossings, overSampling : Integer) : TSingleDynArray;

implementation

uses Math;

// Generate Blackman Window
procedure BlackmanWindow(Data : TSingleDynArray);
var l,i   : Integer;
    f,fm : Double;
const cBlackman : array [0..2] of Double = ( 0.34, -0.5, 0.16);
begin
 l:=Length(Data) - 1;
 fm:=1/l;
 for i:=0 to l do
  begin
   // using the chebyshev polynom identity to get rid of the cos(2*x)
   f:=cos((2*PI*i)*fm);
   Data[i]:= cBlackman[0]+f*(cBlackman[1]+cBlackman[2]*f);
  end;
end;

// Complex Exponential
procedure ComplexExponential(Re, Im : Double; zx, zy: PSingle);
var expx : Double;
begin
 expx := exp(Re);
 zx^  := expx * cos(Im);
 zy^  := expx * sin(Im);
end;

// Compute Real Cepstrum Of Signal
procedure RealCepstrum(signal, realCepstrum : TSingleDynArray);
var realTime, imagTime, realFreq, imagFreq : TSingleDynArray;
    i,sz  : Integer;
begin
 sz:=Length(signal);
 Assert(Length(realCepstrum)=sz);

 SetLength(realTime, sz);
 SetLength(imagTime, sz);
 SetLength(realFreq, sz);
 SetLength(imagFreq, sz);

 // Compose Complex FFT Input
 for i:=0 to sz-1 do
  begin
   realTime[i] := signal[i];
   imagTime[i] := 0;
  end;

 // Perform DFT
 DFT(realTime, imagTime, realFreq, imagFreq);

 // Calculate Log Of Absolute Value
 for i:=0 to sz-1 do
  begin
   realFreq[i]:=log10(ComplexAbsolute(realFreq[i], imagFreq[i]));
   imagFreq[i]:=0;
  end;

 // Perform Inverse FFT
 InverseDFT(realTime, imagTime, realFreq, imagFreq);

 // Output Real Part Of FFT
 for i:=0 to sz-1
  do realCepstrum[i]:=realTime[i];
end;

// Compute Minimum Phase Reconstruction Of Signal
procedure MinimumPhase(realCepstrum, minimumPhase : TSingleDynArray);
var realTime, imagTime, realFreq, imagFreq : TSingleDynArray;
    n, i, nd2 : Integer;
begin
 n:=Length(realCepstrum);
 Assert(Length(minimumPhase)=n);

 nd2:=n div 2;
 SetLength(realTime, n);
 SetLength(imagTime, n);
 SetLength(realFreq, n);
 SetLength(imagFreq, n);

 if ((n mod 2) = 1) then
  begin
   realTime[0]:= realCepstrum[0];
   for i:= 1 to nd2-1 do realTime[i]:= 2.0 * realCepstrum[i];
   for i:= nd2 to n-1 do realTime[i]:= 0.0;
  end
 else
  begin
   realTime[0]:= realCepstrum[0];
   for i:= 1 to nd2-1 do realTime[i]:= 2.0 * realCepstrum[i];
   realTime[nd2]:=realCepstrum[nd2];
   for i:= nd2+1 to n-1 do realTime[i]:= 0.0;
  end;

 for i:= 1 to n-1 do imagTime[i] := 0;
 DFT(realTime, imagTime, realFreq, imagFreq);

 for i:=0 to n-1 do ComplexExponential(realFreq[i], imagFreq[i], @realFreq[i], @imagFreq[i]);
 InverseDFT(realTime, imagTime, realFreq, imagFreq);

 for i:=0 to n-1 do minimumPhase[i]:=realTime[i];
end;

// Generate MinBLEP And Return It In An Array Of Floating Point Values
function GenerateMinBLEP(zeroCrossings, overSampling : Integer) : TSingleDynArray;
var i, n     : Integer;
    r, a, b  : Double;
    buffer1, buffer2 : TSingleDynArray;
begin
 n:=(2 * zeroCrossings * overSampling) + 1;
 SetLength(buffer1,n);
 SetLength(buffer2,n);

 // Generate Sinc
 a:=-zeroCrossings;
 b:= zeroCrossings;
 for i:=0 to n-1 do
  begin
   r:=i/(n-1);
   buffer1[i]:=Sinc(a + (r * (b - a)));
  end;

 // Window Sinc
 BlackmanWindow(buffer2);
 for i:=0 to n-1 do buffer1[i] := buffer1[i]*buffer2[i];

 // Minimum Phase Reconstruction
 RealCepstrum(buffer1, buffer2);
 MinimumPhase(buffer2, buffer1);

 // Integrate Into MinBLEP
 setLength(result,n);
 a:=0;
 for i:=0 to n-1 do
  begin
   a:=a+buffer1[i];
   result[i]:=a;
  end;

 // Normalize
 a:=result[n - 1];
 a:=1/a;
 for i:=0 to n-1
  do result[i] := result[i] * a;
end;

end.
