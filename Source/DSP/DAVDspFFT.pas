unit DAVDspFFT;

interface

implementation

uses
  Math, DAV_Common;

procedure FFT(Buffer: PAVDSingleFixedArray; FFTSize: Integer);
(*
 FFT routine, (C)1996 S.M.Bernsee. Sign = -1 is FFT, 1 is iFFT (inverse)
 Fills fftBuffer[0...2*fftFrameSize-1] with the Fourier transform of the time
 domain data in fftBuffer[0...2*fftFrameSize-1]. The FFT array takes and
 returns the cosine and sine parts in an interleaved manner, ie.
 fftBuffer[0] = cosPart[0], fftBuffer[1] = sinPart[0], asf. fftFrameSize
 must be a power of 2. It expects a complex input signal (see footnote 2), ie.
 when working with 'common' audio signals our input signal has to be passed
 as {in[0],0.,in[1],0.,in[2],0.,...end; asf. In that case, the transform of
 the frequencies of interest is in fftBuffer[0...fftFrameSize].
*)
var
  wr, wi, arg, temp  : Single;
  p1, p2             : PAVDSingleFixedArray;
  tr, ti, ur, ui     : Single;
  p1r, p1i, p2r, p2i : PSingle;
  i, bitm, j, le,
  le2, k, logN       : Integer;

begin
{
 logN := round(log2(FFTSize) + 0.5);

 i := 2;
 while i < 2 * FFTSize - 2 do
  begin

   j := 0;
   bitm := 2;
   while bitm < 2 * FFTSize do
    begin
     if (i and bitm) > 0
      then inc(j);
     j := j shl 1;
    end;

   if (i < j) then
    begin
(*
     p1      := Buffer[i];
     p2      := Buffer[j];
     temp    := *p1;
     *(p1++) := *p2;
     *(p2++) := temp;
     temp    := *p1;
     *p1     := *p2;
     *p2     := temp;
*)
    end;

   inc(i, 2);
  end;

 k := 0;
 le := 2;
 while k < logN do
  begin
   le  := le shl 1;
   le2 := le shr 1;
   ur  := 1.0;
   ui  := 0.0;
   arg := PI / (le2 shr 1);
   GetSinCos(arg, wr, wi);

   j := 0;
   while j < le2 do
    begin
     p1r := Buffer + j;
     p1i := p1r + 1;
     p2r := p1r + le2;
     p2i := p2r + 1;

     i := j;
     while i < 2 * FFTSize do
      begin
       tr := *p2r * ur - *p2i * ui;
       ti := *p2r * ui + *p2i * ur;
       *p2r := *p1r - tr; *p2i = *p1i - ti;
       *p1r := *p1r + tr;
       *p1i := *p1i + ti;
       p1r := p1r + le; p1i := p1i + le;
       p2r := p2r + le; p2i := p2i + le;
       i := i + le;
      end;

     tr := ur * wr - ui * wi;
     ui := ur * wi + ui * wr;
     ur := tr;

     bitm := bitm shl 1;
     inc(j, 2);
    end;
   inc(k);
  end;
}
end;

end.