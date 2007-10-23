unit DDspDFT;

interface

  uses DAVDCommon, DAVDComplex;

  procedure DFT(realTime,imagTime,realFreq,imagFreq : TSingleDynArray); overload;
  procedure DFT(realTime,imagTime,realFreq,imagFreq : TDoubleDynArray); overload;
  procedure InverseDFT(realTime,imagTime,realFreq,imagFreq : TSingleDynArray); overload;
  procedure InverseDFT(realTime,imagTime,realFreq,imagFreq : TDoubleDynArray); overload;

  procedure DFT(realTime,realFreq,imagFreq : TSingleDynArray); overload;
  procedure DFT(realTime,realFreq,imagFreq : TDoubleDynArray); overload;
  procedure InverseDFT(realTime,realFreq,imagFreq : TSingleDynArray); overload;
  procedure InverseDFT(realTime,realFreq,imagFreq : TDoubleDynArray); overload;
  
implementation

procedure DFT(realTime,imagTime,realFreq,imagFreq : TSingleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(imagTime));
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realFreq[0],sz*sizeOf(Single),0);
 FillChar(imagFreq[0],sz*sizeOf(Single),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr) + (imagTime[i] * si);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si) + (imagTime[i] * sr);
    end;
  end;
end;

procedure InverseDFT(realTime,imagTime,realFreq,imagFreq : TSingleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(imagTime));
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realTime[0],sz*sizeOf(Single),0);
 FillChar(imagTime[0],sz*sizeOf(Single),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k]*sd;
   imagTime[k] := imagTime[k]*sd;
  end;
end;

procedure DFT(realTime,imagTime,realFreq,imagFreq : TDoubleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(imagTime));
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realFreq[0],sz*sizeOf(Double),0);
 FillChar(imagFreq[0],sz*sizeOf(Double),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr) + (imagTime[i] * si);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si) + (imagTime[i] * sr);
    end;
  end;
end;

procedure InverseDFT(realTime,imagTime,realFreq,imagFreq : TDoubleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(imagTime));
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realTime[0],sz*sizeOf(Double),0);
 FillChar(imagTime[0],sz*sizeOf(Double),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k]*sd;
   imagTime[k] := imagTime[k]*sd;
  end;
end;




procedure DFT(realTime,realFreq,imagFreq : TSingleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realFreq[0],sz*sizeOf(Single),0);
 FillChar(imagFreq[0],sz*sizeOf(Single),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si);
    end;
  end;
end;

procedure InverseDFT(realTime,realFreq,imagFreq : TSingleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realTime[0],sz*sizeOf(Single),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k]*sd;
  end;
end;

procedure DFT(realTime,realFreq,imagFreq : TDoubleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realFreq[0],sz*sizeOf(Double),0);
 FillChar(imagFreq[0],sz*sizeOf(Double),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si);
    end;
  end;
end;

procedure InverseDFT(realTime,realFreq,imagFreq : TDoubleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz:=Length(realTime);
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd:=1/sz;
 FillChar(realTime[0],sz*sizeOf(Double),0);

 for k:=0 to sz-1 do
  begin
   kc:=2*PI*k*sd;
   for i:=0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k]*sd;
  end;
end;

end.
