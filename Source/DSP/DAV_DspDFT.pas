unit DAV_DspDFT;

interface

{$I ASIOVST.INC}

uses
  DAV_Common, DAV_Complex;

procedure DFT(realTime, imagTime, realFreq, imagFreq : TAVDSingleDynArray); overload;
procedure DFT(realTime, imagTime, realFreq, imagFreq : TAVDDoubleDynArray); overload;
procedure InverseDFT(realTime, imagTime, realFreq, imagFreq : TAVDSingleDynArray); overload;
procedure InverseDFT(realTime, imagTime, realFreq, imagFreq : TAVDDoubleDynArray); overload;

procedure DFT(realTime, realFreq, imagFreq : TAVDSingleDynArray); overload;
procedure DFT(realTime, realFreq, imagFreq : TAVDDoubleDynArray); overload;
procedure InverseDFT(realTime, realFreq, imagFreq : TAVDSingleDynArray); overload;
procedure InverseDFT(realTime, realFreq, imagFreq : TAVDDoubleDynArray); overload;

function Goertzel(TimeSignal: TAVDSingleDynArray; const NormFrequency: Double): TComplexSingle; overload;
function Goertzel(TimeSignal: TAVDDoubleDynArray; const NormFrequency: Double): TComplexDouble; overload;
function Goertzel(TimeSignal: PAVDSingleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexSingle; overload;
function Goertzel(TimeSignal: PAVDDoubleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexDouble; overload;

implementation

procedure DFT(realTime, imagTime, realFreq, imagFreq : TAVDSingleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Extended;
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

procedure InverseDFT(realTime, imagTime, realFreq, imagFreq : TAVDSingleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Extended;
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

procedure DFT(realTime, imagTime, realFreq, imagFreq : TAVDDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Extended;
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

procedure InverseDFT(realTime,imagTime,realFreq,imagFreq : TAVDDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Extended;
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




procedure DFT(realTime,realFreq,imagFreq : TAVDSingleDynArray);
var k, i, sz       : Integer;
    sr, si, sd, kc : Extended;
begin
 sz := Length(realTime);
 Assert(sz=Length(realFreq));
 Assert(sz=Length(imagFreq));

 sd := 1/sz;
 FillChar(realFreq[0],sz*SizeOf(Single),0);
 FillChar(imagFreq[0],sz*SizeOf(Single),0);

 for k := 0 to sz-1 do
  begin
   kc := 2*PI*k*sd;
   for i := 0 to sz-1 do
    begin
     GetSinCos(kc*i, sr, si);
     realFreq[k] := realFreq[k] + (realTime[i] * sr);
     imagFreq[k] := imagFreq[k] - (realTime[i] * si);
    end;
  end;
end;

procedure InverseDFT(realTime,realFreq,imagFreq : TAVDSingleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Extended;
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
     GetSinCos(kc*i, sr, si);
     realTime[k] := realTime[k] + (realFreq[i] * sr) + (imagFreq[i] * si);
     realTime[k] := realTime[k] - (realFreq[i] * si) + (imagFreq[i] * sr);
    end;

   realTime[k] := realTime[k] * sd;
  end;
end;

procedure DFT(realTime,realFreq,imagFreq : TAVDDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Extended;
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

procedure InverseDFT(realTime,realFreq,imagFreq : TAVDDoubleDynArray);
var
  k, i, sz       : Integer;
  sr, si, sd, kc : Extended;
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

function Goertzel(TimeSignal: TAVDSingleDynArray; const NormFrequency: Double): TComplexSingle;
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
   ComplexMultiplyInplace(Pos, Angle);
   result.Re := result.Re + Pos.Re * TimeSignal[i];
   result.Im := result.Im + Pos.Im * TimeSignal[i];
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

function Goertzel(TimeSignal: TAVDDoubleDynArray; const NormFrequency: Double): TComplexDouble;
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
   ComplexMultiplyInplace(Pos, Angle);
   result.Re := result.Re + Pos.Re * TimeSignal[i];
   result.Im := result.Im + Pos.Im * TimeSignal[i];
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

function Goertzel(TimeSignal: PAVDSingleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexSingle; overload;
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
 for i := 1 to Length - 1 do
  begin
   ComplexMultiplyInplace(Pos, Angle);
   result.Re := result.Re + Pos.Re * TimeSignal[i];
   result.Im := result.Im + Pos.Im * TimeSignal[i];
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

function Goertzel(TimeSignal: PAVDDoubleFixedArray; const Length: Integer; const NormFrequency: Double): TComplexDouble; overload;
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
 for i := 1 to Length - 1 do
  begin
   ComplexMultiplyInplace(Pos, Angle);
   result.Re := result.Re + Pos.Re * TimeSignal[i];
   result.Im := result.Im + Pos.Im * TimeSignal[i];
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

end.
