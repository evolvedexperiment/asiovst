unit DAVDBufferMathPascal;

interface

uses DAVDCommon;

{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}

{TYPE: TAVDSingleDynArray}
procedure AddArrays(const input1, input2, output: TAVDSingleDynArray; dim2: integer); overload;
procedure SubArrays(const from,   amount, output: TAVDSingleDynArray; dim2: integer); overload;
procedure MulArrays(const input1, input2, output: TAVDSingleDynArray; dim2: integer); overload;

procedure AddArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; dim2: integer); overload;
procedure SubArrays(const from:   TAVDSingleDynArray; const amount: single; const output: TAVDSingleDynArray; dim2: integer); overload;
procedure MulArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDSingleDynArray; const factor1, factor2: single; const output: TAVDSingleDynArray; dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDSingleDynArray; const output: TAVDSingleDynArray; dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: TAVDSingleDynArray; dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2: single; const summand, output: TAVDSingleDynArray; dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDSingleDynArray; const summand: single; const output: TAVDSingleDynArray; dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2, summand: single; const output: TAVDSingleDynArray; dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDSingleDynArray; dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2: single; const factor, output: TAVDSingleDynArray; dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDSingleDynArray; const factor: single; const output: TAVDSingleDynArray; dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2, factor: single; const output: TAVDSingleDynArray; dim2: integer); overload;


{TYPE: TArrayOfSingleDynArray}
procedure AddArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;

procedure AddArrays(const input1: TArrayOfSingleDynArray; const input2:single;
                    const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure SubArrays(const from:   TArrayOfSingleDynArray; const amount:single;
                    const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure MulArrays(const input1: TArrayOfSingleDynArray; const input2:single;
                    const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, output: TArrayOfSingleDynArray;
                       dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TArrayOfSingleDynArray; const summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TArrayOfSingleDynArray;
                       dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2: single;
                       const factor, output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TArrayOfSingleDynArray; const factor: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2, factor: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TArrayOfSingleDynArray; const factor1, factor2: single; output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;


procedure ClearArrays(const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure CopyArrays(const input, output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;

// when output has no dimensions use this:
procedure CreateArrayCopy(const input: TArrayOfSingleDynArray; out output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure CreateEmptyArray(out output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;



{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

{TYPE: TAVDDoubleDynArray}
procedure AddArrays(const input1, input2, output: TAVDDoubleDynArray; dim2: integer); overload;
procedure SubArrays(const from,   amount, output: TAVDDoubleDynArray; dim2: integer); overload;
procedure MulArrays(const input1, input2, output: TAVDDoubleDynArray; dim2: integer); overload;

procedure AddArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;
procedure SubArrays(const from:   TAVDDoubleDynArray; const amount: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;
procedure MulArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDDoubleDynArray; const factor1, factor2: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDDoubleDynArray; const output: TAVDDoubleDynArray; dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: TAVDDoubleDynArray; dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2: Double; const summand, output: TAVDDoubleDynArray; dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDDoubleDynArray; const summand: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2, summand: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDDoubleDynArray; dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2: Double; const factor, output: TAVDDoubleDynArray; dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDDoubleDynArray; const factor: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2, factor: Double; const output: TAVDDoubleDynArray; dim2: integer); overload;



{TYPE: TArrayOfDoubleDynArray}
procedure AddArrays(const input1, input2, output: TArrayOfDoubleDynArray; dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TArrayOfDoubleDynArray; dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TArrayOfDoubleDynArray; dim1, dim2: integer);  overload;

procedure AddArrays(const input1: TArrayOfDoubleDynArray; const input2:Double;
                    const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure SubArrays(const from:   TArrayOfDoubleDynArray; const amount:Double;
                    const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure MulArrays(const input1: TArrayOfDoubleDynArray; const input2:Double;
                    const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, output: TArrayOfDoubleDynArray;
                       dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TArrayOfDoubleDynArray; const factor2: Double;
                       const summand, output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TArrayOfDoubleDynArray; const summand: Double;
                       const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TArrayOfDoubleDynArray; const factor2, summand: Double;
                       const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TArrayOfDoubleDynArray;
                       dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TArrayOfDoubleDynArray; const summand2: Double;
                       const factor, output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TArrayOfDoubleDynArray; const factor: Double;
                       const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TArrayOfDoubleDynArray; const summand2, factor: Double;
                       const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TArrayOfDoubleDynArray; const factor1, factor2: Double; output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;


procedure ClearArrays(const output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure CopyArrays(const input, output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;

// when output has no dimensions use this:
procedure CreateArrayCopy(const input: TArrayOfDoubleDynArray; out output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;
procedure CreateEmptyArray(out output: TArrayOfDoubleDynArray; dim1, dim2: integer); overload;




implementation


{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}
procedure AddArrays(const input1, input2, output: TAVDSingleDynArray;dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    inp2: TAVDSingleDynArray absolute input2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] + inp2[j];
end;

procedure SubArrays(const from, amount, output: TAVDSingleDynArray; dim2: integer);
var inp1: TAVDSingleDynArray absolute from;
    inp2: TAVDSingleDynArray absolute amount;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] - inp2[j];
end;

procedure MulArrays(const input1, input2,output: TAVDSingleDynArray; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    inp2: TAVDSingleDynArray absolute input2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] * inp2[j];
end;

procedure AddArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] + input2;
end;

procedure SubArrays(const from:   TAVDSingleDynArray; const amount: single; const output: TAVDSingleDynArray; dim2: integer);
var inp1: TAVDSingleDynArray absolute from;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] - amount;
end;

procedure MulArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] * input2;
end;


procedure MulAddArrays(const factor1, factor2, summand, output: TAVDSingleDynArray; dim2: integer);
var fac1: TAVDSingleDynArray absolute factor1;
    fac2: TAVDSingleDynArray absolute factor2;
    summ: TAVDSingleDynArray absolute summand;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * fac2[j] + summ[j];
end;

procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2: single; const summand, output: TAVDSingleDynArray; dim2: integer);
var fac1: TAVDSingleDynArray absolute factor1;
    summ: TAVDSingleDynArray absolute summand;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * factor2 + summ[j];
end;

procedure MulAddArrays(const factor1, factor2: TAVDSingleDynArray; const summand: single; const output: TAVDSingleDynArray; dim2: integer);
var fac1: TAVDSingleDynArray absolute factor1;
    fac2: TAVDSingleDynArray absolute factor2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * fac2[j] + summand;
end;

procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2, summand: single; const output: TAVDSingleDynArray; dim2: integer);
var fac1: TAVDSingleDynArray absolute factor1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * factor2 + summand;
end;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDSingleDynArray; dim2: integer);
var sum1: TAVDSingleDynArray absolute summand1;
    sum2: TAVDSingleDynArray absolute summand2;
    fact: TAVDSingleDynArray absolute factor;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + sum2[j]) * fact[j];
end;

procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2: single; const factor, output: TAVDSingleDynArray; dim2: integer);
var sum1: TAVDSingleDynArray absolute summand1;
    fact: TAVDSingleDynArray absolute factor;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + summand2) * fact[j];
end;

procedure AddMulArrays(const summand1, summand2: TAVDSingleDynArray; const factor: single; const output: TAVDSingleDynArray; dim2: integer);
var sum1: TAVDSingleDynArray absolute summand1;
    sum2: TAVDSingleDynArray absolute summand2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + sum2[j]) * factor;
end;

procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2, factor: single; const output: TAVDSingleDynArray; dim2: integer);
var sum1: TAVDSingleDynArray absolute summand1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + summand2) * factor;
end;




procedure AddScaledArrays(const input1, input2: TAVDSingleDynArray; const factor1, factor2: single; const output: TAVDSingleDynArray; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    inp2: TAVDSingleDynArray absolute input2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j]*factor1 + inp2[j]*factor2;
end;



procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDSingleDynArray; const output: TAVDSingleDynArray; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    inp2: TAVDSingleDynArray absolute input2;
    env1: TAVDSingleDynArray absolute envelope1;
    env2: TAVDSingleDynArray absolute envelope2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j]*env1[j] + inp2[j]*env2[j];
end;





procedure AddArrays(const input1,input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2[i], output[i], dim2);
end;

procedure SubArrays(const from, amount, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount[i], output[i], dim2);
end;

procedure MulArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2[i], output[i], dim2);
end;





procedure AddArrays(const input1: TArrayOfSingleDynArray; const input2: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2, output[i], dim2);
end;


procedure SubArrays(const from: TArrayOfSingleDynArray; const amount: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount, output[i], dim2);
end;

procedure MulArrays(const input1: TArrayOfSingleDynArray; const input2: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2, output[i], dim2);
end;





procedure MulAddArrays(const factor1, factor2, summand, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TArrayOfSingleDynArray; const summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand, output[i], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand, output[i], dim2);
end;




procedure AddMulArrays(const summand1, summand2, factor, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2: single; const factor, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TArrayOfSingleDynArray; const factor: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor, output[i], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2, factor: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor, output[i], dim2);
end;



procedure AddScaledArrays(const input1, input2: TArrayOfSingleDynArray; const factor1, factor2: single; output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(input1[i], input2[i], factor1, factor2, output[i], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(input1[i], input2[i], envelope1[i], envelope2[i], output[i], dim2);
end;




procedure ClearArrays(const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do Fillchar(output[i,0], dim2 * SizeOf(Single),0);
end;




procedure CopyArrays(const input, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do move(input[i,0], output[i,0], dim2 * SizeOf(Single));
end;


procedure CreateArrayCopy(const input: TArrayOfSingleDynArray; out output: TArrayOfSingleDynArray; dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  CopyArrays(input, output, dim1, dim2);
end;

procedure CreateEmptyArray(out output: TArrayOfSingleDynArray; dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  ClearArrays(output, dim1, dim2);
end;








{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

procedure AddArrays(const input1, input2, output: TAVDDoubleDynArray;dim2: integer);
var inp1: TAVDDoubleDynArray absolute input1;
    inp2: TAVDDoubleDynArray absolute input2;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] + inp2[j];
end;

procedure SubArrays(const from, amount, output: TAVDDoubleDynArray; dim2: integer);
var inp1: TAVDDoubleDynArray absolute from;
    inp2: TAVDDoubleDynArray absolute amount;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] - inp2[j];
end;

procedure MulArrays(const input1, input2,output: TAVDDoubleDynArray; dim2: integer);
var inp1: TAVDDoubleDynArray absolute input1;
    inp2: TAVDDoubleDynArray absolute input2;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] * inp2[j];
end;

procedure AddArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; dim2: integer);
var inp1: TAVDDoubleDynArray absolute input1;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] + input2;
end;

procedure SubArrays(const from:   TAVDDoubleDynArray; const amount: Double; const output: TAVDDoubleDynArray; dim2: integer);
var inp1: TAVDDoubleDynArray absolute from;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] - amount;
end;

procedure MulArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; dim2: integer);
var inp1: TAVDDoubleDynArray absolute input1;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] * input2;
end;


procedure MulAddArrays(const factor1, factor2, summand, output: TAVDDoubleDynArray; dim2: integer);
var fac1: TAVDDoubleDynArray absolute factor1;
    fac2: TAVDDoubleDynArray absolute factor2;
    summ: TAVDDoubleDynArray absolute summand;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * fac2[j] + summ[j];
end;

procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2: Double; const summand, output: TAVDDoubleDynArray; dim2: integer);
var fac1: TAVDDoubleDynArray absolute factor1;
    summ: TAVDDoubleDynArray absolute summand;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * factor2 + summ[j];
end;

procedure MulAddArrays(const factor1, factor2: TAVDDoubleDynArray; const summand: Double; const output: TAVDDoubleDynArray; dim2: integer);
var fac1: TAVDDoubleDynArray absolute factor1;
    fac2: TAVDDoubleDynArray absolute factor2;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * fac2[j] + summand;
end;

procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2, summand: Double; const output: TAVDDoubleDynArray; dim2: integer);
var fac1: TAVDDoubleDynArray absolute factor1;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * factor2 + summand;
end;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDDoubleDynArray; dim2: integer);
var sum1: TAVDDoubleDynArray absolute summand1;
    sum2: TAVDDoubleDynArray absolute summand2;
    fact: TAVDDoubleDynArray absolute factor;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + sum2[j]) * fact[j];
end;

procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2: Double; const factor, output: TAVDDoubleDynArray; dim2: integer);
var sum1: TAVDDoubleDynArray absolute summand1;
    fact: TAVDDoubleDynArray absolute factor;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + summand2) * fact[j];
end;

procedure AddMulArrays(const summand1, summand2: TAVDDoubleDynArray; const factor: Double; const output: TAVDDoubleDynArray; dim2: integer);
var sum1: TAVDDoubleDynArray absolute summand1;
    sum2: TAVDDoubleDynArray absolute summand2;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + sum2[j]) * factor;
end;

procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2, factor: Double; const output: TAVDDoubleDynArray; dim2: integer);
var sum1: TAVDDoubleDynArray absolute summand1;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + summand2) * factor;
end;




procedure AddScaledArrays(const input1, input2: TAVDDoubleDynArray; const factor1, factor2: Double; const output: TAVDDoubleDynArray; dim2: integer);
var inp1: TAVDDoubleDynArray absolute input1;
    inp2: TAVDDoubleDynArray absolute input2;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j]*factor1 + inp2[j]*factor2;
end;



procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDDoubleDynArray; const output: TAVDDoubleDynArray; dim2: integer);
var inp1: TAVDDoubleDynArray absolute input1;
    inp2: TAVDDoubleDynArray absolute input2;
    env1: TAVDDoubleDynArray absolute envelope1;
    env2: TAVDDoubleDynArray absolute envelope2;
    outp: TAVDDoubleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j]*env1[j] + inp2[j]*env2[j];
end;





procedure AddArrays(const input1,input2, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2[i], output[i], dim2);
end;

procedure SubArrays(const from, amount, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount[i], output[i], dim2);
end;

procedure MulArrays(const input1, input2, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2[i], output[i], dim2);
end;





procedure AddArrays(const input1: TArrayOfDoubleDynArray; const input2: Double; const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2, output[i], dim2);
end;


procedure SubArrays(const from: TArrayOfDoubleDynArray; const amount: Double; const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount, output[i], dim2);
end;

procedure MulArrays(const input1: TArrayOfDoubleDynArray; const input2: Double; const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2, output[i], dim2);
end;





procedure MulAddArrays(const factor1, factor2, summand, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfDoubleDynArray; const factor2: Double;
                       const summand, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TArrayOfDoubleDynArray; const summand: Double;
                       const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand, output[i], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfDoubleDynArray; const factor2, summand: Double;
                       const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand, output[i], dim2);
end;




procedure AddMulArrays(const summand1, summand2, factor, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfDoubleDynArray; const summand2: Double; const factor, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TArrayOfDoubleDynArray; const factor: Double; const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor, output[i], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfDoubleDynArray; const summand2, factor: Double; const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor, output[i], dim2);
end;



procedure AddScaledArrays(const input1, input2: TArrayOfDoubleDynArray; const factor1, factor2: Double; output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(input1[i], input2[i], factor1, factor2, output[i], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(input1[i], input2[i], envelope1[i], envelope2[i], output[i], dim2);
end;




procedure ClearArrays(const output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do Fillchar(output[i,0], dim2 * SizeOf(Double),0);
end;




procedure CopyArrays(const input, output: TArrayOfDoubleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do move(input[i,0], output[i,0], dim2 * SizeOf(Double))
end;


procedure CreateArrayCopy(const input: TArrayOfDoubleDynArray; out output: TArrayOfDoubleDynArray; dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  CopyArrays(input, output, dim1, dim2);
end;

procedure CreateEmptyArray(out output: TArrayOfDoubleDynArray; dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  ClearArrays(output, dim1, dim2);
end;




end.
