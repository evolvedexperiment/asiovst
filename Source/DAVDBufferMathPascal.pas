unit DAVDBufferMathPascal;

interface

uses DAVDCommon;

procedure AddArrays(const input1, input2, output: PSingle; dim2: integer); overload;
procedure SubArrays(const from,   amount, output: PSingle; dim2: integer); overload;
procedure MulArrays(const input1, input2, output: PSingle; dim2: integer); overload;

procedure AddArrays(const input1: PSingle; const input2: single; const output: PSingle; dim2: integer); overload;
procedure SubArrays(const from:   PSingle; const amount: single; const output: PSingle; dim2: integer); overload;
procedure MulArrays(const input1: PSingle; const input2: single; const output: PSingle; dim2: integer); overload;

procedure AddArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;

procedure AddScaledArrays(const input1, input2: PSingle; const factor1, factor2: single; const output: PSingle; dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: PSingle; const output: PSingle; dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: PSingle; dim2: integer); overload;
procedure MulAddArrays(const factor1: PSingle; const factor2: single; const summand, output: PSingle; dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: PSingle; const summand: single; const output: PSingle; dim2: integer); overload;
procedure MulAddArrays(const factor1: PSingle; const factor2, summand: single; const output: PSingle; dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: PSingle; dim2: integer); overload;
procedure AddMulArrays(const summand1: PSingle; const summand2: single; const factor, output: PSingle; dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: PSingle; const factor: single; const output: PSingle; dim2: integer); overload;
procedure AddMulArrays(const summand1: PSingle; const summand2, factor: single; const output: PSingle; dim2: integer); overload;

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


procedure ClearArrays(const output: TArrayOfSingleDynArray; dim1, dim2: integer);
procedure CopyArrays(const input, output: TArrayOfSingleDynArray; dim1, dim2: integer);

implementation


procedure AddArrays(const input1, input2, output: PSingle;dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    inp2: TAVDSingleDynArray absolute input2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] + inp2[j];
end;

procedure SubArrays(const from, amount, output: PSingle; dim2: integer);
var inp1: TAVDSingleDynArray absolute from;
    inp2: TAVDSingleDynArray absolute amount;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] - inp2[j];
end;

procedure MulArrays(const input1, input2,output: PSingle; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    inp2: TAVDSingleDynArray absolute input2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] * inp2[j];
end;

procedure AddArrays(const input1: PSingle; const input2: single; const output: PSingle; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] + input2;
end;

procedure SubArrays(const from:   PSingle; const amount: single; const output: PSingle; dim2: integer);
var inp1: TAVDSingleDynArray absolute from;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] - amount;
end;

procedure MulArrays(const input1: PSingle; const input2: single; const output: PSingle; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j] * input2;
end;


procedure MulAddArrays(const factor1, factor2, summand, output: PSingle; dim2: integer);
var fac1: TAVDSingleDynArray absolute factor1;
    fac2: TAVDSingleDynArray absolute factor2;
    summ: TAVDSingleDynArray absolute summand;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * fac2[j] + summ[j];
end;

procedure MulAddArrays(const factor1: PSingle; const factor2: single; const summand, output: PSingle; dim2: integer);
var fac1: TAVDSingleDynArray absolute factor1;
    summ: TAVDSingleDynArray absolute summand;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * factor2 + summ[j];
end;

procedure MulAddArrays(const factor1, factor2: PSingle; const summand: single; const output: PSingle; dim2: integer);
var fac1: TAVDSingleDynArray absolute factor1;
    fac2: TAVDSingleDynArray absolute factor2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * fac2[j] + summand;
end;

procedure MulAddArrays(const factor1: PSingle; const factor2, summand: single; const output: PSingle; dim2: integer); 
var fac1: TAVDSingleDynArray absolute factor1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := fac1[j] * factor2 + summand;
end;

procedure AddMulArrays(const summand1, summand2, factor, output: PSingle; dim2: integer);
var sum1: TAVDSingleDynArray absolute summand1;
    sum2: TAVDSingleDynArray absolute summand2;
    fact: TAVDSingleDynArray absolute factor;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + sum2[j]) * fact[j];
end;

procedure AddMulArrays(const summand1: PSingle; const summand2: single; const factor, output: PSingle; dim2: integer); 
var sum1: TAVDSingleDynArray absolute summand1;
    fact: TAVDSingleDynArray absolute factor;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + summand2) * fact[j];
end;

procedure AddMulArrays(const summand1, summand2: PSingle; const factor: single; const output: PSingle; dim2: integer);
var sum1: TAVDSingleDynArray absolute summand1;
    sum2: TAVDSingleDynArray absolute summand2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + sum2[j]) * factor;
end;

procedure AddMulArrays(const summand1: PSingle; const summand2, factor: single; const output: PSingle; dim2: integer);
var sum1: TAVDSingleDynArray absolute summand1;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := (sum1[j] + summand2) * factor;
end;




procedure AddScaledArrays(const input1, input2: PSingle; const factor1, factor2: single; const output: PSingle; dim2: integer);
var inp1: TAVDSingleDynArray absolute input1;
    inp2: TAVDSingleDynArray absolute input2;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    outp[j] := inp1[j]*factor1 + inp2[j]*factor2;
end;



procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: PSingle; const output: PSingle; dim2: integer);
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
  for i:=0 to dim1-1 do AddArrays(@input1[i,0], @input2[i,0], @output[i,0], dim2);
end;

procedure SubArrays(const from, amount, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(@from[i,0], @amount[i,0], @output[i,0], dim2);
end;

procedure MulArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(@input1[i,0], @input2[i,0], @output[i,0], dim2);
end;





procedure AddArrays(const input1: TArrayOfSingleDynArray; const input2: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(@input1[i,0], input2, @output[i,0], dim2);
end;


procedure SubArrays(const from: TArrayOfSingleDynArray; const amount: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(@from[i,0], amount, @output[i,0], dim2);
end;

procedure MulArrays(const input1: TArrayOfSingleDynArray; const input2: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(@input1[i,0], input2, @output[i,0], dim2);
end;





procedure MulAddArrays(const factor1, factor2, summand, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], @factor2[i,0], @summand[i,0], @output[i,0], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], factor2, @summand[i,0], @output[i,0], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TArrayOfSingleDynArray; const summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], @factor2[i,0], summand, @output[i,0], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], factor2, summand, @output[i,0], dim2);
end;




procedure AddMulArrays(const summand1, summand2, factor, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], @summand2[i,0], @factor[i,0], @output[i,0], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2: single; const factor, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], summand2, @factor[i,0], @output[i,0], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TArrayOfSingleDynArray; const factor: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], @summand2[i,0], factor, @output[i,0], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2, factor: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], summand2, factor, @output[i,0], dim2);
end;



procedure AddScaledArrays(const input1, input2: TArrayOfSingleDynArray; const factor1, factor2: single; output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(@input1[i,0], @input2[i,0], factor1, factor2, @output[i,0], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(@input1[i,0], @input2[i,0], @envelope1[i,0], @envelope2[i,0], @output[i,0], dim2);
end;




procedure ClearArrays(const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do Fillchar(output[i,0], dim2*4,0);
end;




procedure CopyArrays(const input, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do move(input[i,0], output[i,0], dim2*4)//copy(input[i], 0, dim2*4);
end;


end.
