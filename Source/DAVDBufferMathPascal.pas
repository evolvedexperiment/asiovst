unit DAVDBufferMathPascal;

interface

uses DAVDCommon;

{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}

{TYPE: TAVDSingleDynArray}
procedure AddArrays(const input1, input2, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1, input2, output: TAVDSingleDynArray; const dim2: integer); overload;

procedure AddArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TAVDSingleDynArray; const amount: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDSingleDynArray; const factor1, factor2: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDSingleDynArray; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2: single; const summand, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDSingleDynArray; const summand: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2, summand: single; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2: single; const factor, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDSingleDynArray; const factor: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2, factor: single; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure GetPeaks(const input: TAVDSingleDynArray; out outputmin, outputmax: Single; const dim2: integer); overload;
procedure GetSums(const input: TAVDSingleDynArray; out outputmin, outputmax: Single; const dim2: integer); overload;

{TYPE: TAVDArrayOfSingleDynArray}
procedure AddArrays(const input1, input2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const input1: TAVDArrayOfSingleDynArray; const input2:single;
                    const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TAVDArrayOfSingleDynArray; const amount:single;
                    const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const input1: TAVDArrayOfSingleDynArray; const input2:single;
                    const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDArrayOfSingleDynArray; const summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2: single;
                       const factor, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDArrayOfSingleDynArray; const factor: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2, factor: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDArrayOfSingleDynArray; const factor1, factor2: single; output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;


procedure ClearArrays(const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const input, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

// when output has no dimensions use this:
procedure CreateArrayCopy(const input: TAVDArrayOfSingleDynArray; out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure GetPeaks(const input: TAVDArrayOfSingleDynArray; out outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const input: TAVDArrayOfSingleDynArray; out outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer); overload;

{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

{TYPE: TAVDDoubleDynArray}
procedure AddArrays(const input1, input2, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1, input2, output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure AddArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TAVDDoubleDynArray; const amount: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDDoubleDynArray; const factor1, factor2: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDDoubleDynArray; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2: Double; const summand, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDDoubleDynArray; const summand: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2, summand: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2: Double; const factor, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDDoubleDynArray; const factor: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2, factor: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure GetPeaks(const input: TAVDDoubleDynArray; out outputmin, outputmax: Double; const dim2: integer); overload;
procedure GetSums(const input: TAVDDoubleDynArray; out outputmin, outputmax: Double; const dim2: integer); overload;


{TYPE: TAVDArrayOfDoubleDynArray}
procedure AddArrays(const input1, input2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const input1: TAVDArrayOfDoubleDynArray; const input2:Double;
                    const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TAVDArrayOfDoubleDynArray; const amount:Double;
                    const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const input1: TAVDArrayOfDoubleDynArray; const input2:Double;
                    const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2: Double;
                       const summand, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDArrayOfDoubleDynArray; const summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2, summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2: Double;
                       const factor, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDArrayOfDoubleDynArray; const factor: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2, factor: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDArrayOfDoubleDynArray; const factor1, factor2: Double; output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure ClearArrays(const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const input, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

// when output has no dimensions use this:
procedure CreateArrayCopy(const input: TAVDArrayOfDoubleDynArray; out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure SetDimensions(var output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);

procedure GetPeaks(const input: TAVDArrayOfDoubleDynArray; out outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const input: TAVDArrayOfDoubleDynArray; out outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer); overload;

implementation


{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}
procedure AddArrays(const input1, input2, output: TAVDSingleDynArray;const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] + input2[j];
end;

procedure SubArrays(const from, amount, output: TAVDSingleDynArray; const dim2: integer);
var input1: TAVDSingleDynArray absolute from;
    input2: TAVDSingleDynArray absolute amount;
    outp: TAVDSingleDynArray absolute output;
    j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] - input2[j];
end;

procedure MulArrays(const input1, input2,output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] * input2[j];
end;

procedure AddArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] + input2;
end;

procedure SubArrays(const from:   TAVDSingleDynArray; const amount: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := from[j] - amount;
end;

procedure MulArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] * input2;
end;


procedure MulAddArrays(const factor1, factor2, summand, output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2[j] + summand[j];
end;

procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2: single; const summand, output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2 + summand[j];
end;

procedure MulAddArrays(const factor1, factor2: TAVDSingleDynArray; const summand: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2[j] + summand;
end;

procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2, summand: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2 + summand;
end;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2[j]) * factor[j];
end;

procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2: single; const factor, output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2) * factor[j];
end;

procedure AddMulArrays(const summand1, summand2: TAVDSingleDynArray; const factor: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2[j]) * factor;
end;

procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2, factor: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2) * factor;
end;




procedure AddScaledArrays(const input1, input2: TAVDSingleDynArray; const factor1, factor2: single; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j]*factor1 + input2[j]*factor2;
end;



procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDSingleDynArray; const output: TAVDSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j]*envelope1[j] + input2[j]*envelope2[j];
end;





procedure AddArrays(const input1,input2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2[i], output[i], dim2);
end;

procedure SubArrays(const from, amount, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount[i], output[i], dim2);
end;

procedure MulArrays(const input1, input2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2[i], output[i], dim2);
end;





procedure AddArrays(const input1: TAVDArrayOfSingleDynArray; const input2: single; const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2, output[i], dim2);
end;


procedure SubArrays(const from: TAVDArrayOfSingleDynArray; const amount: single; const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount, output[i], dim2);
end;

procedure MulArrays(const input1: TAVDArrayOfSingleDynArray; const input2: single; const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2, output[i], dim2);
end;





procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TAVDArrayOfSingleDynArray; const summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand, output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand, output[i], dim2);
end;




procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2: single; const factor, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TAVDArrayOfSingleDynArray; const factor: single; const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor, output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2, factor: single; const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor, output[i], dim2);
end;



procedure AddScaledArrays(const input1, input2: TAVDArrayOfSingleDynArray; const factor1, factor2: single; output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(input1[i], input2[i], factor1, factor2, output[i], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(input1[i], input2[i], envelope1[i], envelope2[i], output[i], dim2);
end;




procedure ClearArrays(const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do Fillchar(output[i,0], dim2 * SizeOf(Single),0);
end;




procedure CopyArrays(const input, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do move(input[i,0], output[i,0], dim2 * SizeOf(Single));
end;


procedure CreateArrayCopy(const input: TAVDArrayOfSingleDynArray; out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  CopyArrays(input, output, dim1, dim2);
end;

procedure CreateEmptyArray(out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  ClearArrays(output, dim1, dim2);
end;








{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

procedure AddArrays(const input1, input2, output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] + input2[j];
end;

procedure SubArrays(const from, amount, output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := from[j] - amount[j];
end;

procedure MulArrays(const input1, input2,output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] * input2[j];
end;

procedure AddArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] + input2;
end;

procedure SubArrays(const from:   TAVDDoubleDynArray; const amount: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := from[j] - amount;
end;

procedure MulArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j] * input2;
end;


procedure MulAddArrays(const factor1, factor2, summand, output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2[j] + summand[j];
end;

procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2: Double; const summand, output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2 + summand[j];
end;

procedure MulAddArrays(const factor1, factor2: TAVDDoubleDynArray; const summand: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2[j] + summand;
end;

procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2, summand: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := factor1[j] * factor2 + summand;
end;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2[j]) * factor[j];
end;

procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2: Double; const factor, output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2) * fact[j];
end;

procedure AddMulArrays(const summand1, summand2: TAVDDoubleDynArray; const factor: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2[j]) * factor;
end;

procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2, factor: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := (summand1[j] + summand2) * factor;
end;




procedure AddScaledArrays(const input1, input2: TAVDDoubleDynArray; const factor1, factor2: Double; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j]*factor1 + input2[j]*factor2;
end;



procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDDoubleDynArray; const output: TAVDDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    output[j] := input1[j]*envelope1[j] + input2[j]*envelope2[j];
end;





procedure AddArrays(const input1,input2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2[i], output[i], dim2);
end;

procedure SubArrays(const from, amount, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount[i], output[i], dim2);
end;

procedure MulArrays(const input1, input2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2[i], output[i], dim2);
end;





procedure AddArrays(const input1: TAVDArrayOfDoubleDynArray; const input2: Double; const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(input1[i], input2, output[i], dim2);
end;


procedure SubArrays(const from: TAVDArrayOfDoubleDynArray; const amount: Double; const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount, output[i], dim2);
end;

procedure MulArrays(const input1: TAVDArrayOfDoubleDynArray; const input2: Double; const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(input1[i], input2, output[i], dim2);
end;





procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2: Double;
                       const summand, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TAVDArrayOfDoubleDynArray; const summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand, output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2, summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand, output[i], dim2);
end;




procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2: Double; const factor, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TAVDArrayOfDoubleDynArray; const factor: Double; const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor, output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2, factor: Double; const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor, output[i], dim2);
end;



procedure AddScaledArrays(const input1, input2: TAVDArrayOfDoubleDynArray; const factor1, factor2: Double; output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(input1[i], input2[i], factor1, factor2, output[i], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(input1[i], input2[i], envelope1[i], envelope2[i], output[i], dim2);
end;




procedure ClearArrays(const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do Fillchar(output[i,0], dim2 * SizeOf(Double),0);
end;




procedure CopyArrays(const input, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do move(input[i,0], output[i,0], dim2 * SizeOf(Double))
end;

procedure CreateArrayCopy(const input: TAVDArrayOfDoubleDynArray; out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  CopyArrays(input, output, dim1, dim2);
end;

procedure CreateEmptyArray(out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  ClearArrays(output, dim1, dim2);
end;   

procedure SetDimensions(var output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  setlength(output, dim1, dim2);
end;

procedure GetPeaks(const input: TAVDSingleDynArray; out outputmin, outputmax: Single; const dim2: integer);
begin

end;

procedure GetSums(const input: TAVDSingleDynArray; out outputmin, outputmax: Single; const dim2: integer);
begin

end;
procedure GetPeaks(const input: TAVDDoubleDynArray; out outputmin, outputmax: Double; const dim2: integer);
begin

end;
procedure GetSums(const input: TAVDDoubleDynArray; out outputmin, outputmax: Double; const dim2: integer);
begin

end;

procedure GetPeaks(const input: TAVDArrayOfSingleDynArray; out outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer);
begin

end;
procedure GetSums(const input: TAVDArrayOfSingleDynArray; out outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer);
begin

end;
procedure GetPeaks(const input: TAVDArrayOfDoubleDynArray; out outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer);
begin

end;
procedure GetSums(const input: TAVDArrayOfDoubleDynArray; out outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer);
begin

end;


end.
