unit BMTestSingleU;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TBufferMathForm = class(TForm)
    TestCopyBtn: TButton;
    TestAddBtn: TButton;
    ResultMemo: TMemo;
    TestSubBtn: TButton;
    TestMulBtn: TButton;
    TestClearBtn: TButton;
    TestCopyBufBtn: TButton;
    TestMulAddBtn: TButton;
    TestAddMulBtn: TButton;
    TestAddScaledBtn: TButton;
    TestAddModulatedBtn: TButton;
    TestFindPeaksBtn: TButton;
    TestBufferSumsBtn: TButton;
    procedure TestCopyBtnClick(Sender: TObject);
    procedure TestAddBtnClick(Sender: TObject);
    procedure TestSubBtnClick(Sender: TObject);
    procedure TestMulBtnClick(Sender: TObject);
    procedure TestClearBtnClick(Sender: TObject);
    procedure TestCopyBufBtnClick(Sender: TObject);
    procedure TestMulAddBtnClick(Sender: TObject);
    procedure TestAddMulBtnClick(Sender: TObject);
    procedure TestAddScaledBtnClick(Sender: TObject);
    procedure TestAddModulatedBtnClick(Sender: TObject);
    procedure TestFindPeaksBtnClick(Sender: TObject);
    procedure TestBufferSumsBtnClick(Sender: TObject);
  end;


const
  TEST_DIM_1 = 20;
  TEST_DIM_2 = 512;
  TEST_RUNS = 20000;

var
  BufferMathForm: TBufferMathForm;

implementation

{$R *.dfm}

uses
  DAVDCommon, DAVDBufferMathAsm, DAVDBufferMathPascal, DVSTEffect;

resourcestring
  RCDone = 'DONE';

const
  CHorizontalRule = '---------------------------------------------------------------------------';
  CVerticalRule   = ' | ';

procedure GenerateTestBuffers(var Input1, Input2, Input3, Output: TAVDArrayOfSingleDynArray);
var
  i, j: Integer;
begin
  SetLength(Input1, TEST_DIM_1, TEST_DIM_2);
  SetLength(Input2, TEST_DIM_1, TEST_DIM_2);
  SetLength(Input3, TEST_DIM_1, TEST_DIM_2);
  SetLength(Output, TEST_DIM_1, TEST_DIM_2);
  for i := 0 to TEST_DIM_1 - 1 do
    for j := 0 to TEST_DIM_2 - 1 do
     begin
      if i mod 2 = 0 then
        Input1[i, j] := (j + 1)
      else
        Input1[i, j] := -1 * (j + 1);

      Input2[i, j] := i + 1;
      Input3[i, j] := 15;
      Output[i, j] := 5;
     end;
end;


procedure TBufferMathForm.TestCopyBtnClick(Sender: TObject);
var
  x    : PPSingle;
  i, j : Integer;
  n    : TAVDArrayOfSingleDynArray;
begin
 GetMem(x, 2 * SizeOf(PSingle));
 try
  for j := 0 to 1 do
   begin
    GetMem(x^, 200 * SizeOf(Single));
    try
     for i := 0 to 199 do
      begin
       x^^ := i + (j * 200);
       Inc(x^);
      end;
     for i := 0 to 199 do Dec(x^);
    finally
     Dispose(x^);
    end;
    Inc(x);
   end;
  for j := 0 to 1 do Dec(x);
 finally
  Dispose(x);
 end;

  SetLength(n, 2);
  SetLength(n[0], 200);
  SetLength(n[1], 200);

  Move(x^^, n[0, 0], 200 * SizeOf(Single));
  Inc(x);
  Move(x^^, n[1, 0], 200 * SizeOf(Single));
//  showmessage(FloatToStr(x^^));
  ShowMessage(FloatToStr(n[0, 0]));
  ShowMessage(FloatToStr(n[1, 45]));
end;



procedure TBufferMathForm.TestAddBtnClick(Sender: TObject);
var
  Input1, Input2,
  dummy, Output   : TAVDArrayOfSingleDynArray;
  i               : Integer;
  A, B, freq      : Int64;
begin
  ResultMemo.Clear;
  refresh;
  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddArrays(Input1, Input2, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms adding with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddArrays(Input1, Input2, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms adding with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddArrays(Input1, 5, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms adding Single value with Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddArrays(Input1, 5, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms adding Single value with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestSubBtnClick(Sender: TObject);
var
  Input1, Input2,
  dummy, Output   : TAVDArrayOfSingleDynArray;
  i               : Integer;
  A, B, freq      : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.SubArrays(Input1, Input2, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) +  ' ms subtracting with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.SubArrays(Input1, Input2, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms subtracting with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.SubArrays(Input1, 5, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms subtracting Single value with Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.SubArrays(Input1, 5, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms subtracting Single value with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestMulBtnClick(Sender: TObject);
var
  Input1, Input2,
  dummy, Output   : TAVDArrayOfSingleDynArray;
  i               : Integer;
  A, B, freq      : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.MulArrays(Input1, Input2, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.MulArrays(Input1, Input2, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.MulArrays(Input1, 5, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply Single value with Pascal,  Testvals: '
    +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.MulArrays(Input1, 5, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply Single value with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestClearBtnClick(Sender: TObject);
var
  dummy, Output : TAVDArrayOfSingleDynArray;
  i             : Integer;
  A, B, freq    : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(dummy, dummy, dummy, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.ClearArrays(Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms clear with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));


  GenerateTestBuffers(dummy, dummy, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.ClearArrays(Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms clear with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestCopyBufBtnClick(Sender: TObject);
var
  Input, dummy,
  Output        : TAVDArrayOfSingleDynArray;
  i             : Integer;
  A, B, freq    : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input, dummy, dummy, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.CopyArrays(Input, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms copy with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input, dummy, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathASM.CopyArrays(Input, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms copy with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;


procedure TBufferMathForm.TestMulAddBtnClick(Sender: TObject);
var
  Input1, Input2,
  Input3, Output  : TAVDArrayOfSingleDynArray;
  i               : Integer;
  A, B, freq      : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(Input1, Input2, Input3,
      Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply then add with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(Input1, Input2, Input3, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply then add with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(Input1, 5, Input3, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply Single value then add with Pascal,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(Input1, 5, Input3, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply Single value then add with ASM,  Testvals: '
    +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(Input1, Input2, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply then add Single value with Pascal,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(Input1, Input2, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply then add Single value with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(Input1, 5, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply Single value then add Single value with Pascal,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(Input1, 5, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms multiply Single value then add Single value with ASM,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestAddMulBtnClick(Sender: TObject);
var
  Input1, Input2,
  Input3, Output  : TAVDArrayOfSingleDynArray;
  i               : Integer;
  A, B, freq      : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(Input1, Input2, Input3,
      Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add then multiply with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(Input1, Input2, Input3, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add then multiply with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(Input1, 5, Input3, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add Single value then multiply with Pascal,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(Input1, 5, Input3, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add Single value then multiply with ASM,  Testvals: '
    +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(Input1, Input2, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add then multiply Single value with Pascal,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(Input1, Input2, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add then multiply Single value with ASM,  Testvals: '
    +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(Input1, 5, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add Single value then multiply Single value with Pascal,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(Input1, 5, 5, Output,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add Single value then multiply Single value with ASM,  Testvals: '
    + FloatToStr(Output[0, 0]) +
    ' | ' + FloatToStr(Output[0, TEST_DIM_2 - 1]) +
    ' | ' + FloatToStr(Output[TEST_DIM_1 - 1, 0]) +
    ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;



procedure TBufferMathForm.TestAddScaledBtnClick(Sender: TObject);
var
  Input1, Input2,
  dummy, Output   : TAVDArrayOfSingleDynArray;
  i               : Integer;
  A, B, freq      : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddScaledArrays(Input1, Input2, 5, 5,
      Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add scaled with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, dummy, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddScaledArrays(Input1, Input2, 5, 5,
      Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add scaled with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestAddModulatedBtnClick(Sender: TObject);
var
  Input1, Input2,
  Input3, Output   : TAVDArrayOfSingleDynArray;
  i                : Integer;
  A, B, freq       : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.AddModulatedArrays(Input1, Input2,
      Input3, Input3, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add modulated with pure Pascal,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.AddModulatedArrays(Input1, Input2, Input3,
      Input3, Output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms add modulated with ASM,  Testvals: ' +
    FloatToStr(Output[0, 0]) + ' | ' +
    FloatToStr(Output[0, TEST_DIM_2 - 1]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, 0]) + ' | ' +
    FloatToStr(Output[TEST_DIM_1 - 1, TEST_DIM_2 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestFindPeaksBtnClick(Sender: TObject);
var
  Input1, Input2,
  Input3, Output     : TAVDArrayOfSingleDynArray;
  MinPeaks, MaxPeaks : TAVDSingleDynArray;
  i                  : Integer;
  A, B, freq         : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, Input3, Output);
  SetLength(MinPeaks, TEST_DIM_1);
  SetLength(MaxPeaks, TEST_DIM_1);
  FillChar(MinPeaks[0], TEST_DIM_1 * SizeOf(Single), 0);
  FillChar(MaxPeaks[0], TEST_DIM_1 * SizeOf(Single), 0);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.GetPeaks(Input1, MinPeaks, MaxPeaks,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms find peaks with pure Pascal,  Testvals: ' +
    FloatToStr(MinPeaks[0]) + ' | ' +
    FloatToStr(MaxPeaks[0]) + ' | ' +
    FloatToStr(MinPeaks[TEST_DIM_1 - 1]) + ' | ' +
    FloatToStr(MaxPeaks[TEST_DIM_1 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);
  FillChar(MinPeaks[0], TEST_DIM_1 * SizeOf(Single), 0);
  FillChar(MaxPeaks[0], TEST_DIM_1 * SizeOf(Single), 0);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.GetPeaks(Input1, MinPeaks, MaxPeaks,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms find peaks ASM,  Testvals: ' +
    FloatToStr(MinPeaks[0]) + ' | ' +
    FloatToStr(MaxPeaks[0]) + ' | ' +
    FloatToStr(MinPeaks[TEST_DIM_1 - 1]) + ' | ' +
    FloatToStr(MaxPeaks[TEST_DIM_1 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

procedure TBufferMathForm.TestBufferSumsBtnClick(Sender: TObject);
var
  Input1, Input2,
  Input3, Output   : TAVDArrayOfSingleDynArray;
  MinSums, MaxSums : TAVDSingleDynArray;
  i                : Integer;
  A, B, freq       : Int64;
begin
  ResultMemo.Clear;
  Refresh;
  GenerateTestBuffers(Input1, Input2, Input3, Output);
  SetLength(MinSums, TEST_DIM_1);
  SetLength(MaxSums, TEST_DIM_1);
  FillChar(MinSums[0], TEST_DIM_1 * SizeOf(Single), 0);
  FillChar(MaxSums[0], TEST_DIM_1 * SizeOf(Single), 0);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathPascal.GetSums(Input1, MinSums, MaxSums,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms find sums with pure Pascal,  Testvals: ' +
    FloatToStr(MinSums[0]) + ' | ' +
    FloatToStr(MaxSums[0]) + ' | ' +
    FloatToStr(MinSums[TEST_DIM_1 - 1]) + ' | ' +
    FloatToStr(MaxSums[TEST_DIM_1 - 1]));

  GenerateTestBuffers(Input1, Input2, Input3, Output);
  FillChar(MinSums[0], TEST_DIM_1 * SizeOf(Single), 0);
  FillChar(MaxSums[0], TEST_DIM_1 * SizeOf(Single), 0);

  QueryPerformanceCounter(A);
  for i := 0 to TEST_RUNS do
    DAVDBufferMathAsm.GetSums(Input1, MinSums, MaxSums,
      TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B - A) * 1000) / freq, ffFixed, 15, 2) + ' ms find sums ASM,  Testvals: ' +
    FloatToStr(MinSums[0]) + ' | ' +
    FloatToStr(MaxSums[0]) + ' | ' +
    FloatToStr(MinSums[TEST_DIM_1 - 1]) + ' | ' +
    FloatToStr(MaxSums[TEST_DIM_1 - 1]));

  ResultMemo.Lines.Add(CHorizontalRule);
  ResultMemo.Lines.Add(RCDone);
end;

end.
