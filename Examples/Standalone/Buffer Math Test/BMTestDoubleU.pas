unit BMTestDoubleU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;


const TEST_DIM_1 = 20;
      TEST_DIM_2 = 512;
      TEST_RUNS  = 20000;

var
  BufferMathForm: TBufferMathForm;

implementation

{$R *.dfm}

uses DAVDCommon, DAVDBufferMathAsm, DAVDBufferMathPascal, DVSTEffect;

procedure GenerateTestBuffers(var input1,input2,input3, output: TArrayOfDoubleDynArray);
var i,j: integer;
begin
  setlength(input1, TEST_DIM_1, TEST_DIM_2);
  setlength(input2, TEST_DIM_1, TEST_DIM_2);
  setlength(input3, TEST_DIM_1, TEST_DIM_2);
  setlength(output, TEST_DIM_1, TEST_DIM_2);
  for i:=0 to TEST_DIM_1-1 do for j:=0 to TEST_DIM_2-1 do
  begin
    input1[i,j] := j+1;
    input2[i,j] := i+1;
    input3[i,j] := 15;
    output[i,j] := 5;
  end;
end;










procedure TBufferMathForm.TestCopyBtnClick(Sender: TObject);
var x: PPDouble; i,j: integer; n: TArrayOfDoubleDynArray;
begin
  getmem(x, 2*sizeof(PDouble));
  for j:=0 to 1 do
  begin
    getmem(x^, 200*sizeof(Double));
    for i:=0 to 199 do begin x^^:=i+(j*200); inc(x^); end;
    for i:=0 to 199 do dec(x^);
    inc(x);
  end;
  for j:=0 to 1 do dec(x);

  setlength(n,2);
  setlength(n[0],200);
  setlength(n[1],200);

  move(x^^,n[0,0],200*sizeof(double));
  inc(x);
  move(x^^,n[1,0],200*sizeof(double));
//  showmessage(floattostr(x^^));
  showmessage(floattostr(n[0,0]));
  showmessage(floattostr(n[1,45]));
end;







procedure TBufferMathForm.TestAddBtnClick(Sender: TObject);
var input1,input2,dummy, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin       
  ResultMemo.clear; refresh;
  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddArrays(input1, input2, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms adding with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddArrays(input1, input2, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms adding with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddArrays(input1, 5,output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms adding double value with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddArrays(input1, 5,output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms adding double value with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));    
  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;

procedure TBufferMathForm.TestSubBtnClick(Sender: TObject);
var input1,input2,dummy, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin   
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.SubArrays(input1, input2, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms subtracting with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.SubArrays(input1, input2, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms subtracting with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.SubArrays(input1, 5,output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms subtracting double value with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.SubArrays(input1, 5,output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms subtracting double value with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1])); 
  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;

procedure TBufferMathForm.TestMulBtnClick(Sender: TObject);
var input1,input2,dummy, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin     
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.MulArrays(input1, input2, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.MulArrays(input1, input2, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));  

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.MulArrays(input1, 5,output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply double value with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.MulArrays(input1, 5,output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply double value with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1])); 
  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;

procedure TBufferMathForm.TestClearBtnClick(Sender: TObject);
var dummy, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin  
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(dummy, dummy, dummy, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.ClearArrays(output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms clear with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));


  GenerateTestBuffers(dummy, dummy, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.ClearArrays(output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms clear with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1])); 
  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;  

procedure TBufferMathForm.TestCopyBufBtnClick(Sender: TObject);
var input, dummy, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin     
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(input, dummy, dummy, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.CopyArrays(input, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms copy with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input, dummy, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathASM.CopyArrays(input, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms copy with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));
  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;


procedure TBufferMathForm.TestMulAddBtnClick(Sender: TObject);
var input1,input2,input3, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin     
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(input1, input2, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply then add with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(input1, input2, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply then add with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(input1, 5, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply double value then add with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(input1, 5, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply double value then add with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(input1, input2, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply then add double value with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(input1, input2, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply then add double value with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.MulAddArrays(input1, 5, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply double value then add double value with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.MulAddArrays(input1, 5, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms multiply double value then add double value with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));   
  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;

procedure TBufferMathForm.TestAddMulBtnClick(Sender: TObject);
var input1,input2,input3, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(input1, input2, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add then multiply with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(input1, input2, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add then multiply with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(input1, 5, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add double value then multiply with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(input1, 5, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add double value then multiply with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(input1, input2, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add then multiply double value with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(input1, input2, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add then multiply double value with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddMulArrays(input1, 5, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add double value then multiply double value with Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddMulArrays(input1, 5, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add double value then multiply double value with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));
  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;



procedure TBufferMathForm.TestAddScaledBtnClick(Sender: TObject);
var input1,input2,dummy, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddScaledArrays(input1, input2, 5, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add scaled with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, dummy, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddScaledArrays(input1, input2, 5, 5, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add scaled with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;

procedure TBufferMathForm.TestAddModulatedBtnClick(Sender: TObject);

var input1,input2,input3, output: TArrayOfDoubleDynArray;
    i: integer;
    A,B, freq: Int64;
begin
  ResultMemo.clear; Refresh;
  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathPascal.AddModulatedArrays(input1, input2, input3, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add modulated with pure Pascal,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  GenerateTestBuffers(input1,input2, input3, output);

  QueryPerformanceCounter(A);
  for i:=0 to TEST_RUNS do
    DAVDBufferMathAsm.AddModulatedArrays(input1, input2, input3, input3, output, TEST_DIM_1, TEST_DIM_2);

  QueryPerformanceCounter(B);
  ResultMemo.Lines.Add(FloatToStrF(((B-A)*1000)/freq, ffFixed,15,2)+ ' ms add modulated with ASM,  Testvals: '
                           + floattostr(output[0,0]) + ' | '
                           + floattostr(output[0,TEST_DIM_2-1]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,0]) + ' | '
                           + floattostr(output[TEST_DIM_1-1,TEST_DIM_2-1]));

  ResultMemo.Lines.Add('---------------------------------------------------------------------------');
  ResultMemo.Lines.Add('DONE');
end;

end.
