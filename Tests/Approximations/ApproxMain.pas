unit ApproxMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFmApproximationBenchmark = class(TForm)
    Memo: TMemo;
    procedure MemoClick(Sender: TObject);
  end;

var
  FmApproximationBenchmark: TFmApproximationBenchmark;

implementation

uses
  Math, DAV_Common, DAV_Approximations;

{$R *.dfm}

procedure TFmApproximationBenchmark.MemoClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Cos() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Cos(i * Temp);
   Cos(i * Temp);
   Cos(i * Temp);
   Cos(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Cos(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCos3Term(i * Temp);
   FastCos3Term(i * Temp);
   FastCos3Term(i * Temp);
   FastCos3Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCos3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCos4Term(i * Temp);
   FastCos4Term(i * Temp);
   FastCos4Term(i * Temp);
   FastCos4Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCos4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCos5Term(i * Temp);
   FastCos5Term(i * Temp);
   FastCos5Term(i * Temp);
   FastCos5Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCos5Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCos6Term(i * Temp);
   FastCos6Term(i * Temp);
   FastCos6Term(i * Temp);
   FastCos6Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCos6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Tan(i * Temp);
   Tan(i * Temp);
   Tan(i * Temp);
   Tan(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Tan(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTan3Term(i * Temp);
   FastTan3Term(i * Temp);
   FastTan3Term(i * Temp);
   FastTan3Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTan3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTan4Term(i * Temp);
   FastTan4Term(i * Temp);
   FastTan4Term(i * Temp);
   FastTan4Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTan4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

  QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTan6Term(i * Temp);
   FastTan6Term(i * Temp);
   FastTan6Term(i * Temp);
   FastTan6Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTan6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast Log2() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Log2(i * Temp);
   Log2(i * Temp);
   Log2(i * Temp);
   Log2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Log2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError2(i * Temp);
   FastLog2MinError2(i * Temp);
   FastLog2MinError2(i * Temp);
   FastLog2MinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError3(i * Temp);
   FastLog2MinError3(i * Temp);
   FastLog2MinError3(i * Temp);
   FastLog2MinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError4(i * Temp);
   FastLog2MinError4(i * Temp);
   FastLog2MinError4(i * Temp);
   FastLog2MinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError5(i * Temp);
   FastLog2MinError5(i * Temp);
   FastLog2MinError5(i * Temp);
   FastLog2MinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast Power2() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Power(2, i * Temp);
   Power(2, i * Temp);
   Power(2, i * Temp);
   Power(2, i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Power(2, x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError2(i * Temp);
   FastPower2MinError2(i * Temp);
   FastPower2MinError2(i * Temp);
   FastPower2MinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError3(i * Temp);
   FastPower2MinError3(i * Temp);
   FastPower2MinError3(i * Temp);
   FastPower2MinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError4(i * Temp);
   FastPower2MinError4(i * Temp);
   FastPower2MinError4(i * Temp);
   FastPower2MinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError5(i * Temp);
   FastPower2MinError5(i * Temp);
   FastPower2MinError5(i * Temp);
   FastPower2MinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

end.
