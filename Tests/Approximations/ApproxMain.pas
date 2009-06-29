unit ApproxMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, DAV_GuiBaseControl, DAV_GuiGraphXY;

type
  TFmApproximationBenchmark = class(TForm)
    Memo: TMemo;
    GuiGraphXY: TGuiGraphXY;
    Splitter: TSplitter;
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    MIBenchmark: TMenuItem;
    MICos: TMenuItem;
    MISin: TMenuItem;
    MITan: TMenuItem;
    MITanh: TMenuItem;
    MIFastCos3Term: TMenuItem;
    MIFastCos4Term: TMenuItem;
    MIFastCos5Term: TMenuItem;
    MIFastCos6Term: TMenuItem;
    MISinComplete3Term: TMenuItem;
    MISinComplete4Term: TMenuItem;
    MISinComplete5Term: TMenuItem;
    MISinComplete6Term: TMenuItem;
    N1: TMenuItem;
    MIInBounds3Term: TMenuItem;
    MICosInBounds4Term: TMenuItem;
    MICosInBounds5Term: TMenuItem;
    MICosInBounds6Term: TMenuItem;
    MITanComplete2Term: TMenuItem;
    MITanComplete3Term: TMenuItem;
    MITanComplete4Term: TMenuItem;
    MITanComplete6Term: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    MISinInbounds3Term: TMenuItem;
    MISinInbounds4Term: TMenuItem;
    MISinInbounds5Term: TMenuItem;
    MISinInbounds6Term: TMenuItem;
    MITanInbounds2Term: TMenuItem;
    MITanInbounds3Term: TMenuItem;
    MITanInbounds4Term: TMenuItem;
    MITanInbounds6Term: TMenuItem;
    MITanhComplete3Term: TMenuItem;
    MITanhComplete4Term: TMenuItem;
    MITanhComplete5Term: TMenuItem;
    MITanhComplete7Term: TMenuItem;
    MITanhComplete6Term: TMenuItem;
    N4: TMenuItem;
    MITanhRationalPolynom3TermFPU: TMenuItem;
    MITanhRationalPolynom4TermFPU: TMenuItem;
    MITanhRationalPolynom5TermFPU: TMenuItem;
    MITanhRationalPolynom6TermFPU: TMenuItem;
    MITanhRationalPolynom7TermFPU: TMenuItem;
    N5: TMenuItem;
    MITanhExp2TermMinError: TMenuItem;
    MITanhExp3TermMinError: TMenuItem;
    MITanhExp4TermMinError: TMenuItem;
    MITanhExp5TermMinError: TMenuItem;
    MITanhExp2TermContError: TMenuItem;
    MITanhExp3TermContError: TMenuItem;
    MITanhExp4TermContError: TMenuItem;
    MITanhExp5TermContError: TMenuItem;
    MILog2x: TMenuItem;
    MIPower2: TMenuItem;
    MILog2MinError2Term: TMenuItem;
    MILog2MinError5Term: TMenuItem;
    MILog2MinError4Term: TMenuItem;
    MILog2MinError3Term: TMenuItem;
    N6: TMenuItem;
    MILog2ContError2Term: TMenuItem;
    MILog2ContError3Term: TMenuItem;
    MILog2ContError4Term: TMenuItem;
    MILog2ContError5Term: TMenuItem;
    MIPower2MinError2Term: TMenuItem;
    MIPower2MinError3Term: TMenuItem;
    MIPower2MinError4Term: TMenuItem;
    MIPower2MinError5Term: TMenuItem;
    N7: TMenuItem;
    MIPower2ContError2Term: TMenuItem;
    MIPower2ContError3Term: TMenuItem;
    MIPower2ContError4Term: TMenuItem;
    MIPower2ContError5Term: TMenuItem;
    MISaveLog: TMenuItem;
    N8: TMenuItem;
    MIExp: TMenuItem;
    MIExpContError5Term: TMenuItem;
    MIExpContError4Term: TMenuItem;
    MIExpContError3Term: TMenuItem;
    MIExpContError2Term: TMenuItem;
    N9: TMenuItem;
    MIExpMinError5Term: TMenuItem;
    MIExpMinError4Term: TMenuItem;
    MIExpMinError3Term: TMenuItem;
    MIExpMinError2Term: TMenuItem;
    procedure MemoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function Evaluation(Sender: TObject; X: Double): Double;
    procedure MILog2ContError2TermClick(Sender: TObject);
    procedure MILog2ContError3TermClick(Sender: TObject);
    procedure MILog2ContError4TermClick(Sender: TObject);
    procedure MILog2ContError5TermClick(Sender: TObject);
    procedure MICosClick(Sender: TObject);
    procedure MICosInBounds3TermClick(Sender: TObject);
    procedure MICosInBounds4TermClick(Sender: TObject);
    procedure MICosInBounds5TermClick(Sender: TObject);
    procedure MICosInBounds6TermClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIFastCos3TermClick(Sender: TObject);
    procedure MIFastCos4TermClick(Sender: TObject);
    procedure MIFastCos5TermClick(Sender: TObject);
    procedure MIFastCos6TermClick(Sender: TObject);
    procedure MILog2Click(Sender: TObject);
    procedure MILog2MinError2TermClick(Sender: TObject);
    procedure MILog2MinError3TermClick(Sender: TObject);
    procedure MILog2MinError4TermClick(Sender: TObject);
    procedure MILog2MinError5TermClick(Sender: TObject);
    procedure MISinClick(Sender: TObject);
    procedure MISinComplete3TermClick(Sender: TObject);
    procedure MISinComplete4TermClick(Sender: TObject);
    procedure MISinComplete5TermClick(Sender: TObject);
    procedure MISinComplete6TermClick(Sender: TObject);
    procedure MISinInbounds3TermClick(Sender: TObject);
    procedure MISinInbounds4TermClick(Sender: TObject);
    procedure MISinInbounds5TermClick(Sender: TObject);
    procedure MISinInbounds6TermClick(Sender: TObject);
    procedure MITanClick(Sender: TObject);
    procedure MITanComplete2TermClick(Sender: TObject);
    procedure MITanComplete3TermClick(Sender: TObject);
    procedure MITanComplete4TermClick(Sender: TObject);
    procedure MITanComplete6TermClick(Sender: TObject);
    procedure MITanhClick(Sender: TObject);
    procedure MITanhComplete3TermClick(Sender: TObject);
    procedure MITanhComplete4TermClick(Sender: TObject);
    procedure MITanhComplete5TermClick(Sender: TObject);
    procedure MITanhComplete6TermClick(Sender: TObject);
    procedure MITanhComplete7TermClick(Sender: TObject);
    procedure MITanhExp2TermContErrorClick(Sender: TObject);
    procedure MITanhExp2TermMinErrorClick(Sender: TObject);
    procedure MITanhExp3TermContErrorClick(Sender: TObject);
    procedure MITanhExp3TermMinErrorClick(Sender: TObject);
    procedure MITanhExp4TermContErrorClick(Sender: TObject);
    procedure MITanhExp4TermMinErrorClick(Sender: TObject);
    procedure MITanhExp5TermContErrorClick(Sender: TObject);
    procedure MITanhExp5TermMinErrorClick(Sender: TObject);
    procedure MITanhRationalPolynom3TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom4TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom5TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom6TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom7TermFPUClick(Sender: TObject);
    procedure MITanInbounds2TermClick(Sender: TObject);
    procedure MITanInbounds3TermClick(Sender: TObject);
    procedure MITanInbounds4TermClick(Sender: TObject);
    procedure MITanInbounds6TermClick(Sender: TObject);
    procedure MIPower2Click(Sender: TObject);
    procedure MIPower2MinError2TermClick(Sender: TObject);
    procedure MIPower2MinError3TermClick(Sender: TObject);
    procedure MIPower2MinError4TermClick(Sender: TObject);
    procedure MIPower2MinError5TermClick(Sender: TObject);
    procedure MIPower2ContError2TermClick(Sender: TObject);
    procedure MIPower2ContError3TermClick(Sender: TObject);
    procedure MIPower2ContError4TermClick(Sender: TObject);
    procedure MIPower2ContError5TermClick(Sender: TObject);
    procedure MISaveLogClick(Sender: TObject);
    procedure MIExpMinError2TermClick(Sender: TObject);
    procedure MIExpMinError3TermClick(Sender: TObject);
    procedure MIExpMinError4TermClick(Sender: TObject);
    procedure MIExpMinError5TermClick(Sender: TObject);
    procedure MIExpContError2TermClick(Sender: TObject);
    procedure MIExpContError3TermClick(Sender: TObject);
    procedure MIExpContError4TermClick(Sender: TObject);
    procedure MIExpContError5TermClick(Sender: TObject);
  protected
    function EvaluateCosine(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine3(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine4(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine5(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine6(Sender: TObject; X: Double): Double;
    function EvaluateSine(Sender: TObject; X: Double): Double;
    function EvaluateFastSine3(Sender: TObject; X: Double): Double;
    function EvaluateFastSine4(Sender: TObject; X: Double): Double;
    function EvaluateFastSine5(Sender: TObject; X: Double): Double;
    function EvaluateFastSine6(Sender: TObject; X: Double): Double;
  end;

var
  FmApproximationBenchmark: TFmApproximationBenchmark;

implementation

uses
  Math, DAV_Common, DAV_Approximations;

{$R *.dfm}

procedure TFmApproximationBenchmark.FormShow(Sender: TObject);
begin
// MemoClick(Sender);
 TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series).OnEvaluate := Evaluation;
end;

procedure TFmApproximationBenchmark.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmApproximationBenchmark.MIExpContError2TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpContinousError2(i * Temp);
   FastExpContinousError2(i * Temp);
   FastExpContinousError2(i * Temp);
   FastExpContinousError2(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpContinousError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpContError3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpContinousError3(i * Temp);
   FastExpContinousError3(i * Temp);
   FastExpContinousError3(i * Temp);
   FastExpContinousError3(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpContinousError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpContError4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpContinousError4(i * Temp);
   FastExpContinousError4(i * Temp);
   FastExpContinousError4(i * Temp);
   FastExpContinousError4(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpContinousError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpContError5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpContinousError5(i * Temp);
   FastExpContinousError5(i * Temp);
   FastExpContinousError5(i * Temp);
   FastExpContinousError5(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpContinousError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError2TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpMinError2(i * Temp);
   FastExpMinError2(i * Temp);
   FastExpMinError2(i * Temp);
   FastExpMinError2(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpMinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpMinError3(i * Temp);
   FastExpMinError3(i * Temp);
   FastExpMinError3(i * Temp);
   FastExpMinError3(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpMinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpMinError4(i * Temp);
   FastExpMinError4(i * Temp);
   FastExpMinError4(i * Temp);
   FastExpMinError4(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpMinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastExpMinError5(i * Temp);
   FastExpMinError5(i * Temp);
   FastExpMinError5(i * Temp);
   FastExpMinError5(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastExpMinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

function TFmApproximationBenchmark.Evaluation(Sender: TObject;
  X: Double): Double;
var
  s : Single;
begin
// result := Amp_to_dB(abs(x)) - FastAmpTodBMinError5(abs(x));
// result := Amp_to_dB(abs(x)) - FastAmptodBContinousError5(abs(x));
 s := x;
 result := dB_to_Amp(x) - FastdBtoAmpMinError3(s);
end;

function TFmApproximationBenchmark.EvaluateCosine(Sender: TObject;
  X: Double): Double;
begin
 result := cos(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine3(Sender: TObject;
  X: Double): Double;
begin
 result := FastCos3Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine4(Sender: TObject;
  X: Double): Double;
begin
 result := FastCos4Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine5(Sender: TObject;
  X: Double): Double;
begin
 result := FastCos5Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine6(Sender: TObject;
  X: Double): Double;
begin
 result := FastCos6Term(x);
end;

function TFmApproximationBenchmark.EvaluateSine(Sender: TObject;
  X: Double): Double;
begin
 result := sin(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine3(Sender: TObject;
  X: Double): Double;
begin
 result := FastSin3Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine4(Sender: TObject;
  X: Double): Double;
begin
 result := FastSin4Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine5(Sender: TObject;
  X: Double): Double;
begin
 result := FastSin5Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine6(Sender: TObject;
  X: Double): Double;
begin
 result := FastSin6Term(x);
end;

procedure TFmApproximationBenchmark.MICosClick(Sender: TObject);
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

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateCosine;

 GuiGraphXY.YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
 GuiGraphXY.YAxis.SetBounds(-1, 1);
 GuiGraphXY.UpdateGraph;

 // evaluate performance for cos(x)
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
end;

procedure TFmApproximationBenchmark.MISaveLogClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   Filter := 'Text (*.txt)|*.txt';
   DefaultExt := 'txt';
   if Execute then
    begin
     Memo.Lines.SaveToFile(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmApproximationBenchmark.MISinClick(Sender: TObject);
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

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateSine;

 GuiGraphXY.YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
 GuiGraphXY.YAxis.SetBounds(-1, 1);
 GuiGraphXY.UpdateGraph;

 // evaluate performance for cos(x)
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Sin(i * Temp);
   Sin(i * Temp);
   Sin(i * Temp);
   Sin(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Sin(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

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
end;

procedure TFmApproximationBenchmark.MITanhClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Tanh(i * Temp);
   Tanh(i * Temp);
   Tanh(i * Temp);
   Tanh(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Tanh(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2Click(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

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
end;

procedure TFmApproximationBenchmark.MIPower2Click(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

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
end;

procedure TFmApproximationBenchmark.MITanComplete2TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTan2Term(i * Temp);
   FastTan2Term(i * Temp);
   FastTan2Term(i * Temp);
   FastTan2Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTan2Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanComplete3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MITanComplete4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MITanComplete6TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MITanhComplete3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt3Term(i * Temp);
   FastTanhOpt3Term(i * Temp);
   FastTanhOpt3Term(i * Temp);
   FastTanhOpt3Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt4Term(i * Temp);
   FastTanhOpt4Term(i * Temp);
   FastTanhOpt4Term(i * Temp);
   FastTanhOpt4Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt5Term(i * Temp);
   FastTanhOpt5Term(i * Temp);
   FastTanhOpt5Term(i * Temp);
   FastTanhOpt5Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt5Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete6TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt6Term(i * Temp);
   FastTanhOpt6Term(i * Temp);
   FastTanhOpt6Term(i * Temp);
   FastTanhOpt6Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete7TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt7Term(i * Temp);
   FastTanhOpt7Term(i * Temp);
   FastTanhOpt7Term(i * Temp);
   FastTanhOpt7Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt7Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp2TermContErrorClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhContinousError2(i * Temp);
   FastTanhContinousError2(i * Temp);
   FastTanhContinousError2(i * Temp);
   FastTanhContinousError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhContinousError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp2TermMinErrorClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhMinError2(i * Temp);
   FastTanhMinError2(i * Temp);
   FastTanhMinError2(i * Temp);
   FastTanhMinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhMinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp3TermContErrorClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhContinousError3(i * Temp);
   FastTanhContinousError3(i * Temp);
   FastTanhContinousError3(i * Temp);
   FastTanhContinousError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhContinousError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp3TermMinErrorClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhMinError3(i * Temp);
   FastTanhMinError3(i * Temp);
   FastTanhMinError3(i * Temp);
   FastTanhMinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhMinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp4TermContErrorClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhContinousError4(i * Temp);
   FastTanhContinousError4(i * Temp);
   FastTanhContinousError4(i * Temp);
   FastTanhContinousError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhContinousError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp4TermMinErrorClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhMinError4(i * Temp);
   FastTanhMinError4(i * Temp);
   FastTanhMinError4(i * Temp);
   FastTanhMinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhMinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp5TermContErrorClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhContinousError5(i * Temp);
   FastTanhContinousError5(i * Temp);
   FastTanhContinousError5(i * Temp);
   FastTanhContinousError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhContinousError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp5TermMinErrorClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhMinError5(i * Temp);
   FastTanhMinError5(i * Temp);
   FastTanhMinError5(i * Temp);
   FastTanhMinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhMinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom3TermFPUClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt3TermFPU(i * Temp);
   FastTanhOpt3TermFPU(i * Temp);
   FastTanhOpt3TermFPU(i * Temp);
   FastTanhOpt3TermFPU(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt3TermFPU(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom4TermFPUClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt4TermFPU(i * Temp);
   FastTanhOpt4TermFPU(i * Temp);
   FastTanhOpt4TermFPU(i * Temp);
   FastTanhOpt4TermFPU(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt4TermFPU(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom5TermFPUClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt5TermFPU(i * Temp);
   FastTanhOpt5TermFPU(i * Temp);
   FastTanhOpt5TermFPU(i * Temp);
   FastTanhOpt5TermFPU(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt5TermFPU(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom6TermFPUClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt6TermFPU(i * Temp);
   FastTanhOpt6TermFPU(i * Temp);
   FastTanhOpt6TermFPU(i * Temp);
   FastTanhOpt6TermFPU(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt6TermFPU(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom7TermFPUClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanhOpt7TermFPU(i * Temp);
   FastTanhOpt7TermFPU(i * Temp);
   FastTanhOpt7TermFPU(i * Temp);
   FastTanhOpt7TermFPU(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanhOpt7TermFPU(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds2TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanInBounds2Term(i * Temp);
   FastTanInBounds2Term(i * Temp);
   FastTanInBounds2Term(i * Temp);
   FastTanInBounds2Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanInBounds2Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanInBounds3Term(i * Temp);
   FastTanInBounds3Term(i * Temp);
   FastTanInBounds3Term(i * Temp);
   FastTanInBounds3Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanInBounds3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanInBounds4Term(i * Temp);
   FastTanInBounds4Term(i * Temp);
   FastTanInBounds4Term(i * Temp);
   FastTanInBounds4Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanInBounds4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds6TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTanInBounds6Term(i * Temp);
   FastTanInBounds6Term(i * Temp);
   FastTanInBounds6Term(i * Temp);
   FastTanInBounds6Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTanInBounds6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIFastCos3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MIFastCos4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos4Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MIFastCos5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos4Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MIFastCos6TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MILog2MinError2TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MILog2MinError3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MILog2MinError4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MILog2MinError5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MIPower2ContError2TermClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2ContinousError2(i * Temp);
   FastPower2ContinousError2(i * Temp);
   FastPower2ContinousError2(i * Temp);
   FastPower2ContinousError2(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2ContinousError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2ContError3TermClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2ContinousError3(i * Temp);
   FastPower2ContinousError3(i * Temp);
   FastPower2ContinousError3(i * Temp);
   FastPower2ContinousError3(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2ContinousError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2ContError4TermClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2ContinousError4(i * Temp);
   FastPower2ContinousError4(i * Temp);
   FastPower2ContinousError4(i * Temp);
   FastPower2ContinousError4(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2ContinousError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2ContError5TermClick(
  Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2ContinousError5(i * Temp);
   FastPower2ContinousError5(i * Temp);
   FastPower2ContinousError5(i * Temp);
   FastPower2ContinousError5(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2ContinousError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2MinError2TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MIPower2MinError3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MIPower2MinError4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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
end;

procedure TFmApproximationBenchmark.MIPower2MinError5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
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

procedure TFmApproximationBenchmark.MILog2ContError2TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2ContinousError2(i * Temp);
   FastLog2ContinousError2(i * Temp);
   FastLog2ContinousError2(i * Temp);
   FastLog2ContinousError2(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2ContinousError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2ContError3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2ContinousError3(i * Temp);
   FastLog2ContinousError3(i * Temp);
   FastLog2ContinousError3(i * Temp);
   FastLog2ContinousError3(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2ContinousError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2ContError4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2ContinousError4(i * Temp);
   FastLog2ContinousError4(i * Temp);
   FastLog2ContinousError4(i * Temp);
   FastLog2ContinousError4(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2ContinousError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2ContError5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MILog2Click(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2ContinousError5(i * Temp);
   FastLog2ContinousError5(i * Temp);
   FastLog2ContinousError5(i * Temp);
   FastLog2ContinousError5(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2ContinousError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCosInBounds3Term(i * Temp);
   FastCosInBounds3Term(i * Temp);
   FastCosInBounds3Term(i * Temp);
   FastCosInBounds3Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCosInBounds3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCosInBounds4Term(i * Temp);
   FastCosInBounds4Term(i * Temp);
   FastCosInBounds4Term(i * Temp);
   FastCosInBounds4Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCosInBounds4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCosInBounds5Term(i * Temp);
   FastCosInBounds5Term(i * Temp);
   FastCosInBounds5Term(i * Temp);
   FastCosInBounds5Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCosInBounds5Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds6TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastCosInBounds6Term(i * Temp);
   FastCosInBounds6Term(i * Temp);
   FastCosInBounds6Term(i * Temp);
   FastCosInBounds6Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastCosInBounds6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSin3Term(i * Temp);
   FastSin3Term(i * Temp);
   FastSin3Term(i * Temp);
   FastSin3Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSin3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSin4Term(i * Temp);
   FastSin4Term(i * Temp);
   FastSin4Term(i * Temp);
   FastSin4Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSin4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSin5Term(i * Temp);
   FastSin5Term(i * Temp);
   FastSin5Term(i * Temp);
   FastSin5Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSin5Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete6TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSin6Term(i * Temp);
   FastSin6Term(i * Temp);
   FastSin6Term(i * Temp);
   FastSin6Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSin6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds3TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSinInBounds3Term(i * Temp);
   FastSinInBounds3Term(i * Temp);
   FastSinInBounds3Term(i * Temp);
   FastSinInBounds3Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSinInBounds3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds4TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSinInBounds4Term(i * Temp);
   FastSinInBounds4Term(i * Temp);
   FastSinInBounds4Term(i * Temp);
   FastSinInBounds4Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSinInBounds4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds5TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSinInBounds5Term(i * Temp);
   FastSinInBounds5Term(i * Temp);
   FastSinInBounds5Term(i * Temp);
   FastSinInBounds5Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSinInBounds5Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds6TermClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastSinInBounds6Term(i * Temp);
   FastSinInBounds6Term(i * Temp);
   FastSinInBounds6Term(i * Temp);
   FastSinInBounds6Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastSinInBounds6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MemoClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
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

 // Fast AmpTodB() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Amp_To_dB(i * Temp);
   Amp_To_dB(i * Temp);
   Amp_To_dB(i * Temp);
   Amp_To_dB(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Amp_To_dB(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError2(i * Temp);
   FastAmpTodBMinError2(i * Temp);
   FastAmpTodBMinError2(i * Temp);
   FastAmpTodBMinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError3(i * Temp);
   FastAmpTodBMinError3(i * Temp);
   FastAmpTodBMinError3(i * Temp);
   FastAmpTodBMinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError4(i * Temp);
   FastAmpTodBMinError4(i * Temp);
   FastAmpTodBMinError4(i * Temp);
   FastAmpTodBMinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError5(i * Temp);
   FastAmpTodBMinError5(i * Temp);
   FastAmpTodBMinError5(i * Temp);
   FastAmpTodBMinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast dBtoAmp() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   dB_to_Amp(i * Temp);
   dB_to_Amp(i * Temp);
   dB_to_Amp(i * Temp);
   dB_to_Amp(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: dB_to_Amp(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError2(i * Temp);
   FastdBtoAmpMinError2(i * Temp);
   FastdBtoAmpMinError2(i * Temp);
   FastdBtoAmpMinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError3(i * Temp);
   FastdBtoAmpMinError3(i * Temp);
   FastdBtoAmpMinError3(i * Temp);
   FastdBtoAmpMinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError4(i * Temp);
   FastdBtoAmpMinError4(i * Temp);
   FastdBtoAmpMinError4(i * Temp);
   FastdBtoAmpMinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError5(i * Temp);
   FastdBtoAmpMinError5(i * Temp);
   FastdBtoAmpMinError5(i * Temp);
   FastdBtoAmpMinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

end.
