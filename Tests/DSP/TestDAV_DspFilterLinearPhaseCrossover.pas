unit TestDAV_DspFilterLinearPhaseCrossover;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

{$I DAV_Compiler.inc}

uses
  TestFramework, DAV_DspWindowing, Classes, DAV_DspCommon, DAV_Common,
  DAV_DspFilter, DAV_DspFilterLinearPhaseCrossover;

type
  // Test methods for class TLinearPhaseCrossover
  TestTLinearPhaseCrossover = class(TTestCase)
  strict private
    FLinearPhaseCrossover: TLinearPhaseCrossover;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSampleSingle;
    procedure TestProcessSampleDouble;
  end;

implementation

uses
  SysUtils;

procedure TestTLinearPhaseCrossover.SetUp;
begin
 FLinearPhaseCrossover := TLinearPhaseCrossover.Create;
end;

procedure TestTLinearPhaseCrossover.TearDown;
begin
 FreeAndNil(FLinearPhaseCrossover);
end;

procedure TestTLinearPhaseCrossover.TestProcessSampleSingle;
var
  High   : Single;
  Low    : Single;
  Sample : Integer;
begin
 with FLinearPhaseCrossover do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   FilterLength := 64;
   ProcessSample(1, Low, High);
   CheckTrue((Low = 0) and (High = 0), 'First sample output <> 0');

   for Sample := 0 to FilterLength div 2 - 1
    do ProcessSample(0, Low, High);

   CheckTrue(Low > 0, 'Low is negative');
   CheckTrue(High > 0, 'High is negative');
  end;
end;

procedure TestTLinearPhaseCrossover.TestProcessSampleDouble;
var
  High  : Double;
  Low   : Double;
  Sample : Integer;
begin
 with FLinearPhaseCrossover do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   FilterLength := 64;
   ProcessSample(1, Low, High);
   CheckTrue((Low = 0) and (High = 0), 'First sample output <> 0');

   for Sample := 0 to FilterLength div 2 - 1
    do ProcessSample(0, Low, High);

   CheckTrue(Low > 0, 'Low is negative');
   CheckTrue(High > 0, 'High is negative');
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTLinearPhaseCrossover.Suite);
end.

