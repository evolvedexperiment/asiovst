unit TestDAV_DspDelayLines;
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
  TestFramework, DAV_Complex, DAV_DspDelayLines, DAV_Common, DAV_DspCommon;

type
  // Test methods for class TDelayLineSamples32
  TestTDelayLineSamples32 = class(TTestCase)
  strict private
    FDelayLineSamples32: TDelayLineSamples32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestProcessSample;
  end;

  // Test methods for class TDelayLineSamples64
  TestTDelayLineSamples64 = class(TTestCase)
  strict private
    FDelayLineSamples64: TDelayLineSamples64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestProcessSample;
  end;

  // Test methods for class TDelayLineTime32
  TestTDelayLineTime32 = class(TTestCase)
  strict private
    FDelayLineTime32: TDelayLineTime32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
    procedure TestProcessSample;
  end;

implementation

uses
  SysUtils;

{ TestTDelayLineSamples32 }

procedure TestTDelayLineSamples32.SetUp;
begin
  FDelayLineSamples32 := TDelayLineSamples32.Create;
end;

procedure TestTDelayLineSamples32.TearDown;
begin
 FreeAndNil(FDelayLineSamples32);
end;

procedure TestTDelayLineSamples32.TestReset;
var
  CurrentSample : Integer;
const
  CSampleFrames = 1000;
begin
 with FDelayLineSamples32 do
  begin
   // Initialize delay line
   BufferSize := 1000;

   // Test delay line
   for CurrentSample := 0 to CSampleFrames - 1
    do ProcessSample32(1);

   Reset;

   // Validate result
   CheckTrue(ProcessSample32(1) = 0);
  end;
end;

procedure TestTDelayLineSamples32.TestProcessSample;
var
  CurrentSample : Integer;
const
  CSampleFrames = 1000;
begin
 with FDelayLineSamples32 do
  begin
   // Initialize delay line
   BufferSize := CSampleFrames;

   // Test delay line
   ProcessSample32(1);
   for CurrentSample := 1 to CSampleFrames - 1
    do ProcessSample32(0);

   // Validate result
   CheckEquals(ProcessSample32(1), 1);
  end;
end;


{ TestTDelayLineSamples64 }

procedure TestTDelayLineSamples64.SetUp;
begin
  FDelayLineSamples64 := TDelayLineSamples64.Create;
end;

procedure TestTDelayLineSamples64.TearDown;
begin
 FreeAndNil(FDelayLineSamples64);
end;

procedure TestTDelayLineSamples64.TestReset;
var
  CurrentSample : Integer;
const
  CSampleFrames = 1000;
begin
 with FDelayLineSamples64 do
  begin
   // Initialize delay line
   BufferSize := 1000;

   // Test delay line
   for CurrentSample := 0 to CSampleFrames - 1
    do ProcessSample64(1);

   Reset;

   // Validate result
   CheckTrue(ProcessSample64(1) = 0);
  end;
end;

procedure TestTDelayLineSamples64.TestProcessSample;
var
  CurrentSample : Integer;
const
  CSampleFrames = 1000;
begin
 with FDelayLineSamples64 do
  begin
   // Initialize delay line
   BufferSize := CSampleFrames;

   // Test delay line
   ProcessSample64(1);
   for CurrentSample := 1 to CSampleFrames - 1
    do ProcessSample64(0);

   // Validate result
   CheckEquals(ProcessSample64(1), 1);
  end;
end;


{ TestTDelayLineTime32 }

procedure TestTDelayLineTime32.SetUp;
begin
 FDelayLineTime32 := TDelayLineTime32.Create;
end;

procedure TestTDelayLineTime32.TearDown;
begin
 FreeAndNil(FDelayLineTime32);
end;

procedure TestTDelayLineTime32.TestReset;
var
  CurrentSample : Integer;
const
  CSampleFrames = 1000;
begin
 with FDelayLineTime32 do
  begin
   // Initialize delay line
   Time := 1000 / Samplerate;

   // Test delay line
   for CurrentSample := 0 to CSampleFrames - 1
    do ProcessSample32(1);

   Reset;

   // Validate result
   CheckTrue(ProcessSample32(1) = 0);
  end;
end;

procedure TestTDelayLineTime32.TestProcessSample;
var
  CurrentSample : Integer;
const
  CSampleFrames = 1000;
begin
 with FDelayLineTime32 do
  begin
   // Initialize delay line
   Time := 1000 / Samplerate;

   // Test delay line
   ProcessSample32(1);
   for CurrentSample := 1 to CSampleFrames - 1
    do ProcessSample32(0);

   // Validate result
   CheckTrue(abs(ProcessSample32(1) - 1) < 1E-10);
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTDelayLineSamples32.Suite);
  RegisterTest(TestTDelayLineSamples64.Suite);
  RegisterTest(TestTDelayLineTime32.Suite);
end.

