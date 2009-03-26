unit TestDAV_DspBarberpole;
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
  TestFramework, DAV_DspLFO, DAV_DspBarberpole, Classes, DAV_Common, DAV_DspCommon;

type
  // Test methods for class TDspBarberpole32
  TestTDspBarberpole32 = class(TTestCase)
  strict private
    FDspBarberpole32: TDspBarberpole32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestReset;
  end;

  // Test methods for class TDspBarberpole64
  TestTDspBarberpole64 = class(TTestCase)
  strict private
    FDspBarberpole64: TDspBarberpole64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestReset;
  end;

implementation

uses
  SysUtils;

{ TestTDspBarberpole32 }

procedure TestTDspBarberpole32.SetUp;
begin
  FDspBarberpole32 := TDspBarberpole32.Create;
end;

procedure TestTDspBarberpole32.TearDown;
begin
 FreeAndNil(FDspBarberpole32);
end;

procedure TestTDspBarberpole32.TestProcess;
var
  ReturnValue : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole32 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 ReturnValue := FDspBarberpole32.Process(1);

 // Validate method results
 CheckTrue(ReturnValue <> 0);

 // Test chorus process series
 for Sample := 0 to CSampleFrames do
  begin
   ReturnValue := FDspBarberpole32.Process(0);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspBarberpole32.TestReset;
var
  Input       : Single;
  Sample      : Integer;
  Value       : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole32 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspBarberpole32.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole32.Process(Input);

 // store current value
 Value := FDspBarberpole32.Process(Input);

 // reset quque
 FDspBarberpole32.Reset;

 // Call chorus function
 Input := 1;
 FDspBarberpole32.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole32.Process(Input);

 CheckEquals(Value, FDspBarberpole32.Process(Input));
end;


{ TestTDspBarberpole64 }

procedure TestTDspBarberpole64.SetUp;
begin
  FDspBarberpole64 := TDspBarberpole64.Create;
end;

procedure TestTDspBarberpole64.TearDown;
begin
 FreeAndNil(FDspBarberpole64);
end;

procedure TestTDspBarberpole64.TestProcess;
var
  ReturnValue : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole64 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 ReturnValue := FDspBarberpole64.Process(1);

 // Validate method results
 CheckTrue(ReturnValue <> 0);

 // Test chorus process series
 for Sample := 0 to CSampleFrames do
  begin
   ReturnValue := FDspBarberpole64.Process(0);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspBarberpole64.TestReset;
var
  Input       : Single;
  Sample      : Integer;
  Value       : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspBarberpole64 do
  begin
   Mix   := 1;
   Depth := 1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspBarberpole64.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole64.Process(Input);

 // store current value
 Value := FDspBarberpole64.Process(Input);

 // reset quque
 FDspBarberpole64.Reset;

 // Call chorus function
 Input := 1;
 FDspBarberpole64.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspBarberpole64.Process(Input);

 CheckEquals(Value, FDspBarberpole64.Process(Input));
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTDspBarberpole32.Suite);
  RegisterTest(TestTDspBarberpole64.Suite);
end.
