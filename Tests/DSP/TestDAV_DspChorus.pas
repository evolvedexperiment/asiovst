unit TestDAV_DspChorus;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_DspLFO, DAV_Common, Classes, DAV_DspChorus, DAV_DspCommon;
type
  // Test methods for class TCustomDspChorus
  
  TestTCustomDspChorus = class(TTestCase)
  strict private
    FCustomDspChorus: TCustomDspChorus;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReset;
  end;
  // Test methods for class TDspChorus32
  
  TestTDspChorus32 = class(TTestCase)
  strict private
    FDspChorus32: TDspChorus32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestReset;
  end;
  // Test methods for class TDspChorus64
  
  TestTDspChorus64 = class(TTestCase)
  strict private
    FDspChorus64: TDspChorus64;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestReset;
  end;

implementation

procedure TestTCustomDspChorus.SetUp;
begin
  FCustomDspChorus := TCustomDspChorus.Create;
end;

procedure TestTCustomDspChorus.TearDown;
begin
  FCustomDspChorus.Free;
  FCustomDspChorus := nil;
end;

procedure TestTCustomDspChorus.TestReset;
begin
  FCustomDspChorus.Reset;
  // TODO: Validate method results
end;

procedure TestTDspChorus32.SetUp;
begin
  FDspChorus32 := TDspChorus32.Create;
end;

procedure TestTDspChorus32.TearDown;
begin
  FDspChorus32.Free;
  FDspChorus32 := nil;
end;

procedure TestTDspChorus32.TestProcess;
var
  ReturnValue : Single;
  Input       : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus32 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0.1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 ReturnValue := FDspChorus32.Process(Input);

 // Validate method results
 CheckTrue(ReturnValue <> 0);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames do
  begin
   ReturnValue := FDspChorus32.Process(Input);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspChorus32.TestReset;
var
  Input       : Single;
  Sample      : Integer;
  Value       : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus32 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspChorus32.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus32.Process(Input);

 // store current value
 Value := FDspChorus32.Process(Input);

 // reset quque
 FDspChorus32.Reset;

 // Call chorus function
 Input := 1;
 FDspChorus32.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus32.Process(Input);

 CheckEquals(Value, FDspChorus32.Process(Input));
end;

procedure TestTDspChorus64.SetUp;
begin
  FDspChorus64 := TDspChorus64.Create;
end;

procedure TestTDspChorus64.TearDown;
begin
  FDspChorus64.Free;
  FDspChorus64 := nil;
end;

procedure TestTDspChorus64.TestProcess;
var
  ReturnValue : Single;
  Input       : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus64 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0.1;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 ReturnValue := FDspChorus64.Process(Input);

 // Validate method results
 CheckTrue(ReturnValue <> 0);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to 1000 do
  begin
   ReturnValue := FDspChorus64.Process(Input);
   CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
  end;
end;

procedure TestTDspChorus64.TestReset;
var
  Input       : Single;
  Sample      : Integer;
  Value       : Single;
const
  CSampleFrames = 1000;
begin
 // Initialize chorus
 with FDspChorus64 do
  begin
   Mix   := 1;
   Depth := 1;
   Drift := 0;
   Mix   := 0.5;
   SampleRate := 44100;
   Stages := 2;
  end;

 // Call chorus function
 Input := 1;
 FDspChorus64.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus64.Process(Input);

 // store current value
 Value := FDspChorus64.Process(Input);

 // reset quque
 FDspChorus64.Reset;

 // Call chorus function
 Input := 1;
 FDspChorus64.Process(Input);

 // Test chorus process series
 Input := 0;
 for Sample := 0 to CSampleFrames
  do FDspChorus64.Process(Input);

 CheckEquals(Value, FDspChorus64.Process(Input));
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTCustomDspChorus.Suite);
  RegisterTest(TestTDspChorus32.Suite);
  RegisterTest(TestTDspChorus64.Suite);
end.

