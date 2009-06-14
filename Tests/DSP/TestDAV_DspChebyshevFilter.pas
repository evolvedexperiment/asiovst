unit TestDAV_DSPFilterChebyshev;
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
  TestFramework, DAV_Complex, DAV_DSPFilterChebyshev, DAV_DspFilter, DAV_Common;

type
  // Test methods for class TChebyshev1LowpassFilter
  TestTChebyshev1LowpassFilter = class(TTestCase)
  strict private
    FChebyshev1LowpassFilter: TChebyshev1LowpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

  // Test methods for class TChebyshev1LowpassFilterAutomatable
  TestTChebyshev1LowpassFilterAutomatable = class(TTestCase)
  strict private
    FChebyshev1LowpassFilterAutomatable: TChebyshev1LowpassFilterAutomatable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

  // Test methods for class TChebyshev1HighpassFilter
  TestTChebyshev1HighpassFilter = class(TTestCase)
  strict private
    FChebyshev1HighpassFilter: TChebyshev1HighpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

  // Test methods for class TChebyshev1HighpassFilterAutomatable
  TestTChebyshev1HighpassFilterAutomatable = class(TTestCase)
  strict private
    FChebyshev1HighpassFilterAutomatable: TChebyshev1HighpassFilterAutomatable;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestMagnitudeSquared;
  end;

implementation

uses
  SysUtils;

{ TestTChebyshev1LowpassFilter }

procedure TestTChebyshev1LowpassFilter.SetUp;
begin
 FChebyshev1LowpassFilter := TChebyshev1LowpassFilter.Create;
end;

procedure TestTChebyshev1LowpassFilter.TearDown;
begin
 FreeAndNil(FChebyshev1LowpassFilter);
end;

procedure TestTChebyshev1LowpassFilter.TestCalculateCoefficients;
begin
 FChebyshev1LowpassFilter.CalculateCoefficients;
end;

procedure TestTChebyshev1LowpassFilter.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
  // Initialize filter
  with FChebyshev1LowpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0, 0.5);
    Order := 2;
   end;

  // Calculate Filter
  with FChebyshev1LowpassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-15);
end;


{ TestTChebyshev1LowpassFilterAutomatable }

procedure TestTChebyshev1LowpassFilterAutomatable.SetUp;
begin
 FChebyshev1LowpassFilterAutomatable := TChebyshev1LowpassFilterAutomatable.Create;
end;

procedure TestTChebyshev1LowpassFilterAutomatable.TearDown;
begin
 FreeAndNil(FChebyshev1LowpassFilterAutomatable);
end;

procedure TestTChebyshev1LowpassFilterAutomatable.TestCalculateCoefficients;
begin
 FChebyshev1LowpassFilterAutomatable.CalculateCoefficients;
end;

procedure TestTChebyshev1LowpassFilterAutomatable.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
 // Initialize filter
 with FChebyshev1LowpassFilterAutomatable do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0, 0.5);
   Order := 2;
  end;

  // Calculate Filter
  with FChebyshev1LowpassFilterAutomatable
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-2);
end;


{ TestTChebyshev1HighpassFilter }

procedure TestTChebyshev1HighpassFilter.SetUp;
begin
 FChebyshev1HighpassFilter := TChebyshev1HighpassFilter.Create;
end;

procedure TestTChebyshev1HighpassFilter.TearDown;
begin
 FreeAndNil(FChebyshev1HighpassFilter);
end;

procedure TestTChebyshev1HighpassFilter.TestCalculateCoefficients;
begin
 FChebyshev1HighpassFilter.CalculateCoefficients;
end;

procedure TestTChebyshev1HighpassFilter.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
 // Initialize filter
 with FChebyshev1HighpassFilter do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0, 0.5);
   Order := 2;
  end;

  // Calculate Filter
  with FChebyshev1HighpassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-13);
end;


{ TestTChebyshev1HighpassFilterAutomatable }

procedure TestTChebyshev1HighpassFilterAutomatable.SetUp;
begin
 FChebyshev1HighpassFilterAutomatable := TChebyshev1HighpassFilterAutomatable.Create;
end;

procedure TestTChebyshev1HighpassFilterAutomatable.TearDown;
begin
 FreeAndNil(FChebyshev1HighpassFilterAutomatable);
end;

procedure TestTChebyshev1HighpassFilterAutomatable.TestCalculateCoefficients;
begin
 FChebyshev1HighpassFilterAutomatable.CalculateCoefficients;
end;

procedure TestTChebyshev1HighpassFilterAutomatable.TestMagnitudeSquared;
var
  ReturnValue: Double;
begin
 // Initialize filter
 with FChebyshev1HighpassFilterAutomatable do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0, 0.5);
   Order := 2;
  end;

  // Calculate Filter
  with FChebyshev1HighpassFilterAutomatable
   do ReturnValue := MagnitudeSquared(Frequency);

  CheckTrue(abs(ReturnValue - 1) < 1E-2);
end;


initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTChebyshev1LowpassFilter.Suite);
  RegisterTest(TestTChebyshev1LowpassFilterAutomatable.Suite);
  RegisterTest(TestTChebyshev1HighpassFilter.Suite);
  RegisterTest(TestTChebyshev1HighpassFilterAutomatable.Suite);

end.

