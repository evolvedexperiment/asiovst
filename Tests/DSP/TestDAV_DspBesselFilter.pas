unit TestDAV_DspBesselFilter;
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
  TestFramework, DAV_DspBesselFilter, DAV_Common, DAV_DspFilter;

type
  // Test methods for class TBesselLowpassFilter
  TestTBesselLowpassFilter = class(TTestCase)
  strict private
    FBesselLowpassFilter: TBesselLowpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestProcessSample;
    procedure TestMagnitudeSquared;
    procedure TestPhase;
  end;

  // Test methods for class TBesselHighpassFilter
  TestTBesselHighpassFilter = class(TTestCase)
  strict private
    FBesselHighpassFilter: TBesselHighpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCalculateCoefficients;
    procedure TestProcessSample;
    procedure TestMagnitudeSquared;
  end;

implementation

uses
  SysUtils;

procedure TestTBesselLowpassFilter.SetUp;
begin
 FBesselLowpassFilter := TBesselLowpassFilter.Create;
end;

procedure TestTBesselLowpassFilter.TearDown;
begin
 FreeAndNil(FBesselLowpassFilter);
end;

procedure TestTBesselLowpassFilter.TestCalculateCoefficients;
begin
 FBesselLowpassFilter.CalculateCoefficients;
end;

procedure TestTBesselLowpassFilter.TestProcessSample;
var
  ReturnValue : Double;
  Input       : Double;
const
  CSampleFrames = 1000;
begin
  // Initialize filter
  with FBesselLowpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;

  // Calculate filter
  Input := 1;
  ReturnValue := FBesselLowpassFilter.ProcessSample(Input);

  // Validate results
  CheckTrue((ReturnValue > -1) and (ReturnValue < 1));
end;

procedure TestTBesselLowpassFilter.TestMagnitudeSquared;
var
  ReturnValue : Double;
begin
 // Initialize filter
 with FBesselLowpassFilter do
  begin
   SampleRate := 44100;
   SetFilterValues(1000, 0);
   Order := 2;
  end;

 // Calculate Filter
 with FBesselLowpassFilter
  do ReturnValue := MagnitudeSquared(Frequency);

 // Validate result
 CheckTrue(abs(ReturnValue - 1) < 1E-10);
end;

procedure TestTBesselLowpassFilter.TestPhase;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FBesselLowpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;

  with FBesselLowpassFilter
   do ReturnValue := Phase(Frequency);

 // Validate result
 CheckTrue(abs(ReturnValue - 0) < 1E-10);
end;

procedure TestTBesselHighpassFilter.SetUp;
begin
 FBesselHighpassFilter := TBesselHighpassFilter.Create;
end;

procedure TestTBesselHighpassFilter.TearDown;
begin
 FreeAndNil(FBesselHighpassFilter);
end;

procedure TestTBesselHighpassFilter.TestCalculateCoefficients;
begin
 FBesselHighpassFilter.CalculateCoefficients;
end;

procedure TestTBesselHighpassFilter.TestProcessSample;
var
  ReturnValue : Double;
  Input       : Double;
begin
  // Initialize filter
  with FBesselHighpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;
  Input := 1;

  // Calculate Filter
  with FBesselHighpassFilter
   do ReturnValue := ProcessSample(Input);

 // Validate result
 CheckTrue(abs(ReturnValue - 0) < 1E-10);
end;

procedure TestTBesselHighpassFilter.TestMagnitudeSquared;
var
  ReturnValue : Double;
begin
  // Initialize filter
  with FBesselHighpassFilter do
   begin
    SampleRate := 44100;
    SetFilterValues(1000, 0);
    Order := 2;
   end;

  // Calculate Filter
  with FBesselHighpassFilter
   do ReturnValue := MagnitudeSquared(Frequency);

 // Validate result
 CheckTrue(abs(ReturnValue - 1) < 1E-10);
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTBesselLowpassFilter.Suite);
  RegisterTest(TestTBesselHighpassFilter.Suite);
  
end.

