unit TestDAV_DspFilterSpectralDelay;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Classes, DAV_DspFilterSpectralDelay, DAV_DspFilter,
  DAV_Common, DAV_Classes;

type
  // Test methods for class TSpectralDelayFilter
  TestTSpectralDelayFilter = class(TTestCase)
  strict private
    FSpectralDelayFilter: TSpectralDelayFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample32;
    procedure TestProcessSample64;
    procedure TestMagnitudeLog10;
    procedure TestMagnitudeSquared;
    procedure TestReset;
    procedure TestResetStates;
    procedure TestResetStatesInt64;
    procedure TestPushPopStates;
  end;

implementation

uses
  SysUtils;

procedure TestTSpectralDelayFilter.SetUp;
begin
 FSpectralDelayFilter := TSpectralDelayFilter.Create;
end;

procedure TestTSpectralDelayFilter.TearDown;
begin
 FreeAndNil(FSpectralDelayFilter);
end;

procedure TestTSpectralDelayFilter.TestProcessSample32;
var
  ReturnValue : Single;
  Input       : Single;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.ProcessSample32(Input);
 // TODO: Validate method results
end;

procedure TestTSpectralDelayFilter.TestProcessSample64;
var
  ReturnValue: Double;
  Input: Double;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.ProcessSample64(Input);
 // TODO: Validate method results
end;

procedure TestTSpectralDelayFilter.TestMagnitudeLog10;
var
  ReturnValue: Double;
  Frequency: Double;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.MagnitudeLog10(Frequency);
 // TODO: Validate method results
end;

procedure TestTSpectralDelayFilter.TestMagnitudeSquared;
var
  ReturnValue: Double;
  Frequency: Double;
begin
 // TODO: Setup method call parameters
 ReturnValue := FSpectralDelayFilter.MagnitudeSquared(Frequency);
 // TODO: Validate method results
end;

procedure TestTSpectralDelayFilter.TestReset;
begin
 FSpectralDelayFilter.Reset;
 // TODO: Validate method results
end;

procedure TestTSpectralDelayFilter.TestResetStates;
begin
 FSpectralDelayFilter.ResetStates;
 // TODO: Validate method results
end;

procedure TestTSpectralDelayFilter.TestResetStatesInt64;
begin
 FSpectralDelayFilter.ResetStatesInt64;
 // TODO: Validate method results
end;

procedure TestTSpectralDelayFilter.TestPushPopStates;
begin
 FSpectralDelayFilter.PushStates;
 FSpectralDelayFilter.PopStates;
 // TODO: Validate method results
end;

initialization
 // Alle Testfälle beim Test-Runner registrieren
 RegisterTest(TestTSpectralDelayFilter.Suite);

end.
