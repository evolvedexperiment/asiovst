unit TestDAV_DspBarberpoleTuner;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_Common, DAV_DspFilterButterworth, DAV_DspTuner,
  DAV_DspCommon, DAV_DspLfo, DAV_DspBarberpoleTuner;

type
  // Test methods for class TBarberpoleFilter
  TestTBarberpoleFilter = class(TTestCase)
  strict private
    FBarberpoleFilter: TBarberpoleFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
  end;

  // Test methods for class TBarberpoleTuner
  TestTBarberpoleTuner = class(TTestCase)
  strict private
    FBarberpoleTuner: TBarberpoleTuner;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
  end;

implementation

uses
  Math, SysUtils; 

{ TestTBarberpoleFilter }

procedure TestTBarberpoleFilter.SetUp;
begin
 FBarberpoleFilter := TBarberpoleFilter.Create;
end;

procedure TestTBarberpoleFilter.TearDown;
begin
 FreeAndNil(FBarberpoleFilter);
end;


procedure TestTBarberpoleFilter.TestProcess;
begin
 FBarberpoleTuner.Process();
end;

{ TestTBarberpoleTuner }

procedure TestTBarberpoleTuner.SetUp;
begin
 FBarberpoleTuner := TBarberpoleTuner.Create;
end;

procedure TestTBarberpoleTuner.TearDown;
begin
 FreeAndNil(FBarberpoleTuner);
end;

procedure TestTBarberpoleTuner.TestProcess;
begin
 FBarberpoleTuner.Process();
 //   procedure Process(Input: Single); override;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTBarberpoleFilter.Suite);
  RegisterTest(TestTBarberpoleTuner.Suite);
end.

