unit TestDAV_DspWaveshaper;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_DspCommon, DAV_Common, Classes, DAV_DspWaveshaper;

type
  TestTChebyshevWaveshaper = class(TTestCase)
  strict private
    FChebyshevWaveshaper: TChebyshevWaveshaper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample64;
  end;

implementation

uses
  SysUtils;

procedure TestTChebyshevWaveshaper.SetUp;
begin
 FChebyshevWaveshaper := TChebyshevWaveshaper.Create;
end;

procedure TestTChebyshevWaveshaper.TearDown;
begin
 FreeAndNil(FChebyshevWaveshaper);
end;

procedure TestTChebyshevWaveshaper.TestProcessSample64;
begin
 with FChebyshevWaveshaper do
  begin
   Order := 2;
   CheckEquals(0, ProcessSample64(0), 'ProcessSample64(0) <> 0!');

   Order := 1;
   Gain[0] := 0;
   CheckEquals(0, ProcessSample64(1), 'No coefficients, but output detected');
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTChebyshevWaveshaper.Suite);
end.

