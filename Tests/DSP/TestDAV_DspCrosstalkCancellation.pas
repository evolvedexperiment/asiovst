unit TestDAV_DspCrosstalkCancellation;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_Common, DAV_DspCommon, DAV_DspDelayLines,
  DAV_DspCrosstalkCancellation, DAV_DspFilterBasics;

type
  // Test methods for class TCrosstalkCancellation32

  TestTCrosstalkCancellation32 = class(TTestCase)
  strict private
    FCrosstalkCancellation32: TCrosstalkCancellation32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessStereo;
  end;

implementation

uses
  SysUtils;

{ TestTCrosstalkCancellation32 }

procedure TestTCrosstalkCancellation32.SetUp;
begin
 FCrosstalkCancellation32 := TCrosstalkCancellation32.Create;
end;

procedure TestTCrosstalkCancellation32.TearDown;
begin
 FreeAndNil(FCrosstalkCancellation32);
end;

procedure TestTCrosstalkCancellation32.TestProcessStereo;
var
  Right  : Single;
  Left   : Single;
  Sum    : array [0..1] of Single;
  Sample : Integer;
begin
 with FCrosstalkCancellation32 do
  begin
   Left := 1;
   Right := 0;
   ProcessStereo(Left, Right);
   Sum[0] := abs(Left);
   Sum[1] := abs(Right);
   for Sample := 1 to 1024 do
    begin
     Left := 0;
     Right := 0;
     ProcessStereo(Left, Right);
     Sum[0] := Sum[0] + abs(Left);
     Sum[1] := Sum[1] + abs(Right);
    end;

   CheckTrue(Sum[0] > Sum[1]);
   CheckTrue(Sum[1] > 0);
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTCrosstalkCancellation32.Suite);
end.

