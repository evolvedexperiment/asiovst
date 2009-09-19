unit TestDAV_DspCrosstalkSimulator;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_DspFilterBasics, DAV_DspFilter, DAV_DspCrosstalkSimulator,
  DAV_Common, DAV_DspCommon;
type
  // Test methods for class TIIRCrosstalkSimulator
  
  TestTIIRCrosstalkSimulator = class(TTestCase)
  strict private
    FIIRCrosstalkSimulator: TIIRCrosstalkSimulator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess32;
    procedure TestProcess64;
  end;

implementation

uses
  SysUtils;

{ TestTIIRCrosstalkSimulator }

procedure TestTIIRCrosstalkSimulator.SetUp;
begin
 FIIRCrosstalkSimulator := TIIRCrosstalkSimulator.Create;
end;

procedure TestTIIRCrosstalkSimulator.TearDown;
begin
 FreeAndNil(FIIRCrosstalkSimulator);
end;

procedure TestTIIRCrosstalkSimulator.TestProcess32;
var
  Right  : Single;
  Left   : Single;
  Sum    : array [0..1] of Single;
  Sample : Integer;
begin
 with FIIRCrosstalkSimulator do
  begin
   Left := 1;
   Right := 0;
   ProcessSample(Left, Right);
   Sum[0] := abs(Left);
   Sum[1] := abs(Right);
   for Sample := 1 to 1024 do
    begin
     Left := 0;
     Right := 0;
     ProcessSample(Left, Right);
     Sum[0] := Sum[0] + abs(Left);
     Sum[1] := Sum[1] + abs(Right);
    end;

   CheckTrue(Sum[0] > Sum[1]);
   CheckTrue(Sum[1] > 0);
  end;
end;

procedure TestTIIRCrosstalkSimulator.TestProcess64;
var
  Right  : Double;
  Left   : Double;
  Sum    : array [0..1] of Double;
  Sample : Integer;
begin
 with FIIRCrosstalkSimulator do
  begin
   Left := 1;
   Right := 0;
   ProcessSample(Left, Right);
   Sum[0] := abs(Left);
   Sum[1] := abs(Right);
   for Sample := 1 to 1024 do
    begin
     Left := 0;
     Right := 0;
     ProcessSample(Left, Right);
     Sum[0] := Sum[0] + abs(Left);
     Sum[1] := Sum[1] + abs(Right);
    end;

   CheckTrue(Sum[0] > Sum[1]);
   CheckTrue(Sum[1] > 0);
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTIIRCrosstalkSimulator.Suite);
end.

