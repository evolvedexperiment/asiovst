unit TestDAV_DspAmbience;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_DspFilterLinkwitzRiley, DAV_Common, DAV_DSPFilterButterworth, 
  DAV_DspFilter, DAV_DspAmbience;

type
  // Test methods for class TCustomAmbience
  
  TestTAmbience = class(TTestCase)
  strict private
    FAmbience: TAmbience;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcess;
    procedure TestProcess32;
  end;

implementation

uses
  Math, SysUtils;

procedure TestTAmbience.SetUp;
begin
 FAmbience := TAmbience.Create;
end;

procedure TestTAmbience.TearDown;
begin
 FreeAndNil(FAmbience);
end;

procedure TestTAmbience.TestProcess;
var
  ReturnValue : Single;
  Sample      : Integer;
const
  CSampleFrames = 1000;
begin
 FAmbience.Process(1);

 for Sample := 0 to CSampleFrames - 1
  do ReturnValue := FAmbience.Process(0);

 CheckFalse(IsNan(ReturnValue), 'Return value is not a number');
end;

procedure TestTAmbience.TestProcess32;
var
  Right  : Single;
  Left   : Single;
  Sample : Integer;
const
  CSampleFrames = 1000;  
begin
 Left := 1;
 Right := 1;
 FAmbience.Process(Left, Right);

 Left := 0;
 Right := 0;
 for Sample := 0 to CSampleFrames - 1
  do FAmbience.Process(Left, Right);

 CheckFalse(IsNan(Left), 'Left channel result is not a number');
 CheckFalse(IsNan(Right), 'Right channel result is not a number');
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTAmbience.Suite);
end.

