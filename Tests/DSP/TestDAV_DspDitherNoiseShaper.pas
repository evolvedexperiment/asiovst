unit TestDAV_DspDitherNoiseShaper;
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
  TestFramework, DAV_DspDitherNoiseShaper, DAV_Common, DAV_DspCommon;

type
  // Test methods for class TDitherNoiseShaper
  TestTDitherNoiseShaper = class(TTestCase)
  strict private
    FDitherNoiseShaper: TDitherNoiseShaper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessInteger;
    procedure TestProcessFloat;
    procedure TestReset;
  end;

implementation

uses
  SysUtils;

procedure TestTDitherNoiseShaper.SetUp;
begin
 FDitherNoiseShaper := TDitherNoiseShaper.Create;
end;

procedure TestTDitherNoiseShaper.TearDown;
begin
 FreeAndNil(FDitherNoiseShaper);
end;

procedure TestTDitherNoiseShaper.TestProcessInteger;
var
  ReturnValue : Integer;
  Input       : Double;
begin
 with FDitherNoiseShaper do
  begin
   BitDepth    := 8;
   Input       := 1 / (2 shl BitDepth - 1);
   DitherType  := dtor2Sc;
   ReturnValue := ProcessInteger(Input);

   // Validate method results
   CheckTrue(ReturnValue <> (2 shl BitDepth - 1));
  end;
end;

procedure TestTDitherNoiseShaper.TestProcessFloat;
var
  ReturnValue : Double;
  Input       : Double;
begin
 with FDitherNoiseShaper do
  begin
   BitDepth    := 8;
   Input       := 1 / (2 shl BitDepth - 1);
   DitherType  := dtor2Sc;
   ReturnValue := ProcessFloat(Input);

   // Validate method results
   CheckTrue(Input <> ReturnValue);
  end;
end;

procedure TestTDitherNoiseShaper.TestReset;
begin
 FDitherNoiseShaper.Reset;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTDitherNoiseShaper.Suite);
end.

