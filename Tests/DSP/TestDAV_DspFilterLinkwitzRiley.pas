unit TestDAV_DspFilterLinkwitzRiley;
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
  TestFramework, Classes, DAV_DspButterworthFilter, DAV_DspFilterLinkwitzRiley,
  DAV_Common, DAV_DspCommon, DAV_DspFilter;

type
  // Test methods for class TLinkwitzRiley
  TestTLinkwitzRiley = class(TTestCase)
  strict private
    FLinkwitzRiley: TLinkwitzRiley;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSampleSingle;
    procedure TestProcessSampleDouble;
  end;

implementation

uses
  SysUtils;

procedure TestTLinkwitzRiley.SetUp;
begin
 FLinkwitzRiley := TLinkwitzRiley.Create;
end;

procedure TestTLinkwitzRiley.TearDown;
begin
 FreeAndNil(FLinkwitzRiley);
end;

procedure TestTLinkwitzRiley.TestProcessSampleSingle;
var
  High  : Single;
  Low   : Single;
begin
 with FLinkwitzRiley do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   ProcessSample(1, Low, High);
   CheckTrue(Low > 0);
   CheckTrue(High > 0);
  end;
end;

procedure TestTLinkwitzRiley.TestProcessSampleDouble;
var
  High  : Double;
  Low   : Double;
begin
 with FLinkwitzRiley do
  begin
   SampleRate := 44100;
   Frequency  := 1000;
   ProcessSample(1, Low, High);
   CheckTrue(Low > 0);
   CheckTrue(High > 0);
  end;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTLinkwitzRiley.Suite);
end.

