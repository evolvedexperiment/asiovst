unit TestDAV_DspExciter;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, DAV_DspPolyphaseUpsampler, DAV_DspExciter, 
  DAV_DspFilterLinkwitzRiley, DAV_Common, Classes, DAV_DSPFilterButterworth, 
  DAV_DspCommon, DAV_DspDynamics, DAV_DspPolyphaseDownsampler, 
  DAV_DspLightweightDynamics, DAV_DspFilter;

type
  // Test methods for class TestTExciter
  TestTExciter = class(TTestCase)
  strict private
    FExciter: TExciter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample32;
    procedure TestProcessSample64;
  end;

implementation

uses
  SysUtils;

procedure TestTExciter.SetUp;
begin
  FExciter := TExciter.Create;
end;

procedure TestTExciter.TearDown;
begin
 FreeAndNil(FExciter);
end;

procedure TestTExciter.TestProcessSample32;
begin
 FExciter.ProcessSample32(1);
end;

procedure TestTExciter.TestProcessSample64;
begin
 FExciter.ProcessSample64(1);
end;

initialization
  RegisterTest(TestTExciter.Suite);
  
end.

