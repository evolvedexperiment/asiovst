unit TestDAV_DspFilterBasicsAutomatable;
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
  TestFramework, DAV_DspFilterBasicsAutomatable, Classes, DAV_DspCommon,
  DAV_Complex, DAV_Common, DAV_DspFilter;

type
  // Test methods for class TAutomatableGainFilter
  TestTAutomatableGainFilter = class(TTestCase)
  strict private
    FAutomatableGainFilter: TAutomatableGainFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatablePeakFilter
  TestTAutomatablePeakFilter = class(TTestCase)
  strict private
    FAutomatablePeakFilter: TAutomatablePeakFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableAllpassFilter
  TestTAutomatableAllpassFilter = class(TTestCase)
  strict private
    FAutomatableAllpassFilter: TAutomatableAllpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableLowShelfFilter
  TestTAutomatableLowShelfFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfFilter: TAutomatableLowShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableLowShelfAFilter
  TestTAutomatableLowShelfAFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfAFilter: TAutomatableLowShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableLowShelfBFilter
  TestTAutomatableLowShelfBFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfBFilter: TAutomatableLowShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableHighShelfFilter
  TestTAutomatableHighShelfFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfFilter: TAutomatableHighShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableHighShelfAFilter
  TestTAutomatableHighShelfAFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfAFilter: TAutomatableHighShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableHighShelfBFilter
  TestTAutomatableHighShelfBFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfBFilter: TAutomatableHighShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableHighcutFilter
  TestTAutomatableHighcutFilter = class(TTestCase)
  strict private
    FAutomatableHighcutFilter: TAutomatableHighcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableLowcutFilter
  TestTAutomatableLowcutFilter = class(TTestCase)
  strict private
    FAutomatableLowcutFilter: TAutomatableLowcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableBandpassFilter
  TestTAutomatableBandpassFilter = class(TTestCase)
  strict private
    FAutomatableBandpassFilter: TAutomatableBandpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TAutomatableNotchFilter
  TestTAutomatableNotchFilter = class(TTestCase)
  strict private
    FAutomatableNotchFilter: TAutomatableNotchFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  SysUtils;

{ TestTAutomatableGainFilter }

procedure TestTAutomatableGainFilter.SetUp;
begin
  FAutomatableGainFilter := TAutomatableGainFilter.Create;
end;

procedure TestTAutomatableGainFilter.TearDown;
begin
 FreeAndNil(FAutomatableGainFilter);
end;

procedure TestTAutomatableGainFilter.TestProcessSample;
begin
 with FAutomatableGainFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   CheckEquals(ProcessSample(0.0), 0);
   CheckTrue(abs(ProcessSample(CHalf32) - dB_to_Amp(Gain) * CHalf32) < 1E-9, 'ProcessSample(0.5)  <> 0.5');
   CheckTrue(abs(ProcessSample(1.0) - dB_to_Amp(Gain)) < 1E-9, 'ProcessSample(1)  <> 1');
  end;
end;


{ TestTAutomatablePeakFilter }

procedure TestTAutomatablePeakFilter.SetUp;
begin
 FAutomatablePeakFilter := TAutomatablePeakFilter.Create;
end;

procedure TestTAutomatablePeakFilter.TearDown;
begin
 FreeAndNil(FAutomatablePeakFilter);
end;


{ TestTAutomatableAllpassFilter }

procedure TestTAutomatableAllpassFilter.SetUp;
begin
 FAutomatableAllpassFilter := TAutomatableAllpassFilter.Create;
end;

procedure TestTAutomatableAllpassFilter.TearDown;
begin
 FreeAndNil(FAutomatableAllpassFilter);
end;


{ TestTAutomatableLowShelfFilter }

procedure TestTAutomatableLowShelfFilter.SetUp;
begin
 FAutomatableLowShelfFilter := TAutomatableLowShelfFilter.Create;
end;

procedure TestTAutomatableLowShelfFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowShelfFilter);
end;


{ TestTAutomatableLowShelfAFilter }

procedure TestTAutomatableLowShelfAFilter.SetUp;
begin
 FAutomatableLowShelfAFilter := TAutomatableLowShelfAFilter.Create;
end;

procedure TestTAutomatableLowShelfAFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowShelfAFilter);
end;


{ TestTAutomatableLowShelfBFilter }

procedure TestTAutomatableLowShelfBFilter.SetUp;
begin
 FAutomatableLowShelfBFilter := TAutomatableLowShelfBFilter.Create;
end;

procedure TestTAutomatableLowShelfBFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowShelfBFilter);
end;


{ TestTAutomatableHighShelfFilter }

procedure TestTAutomatableHighShelfFilter.SetUp;
begin
 FAutomatableHighShelfFilter := TAutomatableHighShelfFilter.Create;
end;

procedure TestTAutomatableHighShelfFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighShelfFilter);
end;


{ TestTAutomatableHighShelfAFilter }

procedure TestTAutomatableHighShelfAFilter.SetUp;
begin
 FAutomatableHighShelfAFilter := TAutomatableHighShelfAFilter.Create;
end;

procedure TestTAutomatableHighShelfAFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighShelfAFilter);
end;


{ TestTAutomatableHighShelfBFilter }

procedure TestTAutomatableHighShelfBFilter.SetUp;
begin
 FAutomatableHighShelfBFilter := TAutomatableHighShelfBFilter.Create;
end;

procedure TestTAutomatableHighShelfBFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighShelfBFilter);
end;


{ TestTAutomatableHighcutFilter }

procedure TestTAutomatableHighcutFilter.SetUp;
begin
 FAutomatableHighcutFilter := TAutomatableHighcutFilter.Create;
end;

procedure TestTAutomatableHighcutFilter.TearDown;
begin
 FreeAndNil(FAutomatableHighcutFilter);
end;


{ TestTAutomatableLowcutFilter }

procedure TestTAutomatableLowcutFilter.SetUp;
begin
  FAutomatableLowcutFilter := TAutomatableLowcutFilter.Create;
end;

procedure TestTAutomatableLowcutFilter.TearDown;
begin
 FreeAndNil(FAutomatableLowcutFilter);
end;


{ TestTAutomatableBandpassFilter }

procedure TestTAutomatableBandpassFilter.SetUp;
begin
  FAutomatableBandpassFilter := TAutomatableBandpassFilter.Create;
end;

procedure TestTAutomatableBandpassFilter.TearDown;
begin
 FreeAndNil(FAutomatableBandpassFilter);
end;


{ TestTAutomatableNotchFilter }

procedure TestTAutomatableNotchFilter.SetUp;
begin
  FAutomatableNotchFilter := TAutomatableNotchFilter.Create;
end;

procedure TestTAutomatableNotchFilter.TearDown;
begin
 FreeAndNil(FAutomatableNotchFilter);
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTAutomatableGainFilter.Suite);
  RegisterTest(TestTAutomatablePeakFilter.Suite);
  RegisterTest(TestTAutomatableAllpassFilter.Suite);
  RegisterTest(TestTAutomatableLowShelfFilter.Suite);
  RegisterTest(TestTAutomatableLowShelfAFilter.Suite);
  RegisterTest(TestTAutomatableLowShelfBFilter.Suite);
  RegisterTest(TestTAutomatableHighShelfFilter.Suite);
  RegisterTest(TestTAutomatableHighShelfAFilter.Suite);
  RegisterTest(TestTAutomatableHighShelfBFilter.Suite);
  RegisterTest(TestTAutomatableHighcutFilter.Suite);
  RegisterTest(TestTAutomatableLowcutFilter.Suite);
  RegisterTest(TestTAutomatableBandpassFilter.Suite);
  RegisterTest(TestTAutomatableNotchFilter.Suite);
end.

