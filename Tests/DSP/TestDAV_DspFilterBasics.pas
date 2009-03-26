unit TestDAV_DspFilterBasics;
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
  TestFramework, DAV_Common, Classes, DAV_DspCommon, DAV_Complex, DAV_DspFilterBasics,
  DAV_DspFilter;

type
  // Test methods for class TBasicGainFilter
  TestTBasicGainFilter = class(TTestCase)
  strict private
    FBasicGainFilter: TBasicGainFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TBasicPeakFilter
  TestTBasicPeakFilter = class(TTestCase)
  strict private
    FBasicPeakFilter: TBasicPeakFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicAllpassFilter
  TestTBasicAllpassFilter = class(TTestCase)
  strict private
    FBasicAllpassFilter: TBasicAllpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicLowShelfFilter
  TestTBasicLowShelfFilter = class(TTestCase)
  strict private
    FBasicLowShelfFilter: TBasicLowShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicLowShelfAFilter
  TestTBasicLowShelfAFilter = class(TTestCase)
  strict private
    FBasicLowShelfAFilter: TBasicLowShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicLowShelfBFilter
  TestTBasicLowShelfBFilter = class(TTestCase)
  strict private
    FBasicLowShelfBFilter: TBasicLowShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicHighShelfFilter
  TestTBasicHighShelfFilter = class(TTestCase)
  strict private
    FBasicHighShelfFilter: TBasicHighShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicHighShelfAFilter
  TestTBasicHighShelfAFilter = class(TTestCase)
  strict private
    FBasicHighShelfAFilter: TBasicHighShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicHighShelfBFilter
  TestTBasicHighShelfBFilter = class(TTestCase)
  strict private
    FBasicHighShelfBFilter: TBasicHighShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicHighcutFilter
  TestTBasicHighcutFilter = class(TTestCase)
  strict private
    FBasicHighcutFilter: TBasicHighcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicLowcutFilter
  TestTBasicLowcutFilter = class(TTestCase)
  strict private
    FBasicLowcutFilter: TBasicLowcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicBandpassFilter
  TestTBasicBandpassFilter = class(TTestCase)
  strict private
    FBasicBandpassFilter: TBasicBandpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TBasicNotchFilter
  TestTBasicNotchFilter = class(TTestCase)
  strict private
    FBasicNotchFilter: TBasicNotchFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  SysUtils;

{ TestTBasicGainFilter }

procedure TestTBasicGainFilter.SetUp;
begin
  FBasicGainFilter := TBasicGainFilter.Create;
end;

procedure TestTBasicGainFilter.TearDown;
begin
  FBasicGainFilter.Free;
  FBasicGainFilter := nil;
end;

procedure TestTBasicGainFilter.TestProcessSample;
begin
 with FBasicGainFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   CheckEquals(ProcessSample(0.0), 0);
   CheckTrue(abs(ProcessSample(CHalf32) - dB_to_Amp(Gain) * CHalf32) < 1E-9, 'ProcessSample(0.5)  <> 0.5');
   CheckTrue(abs(ProcessSample(1.0) - dB_to_Amp(Gain)) < 1E-9, 'ProcessSample(1)  <> 1');
  end;
end;


{ TestTBasicPeakFilter }

procedure TestTBasicPeakFilter.SetUp;
begin
  FBasicPeakFilter := TBasicPeakFilter.Create;
end;

procedure TestTBasicPeakFilter.TearDown;
begin
 FreeAndNil(FBasicPeakFilter);
end;        


{ TestTBasicAllpassFilter }

procedure TestTBasicAllpassFilter.SetUp;
begin
  FBasicAllpassFilter := TBasicAllpassFilter.Create;
end;

procedure TestTBasicAllpassFilter.TearDown;
begin
 FreeAndNil(FBasicAllpassFilter);
end;


{ TestTBasicLowShelfFilter }

procedure TestTBasicLowShelfFilter.SetUp;
begin
 FBasicLowShelfFilter := TBasicLowShelfFilter.Create;
end;

procedure TestTBasicLowShelfFilter.TearDown;
begin
 FreeAndNil(FBasicLowShelfFilter);
end;


{ TestTBasicLowShelfAFilter }

procedure TestTBasicLowShelfAFilter.SetUp;
begin
 FBasicLowShelfAFilter := TBasicLowShelfAFilter.Create;
end;

procedure TestTBasicLowShelfAFilter.TearDown;
begin
 FreeAndNil(FBasicLowShelfAFilter);
end;


{ TestTBasicLowShelfBFilter }

procedure TestTBasicLowShelfBFilter.SetUp;
begin
 FBasicLowShelfBFilter := TBasicLowShelfBFilter.Create;
end;

procedure TestTBasicLowShelfBFilter.TearDown;
begin
 FreeAndNil(FBasicLowShelfBFilter);
end;


{ TestTBasicHighShelfFilter }

procedure TestTBasicHighShelfFilter.SetUp;
begin
 FBasicHighShelfFilter := TBasicHighShelfFilter.Create;
end;

procedure TestTBasicHighShelfFilter.TearDown;
begin
 FreeAndNil(FBasicHighShelfFilter);
end;


{ TestTBasicHighShelfAFilter }

procedure TestTBasicHighShelfAFilter.SetUp;
begin
 FBasicHighShelfAFilter := TBasicHighShelfAFilter.Create;
end;

procedure TestTBasicHighShelfAFilter.TearDown;
begin
 FreeAndNil(FBasicHighShelfAFilter);
end;


{ TestTBasicHighShelfBFilter }

procedure TestTBasicHighShelfBFilter.SetUp;
begin
 FBasicHighShelfBFilter := TBasicHighShelfBFilter.Create;
end;

procedure TestTBasicHighShelfBFilter.TearDown;
begin
 FreeAndNil(FBasicHighShelfBFilter);
end;


{ TestTBasicHighcutFilter }

procedure TestTBasicHighcutFilter.SetUp;
begin
 FBasicHighcutFilter := TBasicHighcutFilter.Create;
end;

procedure TestTBasicHighcutFilter.TearDown;
begin
 FreeAndNil(FBasicHighcutFilter);
end;


{ TestTBasicLowcutFilter }

procedure TestTBasicLowcutFilter.SetUp;
begin
 FBasicLowcutFilter := TBasicLowcutFilter.Create;
end;

procedure TestTBasicLowcutFilter.TearDown;
begin
 FreeAndNil(FBasicLowcutFilter);
end;


{ TestTBasicBandpassFilter }

procedure TestTBasicBandpassFilter.SetUp;
begin
 FBasicBandpassFilter := TBasicBandpassFilter.Create;
end;

procedure TestTBasicBandpassFilter.TearDown;
begin
 FreeAndNil(FBasicBandpassFilter);
end;


{ TestTBasicNotchFilter }

procedure TestTBasicNotchFilter.SetUp;
begin
 FBasicNotchFilter := TBasicNotchFilter.Create;
end;

procedure TestTBasicNotchFilter.TearDown;
begin
 FreeAndNil(FBasicNotchFilter);
end;


initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTBasicGainFilter.Suite);
  RegisterTest(TestTBasicPeakFilter.Suite);
  RegisterTest(TestTBasicAllpassFilter.Suite);
  RegisterTest(TestTBasicLowShelfFilter.Suite);
  RegisterTest(TestTBasicLowShelfAFilter.Suite);
  RegisterTest(TestTBasicLowShelfBFilter.Suite);
  RegisterTest(TestTBasicHighShelfFilter.Suite);
  RegisterTest(TestTBasicHighShelfAFilter.Suite);
  RegisterTest(TestTBasicHighShelfBFilter.Suite);
  RegisterTest(TestTBasicHighcutFilter.Suite);
  RegisterTest(TestTBasicLowcutFilter.Suite);
  RegisterTest(TestTBasicBandpassFilter.Suite);
  RegisterTest(TestTBasicNotchFilter.Suite);
end.

