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
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableAllpassFilter
  TestTAutomatableAllpassFilter = class(TTestCase)
  strict private
    FAutomatableAllpassFilter: TAutomatableAllpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowShelfFilter
  TestTAutomatableLowShelfFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfFilter: TAutomatableLowShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowShelfAFilter
  TestTAutomatableLowShelfAFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfAFilter: TAutomatableLowShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowShelfBFilter
  TestTAutomatableLowShelfBFilter = class(TTestCase)
  strict private
    FAutomatableLowShelfBFilter: TAutomatableLowShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighShelfFilter
  TestTAutomatableHighShelfFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfFilter: TAutomatableHighShelfFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighShelfAFilter
  TestTAutomatableHighShelfAFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfAFilter: TAutomatableHighShelfAFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighShelfBFilter
  TestTAutomatableHighShelfBFilter = class(TTestCase)
  strict private
    FAutomatableHighShelfBFilter: TAutomatableHighShelfBFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableHighcutFilter
  TestTAutomatableHighcutFilter = class(TTestCase)
  strict private
    FAutomatableHighcutFilter: TAutomatableHighcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableLowcutFilter
  TestTAutomatableLowcutFilter = class(TTestCase)
  strict private
    FAutomatableLowcutFilter: TAutomatableLowcutFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableBandpassFilter
  TestTAutomatableBandpassFilter = class(TTestCase)
  strict private
    FAutomatableBandpassFilter: TAutomatableBandpassFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
  end;

  // Test methods for class TAutomatableNotchFilter
  TestTAutomatableNotchFilter = class(TTestCase)
  strict private
    FAutomatableNotchFilter: TAutomatableNotchFilter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
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
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(abs(ProcessSample(CHalf32) - dB_to_Amp(Gain) * CHalf32) < 1E-7, 'ProcessSample(0.5)  <> 0.5');
   CheckTrue(abs(ProcessSample(1.0) - dB_to_Amp(Gain)) < 1E-7, 'ProcessSample(1)  <> 1');
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

procedure TestTAutomatablePeakFilter.TestProcessSample;
begin
 with FAutomatablePeakFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableAllpassFilter.TestProcessSample;
begin
 with FAutomatableAllpassFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableLowShelfFilter.TestProcessSample;
begin
 with FAutomatableLowShelfFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableLowShelfAFilter.TestProcessSample;
begin
 with FAutomatableLowShelfAFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableLowShelfBFilter.TestProcessSample;
begin
 with FAutomatableLowShelfBFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableHighShelfFilter.TestProcessSample;
begin
 with FAutomatableHighShelfFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableHighShelfAFilter.TestProcessSample;
begin
 with FAutomatableHighShelfAFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableHighShelfBFilter.TestProcessSample;
begin
 with FAutomatableHighShelfBFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableHighcutFilter.TestProcessSample;
begin
 with FAutomatableHighcutFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableLowcutFilter.TestProcessSample;
begin
 with FAutomatableLowcutFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableBandpassFilter.TestProcessSample;
begin
 with FAutomatableBandpassFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

procedure TestTAutomatableNotchFilter.TestProcessSample;
begin
 with FAutomatableNotchFilter do
  begin
   Gain := Amp_to_dB(CHalf32);
   Frequency := 1000;
   Bandwidth := 2;
   CheckEquals(ProcessSample(0.0), 0, 'ProcessSample(0.0) <> 0');
   CheckTrue(ProcessSample(1) > dB_to_Amp(Gain), 'ProcessSample(1) > 1');
  end;
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

