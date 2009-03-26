unit TestDAV_DspDynamics;
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
  TestFramework, DAV_DspDynamics, DAV_Common, DAV_DspCommon,
  DAV_DspButterworthFilter;

type
  // Test methods for class TSimpleDirectGate
  TestTSimpleDirectGate = class(TTestCase)
  strict private
    FSimpleDirectGate: TSimpleDirectGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
    procedure TestProcessSample;
  end;

  // Test methods for class TSoftDirectGate
  TestTSoftDirectGate = class(TTestCase)
  strict private
    FSoftDirectGate: TSoftDirectGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestInputSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TBrickwallLimiter
  TestTBrickwallLimiter = class(TTestCase)
  strict private
    FBrickwallLimiter: TBrickwallLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
    procedure TestCharacteristicCurve;
  end;

  // Test methods for class TSoftBrickwallLimiter
  TestTSoftBrickwallLimiter = class(TTestCase)
  strict private
    FSoftBrickwallLimiter: TSoftBrickwallLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleSoftBrickwallLimiter
  TestTSimpleSoftBrickwallLimiter = class(TTestCase)
  strict private
    FSimpleSoftBrickwallLimiter: TSimpleSoftBrickwallLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TLimiter
  TestTLimiter = class(TTestCase)
  strict private
    FLimiter: TLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
    procedure TestCharacteristicCurve;
  end;

  // Test methods for class TSoftKneeLimiter
  TestTSoftKneeLimiter = class(TTestCase)
  strict private
    FSoftKneeLimiter: TSoftKneeLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleSoftKneeLimiter
  TestTSimpleSoftKneeLimiter = class(TTestCase)
  strict private
    FSimpleSoftKneeLimiter: TSimpleSoftKneeLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestProcessSample;
  end;

  // Test methods for class TClassicGate
  TestTClassicGate = class(TTestCase)
  strict private
    FClassicGate: TClassicGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
    procedure TestProcessSample;
  end;

  // Test methods for class TClassicSoftRangeGate
  TestTClassicSoftRangeGate = class(TTestCase)
  strict private
    FClassicSoftRangeGate: TClassicSoftRangeGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
  end;

  // Test methods for class TClassicSoftKneeGate
  TestTClassicSoftKneeGate = class(TTestCase)
  strict private
    FClassicSoftKneeGate: TClassicSoftKneeGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
  end;

  // Test methods for class TAdvancedGate
  TestTAdvancedGate = class(TTestCase)
  strict private
    FAdvancedGate: TAdvancedGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInputSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleCompressor
  TestTSimpleCompressor = class(TTestCase)
  strict private
    FSimpleCompressor: TSimpleCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSoftKneeCompressor
  TestTSoftKneeCompressor = class(TTestCase)
  strict private
    FSoftKneeCompressor: TSoftKneeCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleFeedbackCompressor
  TestTSimpleFeedbackCompressor = class(TTestCase)
  strict private
    FSimpleFeedbackCompressor: TSimpleFeedbackCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
    procedure TestCharacteristicCurve;
  end;

  // Test methods for class TSoftKneeFeedbackCompressor
  TestTSoftKneeFeedbackCompressor = class(TTestCase)
  strict private
    FSoftKneeFeedbackCompressor: TSoftKneeFeedbackCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleRMSCompressor
  TestTSimpleRMSCompressor = class(TTestCase)
  strict private
    FSimpleRMSCompressor: TSimpleRMSCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestInputSample;
  end;

  // Test methods for class TCompressor
  TestTCompressor = class(TTestCase)
  strict private
    FCompressor: TCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInputSample;
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

implementation

uses
  SysUtils;

procedure TestTSimpleDirectGate.SetUp;
begin
 FSimpleDirectGate := TSimpleDirectGate.Create;
end;

procedure TestTSimpleDirectGate.TearDown;
begin
 FreeAndNil(FSimpleDirectGate);
end;

procedure TestTSimpleDirectGate.TestTranslatePeakToGain;
begin
 with FSimpleDirectGate do
  begin
   Threshold_dB := -10;
   CheckTrue(abs(TranslatePeakToGain(0.5) - 0.5) < 1E-15);
  end;
end;

procedure TestTSimpleDirectGate.TestInputSample;
begin
 with FSimpleDirectGate do
  begin
   Threshold_dB := -6;
   InputSample(0);
   CheckEquals(GainSample(0), 0);
   InputSample(1);
   CheckEquals(GainSample(1), 1);
   InputSample(0.1);
   CheckEquals(GainSample(1), 0);
  end;
end;

procedure TestTSimpleDirectGate.TestProcessSample;
begin
 with FSimpleDirectGate do
  begin
   Threshold_dB := -6;
   CheckEquals(ProcessSample(0), 0);
   CheckEquals(ProcessSample(1), 1);
   CheckEquals(ProcessSample(0.1), 0);
  end;
end;


{ TestTSoftDirectGate }

procedure TestTSoftDirectGate.SetUp;
begin
 FSoftDirectGate := TSoftDirectGate.Create;
end;

procedure TestTSoftDirectGate.TearDown;
begin
 FreeAndNil(FSoftDirectGate);
end;

procedure TestTSoftDirectGate.TestProcessSample;
begin
 with FSoftDirectGate do
  begin
   Threshold_dB := -6;
   CheckEquals(ProcessSample(0), 0);
   CheckEquals(ProcessSample(1), 1);
   CheckEquals(ProcessSample(0.1), 0);
  end;
end;

procedure TestTSoftDirectGate.TestInputSample;
begin
 with FSoftDirectGate do
  begin
   Threshold_dB := -6;
   InputSample(0);
   CheckEquals(GainSample(0), 0);
   InputSample(1);
   CheckEquals(GainSample(1), 1);
   InputSample(0.1);
   CheckEquals(GainSample(1), 0);
  end;
end;

procedure TestTSoftDirectGate.TestTranslatePeakToGain;
begin
 with FSoftDirectGate do
  begin
   Threshold_dB := -10;
   CheckTrue(abs(TranslatePeakToGain(0.5) - 0.5) < 1E-15);
  end;
end;


{ TestTBrickwallLimiter }

procedure TestTBrickwallLimiter.SetUp;
begin
 FBrickwallLimiter := TBrickwallLimiter.Create;
end;

procedure TestTBrickwallLimiter.TearDown;
begin
 FreeAndNil(FBrickwallLimiter);
end;

procedure TestTBrickwallLimiter.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FBrickwallLimiter do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample(0), 0);
   CheckTrue(abs(ProcessSample(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(abs(ProcessSample(1) - ThresholdFactor) < 1E-5);
  end;
end;

procedure TestTBrickwallLimiter.TestTranslatePeakToGain;
begin
 with FBrickwallLimiter do
  begin
   Threshold_dB := 0;
   CheckTrue(abs(TranslatePeakToGain(0.5) - 0.5) < 1E-15);
  end;
end;

procedure TestTBrickwallLimiter.TestCharacteristicCurve;
begin
 with FBrickwallLimiter do
  begin
   Threshold_dB := -10;
   CheckTrue(abs(CharacteristicCurve(0) + 10) < 1E-5);
   CheckTrue(abs(CharacteristicCurve(-10) + 10) < 1E-5);
   CheckTrue(abs(CharacteristicCurve(-20) + 20) < 1E-5);
  end;
end;


{ TestTSoftBrickwallLimiter }

procedure TestTSoftBrickwallLimiter.SetUp;
begin
 FSoftBrickwallLimiter := TSoftBrickwallLimiter.Create;
end;

procedure TestTSoftBrickwallLimiter.TearDown;
begin
 FreeAndNil(FSoftBrickwallLimiter);
end;

procedure TestTSoftBrickwallLimiter.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftBrickwallLimiter.ProcessSample(Input);
  // TODO: Validate method results
end;

procedure TestTSoftBrickwallLimiter.TestTranslatePeakToGain;
begin
 with FSoftBrickwallLimiter do
  begin
   Threshold_dB := 0;
   CheckTrue(abs(TranslatePeakToGain(0.01) - 0.01) < 1E-15);
  end;
end;


{ TestTSimpleSoftBrickwallLimiter }

procedure TestTSimpleSoftBrickwallLimiter.SetUp;
begin
 FSimpleSoftBrickwallLimiter := TSimpleSoftBrickwallLimiter.Create;
end;

procedure TestTSimpleSoftBrickwallLimiter.TearDown;
begin
 FreeAndNil(FSimpleSoftBrickwallLimiter);
end;

procedure TestTSimpleSoftBrickwallLimiter.TestTranslatePeakToGain;
begin
 with FSimpleSoftBrickwallLimiter do
  begin
   Threshold_dB := 0;
   CheckTrue(abs(TranslatePeakToGain(0.01) - 0.01) < 1E-15);
  end;
end;


{ TestTLimiter }

procedure TestTLimiter.SetUp;
begin
  FLimiter := TLimiter.Create;
end;

procedure TestTLimiter.TearDown;
begin
 FreeAndNil(FLimiter);
end;

procedure TestTLimiter.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
 // TODO: Setup method call parameters
 ReturnValue := FLimiter.ProcessSample(Input);
 // TODO: Validate method results
end;

procedure TestTLimiter.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FLimiter.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTLimiter.TestCharacteristicCurve;
var
  ReturnValue: Double;
  InputLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FLimiter.CharacteristicCurve(InputLevel);
  // TODO: Validate method results
end;


{ TestTSoftKneeLimiter }

procedure TestTSoftKneeLimiter.SetUp;
begin
  FSoftKneeLimiter := TSoftKneeLimiter.Create;
end;

procedure TestTSoftKneeLimiter.TearDown;
begin
 FreeAndNil(FSoftKneeLimiter);
end;

procedure TestTSoftKneeLimiter.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeLimiter.ProcessSample(Input);
  // TODO: Validate method results
end;

procedure TestTSoftKneeLimiter.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeLimiter.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSimpleSoftKneeLimiter }

procedure TestTSimpleSoftKneeLimiter.SetUp;
begin
  FSimpleSoftKneeLimiter := TSimpleSoftKneeLimiter.Create;
end;

procedure TestTSimpleSoftKneeLimiter.TearDown;
begin
 FreeAndNil(FSimpleSoftKneeLimiter);
end;

procedure TestTSimpleSoftKneeLimiter.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleSoftKneeLimiter.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTSimpleSoftKneeLimiter.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleSoftKneeLimiter.ProcessSample(Input);
  // TODO: Validate method results
end;


{ TestTClassicGate }

procedure TestTClassicGate.SetUp;
begin
  FClassicGate := TClassicGate.Create;
end;

procedure TestTClassicGate.TearDown;
begin
 FreeAndNil(FClassicGate);
end;

procedure TestTClassicGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FClassicGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTClassicGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FClassicGate.InputSample(Input);
  // TODO: Validate method results
end;

procedure TestTClassicGate.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FClassicGate.ProcessSample(Input);
  // TODO: Validate method results
end;


{ TestTClassicSoftRangeGate }

procedure TestTClassicSoftRangeGate.SetUp;
begin
  FClassicSoftRangeGate := TClassicSoftRangeGate.Create;
end;

procedure TestTClassicSoftRangeGate.TearDown;
begin
 FreeAndNil(FClassicSoftRangeGate);
end;

procedure TestTClassicSoftRangeGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FClassicSoftRangeGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTClassicSoftRangeGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FClassicSoftRangeGate.InputSample(Input);
  // TODO: Validate method results
end;


{ TestTClassicSoftKneeGate }

procedure TestTClassicSoftKneeGate.SetUp;
begin
  FClassicSoftKneeGate := TClassicSoftKneeGate.Create;
end;

procedure TestTClassicSoftKneeGate.TearDown;
begin
 FreeAndNil(FClassicSoftKneeGate);
end;

procedure TestTClassicSoftKneeGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FClassicSoftKneeGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTClassicSoftKneeGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FClassicSoftKneeGate.InputSample(Input);
  // TODO: Validate method results
end;


{ TestTAdvancedGate }

procedure TestTAdvancedGate.SetUp;
begin
  FAdvancedGate := TAdvancedGate.Create;
end;

procedure TestTAdvancedGate.TearDown;
begin
 FreeAndNil(FAdvancedGate);
end;

procedure TestTAdvancedGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FAdvancedGate.InputSample(Input);
  // TODO: Validate method results
end;

procedure TestTAdvancedGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FAdvancedGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSimpleCompressor }

procedure TestTSimpleCompressor.SetUp;
begin
  FSimpleCompressor := TSimpleCompressor.Create;
end;

procedure TestTSimpleCompressor.TearDown;
begin
 FreeAndNil(FSimpleCompressor);
end;

procedure TestTSimpleCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSoftKneeCompressor }

procedure TestTSoftKneeCompressor.SetUp;
begin
  FSoftKneeCompressor := TSoftKneeCompressor.Create;
end;

procedure TestTSoftKneeCompressor.TearDown;
begin
 FreeAndNil(FSoftKneeCompressor);
end;

procedure TestTSoftKneeCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSimpleFeedbackCompressor }

procedure TestTSimpleFeedbackCompressor.SetUp;
begin
  FSimpleFeedbackCompressor := TSimpleFeedbackCompressor.Create;
end;

procedure TestTSimpleFeedbackCompressor.TearDown;
begin
 FreeAndNil(FSimpleFeedbackCompressor);
end;

procedure TestTSimpleFeedbackCompressor.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleFeedbackCompressor.ProcessSample(Input);
  // TODO: Validate method results
end;

procedure TestTSimpleFeedbackCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleFeedbackCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTSimpleFeedbackCompressor.TestCharacteristicCurve;
var
  ReturnValue: Double;
  InputLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleFeedbackCompressor.CharacteristicCurve(InputLevel);
  // TODO: Validate method results
end;


{ TestTSoftKneeFeedbackCompressor }

procedure TestTSoftKneeFeedbackCompressor.SetUp;
begin
  FSoftKneeFeedbackCompressor := TSoftKneeFeedbackCompressor.Create;
end;

procedure TestTSoftKneeFeedbackCompressor.TearDown;
begin
 FreeAndNil(FSoftKneeFeedbackCompressor);
end;

procedure TestTSoftKneeFeedbackCompressor.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeFeedbackCompressor.ProcessSample(Input);
  // TODO: Validate method results
end;

procedure TestTSoftKneeFeedbackCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeFeedbackCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSimpleRMSCompressor }

procedure TestTSimpleRMSCompressor.SetUp;
begin
  FSimpleRMSCompressor := TSimpleRMSCompressor.Create;
end;

procedure TestTSimpleRMSCompressor.TearDown;
begin
 FreeAndNil(FSimpleRMSCompressor);
end;

procedure TestTSimpleRMSCompressor.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleRMSCompressor.ProcessSample(Input);
  // TODO: Validate method results
end;

procedure TestTSimpleRMSCompressor.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FSimpleRMSCompressor.InputSample(Input);
  // TODO: Validate method results
end;


{ TestTCompressor }

procedure TestTCompressor.SetUp;
begin
  FCompressor := TCompressor.Create;
end;

procedure TestTCompressor.TearDown;
begin
 FreeAndNil(FCompressor);
end;

procedure TestTCompressor.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FCompressor.InputSample(Input);
  // TODO: Validate method results
end;

procedure TestTCompressor.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FCompressor.ProcessSample(Input);
  // TODO: Validate method results
end;

procedure TestTCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTSimpleDirectGate.Suite);
  RegisterTest(TestTSoftDirectGate.Suite);
  RegisterTest(TestTBrickwallLimiter.Suite);
  RegisterTest(TestTSoftBrickwallLimiter.Suite);
  RegisterTest(TestTSimpleSoftBrickwallLimiter.Suite);
  RegisterTest(TestTLimiter.Suite);
  RegisterTest(TestTSoftKneeLimiter.Suite);
  RegisterTest(TestTSimpleSoftKneeLimiter.Suite);
  RegisterTest(TestTClassicGate.Suite);
  RegisterTest(TestTClassicSoftRangeGate.Suite);
  RegisterTest(TestTClassicSoftKneeGate.Suite);
  RegisterTest(TestTAdvancedGate.Suite);
  RegisterTest(TestTSimpleCompressor.Suite);
  RegisterTest(TestTSoftKneeCompressor.Suite);
  RegisterTest(TestTSimpleFeedbackCompressor.Suite);
  RegisterTest(TestTSoftKneeFeedbackCompressor.Suite);
  RegisterTest(TestTSimpleRMSCompressor.Suite);
  RegisterTest(TestTCompressor.Suite);
end.
