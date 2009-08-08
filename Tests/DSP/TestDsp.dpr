program TestDsp;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enthält das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners fügen Sie den konditinalen Definitionen  
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardmäßig 
  der GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  DAV_DspBarberpole in '..\..\Source\DSP\DAV_DspBarberpole.pas',
  DAV_DspBesselFilter in '..\..\Source\DSP\DAV_DspBesselFilter.pas',
  DAV_DspChorus in '..\..\Source\DSP\DAV_DspChorus.pas',
  DAV_DspConvolution in '..\..\Source\DSP\DAV_DspConvolution.pas',
  DAV_DspCrosstalkCancellation in '..\..\Source\DSP\DAV_DspCrosstalkCancellation.pas',
  DAV_DspCrosstalkSimulator in '..\..\Source\DSP\DAV_DspCrosstalkSimulator.pas',
  DAV_DspDelayLines in '..\..\Source\DSP\DAV_DspDelayLines.pas',
  DAV_DspDitherNoiseShaper in '..\..\Source\DSP\DAV_DspDitherNoiseShaper.pas',
  DAV_DspDynamics in '..\..\Source\DSP\DAV_DspDynamics.pas',
  DAV_DspFilterBasics in '..\..\Source\DSP\DAV_DspFilterBasics.pas',
  DAV_DspFilterBasicsAutomatable in '..\..\Source\DSP\DAV_DspFilterBasicsAutomatable.pas',
  DAV_DspFilterButterworth in '..\..\Source\DSP\DAV_DspFilterButterworth.pas',
  DAV_DspFilterChebyshev in '..\..\Source\DSP\DAV_DspFilterChebyshev.pas',
  DAV_DspFilterLinearPhaseCrossover in '..\..\Source\DSP\DAV_DspFilterLinearPhaseCrossover.pas',
  DAV_DspFilterLinkwitzRiley in '..\..\Source\DSP\DAV_DspFilterLinkwitzRiley.pas',
  TestDAV_DspBarberpole in 'TestDAV_DspBarberpole.pas',
  TestDAV_DspBesselFilter in 'TestDAV_DspBesselFilter.pas',
  TestDAV_DspChorus in 'TestDAV_DspChorus.pas',
  TestDAV_DspConvolution in 'TestDAV_DspConvolution.pas',
  TestDAV_DspCrosstalkCancellation in 'TestDAV_DspCrosstalkCancellation.pas',
  TestDAV_DspCrosstalkSimulator in 'TestDAV_DspCrosstalkSimulator.pas',
  TestDAV_DspDelayLines in 'TestDAV_DspDelayLines.pas',
  TestDAV_DspDitherNoiseShaper in 'TestDAV_DspDitherNoiseShaper.pas',
  TestDAV_DspDynamics in 'TestDAV_DspDynamics.pas',
  TestDAV_DspFilterBasics in 'TestDAV_DspFilterBasics.pas',
  TestDAV_DspFilterBasicsAutomatable in 'TestDAV_DspFilterBasicsAutomatable.pas',
  TestDAV_DspFilterButterworth in 'TestDAV_DspFilterButterworth.pas',
  TestDAV_DspFilterChebyshev in 'TestDAV_DspFilterChebyshev.pas',
  TestDAV_DspFilterLinearPhaseCrossover in 'TestDAV_DspFilterLinearPhaseCrossover.pas',
  TestDAV_DspFilterLinkwitzRiley in 'TestDAV_DspFilterLinkwitzRiley.pas',
  TestDAV_DspAmbience in 'TestDAV_DspAmbience.pas',
  DAV_DspAmbience in '..\..\Source\DSP\DAV_DspAmbience.pas',
  TestDAV_DspBarberpoleTuner in 'TestDAV_DspBarberpoleTuner.pas',
  DAV_DspBarberpoleTuner in '..\..\Source\DSP\DAV_DspBarberpoleTuner.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

