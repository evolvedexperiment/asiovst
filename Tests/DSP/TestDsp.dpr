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
  DAV_DspButterworthFilter in '..\..\Source\DSP\DAV_DspButterworthFilter.pas',
  DAV_DspChebyshevFilter in '..\..\Source\DSP\DAV_DspChebyshevFilter.pas',
  DAV_DspChorus in '..\..\Source\DSP\DAV_DspChorus.pas',
  DAV_DspConvolution in '..\..\Source\DSP\DAV_DspConvolution.pas',
  DAV_DspDelayLines in '..\..\Source\DSP\DAV_DspDelayLines.pas',
  DAV_DspDitherNoiseShaper in '..\..\Source\DSP\DAV_DspDitherNoiseShaper.pas',
  DAV_DspDynamics in '..\..\Source\DSP\DAV_DspDynamics.pas',
  TestDAV_DspBarberpole in 'TestDAV_DspBarberpole.pas',
  TestDAV_DspBesselFilter in 'TestDAV_DspBesselFilter.pas',
  TestDAV_DspButterworthFilter in 'TestDAV_DspButterworthFilter.pas',
  TestDAV_DspChebyshevFilter in 'TestDAV_DspChebyshevFilter.pas',
  TestDAV_DspChorus in 'TestDAV_DspChorus.pas',
  TestDAV_DspConvolution in 'TestDAV_DspConvolution.pas',
  TestDAV_DspDelayLines in 'TestDAV_DspDelayLines.pas',
  TestDAV_DspDitherNoiseShaper in 'TestDAV_DspDitherNoiseShaper.pas',
  TestDAV_DspDynamics in 'TestDAV_DspDynamics.pas',
  TestDAV_DspFilterBasics in 'TestDAV_DspFilterBasics.pas',
  DAV_DspFilterBasics in '..\..\Source\DSP\DAV_DspFilterBasics.pas',
  TestDAV_DspFilterBasicsAutomatable in 'TestDAV_DspFilterBasicsAutomatable.pas',
  DAV_DspFilterBasicsAutomatable in '..\..\Source\DSP\DAV_DspFilterBasicsAutomatable.pas',
  DAV_DspFilterLinkwitzRiley in '..\..\Source\DSP\DAV_DspFilterLinkwitzRiley.pas',
  TestDAV_DspFilterLinkwitzRiley in 'TestDAV_DspFilterLinkwitzRiley.pas',
  TestDAV_DspFilterLinearPhaseCrossover in 'TestDAV_DspFilterLinearPhaseCrossover.pas',
  DAV_DspFilterLinearPhaseCrossover in '..\..\Source\DSP\DAV_DspFilterLinearPhaseCrossover.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

