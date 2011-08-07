program VSTPluginUnitTest;
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
  FastMM4,
  FastMove,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
{$IFNDEF CONSOLE_TESTRUNNER}
  SplashScreen in 'SplashScreen.pas' {FmSplashScreen},
{$ENDIF}
  DAV_TestVSTHost in 'DAV_TestVSTHost.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Unit Test';
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

