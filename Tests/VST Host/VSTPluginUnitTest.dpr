program VSTPluginUnitTest;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enth�lt das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners f�gen Sie den konditinalen Definitionen  
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardm��ig 
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
{$IFNDEF CONSOLE_TESTRUNNER}
  SplashScreen in 'SplashScreen.pas' {FmSplashScreen},
{$ENDIF}
  DAV_TestVSTHost in 'DAV_TestVSTHost.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole
   then TextTestRunner.RunRegisteredTests
   else GUITestRunner.RunRegisteredTests;
end.

