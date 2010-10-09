program ZLibUnitTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  DAV_ZLib in '..\..\Source\DAV_ZLib.pas',
  DAV_Adler32 in '..\..\Source\DAV_Adler32.pas',
  TestDAV_ZLib in 'TestDAV_ZLib.pas';

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.


