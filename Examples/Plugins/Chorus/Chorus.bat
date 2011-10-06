@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\Chorus.dll " .\Chorus.mes
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Chorus.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Chorus.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Chorus.dll" "..\..\..\Screenshots\Chorus.png"
@7z a "..\..\..\Archive\Chorus.7z" "..\..\..\Bin\*\VST\Chorus.dll" "..\..\..\Manuals\Chorus.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chorus.nsi"
@ftps -s:"..\..\..\Release Scripts\Chorus.ftp"
@WinSCP -script="..\..\..\Release Scripts\Chorus.scp"
pause
