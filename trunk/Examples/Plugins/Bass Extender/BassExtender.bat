@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BassExtender.dll " .\BassExtender.mes
@move "..\..\..\Bin\Win32\VST\BassExtender.dll" "..\..\..\Bin\Win32\VST\Bass Extender.dll"
@move "..\..\..\Bin\Win64\VST\BassExtender.dll" "..\..\..\Bin\Win64\VST\Bass Extender.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Bass Extender.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Bass Extender.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Bass Extender.dll" "..\..\..\Screenshots\Bass Extender.png"
@7z a "..\..\..\Archive\Bass Extender.7z" "..\..\..\Bin\*\VST\Bass Extender.dll" "..\..\..\Manuals\Bass Extender.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Bass Extender.nsi"
@ftps -s:"..\..\..\Release Scripts\Bass Extender.ftp"
@WinSCP -script="..\..\..\Release Scripts\Bass Extender.scp"
pause
