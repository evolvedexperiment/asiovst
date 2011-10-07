@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\SimpleFlanger.dll " .\SimpleFlanger.mes
@move "..\..\..\Bin\Win32\VST\SimpleFlanger.dll" "..\..\..\Bin\Win32\VST\Flanger.dll"
@move "..\..\..\Bin\Win64\VST\SimpleFlanger.dll" "..\..\..\Bin\Win64\VST\Flanger.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Flanger.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Flanger.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Flanger.dll" "..\..\..\Screenshots\Flanger.png"
@7z a "..\..\..\Archive\Flanger.7z" "..\..\..\Bin\*\VST\Flanger.dll" "..\..\..\Manuals\Flanger.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Flanger.nsi"
@ftps -s:"..\..\..\Release Scripts\Flanger.ftp"
@WinSCP -script="..\..\..\Release Scripts\Flanger.scp"
pause
