@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\SimpleFlanger.dll " .\SimpleFlanger.mes
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\SimpleFlanger.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\SimpleFlanger.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\SimpleFlanger.dll" "..\..\..\Screenshots\SimpleFlanger.png"
@7z a "..\..\..\Archive\SimpleFlanger.7z" "..\..\..\Bin\*\VST\SimpleFlanger.dll" "..\..\..\Manuals\SimpleFlanger.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script SimpleFlanger.nsi"
@ftps -s:"..\..\..\Release Scripts\SimpleFlanger.ftp"
@WinSCP -script="..\..\..\Release Scripts\SimpleFlanger.scp"
pause
