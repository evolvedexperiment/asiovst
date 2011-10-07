@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll " .\BarberpoleFlanger.mes
@move "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll" "..\..\..\Bin\Win32\VST\Barberpole.dll"
@move "..\..\..\Bin\Win64\VST\BarberpoleFlanger.dll" "..\..\..\Bin\Win64\VST\Barberpole.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Barberpole.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Barberpole.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Barberpole.dll" "..\..\..\Screenshots\Barberpole.png"
@7z a "..\..\..\Archive\Barberpole.7z" "..\..\..\Bin\*\VST\Barberpole.dll" "..\..\..\Manuals\Barberpole.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Barberpole.nsi"
@ftps -s:"..\..\..\Release Scripts\Barberpole.ftp"
@WinSCP -script="..\..\..\Release Scripts\Barberpole.scp"
pause
