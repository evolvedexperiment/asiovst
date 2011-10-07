@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll " .\BarberpoleFlanger.mes
@if exist "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll" @move "..\..\..\Bin\Win32\VST\BarberpoleFlanger.dll" "..\..\..\Bin\Win32\VST\Barberpole.dll"
@if exist "..\..\..\Bin\Win64\VST\BarberpoleFlanger.dll" @move "..\..\..\Bin\Win64\VST\BarberpoleFlanger.dll" "..\..\..\Bin\Win64\VST\Barberpole.dll"
@if not exist "..\..\..\Bin\Win32\VST\Barberpole.dll" goto Error
@if not exist "..\..\..\Bin\Win64\VST\Barberpole.dll" goto Error
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Barberpole.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Barberpole.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Barberpole.dll" "..\..\..\Screenshots\Barberpole.png"
@7z a "..\..\..\Archive\Barberpole.7z" "..\..\..\Bin\*\VST\Barberpole.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Barberpole.nsi"
@IF ERRORLEVEL==1 GOTO Error
@ftps -s:"..\..\..\Release Scripts\Barberpole.ftp"
@WinSCP -script="..\..\..\Release Scripts\Barberpole.scp"
Exit
:Error
echo Script Error
Pause
