@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" ..\..\..\Bin\BarberpoleFlanger.DLL 
@copy "..\..\..\Bin\BarberpoleFlanger.dll" "..\..\..\Bin\Barberpole Flanger.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\Barberpole Flanger.dll"
@move "..\..\..\Bin\Barberpole Flanger.dll.png" "..\..\..\Screenshots\Barberpole Flanger.png"
@7z a "..\..\..\Archive\BarberpoleFlanger.7z" "..\..\..\Bin\Barberpole Flanger.dll" "..\..\..\Bin\Barberpole Flanger.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Barberpole Flanger.nsi"
@ftps -s:"..\..\..\Release Scripts\BarberpoleFlanger.ftp"
@WinSCP -script="..\..\..\Release Scripts\BarberpoleFlanger.scp"