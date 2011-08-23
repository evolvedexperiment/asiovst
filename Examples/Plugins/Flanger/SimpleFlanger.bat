@copy "..\..\..\Bin\VST\32-Bit\SimpleFlanger.dll" "..\..\..\Bin\VST\32-Bit\Flanger.dll"
@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\VST\32-Bit\Flanger.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\VST\32-Bit\Flanger.dll"
@move "..\..\..\Bin\VST\32-Bit\Flanger.png" "..\..\..\Screenshots\Flanger.png"
@C:\Progra~1\7-Zip\7z.exe a "..\..\..\Archive\Flanger.7z" "..\..\..\Bin\VST\*\Flanger*.dll" "..\..\..\Bin\Flanger.pdf" "..\..\..\Bin\License.txt"
@C:\Progra~1\NSIS\makensis.exe /V2 "..\..\..\Install Scripts\Install Script Flanger.nsi"
@ftps -s:"..\..\..\Release Scripts\Flanger.ftp"
@WinSCP -script="..\..\..\Release Scripts\Flanger.scp"
@Pause