@copy "..\..\..\Bin\Win32\VST\SimpleFlanger.dll" "..\..\..\Bin\Win32\VST\Flanger.dll"
@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Flanger.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\Win32\VST\Flanger.dll"
@move "..\..\..\Bin\Win32\VST\Flanger.png" "..\..\..\Screenshots\Flanger.png"
@C:\Progra~1\7-Zip\7z.exe a "..\..\..\Archive\Flanger.7z" "..\..\..\Bin\*\VST\Flanger*.dll" "..\..\..\Bin\Flanger.pdf" "..\..\..\Bin\License.txt"
@C:\Progra~1\NSIS\makensis.exe /V2 "..\..\..\Install Scripts\Install Script Flanger.nsi"
@ftps -s:"..\..\..\Release Scripts\Flanger.ftp"
@WinSCP -script="..\..\..\Release Scripts\Flanger.scp"
@Pause