@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" ..\..\..\Bin\AdvancedClipper.DLL 
@copy "..\..\..\Bin\AdvancedClipper.DLL" "..\..\..\Bin\Advanced Clipper.DLL"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\Advanced Clipper.DLL"
@7z.exe a "..\..\..\Archive\AdvancedClipper.7z" "..\..\..\Bin\Advanced Clipper.DLL" "..\..\..\Bin\Advanced Clipper Manual.pdf" "..\..\..\Bin\License.txt"
@makensis.exe /V2 "..\..\..\Install Scripts\Install Script Advanced Clipper.nsi"
@ftps -s:"..\..\..\Release Scripts\AdvancedClipper.ftp"
@WinSCP -script="..\..\..\Release Scripts\AdvancedClipper.scp"