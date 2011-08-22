@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" ..\..\..\Bin\VST\32-Bit\TwoBandDistortion.DLL 
@copy "..\..\..\Bin\VST\32-Bit\TwoBandDistortion.DLL" "..\..\..\Bin\VST\32-Bit\2-Band Distortion.DLL"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\VST\32-Bit\2-Band Distortion.dll"
@move "..\..\..\Bin\VST\32-Bit\2-Band Distortion.png" "..\..\..\Screenshots\2-Band Distortion.png"
@7z a "..\..\..\Archive\2-Band_Distortion.7z" "..\..\..\Bin\VST\*\2-Band Distortion.dll" "..\..\..\Manuals\2-Band Distortion.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script 2-Band Distortion.nsi"
@ftps -s:"..\..\..\Release Scripts\2-Band Distortion.ftp"
@WinSCP -script="..\..\..\Release Scripts\2-Band Distortion.scp"
pause