@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" ..\..\..\Bin\TwoBandDistortion.DLL 
@copy "..\..\..\Bin\TwoBandDistortion.DLL" "..\..\..\Bin\2-Band Distortion.DLL"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\2-Band Distortion.dll"
@move "..\..\..\Bin\2-Band Distortion.dll.png" "..\..\..\Screenshots\2-Band Distortion.png"
@7z a "..\..\..\Archive\2-Band_Distortion.7z" "..\..\..\Bin\2-Band Distortion.dll" "..\..\..\Bin\2-Band Distortion (x64).DLL" "..\..\..\Bin\2-Band Distortion.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script 2-Band Distortion.nsi"
@ftps -s:"..\..\..\Release Scripts\TwoBandDistortion.ftp"
@WinSCP -script="..\..\..\Release Scripts\TwoBandDistortion.scp"
pause