@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\TwoBandDistortion.dll " .\TwoBandDistortion.mes
@move "..\..\..\Bin\Win32\VST\TwoBandDistortion.dll" "..\..\..\Bin\Win32\VST\2-Band Distortion.dll"
@move "..\..\..\Bin\Win64\VST\TwoBandDistortion.dll" "..\..\..\Bin\Win64\VST\2-Band Distortion.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\2-Band Distortion.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\2-Band Distortion.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\2-Band Distortion.dll" "..\..\..\Screenshots\2-Band Distortion.png"
@7z a "..\..\..\Archive\2BandDistortion.7z" "..\..\..\Bin\*\VST\2-Band Distortion.dll" "..\..\..\Manuals\2-Band Distortion.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script 2-Band Distortion.nsi"
@ftps -s:"..\..\..\Release Scripts\2-Band Distortion.ftp"
@WinSCP -script="..\..\..\Release Scripts\2-Band Distortion.scp"
pause
