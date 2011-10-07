@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\AdvancedClipper.dll " .\AdvancedClipper.mes
@move "..\..\..\Bin\Win32\VST\AdvancedClipper.dll" "..\..\..\Bin\Win32\VST\Advanced Clipper.dll"
@move "..\..\..\Bin\Win64\VST\AdvancedClipper.dll" "..\..\..\Bin\Win64\VST\Advanced Clipper.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Advanced Clipper.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Advanced Clipper.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Advanced Clipper.dll" "..\..\..\Screenshots\Advanced Clipper.png"
@7z a "..\..\..\Archive\AdvancedClipper.7z" "..\..\..\Bin\*\VST\Advanced Clipper.dll" "..\..\..\Manuals\Advanced Clipper.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Advanced Clipper.nsi"
@ftps -s:"..\..\..\Release Scripts\Advanced Clipper.ftp"
@WinSCP -script="..\..\..\Release Scripts\Advanced Clipper.scp"
pause
