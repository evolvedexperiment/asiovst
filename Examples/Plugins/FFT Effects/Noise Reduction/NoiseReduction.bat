@"..\..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\..\Bin\Win32\VST\NoiseReduction.dll " .\NoiseReduction.mes
@move "..\..\..\..\Bin\Win32\VST\NoiseReduction.dll" "..\..\..\..\Bin\Win32\VST\Noise Reduction.dll"
@move "..\..\..\..\Bin\Win64\VST\NoiseReduction.dll" "..\..\..\..\Bin\Win64\VST\Noise Reduction.dll"
@"..\..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win32\VST\Noise Reduction.dll"
@"..\..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win64\VST\Noise Reduction.dll"
@"..\..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\..\Bin\Win32\VST\Noise Reduction.dll" "..\..\..\..\Screenshots\Noise Reduction.png"
@7z a "..\..\..\..\Archive\Noise Reduction.7z" "..\..\..\..\Bin\*\VST\Noise Reduction.dll" "..\..\..\..\Manuals\Noise Reduction.pdf" "..\..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\..\Install Scripts\Install Script Noise Reduction.nsi"
@ftps -s:"..\..\..\..\Release Scripts\Noise Reduction.ftp"
@WinSCP -script="..\..\..\..\Release Scripts\Noise Reduction.scp"
pause
