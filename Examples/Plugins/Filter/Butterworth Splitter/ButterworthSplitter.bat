@"..\..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\..\Bin\Win32\VST\ButterworthSplitter.dll " .\ButterworthSplitter.mes
@move "..\..\..\..\Bin\Win32\VST\ButterworthSplitter.dll" "..\..\..\..\Bin\Win32\VST\Butterworth Splitter.dll"
@move "..\..\..\..\Bin\Win64\VST\ButterworthSplitter.dll" "..\..\..\..\Bin\Win64\VST\Butterworth Splitter.dll"
@"..\..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win32\VST\Butterworth Splitter.dll"
@"..\..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\..\Bin\Win64\VST\Butterworth Splitter.dll"
@"..\..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\..\Bin\Win32\VST\Butterworth Splitter.dll" "..\..\..\..\Screenshots\Butterworth Splitter.png"
@7z a "..\..\..\..\Archive\ButterworthSplitter.7z" "..\..\..\..\Bin\*\VST\Butterworth Splitter.dll" "..\..\..\..\Manuals\Butterworth Splitter.pdf" "..\..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\..\Install Scripts\Install Script Butterworth Splitter.nsi"
@ftps -s:"..\..\..\..\Release Scripts\Butterworth Splitter.ftp"
@WinSCP -script="..\..\..\..\Release Scripts\Butterworth Splitter.scp"
pause
