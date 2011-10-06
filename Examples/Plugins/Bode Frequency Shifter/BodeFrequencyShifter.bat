@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\BodeFrequencyShifter.dll " .\BodeFrequencyShifter.mes
@move "..\..\..\Bin\Win32\VST\BodeFrequencyShifter.dll" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll"
@move "..\..\..\Bin\Win64\VST\BodeFrequencyShifter.dll" "..\..\..\Bin\Win64\VST\Bode Frequency Shifter.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Bode Frequency Shifter.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll" "..\..\..\Screenshots\Bode Frequency Shifter.png"
@7z a "..\..\..\Archive\Bode Frequency Shifter.7z" "..\..\..\Bin\*\VST\Bode Frequency Shifter.dll" "..\..\..\Manuals\Bode Frequency Shifter.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Bode Frequency Shifter.nsi"
@ftps -s:"..\..\..\Release Scripts\Bode Frequency Shifter.ftp"
@WinSCP -script="..\..\..\Release Scripts\Bode Frequency Shifter.scp"
pause
