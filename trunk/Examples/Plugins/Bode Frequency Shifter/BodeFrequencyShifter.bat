@copy "..\..\..\Bin\Win32\VST\BodeFrequencyShifter.dll" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll"
@copy "..\..\..\Bin\Win32\VST\BodeFrequencyShifterStereo.dll" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter (mono).dll"
@copy "..\..\..\Bin\Win32\VST\BodeFrequencyShifterStereo.dll" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter (stereo).dll"
@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.dll"
@move "..\..\..\Bin\Win32\VST\Bode Frequency Shifter.png" "..\..\..\Screenshots\Bode Frequency Shifter.png"
@C:\Progra~1\7-Zip\7z.exe a "..\..\..\Archive\BodeFrequencyShifter.7z" "..\..\..\Bin\*\VST\Bode Frequency Shifter*.dll" "..\..\..\Bin\Bode Frequency Shifter.pdf" "..\..\..\Bin\License.txt"
@C:\Progra~1\NSIS\makensis.exe /V2 "..\..\..\Install Scripts\Install Script Bode Frequency Shifter.nsi"
@ftps -s:"..\..\..\Release Scripts\Bode Frequency Shifter.ftp"
@WinSCP -script="..\..\..\Release Scripts\Bode Frequency Shifter.scp"