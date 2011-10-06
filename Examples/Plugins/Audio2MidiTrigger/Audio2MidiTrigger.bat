@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\Audio2MidiTrigger.dll " .\Audio2MidiTrigger.mes
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Audio2MidiTrigger.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Audio2MidiTrigger.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Audio2MidiTrigger.dll" "..\..\..\Screenshots\Audio2MidiTrigger.png"
@7z a "..\..\..\Archive\Audio2MidiTrigger.7z" "..\..\..\Bin\*\VST\Audio2MidiTrigger.dll" "..\..\..\Manuals\Audio2MidiTrigger.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Audio2MidiTrigger.nsi"
@ftps -s:"..\..\..\Release Scripts\Audio2MidiTrigger.ftp"
@WinSCP -script="..\..\..\Release Scripts\Audio2MidiTrigger.scp"
pause
