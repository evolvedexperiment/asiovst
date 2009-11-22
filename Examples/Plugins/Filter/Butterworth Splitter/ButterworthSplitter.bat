@"..\..\..\..\Bin\VST-Plugin Unit Test (command line).exe" ..\..\..\..\Bin\ButterworthSplitter.DLL 
@copy "..\..\..\..\Bin\ButterworthSplitter.dll" "..\..\..\..\Bin\Butterworth Splitter.dll"
@C:\Progra~1\7-Zip\7z.exe a "..\..\..\..\Archive\ButterworthSplitter.7z" "..\..\..\..\Bin\License.txt"
@C:\Progra~1\7-Zip\7z.exe a "..\..\..\..\Archive\ButterworthSplitter.7z" "..\..\..\..\Bin\Butterworth Splitter.dll"
@C:\Progra~1\NSIS\makensis.exe /V2 "..\..\..\..\Install Scripts\Install Script Butterworth Splitter.nsi"
@"C:\Windows\System32\ftps.exe" -s:%0
goto done
open www.savioursofsoul.de
ftp1122403-vst
Bdz1plpsf
binary
passive
put "..\..\..\..\Archive\ButterworthSplitter.7z"
put "..\..\..\..\Install Scripts\ButterworthSplitter_Install.exe"
bye
:done
