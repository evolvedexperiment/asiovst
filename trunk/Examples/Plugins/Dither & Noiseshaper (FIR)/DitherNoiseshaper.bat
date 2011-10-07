@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\DitherNoiseshaper.dll " .\DitherNoiseshaper.mes
@move "..\..\..\Bin\Win32\VST\DitherNoiseshaper.dll" "..\..\..\Bin\Win32\VST\Dither & Noiseshaper (FIR).dll"
@move "..\..\..\Bin\Win64\VST\DitherNoiseshaper.dll" "..\..\..\Bin\Win64\VST\Dither & Noiseshaper (FIR).dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Dither & Noiseshaper (FIR).dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Dither & Noiseshaper (FIR).dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Dither & Noiseshaper (FIR).dll" "..\..\..\Screenshots\Dither & Noiseshaper (FIR).png"
@7z a "..\..\..\Archive\Dither&Noiseshaper(FIR).7z" "..\..\..\Bin\*\VST\Dither & Noiseshaper (FIR).dll" "..\..\..\Manuals\Dither & Noiseshaper (FIR).pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Dither & Noiseshaper (FIR).nsi"
@ftps -s:"..\..\..\Release Scripts\Dither & Noiseshaper (FIR).ftp"
@WinSCP -script="..\..\..\Release Scripts\Dither & Noiseshaper (FIR).scp"
pause
