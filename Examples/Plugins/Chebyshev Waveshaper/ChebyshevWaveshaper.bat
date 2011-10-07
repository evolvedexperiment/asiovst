@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\ChebyshevWaveshaper.dll " .\ChebyshevWaveshaper.mes
@move "..\..\..\Bin\Win32\VST\ChebyshevWaveshaper.dll" "..\..\..\Bin\Win32\VST\Chebyshev Waveshaper.dll"
@move "..\..\..\Bin\Win64\VST\ChebyshevWaveshaper.dll" "..\..\..\Bin\Win64\VST\Chebyshev Waveshaper.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Chebyshev Waveshaper.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Chebyshev Waveshaper.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Chebyshev Waveshaper.dll" "..\..\..\Screenshots\Chebyshev Waveshaper.png"
@7z a "..\..\..\Archive\ChebyshevWaveshaper.7z" "..\..\..\Bin\*\VST\Chebyshev Waveshaper.dll" "..\..\..\Manuals\Chebyshev Waveshaper.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chebyshev Waveshaper.nsi"
@ftps -s:"..\..\..\Release Scripts\Chebyshev Waveshaper.ftp"
@WinSCP -script="..\..\..\Release Scripts\Chebyshev Waveshaper.scp"
pause
