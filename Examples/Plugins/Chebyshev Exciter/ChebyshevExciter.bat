@"..\..\..\Bin\Win32\madExceptPatch.exe" "..\..\..\Bin\Win32\VST\ChebyshevExciter.dll " .\ChebyshevExciter.mes
@move "..\..\..\Bin\Win32\VST\ChebyshevExciter.dll" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll"
@move "..\..\..\Bin\Win64\VST\ChebyshevExciter.dll" "..\..\..\Bin\Win64\VST\Chebyshev Exciter.dll"
@"..\..\..\Bin\Win32\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll"
@"..\..\..\Bin\Win64\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\Win64\VST\Chebyshev Exciter.dll"
@"..\..\..\Bin\Win32\VST-Plugin Screenshot Tool.exe" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll" "..\..\..\Screenshots\Chebyshev Exciter.png"
@7z a "..\..\..\Archive\ChebyshevExciter.7z" "..\..\..\Bin\*\VST\Chebyshev Exciter.dll" "..\..\..\Manuals\Chebyshev Exciter.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chebyshev Exciter.nsi"
@ftps -s:"..\..\..\Release Scripts\Chebyshev Exciter.ftp"
@WinSCP -script="..\..\..\Release Scripts\Chebyshev Exciter.scp"
pause
