@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" ..\..\..\Bin\Win32\VST\ChebyshevExciter.dll 
@copy "..\..\..\Bin\Win32\VST\ChebyshevExciter.dll" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\Win32\VST\Chebyshev Exciter.dll"
@move "..\..\..\Bin\Win32\VST\Chebyshev Exciter.png" "..\..\..\Screenshots\Chebyshev Exciter.png"
@7z a "..\..\..\Archive\ChebyshevExciter.7z" "..\..\..\Bin\*\VST\Chebyshev Exciter.dll" "..\..\..\Manuals\Chebyshev Exciter.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chebyshev Exciter.nsi"
@ftps -s:"..\..\..\Release Scripts\Chebyshev Exciter.ftp"
@WinSCP -script="..\..\..\Release Scripts\Chebyshev Exciter.scp"
pause