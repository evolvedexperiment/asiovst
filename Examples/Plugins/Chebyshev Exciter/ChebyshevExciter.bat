@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" ..\..\..\Bin\VST\32-Bit\ChebyshevExciter.dll 
@copy "..\..\..\Bin\VST\32-Bit\ChebyshevExciter.dll" "..\..\..\Bin\VST\32-Bit\Chebyshev Exciter.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\VST\32-Bit\Chebyshev Exciter.dll"
@move "..\..\..\Bin\VST\32-Bit\Chebyshev Exciter.png" "..\..\..\Screenshots\Chebyshev Exciter.png"
@7z a "..\..\..\Archive\ChebyshevExciter.7z" "..\..\..\Bin\VST\*\Chebyshev Exciter.dll" "..\..\..\Manuals\Chebyshev Exciter.pdf" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chebyshev Exciter.nsi"
@ftps -s:"..\..\..\Release Scripts\Chebyshev Exciter.ftp"
@WinSCP -script="..\..\..\Release Scripts\Chebyshev Exciter.scp"
pause