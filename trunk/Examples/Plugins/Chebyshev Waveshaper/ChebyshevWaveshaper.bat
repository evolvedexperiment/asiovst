@copy "..\..\..\Bin\VST\32-Bit\ChebyshevWaveshaper.dll" "..\..\..\Bin\VST\32-Bit\Chebyshev Waveshaper.dll"
@"..\..\..\Bin\VST-Plugin Unit Test (command line).exe" "..\..\..\Bin\VST\32-Bit\Chebyshev Waveshaper.dll"
@"..\..\..\Bin\VstPluginScreenshotTool.exe" "..\..\..\Bin\VST\32-Bit\Chebyshev Waveshaper.dll"
@move "..\..\..\Bin\VST\32-Bit\Chebyshev Waveshaper.png" "..\..\..\Screenshots\Chebyshev Waveshaper.png"
@7z a "..\..\..\Archive\ChebyshevWaveshaper.7z" "..\..\..\Bin\VST\*\Chebyshev Waveshaper.dll" "..\..\..\Bin\License.txt"
@makensis /V2 "..\..\..\Install Scripts\Install Script Chebyshev Waveshaper.nsi"
@ftps -s:"..\..\..\Release Scripts\Chebyshev Waveshaper.ftp"
@WinSCP -script="..\..\..\Release Scripts\Chebyshev Waveshaper.scp"
@Pause