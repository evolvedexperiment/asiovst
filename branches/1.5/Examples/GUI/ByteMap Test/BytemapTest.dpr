program BytemapTest;

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  BtMain in 'BtMain.pas' {FmBytemapTest},
  DAV_GuiPixelMap in '..\..\..\Source\GUI\DAV_GuiPixelMap.pas',
  DAV_GuiByteMap in '..\..\..\Source\GUI\DAV_GuiByteMap.pas',
  DAV_GuiBlend in '..\..\..\Source\GUI\DAV_GuiBlend.pas',
  DAV_MemoryUtils in '..\..\..\Source\DAV_MemoryUtils.pas',
  DAV_GuiCustomMap in '..\..\..\Source\GUI\DAV_GuiCustomMap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmBytemapTest, FmBytemapTest);
  Application.Run;
end.
