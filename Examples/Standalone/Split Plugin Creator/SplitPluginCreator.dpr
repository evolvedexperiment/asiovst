program SplitPluginCreator;

{$R 'SplitTemplate.res' 'SplitTemplate.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  RTLVCLOptimize,
  Forms,
  SPCmain in 'SPCmain.pas' {FmSplitPluginCreator},
  DAV_ChunkPluginGUI in '..\..\..\Source\DAV_ChunkPluginGUI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSplitPluginCreator, FmSplitPluginCreator);
  Application.Run;
end.
