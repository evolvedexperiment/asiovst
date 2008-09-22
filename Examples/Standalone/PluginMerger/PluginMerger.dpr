program PluginMerger;

{$R 'CustomWrapper.res' 'CustomWrapper.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  RTLVCLOptimize,
  Forms,
  PMmain in 'PMmain.pas' {FmPluginMerger},
  DAV_ChunkPluginGUI in '..\..\..\Source\DAV_ChunkPluginGUI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPluginMerger, FmPluginMerger);
  Application.Run;
end.
