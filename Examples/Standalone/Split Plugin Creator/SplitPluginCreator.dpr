program SplitPluginCreator;

{$R 'SplitTemplate.res' 'SplitTemplate.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,
  RTLVCLOptimize,
  Forms,
  SPCmain in 'SPCmain.pas' {FmSplitPluginCreator};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSplitPluginCreator, FmSplitPluginCreator);
  Application.Run;
end.
