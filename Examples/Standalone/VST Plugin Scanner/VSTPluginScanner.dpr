program VSTPluginScanner;

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  RTLVCLOptimize,
  Forms,
  VPSmain in 'VPSmain.pas' {FmVSTPluginScanner};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Scanner';
  Application.CreateForm(TFmVSTPluginScanner, FmVSTPluginScanner);
  Application.Run;
end.
