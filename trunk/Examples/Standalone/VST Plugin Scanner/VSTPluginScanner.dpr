program VSTPluginScanner;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  VPSmain in 'VPSmain.pas' {FmVSTPluginScanner};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Scanner';
  Application.CreateForm(TFmVSTPluginScanner, FmVSTPluginScanner);
  Application.Run;
end.
