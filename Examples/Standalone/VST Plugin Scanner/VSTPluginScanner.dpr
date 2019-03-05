program VSTPluginScanner;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  VPSmain in 'VPSmain.pas' {FormVSTPluginScanner};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Scanner';
  Application.CreateForm(TFormVSTPluginScanner, FormVSTPluginScanner);
  Application.Run;
end.
