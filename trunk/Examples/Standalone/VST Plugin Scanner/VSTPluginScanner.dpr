program VSTPluginScanner;

{$I DAV_Compiler.inc}

uses
{$IFNDEF COMPILER16_UP}
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
{$ENDIF}
  Forms,
  VPSmain in 'VPSmain.pas' {FmVSTPluginScanner};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Scanner';
  Application.CreateForm(TFmVSTPluginScanner, FmVSTPluginScanner);
  Application.Run;
end.
