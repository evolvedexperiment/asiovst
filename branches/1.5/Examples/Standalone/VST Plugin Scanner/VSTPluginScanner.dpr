program VSTPluginScanner;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove,
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
