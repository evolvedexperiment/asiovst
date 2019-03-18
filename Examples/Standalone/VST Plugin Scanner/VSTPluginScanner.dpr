program VSTPluginScanner;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madListProcesses,
  madListModules, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  VPSmain in 'VPSmain.pas' {FormVSTPluginScanner};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Scanner';
  Application.CreateForm(TFormVSTPluginScanner, FormVSTPluginScanner);
  Application.Run;
end.
