program VSTAnalyser;

{$I DAV_Compiler.inc}

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
  VAMain in 'VAMain.pas' {FormVSTAnalyser},
  VAPlotIR in 'VAPlotIR.pas' {FormPlotIR};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple VST Plugin Analyser';
  Application.CreateForm(TFormVSTAnalyser, FormVSTAnalyser);
  Application.CreateForm(TFormPlotIR, FormPlotIR);
  Application.Run;
end.

