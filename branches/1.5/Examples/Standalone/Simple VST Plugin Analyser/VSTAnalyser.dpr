program VSTAnalyser;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
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

