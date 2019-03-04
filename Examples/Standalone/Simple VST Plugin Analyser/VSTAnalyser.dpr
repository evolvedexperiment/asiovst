program VSTAnalyser;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  VAMain in 'VAMain.pas' {FmVSTEditor},
  VAPlotIR in 'VAPlotIR.pas' {FmPlotIR};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple VST Plugin Analyser';
  Application.CreateForm(TFmVSTAnalyser, FmVSTAnalyser);
  Application.CreateForm(TFmPlotIR, FmPlotIR);
  Application.Run;
end.

