program VSTAnalyser;

uses
  FastMM4,
  FastMove,
  RTLVCLOptimize,
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

