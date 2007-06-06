program VSTAnalyser;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  HostVSTLaz, 
  HostASIOLaz,
  VAMain in 'VAMain.pas' {FmVSTAnalyser},
  VAPlotIR in 'VAPlotIR.pas' {FmPlotIR};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Editor';
  Application.CreateForm(TFmVSTAnalyser, FmVSTAnalyser);
  Application.CreateForm(TFmPlotIR, FmPlotIR);
  Application.Run;
end.

