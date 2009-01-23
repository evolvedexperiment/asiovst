program ApproximationBenchmark;

uses
//  FastMM4,
  FastMove,
  Forms,
  ApproxMain in 'ApproxMain.pas' {FmApproximationBenchmark};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmApproximationBenchmark, FmApproximationBenchmark);
  Application.Run;
end.
