program Analyser;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  FastCode,
  RTLVCLOptimize,
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Third-Octave Goertzel ASIO Analyser';
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
