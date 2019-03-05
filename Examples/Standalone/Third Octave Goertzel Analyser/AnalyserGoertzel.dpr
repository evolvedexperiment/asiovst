program AnalyserGoertzel;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FormAnalyser};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Third-Octave Goertzel ASIO Analyser';
  Application.CreateForm(TFormAnalyser, FormAnalyser);
  Application.Run;
end.
