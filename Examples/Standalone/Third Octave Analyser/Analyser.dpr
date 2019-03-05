program Analyser;

uses
  FastMM4,
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF }
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FormAnalyser};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Third-Octave ASIO Analyser';
  Application.CreateForm(TFormAnalyser, FormAnalyser);
  Application.Run;
end.
