program AnalyserGoertzel;

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
  AnalyserForm in 'AnalyserForm.pas' {FormAnalyser};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Third-Octave Goertzel ASIO Analyser';
  Application.CreateForm(TFormAnalyser, FormAnalyser);
  Application.Run;
end.
