program WinEar;

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
  WeMain in 'WeMain.pas' {FmWinEar},
  WeRetry in 'WeRetry.pas' {FmRetry},
  WeEndOfTest in 'WeEndOfTest.pas' {FmEndOfTest},
  WeHelp in 'WeHelp.pas' {FmAbout};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'WinEar Trainer';
  Application.CreateForm(TFmWinEar, FmWinEar);
  Application.CreateForm(TFmRetry, FmRetry);
  Application.CreateForm(TFmEndOfTest, FmEndOfTest);
  Application.Run;
end.
