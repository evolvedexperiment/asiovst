program JustNoticableDifferenceEqualizerTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madListHardware,
  madListProcesses,
  madListModules, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  JNDEQTmain in 'JNDEQTmain.pas' {FormJNDEQT},
  JNDEQTaudio in 'JNDEQTaudio.pas' {FormSetup},
  JNDEQTsurvey in 'JNDEQTsurvey.pas' {FormSurvey};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormJNDEQT, FormJNDEQT);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.Run;
end.
