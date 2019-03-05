program JustNoticableDifferenceEqualizerTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$IFDEF UseMadExcept}
  madExcept,
  {$ENDIF }
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
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
