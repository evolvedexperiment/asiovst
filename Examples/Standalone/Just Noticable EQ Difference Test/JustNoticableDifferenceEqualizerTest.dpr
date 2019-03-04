program JustNoticableDifferenceEqualizerTest;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  JNDEQTmain in 'JNDEQTmain.pas' {FmJNDEQT},
  JNDEQTaudio in 'JNDEQTaudio.pas' {FmSetup},
  JNDEQTsurvey in 'JNDEQTsurvey.pas' {FmSurvey};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmJNDEQT, FmJNDEQT);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
