program JustNoticableDifferenceEqualizerTest;

uses
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
