program PortAudioDemo;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download the library or disable the feature
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  PortAudioDemoForm in 'PortAudioDemoForm.pas' {FmPortAudio};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for PortAudio-Host';
  Application.CreateForm(TFmPortAudio, FmPortAudio);
  Application.Run;
end.
