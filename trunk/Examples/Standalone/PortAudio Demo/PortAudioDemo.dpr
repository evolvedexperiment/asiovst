program PortAudioDemo;

{$I DAV_Compiler.inc}

uses
{$IFDEF CPU32}
(*
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
*)
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
