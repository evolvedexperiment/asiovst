program PortAudioDemo;

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
  PortAudioDemoForm in 'PortAudioDemoForm.pas' {FormPortAudio};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for PortAudio-Host';
  Application.CreateForm(TFormPortAudio, FormPortAudio);
  Application.Run;
end.
