program PortAudioDemo;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
{$IFDEF UseFastMove}
  FastMove,
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
