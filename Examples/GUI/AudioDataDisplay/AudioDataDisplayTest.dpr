program AudioDataDisplayTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  AudioDataDisplayTestMain in 'AudioDataDisplayTestMain.pas' {FmAudioDataDisplay};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAudioDataDisplay, FmAudioDataDisplay);
  Application.Run;
end.
