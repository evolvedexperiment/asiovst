program Metronome;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  MetronomeForm in 'MetronomeForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Metronome';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
