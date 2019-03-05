program Metronome;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  MetronomeForm in 'MetronomeForm.pas' {FormASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Metronome';
  Application.CreateForm(TFormASIO, FormASIO);
  Application.Run;
end.
