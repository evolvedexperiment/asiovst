program SimpleAsioRecorder;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  SarMain in 'SarMain.pas' {FormRecordAudio};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRecordAudio, FormRecordAudio);
  Application.Run;
end.
