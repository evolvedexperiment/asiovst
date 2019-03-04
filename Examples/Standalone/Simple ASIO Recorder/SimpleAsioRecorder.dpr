program SimpleAsioRecorder;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  SarMain in 'SarMain.pas' {FmRecordAudio};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmRecordAudio, FmRecordAudio);
  Application.Run;
end.
