program Generator;

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  Forms,
  GenMain in 'GenMain.pas' {FmGenerator};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Generator';
  Application.CreateForm(TFmGenerator, FmGenerator);
  Application.Run;
end.
