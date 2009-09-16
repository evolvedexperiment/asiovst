program Generator;

uses
  FastMM4,
  FastMove,
  Forms,
  GenMain in 'GenMain.pas' {FmGenerator};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmGenerator, FmGenerator);
  Application.Run;
end.
