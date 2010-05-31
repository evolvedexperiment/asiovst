program UnknownFileExplorer;

uses
  Forms,
  UFEmain in 'UFEmain.pas' {FmUnknownFileExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmUnknownFileExplorer, FmUnknownFileExplorer);
  Application.Run;
end.
