program Artumes;

uses
  Forms,
  ArtMain in 'ArtMain.pas' {FmArtumes},
  ArtProject in 'ArtProject.pas' {FmProject},
  ArtAbout in 'ArtAbout.pas' {FmAbout},
  ArtItemSource in 'ArtItemSource.pas',
  ArtItemAnalysis in 'ArtItemAnalysis.pas',
  ArtItemDestination in 'ArtItemDestination.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFmArtumes, FmArtumes);
  Application.CreateForm(TFmAbout, FmAbout);
  Application.Run;
end.
