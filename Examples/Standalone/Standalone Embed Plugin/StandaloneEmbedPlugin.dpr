program StandaloneEmbedPlugin;

{$R 'DAV_Convolution.res' 'DAV_Convolution.rc'}
{$R 'Standalone.res' 'Standalone.rc'}

uses
  Forms,
  SEPmain in 'SEPmain.pas' {FmStandaloneEmbedPlugin};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmStandaloneEmbedPlugin, FmStandaloneEmbedPlugin);
  Application.Run;
end.
