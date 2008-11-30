program ChunkParser;

uses
  Forms,
  CPmain in 'CPmain.pas' {FmChunkParser};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmChunkParser, FmChunkParser);
  Application.Run;
end.
