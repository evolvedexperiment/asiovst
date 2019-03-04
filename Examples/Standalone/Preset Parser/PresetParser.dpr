program PresetParser;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  PPmain in 'PPmain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPresetParser, FmPresetParser);
  Application.Run;
end.
