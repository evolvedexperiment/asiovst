program PresetParser;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  PPmain in 'PPmain.pas' {FormPresetParser};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPresetParser, FormPresetParser);
  Application.Run;
end.
