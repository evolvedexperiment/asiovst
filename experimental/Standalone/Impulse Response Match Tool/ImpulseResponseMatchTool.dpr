program ImpulseResponseMatchTool;

uses
  Forms,
  IRMTmain in 'IRMTmain.pas' {FmImpulseResponseMatchTool};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmImpulseResponseMatchTool, FmImpulseResponseMatchTool);
  Application.Run;
end.
