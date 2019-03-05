program NoiseshapingFilterDesigner;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  NfdMain in 'NfdMain.pas' {FormNoiseshapingFilterDesigner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormNoiseshapingFilterDesigner, FormNoiseshapingFilterDesigner);
  Application.Run;
end.
