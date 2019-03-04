program NoiseshapingFilterDesigner;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  NfdMain in 'NfdMain.pas' {FmNoiseshapingFilterDesigner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmNoiseshapingFilterDesigner, FmNoiseshapingFilterDesigner);
  Application.Run;
end.
