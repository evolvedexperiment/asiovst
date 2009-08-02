program NoiseshapingFilterDesigner;

uses
  Forms,
  NfdMain in 'NfdMain.pas' {FmNoiseshapingFilterDesigner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmNoiseshapingFilterDesigner, FmNoiseshapingFilterDesigner);
  Application.Run;
end.
