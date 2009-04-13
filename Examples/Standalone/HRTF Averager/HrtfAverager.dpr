program HrtfAverager;

uses
  Forms,
  HAmain in 'HAmain.pas' {FmHrtfAverager};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmHrtfAverager, FmHrtfAverager);
  Application.Run;
end.
