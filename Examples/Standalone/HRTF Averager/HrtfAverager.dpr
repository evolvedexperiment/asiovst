program HrtfAverager;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove,
  Forms,
  HAmain in 'HAmain.pas' {FmHrtfAverager};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmHrtfAverager, FmHrtfAverager);
  Application.Run;
end.
