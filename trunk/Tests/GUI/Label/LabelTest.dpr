program LabelTest;

uses
  Forms,
  LabelTestMain in 'LabelTestMain.pas' {FmLabelTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmLabelTest, FmLabelTest);
  Application.Run;
end.
