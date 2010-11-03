program GroupBoxTest;

uses
  Forms,
  GroupBoxTestMain in 'GroupBoxTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmGroupBoxTest, FmGroupBoxTest);
  Application.Run;
end.
