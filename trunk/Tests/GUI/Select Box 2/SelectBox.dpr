program SelectBox;

uses
  Forms,
  SelectBoxTest in 'SelectBoxTest.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
