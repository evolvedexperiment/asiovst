program CircledPictureDrawing;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmCircledPictureDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Circled Picture Drawing';
  Application.CreateForm(TFmCircledPictureDialog, FmCircledPictureDialog);
  Application.Run;
end.

