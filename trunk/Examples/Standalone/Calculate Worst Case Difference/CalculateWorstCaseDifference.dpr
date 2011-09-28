program CalculateWorstCaseDifference;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FmCalculateWorstCaseDifference};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmCalculateWorstCaseDifference, FmCalculateWorstCaseDifference);
  Application.Run;
end.

