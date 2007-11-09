program BMTest;

uses
  Forms,
  BMTestU in 'BMTestU.pas' {BufferMathForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBufferMathForm, BufferMathForm);
  Application.Run;
end.
