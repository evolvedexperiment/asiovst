program BMTestDouble;

uses
  Forms,
  BMTestSDoubleU in 'BMTestDoubleU.pas' {BufferMathForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBufferMathForm, BufferMathForm);
  Application.Run;
end.
