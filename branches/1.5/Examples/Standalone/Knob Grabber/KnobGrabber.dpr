program KnobGrabber;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  KGmain in 'KGmain.pas' {FormKnobGrabber};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormKnobGrabber, FormKnobGrabber);
  Application.Run;
end.
