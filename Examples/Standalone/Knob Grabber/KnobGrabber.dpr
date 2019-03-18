program KnobGrabber;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  KGmain in 'KGmain.pas' {FormKnobGrabber};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormKnobGrabber, FormKnobGrabber);
  Application.Run;
end.
