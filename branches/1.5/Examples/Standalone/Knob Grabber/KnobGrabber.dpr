program KnobGrabber;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  KGmain in 'KGmain.pas' {FmKnobGrabber};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmKnobGrabber, FmKnobGrabber);
  Application.Run;
end.
