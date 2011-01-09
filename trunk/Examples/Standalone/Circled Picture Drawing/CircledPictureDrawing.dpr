program CircledPictureDrawing;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListModules,
  Forms,
  MainUnit in 'MainUnit.pas' {FmPrimitivePictureEvolution},
  SettingsUnit in 'SettingsUnit.pas' {FmSettings},
  ProgressBarUnit in 'ProgressBarUnit.pas' {FmProgressBar},
  AdditionalChunks in 'AdditionalChunks.pas';

{$IFDEF RELEASE}
{$SetPEFlags 1}
{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF DELPHI10_UP}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.Title := 'Primitive Picture Evolution';
  Application.CreateForm(TFmPrimitivePictureEvolution, FmPrimitivePictureEvolution);
  Application.Run;
end.

