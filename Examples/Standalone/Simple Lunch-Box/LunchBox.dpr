program LunchBox;

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
  Dialogs,
  SysUtils,
  LunchBoxMain in 'LunchBoxMain.pas' {FormLunchBox},
  LunchBoxSetup in 'LunchBoxSetup.pas' {FormSetup},
  LunchBoxAbout in 'LunchBoxAbout.pas' {FormAbout},
  LunchBoxVST in 'LunchBoxVST.pas' {FormVST},
  LunchBoxEventList in 'LunchBoxEventList.pas',
  LunchBoxEvent in 'LunchBoxEvent.pas',
  LunchBoxInputFilter in 'LunchBoxInputFilter.pas';

{$R *.res}

begin
 if not DirectoryExists(ExtractFilePath(Application.ExeName)+'sounds\') then
  begin
   ShowMessage('Download the original Lunchbox Battle and copy the \sound directory to this folder');
   Exit;
  end;
 Application.Initialize;
 Application.Title := 'VST Plugin Editor';
 Application.CreateForm(TFormLunchBox, FormLunchBox);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormVST, FormVST);
  Application.Run;
end.

