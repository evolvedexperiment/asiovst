program LunchBox;

uses
  Forms,
  Dialogs,
  SysUtils,
  LunchBoxMain in 'LunchBoxMain.pas' {FmVSTEditor},
  LunchBoxSetup in 'LunchBoxSetup.pas' {FmSetup},
  LunchBoxAbout in 'LunchBoxAbout.pas' {FmAbout},
  LunchBoxVST in 'LunchBoxVST.pas' {FmVST},
  LunchBoxEventList in 'LunchBoxEventList.pas',
  LunchBoxEvent in 'LunchBoxEvent.pas';

{$R *.res}

begin
 if not DirectoryExists(ExtractFilePath(Application.ExeName)+'sounds\') then
  begin
   ShowMessage('Download the original Lunchbox Battle and copy the \sound directory to this folder');
   Exit;
  end;
 Application.Initialize;
 Application.Title := 'VST Plugin Editor';
 Application.CreateForm(TFmVSTEditor, FmVSTEditor);
 Application.CreateForm(TFmSetup, FmSetup);
 Application.CreateForm(TFmAbout, FmAbout);
 Application.CreateForm(TFmVST, FmVST);
 Application.Run;
end.

