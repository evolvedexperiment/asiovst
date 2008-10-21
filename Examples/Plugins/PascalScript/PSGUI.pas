unit PSGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, StdCtrls,
  ActnList, ComCtrls, ToolWin, Dialogs, DAV_Common, DAV_VSTModule, SynEdit,
  SynEditHighlighter, SynHighlighterPas, uPSCompiler, uPSUtils;

type
  TFmPascalScript = class(TForm)
    ACCompile: TAction;
    ActionList: TActionList;
    BtLoadScript: TButton;
    BtRun: TButton;
    BtSaveScript: TButton;
    DebugBox: TListBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Splitter: TSplitter;
    SynEdit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACCompileExecute(Sender: TObject);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtLoadScriptClick(Sender: TObject);
    procedure BtSaveScriptClick(Sender: TObject);
  private
    fCompiler : TPSPascalCompiler;
  end;

implementation

{$R *.DFM}

uses
  PSDM;

resourcestring
  STR_SUCCESSFULLY_COMPILED = 'Succesfully compiled';

function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: string): Boolean;
begin
 if Proc.Name = 'VSTPROCESSSAMPLE' then
  begin
   if not ExportCheck(Sender, Proc, [btReturnAddress, btS32, btDouble], [pmIn, pmInOut]) then // Check if the proc has the correct params.
    begin
     Sender.MakeError('', ecTypeMismatch, '');
     Result := False;
     Exit;
    end;
   Result := True;
  end
 else Result := True;
end;

procedure TFmPascalScript.ACCompileExecute(Sender: TObject);
var
  str : string;
  i   : Integer;
begin
 DebugBox.Items.Clear;
 if not fCompiler.Compile(SynEdit.Lines.Text) then
  for i := 0 to fCompiler.MsgCount - 1
   do DebugBox.Items.Add(fCompiler.Msg[i].MessageToString)
 else
  begin
   fCompiler.GetOutput(str);
   TPascalScriptDataModule(Owner).ByteCode := str;
   TPascalScriptDataModule(Owner).ScriptCode := SynEdit.Lines.Text;
   DebugBox.Items.Add(STR_SUCCESSFULLY_COMPILED);
  end;
end;

procedure TFmPascalScript.BtLoadScriptClick(Sender: TObject);
begin
 if OpenDialog.Execute
  then SynEdit.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TFmPascalScript.BtSaveScriptClick(Sender: TObject);
begin
 if SaveDialog.Execute
  then SynEdit.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TFmPascalScript.FormCreate(Sender: TObject);
begin
 fCompiler := TPSPascalCompiler.Create; // create an instance of the compiler.
 with fCompiler do
  begin
   OnExportCheck := ScriptOnExportCheck; // Assign the onExportCheck event.
   AllowNoBegin := True;
   AllowNoEnd := True; // AllowNoBegin and AllowNoEnd allows it that begin and end are not required in a script.
  end;
end;

procedure TFmPascalScript.FormDestroy(Sender: TObject);
begin
 FreeAndNil(fCompiler);
end;

procedure TFmPascalScript.SynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 120 then ACCompileExecute(Sender);
end;

end.
