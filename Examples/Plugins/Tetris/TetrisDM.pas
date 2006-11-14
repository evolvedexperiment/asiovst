unit TetrisDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DDSPBase, DVSTEffect, DVSTModule;

type
  TTetrisModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleEditorKeyDown(Sender: TObject; var keyCode: TVstKeyCode);
    function VSTModuleCheckKey(Sender: TObject; Key: Char): Boolean;
  private
  public
  end;

implementation

{$R *.DFM}

uses
  TetrisEditor;

function TTetrisModule.VSTModuleCheckKey(Sender: TObject; Key: Char): Boolean;
begin
 result:=True;
end;

procedure TTetrisModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TFmTetris.Create(nil);
  (GUI As TFmTetris).TetrisModule := Self;
end;

procedure TTetrisModule.VSTModuleEditorKeyDown(Sender: TObject;
  var keyCode: TVstKeyCode);
begin
 if assigned(EditorForm) then
  with (EditorForm As TFmTetris) do
   begin
    case keyCode.Character of
     VKEY_SPACE : Tetris.StepGame;
     VKEY_LEFT : Tetris.Left;
     VKEY_right : Tetris.right;
     VKEY_up : Tetris.Rotate;
     VKEY_DOWN : Tetris.StepGame;
    end;
    FormPaint(nil);
   end;
end;

end.
