unit TetrisDM;

{$I ASIOVST.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule, DAV_VSTEffect;

type
  TTetrisModule = class(TVSTModule)
    function VSTModuleCheckKey(Sender: TObject; Key: Char): Boolean;
    procedure VSTModuleEditorKeyDown(Sender: TObject; var keyCode: TVstKeyCode);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
  public
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  TetrisEditor;

function TTetrisModule.VSTModuleCheckKey(Sender: TObject; Key: Char): Boolean;
begin
 result := True;
end;

procedure TTetrisModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
  GUI := TFmTetris.Create(Self);
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
