unit TetrisEditor;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Windows, Types, {$ENDIF}
     SysUtils, Classes, Forms, TetrisUnit, Controls, StdCtrls, ExtCtrls,
     Graphics, DAVDCommon, DVSTModule;

type
  TFmTetris = class(TForm)
    TetrisTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TetrisOnTimer(Sender: TObject);
  private
    fBitmap: TBitmap;
    fTetris: TTetris;
  public
    property Tetris : TTetris read fTetris;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure TFmTetris.FormCreate(Sender: TObject);
begin
 fTetris := TTetris.Create;
 fBitmap := TBitmap.Create;
end;

procedure TFmTetris.FormDestroy(Sender: TObject);
begin
 FreeAndNil(fTetris);
 FreeAndNil(fBitmap);
end;

procedure TFmTetris.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if key = VK_SPACE then fTetris.StepGame;
 if key  = VK_LEFT then fTetris.Left;
  if key  = VK_right then fTetris.right;
   if key  = VK_up then fTetris.Rotate;
    if key  = VK_DOWN then fTetris.StepGame;
 FormPaint(nil);
end;

procedure TFmTetris.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssleft in shift then fTetris.Left;
  if ssright in shift then fTetris.right;
  if ssMiddle in shift then fTetris.Rotate;
  FormPaint(nil);
end;

procedure TFmTetris.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 fTetris.Rotate;
 FormPaint(nil);
end;

procedure TFmTetris.FormPaint(Sender: TObject);
begin
 fTetris.DefaultBitmap(fBitmap);
 Canvas.StretchDraw(clientrect, fBitmap);
 Caption := 'lines ' + inttostr(fTetris.Lines);
end;

procedure TFmTetris.TetrisOnTimer(Sender: TObject);
begin
  fTetris.StepGame;
  FormPaint(nil);
  TetrisTimer.Interval:=TrimInt(1000 - fTetris.lines*10,100,1000);
end;

{$IFDEF FPC}
initialization
  {$i TetrisEditor.lrs}
{$ENDIF}

end.
