unit TetrisUnit;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

(*
___ ____ ___ ____ _ ____    ___  _   _    ____ ____ _  _ ____ 
 |  |___  |  |__/ | [__     |__]  \_/     |__| |__/ |\ | |  | 
 |  |___  |  |  \ | ___]    |__]   |      |  | |  \ | \| |__| 
                                                        arnaud@celermajer.net
*)
interface

uses {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} 
     Messages, Classes, Sysutils, Graphics;

type
  TSprite = array [0..3, 0..3] of Integer;
  TTetris = class
  private
    FLines            : Integer;
    FCurrentSpritePos : TPoint;
    FCurrentSprite    : TSprite;
    FSprite           : Boolean;
    FCurrentColor     : TColor;
    function GetCurrenTSprite: TSprite;
    procedure SetCurrenTSprite(const Value: TSprite);
    procedure SetCurrenTSpritePos(const Value: TPoint);
    function CanSpriteBeHere(asprite: TSprite; apos: TPoint): Boolean;
    procedure NewGameBoard;
    procedure FusionCurrenTSprite;
  public
    GameBoard: array[0..9] of array[0..19] of Integer;
    property CurrenTSprite: TSprite read GetCurrenTSprite write SetCurrenTSprite;
    property CurrenTSpritePos: TPoint read FCurrentSpritePos write SetCurrenTSpritePos;
    procedure StepGame;
    procedure Rotate;
    procedure Left;
    procedure Right;
    procedure DefaultBitmap(bmp: TBitmap);
    property Lines: Integer read FLines write FLines;
  end;

const
  AllSpriteKind: array[0..6] of TSprite =
  (((0, 1, 0, 0), (0, 1, 0, 0), (0, 1, 0, 0), (0, 1, 0, 0)),
   ((0, 0, 0, 0), (0, 1, 1, 0), (1, 1, 0, 0), (0, 0, 0, 0)),
   ((0, 0, 0, 0), (1, 1, 0, 0), (0, 1, 1, 0), (0, 0, 0, 0)), ((0, 0, 0, 0),
    (0, 1, 0, 0), (1, 1, 1, 0), (0, 0, 0, 0)), ((0, 0, 0, 0), (0, 0, 1, 0),
    (1, 1, 1, 0), (0, 0, 0, 0)), ((0, 0, 0, 0), (1, 1, 1, 0), (0, 0, 1, 0),
    (0, 0, 0, 0)), ((0, 0, 0, 0), (0, 1, 1, 0), (0, 1, 1, 0), (0, 0, 0, 0)));

function TrimInt(al, min, max: integer): integer;
    
implementation

{ TTetris }

function TrimInt(al, min, max: integer): integer;
begin
 if al < min then result := min else
  if al > max then result := max
   else result := al;
end;

function TTetris.CanSpriteBeHere(asprite: TSprite; apos: TPoint): Boolean;
var
  i, ii, jj, j: integer;
begin
  result := true;
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      if asprite[i, j] = 0 then Continue;
      ii := i + apos.X;
      jj := j + apos.Y;
      if ii < 0 then result := false;
      if ii >= Length(GameBoard) then result := false;
      if jj >= Length(GameBoard[0]) then result := false;
      if result then
      begin
        if GameBoard[ii, jj] <> 0 then result := false;
      end;
    end;
end;

procedure TTetris.StepGame;
var
  pos: TPoint;
begin
  pos := currenTSpritePos;
  pos.Y := currenTSpritePos.Y + 1;
  if CanSpriteBeHere(currenTSprite, Pos)
   then currenTSpritePos := pos
   else fusionCurrenTSprite;
end;

procedure TTetris.DefaultBitmap(bmp: Tbitmap);
var
  i, ii, jj, j: integer;
  col1: cardinal;
  factor: integer;
begin
  factor := 20;
  with bmp do
   begin
    Width := 0;
    Canvas.Brush.Color := clBlack;
    Height := factor * Length(GameBoard[0]);
    Width := factor * Length(GameBoard);
   end;

  for i := 0 to Length(GameBoard) - 1 do
    for j := 0 to length(GameBoard[0]) - 1 do
     begin
      if GameBoard[i, j] = 0 then
       with bmp.Canvas do
        begin
         Pen.Color := clred;
         Brush.Color := GameBoard[i, j];
        end;
      if GameBoard[i, j] <> 0 then
       begin
        col1 := ColorToRGB(GameBoard[i, j]);
        col1 := RGB(trimint(GetRValue(col1) - 50, 0, 255),
          trimint(GetGValue(col1) - 50, 0, 255),
          trimint(GetBValue(col1) - 50, 0, 255));
        with bmp.Canvas do
         begin
          Brush.Color := col1;
          pen.Color := bmp.Canvas.Brush.Color;
          RoundRect(factor * i, factor * j, factor * i + (factor - 1),
                    factor * j + (factor - 1), 2, 2);
          Brush.Color := GameBoard[i, j];
          pen.Color := bmp.Canvas.Brush.Color;
          RoundRect(1 + factor * i, 1 + factor * j, factor * i + factor,
                    factor * j + factor, 2, 2);
        end;
       end;
     end;
  for i := 0 to 3 do
    for j := 0 to 3 do
      if currenTSprite[i, j] <> 0 then
      begin
        ii := i + currenTSpritePos.x;
        jj := j + currenTSpritePos.y;

        col1 := ColorToRGB(fCurrentColor);
        col1 := RGB(trimint(GetRValue(col1) - 50, 0, 255),
          trimint(GetGValue(col1) - 50, 0, 255), trimint(GetBValue(col1) - 50, 0, 255));
        with bmp.Canvas do
         begin
          Pen.Color := ColorToRGB(fCurrentColor);
          Brush.Color := ColorToRGB(fCurrentColor);
          RoundRect(factor * ii, factor * jj, factor * ii + (factor - 1), factor * jj + (factor - 1), 2, 2);
          Brush.Color := col1;
          Pen.Color := col1;
          RoundRect(1 + factor * ii, 1 + factor * jj, factor * ii + factor, factor * jj + factor, 2, 2);
         end;
      end;

end;

procedure TTetris.fusionCurrenTSprite;
var
  k, i, ii, jj, j: integer;
begin
  if fsprite then
  begin

    for i := 0 to 3 do
      for j := 0 to 3 do
      begin
        if CurrenTSprite[i, j] = 0 then continue;
        ii := i + CurrenTSpritePos.X;
        jj := j + CurrenTSpritePos.Y;
        GameBoard[ii, jj] := fCurrentColor;
      end;
    Fsprite := False;
  end;
  for j := 0 to Length(GameBoard[0]) - 1 do
  begin
    k := 0;
    for i := 0 to length(GameBoard) - 1 do
    begin
      if GameBoard[i][j] <> 0 then inc(k);
    end;
    if k = length(GameBoard) then
    begin
      inc(Flines);
      for jj := j downto 1 do
        for ii := 0 to Length(GameBoard) - 1 do
          GameBoard[ii][jj] := GameBoard[ii][jj - 1];
      for ii := 0 to Length(GameBoard) - 1 do
        GameBoard[ii][0] := 0;
    end;
  end;
end;

function TTetris.getcurrenTSprite: TSprite;
var
  i: integer;
begin
  if Fsprite then
    Result := FCurrentSprite
  else
  begin
    FSprite := true;
    Randomize;
    i := Random(7);
    FCurrentSprite := AllSpriteKind[i];
    Result := FCurrentSprite;
    case i of
      0: fCurrentColor := cllime;
      1: fCurrentColor := clblue;
      2: fCurrentColor := clOlive;
      3: fCurrentColor := clRed;
      4: fCurrentColor := clAqua;
      5: fCurrentColor := clWhite;
      6: fCurrentColor := clMoneyGreen;
    end;
    CurrenTSpritePos := point(3, 0);
    if not CanSpriteBeHere(FCurrentSprite, CurrenTSpritePos) then
    begin
      NewGameBoard;
    end;
  end;
end;

procedure TTetris.Left;
var
  pos: TPoint;
begin
  pos := CurrenTSpritePos;
  pos.x := CurrenTSpritePos.X - 1;
  if CanSpriteBeHere(CurrenTSprite, Pos) then
    currenTSpritePos := pos
end;

procedure TTetris.NewGameBoard;
var
  i, j: integer;
begin
  flines := 0;
  for i := 0 to Length(GameBoard) - 1 do
    for j := 0 to length(GameBoard[0]) - 1 do
      GameBoard[i, j] := 0;
end;

procedure TTetris.Right;
var
  pos: TPoint;
begin
  pos := CurrenTSpritePos;
  pos.x := CurrenTSpritePos.X + 1;
  if CanSpriteBeHere(CurrenTSprite, Pos) then
    CurrenTSpritePos := pos
end;

procedure TTetris.Rotate;
var
  sprite: TSprite;
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      sprite[3 - j, i] := CurrenTSprite[i, j];
    end;
  if CanSpriteBeHere(sprite, CurrenTSpritePos) then
    CurrenTSprite := sprite;
end;

procedure TTetris.SetcurrenTSprite(const Value: TSprite);
begin
  FCurrentSprite := Value;
end;

procedure TTetris.SetcurrenTSpritePos(const Value: tpoint);
begin
  FCurrentSpritePos := Value;
end;

end.

