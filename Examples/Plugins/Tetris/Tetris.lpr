{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Tetris;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  DVSTEffect,
  TetrisUnit in 'TetrisUnit.pas',
  TetrisDM in 'TetrisDM.pas' {TetrisModule: TVSTModule},
  TetrisEditor in 'TetrisEditor.pas' {FmTetris};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  TetrisModule: TTetrisModule;
begin
  try
    TetrisModule := TTetrisModule.Create(Application);
    TetrisModule.Effect^.user := TetrisModule;
    TetrisModule.AudioMaster := audioMaster;
    Result := TetrisModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
