unit uEditor;

interface
uses Windows, DAudioEffectX, Forms,
  StdCtrls, Dialogs, ChessBrd, Menus, ExtCtrls,
  Classes, Controls, Graphics;

type
  TPluginEditorWindow = class(TForm)
    bg: TImage;
    ChessBrd1: TChessBrd;
    Image5: TImage;
    Image4: TImage;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image6: TImage;
    Image7: TImage;
    options: TPopupMenu;
    ShowBoardlines1: TMenuItem;
    ShowCoordinates1: TMenuItem;
    NewGame1: TMenuItem;
    WhiteonTop1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Player11: TMenuItem;
    Player21: TMenuItem;
    Human1: TMenuItem;
    Machine1: TMenuItem;
    Human2: TMenuItem;
    Machine2: TMenuItem;
    EngineSearchDepth1: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    N71: TMenuItem;
    N81: TMenuItem;
    BoardSet1: TMenuItem;
    PiecesSet1: TMenuItem;
    Set11: TMenuItem;
    Set21: TMenuItem;
    Set31: TMenuItem;
    Set41: TMenuItem;
    Set51: TMenuItem;
    Set12: TMenuItem;
    Set22: TMenuItem;
    Set32: TMenuItem;
    Set42: TMenuItem;
    Set52: TMenuItem;
    Set61: TMenuItem;
    Image8: TImage;
    white1: TImage;
    white2: TImage;
    black1: TImage;
    black2: TImage;
    Image9: TImage;
    Updater: TTimer;
    Image10: TImage;
    AboutBox: TPanel;
    Memo1: TMemo;
    Image11: TImage;
    Label1: TLabel;
    procedure ChessBrd1LegalMove(Sender: TObject; oldSq, newSq: Square);
    procedure ChessBrd1Mate(Sender: TObject; oldSq, newSq: Square);
    procedure ChessBrd1Draw(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChessBrd1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowBoardlines1Click(Sender: TObject);
    procedure ShowCoordinates1Click(Sender: TObject);
    procedure WhiteonTop1Click(Sender: TObject);
    procedure NewGame1Click(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure N41Click(Sender: TObject);
    procedure N51Click(Sender: TObject);
    procedure N61Click(Sender: TObject);
    procedure N71Click(Sender: TObject);
    procedure N81Click(Sender: TObject);
    procedure Set11Click(Sender: TObject);
    procedure Set21Click(Sender: TObject);
    procedure Set31Click(Sender: TObject);
    procedure Set41Click(Sender: TObject);
    procedure Set51Click(Sender: TObject);
    procedure Set12Click(Sender: TObject);
    procedure Set22Click(Sender: TObject);
    procedure Set32Click(Sender: TObject);
    procedure Set42Click(Sender: TObject);
    procedure Set52Click(Sender: TObject);
    procedure Set61Click(Sender: TObject);
    procedure Human1Click(Sender: TObject);
    procedure Machine1Click(Sender: TObject);
    procedure Human2Click(Sender: TObject);
    procedure Machine2Click(Sender: TObject);
    procedure Image8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure white1Click(Sender: TObject);
    procedure white2Click(Sender: TObject);
    procedure black2Click(Sender: TObject);
    procedure black1Click(Sender: TObject);
    procedure Image9Click(Sender: TObject);
    procedure UpdaterTimer(Sender: TObject);
    procedure Image10Click(Sender: TObject);
  private
    FEffect:AudioEffectX;
  public
    pars:array[1..8] of single;
    s:string;
    property Effect: AudioEffectX read FEffect write FEffect;
  private
  end;

implementation

{$R *.DFM}

uses uPlugin;

procedure TPluginEditorWindow.ChessBrd1LegalMove(Sender: TObject; oldSq,
  newSq: Square);
var i:integer;
begin
 s:=chessbrd1.position;
 for i:=1 to 64 do effect.setparameter(i,ord(s[i])/1000);
 if chessbrd1.whitetomove then effect.setparameter(73,0)
 else effect.setparameter(73,1);
end;

procedure TPluginEditorWindow.ChessBrd1Mate(Sender: TObject; oldSq,
  newSq: Square);
begin
 MessageDlg('Game Over: Mate',mtInformation,[mbOk],0);
 Chessbrd1.NewGame;
end;

procedure TPluginEditorWindow.ChessBrd1Draw(Sender: TObject);
begin
 MessageDlg('Game Over: Draw',mtInformation,[mbOk],0);
 Chessbrd1.NewGame;
end;

procedure TPluginEditorWindow.FormCreate(Sender: TObject);
begin
 Chessbrd1.PopupMenu:=options;
 Chessbrd1.squaredark:=Image6.Picture.Bitmap;
 Chessbrd1.squarelight:=Image7.Picture.Bitmap;
 AboutBox.left:=40;
 AboutBox.top:=136;
end;

procedure TPluginEditorWindow.ChessBrd1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var cset: CoordSet;
begin
 if (x<24) or (x>chessbrd1.width-24) or (y<24) or (y>chessbrd1.height-24) then
 begin
    if (not showcoordinates1.checked) then
    begin
        Include (cset,North);
        Include (cset,East);
        Include (cset,West);
        Include (cset,South);
        showcoordinates1.checked:=true;
    end
    else
    begin
        Exclude (cset,North);
        Exclude (cset,East);
        Exclude (cset,West);
        Exclude (cset,South);
        showcoordinates1.checked:=false;
    end;
    Chessbrd1.DisplayCoords:=cset;
 end;
end;

procedure TPluginEditorWindow.ShowBoardlines1Click(Sender: TObject);
begin
 showboardlines1.checked:=not showboardlines1.checked;
 Chessbrd1.BoardLines:=Showboardlines1.checked;
 if chessbrd1.boardlines then
  effect.setparameter(70,1) else effect.setparameter(70,0);
end;

procedure TPluginEditorWindow.ShowCoordinates1Click(Sender: TObject);
var cset: CoordSet;
begin
    if (not showcoordinates1.checked) then
    begin
        Include (cset,North);
        Include (cset,East);
        Include (cset,West);
        Include (cset,South);
        showcoordinates1.checked:=true;
        effect.setparameter(71,1);
    end
    else
    begin
        Exclude (cset,North);
        Exclude (cset,East);
        Exclude (cset,West);
        Exclude (cset,South);
        showcoordinates1.checked:=false;
        effect.setparameter(71,0);
    end;
    Chessbrd1.DisplayCoords:=cset;
end;

procedure TPluginEditorWindow.WhiteonTop1Click(Sender: TObject);
begin
  WhiteOnTop1.checked:=not WhiteOnTop1.checked;
  Chessbrd1.WhiteOnTop:=WhiteOnTop1.checked;
  if chessbrd1.whiteontop then
   effect.setparameter(72,1) else effect.setparameter(72,0);
end;

procedure TPluginEditorWindow.NewGame1Click(Sender: TObject);
begin
 Chessbrd1.NewGame;
 if Chessbrd1.ComputerPlaysWhite then Chessbrd1.Think;
end;

procedure TPluginEditorWindow.N11Click(Sender: TObject);
begin
 N11.checked:=true;
 chessbrd1.SearchDepth:=1;
 effect.setparameter(67,0);
end;

procedure TPluginEditorWindow.N21Click(Sender: TObject);
begin
 N21.checked:=true;
 chessbrd1.SearchDepth:=2;
 effect.setparameter(67,0.1);
end;

procedure TPluginEditorWindow.N31Click(Sender: TObject);
begin
 N31.checked:=true;
 chessbrd1.SearchDepth:=3;
 effect.setparameter(67,0.2);
end;

procedure TPluginEditorWindow.N41Click(Sender: TObject);
begin
 N41.checked:=true;
 chessbrd1.SearchDepth:=4;
 effect.setparameter(67,0.3);
end;

procedure TPluginEditorWindow.N51Click(Sender: TObject);
begin
 N51.checked:=true;
 chessbrd1.SearchDepth:=5;
 effect.setparameter(67,0.4);
end;

procedure TPluginEditorWindow.N61Click(Sender: TObject);
begin
 N61.checked:=true;
 chessbrd1.SearchDepth:=6;
 effect.setparameter(67,0.5);
end;

procedure TPluginEditorWindow.N71Click(Sender: TObject);
begin
 N71.checked:=true;
 chessbrd1.SearchDepth:=7;
 effect.setparameter(67,0.6);
end;

procedure TPluginEditorWindow.N81Click(Sender: TObject);
begin
 N81.checked:=true;
 chessbrd1.SearchDepth:=8;
 effect.setparameter(67,0.7);
end;

procedure TPluginEditorWindow.Set11Click(Sender: TObject);
begin
 set11.checked:=true;
 Chessbrd1.squaredark:=Image6.Picture.Bitmap;
 Chessbrd1.squarelight:=Image7.Picture.Bitmap;
 effect.setparameter(68,0);
end;

procedure TPluginEditorWindow.Set21Click(Sender: TObject);
begin
 set21.checked:=true;
 Chessbrd1.squaredark:=nil;
 Chessbrd1.squarelight:=nil;
 Chessbrd1.squarecolordark:=clgray;
 Chessbrd1.squarecolorlight:=clsilver;
 effect.setparameter(68,0.1);
end;

procedure TPluginEditorWindow.Set31Click(Sender: TObject);
begin
 set31.checked:=true;
 Chessbrd1.squaredark:=nil;
 Chessbrd1.squarelight:=nil;
 Chessbrd1.squarecolordark:=clblack;
 Chessbrd1.squarecolorlight:=clwhite;
 effect.setparameter(68,0.2);
end;

procedure TPluginEditorWindow.Set41Click(Sender: TObject);
begin
 set41.checked:=true;
 Chessbrd1.squaredark:=nil;
 Chessbrd1.squarelight:=nil;
 Chessbrd1.squarecolordark:=$00C08080;
 Chessbrd1.squarecolorlight:=$00C8C8C8;
 effect.setparameter(68,0.3);
end;

procedure TPluginEditorWindow.Set51Click(Sender: TObject);
begin
 set51.checked:=true;
 Chessbrd1.squaredark:=nil;
 Chessbrd1.squarelight:=nil;
 Chessbrd1.squarecolordark:=$00404080;
 Chessbrd1.squarecolorlight:=clgray;
 effect.setparameter(68,0.4);
end;

procedure TPluginEditorWindow.Set12Click(Sender: TObject);
begin
 set12.checked:=true;
 Chessbrd1.CustomPieceSet:=nil;
 effect.setparameter(69,0);
end;

procedure TPluginEditorWindow.Set22Click(Sender: TObject);
begin
 set22.checked:=true;
 Chessbrd1.CustomPieceSet:=Image1.Picture.Bitmap;
 effect.setparameter(69,0.1);
end;

procedure TPluginEditorWindow.Set32Click(Sender: TObject);
begin
 set32.checked:=true;
 Chessbrd1.CustomPieceSet:=Image2.Picture.Bitmap;
 effect.setparameter(69,0.2);
end;

procedure TPluginEditorWindow.Set42Click(Sender: TObject);
begin
 set42.checked:=true;
 Chessbrd1.CustomPieceSet:=Image3.Picture.Bitmap;
 effect.setparameter(69,0.3);
end;

procedure TPluginEditorWindow.Set52Click(Sender: TObject);
begin
 set52.checked:=true;
 Chessbrd1.CustomPieceSet:=Image4.Picture.Bitmap;
 effect.setparameter(69,0.4);
end;

procedure TPluginEditorWindow.Set61Click(Sender: TObject);
begin
 set61.checked:=true;
 Chessbrd1.CustomPieceSet:=Image5.Picture.Bitmap;
 effect.setparameter(69,0.5);
end;

procedure TPluginEditorWindow.Human1Click(Sender: TObject);
begin
 human1.checked:=true;
 white2.visible:=false;
 Chessbrd1.ComputerPlaysWhite:=false;
 effect.setparameter(65,0);
end;

procedure TPluginEditorWindow.Machine1Click(Sender: TObject);
begin
 machine1.checked:=true;
 white2.visible:=true;
 Chessbrd1.ComputerPlaysWhite:=true;
 effect.setparameter(65,1);
end;

procedure TPluginEditorWindow.Human2Click(Sender: TObject);
begin
 human2.checked:=true;
 black2.visible:=false;
 Chessbrd1.ComputerPlaysBlack:=false;
 effect.setparameter(66,0);
end;

procedure TPluginEditorWindow.Machine2Click(Sender: TObject);
begin
 machine2.checked:=true;
 black2.visible:=true;
 Chessbrd1.ComputerPlaysBlack:=true;
 effect.setparameter(66,1);
end;

procedure TPluginEditorWindow.Image8MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if x<19 then Chessbrd1.MoveBackward
 else Chessbrd1.MoveForward;
end;

procedure TPluginEditorWindow.white1Click(Sender: TObject);
begin
 white2.visible:=true;
 machine1click(sender);
end;

procedure TPluginEditorWindow.white2Click(Sender: TObject);
begin
 white2.visible:=false;
 human1click(sender);
end;

procedure TPluginEditorWindow.black2Click(Sender: TObject);
begin
 black2.visible:=false;
 human2click(sender);
end;

procedure TPluginEditorWindow.black1Click(Sender: TObject);
begin
 black2.visible:=true;
 machine2click(sender);
end;

procedure TPluginEditorWindow.Image9Click(Sender: TObject);
begin
 Chessbrd1.NewGame;
 if Chessbrd1.ComputerPlaysWhite then Chessbrd1.Think;
end;

procedure TPluginEditorWindow.UpdaterTimer(Sender: TObject);
begin
 with chessbrd1 do
 begin
 case round(pars[4]*10) of
 0:if not set11.checked then set11click(nil);
 1:if not set21.checked then set21click(nil);
 2:if not set31.checked then set31click(nil);
 3:if not set41.checked then set41click(nil);
 4:if not set51.checked then set51click(nil);
 end;

 if (pars[1]>0.5) then begin
 if not machine1.checked then machine1click(sender) end
 else begin if not human1.checked then human1click(sender) end;

 if pars[2]>0.5 then begin
 if not machine2.checked then machine2click(sender);end
 else begin if not human2.checked then human2click(sender) end;

 case round(10*pars[3]) of
 0:if not N11.checked then N11click(nil);
 1:if not N21.checked then N21click(nil);
 2:if not N31.checked then N31click(nil);
 3:if not N41.checked then N41click(nil);
 4:if not N51.checked then N51click(nil);
 5:if not N61.checked then N61click(nil);
 6:if not N71.checked then N71click(nil);
 7:if not N81.checked then N81click(nil);
 end;

 case round(pars[5]*10) of
 0:if not set12.checked then set12click(nil);
 1:if not set22.checked then set22click(nil);
 2:if not set32.checked then set32click(nil);
 3:if not set42.checked then set42click(nil);
 4:if not set52.checked then set52click(nil);
 5:if not set61.checked then set61click(nil);
 end;

 if pars[6]>0.5 then begin
 if not boardlines then boardlines:=true end
 else begin if boardlines then boardlines:=false;
 end;
 if pars[7]>0.5 then
 begin
  if not showcoordinates1.checked then
  begin
   showcoordinates1.checked:=true;
   showcoordinates1click(nil);
  end;
 end
 else
 begin
  if showcoordinates1.checked then
  begin
   showcoordinates1.checked:=false;
   showcoordinates1click(nil);
  end;
 end;
 if (pars[8]>0.5) then
 begin if not whiteontop then whiteontop:=true end
 else begin if whiteontop then whiteontop:=false; end;
 end;
end;

procedure TPluginEditorWindow.Image10Click(Sender: TObject);
begin
 chessbrd1.visible:=not chessbrd1.visible;
 AboutBox.visible:=not AboutBox.visible;
end;

end.


