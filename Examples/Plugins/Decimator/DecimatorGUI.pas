unit DecimatorGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ExtCtrls, Controls, Gauges,
  StdCtrls, Graphics, DAV_Common, DAV_VSTModule;

type
  TMouseContext = (mcNone, mcSHRate, mcBits, mcCut, mcRes, mcMix, mcVol);
  TVSTGUI = class(TForm)
    bg: TImage;
    LbMix: TLabel;
    LbRate: TLabel;
    LbCut: TLabel;
    LbRes: TLabel;
    LbVol: TLabel;
    LbBits: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LbVSTTechnology: TLabel;
    PanelAbout: TPanel;
    FltType: TImage;
    FltType2: TImage;
    ImageAbout: TImage;
    ImageTobyBear: TImage;
    ImageDecimator: TImage;
    ShSHRateBg: TShape;
    ShSHRate: TShape;
    ShBitsBg: TShape;
    ShBits: TShape;
    ShMixBg: TShape;
    ShMix: TShape;
    shVolBg: TShape;
    ShVol: TShape;
    ShCutBg: TShape;
    ShCut: TShape;
    ShResBg: TShape;
    ShRes: TShape;
    procedure FltType2Click(Sender: TObject);
    procedure ImageTobyBearClick(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure ImageDecimatorClick(Sender: TObject);
    procedure ShSHRateBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShCutBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShResBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShMixBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shVolBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShSHRateBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShSHRateMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShSHRateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShCutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShResMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShMixMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShVolMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShCutBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShResBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShMixBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure shVolBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShCutMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShResMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShMixMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShVolMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    fMouseContext  : TMouseContext;
  public
  end;

implementation

{$R *.DFM}

uses DecimatorModule;

procedure TVSTGUI.FormCreate(Sender: TObject);
begin
 PanelAbout.Top:=ImageDecimator.Height-1;
end;

procedure TVSTGUI.FltType2Click(Sender: TObject);
begin
 if FltType.visible then
  begin
   FltType.visible:=false;
   TVSTDecimator(Owner).Parameter[4]:=0;
  end
 else
  begin
   FltType.visible:=true;
   TVSTDecimator(Owner).Parameter[4]:=1;
  end;
end;

procedure TVSTGUI.ImageTobyBearClick(Sender: TObject);
begin
 PanelAbout.visible:=not PanelAbout.visible;
end;

procedure TVSTGUI.Label3Click(Sender: TObject);
begin
 PanelAbout.visible:=false;
end;

procedure TVSTGUI.ImageDecimatorClick(Sender: TObject);
begin
 PanelAbout.visible:=not PanelAbout.visible;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////// Mouse Downs /////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TVSTGUI.ShSHRateBgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcSHRate;
 ShSHRateBgMouseMove(Sender,Shift,X,Y);
end;

procedure TVSTGUI.ShSHRateMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcSHRate;
 ShSHRateBgMouseMove(Sender,Shift,X,ShSHRate.Top-ShSHRateBg.Top+Y);
end;

procedure TVSTGUI.ShBitsBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcBits;
 ShBitsBgMouseMove(Sender,Shift,X,Y);
end;

procedure TVSTGUI.ShCutBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcCut;
 ShCutBgMouseMove(Sender,Shift,X,Y);
end;

procedure TVSTGUI.ShResBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcRes;
 ShResBgMouseMove(Sender,Shift,X,Y);
end;

procedure TVSTGUI.ShMixBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcMix;
 ShMixBgMouseMove(Sender,Shift,X,Y);
end;

procedure TVSTGUI.shVolBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcVol;
 ShVolBgMouseMove(Sender,Shift,X,Y);
end;

procedure TVSTGUI.ShBitsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcBits;
 ShBitsBgMouseMove(Sender,Shift,X,ShBits.Top-ShBitsBg.Top+Y);
end;

procedure TVSTGUI.ShCutMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcCut;
 ShCutBgMouseMove(Sender,Shift,X,ShCut.Top-ShCutBg.Top+Y);
end;

procedure TVSTGUI.ShResMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcRes;
 ShResBgMouseMove(Sender,Shift,X,ShRes.Top-ShResBg.Top+Y);
end;

procedure TVSTGUI.ShMixMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcMix;
 ShMixBgMouseMove(Sender,Shift,X,ShMix.Top-ShMixBg.Top+Y);
end;

procedure TVSTGUI.ShVolMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 fMouseContext:=mcVol;
 ShVolBgMouseMove(Sender,Shift,X,ShVol.Top-ShVolBg.Top+Y);
end;

procedure TVSTGUI.shMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 case fMouseContext of
  mcSHRate : LbRate.Caption := 'Rate';
  mcBits   : LbBits.Caption := 'Bits';
  mcCut    : LbCut.Caption  := 'Cut';
  mcRes    : LbRes.Caption  := 'Res';
  mcMix    : LbMix.Caption  := 'Mix';
  mcVol    : LbVol.Caption  := 'Vol';
 end;

 fMouseContext:=mcNone;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////// Mouse Moves /////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TVSTGUI.ShSHRateBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (fMouseContext=mcSHRate)  then
  begin
   if ShSHRateBg.Height-Y>ShSHRateBg.Height
    then TVSTDecimator(Owner).Parameter[0]:=44100
    else
     if ShSHRateBg.Height-Y<0
      then TVSTDecimator(Owner).Parameter[0]:=44.1
      else TVSTDecimator(Owner).Parameter[0]:=FreqLinearToLog(1-Y/ShSHRateBg.Height)*2.205;
   LbRate.Caption:=FloatToStrF(TVSTDecimator(Owner).Parameter[0],ffGeneral,5,2);
  end;
end;

procedure TVSTGUI.ShBitsBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (fMouseContext=mcBits) then
  begin
   if ShBitsBg.Height-Y>ShBitsBg.Height
    then TVSTDecimator(Owner).Parameter[1]:=24
    else
     if ShBitsBg.Height-Y<0
      then TVSTDecimator(Owner).Parameter[1]:=1
      else TVSTDecimator(Owner).Parameter[1]:=Round(23*(1-(Y/ShBitsBg.Height))+1);
   LbBits.Caption:=IntToStr(Round(TVSTDecimator(Owner).Parameter[1]));
  end;
end;

procedure TVSTGUI.ShCutBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (fMouseContext=mcCut) and (Y>=-10) then
  begin
   if ShCutBg.Height-Y>ShCutBg.Height
    then TVSTDecimator(Owner).Parameter[2]:=20000
    else
     if ShCutBg.Height-Y<0
      then TVSTDecimator(Owner).Parameter[2]:=20
      else TVSTDecimator(Owner).Parameter[2]:=FreqLinearToLog(1-Y/ShCutBg.Height);
   LbCut.Caption:=FloatToStrF(TVSTDecimator(Owner).Parameter[2],ffGeneral,5,2);
  end;
end;

procedure TVSTGUI.ShResBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (fMouseContext=mcRes) then
  begin
   if ShResBg.Height-Y>ShResBg.Height
    then TVSTDecimator(Owner).Parameter[3]:=8
    else
     if ShResBg.Height-Y<=0
      then TVSTDecimator(Owner).Parameter[3]:=0.1
      else TVSTDecimator(Owner).Parameter[3]:=8*(1-Y/ShResBg.Height);
   LbRes.Caption:=FloatToStrF(TVSTDecimator(Owner).Parameter[3],ffGeneral,2,2);
  end;
end;

procedure TVSTGUI.ShMixBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (fMouseContext=mcMix) then
  begin
   if ShMixBg.Height-Y>ShMixBg.Height
    then TVSTDecimator(Owner).Parameter[5]:=100
    else
     if ShMixBg.Height-Y<=0
      then TVSTDecimator(Owner).Parameter[5]:=0
      else TVSTDecimator(Owner).Parameter[5]:=100*(1-Y/ShMixBg.Height);
   LbMix.Caption:=IntToStr(round(TVSTDecimator(Owner).Parameter[5]));
  end;
end;

procedure TVSTGUI.shVolBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (fMouseContext=mcVol) then
  begin
   if ShVolBg.Height-Y>ShVolBg.Height
    then TVSTDecimator(Owner).Parameter[6]:=-24
    else
     if ShVolBg.Height-Y<=0
      then TVSTDecimator(Owner).Parameter[6]:=6
      else TVSTDecimator(Owner).Parameter[6]:=6-30*Y/ShVolBg.Height;
   LbVol.Caption:=IntToStr(round(TVSTDecimator(Owner).Parameter[6]));
  end;
end;

procedure TVSTGUI.ShSHRateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShSHRateBgMouseMove(Sender,Shift,X,ShSHRate.Top-ShSHRateBg.Top+Y);
end;

procedure TVSTGUI.ShBitsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShBitsBgMouseMove(Sender,Shift,X,ShBits.Top-ShBitsBg.Top+Y);
end;

procedure TVSTGUI.ShCutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShCutBgMouseMove(Sender,Shift,X,ShCut.Top-ShCutBg.Top+Y);
end;

procedure TVSTGUI.ShResMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShResBgMouseMove(Sender,Shift,X,ShRes.Top-ShResBg.Top+Y);
end;

procedure TVSTGUI.ShMixMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShMixBgMouseMove(Sender,Shift,X,ShMix.Top-ShMixBg.Top+Y);
end;

procedure TVSTGUI.ShVolMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShVolBgMouseMove(Sender,Shift,X,ShVol.Top-ShVolBg.Top+Y);
end;

end.
